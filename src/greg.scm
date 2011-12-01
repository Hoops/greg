;;; Regression testing framework written in Guile
;;; Copyright (C) 1998, 2001, 2003, 2005 Free Software Foundation, Inc.
;;;
;;; Written by:  Richard frith-Macdonald <rfm@gnu.org>
;;; Modified by: Maurizio Boriani <baux@member.fsf.org>
;;;   
;;; This file is part of the Greg package 
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;; 
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;

;;;
;;;        greg routines (GNUstep-Guile regression testing)
;;;
;;;    To run tests you use -
;;;
;;;        (greg-test-run)                to run normal tests
;;;                                This returns #t on success
;;;
;;;    To write tests, you place '.scm' files in a test subdirectory and write
;;;    Guile code to perform the tests and recording the results by calling the
;;;    procedure -
;;;
;;;        (greg-testcase assertion condition thunk)
;;;
;;;            The 'assertion' is a string describing what is being tested.
;;;            The 'condition' is a boolean defining whether the assertion is
;;;                expected to be true or not.
;;;            The 'thunk' is the Guile procedure to actually perform
;;;                the test, this must return #t if the test succeeds, #f if
;;;                it fails, and any other value if the result is undefined.
;;;                Alternatively, it may throw an exception whose value may
;;;                be 'pass, 'fail, 'untested, 'unsupported, and all other
;;;                values effectively being 'unresolved.
;;;
;;;            In normal use this might look like this -
;;;
;;;                (define one 1)
;;;                (define two 2)
;;;
;;;                (greg-testcase "one plus one is two" #t
;;;                (lambda ()
;;;
;;;                (eq? (+ one one) two)
;;;
;;;                ))
;;;
;;;    If you want to produce additional debug output from your script - use
;;;
;;;        (greg-dlog ...)

(define-module (greg)
  #:version (2 1 0)
  #:use-module (ice-9 expect)
  #:export (greg-version
            greg-debug greg-posix greg-verbose greg-obj-dir greg-out-dir
            greg-src-dir *greg-script-dir* greg-files greg-tools
            greg-paths greg-case-begin greg-case-end
            greg-attempts greg-unresolved
            greg-unsupported
            greg-untested
            greg-passes
            greg-failures
            greg-upasses
            greg-ufailures
            greg-directory
            greg-casename
            greg-filename
            greg-toolname greg-dlog greg-testcase greg-test-run
            greg-child greg-send))

;;; Dynamically link the glue code for accessing the pty library,
;;; but only when it isn't already present.

(load-extension "libgreg" "scm_init_greg")

;; Can we get this by introspecting the module version some way?
(define (greg-version) "2.1.0")

;; There are some variables to modify the behaviour of the system

(define greg-debug #f)
(define greg-posix #f)
(define greg-verbose 0)
(define greg-obj-dir "")
(define greg-out-dir "")
(define greg-src-dir ".")
(define *greg-script-dir* "")
(define greg-files '())
(define greg-tools '())
(define greg-paths '())
(define greg-case-begin (lambda () '()))
(define greg-case-end (lambda (result) '()))

;; Define top-level variables for storing test statistics etc.
(define *greg-case-name* "")
(define *greg-file-name* "")
(define *greg-tool-name* "")
(define *greg-num-tools* 0)
(define *greg-test-expects* #t)
(define *greg-individual-test* #f)

(define *greg-tool-expect-timeout* 15)
(define *greg-tool-expect-timeout-proc* expect-timeout-proc)
(define *greg-tool-expect-eof-proc* expect-eof-proc)
(define *greg-tool-expect-char-proc* expect-char-proc)

(define *greg-tool-attempts* 0)
(define *greg-tool-unresolved* 0)
(define *greg-tool-unsupported* 0)
(define *greg-tool-untested* 0)
(define *greg-tool-passes* 0)
(define *greg-tool-failures* 0)
(define *greg-tool-upasses* 0)
(define *greg-tool-ufailures* 0)
(define *greg-test-exceptions* 0)

(define *greg-total-attempts* 0)
(define *greg-total-unresolved* 0)
(define *greg-total-unsupported* 0)
(define *greg-total-untested* 0)
(define *greg-total-passes* 0)
(define *greg-total-failures* 0)
(define *greg-total-upasses* 0)
(define *greg-total-ufailures* 0)

;; Other variables used by the framework

(define *greg-dbg-port* #f)
(define *greg-log-port* #f)
(define *greg-child-info* '())
(define *greg-expect-list* '())

;; Public functions to access some counters

(define (greg-attempts) *greg-total-attempts*)
(define (greg-unresolved) *greg-total-unresolved*)
(define (greg-unsupported) *greg-total-unsupported*)
(define (greg-untested) *greg-total-untested*)
(define (greg-passes) *greg-total-passes*)
(define (greg-failures) *greg-total-failures*)
(define (greg-upasses) *greg-total-upasses*)
(define (greg-ufailures) *greg-total-ufailures*)

;; Access the current script directory path

(define (greg-directory) *greg-script-dir*)
(define (greg-casename) *greg-case-name*)
(define (greg-filename) *greg-file-name*)
(define (greg-toolname) *greg-tool-name*)

(define (insert-before list elem)
  (if (eq? list '())
      (cons elem list)
      (if (string<? elem (car list))
          (cons elem list)
          (begin
            (set-cdr! list (insert-before (cdr list) elem))
            list))))

(define (abspath path)
  (let ((trimmed (if (string=? (substring path 0 1) "/")
                     path
                     (string-append (getcwd) "/" path))))
    (begin
      (set! *greg-script-dir* trimmed)
      (do ()
          ((equal? (substring *greg-script-dir* (- (string-length *greg-script-dir*) 1) (string-length *greg-script-dir*)) "/"))
        (begin
          (set! *greg-script-dir* (substring *greg-script-dir* 0 (- (string-length *greg-script-dir*) 1)))))
      trimmed)))

;; Procedure to start a test run

(define (greg-init file)
  (let ((test-file (if (or (string=? file "") (string=? file ".")) "tests" file)))
    (if (output-port? *greg-log-port*)
        (close-output-port *greg-log-port*))
    (set! *greg-log-port* (open-output-file (string-append (greg-out-file test-file) ".log")))
    (if greg-debug
        (begin
          (if (output-port? *greg-dbg-port*)
              (close-output-port *greg-dbg-port*))
          (set! *greg-dbg-port*
                (open-output-file (string-append (greg-out-file test-file) ".dbg")))))))

;;; Procedure to close log and debug ports (normally done before quitting)
(define greg-close-logs
  (lambda ()
    (begin 
      (if (output-port? *greg-dbg-port*)
          (begin
            (close-output-port *greg-dbg-port*)
            (set! *greg-dbg-port* #f)))
      (if (output-port? *greg-log-port*)
          (begin
            (close-output-port *greg-log-port*)
            (set! *greg-log-port* #f))))))

;;; Procedure to reset stuff at the end of a test run.

(define (greg-reset)
  (let ((ok (= (+ *greg-total-upasses* *greg-total-ufailures* *greg-total-unresolved* *greg-total-unsupported* *greg-total-untested*) 0)))
    (greg-end-child)
    (greg-close-logs)
    (set! *greg-num-tools* 0)
    (set! *greg-tool-name* "")
    (set! *greg-file-name* "")
    (set! *greg-case-name* "")
    
    (set! *greg-tool-attempts* 0)
    (set! *greg-tool-unresolved* 0)
    (set! *greg-tool-unsupported* 0)
    (set! *greg-tool-untested* 0)
    (set! *greg-tool-passes* 0)
    (set! *greg-tool-failures* 0)
    (set! *greg-tool-upasses* 0)
    (set! *greg-tool-ufailures* 0)
    (set! *greg-test-exceptions* 0)

    (set! *greg-total-attempts* 0)
    (set! *greg-total-unresolved* 0)
    (set! *greg-total-unsupported* 0)
    (set! *greg-total-untested* 0)
    (set! *greg-total-passes* 0)
    (set! *greg-total-failures* 0)
    (set! *greg-total-upasses* 0)
    (set! *greg-total-ufailures* 0)
    ok))

;; Procedure to log a message (main output)

(define greg-log
  (lambda args
    (for-each (lambda (arg)
                (display arg)
                (and (output-port? *greg-log-port*) (display arg *greg-log-port*)))
              args)))

;; Procedure to log a message to the debug port - but only when in debug mode
(define greg-dlog
  (lambda args
    (if greg-debug
        (for-each
         (lambda (arg)
           (display arg)
           (and (output-port? *greg-dbg-port*) (display args *greg-dbg-port*)))
         args))))

;; Internal procedure to log a message - but only when in verbose mode

(define greg-vlog
  (lambda (level . args)
    (if (>= greg-verbose level)
        (for-each
         (lambda (arg)
           (display arg)
           (and (output-port? *greg-dbg-port*) (display args *greg-dbg-port*)))
         args))))

(define (greg-obj-file name)
  (if (and (string? greg-obj-dir) (> (string-length greg-obj-dir) 0))
      (string-append greg-obj-dir "/" name)
      name))

(define (greg-out-file name)
  (if (and (string? greg-out-dir) (> (string-length greg-out-dir) 0))
      (string-append greg-out-dir "/" name)
      name))

;; Display a summary of the test session

(define (greg-summary)
  (begin
    (greg-end-tool)
    (greg-log "\n                === Summary of all tests ===\n")
    (if (= *greg-num-tools* 0)
        (greg-log "\n# of tools                 " *greg-num-tools*)
        (greg-vlog 2 "\n# of tools                 " *greg-num-tools*))
    (if (= *greg-total-attempts* 0)
        (greg-log "\n# of testcases attempted   " *greg-total-attempts*)
        (greg-vlog 2 "\n# of testcases attempted   " *greg-total-attempts*))
    (if (or (> greg-verbose 0) (> *greg-total-passes* 0))
        (greg-log "\n# of expected passes       " *greg-total-passes*))
    (if (or (> greg-verbose 0) (> *greg-total-failures* 0))
        (greg-log "\n# of expected failures     " *greg-total-failures*))
    (if (or (> greg-verbose 0) (> *greg-total-upasses* 0))
        (greg-log "\n# of unexpected passes     " *greg-total-upasses*))
    (if (or (> greg-verbose 0) (> *greg-total-ufailures* 0))
        (greg-log "\n# of unexpected failures   " *greg-total-ufailures*))
    (if (or (> greg-verbose 0) (> *greg-total-unresolved* 0))
        (greg-log "\n# of unresolved testcases  " *greg-total-unresolved*))
    (if (or (> greg-verbose 0) (> *greg-total-unsupported* 0))
        (greg-log "\n# of unsupported testcases " *greg-total-unsupported*))
    (if (or (> greg-verbose 0) (> *greg-total-untested* 0))
        (greg-log "\n# of untested testcases    " *greg-total-untested*))
    (if (or (> greg-verbose 0) (> *greg-test-exceptions* 0))
        (greg-log "\n# of files abandoned       " *greg-test-exceptions*))
    (greg-log "\n\n")))

;; Procedure to build a list of all the available tool directories.
(define (greg-list-directories name)
  (if (and (access? name R_OK) (access? name X_OK)
           (eqv? (stat:type (stat name)) 'directory))
      (let ((dir '())
           (dirp (opendir name)))
        (begin
          (do
              ((entry (readdir dirp) (readdir dirp)))
              ((eof-object? entry) (closedir dirp) dir)
            (if (and (not (string=? entry "."))
                     (not (string=? entry ".."))
                     (or (= (length greg-tools) 0)
                         (member entry greg-tools))
                     ((lambda (path)
                        (and (access? path R_OK) (access? path X_OK)
                             (eqv? (stat:type (stat path)) 'directory)))
                      (string-append name "/" entry)))
                (set! dir (insert-before dir entry))))
          (if (eq? (length dir) 0)
              (begin
                (greg-log "\nThe directory '")
                (greg-log name)
                (greg-log "' does not contain any suitable tool directories!\n\n")
                '())
              (if (> (length greg-tools) 0)
                  (let ((tmpdir '()))
                    (begin
                      (for-each
                       (lambda (e)
                         (if (member e dir)
                             (set! tmpdir (cons e tmpdir))))
                       greg-tools)
                      (reverse tmpdir)))
                  dir))))
      ((greg-log "\nThis (")
       (greg-log name)
       (greg-log ") is not the name of an accessible directory!\n\n")
       '())))

;;    Procedure to build a list of all the readable '.scm' files in the
;;    specified directory.
(define (greg-list-files-in-directory name)
  (if (and (access? name R_OK) (access? name X_OK)
           (eqv? (stat:type (stat name)) 'directory))
      (let ((dir '())
           (dirp (opendir name)))
        (begin
          (do
              ((entry (readdir dirp) (readdir dirp)))
              ((eof-object? entry) (closedir dirp) dir)
            (if (and (> (string-length entry) 4)
                     (string=? (substring entry (- (string-length entry) 4)
                                          (string-length entry)) ".scm")
                     ((lambda (path)
                        (if (and (access? path R_OK)
                                 (eqv? (stat:type (stat path)) 'regular))
                            #t
                            (begin
                              (greg-log "\nThe file '")
                              (greg-log path)
                              (greg-log "' is not a normal readable file!\n\n")
                              #f)))
                      (string-append name "/" entry)))
                (set! dir (insert-before dir entry))))
          (if (eq? (length dir) 0)
              (begin
                (greg-log "\nThe directory '")
                (greg-log name)
                (greg-log "' does not contain any suitable scripts!\n\n")
                '())
              (if (> (length greg-files) 0)
                  (let ((tmpdir '()))
                    (begin
                      (for-each
                       (lambda (e)
                         (if (member e dir)
                             (set! tmpdir (cons e tmpdir))
                             (begin
                               (set! e (string-append e ".scm"))
                               (if (member e dir)
                                   (set! tmpdir (cons e tmpdir))))))
                       greg-files)
                      (reverse tmpdir)))
                  dir))))
      ((greg-log "\nThis (")
       (greg-log name)
       (greg-log ") is not the name of an accessible directory!\n\n")
       '())))

;;  This is the procedure to run a test - it attempts to load each '.scm'
;;  file in the test directory in turn.
(define (greg-do-test name)
  (begin
    (greg-start-tool name)
    (let ((ok-to-continue #t)
         (path (string-append name "/begin.grg")))
      (begin
        (if (and (access? path R_OK) (eqv? (stat:type (stat path)) 'regular))
            (catch #t
              (lambda ()
                (greg-vlog 3 "Loading " path " ...\n")
                (load (abspath path))
                (greg-vlog 3 "Loaded " path "\n"))
              (lambda key
                (begin
                  (set! *greg-test-exceptions* (+ *greg-test-exceptions* 1))
                  (set! *greg-tool-unresolved* (+ *greg-tool-unresolved* 1))
                  (set! *greg-total-unresolved* (+ *greg-total-unresolved* 1))
                  (greg-log "ERROR: " name " - Exception in " path ": " key "\n")
                  (set! ok-to-continue #f)
                  (if (eqv? (car key) 'quit)
                      (begin (greg-close-logs) (quit)))))))
        
        (if ok-to-continue
            (do
                ((file-list (greg-list-files-in-directory name) (cdr file-list)))
                ((eq? file-list '()) (greg-end-tool))
              (begin
                (set! *greg-file-name* (car file-list))
                (greg-vlog 2 "Running " *greg-file-name* " ...\n")
                (catch #t
                  (lambda ()
                    (set! path (string-append name "/" *greg-file-name*))
                    (load (abspath path)))
                  (lambda key
                    (begin
                      (set! *greg-test-exceptions* (+ *greg-test-exceptions* 1))
                      (set! *greg-tool-unresolved* (+ *greg-tool-unresolved* 1))
                      (set! *greg-total-unresolved* (+ *greg-total-unresolved* 1))
                      (greg-log *greg-tool-name* " - " *greg-file-name*)
                      (greg-log " - Exception: " key "\n")
                      (if (eqv? (car key) 'quit)
                          (begin (greg-close-logs) (quit))))))
                (greg-vlog 2 "Ended " *greg-file-name* "\n"))))

        (set! path (string-append name "/end.grg"))
        (if (and (access? path R_OK) (eqv? (stat:type (stat path)) 'regular))
            (catch #t
              (lambda ()
                (greg-vlog 3 "Loading " path " ...\n")
                (load (abspath path))
                (greg-vlog 3 "Loaded " path "\n"))
              (lambda key
                (begin
                  (set! *greg-test-exceptions* (+ *greg-test-exceptions* 1))
                  (set! *greg-tool-unresolved* (+ *greg-tool-unresolved* 1))
                  (set! *greg-total-unresolved* (+ *greg-total-unresolved* 1))
                  (greg-log "ERROR: " name " - Exception in " path ": " key "\n")
                  (if (eqv? (car key) 'quit)
                      (begin (greg-close-logs) (quit)))))))))
    (greg-end-tool)))

;; This is the procedure to run a testcase.  It produces the appropriate
;; output for the test depending on the return value of the thunk run
;; or any exception thrown by that thunk.
(define (greg-testcase assertion expecting thunk)
  (greg-vlog 4 "Entered testcase - " assertion "\n")
  (if (string=? *greg-case-name* "")
      (let ((testcase-return #f)
            (testcase-handled #f)
            (testcase-result #f))
        (begin
          (set! *greg-case-name* assertion)
          (greg-case-begin)
          (set! *greg-tool-attempts* (+ *greg-tool-attempts* 1))
          (set! *greg-total-attempts* (+ *greg-total-attempts* 1))
          (set! testcase-result
                (catch #t
                  (lambda ()
                    (let
                        ((tmpval (thunk)))
                      (begin
                        (gc)
                        tmpval)))
                  (lambda key
                    (set! testcase-handled #t)
                    (cond
                     ((eqv? (car key) 'fail)
                      (if expecting
                          (begin
                            (set! *greg-tool-ufailures* (+ *greg-tool-ufailures* 1))
                            (set! *greg-total-ufailures* (+ *greg-total-ufailures* 1))
                            (greg-log "FAIL: " *greg-case-name* "\n"))
                          (begin
                            (set! *greg-tool-failures* (+ *greg-tool-failures* 1))
                            (set! *greg-total-failures* (+ *greg-total-failures* 1))
                            (if greg-posix
                                (greg-log "FAIL: " *greg-case-name* "\n")
                                (greg-vlog 1 "XFAIL: " *greg-case-name* "\n")))))
                     ((eqv? (car key) 'pass)
                      (if expecting
                          (begin
                            (set! *greg-tool-passes* (+ *greg-tool-passes* 1))
                            (set! *greg-total-passes* (+ *greg-total-passes* 1))
                            (greg-vlog 1 "PASS: " *greg-case-name* "\n"))
                          (begin
                            (set! *greg-tool-upasses* (+ *greg-tool-upasses* 1))
                            (set! *greg-total-upasses* (+ *greg-total-upasses* 1))
                            (if greg-posix
                                (greg-vlog 1 "PASS: " *greg-case-name* "\n")
                                (greg-log "UPASS: " *greg-case-name* "\n")))))
                     ((eqv? (car key) 'unsupported)
                      (set! *greg-tool-unsupported* (+ *greg-tool-unsupported* 1))
                      (set! *greg-total-unsupported* (+ *greg-total-unsupported* 1))
                      (greg-log "UNSUPPORTED: " *greg-case-name* "\n"))
                     ((eqv? (car key) 'untested)
                      (set! *greg-tool-untested* (+ *greg-tool-untested* 1))
                      (set! *greg-total-untested* (+ *greg-total-untested* 1))
                      (greg-log "UNTESTED: " *greg-case-name* "\n"))
                     ((eqv? (car key) 'unresolved)
                      (set! *greg-tool-unresolved* (+ *greg-tool-unresolved* 1))
                      (set! *greg-total-unresolved* (+ *greg-total-unresolved* 1))
                      (greg-log "UNRESOLVED: " *greg-case-name* "\n"))
                     (else
                      (greg-log "Exception: " key "\n")
                      (set! *greg-tool-unresolved* (+ *greg-tool-unresolved* 1))
                      (set! *greg-total-unresolved* (+ *greg-total-unresolved* 1))
                      (greg-log "UNRESOLVED: " *greg-case-name* "\n")
                      (if (eqv? (car key) 'quit)
                          (begin (greg-close-logs) (quit))))))))
          (greg-case-end testcase-result)
          (if (boolean? testcase-result)
              (if testcase-result
                  (if expecting
                      (begin
                        (set! *greg-tool-passes* (+ *greg-tool-passes* 1))
                        (set! *greg-total-passes* (+ *greg-total-passes* 1))
                        (greg-vlog 1 "PASS: " *greg-case-name* "\n")
                        (set! testcase-return #t))
                      (begin
                        (set! *greg-tool-upasses* (+ *greg-tool-upasses* 1))
                        (set! *greg-total-upasses* (+ *greg-total-upasses* 1))
                        (if greg-posix
                            (greg-vlog 1 "PASS: " *greg-case-name* "\n")
                            (greg-log "UPASS: " *greg-case-name* "\n"))))
                  (if expecting
                      (begin
                        (set! *greg-tool-ufailures* (+ *greg-tool-ufailures* 1))
                        (set! *greg-total-ufailures* (+ *greg-total-ufailures* 1))
                        (greg-log "FAIL: " *greg-case-name* "\n"))
                      (begin
                        (set! *greg-tool-failures* (+ *greg-tool-failures* 1))
                        (set! *greg-total-failures* (+ *greg-total-failures* 1))
                        (if greg-posix
                            (greg-log "FAIL: " *greg-case-name* "\n")
                            (greg-vlog 1 "XFAIL: " *greg-case-name* "\n")))))
              (if (not testcase-handled)
                  (begin
                    (set! *greg-tool-unresolved* (+ *greg-tool-unresolved* 1))
                    (set! *greg-total-unresolved* (+ *greg-total-unresolved* 1))
                    (greg-log "UNRESOLVED: " *greg-case-name* "\n"))))
          (set! *greg-case-name* "")
          testcase-return))
      (begin
        (greg-log "ERROR: nested calls to greg-testcase are not permitted\n")
        #f)))

;;  This is the procedure to run all tests - it invokes the internal
;;  procedure 'greg-do-test' on all subdirectories in turn.
(define (greg-test-all home)
  (begin
    (if (eq? (length greg-tools) 1)
        (begin
          (greg-init (car greg-tools))
          (set! *greg-individual-test* #t))
        (begin
          (greg-init home)
          (set! *greg-individual-test* #f)))
    (let ((greg-old-home (getcwd))
         (ok-to-continue #t)
         (path (string-append home "/begin.grg")))
      (begin
        (if (and (access? path R_OK) (eqv? (stat:type (stat path)) 'regular))
            (catch #t
              (lambda ()
                (greg-vlog 3 "Loading " path " ...\n")
                (load (abspath path))
                (greg-vlog 3 "Loaded " path "\n"))
              (lambda key
                (begin
                  (set! *greg-total-unresolved* (+ *greg-total-unresolved* 1))
                  (greg-log "ERROR: Exception in " path ": " key "\n")
                  (set! ok-to-continue #f)
                  (if (eqv? (car key) 'quit)
                      (begin (greg-close-logs) (quit)))))))

        (if ok-to-continue
            (if (eq? greg-paths '())
                                        ; No paths specified - so we must be testing all the tool
                                        ; directories in our default directory
                (do
                    ((dir-list (greg-list-directories home) (cdr dir-list)))
                    ((eq? dir-list '()) (greg-summary))
                  (begin
                    (chdir home)
                    (greg-do-test (car dir-list))
                    (chdir greg-old-home)))
                                        ; Got some paths of specific tool files to run - so run them as
                                        ; a simulated 'tool'
                (begin
                  (greg-start-tool "testing files from greg-paths")
                  (chdir greg-old-home)
                  (do
                      ((file-list greg-paths (cdr file-list)))
                      ((eq? file-list '()) (begin (greg-end-tool) (greg-summary)))
                    (begin
                      (set! *greg-file-name* (car file-list))
                      (greg-vlog 2 "Running " *greg-file-name* " ...\n")
                      (catch #t
                        (lambda ()
                          (set! path *greg-file-name*)
                          (load (abspath path)))
                        (lambda key
                          (begin
                            (set! *greg-test-exceptions* (+ *greg-test-exceptions* 1))
                            (set! *greg-tool-unresolved* (+ *greg-tool-unresolved* 1))
                            (set! *greg-total-unresolved* (+ *greg-total-unresolved* 1))
                            (greg-log *greg-tool-name* " - " *greg-file-name*)
                            (greg-log " - Exception: " key "\n")
                            (if (eqv? (car key) 'quit)
                                (begin (greg-close-logs) (quit))))))
                      (greg-vlog 2 "Ended " *greg-file-name* "\n")))
                  (greg-end-tool))))

        (set! path (string-append home "/end.grg"))
        (if (and (access? path R_OK) (eqv? (stat:type (stat path)) 'regular))
            (catch #t
              (lambda ()
                (greg-vlog 3 "Loading " path " ...\n")
                (load (abspath path))
                (greg-vlog 3 "Loaded " path "\n"))
              (lambda key
                (begin
                  (set! *greg-total-unresolved* (+ *greg-total-unresolved* 1))
                  (greg-log "ERROR: Exception in " path ": " key "\n")
                  (if (eqv? (car key) 'quit)
                      (begin (greg-close-logs) (quit)))))))))
    (greg-reset)))

;;  This is the standard procedure for running all tests
(define (greg-test-run)
  (if (and (access? greg-src-dir R_OK) (access? greg-src-dir X_OK)
           (eqv? (stat:type (stat greg-src-dir)) 'directory))
      (greg-test-all greg-src-dir)
      (begin
        (greg-log "\nNo '" greg-src-dir "' directory found - using '.'.\n")
        (greg-test-all "."))))

;;        Start a group of tests.
(define (greg-start-tool name)
  (begin
    (greg-end-tool)
    (set! *greg-tool-name* name)
    (greg-vlog 2 "\n                 === " *greg-tool-name* " tests ===\n\n")
    (set! *greg-case-name* "")
    (set! *greg-tool-attempts* 0)
    (set! *greg-tool-passes* 0)
    (set! *greg-tool-failures* 0)
    (set! *greg-tool-upasses* 0)
    (set! *greg-tool-ufailures* 0)
    (set! *greg-tool-unresolved* 0)

    (set! *greg-tool-expect-timeout-proc* expect-timeout-proc)
    (set! expect-timeout-proc
          (lambda (arg)
            (begin
              (greg-dlog "timed out in expect after '" arg "'\n")
              (throw `fail))))
    (set! *greg-tool-expect-eof-proc* expect-eof-proc)
    (set! expect-eof-proc
          (lambda (arg)
            (begin
              (greg-dlog "end-of-file in expect after '" arg "'\n")
              (throw `fail))))
    (set! *greg-tool-expect-char-proc* expect-char-proc)
    (set! expect-char-proc greg-dlog)
    (set! expect-timeout *greg-tool-expect-timeout*)

    (set! *greg-num-tools* (+ *greg-num-tools* 1))))

;;        End a group of tests.
(define (greg-end-tool)
  (if (not (eqv? *greg-tool-name* ""))
      (begin
        (set! expect-timeout *greg-tool-expect-timeout*)
        (set! expect-timeout-proc *greg-tool-expect-timeout-proc*)
        (set! expect-eof-proc *greg-tool-expect-eof-proc*)
        (set! expect-char-proc *greg-tool-expect-char-proc*)
        (if (or (> greg-verbose 0) *greg-individual-test*)
            (begin
              (greg-vlog 2 "\n                === " *greg-tool-name* " Summary ===\n")
              (greg-log "\n# of testcases attempted   " *greg-tool-attempts*)
              (if (or (> greg-verbose 0) (> *greg-tool-passes* 0))
                  (greg-log "\n# of expected passes       " *greg-tool-passes*))
              (if (or (> greg-verbose 0) (> *greg-tool-failures* 0))
                  (greg-log "\n# of expected failures     " *greg-tool-failures*))
              (if (or (> greg-verbose 0) (> *greg-tool-upasses* 0))
                  (greg-log "\n# of unexpected passes     " *greg-tool-upasses*))
              (if (or (> greg-verbose 0) (> *greg-tool-ufailures* 0))
                  (greg-log "\n# of unexpected failures   " *greg-tool-ufailures*))
              (if (or (> greg-verbose 0) (> *greg-tool-unresolved* 0))
                  (greg-log "\n# of unresolved testcases  " *greg-tool-unresolved*))
              (if (or (> greg-verbose 0) (> *greg-tool-unsupported* 0))
                  (greg-log "\n# of unsupported testcases " *greg-tool-unsupported*))
              (if (or (> greg-verbose 0) (> *greg-tool-untested* 0))
                  (greg-log "\n# of untested testcases    " *greg-tool-untested*))
              (greg-log "\n")
              (greg-vlog 2 "\n")))
        (set! *greg-tool-name* ""))))

;; Restore expect settings from stack and return the restored settings.
(define greg-expect-pop
  (lambda ()
    (if (> (length *greg-expect-list*) 0)
        (let ((state (car *greg-expect-list*)))
          (set! expect-port state)
          (set! *greg-expect-list* (cdr *greg-expect-list*))
          state))))

;; Save current expect settings to stack.
(define greg-expect-push
  (lambda ()
    (set! *greg-expect-list* (cons expect-port *greg-expect-list*))))

(define greg-end-child
  (lambda ()
    (if (> (length *greg-child-info*) 1)
        (begin
          (greg-expect-pop)
          (if (input-port? (car *greg-child-info*))
              (close-input-port (car *greg-child-info*)))
          (if (output-port? (cadr *greg-child-info*))
              (close-output-port (cadr *greg-child-info*)))
          (if (> (length *greg-child-info*) 2)
              (waitpid (caddr *greg-child-info*)))
          (set! *greg-child-info* '())))))

(define greg-child
  (lambda args
    (greg-end-child)
                                        ; If we are given a list containing a list - grab the inner list.
    (if (list? (car args))
        (set! args (car args)))
    (let ((name (car args)))
      (begin
                                        ; If the program name is not nul, and does not begin with a slash -
                                        ; see if the program exists in the 'greg-obj-dir' directory and, if it
                                        ; does, arrange to run that executable.
        (if (not (string=? "" name))
            (begin
              (if (not (string=? "/" (substring name 0 1)))
                  (set! name (greg-obj-file name)))
              (if (and (access? name R_OK) (access? name X_OK)
                       (eqv? (stat:type (stat name)) 'regular))
                  (set! args (cons name (cdr args)))
                  (greg-log "Unable to find executable - " name "\n"))))
                                        ; Now try to launch the child process and set things up to talk to it.
        (set! *greg-child-info* (pty-child args))
        (if (> (length *greg-child-info*) 0)
            (if (input-port? (car *greg-child-info*))
                (begin
                  (greg-expect-push)
                  (set! expect-port (car *greg-child-info*))
                  *greg-child-info*)
                *greg-child-info*)
            (throw 'failed-to-create-child args))))))

;; Utility macros

;; Send a string as input to a child process.
(define greg-send 
  (lambda args
    (if (and (> (length *greg-child-info*) 1)
             (output-port? (cadr *greg-child-info*)))
        (for-each
         (lambda (arg)
           (display arg (cadr *greg-child-info*))
           (force-output (cadr *greg-child-info*)))
         args))))

;; Require a child process to produce output matching one of the
;; regular expressions in the list.
(defmacro-public greg-recv clauses
  `(expect-strings ,@clauses))

;; Shorthand for the most common sort of testcase - the test body
;; is expected to pass.
(defmacro-public greg-expect-pass (assertion body)
  `(greg-testcase ,assertion #t (lambda () ,body)))

;; And a test that is expected to fail.
(defmacro-public greg-expect-fail (assertion body)
  `(greg-testcase ,assertion #f (lambda () ,body)))
