;; NOTE: Binary was invalid when compiling with sb-ext:save-lisp-and-die but works with
;; (setq uiop:*image-entry-point* #'slickposes:main)
;; (uiop:dump-image "slickposes" :executable t :compression 10)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload 'clingon :silent t)
  (ql:quickload 'bordeaux-threads :silent t)
  (ql:quickload 'str :silent t)
  (ql:quickload 'cl-gtk4 :silent t)
  (ql:quickload 'cl-gtk4.adw :silent t))


(defpackage slickposes
  (:use #:cl #:gtk4)
  (:export #:main))

(in-package #:slickposes)

(defvar auto-advance-seconds 60.0d0)
(defvar current-duration-seconds 0.0d0)
(defvar advancement-paused-p nil)
(defvar source-directory ".")

(defun nshuffle (sequence)
  (declare (type list sequence))
  (declare (optimize (speed 3) (safety 0)))
  (setf *random-state* (make-random-state t))
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun get-image-files-in-path (path-string)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type string path-string))
  (remove-if-not (lambda (s)
                   (or
                    (str:ends-with-p ".jpg" (uiop:unix-namestring s) :ignore-case str:*ignore-case*)
                    (str:ends-with-p ".png" (uiop:unix-namestring s) :ignore-case str:*ignore-case*)))
                 (uiop:directory-files path-string)))

(define-application (:name slickposes
                     :id "org.harvlin.slickposes")
  (define-main-window (window (adw:make-application-window :app *application*))
    (let ()

      (let ((box (make-box :orientation +orientation-vertical+
                           :spacing 4)))

        (setf (adw:window-content window) box)

        (let* ((header-bar (adw:make-header-bar))
               (image-files (get-image-files-in-path source-directory))
               (button-box (make-box :orientation +orientation-horizontal+
                                     :spacing 4))
               (skip-button (make-button :label "Skip"))
               (pause-switch-row (adw:make-switch-row))
               (progress-bar (make-progress-bar))
               (image-carousel (adw:make-carousel))
               (carousel-indicator (adw:make-carousel-indicator-dots))
               (scroll-to-next-image
                 (lambda ()
                   (unless (null image-files)
                     (let ((image (gtk4:make-image :filename (pop image-files))))
                       (setf (widget-hexpand-p image) t
                             (widget-vexpand-p image) t)
                       (adw:carousel-append image-carousel image)
                       (sleep 0.1) ; Since the run-in-main-event-loop macro is broken, wait a bit after adding image to make sure the subsequent scroll-to goes through
                       (adw:carousel-scroll-to image-carousel image t))
                     (when (null image-files)
                       (gtk4:widget-remove-css-class skip-button "suggested-action"))))))

          (nshuffle image-files)
          (setf image-files
                (mapcar
                 #'(lambda (p)
                     (uiop:unix-namestring p))
                 image-files))

          (connect skip-button "clicked"
                   (lambda (button)
                     (declare (ignore button))
                     (funcall scroll-to-next-image)
                     (setf current-duration-seconds 0.0d0)))
          (connect pause-switch-row "notify"
                   (lambda (switch param)
                     (declare (ignore param))
                     (setf advancement-paused-p (adw:switch-row-active-p switch))))

          (bt:make-thread
           (lambda ()
             (declare (optimize (speed 3) (safety 0)))
             (declare (type double-float current-duration-seconds auto-advance-seconds))
             (loop while (not (null image-files))
                   do (loop while (< current-duration-seconds auto-advance-seconds)
                            do (progn (sleep 0.05)
                                      (if advancement-paused-p
                                          (progress-bar-pulse progress-bar)
                                          (setf current-duration-seconds (+ current-duration-seconds 0.05d0)
                                                (progress-bar-fraction progress-bar) (/ current-duration-seconds auto-advance-seconds)))))
                      (funcall scroll-to-next-image)
                      (setf current-duration-seconds 0.0d0))))

          (setf (adw:preferences-row-title pause-switch-row) "Pause")

          (setf (gtk4:widget-hexpand-p button-box) t
                (gtk4:widget-halign button-box) gtk4:+align-fill+)

          (setf (gtk4:widget-css-classes skip-button) '("pill" "suggested-action")
                (gtk4:widget-margin-all skip-button) 10
                (gtk:widget-hexpand-p skip-button) t)

          (setf (adw:header-bar-title-widget header-bar)
                (adw:make-window-title :title "slickposes" :subtitle ""))

          (setf (adw:carousel-interactive-p image-carousel) nil)

          (setf (adw:carousel-indicator-dots-carousel carousel-indicator) image-carousel)

          (unless (null image-files)
            (let ((i (gtk4:make-image :filename (pop image-files))))
              (setf (widget-hexpand-p i) t
                    (widget-vexpand-p i) t)
              (adw:carousel-append image-carousel i)
              (adw:carousel-scroll-to image-carousel i t)))

          (box-append button-box skip-button)

          ;; (box-append box header-bar)
          (box-append box pause-switch-row)
          (box-append box image-carousel)
          (box-append box button-box)
          (box-append box carousel-indicator)
          (box-append box progress-bar)))

      (unless (widget-visible-p window)
        (window-present window)))))

(defun cli/options ()
  "Returns a list of options for the main cli command."
  (list
   (clingon:make-option
    :integer
    :description "Duration of the auto advance timer"
    :short-name #\d
    :long-name "duration"
    :initial-value 60
    :key :duration)))

(defun cli/handler (cmd)
  "Handler function for the main cli command"
  (let ((free-args (clingon:command-arguments cmd))
        (duration (clingon:getopt cmd :duration)))
    (setf auto-advance-seconds (* duration 1.0d0)
          source-directory (if (not (null free-args)) (first free-args) (string "."))))
  (unless (adw:initialized-p)
    (adw:init))
  (slickposes))

(defun cli/command ()
  "The main cli command."
  (clingon:make-command
   :name "slickposes"
   :description "A simple GTK based figure drawing program written in Common Lisp"
   :version "0.1.0"
   :authors '("Harvey Lin")
   :license "LGPLv3"
   :options (cli/options)
   :handler #'cli/handler))

(defun main ()
  (clingon:run (cli/command)))
