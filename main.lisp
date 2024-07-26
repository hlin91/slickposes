;; NOTE: Binary was invalid when compiling with sb-ext:save-lisp-and-die but works with
;; (setq uiop:*image-entry-point* #'fastposes:main :compression 10)
;; (uiop:dump-image "fastposes" :executable t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload 'clingon :silent t)
  (ql:quickload 'bordeaux-threads :silent t)
  (ql:quickload 'str :silent t)
  (ql:quickload 'cl-gtk4 :silent t)
  (ql:quickload 'cl-gtk4.adw :silent t))


(defpackage fastposes
  (:use #:cl #:gtk4)
  (:export #:main))

(in-package #:fastposes)

(defvar auto-advance-seconds 60)
(defvar advancement-paused-p nil)
(defvar source-directory ".")

(defun get-image-files-in-path (path-string)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type string path-string))
  (remove-if-not (lambda (s)
                   (or
                    (str:ends-with-p ".jpg" (uiop:unix-namestring s) :ignore-case str:*ignore-case*)
                    (str:ends-with-p ".png" (uiop:unix-namestring s) :ignore-case str:*ignore-case*)))
                 (uiop:directory-files path-string)))

(define-application (:name fastposes
                     :id "org.harvlin.fastposes")
  (define-main-window (window (adw:make-application-window :app *application*))
    (let ()

      (let ((box (make-box :orientation +orientation-vertical+
                           :spacing 4)))

        (setf (adw:window-content window) box)

        (let* ((header-bar (adw:make-header-bar))
               (images (mapcar #'(lambda (s)
                                   (declare (optimize (speed 3) (safety 0)))
                                   (declare (type pathname s))
                                   (gtk4:make-image :filename (uiop:unix-namestring s)))
                               (get-image-files-in-path source-directory)))
               (button-box (make-box :orientation +orientation-horizontal+
                                     :spacing 4))
               (skip-button (make-button :label "Skip"))
               (pause-switch-row (adw:make-switch-row))
               (progress-bar (make-progress-bar)) ; TODO: Hook up progress bar
               (image-carousel (adw:make-carousel))
               (carousel-indicator (adw:make-carousel-indicator-dots))
               (scroll-to-next-image
                 (lambda (button)
                   (declare (ignore button))
                   (unless (or (null images) (null (cdr images)))
                     (setf images (cdr images))
                     (when (null (cdr images))
                       (gtk4:widget-remove-css-class skip-button "suggested-action"))
                     (adw:carousel-scroll-to image-carousel (car images) t)))))

          (connect skip-button "clicked" scroll-to-next-image)
          (connect pause-switch-row "notify"
                   (lambda (switch param)
                     (declare (ignore param))
                     (setf advancement-paused-p (adw:switch-row-active-p switch))))

          (bt:make-thread
           (lambda ()
             (loop while (not (and (null images) (null (cdr images))))
                   do (let ((curr-duration 0.0d0))
                        (loop while (< curr-duration auto-advance-seconds)
                              do (progn (sleep 0.05)
                                        (if advancement-paused-p
                                            (progress-bar-pulse progress-bar)
                                            (setf curr-duration (+ curr-duration 0.10)
                                                  (progress-bar-fraction progress-bar) (/ curr-duration auto-advance-seconds)))))
                        (funcall scroll-to-next-image nil)
                        (setf curr-duration 0)))))

          (setf (adw:carousel-indicator-dots-carousel carousel-indicator) image-carousel
                (adw:carousel-interactive-p image-carousel) nil)

          (setf (adw:preferences-row-title pause-switch-row) "Pause")

          (setf (gtk4:widget-hexpand-p button-box) t
                (gtk4:widget-halign button-box) gtk4:+align-fill+)

          (setf (gtk4:widget-css-classes skip-button) '("pill" "suggested-action")
                (gtk4:widget-margin-all skip-button) 10
                (gtk:widget-hexpand-p skip-button) t)

          (setf (adw:header-bar-title-widget header-bar)
                (adw:make-window-title :title "fastposes" :subtitle ""))

          (mapcar #'(lambda (i)
                      (setf (widget-hexpand-p i) t
                            (widget-vexpand-p i) t)
                      (adw:carousel-append image-carousel i))
                  images)

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
    (setf auto-advance-seconds duration
          source-directory (if (not (null free-args)) (first free-args) (string "."))))
  (unless (adw:initialized-p)
    (adw:init))
  (fastposes))

(defun cli/command ()
  "The main cli command."
  (clingon:make-command
   :name "fastposes"
   :description "A simple GTK based figure drawing program"
   :version "0.1.0"
   :authors '("Harvey Lin")
   :license "LGPLv3"
   :options (cli/options)
   :handler #'cli/handler))

(defun main ()
  (clingon:run (cli/command)))
