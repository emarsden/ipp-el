;;; test-ipp.el --- Testing code for ipp.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Eric Marsden

;; Author: Eric Marsden <eric.marsden@risk-engineering.org>
;; Keywords:



(require 'cl-lib)


(defun check-fixture (filename)
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (let ((reply (make-ipp-reply)))
      (setf (ipp-reply-operation-id reply) (ipp-demarshal-int 2))
      (setf (ipp-reply-status reply) (ipp-demarshal-int 2))
      (setf (ipp-reply-request-id reply) (ipp-demarshal-int 4))
      (ipp-demarshal-attributes reply)
      reply)))

    ;; can't use this because the fixtures don't include the HTTP header
    ;; (ipp-decode-reply (current-buffer))


(defun check-fixtures ()
  (cl-loop for fixture in (directory-files "fixtures/")
           do (check-fixture fixture)))


