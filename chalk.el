;;; chalk.el --- Log message cleanly with color on it  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-09-22 16:47:44

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Log message cleanly with color on it.
;; Keyword: color log message text string
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs-elpa/chalk

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Log message cleanly with color on it.
;;

;;; Code:

(defgroup chalk nil
  "Log message cleanly with color on it."
  :prefix "chalk-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/chalk"))

(defcustom chalk-disable-log nil
  "When non-nil, no log can be output."
  :type 'boolean
  :group 'chalk)

(defcustom chalk-flush nil
  "When non-nil, the color will be preserved in *Message* buffer."
  :type 'boolean
  :group 'chalk)

;;
;; (@* "Constant" )
;;

(defconst chalk-normal 'normal)

(defconst chalk-black "black")
(defconst chalk-white "white")
(defconst chalk-red "red")
(defconst chalk-green "green")
(defconst chalk-blue "blue")
(defconst chalk-orange "#FFA500")
(defconst chalk-yellow "yellow")
(defconst chalk-cyan "cyan")
(defconst chalk-violet "#EE82EE")
(defconst chalk-olive "#808000")
(defconst chalk-pink "#FFC0CB")
(defconst chalk-silver "#C0C0C0")
(defconst chalk-sky-blue "#87CEEB")
(defconst chalk-purple "#800080")

(defconst chalk-ultra-bold 'ultra-bold)
(defconst chalk-extra-bold 'extra-bold)
(defconst chalk-bold 'bold)
(defconst chalk-semi-bold 'semi-bold)

(defconst chalk-semi-light 'semi-light)
(defconst chalk-light 'light)
(defconst chalk-extra-light 'extra-light)
(defconst chalk-ultra-light 'ultra-light)

(defconst chalk-italic 'italic)
(defconst chalk-oblique 'oblique)
(defconst chalk-reverse-italic 'reverse-italic)
(defconst chalk-reverse-oblique 'reverse-oblique)

(defconst chalk-underline t)
(defconst chalk-overline t)
(defconst chalk-strike-through t)

(defconst chalk-inverse-video t)

(defconst chalk-inherit 'default)

;;
;; (@* "Util" )
;;

(defun chalk--set-prop (plist prop val)
  "Set PLIST by PROP and VAL; then return it."
  (when val (setq plist (plist-put plist prop val))) plist)

(defun chalk--message (format-string &rest args)
  "Acts like `message' but preserves string properties in the *Messages* buffer.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (let ((message-log-max nil)) (apply 'message format-string args))
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (unless (zerop (current-column)) (insert "\n"))
        (insert (apply 'format format-string args))
        (insert "\n")))))

;;
;; (@* "Log" )
;;

(defun chalk-log (format-string &rest args)
  "Basic chalk message.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-normal (apply 'format format-string args))))

(defun chalk-log-black (format-string &rest args)
  "Log message with color black.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-black (apply 'format format-string args))))

(defun chalk-log-white (format-string &rest args)
  "Log message with color white.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-white (apply 'format format-string args))))

(defun chalk-log-red (format-string &rest args)
  "Log message with color red.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-red (apply 'format format-string args))))

(defun chalk-log-green (format-string &rest args)
  "Log message with color green.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-green (apply 'format format-string args))))

(defun chalk-log-blue (format-string &rest args)
  "Log message with color blue.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-blue (apply 'format format-string args))))

(defun chalk-log-orange (format-string &rest args)
  "Log message with color orange.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-orange (apply 'format format-string args))))

(defun chalk-log-yellow (format-string &rest args)
  "Log message with color yellow.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-yellow (apply 'format format-string args))))

(defun chalk-log-cyan (format-string &rest args)
  "Log message with color cyan.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-cyan (apply 'format format-string args))))

(defun chalk-log-violet (format-string &rest args)
  "Log message with color violet.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-violet (apply 'format format-string args))))

(defun chalk-log-olive (format-string &rest args)
  "Log message with color olive.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-olive (apply 'format format-string args))))

(defun chalk-log-pink (format-string &rest args)
  "Log message with color pink.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-pink (apply 'format format-string args))))

(defun chalk-log-silver (format-string &rest args)
  "Log message with color silver.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-silver (apply 'format format-string args))))

(defun chalk-log-sky-blue (format-string &rest args)
  "Log message with color sky blue.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-sky-blue (apply 'format format-string args))))

(defun chalk-log-purple (format-string &rest args)
  "Log message with color purple.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-purple (apply 'format format-string args))))

(defun chalk-log-ultra-bold (format-string &rest args)
  "Log message with weight ultra-bold.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-ultra-bold (apply 'format format-string args))))

(defun chalk-log-extra-bold (format-string &rest args)
  "Log message with weight extra-bold.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-extra-bold (apply 'format format-string args))))

(defun chalk-log-bold (format-string &rest args)
  "Log message with weight bold.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-bold (apply 'format format-string args))))

(defun chalk-log-semi-bold (format-string &rest args)
  "Log message with weight semi-bold.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-semi-bold (apply 'format format-string args))))

(defun chalk-log-semi-light (format-string &rest args)
  "Log message with weight semi-light.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-semi-light (apply 'format format-string args))))

(defun chalk-log-light (format-string &rest args)
  "Log message with weight light.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-light (apply 'format format-string args))))

(defun chalk-log-extra-light (format-string &rest args)
  "Log message with weight extra-light.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-extra-light (apply 'format format-string args))))

(defun chalk-log-ultra-light (format-string &rest args)
  "Log message with weight ultra-light.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-ultra-light (apply 'format format-string args))))

(defun chalk-log-italic (format-string &rest args)
  "Log message with slant italic.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-italic (apply 'format format-string args))))

(defun chalk-log-oblique (format-string &rest args)
  "Log message with slant oblique.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-oblique (apply 'format format-string args))))

(defun chalk-log-reverse-italic (format-string &rest args)
  "Log message with slant reverse-italic.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-reverse-italic (apply 'format format-string args))))

(defun chalk-log-reverse-oblique (format-string &rest args)
  "Log message with slant reverse-oblique.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-reverse-oblique (apply 'format format-string args))))

(defun chalk-log-underline (format-string &rest args)
  "Log message with underline.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-underline (apply 'format format-string args))))

(defun chalk-log-overline (format-string &rest args)
  "Log message with overline.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-overline (apply 'format format-string args))))

(defun chalk-log-strike-through (format-string &rest args)
  "Log message with strike-through.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-strike-through (apply 'format format-string args))))

(defun chalk-log-inverse-video (format-string &rest args)
  "Log message with inverse-video.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-inverse-video (apply 'format format-string args))))

(defun chalk-log-inherit (format-string &rest args)
  "Log message with inherit.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (chalk--log (chalk-inherit (apply 'format format-string args))))

;;
;; (@* "Color" )
;;

(defun chalk-normal (string)
  "Propertize STRING with everything default."
  (chalk string))

(defun chalk-black (string)
  "Propertize STRING with color black."
  (chalk string :foreground chalk-black))

(defun chalk-white (string)
  "Propertize STRING with color white."
  (chalk string :foreground chalk-white))

(defun chalk-red (string)
  "Propertize STRING with red color."
  (chalk string :foreground chalk-red))

(defun chalk-green (string)
  "Propertize STRING with color green."
  (chalk string :foreground chalk-green))

(defun chalk-blue (string)
  "Propertize STRING with color blue."
  (chalk string :foreground chalk-blue))

(defun chalk-orange (string)
  "Propertize STRING with color orange."
  (chalk string :foreground chalk-orange))

(defun chalk-yellow (string)
  "Propertize STRING with color yellow."
  (chalk string :foreground chalk-yellow))

(defun chalk-cyan (string)
  "Propertize STRING with color cyan."
  (chalk string :foreground chalk-cyan))

(defun chalk-violet (string)
  "Propertize STRING with color violet."
  (chalk string :foreground chalk-violet))

(defun chalk-olive (string)
  "Propertize STRING with color olive."
  (chalk string :foreground chalk-olive))

(defun chalk-pink (string)
  "Propertize STRING with color pink."
  (chalk string :foreground chalk-pink))

(defun chalk-silver (string)
  "Propertize STRING with color silver."
  (chalk string :foreground chalk-silver))

(defun chalk-sky-blue (string)
  "Propertize STRING with color sky-blue."
  (chalk string :foreground chalk-sky-blue))

(defun chalk-purple (string)
  "Propertize STRING with color purple."
  (chalk string :foreground chalk-purple))

(defun chalk-ultra-bold (string)
  "Propertize STRING with weight ultra-bold."
  (chalk string :weight 'ultra-bold))

(defun chalk-extra-bold (string)
  "Propertize STRING with weight extra-bold."
  (chalk string :weight 'extra-bold))

(defun chalk-bold (string)
  "Propertize STRING with weight bold."
  (chalk string :weight 'bold))

(defun chalk-semi-light (string)
  "Propertize STRING with weight semi-light."
  (chalk string :weight 'semi-light))

(defun chalk-light (string)
  "Propertize STRING with weight light."
  (chalk string :weight 'light))

(defun chalk-extra-light (string)
  "Propertize STRING with weight extra-light."
  (chalk string :weight 'extra-light))

(defun chalk-ultra-light (string)
  "Propertize STRING with weight ultra-light."
  (chalk string :weight 'ultra-light))

(defun chalk-italic (string)
  "Propertize STRING with slant italic."
  (chalk string :slant 'italic))

(defun chalk-oblique (string)
  "Propertize STRING with slant oblique."
  (chalk string :slant 'oblique))

(defun chalk-reverse-italic (string)
  "Propertize STRING with slant italic."
  (chalk string :slant 'reverse-italic))

(defun chalk-reverse-oblique (string)
  "Propertize STRING with slant italic."
  (chalk string :slant 'reverse-oblique))

(cl-defun chalk-underline (string &key underline)
  "Propertize STRING with underline."
  (unless underline (setq underline chalk-underline))
  (chalk string :underline underline))

(cl-defun chalk-overline (string &key overline)
  "Propertize STRING with overline."
  (unless overline (setq overline chalk-overline))
  (chalk string :overline overline))

(cl-defun chalk-strike-through (string &key strike-through)
  "Propertize STRING with strike-through."
  (unless strike-through (setq strike-through chalk-strike-through))
  (chalk string :strike-through strike-through))

(cl-defun chalk-inverse-video (string &key inverse-video)
  "Propertize STRING with inverse-video."
  (unless inverse-video (setq inverse-video chalk-inverse-video))
  (chalk string :inverse-video inverse-video))

(cl-defun chalk-inherit (string &key inherit)
  "Propertize STRING with inverse-video."
  (unless inherit (setq inherit chalk-inherit))
  (chalk string :inherit inherit))

;;
;; (@* "Core" )
;;

(cl-defun chalk--log (string &key
                             family foundry width height weight slant distant-foreground
                             foreground background underline overline strike-through
                             box inverse-video stipple font inherit)
  "Message STRING with keys.

See `propertize' function's description for arguments FAMILY, FOUNDRY, WIDTH,
HEIGHT, WEIGHT, SLANT, DISTANT-FOREGROUND, FOREGROUND, BACKGROUND, UNDERLINE,
OVERLINE, STRIKE-THROUGH, BOX, INVERSE-VIDEO, STIPPLE, FONT and INHERIT"
  (unless chalk-disable-log
    (apply (if chalk-flush #'chalk--message #'message)
           (list (chalk string
                        :family family :foundry foundry :width width :height height
                        :weight weight :slant slant
                        :distant-foreground distant-foreground
                        :foreground foreground :background background
                        :underline underline :overline overline
                        :strike-through strike-through
                        :box box :inverse-video inverse-video :stipple stipple
                        :font font :inherit inherit)))))

(cl-defun chalk (string &key
                        family foundry width height weight slant distant-foreground
                        foreground background underline overline strike-through
                        box inverse-video stipple font inherit)
  "Propertize STRING with keys.

See `propertize' function's description for arguments FAMILY, FOUNDRY, WIDTH,
HEIGHT, WEIGHT, SLANT, DISTANT-FOREGROUND, FOREGROUND, BACKGROUND, UNDERLINE,
OVERLINE, STRIKE-THROUGH, BOX, INVERSE-VIDEO, STIPPLE, FONT and INHERIT"
  (let ((prop '()))
    (setq prop (chalk--set-prop prop :family family)
          prop (chalk--set-prop prop :foundry foundry)
          prop (chalk--set-prop prop :width width)
          prop (chalk--set-prop prop :height height)
          prop (chalk--set-prop prop :weight weight)
          prop (chalk--set-prop prop :slant slant)
          prop (chalk--set-prop prop :distant-foreground distant-foreground)
          prop (chalk--set-prop prop :foreground foreground)
          prop (chalk--set-prop prop :background background)
          prop (chalk--set-prop prop :underline underline)
          prop (chalk--set-prop prop :overline overline)
          prop (chalk--set-prop prop :strike-through strike-through)
          prop (chalk--set-prop prop :box box)
          prop (chalk--set-prop prop :inverse-video inverse-video)
          prop (chalk--set-prop prop :stipple stipple)
          prop (chalk--set-prop prop :font font)
          prop (chalk--set-prop prop :inherit inherit))
    (if prop (propertize string 'face prop) string)))

(provide 'chalk)
;;; chalk.el ends here
