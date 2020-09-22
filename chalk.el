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

;;
;; (@* "Constant" )
;;

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

;;
;; (@* "Util" )
;;

(defun chalk--set-prop (plist prop val)
  "Set PLIST by PROP and VAL; then return it."
  (when val (setq plist (plist-put plist prop val))) plist)

;;
;; (@* "Log" )
;;

(defun chalk-log-red (format-string &rest args)
  "Log message with color red.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (apply 'chalk--log (list (apply 'format format-string args)
                           :foreground chalk-red)))

(defun chalk-log (format-string &rest args)
  "Basic chalk message.
See `message' function's description for arguments FORMAT-STRING and ARGS."
  (apply 'chalk--log (list (apply 'format format-string args))))

;;
;; (@* "Color" )
;;

(defun chalk-black (string)
  "Propertize STRING with black color."
  (chalk string :foreground chalk-black))

(defun chalk-white (string)
  "Propertize STRING with white color."
  (chalk string :foreground chalk-white))

(defun chalk-red (string)
  "Propertize STRING with red color."
  (chalk string :foreground chalk-red))

(defun chalk-green (string)
  "Propertize STRING with green color."
  (chalk string :foreground chalk-green))

(defun chalk-blue (string)
  "Propertize STRING with blue color."
  (chalk string :foreground chalk-blue))

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
    (apply 'message
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
    (propertize string 'face prop)))

(provide 'chalk)
;;; chalk.el ends here
