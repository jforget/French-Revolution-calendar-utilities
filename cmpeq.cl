#!/usr/bin/clisp
;
; Comparing the equinox rule with Romme's arithmetic rule in the French Revolutionary calendar
; Comparaison de la règle des équinoxes avec la règle arithmétique de Romme dans le calendrier républicain
;
; Copyright (C) 2019, Jean Forget
;
; Author: Jean Forget
; Maintainer: Jean Forget
; Keywords: French Revolution, calendar
;
; This program is free software; you can redistribute it and modify
; it under the terms of Emacs:
; the GNU General Public License as published by
; the Free Software Foundation; version 3, or (at your option)
; any later version,
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, check the website of the Free Software
; Foundation, Inc., <https://www.fsf.org/>.
;
; This program relies upon another program by E. M. Reingold and N. Dershowitz,
; "calendrica-3.0.cl" available at
; https://github.com/espinielli/pycalcal
;
; See the license terms of "calendrica-3.0.cl" within the source file.
;
; The new version, published with the 4th edition of "Calendrical Calculations", is available in the "resources" tab of
; https://www.cambridge.org/ch/academic/subjects/computer-science/computing-general-interest/calendrical-calculations-ultimate-edition-4th-edition?format=PB&isbn=9781107683167#resources#e2jTvv2OqgtTRWik.97
;


(load "/home/jf/Documents/prog/lisp/calendrica-3.0.cl")

(defun comp-eq-arith (annee)
  (let* ((date-equ   (cc3:gregorian-from-fixed (cc3:fixed-from-french            (list annee 1 1))))
         (date-arith (cc3:gregorian-from-fixed (cc3:fixed-from-arithmetic-french (list annee 1 1))))
         (equ (caddr date-equ)) 
         (ari (caddr date-arith)) 
         (resultat   (cond ((eq equ ari) #\Space ) ((> equ ari) '+) ((< equ ari) '-))) )
     ;(print date-eq)
     (format t "~4D ~D ~D ~D ~A~%" annee (+ annee 1791) equ ari resultat)
  )
)
(let ((annee-max (parse-integer (or (car *args*) "20"))))
  (do ((annee 1 (+ annee 1)))
      ((> annee annee-max))
    (comp-eq-arith annee)
)
)
