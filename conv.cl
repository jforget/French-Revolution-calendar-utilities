#!/usr/bin/clisp
;
; Conversion from Gregorian to French Revolutionary using the astronomical rule (equinox)
; Conversion du calendrier grégorien vers le calendrier républicain avec la règle astronomique des équinoxes
;
; Copyright (C) 2018, Jean Forget
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
; along with GNU Emacs; see the file COPYING. If not, see <http://www.gnu.org/licenses/>.
;
; This program relies upon another program by E. M. Reingold and N. Dershowitz,
; "calendrica-3.0.cl".
;
; See the license terms of "calendrica-3.0.cl" in this file, available in the "resources" tab of
; http://www.cambridge.org/ch/academic/subjects/computer-science/computing-general-interest/calendrical-calculations-3rd-edition#resources#e2jTvv2OqgtTRWik.97
; also available at
; https://github.com/espinielli/pycalcal
;

(load "calendrica-3.0.cl")
(let ((year  (parse-integer (car   *args*) ) )
      (month (parse-integer (cadr  *args*) ) )
      (day1  (parse-integer (caddr *args*) ) ))
    (let ((fixed  (cc3:fixed-from-gregorian (cc3:gregorian-date year month day1))))
         (print (cc3:french-from-fixed fixed))
))

