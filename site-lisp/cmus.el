;;; cmus.el --- A very simple interface to cmus, useable locally and remotely via ssh.
;------------------------------------------------------------------------------
;; Remote control of cmus audio player
;; Copyright (C) 2015-2017, Winfried S. Dietmayer ---==WDI==---

;; Author: Winfried S. Dietmayer ---==WDI==--- <winfried@sunkiddance.de>
;; Version: 0.2
;; Keywords: multimedia, audio player remote control, cmus, ssh, elisp

;; cmus.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; cmus.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cmus.el.  If not, see <http://www.gnu.org/licenses/>.
;;
;; To summarize: This is *free* but copyrighted software,
;; *free* as in *free*dom or *free* will.

;;; Commentary:
;; Overview
;; --------
;; cmus.el provides a simple interface for cmus, the C* music player.
;; It allows to control the basics, i.e. start, stop, pause, previous track,
;; next track, status of cmus player, lyrics for the currently playing track
;; and a possibility to look up the lyrics of a song, given artist and title
;; of the song.
;; It provides this basic functionality either on the local machine with cmus
;; and Emacs running, or on a remote machine issuing the cmus commands via ssh.
;; The main scenario is to remote control cmus from within a virtual machine
;; running on the local machine.  You don't have to leave the virtual machine
;; in order to control cmus.  No distractions only to mute the sound or to go
;; on to the next track.
;;
;; Prerequisites
;;--------------
;; * Local machine
;; ** cmus installed, any version will do
;; ** sshd installed if you plan to use it remotely, i.e. from a virtual machine
;; ** Script to retrieve the lyrics from the internet, say ~/bin/lyrics-cmus.sh
;; * Remote machine
;; ** ssh public-key authentication to localhost configured
;; 
;; Configuration of cmus
;; ---------------------
;; On cmus command line:
;; :set status_display_program=/path/to/program/cmus-artist-title.sh
;; To make this permanent, add the line above to your ~/.cmus/rc
;; cmus-artist-title.sh writes the current cmus status
;; (artist, track title, etc.) to a defined file, say ~/cmus-status.txt.
;;
;; Configuration of cmus.el
;; ------------------------
;; cmus.el support the Emacs Custom interface invoked with 'M-x customize'.
;; cmus is located in the subgroups 'Application/cmus' and 'Multimedia/cmus', respectively.
;; The entries there are self-explanatory, really, believe me 8-) .
;; The parameter 'Cmus Lyrics Command' is the full path to a script/program which retrieves
;; the lyrics of the currently playing track.  It reads this information from cmus-status.txt.
;; Alternatively, the script shall accept two parameters, 'artist' and 'title',
;; for example 'Beatles yesterday'.
;;
;; cmus.el buffer
;; --------------
;; Messages returned to Emacs are logged into buffer *CMUS-Shell-Output*.
;; If things go awry, look there for a clue what went wrong.
;;
;; Configuration of ~/.emacs
;; -------------------------
;; In order to load cmus.el at startup, add the following line to ~/.emacs:
;; (load-file "~/bin/emacs/cmus.el")

;------------------------------------------------------------------------------
;;; Code:
(defgroup cmus ()
  "Very simple remote and local client for the C* music player (cmus)."
  :prefix "cmus-"
  :group 'multimedia
  :group 'applications)
(defcustom
  cmus-remote-ssh-port ""
  "The port number sshd on remote *cmus* host is listening to. Leave blank if invoked locally."
  :type 'string)
(defcustom
  cmus-remote-ssh-login ""
  "The ssh user and remote *cmus* host name to login, f.e. <user@192.168.1.1>. Leave blank if invoked locally."
  :type 'string)
(defcustom
  cmus-local-ssh-command ""
  "The *local* ssh command to connect to remote *cmus* host. Leave blank if invoked locally."
  :type 'string)
(defcustom
  cmus-local-ssh-parameter ""
  "The *local* ssh command parameters to connect to remote *cmus* host. Note that the last parameter has to be -p.
Leave blank if invoked locally."
  :type 'string)
(defcustom
  cmus-command "/opt/local/bin/cmus-remote"
  "The command to control *cmus* locally or remotely."
  :type 'string)
(defcustom
  cmus-lyrics-command "/Users/winfried/bin/lyrics-cmus.sh"
  "The *cmus* command to retrieve the lyrics."
  :type 'string)
;; ------------------------------------------------------------------------------
; cmus helper functions
(defun ssh-command-string ()
"The complete shell command string to control *cmus*."
  (mapconcat 'identity
	     (list cmus-local-ssh-command
		   cmus-local-ssh-parameter
		   cmus-remote-ssh-port
		   cmus-remote-ssh-login
		   cmus-command)
	     " "))

(defun ssh-command-lyrics-string ()
"The complete shell command string to retrieve lyrics."
  (mapconcat 'identity
	     (list cmus-local-ssh-command
		   cmus-local-ssh-parameter
		   cmus-remote-ssh-port
		   cmus-remote-ssh-login
		   cmus-lyrics-command)
	     " "))
;; ------------------------------------------------------------------------------
(defun cmus-play ()
"Start playing the current song in cmus."
  (interactive)
  (start-process-shell-command "*CMUS-Shell-Play*" "*CMUS-Shell-Output*" (concat (ssh-command-string) " -p")))

(defun cmus-pause ()
"Pause playing the current song in cmus."
  (interactive)
  (start-process-shell-command "*CMUS-Shell-Pause*" "*CMUS-Shell-Output*" (concat (ssh-command-string) " -u")))

(defun cmus-stop ()
"Stop playing the current song in cmus."
  (interactive)
  (start-process-shell-command "*CMUS-Shell-Stop*" "*CMUS-Shell-Output*" (concat (ssh-command-string) " -s")))

(defun cmus-vol (volume)
"Set the VOLUME in cmus.
The complete possible command is as follows: [+-]NUM[%].
NUM is a number in (0..100)."
  (interactive "MVolume:(0..100):")
  (start-process-shell-command "*CMUS-Shell-Vol*" "*CMUS-Shell-Output*" (concat (ssh-command-string) " -v" volume)))

(defun cmus-next ()
"Play the next song in cmus."
  (interactive)
  (start-process-shell-command "*CMUS-Shell-Next*" "*CMUS-Shell-Output*" (concat (ssh-command-string) " -n")))

(defun cmus-previous ()
"Play the previous song in cmus."
  (interactive)
  (start-process-shell-command "*CMUS-Shell-Previous*" "*CMUS-Shell-Output*" (concat (ssh-command-string) " -r")))

(defun cmus-status ()
"Show the current state of cmus."
  (interactive)
  (switch-to-buffer (create-file-buffer "*CMUS Status*"))
  (text-mode)
  (insert (shell-command-to-string (concat (ssh-command-string) " -Q")))
  (goto-char (point-min))
  (view-mode))
;------------------------------------------------------------------------------

(provide 'cmus)

;;; cmus.el ends here
