;;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#shell-favorites

(require 'subr-x)

(defvar mp/remote-shell-fav-hosts (make-hash-table :test 'equal)
  "Table of host aliases for IPs or other actual references.")

(defun mp/remote-shell-fav-hosts-map ()
  "Returns the mapping between our simple names of our favorite
hosts and their IP address. If the map is empty, and the function
`mp/remote-shell-fav-hosts-get' has been defined, it calls that
function to populate the map prior to returning it. This may
return an empty map."
  (when (and #'mp/remote-shell-fav-hosts-get
             (hash-table-empty-p mp/remote-shell-fav-hosts))
    (mp/remote-shell-fav-hosts-get))
  mp/remote-shell-fav-hosts)

(defun mp/remote-shell-fav-hosts-list ()
  "Simply returns a list of known hosts from the cached map, or
populates it first if it is empty and the
`mp/remote-shell-fav-hosts-get' function has been defined."
  (hash-table-keys (mp/remote-shell-fav-hosts-map)))

(defun mp/remote-shell-tramp-connection (hostname &optional root directory)
  "Return a TRAMP connection string to HOSTNAME. If ROOT is
non-nil, returns an sudo compatible string."
  (when (null directory)
    (setq directory ""))
  ;; The ip address is either the value from a key in our cache, or whatever we pass in:
  (let ((ipaddr (gethash hostname (mp/remote-shell-fav-hosts-map) hostname)))
    (if root
        (format "/ssh:%s|sudo:%s:%s" ipaddr ipaddr directory)
      (format "/ssh:%s:%s"         ipaddr directory))))

(defun mp/remote-shell-fav-hosts-get ()
  "My hook to the mp/remote-shell processes in order to connect to
    my OpenStack controller, and create a hashtable of host names as
    the keys, and IP addresses as the values."
  (clrhash mp/remote-shell-fav-hosts)
  ;; iterate over numbers of available number crunchers and populate
  ;; the hash of server names/addresses
  (dolist (nr '(1 2 3 4 11))
    (let ((server (concat "bionc" (format "%02d" nr))))
      (puthash server server mp/remote-shell-fav-hosts)))
  (dolist (server '("candy" "taco" "wonton" "dimsum" "floss"))
    (puthash server server mp/remote-shell-fav-hosts)))

(defun mp/remote-shell (hostname &optional root)
  "Start an shell experience on HOSTNAME, that can be an alias to
a virtual machine from my 'cloud' server. With prefix command, opens
the shell as the root user account."
  (interactive
   (list (if (fboundp #'ido-completing-read)
             (ido-completing-read "Hostname: " (mp/remote-shell-fav-hosts-list))
           (completing-read "Hostname: " (mp/remote-shell-fav-hosts-list)))))
  (vterm)
  (vterm-send-string (concat "ssh " hostname))
  (vterm-send-return)
  (vterm-send-string "ts") ;; attach to a default tmux session
  (vterm-send-return)
  (rename-buffer (concat "*vterm-" hostname "*")))

(defun mp/remote-shell-command (hostname command
                                         &optional root bufname directory)
  "On HOSTNAME, run COMMAND (if the command ends with &, run
asynchronously). With a `C-u' prefix, run the command as ROOT.
When non-interactive, you can specify BUFNAME for the buffer's
name, and DIRECTORY where the command should run."
  (interactive
   (list (if #'completing-read
             (completing-read "Hostname: " (mp/remote-shell-fav-hosts-list))
           (completing-read "Hostname: " (mp/remote-shell-fav-hosts-list)))
         (read-string "Command: ")))
  (when (equal current-prefix-arg '(4))
    (setq root t))
  (let ((default-directory (mp/remote-shell-tramp-connection hostname root directory)))
    (shell-command command (mp/remote-shell-buffer-name hostname command bufname))))

(defun mp/ssh-tunnel (&optional port)
  "Start an ssh tunnel to a given HOSTNAME at a specified port."
  (interactive)
  (let* ((port (if (null current-prefix-arg) 9876 current-prefix-arg))
         (hostname (completing-read "Hostname: " (mp/remote-shell-fav-hosts-list)))
         (cmd (format "ssh -N -f -L localhost:%s:localhost:%s %s" port port hostname)))
    (call-process-shell-command cmd)))
