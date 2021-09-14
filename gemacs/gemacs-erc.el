;; IRC
;; TLS configuration to use openssl.
;; Certificate setup: https://libera.chat/guides/certfp
;; use M-x erc-tls to enter server details manually
;; use M-x gemacs/erc-connect-libera to connect to existing libera account for mathsboy
;; See also https://github.com/bzg/dotemacs/blob/master/emacs.org#erc

(setq tls-program '(
    "openssl s_client -connect %h:%p -no_ssl2 -ign_ -cert /home/jupiter/.irc/libera.pem"
    "gnutls-cli --x509cafile %t -p %p %h"
    "gnutls-cli --x509cafile %t -p %p %h --protocols ssl3"
    ))

(use-package erc
  :config
  (require 'erc-services)
  (setq erc-modules '(autoaway autojoin irccontrols log netsplit noncommands
		      notify pcomplete completion ring services stamp
		      track truncate))
  )


(defun gemacs/erc-connect-libera ()
    "Connect to Libera server with ERC."
    (interactive)
    (erc-tls :server "irc.libera.chat"
		:port 6697
		:nick "mathsboy"
		
		))

