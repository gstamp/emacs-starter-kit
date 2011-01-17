;(insert (prin1-to-string (x-select-font)))

(setq url-using-proxy t)
(setq url-proxy-services '(("http" . "sensis-proxy-vs:8080")))


(set-face-attribute 'default nil :font "Bitstream Vera Sans Mono 10")

(require 'custom)
