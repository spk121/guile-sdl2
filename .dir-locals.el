((scheme-mode
  .
  ((eval . (put 'call-with-window 'scheme-indent-function 1))
   (eval . (put 'call-with-renderer 'scheme-indent-function 1))
   (eval . (put 'surface-parse-match 'scheme-indent-function 1))
   (eval . (put 'pixel-format-parse-match 'scheme-indent-function 1))
   (eval . (put 'palette-parse-match 'scheme-indent-function 1))
   (eval . (put 'color-parse-match 'scheme-indent-function 1)))))
