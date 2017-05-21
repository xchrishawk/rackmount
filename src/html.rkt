#lang racket

;; -- Requires --

(require (for-syntax racket))
(require (for-syntax "utility.rkt"))

;; -- Provides --

(provide html)

;; -- Macros --

(define-syntax (html stx)
  (let ([result (quasiquote (string-append ,@(flatten (process-clause stx))))])
    (datum->syntax stx result)))

;; -- Constants --

;; Set of valid HTML tags (https://www.w3schools.com/tags/)
(define-for-syntax valid-html-tags (set "a"
                                        "abbr"
                                        "acronym"
                                        "address"
                                        "applet"
                                        "area"
                                        "article"
                                        "aside"
                                        "audio"
                                        "b"
                                        "base"
                                        "basefont"
                                        "bdi"
                                        "bdo"
                                        "big"
                                        "blockquote"
                                        "body"
                                        "br"
                                        "button"
                                        "canvas"
                                        "caption"
                                        "center"
                                        "cite"
                                        "code"
                                        "col"
                                        "colgroup"
                                        "datalist"
                                        "dd"
                                        "del"
                                        "details"
                                        "dfn"
                                        "dialog"
                                        "dir"
                                        "div"
                                        "dl"
                                        "dt"
                                        "em"
                                        "embed"
                                        "fieldset"
                                        "figcaption"
                                        "figure"
                                        "font"
                                        "footer"
                                        "form"
                                        "frame"
                                        "frameset"
                                        "h1"
                                        "h2"
                                        "h3"
                                        "h4"
                                        "h5"
                                        "h6"
                                        "head"
                                        "header"
                                        "hr"
                                        "html"
                                        "i"
                                        "iframe"
                                        "img"
                                        "input"
                                        "ins"
                                        "kbd"
                                        "keygen"
                                        "label"
                                        "legend"
                                        "li"
                                        "link"
                                        "main"
                                        "map"
                                        "mark"
                                        "menu"
                                        "menuitem"
                                        "meta"
                                        "meter"
                                        "nav"
                                        "noframes"
                                        "noscript"
                                        "object"
                                        "ol"
                                        "optgroup"
                                        "option"
                                        "output"
                                        "p"
                                        "param"
                                        "picture"
                                        "pre"
                                        "progress"
                                        "q"
                                        "rp"
                                        "rt"
                                        "ruby"
                                        "s"
                                        "samp"
                                        "script"
                                        "section"
                                        "select"
                                        "small"
                                        "source"
                                        "span"
                                        "strike"
                                        "strong"
                                        "style"
                                        "sub"
                                        "summary"
                                        "sup"
                                        "table"
                                        "tbody"
                                        "td"
                                        "textarea"
                                        "tfoot"
                                        "th"
                                        "thead"
                                        "time"
                                        "title"
                                        "tr"
                                        "track"
                                        "tt"
                                        "u"
                                        "ul"
                                        "var"
                                        "video"
                                        "wbr"))

;; -- Private Procedures --

;; Process a single clause
(define-for-syntax (process-clause clause)
  (let ([unwrapped (syntax-e clause)])
    (if (list? unwrapped)
        ;; Syntax is a list
        (let ([tag-name (symbol->string (syntax->datum (first unwrapped)))])
          (if (set-member? valid-html-tags tag-name)
              ;; Tag is a valid HTML tag, process it as such
              (let*-values ([(contents attributes) (get-contents-attributes unwrapped)])
                (let ([tag-name (symbol->string (syntax->datum (first contents)))])
                  (quasiquote
                   ("<"
                    ,tag-name
                    ,@(hash-map attributes (λ (name value) (list (format " ~A=\"" name) value "\"")))
                    ,@(if (length-at-least contents 2)
                          ;; There are some contents to include
                          (quasiquote
                           (">"
                            ,@(map process-clause (rest contents))
                            "</"
                            ,tag-name
                            ">"))
                          ;; No contents to include, immediately close the tag
                          (list "/>"))))))
              ;; Not an HTML tag - just return unmodified
              clause))
        ;; Not a list - return unmodified
        clause)))

;; Split syntax into contents and attributes
(define-for-syntax (get-contents-attributes unwrapped)
  (let-values ([(unused attributes)
                (let loop ([remaining unwrapped] [unused null] [attributes (hash)])
                  (if (null? remaining)
                      (values unused attributes)
                      (let* ([this-stx (first remaining)]
                             [this-datum (syntax->datum this-stx)])
                        (if (keyword? this-datum)
                            ;; is a keyword
                            (begin
                              (when (not (length-at-least remaining 2))
                                (error (format "Keyword missing argument: ~A" this-datum)))
                              (loop (drop remaining 2)
                                    unused
                                    (hash-set attributes
                                              (keyword->string this-datum)
                                              (second remaining))))
                            ;; is not a keyword
                            (loop (rest remaining)
                                  (cons this-stx unused)
                                  attributes)))))])
    (values (reverse unused) attributes)))
