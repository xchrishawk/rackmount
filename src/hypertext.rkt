;;
;; hypertext.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Requires --

(require (for-syntax racket))
(require (for-syntax "utility.rkt"))

;; -- Provides --

(provide hypertext)

;; -- Macros --

;;
;; Macro for generating HTML code.
;;
;; Example invocation...
;;
;; (hypertext
;;  (html
;;   (head
;;    (title "The Page Title"))
;;   (body
;;    (h1 "The Header")
;;    (hr)
;;    (p "The first paragraph")
;;    (p (text (format "Text in function calls needs to be escaped with `text`")))
;;    (when some-condition
;;      (p "The optional paragraph"))
;;    (for ([thing (in-list a-list)])
;;      (p (text thing)))
;;    (img #:src "http://www.example.com/example.png"))))
;;

(define-syntax (hypertext stx)
  (let* ([clauses (rest (syntax-e stx))]
         [result (quasiquote
                  (parameterize ([current-output-port (open-output-string)])
                    ,@(for/list ([clause (in-list clauses)])
                        (proc-clause clause))
                    (get-output-string (current-output-port))))])
    (datum->syntax stx result)))

;; -- Private Procedures --

;; Processes an individual clause
(define-for-syntax (proc-clause clause)
  (let* ([unwrapped (syntax-e clause)])
    ;; Verify the first element in the list is a symbol
    ;; This is to guard against special syntax forms like (let ((...)) ...)
    (if (not (identifier? (first unwrapped)))
        ;; Not a method call - return syntax intact
        clause
        ;; Method call - get the tag name and the subclauses
        (let* ([tag (syntax->datum (first unwrapped))]
               [tag-name (symbol->string tag)]
               [subclauses (rest unwrapped)])
          (cond
            ;; It is an HTML tag - get contents and attributes
            [(set-member? valid-html-tags tag-name)
             (let-values ([(contents attributes) (get-contents-attributes subclauses)])
               `(begin
                  (display (format "<~A" ,tag-name))
                  ;; Attributes
                  ,@(for/list ([(key value) (in-hash attributes)])
                      `(display (format " ~A=\"~A\"" ,key ,value)))
                  ;; Do we have any content?
                  ,@(if (null? contents)
                        ;; No content - immediately close tag
                        `((display "/>"))
                        ;; Have content - display it and then close tag
                        `((display ">")
                          ,@(for/list ([content (in-list contents)])
                              (if (list? (syntax-e content))
                                  (proc-clause content)
                                  `(display ,content)))
                          (display (format "</~A>" ,tag-name))))))]
            ;; It is literal text - display it
            [(equal? "text" tag-name)
             `(begin
                ,@(for/list ([subclause (in-list subclauses)])
                    `(display ,subclause)))]
            ;; It is not an HTML tag or literal text
            [else
             `(,tag
               ;; Recursively process subclauses, leaving intact anything that's not a subclause
               ,@(for/list ([subclause (in-list subclauses)])
                   (if (list? (syntax-e subclause))
                       (proc-clause subclause)
                       subclause)))])))))

;; Splits a tag into attributes and contents
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

;; -- Constants --

;; Set of valid HTML tags (https://www.w3schools.com/tags/)
(define-for-syntax
  valid-html-tags
  (set "a"
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
