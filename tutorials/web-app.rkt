#lang web-server/insta

;;; Continue: Web Applications in Racket
;;; https://docs.racket-lang.org/continue/index.html


;; 1 Getting Started
;(define (start request)
;  (response/xexpr
;   '(html
;     (head (title "My blog"))
;     (body (h1 "Under construction")))))


;; 2 The Application


;; 3 Basic Blog
; A blog is a (listof post)
; and a post is a (post title body)
(struct post (title body))

; BLOG: blog
; The static blog.
(define BLOG (list (post "Second Post" "This is another post")
                   (post "First Post!" "Hey, this is my first post!")))


;; 4 Rendering HTML
(define xexpr/c
  (flat-rec-contract
   xexpr
   (or/c string?
         (cons/c symbol? (listof xexpr))
         (cons/c symbol?
                 (cons/c (listof (list/c symbol? string?))
                         (listof xexpr))))))

; render-greeting string -> reponse
; Consume a name, and produces a dynamic response
(define (redner-greeting a-name)
  (response/xexpr
   `(html (head (title "Welcome"))
          (body (p ,(string-append "Hello " a-name))))))

;; render-post : (post? . -> . xexpr/c) ;; or (-> post? xexpr/c)
;; test:
; (render-post (post "First post!" "This is a first post."))
; => '(div ((class "post")) "First post!" (p "This is a first post."))

; exercise:
(define (render-post post)
  `(div ((class "post"))
        ,(post-title post)
        (p ,(post-body post))))

;'(ul (li "Larry")
;     (li "Curly")
;     (li "Moe"))
; <=
; `(ul ,@'((li "Larry") (li "Curly") (li "Moe")))

; render-as-itemized-list (listof xexpr) -> xexpr
; consumes a lit of imtes, and produces a rendering
; as an unordered list.
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

; render-as-item: xexpr -> xexpr
; consumes an xexpr, and produces a rendering
; as a list item.
(define (render-as-item a-fragment)
  `(li ,a-fragment))

; exercise:
; render-posts: ((listof post?) . -> . xexpr/c)
(define (render-posts posts)
  `(div ((class "posts"))
        ,@(map render-post posts)))

; start: request -> response
; consumes a request, and produces a page that displays all of the
; web content
; (define (start request)
;   (render-blog-page BLOG request))

; render-blog-page: blog request -> response
; Consume a blog and a request, and produces an HTML page
; of the content of the blog.
; (define (render-blog-page blog request)
;   (response/xexpr
;     `(html (head (title "My blog"))
;       (body (h1 "My Blog")
;         ,(render-posts blog)))))


;;; 5 Inspecting Request
; ; exercise:
; (define (can-parse-post? bindings)
;   (and (exists-binding? 'title)
;     (exists-binding? 'body)))

; exercise:
; (define (can-parse-post? bindings)
;   )

; start: request -> response
; consumes a request, and produces a page that displays all of the
; web content
(define (start request)
  (define a-blog
    (cond
      [(can-parse-post? (request-bindings request))
        (cons (parse-post (request-bindings request)) BLOG)]
      [else
        BLOG]))
  (render-blog-page a-blog request))

; can-parse-post?: bindings -> boolean
; Produces true if bindings contains values for 'title and 'body.
(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
    (exists-binding? 'body bindings)))

; parse-post: bindings -> post
; Consume a bindings, and produces a post out of the bindings.
(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
    (extract-binding/single 'body bindings)))

; render-blog-page: blog request -> response
; Consume a blog and a request, and produces an HTML page
; of the content of the blog.
(define (render-blog-page a-blog request)
  (response/xexpr
    `(html (head (title "My Blog"))
      (body
        (h1 "My Blog")
        ,(render-posts a-blog)
        (form
          (input ((name "title")))
          (input ((name "body")))
          (input ((type "submit"))))))))