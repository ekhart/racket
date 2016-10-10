#lang racket

;;; More: Systems Programming with Racket
; https://docs.racket-lang.org/more/index.html

;; 1 Ready
;> (require xrepl)
;> ,install!

;> (require readline)
;> (install-readline!)


;; 2 Set
(define (go)
  'yep-it)

; > (enter! "serve.rkt")
;  [loading serve.rkt]
; > (go)
; 'yep-it-works


;; 4 “Hello World” Server
; (define (serve port-no)
; 	(define listener (tcp-listen port-no 5 #t))
; 	(define (loop)
; 		(accept-and-handle listener)
; 		(loop))
; 	(loop))

; (define (accept-and-handle listener)
; 	(define-values (in out) (tcp-accept listener))
; 	(handle in out)
; 	(close-input-port in)
; 	(close-output-port out))

; (define (handle in out)
; 	; Dicard the request header (up to blank line):
; 	(regexp-match #rx"(\r\n|^)\r\n" in)
; 	; Send reply
; 	(display "HTTP/1.0 200 Okay\r\n" out)
;   	(display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
;   	(display "<html><body>Hello, world!</body></html>" out))

; (enter! "serve.rkt")
; > (serve 8080)
; -> working server


;; 5 Server Thread
; > (serve 8080)
; ^Cuser break
; >
; Unfortunately, we cannot now re-start the server with the same port number:
; > (serve 8080)
; tcp-listen: listen on 8080 failed (address already in use)
; (define (serve port-no)
; 	(define listener (tcp-listen port-no 5 #t))
; 	(define (loop)
; 		(accept-and-handle listener)
; 		(loop))
; 	(define t (thread loop))
; 	(lambda ()
; 		(kill-thread t)
; 		(tcp-close listener)))

; Try the new one:
; > (enter! "serve.rkt")
;  [re-loading serve.rkt]
; > (define stop (serve 8081))	; server running
; Your server should now respond to http://localhost:8081, but you can shut down and restart the server on the same port number as often as you like:
; > (stop)						; server killed
; > (define stop (serve 8081))
; > (stop)
; > (define stop (serve 8081))
; > (stop)


;; 6 Connection Threads
; (define (accept-and-handle listener)
; 	(define-values (in out) (tcp-accept listener))
; 	(thread
; 		(lambda ()
; 			(handle in out)
; 			(close-input-port in)
; 			(close-output-port out))))


;; 7 Terminating Connections
; (define (accept-and-handle listener)
; 	(define-values (in out) (tcp-accept listener))
; 	(define t (thread
; 		(lambda ()
; 			(handle in out)
; 			(close-input-port in)
; 			(close-output-port out))))
; 	(thread (lambda ()
; 		(sleep 10)
; 		(kill-thread t))))

; custodian - shutdown mechanism form all resources other then memory: theads, streams e.g
(define (accept-and-handle listener)
	(define cust (make-custodian))
	(custodian-limit-memory cust (* 50 1024 1024))
	(parameterize ([current-custodian cust])
		(define-values (in out) (tcp-accept listener))
		(thread (lambda ()
			(handle in out)
			(close-input-port in)
			(close-output-port out))))
	(thread (lambda ()
		(sleep 10)
		(custodian-shutdown-all cust))))

(define (serve port-no)
	(define main-cust (make-custodian))
	(parameterize ([current-custodian main-cust])
		(define listener (tcp-listen port-no 5 #t))
		(define (loop)
			(accept-and-handle listener)
			(loop))
		(thread loop))
	(lambda ()
		(custodian-shutdown-all main-cust)))

; > (enter! "serve.rkt")
;  [re-loading serve.rkt]
; > (define stop (serve 8081))
; > (define-values (cin cout) (tcp-connect "localhost" 8081))
; Now wait 10 seconds. If you try reading from cin, which is the stream that sends data from the server back to the client, you’ll find that the server has shut down the connection:
; > (read-line cin)
; #<eof>
; Alternatively, you don’t have to wait 10 seconds if you explicitly shut down the server:
; > (define-values (cin2 cout2) (tcp-connect "localhost" 8081))
; > (stop)
; > (read-line cin2)
; #<eof>


;; 8 Dispatching
(require xml net/url)

; > (xexpr->string '(html (head (title "Hello")) (body "Hi!")))
; "<html><head><title>Hello</title></head><body>Hi!</body></html>"

; (define (handle in out)
;   (define req
;     ; Match the first line to extract the request:
;     (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
;                   (read-line in)))
;   (when req
;     ; Discard the rest of the header (up to blank line):
;     (regexp-match #rx"(\r\n|^)\r\n" in)
;     ; Dispatch:
;     (let ([xexpr (dispatch (list-ref req 1))])
;       ; Send reply:
;       (display "HTTP/1.0 200 Okay\r\n" out)
;       (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
;       (display (xexpr->string xexpr) out))))

; > (define u (string->url "http://localhost:8080/foo/bar?x=bye"))
; > (url-path u)
; (list (path/param "foo" '()) (path/param "bar" '()))
; > (map path/param-path (url-path u))
; '("foo" "bar")
; > (url-query u)
; '((x . "bye"))

(define (dispatch str-path)
  ; Parse the request as a URL:
  (define url (string->url str-path))
  ; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ; Find a handler based on the path's first element:
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ; Call a handler:
      (h (url-query url))
      ; No handler found:
      `(html (head (title "Error"))
            (body
             (font ((color "red"))
                   "Unknown page: "
                   ,str-path)))))

(define dispatch-table (make-hash))


(hash-set! dispatch-table "hello"
	(lambda (query)
		`(html (body "Hello, World!"))))


;; 9 Servlets and Session
(define (build-request-page label next-url hidden)
	`(html
		(head (title "Enter a Number to Add"))
		(body ([bgcolor "white"])
			(form ([action ,next-url] [method "get"])
				,label
				(input ([type "text"] [name "number"] [value ""]))
				(input ([type "hidden"] [name "hidden"] [value ,hidden]))
				(input ([type "submit"] [name "enter"] [value "Enter"]))))))

(define (many query)
	(build-request-page "Number of greetings:" "/reply" ""))

(define (reply query)
	(define n (string->number (cdr (assq 'number query))))
	`(html
		(body
			,@(for/list ([i (in-range n)])
				" hello"))))

(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)


;; 10 Limiting Memory Use
; The solution to this class of problems is to limit the memory use of a connection. Inside accept-and-handle, after the definition of cust, add the line
; (custodian-limit-memory cust (* 50 1024 1024))


;; 11 Continuations
(define (sum query)
	(build-request-page "First number:" "/one" ""))

(define (one query)
	(build-request-page "Second number:" "/two" (cdr (assq 'number query))))

(define (two query)
	(let ([n (string->number (cdr (assq 'hidden query)))]
		[m (string->number (cdr (assq 'number query)))])
		`(html (body "The sum is " ,(number->string (+ m n))))))

(define (sum2 query)
	(define m (get-number "First number:"))
	(define n (get-number "Second number:"))
	`(html (body "The sum is " ,(number->string (+ m n)))))

(define (get-number label)
	(define query
		; Generate a URL fo the current computation:
		(send/suspend
			; Receive the computation-as-URL here:
			(lambda (k-url)
				; Generate the query-page result fo this connection.
				; Send the query to the saved-computation URL:
				(build-request-page label k-url ""))))
	(string->number (cdr (assq 'number query))))

(require racket/control)

(define (handle in out)
  (define req
    ; Match the first line to extract the request:
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ; Discard the rest of the header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ; Dispatch:
    (let ([xexpr (prompt (dispatch (list-ref req 1)))])
      ; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (send/suspend mk-page)
	(let/cc k
		(define tag (format "k~a" (current-inexact-milliseconds)))
		(hash-set! dispatch-table tag k)
		(abort (mk-page (string-append "/" tag)))))

(hash-set! dispatch-table "sum2" sum2)
