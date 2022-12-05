#lang racket
(require racket/gui/base)
(provide set-randomness)
(provide should-explore?)
(provide random-action)
(provide init-state)
(provide check-init)
(provide run-n-episodes)
(provide run-graphic-maze)

;; Note: You do not need to understand how any of these functions
;;       work, in fact some of them are purposefully written to be
;;       hard to understand.


;; (set-randomness x) sets the randomness to a preset
;;    cycle of numbers based on the value of x. Each time
;;    you call this it resets to the beginning of the preset
;;    cycle defined for the given x.
;; set-randomness: Nat -> Nothing
(define (set-randomness x)
  (random-seed x))

;; init: ?? (Don't worry about this).
(define (init)
  (* -0.00001 (random 1000 2000)))

;; (init-state state) constructs the initial randomized
;;     ActionMap for the consumed state and produces
;;     the State-ActionMap pair.
;; init-state: State -> (list State Actionmap)
(define (init-state state)
  (list (list (first state) (second state))
        (list (list 'north (init))
              (list 'east (init))
              (list 'south (init))
              (list 'west (init)))))
;; (check-init val-fn seed) checks if the consumed val-fn
;;    is in the correct order and had its random values
;;    constructed appropriately. Does not check if it is
;;    the right size/States for a given Maze!
;; check-init: ValFn Nat -> Bool
(define (check-init val-fn seed)
  (local [(define ret true )
          (define srtd
            (sort val-fn (lambda (x y)
                           (or (< (second (first x)) (second (first y)))
                               (and
                                (= (second (first x)) (second (first y)))
                                (< (first (first x)) (first (first y))))))))]
    (random-seed seed)
    (for [(state srtd)]
      (for [(act-pair (second state))]
        (set! ret (and ret (equal? (second act-pair) (init))))))
    ret))


;; (first-n-inits n seed) produces the first n ActionMaps that
;;    would be generated with init-state function for a test
;;    case that was seeded with seed.

;; first-n-inits: Nat Nat -> (list ActionMap)
(define (first-n-inits n seed)
  (local [(define ret empty)]
    (random-seed seed)
    (for [(i n)]
      (set! ret (append ret (list (second (init-state (list 0 0)))))))
    ret))

;; (random-action loa) consumes a list and produces a random
;;    value from that list.
;; random-action: (listof Any) -> Any
(define (random-action loa)
  (list-ref loa (random 0 (length loa))))

;; (should-explore? epsilon) produces true if the
;;    agent should explore, which will be epsilon percent
;;    of the time.
;; should-explore?: Num -> Bool
(define (should-explore? eps)
  (< (random 0 100) (* 100 eps)))

;; Do not worry about the code below here, they are
;; helper functions for run-graphic-maze and
;; run-n-episodes (as well as those functions themselves).
;; You only need to know as much about them as is told to
;; you in the assignment spec.





(define (state-value x y q)
  (local
    [(define tp (first (filter (lambda (pair) (equal? (list x y) (first pair))) q)))
     (define mx (foldr (lambda (x y)
                          (max (second x) y)) -1000 (second tp)))
     (define eps-over (/ 0.05 (length (second tp))))
     (define pct-chance
       (map (lambda (x)
              (cond [(equal? (second x) mx) (+ 0.95 eps-over)]
                    [else eps-over])) (second tp)))]
 (inexact->exact (foldr (lambda (val pct so-far)
          (+ so-far (* pct (second val)))) 0 (second tp) pct-chance))))

(define (value->color val)
  (make-object color% 46 213 46 (min 1.0 (max 0 (* 5 val)))))

(define (find-goals maze)
  (local [(define goal-states empty)]
    (for ([y (length maze)])
                  (for ([x (string-length (first maze))])
                    (cond [(char=? #\G (string-ref (list-ref maze y) x))
                           (set! goal-states
                                 (cons (list x y) goal-states))]
                          [else void])))
    goal-states))
                          

(define (run-graphic-maze step-fn init-pos maze q eps)
  (local
    [(define cur-pos init-pos)
     (define cur-q q)
     (define w 720)
     (define h 720)
     (define panel-size 55)
     (define run false)
     (define done false)
     (define frame (new
                    (class frame%
                         (super-new
                          [label "Maze"]
                          [style '(no-resize-border)]
                          [width w]
                          [height (+ panel-size h)]
                          [x 0]
                          [y 0])
                        (define/augment (on-close)
                          (set! done true)))))
     (define border-size 1)
     (define num-sqr-hor (string-length (first maze)))
     (define num-sqr-ver (length maze))
     (define sqr-w (floor (- (/ w num-sqr-hor) border-size)))
     (define sqr-h (floor (- (/ h num-sqr-ver) border-size)))
     (define w/lst (range 0 num-sqr-hor 1))
     (define h/lst (range 0 num-sqr-ver 1))
     (define agent-w (floor (* 0.6 sqr-w)))
     (define agent-h (floor (* 0.6 sqr-h)))
     (define agent-off-w (floor (* 0.2 sqr-w)))
     (define agent-off-h (floor (* 0.2 sqr-h)))
     (define goals (find-goals maze))
     (define (draw-agent dc)
       (send dc set-pen "blue" border-size 'solid)
       (send dc set-brush "blue" 'solid)
       (send dc draw-rectangle
             (+ x-off agent-off-w (* sqr-w (first cur-pos)))
             (+ agent-off-h y-off (* sqr-h (second cur-pos)))
             agent-w agent-h))
     (define y-off 0)
     (define x-off 0)
     (define canv (new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 1 1)
                (for ([y h/lst])
                  (for ([x w/lst])
                    (cond
                      [(char=? #\X (string-ref (list-ref maze y) x))
                               (send dc set-brush "black" 'solid)]
                      [else (send dc set-brush
                                 (value->color (state-value x y cur-q))
                                 'solid)])
                                  
                    (send dc set-pen "blue" border-size 'solid)
                    (send dc draw-rectangle
                          (+ x-off (* sqr-w x)) (+ y-off (* sqr-h y))
                          sqr-w sqr-h)
                    (send dc set-text-foreground "black")
                    (send dc set-font (make-font #:size 32 #:family 'roman
                             #:weight 'bold))
                    (for [(pair goals)]
                      (send dc draw-text "G"
                            (+ x-off (* sqr-w (first pair)))
                            (+ y-off (* sqr-h (second pair)))))
                    (draw-agent dc))))]))
;     (define pause
;       (new button% [parent frame]
;             [label "Pause"]
;             [callback (lambda (button event) (set! run false))]))
     (define play
       (new button% [parent frame]
            [label "Play"]
            [callback (lambda (button event) (set! run true))]))
     (define (run-step)
       (cond [(not run) void]
             [else
              (local
                [(define ran (step-fn maze cur-pos cur-q eps))]
                (set! cur-pos (first ran))
                (set! cur-q (second ran))
                (cond
                  [(char=? #\G (string-ref (list-ref maze (second cur-pos)) (first cur-pos)))
                   (set! cur-pos init-pos)]))]))
     (define timer-counter 0)
     (define timer
       (new timer%
            (interval 10)
            (notify-callback
             (lambda ()
               (cond
                 [(not done) 
                             (set! timer-counter (add1 timer-counter))
                             (run-step)
                             (send canv refresh)]
                 [else (send timer stop)])))))
                      
     ]
   (send frame show #t)))

(define (get-tile s maze)
  (string-ref (list-ref maze (second s)) (first s)))


;; run-n-episodes: 
(define (run-n-episodes step-fn n s maze Q eps)
  (local [(define (run-episode-steps  s maze Q steps)
            (cond [(char=? #\G (get-tile s maze)) (list steps Q)]
                  [else
                   (local [(define step (step-fn maze s Q eps))]
                     (run-episode-steps (first step) maze (second step) (add1 steps)))]))
          (define (run-episode s maze Q)
            (run-episode-steps s maze Q 0))
          (define (run-n-episodes/acc n s maze Q acc)
            (cond [(zero? n) (list Q (reverse acc))]
                  [else
                   (local [(define ep (run-episode s maze Q))]
                     (run-n-episodes/acc (sub1 n) s maze
                                         (second ep) (cons (first ep) acc)))]))]
    (run-n-episodes/acc n s maze Q empty)))

