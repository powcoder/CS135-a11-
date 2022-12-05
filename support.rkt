;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname stdout) (read-case-sensitive #t) (teachpacks ()))
;; A Node is a Sym
;; A Map is a (listof (list Node (listof Neighbour)))
;;   requires: Map is directed and acyclic
;; A Neighbour is a (list Node Nat) where the number indicates the
;;   travel time (in minutes) to the neighbour.
(define southern-ontario
  (list (list 'Brantford (list (list 'Hamilton 30)))
        (list 'Cambridge (list (list 'Brantford 30) (list 'Hamilton 45)
                                   (list 'London 70) (list 'TO 80)))
        (list 'Guelph (list (list 'Cambridge 30) (list 'TO 80)))
        (list 'Hamilton empty)
        (list 'London (list (list 'Brantford 70)))
        (list 'KW (list (list 'Cambridge 30) (list 'Guelph 35) (list 'Stratford 40)))
        (list 'Stratford (list (list 'London 50)))
        (list 'TO (list (list 'Hamilton 60)))))

(check-expect (travel-time 'Guelph 'Hamilton southern-ontario) 90)
; travel time for '(Guelph Cambridge Brantford Hamilton)
(check-expect
 (all-paths 'Guelph 'Hamilton southern-ontario)
 (list (list 'Guelph 'Cambridge 'Brantford 'Hamilton)
   (list 'Guelph 'Cambridge 'Hamilton)
   (list 'Guelph 'Cambridge 'London 'Brantford 'Hamilton)
   (list 'Guelph 'Cambridge 'TO 'Hamilton)
   (list 'Guelph 'TO 'Hamilton)))

(check-expect (all-paths 'Stratford 'Guelph southern-ontario) empty)
(check-expect
 (all-travel-times 'Guelph 'Hamilton southern-ontario)
 (list (list 90 (list 'Guelph 'Cambridge 'Brantford 'Hamilton))
   (list 75 (list 'Guelph 'Cambridge 'Hamilton))
   (list 200 (list 'Guelph 'Cambridge 'London 'Brantford 'Hamilton))
   (list 170 (list 'Guelph 'Cambridge 'TO 'Hamilton))
   (list 140 (list 'Guelph 'TO 'Hamilton))))

(check-expect (all-travel-times 'Stratford 'Guelph southern-ontario) empty)



;; An Action Map (ActionMap) is a:
;; (list (list 'north Num)
;;       (list 'east Num)
;;       (list 'south Num)
;;       (list 'west Num))

;; A State is a:
;; (list Nat Nat)

;; A Value Function (ValFn) is a:
;; (listof State ActionMap)
;; A Maze is a (listof Str)
;; Requires: each Str be the same length, and contains only #\X, #\O, or #\G.

;; For example,
(define small-maze (list "OXOGOO"
                         "OXOXOO"
                         "OOOOOO"))
(require "rl-support.rkt")




(check-expect (reward small-maze (list 0 0)) 0)
(check-expect (reward small-maze (list 3 0)) 1)
(check-expect (reward small-maze (list 1 2)) 0)


(check-expect (move small-maze 'east (list 0 0)) (list 0 0))
(check-expect (move small-maze 'south (list 4 1)) (list 4 2))
(check-expect (move small-maze 'east (list 5 2)) (list 5 2))
(check-expect (move small-maze 'west (list 5 2)) (list 4 2))


(check-expect
 (maze-states small-maze)
 (list (list 0 0) (list 1 0) (list 2 0) (list 3 0) (list 4 0) (list 5 0)
       (list 0 1) (list 1 1) (list 2 1) (list 3 1) (list 4 1) (list 5 1)
       (list 0 2) (list 1 2) (list 2 2) (list 3 2) (list 4 2) (list 5 2)))
(check-expect
 (local [(define random (set-randomness 42))]
   (check-init (init-value-fn small-maze) 42)) true)
(check-expect
 (local [(define random (set-randomness 17))]
   (check-init (init-value-fn small-maze) 17)) true)


(define actions (list 'north 'east 'south 'west))

(define tiny-maze (list "OG"))

(define tiny-value-fn
  (list (list (list 0 0)
              (list (list 'north 0.03)
                    (list 'east 0.02)
                    (list 'south 0.01)
                    (list 'west 0.00)))
        (list (list 1 0)
              (list (list 'north 0.03)
                    (list 'east 0.015)
                    (list 'south 0.017)
                    (list 'west 0.033)))))

(check-expect
  (local [(define random (set-randomness 42))]
    (next-action tiny-maze (list 0 0) tiny-value-fn 0.05))
  (local [(define random (set-randomness 42))]
    (cond [(should-explore? 0.05) (random-action actions)]
          [else 'north])))



(check-within
 (new-value tiny-maze (list 0 0) tiny-value-fn 'east)
 0.03262125
 0.0001)



(check-within
 (local [(define random (set-randomness 42))]
   (take-step tiny-maze (list 0 0) tiny-value-fn 0.05))
 (list (list 0 0)
       (list (list (list 0 0)
              (list (list 'north 0.0299625)
                    (list 'east 0.02)
                    (list 'south 0.01)
                    (list 'west 0.00)))
        (list (list 1 0)
              (list (list 'north 0.03)
                    (list 'east 0.015)
                    (list 'south 0.017)
                    (list 'west 0.033)))))
 0.0001)



(define q-small (first (run-n-episodes take-step 3 (list 0 0) small-maze (init-value-fn small-maze) 0.05)))
