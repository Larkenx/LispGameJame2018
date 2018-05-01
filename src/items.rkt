#lang racket

(provide (all-defined-out))

(require
  thing
  "point.rkt"
  2htdp/image)

; All items have:
; - a display char/item if they're on the ground
; - if they're consumed or not (picked up)
; - if they stack or not (stackables use the 'quantity' value)
; - methods for:
; -- being picked up
; -- being dropped
; 
; NOTE: stackable overrides consumable
(define-thing item
  [character #\x]
  [color "white"]
  [consumable #f]
  [stackable #f]
  [quantity 1]
  [category 'unknown]
  [(on-pick-up item entity world) (void)]
  [(on-drop item entity world)    (void)])

(define-thing gem item
  [character "◆"])

(define-thing player
  [character "@"]
  [color "yellow"]
  [name "player"]
  [attack 10]
  [defense 10]
  [health 20]
  [max-health 20]
  [gems (list #f #f #f #f)])

(define-thing sapphire  gem
  [name "sapphire"]
  [color (make-color 0 146 251)]
  [(on-pick-up item entity world)
   (define original-gems (thing-get entity 'gems))
   (define sapphire (first original-gems))
   (define emerald (second original-gems))
   (define ruby (third original-gems))
   (define diamond (fourth original-gems))
   (thing-set! entity 'gems (list #t emerald ruby diamond))])  

(define-thing emerald gem
  [name "emerald"]
  [color "green"]
  [(on-pick-up item entity world)
   (define original-gems (thing-get entity 'gems))
   (define sapphire (first original-gems))
   (define emerald (second original-gems))
   (define ruby (third original-gems))
   (define diamond (fourth original-gems))
   (thing-set! entity 'gems (list sapphire #t ruby diamond))])

(define-thing ruby gem
  [name "ruby"]
  [color "red"]
  [(on-pick-up item entity world)
   (define original-gems (thing-get entity 'gems))
   (define sapphire (first original-gems))
   (define emerald (second original-gems))
   (define ruby (third original-gems))
   (define diamond (fourth original-gems))
   (thing-set! entity 'gems (list sapphire emerald #t diamond))])

(define-thing diamond gem
  [name "diamond"]
  [color "white"]
  [(on-pick-up item entity world)
   (define original-gems (thing-get entity 'gems))
   (define sapphire (first original-gems))
   (define emerald (second original-gems))
   (define ruby (third original-gems))
   (define diamond (fourth original-gems))
   (thing-set! entity 'gems (list sapphire emerald ruby #t))])

; blue green red white
(define *gems*
  (vector
   emerald
   sapphire
   ruby diamond))

; Armor protects the wearer
(define-thing armor item
  [character "\\"]
  [defense 0]
  [category 'armor]
  [(on-pick-up item entity world)
   (thing-set! entity 'defense (+ (thing-get entity 'defense)
                                  (thing-get item 'defense)))]                               
  [(on-drop item entity world)
   (thing-set! entity 'defense (- (thing-get entity 'defense)
                                  (thing-get item 'defense)))])

(define *armors*
  (vector
   (make-thing armor [name "leather"]   [color "brown"]  [defense 1])
   (make-thing armor [name "chain"]     [color "gray"]   [defense 2])
   (make-thing armor [name "plate"]     [color "white"]  [defense 3])
   (make-thing armor [name "enchanted"] [color "purple"] [defense 5])))

; Weapons increase attack
(define-thing weapon item
  [character #\)]
  [attack 0]
  [category 'weapon]
  [(on-pick-up item entity world)
   (thing-set! entity 'attack (+ (thing-get entity 'attack)
                                 (thing-get item 'attack)))]                               
  [(on-drop item entity world)
   (thing-set! entity 'attack (- (thing-get entity 'attack)
                                 (thing-get item 'attack)))])

(define *weapons*
  (vector
   (make-thing weapon [name "club"]        [color "brown"]  [attack 1])
   (make-thing weapon [name "dagger"]      [color "gray"]   [attack 2])
   (make-thing weapon [name "battle axe"]  [color "white"]  [attack 3])
   (make-thing weapon [name "longsword"]   [color "white"]  [attack 3])
   (make-thing weapon [name "magic sword"] [color "purple"] [attack 5])))

; Potions are single use and consumed on contact
(define-thing potion item
  [character #\!]
  [category 'potion]
  [consumable #t])

(define *potions*
  (vector
   (make-thing potion
               [name "health potion"]
               [color "red"]
               [(on-pick-up item entity world)
                (thing-set! entity 'health (+ 10 (thing-get entity 'health)))])))

; Coins are stackables
(define-thing coins item
  [character #\*]
  [category 'coin]
  [stackable #t])

(define *coins*
  (vector
   (make-thing coins [name "copper coin"]   [color "brown"]  [quantity 0.01])
   (make-thing coins [name "silver coin"]   [color "silver"] [quantity 0.1])
   (make-thing coins [name "gold coin"]     [color "yellow"] [quantity 1])
   (make-thing coins [name "platinum coin"] [color "white"]  [quantity 10])))

; All items combined
(define *all-items*
  (vector *armors* *weapons* *potions* *coins*))