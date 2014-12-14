#lang racket
(struct point (x y))

(define rotatePointByAngle (lambda (origin rpoint angleDegrees)
                             (define angle (degrees->radians angleDegrees))       
                             (point (exact-round (+ (- (* (cos angle) (- (point-x rpoint) (point-x origin))) (* (sin angle) (- (point-y rpoint)(point-y origin)))) (point-x origin))) 
                                    (exact-round (+ (- (* (sin angle) (- (point-x rpoint) (point-x origin))) (* (cos angle) (- (point-y rpoint)(point-y origin)))) (point-y origin)))
                             )
                             ))

(define rotateShapeByDegrees (lambda (origin shape angleDegrees) (
                                                  cond [(null? shape) '()]
                                                       [else (cons (rotatePointByAngle origin (car shape) angleDegrees) (rotateShapeByDegrees origin (cdr shape) angleDegrees))] 
                                                  )))
(define rotateShapeAllAngles (lambda (shape) 
                                               (define origin (car shape))
                                               (list shape (list
                                                                 (rotateShapeByDegrees origin shape 90)
                                                                 (rotateShapeByDegrees origin shape 180)
                                                                 (rotateShapeByDegrees origin shape 270)
                                                            )
                                               )))

(define a (list (point 1 2) (point 5 6) (point 9 -10) (point 7 8)))