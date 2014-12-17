#lang racket

(struct point (x y)  #:transparent) ; a point ex : point 3 4
(struct line (m d p)   #:transparent) ;  a line m : mail ,  d : offset p :point
(struct limitedline (l p1 p2)  #:transparent) ; a limited line l :line p1 : point1 , p2 : point2 ex : line
(struct HighWidth (x y)  #:transparent)

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

(define (equalPoints p1 p2)(
	if (or (equal? p1 #f) (equal? p2 #f))
	#f
	(and (equal? (point-x p1) (point-x p2))
		(equal? (point-y p1) (point-y p2)))
	))

(equalPoints (point 1 2) (point 1 2)) ; given #t  true
(equalPoints (point 1 2) (point 1 3)) ; given #f  false


(define (equalLines l1 l2)(
	and (equal? (line-m l1) (line-m l2))
	(equal? (line-d l1) (line-d l2))
	))
(equalLines (line 1 2 (point 1 4)) (line 1 2 (point 1 4))) ; given #t  true 
(equalLines (line 1 2 (point 1 4)) (line 1 3 (point 1 5))) ; given #f  true 

(define (equalSpLines ll1 ll2)(
	and (equalLines (limitedline-l ll1) (limitedline-l ll2))
		(or (and (equalPoints (limitedline-p1 ll1) (limitedline-p1 ll2))
			(equalPoints (limitedline-p2 ll1) (limitedline-p2 ll2)))
			(and (equalPoints (limitedline-p2 ll1) (limitedline-p1 ll2))
			(equalPoints (limitedline-p1 ll1) (limitedline-p2 ll2)))
		)
	))

(equalSpLines (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 2)) (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 2))) ; given #t  true
(equalSpLines (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 2)) (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 3))); given #f  true
(equalSpLines (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 2 2)) (limitedline (line 1 2 (point 1 4)) (point 2 2) (point 1 2))); given #t  true

(define (getLine p1 p2) (
	let* ([m (if (equal? (- (point-x p2) (point-x p1)) 0)
			"undefined"
			(/ (- (point-y p2) (point-y p1)) (- (point-x p2) (point-x p1))))]
		[d (if (equal? m "undefined")
			"undefined"
			(+ (* m (- 0 (point-x p1))) (point-y p1)))])
	(limitedline (line m d p1) p1 p2)))

(line-m (limitedline-l (getLine (point 1 1 ) (point 2 2 )))) ; given 1  true
(line-d (limitedline-l (getLine (point 1 1 ) (point 2 2 )))) ; given 0  true

(define (pointInLine l p) (
	if (equal? (line-m l) "undefined")
	(equal? (point-x (line-p l)) (point-x p))
	(equal? (point-y p) (+ (* (line-m l) (point-x p)) (line-d l)))
	))

(pointInLine (line 1 0 (point 1 1)) (point 1 1)) ; given #t  true
(pointInLine (line 1 0 (point 1 1)) (point 1 2)) ; given #f  true

(define (pointInSpicificLine ll p)(
	let ([desPLp1 (sqrt (+ (sqr (- (point-x p) (point-x (limitedline-p1 ll)))) (sqr (- (point-y p) (point-y (limitedline-p1 ll))))))]
		[desPLp2 (sqrt (+ (sqr (- (point-x p) (point-x (limitedline-p2 ll)))) (sqr (- (point-y p) (point-y (limitedline-p2 ll))))))]
		[desLp1Lp2 (sqrt (+ (sqr (- (point-x (limitedline-p2 ll)) (point-x (limitedline-p1 ll)))) (sqr (- (point-y (limitedline-p2 ll)) (point-y (limitedline-p1 ll))))))])
		(equal? (+ desPLp2 desPLp1) desLp1Lp2)
	))

(pointInSpicificLine (getLine (point 1 1 ) (point 2 2 )) (point 1.5 1.5)) ; given #t  true
(pointInSpicificLine (getLine (point 1 1 ) (point 2 2 )) (point -1 1))  ; given #f  true

(define (match2Lines ll1  ll2)(
	equalSpLines ll1 ll2
	))

(define (halfMatch2LinesByStart ll1 ll2) (
	and (equalLines (limitedline-l ll1) (limitedline-l ll2))
		(equalPoints (limitedline-p1 ll1) (limitedline-p1 ll2))
	))

(halfMatch2LinesByStart (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 4)) (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 2))) ; given #t  true
(halfMatch2LinesByStart (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 1 4)) (limitedline (line 1 2 (point 1 4)) (point 2 2) (point 1 2))) ; given #f  true

(define (halfMatch2LinesByEnd ll1 ll2) (
	and (equalLines (limitedline-l ll1) (limitedline-l ll2))
		(equalPoints (limitedline-p2 ll1) (limitedline-p2 ll2))
	))

(halfMatch2LinesByEnd (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 2 2)) (limitedline (line 1 2 (point 1 4)) (point 10 2) (point 2 2))) ; given #t  true
(halfMatch2LinesByEnd (limitedline (line 1 2 (point 1 4)) (point 1 2) (point 2 2)) (limitedline (line 1 2 (point 1 4)) (point 10 2) (point 3 2))) ; given #f  true

(define (getIntersectionPoint l1 l2)(
	cond [(equal? (line-m l1) (line-m l2)) #f]
		[(equal? (line-m l1) "undefined") (point (point-x (line-p l1)) (+ (* (line-m l2) (point-x (line-p l1))) (line-d l2)))]
		[(equal? (line-m l2) "undefined") (point (point-x (line-p l2)) (+ (* (line-m l1) (point-x (line-p l2))) (line-d l1)))]
		[#t (let* ([x ( / ( - (line-d l2) (line-d l1)) (-(line-m l1) (line-m l2)))]
			[y (+ (* (line-m l1) x) (line-d l1))]
			)
			(point x y))]
	))

(point-x (getIntersectionPoint (line 1 1 (point 1 2)) (line -1 1 (point 1 0)))) ; given 0  true
(point-y (getIntersectionPoint (line 1 1 (point 1 2)) (line -1 1 (point 1 0)))) ; given 1 true
(getIntersectionPoint (line 1 1 (point 1 2)) (line -1 1 (point 1 0))) ; given ( point 0 1 ) ture

(define (endIntersection ll1 ll2 p) (
	if (equal? p #f)
	#f
	(and (pointInSpicificLine ll1 p) (pointInSpicificLine ll2 p) (equalPoints (limitedline-p2 ll2) p))
	))

(define limitedline-test1 (getLine (point 3 1) (point 0 1)))
(define limitedline-test2 (getLine (point 0 3) (point 0 0)))
(define limitedline-test3 (getLine (point 3 1) (point 0 1)))
(endIntersection limitedline-test2 limitedline-test3
	(getIntersectionPoint (limitedline-l limitedline-test2) (limitedline-l limitedline-test3)))
(endIntersection limitedline-test1 limitedline-test3
	(getIntersectionPoint (limitedline-l limitedline-test1) (limitedline-l limitedline-test3)))

(define (startIntersection ll1 ll2 p) (
	if (equal? p #f)
	#f
	(and (pointInSpicificLine ll1 p) (pointInSpicificLine ll2 p) (equalPoints (limitedline-p1 ll2) p))
	))

(define limitedline-test5 (getLine (point 0 1) (point 3 1)))
(define limitedline-test4 (getLine (point 0 3) (point 0 0)))

(startIntersection limitedline-test4 limitedline-test5
	(getIntersectionPoint (limitedline-l limitedline-test4) (limitedline-l limitedline-test5)))
(startIntersection limitedline-test5 limitedline-test4
	(getIntersectionPoint (limitedline-l limitedline-test5) (limitedline-l limitedline-test4)))

(define (outIntersection ll1 ll2 p) (
	if (equal? p #f)
	#f
	(and (pointInSpicificLine ll1 p) (pointInSpicificLine ll2 p) (not (equalPoints (limitedline-p2 ll2) p)) (not (equalPoints (limitedline-p1 ll2) p)))
	))
(define limitedline-test6 (getLine (point 0 1) (point 4 1)))
(define limitedline-test7 (getLine (point 0 3) (point 0 0)))

(outIntersection limitedline-test6 limitedline-test7
	(getIntersectionPoint (limitedline-l limitedline-test6) (limitedline-l limitedline-test7)))

(define (topLeftPoint shape maxPoint) (
	if (null? shape)
		maxPoint
		(if (null? maxPoint) 
			(let ([maxPoint (car shape)])
				(topLeftPoint (cdr shape) maxPoint)
				)
			(cond [(> (point-y (car shape)) (point-y maxPoint)) (topLeftPoint (cdr shape) (car shape))]
				[(and (equal? (point-y (car shape)) (point-y maxPoint)) (< (point-x (car shape)) (point-x maxPoint)))
					 (topLeftPoint (cdr shape) (car shape))]
				[#t (topLeftPoint (cdr shape) maxPoint)]
					 )
			)
	))
(define shape (list (point 1 1) (point 1 0) (point 0 0) (point 0 1)))
(topLeftPoint shape null)

(define (makeGoodShape shape maxPoint)(
	if (null? shape)
	maxPoint

	(if (equal? (car shape) maxPoint)
		shape
		(makeGoodShape (append (cdr shape) (list (car shape))) maxPoint)
	)))
(makeGoodShape shape (topLeftPoint shape null))

(define (getShiftCor p1 p2) (
	HighWidth ( - (point-x p1) (point-x p2)) ( - (point-y p1) (point-y p2)) 
	))

(getShiftCor (point 0 2) (point 0 1))

(define (removePoints shape listOfPoint) (
	if (null? listOfPoint)
		shape
		(removePoints (remove (car listOfPoint) shape) (cdr listOfPoint))
	))
(removePoints (list (point 2 1) (point 2 0.5) (point 2 0) (point 1 0) (point 1 1))  (list (point 2 1) (point 1 1) ))

(define (shiftShape shape shift)(
	if (null? shape)
		'()
		(cons (point (- (point-x (car shape)) (HighWidth-x shift))(- (point-y (car shape)) (HighWidth-y shift)))
			(shiftShape (cdr shape) shift))
	))


(define shape1 (list (point 2 1) (point 2 0) (point 1 0) (point 1 1)))
 
 (shiftShape (makeGoodShape shape1 (topLeftPoint shape1 null)) 
 	(getShiftCor (car (makeGoodShape shape1 (topLeftPoint shape1 null)) )
 	 (car (makeGoodShape shape (topLeftPoint shape null)) )))

(define shape2 (list (point 2 1) (point 2 0.5) (point 2 0) (point 1 0) (point 1 1)))
(define (removeLinePoints shape shapeMain) (
	if (null? shape)
	'()
	(if (null? (cdr shape))
		'()
 	(if (null? (cddr shape))
 		(if (pointInSpicificLine (getLine (car shape) (car shapeMain))  (cadr shape))
 			(cons (car shape) '())
 			shape
 			)
 		(if (pointInSpicificLine (getLine (car shape)  (caddr shape)) (cadr shape))
 			(cons (car shape) (removeLinePoints (cddr shape) shapeMain))
 			(cons (car shape) (removeLinePoints (cdr shape) shapeMain))
 			)
 	))))

(removeLinePoints shape2 shape2)

(define (clearRepeatedPoints shape shapeMain)(
	if (null? shape)
	'()
	(if (null? (cdr shape))
		(if (equalPoints (car shape) (car shapeMain))
			(clearRepeatedPoints (cdr shape) shapeMain)
			(cons (car shape) (clearRepeatedPoints (cdr shape) shapeMain) )
			)
		(if (equalPoints (car shape) (cadr shape))
			(clearRepeatedPoints (cdr shape) shapeMain)
			(cons (car shape) (clearRepeatedPoints (cdr shape) shapeMain))
			)

	)))

(define shape4 (list (point 2 1) (point 2 0) (point 1 0) (point 1 1) (point 2 1) (point 2 1)))
(clearRepeatedPoints shape4 shape4)


(define (lineToShapeState ll2 shape shapeMain addList removeList startedPointBefore)(
	if (null? shape)
		(list addList removeList)
		(let* ([endPoint (null? (cdr shape))]
			[ll1 (if endPoint
					(getLine (car shape) (car shapeMain))
					(getLine (car shape) (cadr shape))
				)]
			[nextShape (cdr shape)]
			[isItFirstLine (equalPoints (limitedline-p1 ll2) (car shapeMain))]
			[isItShapeFirstLine (equalSpLines ll1 (getLine (car shapeMain) (cadr shapeMain)))]
			[listOfStartPoint (list (limitedline-p1 ll2))]
			[listOfEndPoint (list (limitedline-p2 ll2))]
			)
		(cond [(match2Lines ll1 ll2)  (if (and isItFirstLine isItShapeFirstLine) 
											(list '() (append removeList listOfStartPoint listOfEndPoint))
											(if isItFirstLine
												#f
												(list '() (append removeList listOfStartPoint listOfEndPoint))
											))]
			[(halfMatch2LinesByStart ll1 ll2) (list listOfEndPoint listOfStartPoint)]
			[(halfMatch2LinesByEnd ll1 ll2) (list '() '())]
			[#t (let ([intersectionPoint (getIntersectionPoint (limitedline-l ll1) (limitedline-l ll2))])
				(cond [(endIntersection ll1 ll2 intersectionPoint) 
							(lineToShapeState ll2 nextShape shapeMain (append addList listOfEndPoint) removeList startedPointBefore)]
						[(and (startIntersection ll1 ll2 intersectionPoint) (not startedPointBefore))
							(lineToShapeState ll2 nextShape shapeMain addList removeList #t)]
						[(and (startIntersection ll1 ll2 intersectionPoint) startedPointBefore)
						 	(lineToShapeState ll2 nextShape shapeMain (append addList listOfStartPoint) removeList startedPointBefore)]
						[(outIntersection ll1 ll2 intersectionPoint) #f]
						[#t (lineToShapeState ll2 nextShape shapeMain (append addList listOfEndPoint) removeList startedPointBefore)]
					))]))))

(define shape10 (list (point 0 1) (point 1 1) (point 1 0) (point 0 0) ))

(define limoLine1 (getLine (point 0 1) (point 1 1)))
(define limoLine2 (getLine (point 1 1) (point 1 0)))
(define limoLine3 (getLine (point 0 0) (point 0 1)))
(define limoLine4 (getLine (point 0 1) (point 0 0)))
(define limoLine5 (getLine (point 0 1) (point -1 0)))
(define limoLine6 (getLine (point 0 1) (point 1 0)))


; (lineToShapeState limoLine1 shape10 shape10 null null #f)
; (lineToShapeState limoLine2 shape10 shape10 null null #f)
; (lineToShapeState limoLine3 shape10 shape10 null null #f)
; (lineToShapeState limoLine4 shape10 shape10 null null #f)
; (lineToShapeState limoLine6 shape10 shape10 null null #f)

(define (shapeToShapeState smallShape smallShapeMain mainShape addList removeList)(
	if (null? smallShape)
		(let* ([mainShapeAfterDelete (removePoints mainShape removeList)]
				[mainShapeAfterAdd (append mainShapeAfterDelete addList)]
				[mainShapeAfterDeletingRepeatedPoint (clearRepeatedPoints mainShapeAfterAdd mainShapeAfterAdd)]
				[mainShapeAfterFixing (removeLinePoints mainShapeAfterDeletingRepeatedPoint mainShapeAfterDeletingRepeatedPoint)]
				[newMainShape (makeGoodShape mainShapeAfterFixing (topLeftPoint mainShapeAfterFixing null))]
			)	newMainShape)

	(let* ([endPoint (null? (cdr smallShape))]
		[ll (if endPoint
					(getLine (car smallShape) (car smallShapeMain))
					(getLine (car smallShape) (cadr smallShape))
				)]
		[newShapeNeedFix (lineToShapeState ll mainShape mainShape null null #f)]
		)
		(if newShapeNeedFix 
			(shapeToShapeState (cdr smallShape) smallShapeMain mainShape 
				(append (car newShapeNeedFix) addList) (append (cadr newShapeNeedFix) removeList))
				#f
			))


	))
(define debug (getLine (point 2 0) (point 0 0)))
(define limoLine10 (getLine (point 0 2) (point 1 2)))
(define limoLine11 (getLine (point 1 2) (point 1 1)))
(define limoLine12 (getLine (point 1 1) (point 0 1)))
(define limoLine13 (getLine (point 0 1) (point 0 2)))

(define shape12 (list (point 0 2) (point 2 2) (point 2 0) (point 0 0) ))
(define shape13 (list (point 0 2) (point 1 2) (point 1 1) (point 0 1) ))
(define shape14 (list (point 1 2) (point 2 2) (point 2 1) (point 1 1) ))
(define shape15 (list (point 0 1) (point 2 1) (point 2 0) (point 0 0) ))
(define shape16 (list (point 0 1) (point 2 1) (point 2 0) (point 0 10) ))



; (halfMatch2LinesByStart debug limoLine10)
(print "test")
(lineToShapeState limoLine10 shape12 shape12 null null #f)
(lineToShapeState limoLine11 shape12 shape12 null null #f)
; ; limoLine12 
(lineToShapeState limoLine12 shape12 shape12 null null #f)

(lineToShapeState limoLine13 shape12 shape12 null null #f)
; (getLine (point 0.5 1) (point 0.5 0.5))
; (getIntersectionPoint (limitedline-l debug) (limitedline-l limoLine12))
; (outIntersection debug limoLine11 (getIntersectionPoint (limitedline-l debug) (limitedline-l limoLine11)))
; (pointInLine (limitedline-l debug) (point 1 0) )
; (pointInSpicificLine debug (point 1 0) )
; (define shape11 (list (point 0 1) (point 0.5 1) (point 0.5 0) (point 0 0.5) ))
; (shapeToShapeState shape13 shape13 shape13 null null)

; (shapeToShapeState shape13 shape13 shape12 null null)
; (shapeToShapeState shape14 shape14 (shapeToShapeState shape13 shape13 shape12 null null) null null)
(shapeToShapeState shape15 shape15 (shapeToShapeState shape14 shape14 (shapeToShapeState shape13 shape13 shape12 null null) null null)
 null null)

; (print "test")
; (pointInSpicificLine (getLine (point 1 1 ) (point 2 2 )) (point 1.5 1.5)) ; given #t  true
; (pointInSpicificLine (getLine (point 1 1 ) (point 2 2 )) (point -1 1))  ; given #f  true


(define (mainFunc mainShape listOfShapes currentShape) (
	cond [(null? mainShape) '()]
		[(equal? (length listOfShapes)  currentShape) #f]
		[#t (let (
			[newShape (shapeToShapeState (list-ref listOfShapes currentShape) (list-ref listOfShapes currentShape)  mainShape null null)])
			(if (equal? newShape #f)
				(mainFunc mainShape listOfShapes (+ currentShape 1))
				(let ([remainingShape (mainFunc newShape (remove (list-ref listOfShapes currentShape) listOfShapes) 0)])
						(if (equal? remainingShape #f)
							(mainFunc mainShape listOfShapes (+ currentShape 1))
							(cons (list-ref listOfShapes currentShape) remainingShape))
					)

				)
			)]
	))


(mainFunc shape12 (list shape14 shape15 shape16 shape13) 0)


