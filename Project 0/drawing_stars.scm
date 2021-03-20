;;;; This file is project 0 for CS 314 for Spring 2021, taught by Prof. Francisco

;;;; The assignment is to fill in the definitions below, adding your code where ever
;;;; you see the comment 
     ;;  replace this line
;;;; to make each function do what its comments say it should do.  You
;;;; may replace such a line with as many lines as you want to.  You may
;;;; also add your own functions, as long as each function has a
;;;; comment like the ones below.  You may not make any other changes
;;;; to this code.

;;;; see end of file for some examples that will run ONCE THE FUNCTION add-check
;;;; IS FILLED IN

;;;; See the assignment on Sakai for more examples (these may require
;;;; more than add-check to be filled in) and further information,
;;;; including due date.

;;;; code for a program to create closures that generate figures out
;;;; of characters.

;;; figures: a figure is represented by a list of 3 elements: func,
;;; numrows, and numcols where:
;;;   func is a function (ie a closure) of two parameters: row, column
;;;     that returns the character at the given row and column of the
;;;     figure.  If row is out of bounds ie row<0 or row>=numrows, or
;;;     similarly for col, func returns the character #\. (a period
;;;     character)
;;;   numrows is the number of rows in the figure
;;;   numcols is the number of columns in the figure



;;; The following are functions to create and access a figure. Note that
;;; make-figure adds bounds checking to func
(define (make-figure func numrows numcols)
    (list (add-check func numrows numcols) numrows numcols))
(define (figure-func figure) (car figure))
(define (figure-numrows figure)(cadr figure))
(define (figure-numcols figure)(caddr figure))


;;; forn takes three arguments:  start and stop, which are numbers, and func 
;;; which is a function of one argument.
;;; forn calls func several times, first with the argument start, then with start+1
;;; then ... finally with stop.  It returns a list of the values of the calls. 
;;; If start>stop, forn simply returns the empty list without doing any calls to func.
(define (forn start stop func)
  (if (> start stop) '()
      (let ((first-value (func start)))
        (cons first-value (forn (+ 1 start) stop func)))))


;;; range-check takes 4 arguments:  row, numrows, col, numcols It checks if
;;;  0 <= row < numrows and similarly for col and numcols.  If both row and col are in 
;;;  range, range-check returns #t.  If either are out of range, rangecheck  returns #f
(define (range-check row numrows col numcols)
  (not (or (< row 0) (< col 0) (>= row numrows) (>= col numcols))))


;;; add-check takes 3 arguments: func, numrows and numcols.  func is a
;;; function of two numbers, row and col.  add-check returns a new
;;; function, which we will refer to here as func2.  Like func, func2 takes
;;; a row number and a column number as arguments. func2 first calls
;;; range-check to do a range check on these numbers against numrows
;;; and numcols. If row or col is out of range func2 returns #\.,
;;; otherwise it returns the result of (func row col).  You can think of
;;; func2 as a "safe" version of func, like the function returned by
;;; null-safe in Resources > Langauge_Info > Scheme > null-safe.scm except that a
;;; "bad", i.e. out of range, argument to func here will not necessarily
;;; crash scheme the way (car '( )) would.
(define (add-check func numrows numcols)
  (lambda (row col)
    ; check if row and col is in range
    (if (range-check row numrows col numcols)
        (func row col)
        #\.
        )
    )
  )


;;; display-window prints out the characters that make up a rectangular segment of the figure
;;;    startrow and endrow are the first and last rows to print, similarly for startcol and endcol
;;; The last thing display-window does is to call (newline) to print a blank line under the figure.
(define (display-window start-row stop-row start-col stop-col figure)
  (forn start-row stop-row 
         (lambda (r)
           (forn start-col stop-col 
                  (lambda (c)
                    (display ((figure-func figure) r c))))
           (newline)))
  (newline))


;;; charfig takes one argument, a character, and returns a 1-row, 1-column figure consisting of that character
(define (charfig char)
  (make-figure (lambda (row col)
		  char)
		1 1))


;;; sw-corner returns a figure that is a size x size square, in which
;;; the top-left to bottom-right diagonal and everything under it is
;;; the chracter * and everything above the diagonal is the space
;;; character
(define (sw-corner size)
  (make-figure (lambda (row col)
                  (if (>= row col)
                      #\*
                      #\space))
		size
                size))


;;; repeat-cols returns a figure made up of nrepeat copies of
;;; figure, appended horizontally (left and right of each other)
(define (repeat-cols nrepeat figure)
  (make-figure (lambda (row col)
		  ((figure-func figure) row (modulo col (figure-numcols figure))))
		(figure-numrows figure) 
                (* nrepeat (figure-numcols figure)) 
                ;; the function just calls the function that repeat-cols received, but 
                ;; uses modulo to select the right position.
		))


;;; repeat-rows returns a figure made up of nrepeat copies
;;; of a figure, appended vertically (above and below each other)
(define (repeat-rows nrepeat figure)
  (make-figure (lambda (row col)
                 ; Same as repeat-cols, but for row instead
		  ((figure-func figure) (modulo row (figure-numrows figure)) col))
               (* nrepeat (figure-numrows figure)) (figure-numcols figure)
               )
  )


;;; append cols returns the figure made by appending figureb to the
;;; right of figurea the number of rows in the resulting figure is the
;;; smaller of the number of rows in figurea and figureb
(define (append-cols figurea figureb)
  ; Check which figure has the smaller number of rows
  (if (< (figure-numrows figurea) (figure-numrows figureb))
      (make-figure (lambda (row col)
                     ; Check if col is in the figurea 'zone'
                     (if (< col (figure-numcols figurea))
                         ; If it is, draw figurea
                         ((figure-func figurea) row col)
                         ; If it isn't, draw figureb in the correct position
                         ((figure-func figureb) row (- col (figure-numcols figurea)))))
                   (figure-numrows figurea) (+ (figure-numcols figurea) (figure-numcols figureb)))
      (make-figure (lambda (row col)
                     (if (< col (figure-numcols figurea))
                         ((figure-func figurea) row col)
                         ((figure-func figureb) row (- col (figure-numcols figurea)))))
                   (figure-numrows figureb) (+ (figure-numcols figurea) (figure-numcols figureb)))
            )
  )


;;; append-rows returns the figure made by appending figureb below figurea
;;; the number of columns in the resulting figure is the smaller of the number of columns in figurea
;;; and figternb
(define (append-rows figurea figureb)
  ; Check which figure has the smaller number of cols
  (if (< (figure-numcols figurea) (figure-numcols figureb))
      (make-figure (lambda (row col)
                     ; Check if row is in the figurea 'zone'
                     (if (< row (figure-numrows figurea))
                         ; If it is, draw figurea
                         ((figure-func figurea) row col)
                         ; If it isn't, draw figureb in the correct position
                         ((figure-func figureb) (- row (figure-numrows figurea)) col)))
                   (+ (figure-numrows figurea) (figure-numrows figureb)) (figure-numcols figurea))
      (make-figure (lambda (row col)
                     (if (< row (figure-numrows figurea))
                         ((figure-func figurea) row col)
                         ((figure-func figureb) (- row (figure-numrows figurea)) col)))
                   (+ (figure-numrows figurea) (figure-numrows figureb)) (figure-numcols figureb))
            )
  )


;;; flip-cols returns a figure that is the left-right mirror image of figure
(define (flip-cols figure)
  (make-figure (lambda (row col)
                 ; Flip the position of col
                 ((figure-func figure) row (- (- (figure-numcols figure) col) 1)))
               (figure-numrows figure) (figure-numcols figure)
               )
)


;;; flip-rows returns a figure that is the up-down mirror image of figure
(define (flip-rows figure)
  (make-figure (lambda (row col)
                 ; Flip the position of row
                 ((figure-func figure) (- (- (figure-numrows figure) row) 1) col))
               (figure-numrows figure) (figure-numcols figure)
               )
  )


;;; paste draws the composition of two figures
;;; any coordinate in range of either figure should draw,
;;;   and a space should appear only if that is the case for both figures
(define (paste figurea figureb)
  (make-figure (lambda (row col)
                 ; Check both functions for their output
                 ; If either function returns a star, return a star
                 ; Else, return a space
                 (if (or (eq? #\* ((figure-func figurea) row col)) (eq? #\* ((figure-func figureb) row col)))
                     #\*
                     #\space))
               (+ (figure-numrows figurea) (figure-numrows figureb)) (+ (figure-numrows figurea) (figure-numcols figureb)))
  )


;;; rotateCCW causes the figure to be drawn with a 90 degree counter clockwise
;;;   rotation
(define (rotateCCW figure)
  (make-figure (lambda (row col)
                 ; Swap row and col and adjust the new col appropriately
                 ((figure-func figure) col (- (- (figure-numrows figure) row) 1)))
               (figure-numcols figure) (figure-numrows figure))
  )


;;; rotateCCW causes the figure to be drawn with a 90 degree clockwise rotation
(define (rotateCW figure)
  (make-figure (lambda (row col)
                 ; Swap row and col and adjust the new row appropriately
                 ((figure-func figure) (- (- (figure-numcols figure) col) 1) row))
               (figure-numcols figure) (figure-numrows figure)
               )
  )


;;; excludewindow excludes some segment of a figure, replacing any character
;;;   within the figure's normal draw area that is in the exclusion zone
;;;   with a '#\space' character
;;; the exclusion window is rectangular only
(define (excludeWindow minrow maxrow mincol maxcol figure)
  (make-figure (lambda (row col)
                 ; If row or col are not within the ranges of minrow, maxrow,
                 ; mincol, or maxcol, return a space
                 ; Else, call the figure function normally
                 (if (or (< row minrow) (> row maxrow) (< col mincol) (> col maxcol))
                     #\space
                     ((figure-func figure) row col)))
                 (figure-numrows figure) (figure-numcols figure))
  )



;;;; some examples thst should work after just add-check is filled in
;;;; above.  (Remove the ;'s at the start of the lines below.)
(define fig1 (sw-corner 4))
;(display-window 0 3 0 3 fig1)

(define fig2 (repeat-cols 3 fig1))
;(display-window 0 4 0 12 fig2)

;(display-window 0 12 0 4 (repeat-rows 3 fig1))
;(display-window 0 12 0 12 (append-cols fig1 fig1))
;(display-window 0 3 0 3 (flip-rows fig1))

;(display-window 0 3 0 7 (append-cols fig1 (flip-rows (flip-cols fig1)) ))

;(display-window 0 3 0 3 (rotateCW fig1))
;(display-window 0 3 0 3 (rotateCW (rotateCW (rotateCW fig1))) )

;(display-window 0 3 0 3 (paste fig1 (flip-rows fig1)))
;(display-window 0 3 0 3 (excludeWindow 2 3 2 3 fig1))



;PDF Test Cases
(define ca (charfig #\a))
(define cb (charfig #\b))
(define ab (append-cols ca cb))
(define cde (append-cols (charfig #\c)
 (append-cols (charfig #\d)
 (charfig #\e))))
(define abcd (append-rows ab cde))

;(display-window 0 0 0 0 ca)
;(display-window 0 0 0 1 ca)
;(display-window 0 1 0 1 ca)
;(display-window 0 0 0 0 cb)
;(display-window 0 1 0 1 ab)
;(display-window 0 2 0 2 abcd)
 ;note that cde gets truncated to 2 columns
 ;because ab has only 2 columns

;(display-window 0 1 0 2 cde)
;(display-window 0 3 0 3 (sw-corner 4))
;(display-window 0 3 0 3 (flip-rows(sw-corner 4)))
;(display-window 0 3 0 3 (flip-cols(sw-corner 4)))
;(let ((f1 (append-rows ab (flip-cols ab))))
;(display-window 0 2 0 4 (append-cols f1 (flip-rows f1))))
