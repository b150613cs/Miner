(defpackage :miner
  (:nicknames :miner)
  (:use :clamp :experimental :iter)
  (:shadowing-import-from :experimental
     :def :fn :mac :defmemo :defmethod :coerce
     :repeat :while :until :with :in :summing))

(in-package :miner)
(syntax:use-syntax :clamp)

(defparameter prob* 0.2 "The probability a square will have a mine on it.")
(defparameter rows* 10 "The number of rows.")
(defparameter cols* 10 "The number of columns.")

(defparameter print-board-ai* nil
  "If set to T the board will be printed after every turn for an ai-player.")

(defparameter show* nil
  "If set to T the non-revealed squares will be shown when printing a board.")

(defparameter moores* (ret result nil
                        (upto i -1 1
                          (upto j -1 1
                            (unless (and (is i 0) (is j 0))
                              (push (list i j) result)))))
  "The moore's neighborhood.")

;; In case we want other kinds of boards that are not random.
(deftem (board (:conc-name nil))
  grid
  (revealed (make-array (list rows* cols*) :initial-element nil))
  left)

(deftem (rand-board (:include board)))

(defstruct (point (:conc-name nil) (:type list) (:constructor make-point (row col)))
  row col)

(deftem player)
(deftem human-player)
(deftem (ai-player (:conc-name nil))
  (kb (make-array (list rows* cols*) :initial-element nil)))

;;; We need to override get to make it easier to access into boards and
;;; arrays when we have a point.

(defmethod get ((arr array) (p list))
  (apply #'aref arr p))

(defmethod (setf get) (val (arr array) (p list))
  (= (apply #'aref arr p) val))

(defmethod get ((b board) (p list))
  (get b!grid p))

(defmethod (setf get) (val (b board) (p list))
  (= (get b!grid p) val))

(defmethod get ((b board) (p number))
  (get b!grid p))

(defmethod (setf get) (val (b board) (p number))
  (setf (get b!grid p) val))

(defmethod initialize-instance :after ((board rand-board) &key)
  "Initialize all of the squares. Each square has a probability of prob* of having a mine."
  ;; Set up the mines
  (= board!grid (ret result (make-array (list rows* cols*))
                  (up i 0 rows*
                    (up j 0 cols*
                      (= result.i.j (if (< (rand 1.0) prob*) t nil))))))
  ;; Add all of the numbers.
  (zap #'calc-neighbors board!grid)
  ;; Count the number of non-mine squares in the game.
  (= board!left
     (summing s
       (up i 0 rows*
         (up j 0 cols*
           (s (~is board.i.j t))))))
  board)

(def p+ (p1 p2)
  "Adds two points together."
  (map #'+ p1 p2))

(def valid (p)
  "Is this point valid?"
  (and (<= 0 p!row (dec rows*))
       (<= 0 p!col (dec cols*))))

(def calc-neighbors (grid)
  "Places the number of adjacent squares that are mines at each
   location that isn't a mine."
  (up i 0 rows*
    (up j 0 cols*
      (let p (make-point i j)
        (when (and (valid p) (not grid.p))
          (= grid.p
             (ret result 0
               (each neighbor (map [p+ _ p] moores*)
                 (when (and (valid neighbor) (is t grid.neighbor))
                   (++ result)))))))))
  grid)

(def click (board square)
  "'Click' on the given square. Reveal it. If it is a zero reveal all
   of the squares around it. Returns a list of information. Each
   element of that list is a two element list whose first element is
   the point revealed and the second element is T if there was a mine
   on that square."
  (accum a
    (let queue (list square)
      (while queue
        (let next (pop queue)
          (unless board!revealed.next
            (set board!revealed.next)
            (a (list next board.next))
            (when (~is board.next t)
              (-- board!left))
            (when (is board.next 0)
              (each neighbor (map [p+ _ next] moores*)
                (when (valid neighbor)
                  (push neighbor queue))))))))))

(defmethod print-object ((b board) stream)
  (let *standard-output* stream
    (up i 0 rows*
      (up j 0 cols*
        (if (or show* b!revealed.i.j)
            (pr b.i.j)
            (pr "X")))
      (prn))))

(def play (board player)
  "Play a game with the given player."
  (with (clicked-mine nil turn 0)
    (while (and (> board!left 0) (not clicked-mine))
      (++ turn)
      (display board player)
      (let move (read-move board player)
        (each revealed (click board move)
          (when (is (cadr revealed) t)
            (= clicked-mine t))
          (tell revealed player))))
    (display-result board player clicked-mine turn)))

(defgeneric display (board player)
  (:documentation "Display the board to the given player.")
  (:method (x y)))

(defgeneric tell (info player)
  (:documentation "Tell some information to the player.")
  (:method (x y)))

(defmethod display ((b board) (p player))
  (prf "There are ~A squares left.~%" b!left)
  (prn b)
  b)

(defmethod display :around ((b board) (a ai-player))
  (when print-board-ai*
    (call-next-method))
  b)

(defmethod read-move ((b board) (p human-player))
  "Read in a move from a human player"
  (with (*standard-input*  *query-io*
         *standard-output* *query-io*)
    (pr "Enter the row: ")
    (let row (read)
         (pr "Enter the col: ")
         (let col (read)
              (make-point row col)))))

(defmethod display-result (board (player human-player) result turn)
  "Display the result for a human player."
  (prf "~:[You won!.~; You lost!~]~%" result))

(defmethod read-move ((b board) (p ai-player))
  "Read in a move from an ai-player."
  ;; Gather all of the squares that haven't been revealed yet.
  (let sqrs (permutate
              (accum a
                (up i 0 rows*
                  (up j 0 cols*
                    (unless b!revealed.i.j
                      (a (list i j)))))))
    ;; Whenever we have learned the position of a mine, loop again.
    (let loop t
      (while loop
        (wipe loop)
        (each sqr sqrs
          ;; If the probability is 1, we know it is a mine.
          (when (is (prob sqr p!kb) 1.0)
            (set loop)
            (= p!kb.sqr t)))))
    ;; Find the square with the smallest probability of being a mine.
    (best #'< sqrs [prob _ p!kb])))

(defmethod display-result (board (p ai-player) result turn)
  "Return the result for an ai-player. Two values are returned. If the
   player hit a mine the first value will be T otherwise NIL. The
   second number will be the last turn number."
  (values result turn))

(defmethod tell (what (p ai-player))
  "Tell the ai-player WHAT. WHAT should be a list whose first value is
   a location and whose second value is the number found on that
   square."
  (let (square num) what
    (= p!kb.square num)))

(def prob (square kb)
  "Returns the probability of there being a mine on SQUARE given the
   knowledge base KB."
  (if kb.square
      (if (is kb.square t) 1 0)
      (ret prob prob*
        ;; For each neighbor to the square we are looking at
        (each neighbor (map [p+ _ square] moores*)
          ;; if we know its number
          (when (and (valid neighbor) kb.neighbor (~is kb.neighbor t))
            (with (unknown 0 mines 0)
              ;; count the number of mines and the number of unknown squares next to it.
              (each next (map [p+ _ neighbor] moores*)
                (when (and (valid next) (not kb.next))
                  (++ unknown))
                (when (and (valid next) (is kb.next t))
                  (++ mines)))
              ;; then use Bayes' theorem to update the probability of
              ;; the square in question being a mine.
              (with (agb  (bernoulli (dec unknown) (- kb.neighbor mines 1) prob*)
                     b    prob
                     agnb (bernoulli (dec unknown) (- kb.neighbor mines) prob*)
                     nb   (- 1 prob))
                (= prob
                   (/ (* agb b)
                      (+ (* agb b) (* agnb nb)))))))))))

(def bernoulli (total num p)
  "Bernoulli trials. Returns the probability of obtaining NUM heads
   out of TOTAL coin flips, if the probability of flipping heads is P."
  (if (or (< num 0) (< total num))
      0
      (* (expt p num)
         (expt (- 1 p) (- total num))
         (binom total num))))

(def fact (n)
  "Returns N factorial."
  (iter (for i from 1 to n) (multiply i)))

(def binom (r k)
  "Binomial coefficents. Returns choose(R, K)."
  (/ (fact r)
     (fact k)
     (fact (- r k))))

(def permutate (xs)
  "Randomly shuffle the elements in a list."
  (accum a
    (while xs
      (let next (rand-elt xs)
        (a next)
        (zap [rem next _ :count 1] xs)))))

(def play-as-human ()
  "Play a game as a human."
  (play (make-rand-board) (make-human-player)))

(def play-as-ai ()
  "Have the ai play a game."
  (play (make-rand-board) (make-ai-player)))
