(in-package #:pyramid-solver)

;;; Card and Deck definitions
(defvar *rank-characters* "A23456789TJQK"
  "All valid card rank characters.")

(defvar *suit-characters* "cdhs"
  "All valid card suit characters.")

(defvar *all-cards* (loop for suit across *suit-characters*
                          nconc (loop for rank across *rank-characters*
                                      collect (format nil "~C~C" rank suit)))
  "All the cards in a standard 52-card deck.")

(defun cardp (obj)
  "Cards are two-letter strings containing a rank followed by a suit."
  (and (stringp obj)
       (= 2 (length obj))
       (find (char obj 0) *rank-characters*)
       (find (char obj 1) *suit-characters*)))

(defun card-value (card)
  "Return the numeric value of the card: A is always 1, J=11, Q=12, and K=13."
  (1+ (position (char card 0) *rank-characters*)))

(defun missing-cards (list)
  "Return a list of the standard deck cards that are missing from LIST."
  (remove-if (lambda (card)
               (find card list :test #'equal))
             *all-cards*))

(defun duplicate-cards (list)
  "Return a list of the cards that are duplicated in LIST."
  (remove-if (lambda (obj)
               (or (not (cardp obj))
                   (= 1 (count obj list :test #'equal))))
             list))

(defun malformed-cards (list)
  "Return a list of the objects in LIST that aren't cards."
  (remove-if #'cardp list))

(defun standard-deck-p (obj)
  "Standard Decks are proper lists containing exactly all the cards of a 52-card deck."
  (and (listp obj)
       (handler-case (= 52 (list-length obj))
         (type-error () nil)) ; dotted lists can't be standard decks
       (null (missing-cards obj))))

(defun string->card-list (string)
  "Return a list of the cards in STRING."
  (loop for i from 0 below (1- (length string))
        for substring = (subseq string i (+ i 2))
        when (cardp substring)
        collect substring))

(defun deck->string (deck)
  (apply #'format nil "            ~A
          ~A  ~A
        ~A  ~A  ~A
      ~A  ~A  ~A  ~A
    ~A  ~A  ~A  ~A  ~A
  ~A  ~A  ~A  ~A  ~A  ~A
~A  ~A  ~A  ~A  ~A  ~A  ~A
~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A" deck))



;;; Definitions and precalculated data on the 28-card pyramid structure
(deftype pyramid-flags ()
  "Bits indicating the existence of the first 28 cards of a standard deck, which form the pyramid.
The bits refer to the pyramid cards in this order:
            00
          01  02
        03  04  05
      06  07  08  09
    10  11  12  13  14
  15  16  17  18  19  20
21  22  23  24  25  26  27"
  '(unsigned-byte 28))

(deftype pyramid-index ()
  "An index into the 28 pyramid cards."
  '(integer 0 27))

(deftype pyramid-id ()
  "An ID referring to one of the 1430 valid PYRAMID-FLAGS values."
  '(integer 0 1429))

(defun all-pyramid-flags ()
  "Calculate all valid PYRAMID-FLAGS values.  The rule is: a pyramid card can't be removed until
all the cards covering it from below are removed."
  (labels ((previous-row-optional-indexes (row-flags num-cards-in-row)
             "Return the card offsets of the previous row that don't need to have a card."
             (loop for i from 0 below (1- num-cards-in-row)
                   when (zerop (mask-field (byte 2 i) row-flags))
                   collect i))
           (previous-rows (row-flags num-cards-in-row)
             "Return all valid card existence bit flags for the previous row."
             (loop with indexes = (previous-row-optional-indexes row-flags num-cards-in-row)
                   with all-cards-mask = (1- (ash 1 (1- num-cards-in-row)))
                   ;; flags iterates through all combinations of optional cards
                   for flags from 0 below (ash 1 (length indexes))
                   for masks = (loop for index in indexes
                                     for i from 0
                                     when (logbitp i flags)
                                     collect (ash 1 index))
                   collect (logandc2 all-cards-mask (reduce #'logior masks))))
           (pyramid-flags (&rest row-flags)
             "Return a PYRAMID-FLAGS given card existence bits for each row."
             (reduce #'logior (mapcar #'ash row-flags '(0 1 3 6 10 15 21)))))
    (let ((all ()))
      ;; for all possible combinations of existing cards on the bottom row, we calculate all
      ;; possible combinations of existing cards for each previous row, and then combine them all
      ;; into valid PYRAMID-FLAGS values
      (dotimes (row7 (ash 1 7) (sort (coerce all 'vector) #'<))
        (dolist (row6 (previous-rows row7 7))
          (dolist (row5 (previous-rows row6 6))
            (dolist (row4 (previous-rows row5 5))
              (dolist (row3 (previous-rows row4 4))
                (dolist (row2 (previous-rows row3 3))
                  (dolist (row1 (previous-rows row2 2))
                    (push (pyramid-flags row1 row2 row3 row4 row5 row6 row7) all)))))))))))

(defvar *pyramid-flags* (all-pyramid-flags)
  "All valid PYRAMID-FLAGS values.")

(defvar *pyramid-flags->id*
  (let ((ht (make-hash-table)))
    (dotimes (pyramid-id (length *pyramid-flags*) ht)
      (setf (gethash (svref *pyramid-flags* pyramid-id) ht) pyramid-id)))
  "A mapping from PYRAMID-FLAGS to PYRAMID-ID.")

(declaim (inline pyramid-flags->id))
(defun pyramid-flags->id (pyramid-flags)
  "Return the PYRAMID-ID that refers to the given PYRAMID-FLAGS."
  (declare (pyramid-flags pyramid-flags))
  (the pyramid-id (gethash pyramid-flags *pyramid-flags->id*)))

(defun pyramid-existing-indexes (pyramid-flags)
  "Return a list of all indexes of existing cards in PYRAMID-FLAGS."
  (loop for pyramid-index from 0 to 27
        when (logbitp pyramid-index pyramid-flags)
        collect pyramid-index))

(defvar *pyramid-existing-indexes*
  (map 'vector #'pyramid-existing-indexes *pyramid-flags*)
  "For each PYRAMID-FLAGS, a list of all PYRAMID-INDEXes of existing cards.")

(defun pyramid-uncovered-indexes (pyramid-flags)
  "Return a list of all uncovered card indexes in PYRAMID-FLAGS.  An uncovered card exists on the
pyramid and has no cards covering it from below and preventing it from being removed."
  (loop for pyramid-index from 0 to 27
        when (eql (ash 1 pyramid-index)
                  (logand pyramid-flags
                          ;; bits set on the Nth card and the cards covering it
                          (svref #(#b1111111111111111111111111111
                                   #b0111111011111011110111011010
                                   #b1111110111110111101110110100
                                   #b0011111001111001110011001000
                                   #b0111110011110011100110010000
                                   #b1111100111100111001100100000
                                   #b0001111000111000110001000000
                                   #b0011110001110001100010000000
                                   #b0111100011100011000100000000
                                   #b1111000111000110001000000000
                                   #b0000111000011000010000000000
                                   #b0001110000110000100000000000
                                   #b0011100001100001000000000000
                                   #b0111000011000010000000000000
                                   #b1110000110000100000000000000
                                   #b0000011000001000000000000000
                                   #b0000110000010000000000000000
                                   #b0001100000100000000000000000
                                   #b0011000001000000000000000000
                                   #b0110000010000000000000000000
                                   #b1100000100000000000000000000
                                   #b0000001000000000000000000000
                                   #b0000010000000000000000000000
                                   #b0000100000000000000000000000
                                   #b0001000000000000000000000000
                                   #b0010000000000000000000000000
                                   #b0100000000000000000000000000
                                   #b1000000000000000000000000000)
                                 pyramid-index)))
        collect pyramid-index))

(defvar *pyramid-uncovered-indexes*
  (map 'vector #'pyramid-uncovered-indexes *pyramid-flags*)
  "For each PYRAMID-FLAGS, a list of all PYRAMID-INDEXex of uncovered cards.")



;;; Pyramid Soltaire game state definitions
(deftype state ()
  "The current state of a Pyramid Solitaire game, stored in 52 bits.  We only store the minimum we
need to derive the complete game state when given the deck of cards stored separately.
Bits  0-10: PYRAMID-ID - an ID referring the combination of cards remaining in the pyramid
Bits 11-12: CYCLE - how many times the player has recycled the waste pile
Bits 13-18: STOCK-INDEX - which card in the deck is the top of the stock pile
Bits 19-27: unused padding, all zeros, so bits 28-51 and STOCK-INDEX match the 52-card deck order
Bits 28-51: bit flags indicating which stock/waste cards remain"
  '(unsigned-byte 52))

(defconstant +initial-state+ (logior (ash #xffffff 28) (ash 28 13) 1429)
  "The initial state of every Pyramid Solitaire game, all cards in place.")

(declaim (inline state-pyramid-id))
(defun state-pyramid-id (state)
  "Return the PYRAMID-ID stored inside STATE (bits 0-10)."
  (the pyramid-id (mask-field (byte 11 0) (the state state))))

(deftype cycle ()
  "The number of times the player has recycled the waste pile."
  '(integer 0 2))

(declaim (inline state-cycle))
(defun state-cycle (state)
  "Return the CYCLE stored inside STATE (bits 11-12)."
  (the cycle (ldb (byte 2 11) (the state state))))

(deftype deck-flags ()
  "Bit flags for the existence of each card in the deck.
Bits 0-27 are PYRAMID-FLAGS, bits 28-51 are flags for the stock/waste cards."
  '(unsigned-byte 52))

(deftype deck-index ()
  "An index pointing to a card in the deck."
  '(integer 0 51))

(deftype stock-index ()
  "An index into DECK-FLAGS pointing to the top card of the stock pile.  Cards with higher index up
to 51 are the rest of the stock pile cards.  52 means the stock pile is empty."
  '(integer 28 52))

(declaim (inline state-stock-index))
(defun state-stock-index (state)
  "Return the STOCK-INDEX stored inside STATE (bits 13-18)."
  (the stock-index (ldb (byte 6 13) (the state state))))

(declaim (inline stock-empty-p))
(defun stock-empty-p (stock-index)
  "Return T if the stock pile is empty according to the STOCK-INDEX value."
  (eql 52 (the stock-index stock-index)))

(declaim (inline state-adjust-stock-index))
(defun state-adjust-stock-index (state)
  "Return a new copy of STATE making sure STOCK-INDEX is pointing to an existing card or empty."
  (declare (state state))
  (do ((stock-index (state-stock-index state) (1+ stock-index)))
      ((or (stock-empty-p stock-index)
           (not (zerop (logand state (the state (ash 1 stock-index))))))
       (the state (logior (the state (logand state #xffffffff81fff))
                          (the state (ash stock-index 13)))))
    (declare (stock-index stock-index))))

(deftype waste-index ()
  "An index into DECK-FLAGS pointing to the top card of the waste pile.  Cards with lower index
down to 28 are the rest of the waste pile cards.  27 means the waste pile is empty.  This can be
derived from STOCK-INDEX by counting down from it until you find an existing card in DECK-FLAGS."
  '(integer 27 51))

(declaim (inline waste-empty-p))
(defun waste-empty-p (waste-index)
  "Return T if the waste pile is empty according to the WASTE-INDEX value."
  (eql 27 (the waste-index waste-index)))

(declaim (inline state-waste-index))
(defun state-waste-index (state stock-index)
  "Derive the WASTE-INDEX based on the STATE and its STOCK-INDEX."
  (declare (state state) (stock-index stock-index))
  (do* ((i (1- stock-index) (1- i))
        (mask (ash 1 i) (ash mask -1)))
       ((or (waste-empty-p i) (not (zerop (logand mask state)))) i)
    (declare (waste-index i) (state mask))))
           
(deftype flags ()
  '(or pyramid-flags deck-flags))

(deftype index ()
  '(or pyramid-index deck-index))



;;; Precalculated data for a given deck of cards that the solvers will use.
;;; 1. Successor masks: LOGXOR a state with precalculated successor masks to get successor states.
;;; However, the state may still need to have its STOCK-INDEX adjusted afterwards.
;;; 2. H-cost: A heuristic function to estimate how many steps away a state is from a goal state.
;;; 3. Unclearable masks: LOGAND a state with precalculated unclearable masks, and if any result is
;;; zero, then we know we can't remove all the pyramid cards.  It might still be unclearable if the
;;; result is nonzero, but we know for sure it is unclearable if it is zero because it means there
;;; is a pyramid card and no matching card to remove it.
(defun card-values (deck)
  "Return a vector containing the numeric value of each card in DECK."
  (map 'vector #'card-value deck))

(defun successor-masks (card-values)
  "Return a data structure containing lists of XOR masks to make successor states for each state.
The vectors are indexed by the state's PYRAMID-ID -> STOCK-INDEX -> WASTE-INDEX -> CYCLE.  We use
this instead of a multidimensional array to save memory because most of the middle dimensions'
space is unused.  LOGXOR the state with each mask to get each successor state - however, we still
need to update the new state's STOCK-INDEX to point to an existing card."
  (declare (optimize speed (safety 0) (debug 1)))
  (loop with masks = (make-array 1430)
        for pyramid-id fixnum from 0 to 1429
        for pyramid-flags of-type pyramid-flags = (svref *pyramid-flags* pyramid-id)
        for uncovered-indexes of-type list = (svref *pyramid-uncovered-indexes* pyramid-id)
        for svec = (setf (svref masks pyramid-id) (make-array 53 :initial-element ()))
        do (labels ((prepend (mask list)
                      "Return the list with mask added in front unless the mask is null."
                      (if mask (cons mask list) list))
                    (mask1 (i)
                      "Return a mask where bit I is set to 1."
                      (the flags (ash 1 (the index i))))
                    (mask2 (i1 i2)
                      "Return a mask where bits I1 and I2 are set to 1."
                      (the flags (logior (mask1 i1) (mask1 i2))))
                    (mask1p (i)
                      "Return an XOR mask to remove the pyramid card at I from a state."
                      (logxor pyramid-id (pyramid-flags->id (logxor pyramid-flags (mask1 i)))))
                    (mask2p (i1 i2)
                      "Return an XOR mask to remove the pyramid cards at I1 and I2 from a state."
                      (logxor pyramid-id (pyramid-flags->id (logxor pyramid-flags (mask2 i1 i2)))))
                    (mask1s1p (is ip)
                      "Return an XOR mask to remove a stock card and a pyramid card from a state."
                      (logior (mask1 is) (mask1p ip)))
                    (kingp (i)
                      "Return T if the card at index I in the deck is a King."
                      (= 13 (the (integer 1 13) (svref card-values i))))
                    (matchp (i1 i2)
                      "Return T if the cards at index I1 and I2 in the deck add up to 13."
                      (= 13 (the (integer 2 26) (+ (the (integer 1 13) (svref card-values i1))
                                                   (the (integer 1 13) (svref card-values i2))))))
                    (pyramid-masks ()
                      "Calculate successor masks involving only the cards in the pyramid."
                      (loop for (i . others) on uncovered-indexes
                            nconc (if (kingp i)
                                      (list (mask1p i))
                                    (loop for j in others
                                          when (matchp i j)
                                          collect (mask2p i j)))))
                    (stock-masks ()
                      "Calculate successor masks involving the removal of one stock card."
                      (loop with masks = (make-array 53 :initial-element ())
                            for i fixnum from 28 to 51
                            do (if (kingp i)
                                   (push (mask1 i) (svref masks i))
                                 (dolist (j uncovered-indexes)
                                   (when (matchp i j)
                                     (push (mask1s1p i j) (svref masks i)))))
                            finally (return masks)))
                    (stock-waste-mask (is iw)
                      "Calculate a successor mask if the stock/waste cards can be removed together."
                      (unless (or (stock-empty-p is) (waste-empty-p iw) (not (matchp is iw)))
                        (mask2 is iw)))
                    (draw-mask (stock-index)
                      "Calculate a successor mask to draw a card from the stock pile if possible."
                      (declare (stock-index stock-index))
                      (when (< stock-index 52)
                        (the state (ash (logxor stock-index (1+ stock-index)) 13))))
                    (recycle-mask (stock-index cycle)
                      "Calculate a successor mask to recycle the waste pile if possible."
                      (declare (stock-index stock-index) (cycle cycle))
                      (when (and (= stock-index 52) (< cycle 2))
                        (the state (logior (the state (ash (logxor cycle (1+ cycle)) 11))
                                           (the state (ash (logxor 52 28) 13)))))))
             (declare (inline prepend mask1 mask2 mask1p mask2p mask1s1p kingp matchp))
             (loop with pmasks = (pyramid-masks)
                   with smasks = (stock-masks)
                   for stock-index fixnum from 28 to 52
                   for dspmasks = (prepend (draw-mask stock-index)
                                           (append (svref smasks stock-index)
                                                   pmasks))
                   for wvec = (setf (svref svec stock-index) (make-array 52 :initial-element ()))
                   do (loop for waste-index fixnum from 27 below stock-index
                            for wdspmasks = (prepend (stock-waste-mask stock-index waste-index)
                                                     (append (svref smasks waste-index)
                                                             dspmasks))
                            for cvec = (setf (svref wvec waste-index)
                                             (make-array 3 :initial-element ()))
                            do (loop for cycle fixnum from 0 to 2
                                     do (setf (svref cvec cycle)
                                              (prepend (recycle-mask stock-index cycle)
                                                       wdspmasks))))))
        finally (return masks)))

(declaim (inline smref))
(defun smref (successor-masks pyramid-id stock-index waste-index cycle)
  "Like AREF for SUCCESSOR-MASKS (it's 4 levels of vectors instead of a multidimensional array)."
  (svref (svref (svref (svref successor-masks pyramid-id) stock-index) waste-index) cycle))

(defun h-costs (card-values)
  "For each pyramid configuration, estimate the number of steps to remove all pyramid cards.
The estimate is the number of kings remaining in the pyramid, plus the maximum count of the number
of cards of each matching pair.  For example if there are two fours and three nines in the pyramid,
it would take at least three steps to remove all the fours and nines."
  (let ((h-costs (make-array 1430)))
    (dotimes (pyramid-id 1430 h-costs)
      (loop with buckets = (make-array 14 :initial-element 0)
            for i fixnum in (svref *pyramid-existing-indexes* pyramid-id)
            do (incf (svref buckets (svref card-values i)))
            finally (setf (svref h-costs pyramid-id)
                          (+ (svref buckets 13)
                             (max (svref buckets 1) (svref buckets 12))
                             (max (svref buckets 2) (svref buckets 11))
                             (max (svref buckets 3) (svref buckets 10))
                             (max (svref buckets 4) (svref buckets 9))
                             (max (svref buckets 5) (svref buckets 8))
                             (max (svref buckets 6) (svref buckets 7))))))))

(defun card-value-masks (card-values)
  "For each card value (1 - 13), a mask of all DECK-INDEXes of that value.  So index 1 would be a
mask with the bits for each Ace's position set to 1.  This is used to find out which cards of a
specific rank have not been removed yet."
  (let ((masks (make-array 14 :initial-element 0)))
    (dotimes (i 52 masks)
      (let ((card-value (svref card-values i)))
        (setf (svref masks card-value)
              (logior (svref masks card-value) (ash 1 i)))))))

(defun unclearable-masks (card-values)
  "For each pyramid configuration, a list of masks to check if a pyramid can't be cleared.  LOGAND
the masks with the STATE and any zero result means a pyramid card exists that can't be removed
because there is no longer any card available to remove it with."
  (labels ((unrelated-pyramid-card-mask (pyramid-index)
             "Return a mask of pyramid indexes that aren't covering or covered by PYRAMID-INDEX."
             (svref #(#b0000000000000000000000000000
                      #b1000000100000100001000100100
                      #b0000001000001000010001001010
                      #b1100000110000110001100110100
                      #b1000001100001100011001101000
                      #b0000011000011000110011011010
                      #b1110000111000111001110110100
                      #b1100001110001110011101100000
                      #b1000011100011100111011001000
                      #b0000111000111001110111011010
                      #b1111000111100111101110110100
                      #b1110001111001111011100100000
                      #b1100011110011110111001000000
                      #b1000111100111101110011001000
                      #b0001111001111011110111011010
                      #b1111100111110111101110110100
                      #b1111001111101111001100100000
                      #b1110011111011110011000000000
                      #b1100111110111100110001000000
                      #b1001111101111001110011001000
                      #b0011111011111011110111011010
                      #b1111110111110111101110110100
                      #b1111101111100111001100100000
                      #b1111011111001110001000000000
                      #b1110111110011100010000000000
                      #b1101111100111000110001000000
                      #b1011111001111001110011001000
                      #b0111111011111011110111011010)
                    pyramid-index))
           (removablep (pyramid-index pyramid-flags card-value-masks card-value)
             "Return T if the PYRAMID-INDEX card can be removed using another pyramid card."
             (not (zerop (mask-field (byte 28 0)
                                     (logand (svref card-value-masks (- 13 card-value))
                                             pyramid-flags
                                             (unrelated-pyramid-card-mask pyramid-index)))))))
                                             
    (loop with all-masks = (make-array 1430)
          with value-masks = (card-value-masks card-values)
          for pyramid-id from 0 below 1430
          for existing-indexes = (svref *pyramid-existing-indexes* pyramid-id)
          for pyramid-flags = (svref *pyramid-flags* pyramid-id)
          for masks = (loop for i in existing-indexes
                            for value = (svref card-values i)
                            unless (or (= 13 value) (removablep i pyramid-flags value-masks value))
                            collect (mask-field (byte 24 28) (svref value-masks (- 13 value))))
          do (setf (svref all-masks pyramid-id) (if (some #'zerop masks) '(0) masks))
          finally (return all-masks))))



;;; Functions using the precalculated data during the solution search
(declaim (inline pyramid-clear-p))
(defun pyramid-clear-p (pyramid-id)
  "Return T if the PYRAMID-ID refers to the state where all pyramid cards have been removed."
  (zerop (the pyramid-id pyramid-id)))

(declaim (inline pyramid-h-cost))
(defun pyramid-h-cost (pyramid-id h-costs)
  "Return an estimate of how many steps it may take to remove the remaining pyramid cards."
  (declare (pyramid-id pyramid-id) ((simple-vector 1430) h-costs))
  (the (integer 0 102) (svref h-costs pyramid-id)))

(defun state-unclearable-p (state pyramid-id unclearable-masks)
  "Return T if the pyramid state can't be cleared."
  (declare (state state) (pyramid-id pyramid-id) ((simple-vector 1430) unclearable-masks)
           (optimize speed (safety 0) (debug 1)))
  (dolist (mask (svref unclearable-masks pyramid-id))
    (when (zerop (logand (the state mask) state))
      (return-from state-unclearable-p t))))

(declaim (inline state-successor-masks))
(defun state-successor-masks (state pyramid-id successor-masks)
  "Return a list of successor masks for the given state."
  (declare (state state) (pyramid-id pyramid-id) ((simple-vector 1430) successor-masks)
           (optimize speed (safety 0) (debug 1)))
  (let* ((stock-index (state-stock-index state))
         (waste-index (state-waste-index state stock-index))
         (cycle (state-cycle state)))
    (smref successor-masks pyramid-id stock-index waste-index cycle)))



;;; Search Node related definitions
(defun action (deck previous-state current-state)
  "Return a value indicating the action taken to go from PREVIOUS-STATE to CURRENT-STATE.
It'll either be a list of cards that were removed, the string 'Draw', or the string 'Recycle'."
  (let* ((diffs (logxor previous-state current-state))
         (pyramid-id-diff (mask-field (byte 11 0) diffs))
         (cycle-diff (mask-field (byte 2 11) diffs))
         (stock-flags-diff (mask-field (byte 24 28) diffs)))
    (cond ((not (zerop cycle-diff)) "Recycle")
          ((or (not (zerop pyramid-id-diff)) (not (zerop stock-flags-diff)))
           (loop with prev = (svref ps::*pyramid-flags* (mask-field (byte 11 0) previous-state))
                 with curr = (svref ps::*pyramid-flags* (mask-field (byte 11 0) current-state))
                 with deck-removed-card-flags = (logior stock-flags-diff (logxor prev curr))
                 for i from 0 to 51
                 when (logbitp i deck-removed-card-flags)
                 collect (elt deck i)))
          (t "Draw"))))

(defun human-readable-action (action)
  "Return a human-readable string for the Lisp-readable ACTION."
  (cond ((equal action "Recycle")
         "Recycle the waste pile.")
        ((equal action "Draw")
         "Draw a card from the stock pile to the waste pile.")
        (t (format nil "Remove ~{~A~^ and ~}." action))))

(defun actions (node deck)
  "Return a list of Lisp-readable actions to go from the initial node to NODE."
  (loop for states on (reverse (rest node))
        for previous-state = (first states)
        for current-state = (second states)
        while current-state
        collect (action deck previous-state current-state)))



;;; Bucket Queue (Priority Queue) related definitions
(defstruct bucket-queue
  "A priority queue that can be used when priorities are small integers.  It's a vector of lists
where items with priority N are inserted into index N of the vector.  Use CREATE-BUCKET-QUEUE to
handle the parameters correctly."
  (items nil :type simple-vector)
  (front nil :type fixnum))

(defun create-bucket-queue (max-priority)
  "Create a bucket queue for items of priority 0 to MAX-PRIORITY inclusive."
  (let ((size (1+ max-priority)))
    (make-bucket-queue :items (make-array size :initial-element nil)
                       :front size)))

(defun bucket-queue-add (bucket-queue item priority)
  "Add ITEM into the BUCKET-QUEUE with the given PRIORITY."
  (declare (optimize speed (safety 0))
           (type bucket-queue bucket-queue)
           (type fixnum priority))
  (when (< priority (bucket-queue-front bucket-queue))
    (setf (bucket-queue-front bucket-queue) priority))
  (push item (svref (bucket-queue-items bucket-queue) priority)))

(declaim (inline bucket-queue-empty-p))
(defun bucket-queue-empty-p (bucket-queue)
  "Return T if BUCKET-QUEUE has no items, NIL otherwise."
  (declare (optimize speed (safety 0))
           (type bucket-queue bucket-queue))
  (= (length (bucket-queue-items bucket-queue))
     (bucket-queue-front bucket-queue)))

(defun bucket-queue-remove (bucket-queue)
  "Remove and return the lowest priority item from BUCKET-QUEUE."
  (declare (optimize speed (safety 0))
           (type bucket-queue bucket-queue))
  (unless (bucket-queue-empty-p bucket-queue)
    (let* ((front (bucket-queue-front bucket-queue))
           (items (bucket-queue-items bucket-queue))
           (empty-queue-index (length items)))
      (prog1
          (pop (svref items front))
        (unless (svref items front)
          (setf (bucket-queue-front bucket-queue)
                (do ((i (1+ front) (1+ i)))
                    ((or (eql i empty-queue-index) (svref items i)) i)
                  (declare (type fixnum i)))))))))



(defun make-state-cache ()
  (let ((cache (make-array 8192 :initial-element nil)))
    (dotimes (cycle 3 cache)
      (dotimes (pyramid-id 1430)
        (setf (svref cache (logior (ash cycle 11) pyramid-id)) (make-hash-table))))))

(declaim (inline get-state-cache))
(defun get-state-cache (state cache)
  (declare (state state) ((simple-vector 8192) cache))
  (gethash state (svref cache (mask-field (byte 13 0) state))))

(declaim (inline add-state-cache))
(defun add-state-cache (state cache value)
  (declare (state state) ((simple-vector 8192) cache) ((integer 0 102) value))
  (setf (gethash state (svref cache (mask-field (byte 13 0) state))) value))

(defun solve (deck)
  "A* solver for Pyramid Solitaire for the given DECK."
  (declare (optimize speed (safety 0) (debug 1)))
  (let* ((card-values (card-values deck))
         (successor-masks (successor-masks card-values))
         (h-costs (h-costs card-values))
         (unclearable-masks (unclearable-masks card-values))
         (fringe (create-bucket-queue 102))
         (seen-states (make-state-cache))
         (state +initial-state+)
         (pyramid-id (state-pyramid-id state))
         (node (cons 0 (list state))))
    (unless (state-unclearable-p state pyramid-id unclearable-masks)
      (bucket-queue-add fringe node (pyramid-h-cost pyramid-id h-costs)))
    (loop
     (when (bucket-queue-empty-p fringe) (return-from solve nil))
     (setf node (bucket-queue-remove fringe))
     (setf state (second node))
     (setf pyramid-id (state-pyramid-id state))
     (when (pyramid-clear-p pyramid-id) (return-from solve (actions node deck)))
     (dolist (mask (state-successor-masks state pyramid-id successor-masks))
       (let* ((next-state (state-adjust-stock-index (logxor (the state state) (the state mask))))
              (next-pyramid-id (state-pyramid-id next-state))
              (seen-depth (get-state-cache next-state seen-states))
              (next-depth (1+ (the (integer 0 101) (first node)))))
         (when (or (not seen-depth)
                   (< (the (integer 0 102) next-depth) (the (integer 0 102) seen-depth)))
           (add-state-cache next-state seen-states next-depth)
           (unless (state-unclearable-p next-state next-pyramid-id unclearable-masks)
             (bucket-queue-add fringe
                               (cons next-depth (cons next-state (rest node)))
                               (the (integer 0 102)
                                    (+ (the (integer 0 102) next-depth)
                                       (pyramid-h-cost next-pyramid-id h-costs)))))))))))

;;; Testing functions
(defun run ()
  (let ((deck (string->card-list "Th 2h 4d 3h Qd 8h 9h 5d Jc Td 7c 4c Ts Ac 9c 8d 5s 2s 7h 6s 7s 2c 9d Qs 3d 5c 5h Ad 8s Js 6c 9s 4h Kh Jd 4s 2d 6d Ks Qc 3s 3c Kc 7d Tc Ah 6h Qh Kd 8c As Jh")))
    (when (standard-deck-p deck)
      (solve deck))))

(defun run-decks (&optional (filename "resources/random-decks.txt"))
  (declare (optimize speed (safety 0) (debug 0)))
  (with-open-file (in filename)
    (loop for deck-string = (read-line in nil) and count fixnum from 1
          while deck-string
          do (let* ((deck (string->card-list deck-string))
                    (start-time (get-internal-real-time))
                    (solution (solve deck))
                    (total-time (- (get-internal-real-time) start-time)))
               (format t "~S~%" (list deck solution count total-time))))))
