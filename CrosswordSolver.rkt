#lang racket

;This program takes a list of strings that make up the rows of a crossword
;puzzle and finds every word in the linuxwords.txt dictionary that is longer
;than 3 letters that is present in the puzzle. It then displays the output list.
(define (find-words width length letters)
  (let ((lst (file->lines "linuxwords.txt")))
    (let ((lowerlst (getLower lst)))
      ;First removes all words that wouldn't be looked for (too short, too long,
      ;and words with letters not found in the word search)
      (let ((smallerlst (removeBigSmall lowerlst (max width length))))
        (let ((smallestlst (removeImpossible smallerlst (toOneString letters))))
          ;Put the letters in a 2D vector in order to easily get their
          ;x and y positions for hashing
          (let ((letterhash (hashLetters (vectorize letters) width length)))
            (searchPuzzle smallestlst letterhash width length)
          )
        )
      )
    )
  )
)

;This takes every word in the list and makes it fully lowercase
(define (getLower lst)
  (if (null? lst)
      '()
      (cons (string-downcase (car lst)) (getLower (cdr lst)))
  )
)

;This creates a sublist of only words that are more than 3 letters
;and no larger than the largest dimension of the puzzle 
(define (removeBigSmall lowerlst maxlength)
  (if (null? lowerlst)
      '()
      (if (and (> (string-length (car lowerlst)) 3)
               (< (string-length (car lowerlst)) (+ maxlength 1))
          )
          (cons (car lowerlst) (removeBigSmall (cdr lowerlst) maxlength))
          (removeBigSmall (cdr lowerlst) maxlength)
      )
  )
)

;This combines all the strings in the puzzle for use in
;the removeImpossible and possible? functions
(define (toOneString letters)
  (string-join letters "")
)

;This checks each word in the dictionary list to see if it can appear in the puzzle at all
(define (removeImpossible smallerlst letterStr)
  (if (null? smallerlst)
      '()
      (if (possible? (car smallerlst) letterStr 0 1)
          (cons (car smallerlst) (removeImpossible (cdr smallerlst) letterStr))
          (removeImpossible (cdr smallerlst) letterStr)
      )
  )
)

;This determines if a word can exist in the puzzle based on whether or not all
;of its letters appear in the puzzle
(define (possible? word letterStr start end)
  (if (= (string-length word) start)
      #t
      (if (string-contains? letterStr (substring word start end))
          (possible? word letterStr (+ start 1) (+ end 1))
          #f
      )
  )
)

;This puts the puzzle into a 2D vector for easier hashing
(define (vectorize letters)
  (list->vector (map list->vector (map string->list letters)))
)

;These puts the vectors into a hash for faster searching
(define (hashLetters letterVector width length)
  (insertLetters (make-hash) letterVector width length 0 0)
)

(define (insertLetters ht letterVector width length y x)
  (cond ((= y width) ht)
        ((= x length) (insertLetters ht letterVector width length (+ y 1) 0))
        (else
         (hash-set! ht (list x y) (vector-ref (vector-ref letterVector y) x))
         (insertLetters ht letterVector width length y (+ x 1))
        )
  )
)

;This begins the actual searching of the puzzle
(define (searchPuzzle smallestlst letterhash width length)
  ;This if can easily bailout if the smallestlst starts empty due to no possible words
  (if (null? smallestlst)
      '()
      (if (checkWord (string->list (car smallestlst)) letterhash width length 0 0)
          (cons (car smallestlst) (searchPuzzle (cdr smallestlst) letterhash width length))
          (searchPuzzle (cdr smallestlst) letterhash width length)
      )
  )
)

;This checks if a word from the filtered dictionary list starts at the present position
;and if it does, the search of the position's neighbors begin. Otherwise, it moves to the
;next position until it finds the first letter of the word or hits the end of the puzzle.
(define (checkWord word letterhash width length x y)
  (cond ((= y width) #f)
        ((= x length) (checkWord word letterhash width length 0 (+ y 1)))
        ((equal? (car word) (hash-ref letterhash (list x y)))
         (if (checkNeighbors (cdr word) letterhash width length x y)
             #t
             (checkWord word letterhash width length (+ x 1) y)
         )
        )
        (else (checkWord word letterhash width length (+ x 1) y))
  )
)

;This checks all the possible directions a word could be found
(define (checkNeighbors word letterhash width length x y)
  (or (goRight word letterhash width length (+ x 1) y)
      (or (goLeft word letterhash width length (- x 1) y)
          (or (goDown word letterhash width length x (+ y 1))
              (or (goUp word letterhash width length x (- y 1))
                  (or (goLeftDown word letterhash width length (- x 1) (+ y 1))
                      (or (goRightUp word letterhash width length (+ x 1) (- y 1))
                          (or (goRightDown word letterhash width length (+ x 1) (+ y 1))
                              (goLeftUp word letterhash width length (- x 1) (- y 1))
                          )
                      )
                  )
              )
          )
      )
  )
)
;All go[direction] functions take the candidate word and move in the corresponding
;direction to see if all of the word's letters are found in order in this direction
(define (goRight word letterhash width length x y)
  (cond ((null? word) #t)
        ((= x length) #f)
        (else (if (equal? (car word) (hash-ref letterhash (list x y)))
                  (goRight (cdr word) letterhash width length (+ x 1) y)
                  #f
              )
        )
  )
)

(define (goLeft word letterhash width length x y)
  (cond ((null? word) #t)
        ((= x -1) #f)
        (else (if (equal? (car word) (hash-ref letterhash (list x y)))
                  (goLeft (cdr word) letterhash width length (- x 1) y)
                  #f
              )
        )
  )
)

(define (goDown word letterhash width length x y)
  (cond ((null? word) #t)
        ((= y width) #f)
        (else (if (equal? (car word) (hash-ref letterhash (list x y)))
                  (goDown (cdr word) letterhash width length x (+ y 1))
                  #f
              )
        )
  )
)

(define (goUp word letterhash width length x y)
  (cond ((null? word) #t)
        ((= y -1) #f)
        (else (if (equal? (car word) (hash-ref letterhash (list x y)))
                  (goUp (cdr word) letterhash width length x (- y 1))
                  #f
              )
        )
  )
)

(define (goLeftDown word letterhash width length x y)
  (cond ((null? word) #t)
        ((or (= y width) (= x -1)) #f)
        (else (if (equal? (car word) (hash-ref letterhash (list x y)))
                  (goLeftDown (cdr word) letterhash width length (- x 1) (+ y 1))
                  #f
              )
        )
  )
)

(define (goRightUp word letterhash width length x y)
  (cond ((null? word) #t)
        ((or (= y -1) (= x length)) #f)
        (else (if (equal? (car word) (hash-ref letterhash (list x y)))
                  (goRightUp (cdr word) letterhash width length (+ x 1) (- y 1))
                  #f
              )
        )
  )
)

(define (goRightDown word letterhash width length x y)
  (cond ((null? word) #t)
        ((or (= y width) (= x length)) #f)
        (else (if (equal? (car word) (hash-ref letterhash (list x y)))
                  (goRightDown (cdr word) letterhash width length (+ x 1) (+ y 1))
                  #f
              )
        )
  )
)

(define (goLeftUp word letterhash width length x y)
  (cond ((null? word) #t)
        ((or (= y -1) (= x -1)) #f)
        (else (if (equal? (car word) (hash-ref letterhash (list x y)))
                  (goLeftUp (cdr word) letterhash width length (- x 1) (- y 1))
                  #f
              )
        )
  )
)

;Test Cases:
;
;Input:
;(find-words 15 20 '("boarderofcosperdaswr" "allrukidjbqislodasfr" "sdagsimcnxskeowdasdw"
;"egectidoqkmvjfsdasdw" "sxxxkoaicsioaskgbbca" "darknightselfasdfdfk" "boarderofcosperpoiuy"
;"allrukidjbqisloqwert" "sdagsimcnxskeowytrew" "egectidoqkmvjfsdfghj" "sdagsimcnxskeowdasdw"
;"egectidoqkmvjfsdasdw" "sxxxkoaicsioaskgbbcf" "darknightselfasdfdfk" "ssssssssssssssssssss"))
;Output:
;'("alar" "base" "bases" "black" "boar" "board" "boarder" "coco" "dark" "dons"
;"drag" "drags" "dust" "dusts" "fore" "gash" "grim" "joes" "knight" "knights"
;"lack" "lest" "levi" "levis" "monk" "nigh" "night" "nights" "ohio" "olaf"
;"prep" "rags" "reid" "rows" "sags" "self" "togo")
;Time taken:
;23.5 seconds
;
;Input:
;(find-words 5 5 '("catss" "dogst" "flogo" "zlzpp" "fling"))
;Output:
;'("cats" "coop" "dogs" "fling" "flog" "floss" "golf" "logo" "loss" "pots" "stop")
;Time taken:
;~1 second