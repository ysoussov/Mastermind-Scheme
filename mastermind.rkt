;;Yuri Soussov
;;Programing Languages
;;Fall 2010
;;ProgramingAssignment 2
;;To Run: mzscheme -f [scriptname] 
;;change the very last line to vary the ammounts of colors and spaces
;;most variable names are very similar to the ones used in the algorithm for ease of understanding
(define (genc n) ;;this function will generate every possible color given a max color n
       (if (= n 1)
           (list n)
           (append (genc (- n 1)) (list n))
           )
    )

(define (mask x y p) ;;this will create a mask for two possible codes (x and y) it will put a 1 when the numbers/places match a zero otherwise
  (if (= p 0)
      (list)
      (if (= (list-ref x (- p 1)) (list-ref y (- p 1)))
          (append (mask x y (- p 1)) (list 1))
          (append (mask x y (- p 1)) (list 0))
          )
      )
  )

(define (countBlacks x p);;this is a method that will count the ammount of blacks by being passed in a mask
  (if (= p 0)
      0
      (+ (countBlacks x (- p 1)) (list-ref x (- p 1)))
      )
  )
(define (countBlackRunner x y p) ;;a utility function for counting blacks for two codes x and y
  (countBlacks (mask x y p) p)
  )
(define (removeMask x mask p) ;;this will set to zero all the places where the mask equals 1, given a code x  
  (if (= p 0)
      (list)
      (if (= (list-ref mask (- p 1)) 0)
          (append (removeMask x mask (- p 1)) (list (list-ref x (- p 1))))
          (append (removeMask x mask (- p 1)) (list 0))
          )
      )
  )

(define (countColor x color p) ;;this will count how many members there are of a certain color in code x
	(if (= p 0)
		0
		(if (= color (list-ref x (- p 1)))
		(+ 1 (countColor x color (- p 1)))
		(countColor x color (- p 1))
	)
)
)

(define (palitra x y nColor p) ;;this will find how many members are in the wrong positions
	(if (= nColor 0)
		0
                (if (> (countColor x nColor p) (countColor y nColor p))
                    (+ (countColor y nColor p) (palitra x y (- nColor 1) p))
                    (+ (countColor x nColor p) (palitra x y (- nColor 1) p))
                )
  )
                    )
                
 (define (calculateFitness population guesses black white guessesLength p n y) ;;this will create a list of fitnesses given a population y should be the size of the population
   (if (= y 0)
       (list)
       (let ((a (list-ref population (- y 1))))
         (append (calculateFitness population guesses black white guessesLength p n (- y 1))(list (fitness guesses black white a guessesLength 0 p n)))
         )
       )
   )
 (define (sublist x y z) ;;this will return a sublist of x containing values of indicies y (inclusive) to z (not inclusive)
   (if (= y z)
       (list)
       (append (list (list-ref x y)) (sublist x (+ y 1) z))
       )
   )
 (define (singleCross x y p) ;;this function does a genetic single cross on two codes x and y
   (let ((a (random p)))
     (let ((b (random 2)))
       (if (= b 0)
         (append (sublist x 0 a) (sublist y a p))
         (append (sublist y 0 a) (sublist x a p))
         )
     )
   )
   )
 (define (doubleCross x y p) ;;this function does a genetic double cross on two codes x and y
   (let ((a (random (- p 1))))
     (let ((b (+(+ a (random (- p a))) 1)))
       (let ((c (random 2)))
         (if (= c 0)
             (append(append (sublist x 0 a) (sublist y a b))(sublist x b p))
             (append(append (sublist y 0 a) (sublist x a b))(sublist y b p))
             )
         )
       )
     )
   )
 (define (swap x p) ;;this functino will swap (with a probability of 33%) two random members of list x
   (let ((a (random (- p 1))))
     (let ((b (+(+ a (random (- (- p a)1)))1) ))
       (append (append (append (append (sublist x 0 a) (list (list-ref x b))) (sublist x (+ a 1) b)) (list (list-ref x a))) (sublist x (+ b 1) p))
       )
     )
   )
 (define (mutate x p n) ;;this will mutate a single member in x to a color from 1 to n
   (let ((a (random p)))
     (append (append (sublist x 0 a) (list (list-ref (genc n) (random n)))) (sublist x (+ a 1) p))
     )
   )
 
 (define (getElites population fitness populationSize elites) ;;this will find the elites given a population and fitness, an empty list is passed into elites that is filled
   (if (= populationSize 0)
       elites
       (if (= (list-ref fitness (- populationSize 1)) 1)
           (let ((a (list-ref population (- populationSize 1))))
           (if (member a elites)
               (getElites population fitness (- populationSize 1) elites)
               (getElites population fitness (- populationSize 1) (append elites (list a)))
               )
             )
           (getElites population fitness (- populationSize 1) elites)
           )
       )
   )
 
 (define (chooseGuess elites elitesSize maxValue maxIndex iterator p) ;;this function chooses the next guess based on the second approach described in the algorithm
   (if (= iterator elitesSize)
       maxIndex
       (if (> (weightElite (list-ref elites iterator) elites elitesSize p) maxValue)
           (chooseGuess elites elitesSize (weightElite (list-ref elites iterator) elites elitesSize p) iterator (+ iterator 1) p)
           (chooseGuess elites elitesSize maxValue maxIndex (+ iterator 1) p)
           )
       )
   )
  
 (define (weightElite elite elites elitesSize p) ;;a utility function to help choose the next guess
   (if (= 0 elitesSize)
       0
       (+ (countBlackRunner elite (list-ref elites (- elitesSize 1)) p) (weightElite elite elites (- elitesSize 1) p))
       )
   )
                            
 (define (generateNextGeneration population fitness populationSize p n fulfilled codes) ;;this will generate a new generation based on a previous population fulfilled should be 0 and codes should be an empty list
   (if (= fulfilled populationSize)
       (list)
       (let ((a (nextGeneration population fitness populationSize p n)))
         (if (member a codes)
             (generateNextGeneration population fitness populationSize p n fulfilled codes)
             (append (list a) (generateNextGeneration population fitness populationSize p n (+ fulfilled 1) codes))
             )
         )
       )
   )
 (define (nextGeneration population fitness populationSize p n) ;;this does all of the crossover/mutate/swap computation and calling to help create a new generation
   (let ((a (chooseParent fitness populationSize)))
     (let ((b (chooseParent fitness populationSize)))
       (let ((c (random 2)))
         (if (= c 0)
             (let ((d (singleCross (list-ref population a) (list-ref population b) p)))
               (let ((e (random 3)))
                 (if (= e 0)
                     (let ((f (swap d p)))
                       (let ((g (random 3)))
                         (if (= g 0)
                             (mutate f p n)
                             f
                             )
                         )
                       )
                     (let ((h (random 3)))
                       (if (= h 0)
                           (mutate d p n)
                           d
                           )
                       )
                     )
                 )
               )
              (let ((d (doubleCross (list-ref population a) (list-ref population b) p)))
               (let ((e (random 3)))
                 (if (= e 0)
                     (let ((f (swap d p)))
                       (let ((g (random 3)))
                         (if (= g 0)
                             (mutate f p n)
                             f
                             )
                         )
                       )
                     (let ((h (random 3)))
                       (if (= h 0)
                           (mutate d p n)
                           d
                           )
                       )
                     )
                 )
               )
             )
         )
       )
     )
   )
              
 (define (chooseParent fitness populationSize) ;;this is used to choose a parent by the method described in the algorithm
   (let ((a (random populationSize)))
        (let ((b (random populationSize)))
             (let ((c (random populationSize)))
               (if (< (list-ref fitness a) (list-ref fitness b))
                   (if (< (list-ref fitness a) (list-ref fitness c))
                       a
                       c
                       )
                   (if (< (list-ref fitness b) (list-ref fitness c))
                       b
                       c
                       )
                   )
               )
          )
     )
   )
 (define (fitness guesses black white member length position p n) ;;this will calculate the fitness for a single member which is a code
  (if (= length position)
      1
      (let ((currentGuess (list-ref guesses position)))
        (let ((currentBlack (list-ref black position)))
          (let ((currentWhite (list-ref white position)))
            (let ((compareBlack (countBlackRunner currentGuess member p)))
               (let ((compareWhite (runCountWhites currentGuess member p n)))
                 (+ (+ (abs (- currentBlack compareBlack)) (abs (- currentWhite compareWhite))) (fitness guesses black white member length (+ position 1) p n))
            )
          )
        )
       )
        )
      )
  )
                 
(define (runCountWhites x y p n) ;;a driver to count whites 
  (palitra (removeMask x (mask x y p) p)  (removeMask y (mask x y p) p) n p)
  )
(define (findMatch x y p) ;;this finds the first match of color x in code y
   (if (= p 0)
       0
       (if (= x (list-ref y (- p 1)))
        p
        (findMatch x y (- p 1))
        )
)
)
(define (generateGuess n p) ;;generates the first guess used
  (if (= p 1)
      (list (list-ref (genc n) (random n)))
      (append (generateGuess n (- p 1)) (list (list-ref (genc n) (random n))))
      )
)

(define (generateCodes n p ammount codes) ;;generates the initial population
  (if (= 0 ammount)
      (list)
      (begin
         (let ((temp (generateGuess n p))) 
           (if (member temp codes)
                (generateCodes n p ammount codes)      
                (append (list temp) (generateCodes n p (- ammount 1) (append (list temp) codes)))
           )
         )
      )
      )
  )
(define (checkGuess guess) ;;used to check the guess with the user
  (begin
    (print guess)
        (print "Please enter the number of black pins")
        (let ((b (read))) 
          (begin
            (print "Please enter the number of white pins")
            (let ((w (read)))
             (list w b)
              )
            )
          )
        )
  )
 
(define (generateEliteForNextGuess population n p guesses w b generationNumber eliteSize elites) ;generates the set of elites to be used for the next guess
  (if (and (or ( = (length elites) eliteSize) (= generationNumber 0)) (> (length elites) 0))
      elites
      (let ((fitness (calculateFitness population guesses b w (length guesses) p n (length population))))
      (generateEliteForNextGuess (generateNextGeneration population fitness (length population) p n 0 '()) n p guesses w b (- generationNumber 1) eliteSize (getElites population  fitness (length population) elites))
       )
      )
  )

(define (playGame n p w b guesses population elitesSize generationNumber maxGuesses) ;;the main driver for the loop of the game
      (if (> maxGuesses 0)
      (let ((elites (generateEliteForNextGuess population n p guesses w b generationNumber elitesSize '())))
      (let ((currentGuess (list-ref elites (chooseGuess elites (length elites) -1 -1 0 p))))
      (let ((results (checkGuess currentGuess)))         
          (if (= (list-ref results 1) p)
              (print "I win")
              (playGame n p (append w (list (list-ref results 0))) (append b (list (list-ref results 1))) (append guesses (list currentGuess)) (generateCodes n p (length population) '()) elitesSize generationNumber (- maxGuesses 1))
              )
        )
        )
        )
      (print "max guesses reached - gameover")
  )
  )
          
(define (game n p) ;;initializes the game to n colors and p places, makes the first guess and calls the main game loop
  (begin
    (let ((i 0))
      (begin
        (print "Please enter the number of elites to search for")
        (let ((elites (read)))
        (begin
        (print "Please enter the max number of generations to go through")
        (let ((generations (read)))
        (begin
        (print "Please enter the maximum number of guesses for the computer to take")
        (let ((maxGuesses (read)))
        (begin
        (print "Please enter the population size for each generation")
        (let ((popSize (read)))
        (let ((guesses (list (generateGuess n p))))
        (begin
        (print (list-ref guesses i))
        (print "Please enter the number of black pins")
        (let ((b (list (read)))) 
          (begin
            (print "Please enter the number of white pins")
            (let ((w (list (read))))
              (if (= (list-ref b i) p)
                  (print "I win")
                  (playGame n p w b guesses (generateCodes n p popSize '()) elites generations maxGuesses)        
        )
      )
    )
  )
  )
  )
   )
 )
 )
 )
 )
 )
  )
  )
  )
  )
  )
  (game 6 4) ;; runs the game with 6 colors 4 places