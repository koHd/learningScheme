#lang racket
; a game of single player hangman
; a secret word must be guessed by a player via a series of individual letter guesses until they can guess the secret word
; each incorrect guess incurs a penalty lives - 1
; game is played over a number of rounds involving a single guess from the player
; game ends when player's lives reach 0 or secret word is correctly guessed by player
; in each round:
; - display secret word to the player with unknown letters encoded as underscores
; - if all letters are known, player WON and we end game
; - if player's lives == 0, player LOST and we end game
; - the player can either guess a letter or guess the word
; - if the player guesses a letter:
; - - if the letter is in the secret word, the letter becomes a known letter in the secret word
; - - else the player loses a life
; - - continue to next round
; - else if the player gueeses the word:
; - - if the guessed word is correct, all letters become known and we continue to next round
; - - else the player loses a life and we continue to next round

; things that can be added to improve the game:
; - random word chosen as secret word
; - - choose word randomly from a local dictionary file

(define (element-in-set? element set)
  (cond
    [(empty? set) #f]
    [(equal? element (car set)) #t]
    [else (element-in-set? element (cdr set))]))

(define (set-cardinality set)
  (define (count-elements set count)
    (if (empty? set)
        count
        (count-elements (cdr set) (+ count 1))))
  (count-elements set 0))

; store secret word (hard coded temporarily while in the process of developing):
(define secret-word '(s e c r e t))

; check if a guess is in a word:
(define (letter-in-word? letter word)
  (element-in-set? letter word))

; display the secret word to the player with successfully guessed letters revealed
(define (secret-word-known-so-far secret-word guessed-letters)
  (cond
    [(empty? secret-word) null]
    [(letter-in-word? (car secret-word) guessed-letters)
     (cons (car secret-word) (secret-word-known-so-far (cdr secret-word) guessed-letters))]
    [else (cons '_ (secret-word-known-so-far (cdr secret-word) guessed-letters))]))

; round
(define (play-hangman round secret-word player-lives guessed-letters)
  
  (define (win-screen)
    (newline)
    (display "Congratulations, you won! It took you ")
    (display round)
    (display " rounds to reveal the secret word -> ")
    (display secret-word))

  (define (lose-screen)
    (newline)
    (display "Unlucky! You failed to win this time. Better luck next time."))

  (define (new-round-screen)
    (if (zero? round) (displayln "Lets play hangman!") null)
    (newline)
    (display "Round: ")
    (display round)
    (newline)
    (display "Lives Remaining: ")
    (display player-lives)
    (newline)
    (display "Your guesses so far: ")
    (display guessed-letters)
    (newline)
    (display "Can you guess the word? ")
    (display (secret-word-known-so-far secret-word guessed-letters))
    (newline)
    (display "Guess a letter: "))

  (cond
    [(equal? secret-word (secret-word-known-so-far secret-word guessed-letters)) (win-screen)]
    [(zero? player-lives) (lose-screen)]
    [else
     (new-round-screen)
     (define guess (read))
     (cond
       [(letter-in-word? guess secret-word)
        (display "Correct!")
        (newline)
        (play-hangman (+ round 1) secret-word player-lives (cons guess guessed-letters))]
       [else
        (display "Incorrect.")
        (newline)
        (play-hangman (+ round 1) secret-word (- player-lives 1) (cons guess guessed-letters))])]))

(define (hangman)
  (play-hangman 0 secret-word 7 null))
