#lang racket

#|
  Author: fxhxyz
  Email: fxhxyz@proton.mail
  Website: fxhxyz.vercel.app
|#

; Import modules
(require racket/system)
(require json)

; The dsp function encapsulates the use of the display function.
; It takes an argument x and displays it on the screen using display.
(define (dsp x)
  (cond
    [(string? x) (display x)]
    [else "Error"]))

; Function for displaying logo.
(define (logo)
  (dsp " \n")
  (dsp "  _        _       _       \n")
  (dsp " | |      (_)     (_)      \n")
  (dsp " | |_ _ __ ___   ___  __ _ \n")
  (dsp " | __| '__| \\ \\ / / |/ _` |\n")
  (dsp " | |_| |  | |\\ V /| | (_| |\n")
  (dsp "  \\__|_|  |_| \\_/ |_|\\__,_|\n")
  (dsp " \n")
  (dsp " \n"))

; Read questions and answers from JSON file.
(define (read-q file-path)
  (define json-data (call-with-input-file file-path read-json))
  (define q-and-a (hash-ref json-data 'questions))
  (define questions (map (λ (qa) (hash-ref qa 'question)) q-and-a))
  (define answers (map (λ (qa) (hash-ref qa 'answer)) q-and-a))
  (values questions answers))

; Output question and process answer.
; Using tail recursion.
(define (process-q question answer)
  (define (process-q-tail question answer)
    (dsp question)
    (dsp "\nYour answer: ")
    (flush-output)
    (let ((user-answer (string-trim (read-line))))
      (if (string-ci=? user-answer answer)
          (begin
            (dsp "\nCorrect answer\n\n\n"))
          (begin
            (dsp "\nIncorrect answer\n\n\n")
            (process-q question answer)))))
    (process-q-tail question answer))

; Applies the process-q function to each question-answer pair.
(define (process-que questions answers)
  (map process-q questions answers))

; Loop through all questions and process answers.
(define (main)
  (logo)
  (let-values ([(questions answers) (read-q "./data.json")])
    (process-que questions answers)))

(main)