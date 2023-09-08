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
  (for-each dsp '(" \n"
                  "  _        _       _       \n"
                  " | |      (_)     (_)      \n"
                  " | |_ _ __ ___   ___  __ _ \n"
                  " | __| '__| \\ \\ / / |/ _` |\n"
                  " | |_| |  | |\\ V /| | (_| |\n"
                  "  \\__|_|  |_| \\_/ |_|\\__,_|\n"
                  " \n"
                  " \n")))

; Read questions and answers from JSON file.
(define (read-q file-path)
  (define json-data (call-with-input-file file-path read-json))
  (values (map (λ (qa) (hash-ref qa 'question)) (hash-ref json-data 'questions))
          (map (λ (qa) (hash-ref qa 'answer)) (hash-ref json-data 'questions))))

; Output question and process answer.
; Using tail recursion.
(define (process-q question answer)
  (let process-q-tail ()
    (dsp question)
    (dsp "\nYour answer: ")
    (flush-output)
    (if (string-ci=? (string-trim (read-line)) answer)
        (begin
          (dsp "\nCorrect answer\n\n\n"))
        (begin
          (dsp "\nIncorrect answer\n\n\n")
          (process-q question answer)))))

; Applies the process-q function to each question-answer pair.
(define (process-que questions answers)
  (map process-q questions answers))

; Loop through all questions and process answers.
(define (main)
  (logo)
  (let-values ([(questions answers) (read-q "./data.json")])
    (process-que questions answers)))

(main)