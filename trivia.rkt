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

(dsp " \n")
(dsp "  _        _       _       \n")
(dsp " | |      (_)     (_)      \n")
(dsp " | |_ _ __ ___   ___  __ _ \n")
(dsp " | __| '__| \\ \\ / / |/ _` |\n")
(dsp " | |_| |  | |\\ V /| | (_| |\n")
(dsp "  \\__|_|  |_| \\_/ |_|\\__,_|\n")
(dsp " \n")
(dsp " \n")

; Read questions and answers from JSON file
(define (read-questions-and-answers file-path)
  (define json-data (call-with-input-file file-path read-json))
  (define questions-and-answers (hash-ref json-data 'questions))
  (define questions (map (λ (qa) (hash-ref qa 'question)) questions-and-answers))
  (define answers (map (λ (qa) (hash-ref qa 'answer)) questions-and-answers))
  (values questions answers))

; Output question and process answer
(define (process-question-and-answer question answer)
  (let loop ()
    (dsp question)
    (dsp "\nYour answer: ")
    (flush-output)
    (let ([user-answer (string-trim (read-line))])
      (if (string-ci=? user-answer answer)
          (begin
            (dsp "\nCorrect answer\n\n\n"))
          (begin
            (dsp "\nIncorrect answer\n\n\n")
            (loop))))))

; Loop through all questions and process answers
(define (main)
  (let-values ([(questions answers) (read-questions-and-answers "data.json")])
    (for ([i (in-range (length questions))])
      (let ([question (list-ref questions i)]
            [answer (list-ref answers i)])
        (process-question-and-answer question answer)))))

(main)