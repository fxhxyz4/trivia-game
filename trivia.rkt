#lang racket

#|
  Author: fxhxyz
  License: MIT license
  Email: fxhsec@proton.me
  Website: fxhxyz.vercel.app
|#

(require racket/system)
(require json)

(define hyphen "_")

; Incapsulate display func
(define (dsp x)
  (cond
    [(string? x) (display x)]
    [else "Error"]))

(define (logo)
  (for-each dsp '(" \n"
                  "  _        _       _       _       \n"
                  " | |      (_)     (_)      (_)     \n"
                  " | |_ _ __ ___   ___  __ _         \n"
                  " | __| '__| \\ \\ / / |/ _` |        \n"
                  " | |_| |  | |\\ V /| | (_| |        \n"
                  "  \\__|_|  |_| \\_/ |_|\\__,_|        \n"
                  " \n"
                  " \n")))

; Read questions and answers from JSON file.
(define (read-q file-path)
  (define json-data (call-with-input-file file-path read-json))
  (values (map (λ (qa) (hash-ref qa 'question)) (hash-ref json-data 'questions))
          (map (λ (qa) (hash-ref qa 'answer)) (hash-ref json-data 'questions))))

; Hint manager
(define (make-hint word count)
  (let* ([lst (string->list word)]
         [len (length lst)]
         [revealed (for/list ([i (in-range len)])
                     (if (< i count)
                         (string (list-ref lst i))
                         hyphen))])
    (string-join revealed "")))

; Process a single question, return 1 if correct, number of incorrect attempts
(define (process-q question answer)
  (let loop ([incorrect-count 0])
    (define hint (make-hint answer incorrect-count))
    (dsp question)
    (dsp (string-append "\nHint: " hint "\n\n"))
    (dsp "Your answer: ")
    (flush-output)
    (let ([user-answer (string-trim (read-line))])
      (if (string-ci=? user-answer answer)
          (begin
            (dsp "\nCorrect answer\n\n\n")
            (list 1 incorrect-count))
          (begin
            (dsp "\n\n\nIncorrect answer\n\n\n")
            (loop (+ incorrect-count 1)))))))

; Process all questions, accumulate total correct and incorrect answers
(define (process-que questions answers)
  (define (helper qs as total-correct total-incorrect)
    (if (or (null? qs) (null? as))
        (values total-correct total-incorrect)
        (let ([result (process-q (car qs) (car as))])
          (helper (cdr qs) (cdr as)
                  (+ total-correct (first result))
                  (+ total-incorrect (second result))))))
  (helper questions answers 0 0))

; Main entry point
(define (main)
  (logo)
  (let-values ([(questions answers) (read-q "/home/fxhxyz/Desktop/dev/trivia-game/data.json")])
    (let-values ([(total-correct total-incorrect) (process-que questions answers)])
      (dsp (format "Game finished!\nTotal correct answers: ~a\nTotal incorrect attempts: ~a\n"
                   total-correct total-incorrect)))))

; Main call
(main)