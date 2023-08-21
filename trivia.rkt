#lang racket

#|
  trvia game with json package
|#

(require racket/system)
(require json)

; The dsp function encapsulates the use of the display function.
; It takes an argument x and displays it on the screen using display.
(define (dsp x)
  (cond
    [(string? x) (display x)]
    [else "Argument not a string"]))

(dsp " \n")
(dsp "  _        _       _       \n")
(dsp " | |      (_)     (_)      \n")
(dsp " | |_ _ __ ___   ___  __ _ \n")
(dsp " | __| '__| \\ \\ / / |/ _` |\n")
(dsp " | |_| |  | |\\ V /| | (_| |\n")
(dsp "  \\__|_|  |_| \\_/ |_|\\__,_|\n")
(dsp " \n")
(dsp " \n")

; Lol
(define (read-questions-and-answers file-path)
  (define json-data (call-with-input-file file-path read-json))
  (define questions-and-answers (hash-ref json-data 'questions))
  (define questions (map (λ (qa) (hash-ref qa 'question)) questions-and-answers))
  (define answers (map (λ (qa) (hash-ref qa 'answer)) questions-and-answers))
  (values (map string-trim questions) (map string-trim answers)))


;(define question "Qwerty")
;(define qanswer "123")

; Output question.
; Input answer.
(define (answer q)
  (dsp (format "~a ?" q))
  (dsp "\n")
  (dsp "Your answer: ")
  (flush-output)
  (let ([ans (string-trim (symbol->string (read)))])
    (condition q ans)))

; Condition
(define (condition q ans)
  (cond
    [(string-ci=? (string-trim q) (string-trim ans)) (dsp "\n\nCorrect answer\n")]
    [else (dsp "Incorrect answer\n\n\n\n")]))

; The imitation of a do...while loop
(define (while-loop question)
  (let loop ((ans '()))
    (let ([user-ans (answer question)])
      (if (string? user-ans)
          (begin
            (set! ans user-ans)
            (condition question ans)
            (if (not (string-ci=? (string-trim question) (string-trim ans)))
                (loop ans)
                (dsp "Exiting loop\n")))
          (loop ans)))))

; Kek
(define (main)
  (let-values ([(questions answers) (read-questions-and-answers "questions.json")])
    (for ([i (in-range (length questions))])
      (let ([question (list-ref questions i)])
        (while-loop question)))))

(main)