;; APPENDIX: 8-Bit Binary Multiplier
;; By: Karl Vinola & Wai Khoo
;; full-adder
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    ))

;; half-adder
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    ))

;; wire
;; implemented as message-passing style, as a collection of local procedures together with a dispatch procedure
(define (make-wire)
  ;; two local state variables
  ;; signal-value initialized to be 0
  ;; a collection of action-procedures to be run when the signal changes value.
  (let ((signal-value 0) (action-procedures '()))
    
    ;; change signal value only when it's different
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'signal-already-set))
    
    ;; add new procedure to the collection
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

;; call each of the procedures in the collection
(define (call-each procedures)
  (if (null? procedures)
      'signal-set
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

;; external procedures to access the wire dispatch object
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; or-gate
(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  )

;; and-gate
(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  )

;; inverter
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  )

;; after-delay adds new elements to the-agenda
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

;; propagate, which operates on the-agenda, execute each procedure on the agenda in sequence.
;; it will continue the simulation as long as there are items on the agenda.
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; time segment
;; each time segment is a pair consisting of a number (the time) and a queue that holds the procedures that are scheduled to be run during that time segment.
(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

;; agenda - one-dimensional table of time segments, sorted in order of increasing time.
;; current time (the time of the last action) is store at the head of the agenda; by default, it's 0
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

;; add an action to an agenda
;; create a new time segment if the agenda is empty.
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

;; removes the first item from the agenda
;; it also deletes the item at the front of the queue in the first time segment.
;; if the time segment is empty, remove it from the list of segments
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

;; extract the first agenda item at the head of the queue in the first time segment
;; also update the current time
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;; queue class and operations
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

;; select the item at the front of the queue, by returning the car of the pair
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

;; insert the new item at the end of the queue
;; if the queue was initially empty, set the front and rear pointers of the queue to the new item.
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

;; delete the item at the front of the queue by modifying the front pointer so that it points at the second item in the queue.
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;; setting up the simulator
(define the-agenda (make-agenda))
(define inverter-delay 4)
(define and-gate-delay 2)
(define or-gate-delay 3)

;; making all external wires
(define ground (make-wire))
(set-signal! ground 0)

(define inputA7 (make-wire))
(define inputA6 (make-wire))
(define inputA5 (make-wire))
(define inputA4 (make-wire))
(define inputA3 (make-wire))
(define inputA2 (make-wire))
(define inputA1 (make-wire))
(define inputA0 (make-wire))

(define inputB7 (make-wire))
(define inputB6 (make-wire))
(define inputB5 (make-wire))
(define inputB4 (make-wire))
(define inputB3 (make-wire))
(define inputB2 (make-wire))
(define inputB1 (make-wire))
(define inputB0 (make-wire))

(define output15 (make-wire))
(define output14 (make-wire))
(define output13 (make-wire))
(define output12 (make-wire))
(define output11 (make-wire))
(define output10 (make-wire))
(define output9 (make-wire))
(define output8 (make-wire))
(define output7 (make-wire))
(define output6 (make-wire))
(define output5 (make-wire))
(define output4 (make-wire))
(define output3 (make-wire))
(define output2 (make-wire))
(define output1 (make-wire))
(define output0 (make-wire))

;; wrapper 2nd input - 8 bits
;; set the appropriate wire signal based on the input, val
(define setB
  (lambda (val)
    (begin
      (set-signal! inputB7 (car val))
      (set! val (cdr val))
      (set-signal! inputB6 (car val))
      (set! val (cdr val))
      (set-signal! inputB5 (car val))
      (set! val (cdr val))
      (set-signal! inputB4 (car val))
      (set! val (cdr val))
      (set-signal! inputB3 (car val))
      (set! val (cdr val))
      (set-signal! inputB2 (car val))
      (set! val (cdr val))
      (set-signal! inputB1 (car val))
      (set! val (cdr val))
      (set-signal! inputB0 (car val))
      (set! val (cdr val)))))

;; wrapper for 1st input - 8 bits
;; set the appropriate wire signal based on the input, val
(define setA
  (lambda (val)
    (begin
      (set-signal! inputA7 (car val))
      (set! val (cdr val))
      (set-signal! inputA6 (car val))
      (set! val (cdr val))
      (set-signal! inputA5 (car val))
      (set! val (cdr val))
      (set-signal! inputA4 (car val))
      (set! val (cdr val))
      (set-signal! inputA3 (car val))
      (set! val (cdr val))
      (set-signal! inputA2 (car val))
      (set! val (cdr val))
      (set-signal! inputA1 (car val))
      (set! val (cdr val))
      (set-signal! inputA0 (car val))
      (set! val (cdr val)))))

;; read all output wires signal and display it
(define (showresults)
  (list (get-signal output15) (get-signal output14) (get-signal output13) (get-signal output12) (get-signal output11) (get-signal output10) (get-signal output9) (get-signal output8) (get-signal output7) (get-signal output6) (get-signal output5) (get-signal output4) (get-signal output3) (get-signal output2) (get-signal output1) (get-signal output0)))

;; wrapper for the simulator
;; e.g. (mult8 '(0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0))
(define mult8
  (lambda (arg1 arg2)
    (cond ((not(= 8 (length arg1))) '(error: first argument invalid))
          ((cond ((not(= 8 (length arg2))) '(error: second argument invalid))
                 (else
                  (begin
                    (setA arg1)
                    (setB arg2)
                    (propagate)
                    (showresults))))))))

;; one row of full-adders with all the input and output wires
(define (ripple-carry c-in A0 A1 A2 A3 A4 A5 A6 A7 B inA0 inA1 inA2 inA3 inA4 inA5 inA6 inA7 sum0 sum1 sum2 sum3 sum4 sum5 sum6 sum7 carry)
  ;; defining all the internal wires
  (let ((c-out0 (make-wire))
        (c-out1 (make-wire))
        (c-out2 (make-wire))
        (c-out3 (make-wire))
        (c-out4 (make-wire))
        (c-out5 (make-wire))
        (c-out6 (make-wire))
        (B0 (make-wire))
        (B1 (make-wire))
        (B2 (make-wire))
        (B3 (make-wire))
        (B4 (make-wire))
        (B5 (make-wire))
        (B6 (make-wire))
        (B7 (make-wire)))
    
    ;; 1st adder
    (and-gate B inA0 B0)
    (full-adder A0 B0 c-in sum0 c-out0)
    ;; 2nd adder
    (and-gate B inA1 B1)
    (full-adder A1 B1 c-out0 sum1 c-out1)
    ;; 3rd adder
    (and-gate B inA2 B2)
    (full-adder A2 B2 c-out1 sum2 c-out2)
    ;; 4th adder
    (and-gate B inA3 B3)
    (full-adder A3 B3 c-out2 sum3 c-out3)
    ;; 5th adder
    (and-gate B inA4 B4)
    (full-adder A4 B4 c-out3 sum4 c-out4)
    ;; 6th adder
    (and-gate B inA5 B5)
    (full-adder A5 B5 c-out4 sum5 c-out5)
    ;; 7th adder
    (and-gate B inA6 B6)
    (full-adder A6 B6 c-out5 sum6 c-out6)
    ;; 8th adder
    (and-gate B inA7 B7)
    (full-adder A7 B7 c-out6 sum7 carry)
    'ok))

;; the actual multiplier with the two 8-bits input and one 16-bits output
(define (multiplier8 inA0 inA1 inA2 inA3 inA4 inA5 inA6 inA7 inB0 inB1 inB2 inB3 inB4 inB5 inB6 inB7 out0 out1 out2 out3 out4 out5 out6 out7 out8 out9 out10 out11 out12 out13 out14 out15)
  ;; defining all the internal wires
  (let ((and0 (make-wire))
        (and1 (make-wire))
        (and2 (make-wire))
        (and3 (make-wire))
        (and4 (make-wire))
        (and5 (make-wire))
        (and6 (make-wire))
        (sum1a (make-wire))
        (sum2a (make-wire))
        (sum3a (make-wire))
        (sum4a (make-wire))
        (sum5a (make-wire))
        (sum6a (make-wire))
        (sum7a (make-wire))
        (sum1b (make-wire))
        (sum2b (make-wire))
        (sum3b (make-wire))
        (sum4b (make-wire))
        (sum5b (make-wire))
        (sum6b (make-wire))
        (sum7b (make-wire))
        (sum1c (make-wire))
        (sum2c (make-wire))
        (sum3c (make-wire))
        (sum4c (make-wire))
        (sum5c (make-wire))
        (sum6c (make-wire))
        (sum7c (make-wire))
        (sum1d (make-wire))
        (sum2d (make-wire))
        (sum3d (make-wire))
        (sum4d (make-wire))
        (sum5d (make-wire))
        (sum6d (make-wire))
        (sum7d (make-wire))
        (sum1e (make-wire))
        (sum2e (make-wire))
        (sum3e (make-wire))
        (sum4e (make-wire))
        (sum5e (make-wire))
        (sum6e (make-wire))
        (sum7e (make-wire))
        (sum1f (make-wire))
        (sum2f (make-wire))
        (sum3f (make-wire))
        (sum4f (make-wire))
        (sum5f (make-wire))
        (sum6f (make-wire))
        (sum7f (make-wire))
        (carry1 (make-wire))
        (carry2 (make-wire))
        (carry3 (make-wire))
        (carry4 (make-wire))
        (carry5 (make-wire))
        (carry6 (make-wire)))
    
    ;; 1st bit answer
    (and-gate inB0 inA0 out0)
    
    ;; 1st row
    (and-gate inB0 inA1 and0)
    (and-gate inB0 inA2 and1)
    (and-gate inB0 inA3 and2)
    (and-gate inB0 inA4 and3)
    (and-gate inB0 inA5 and4)
    (and-gate inB0 inA6 and5)
    (and-gate inB0 inA7 and6)
    ;; out1 contains 2nd bit answer
    (ripple-carry ground and0 and1 and2 and3 and4 and5 and6 ground inB1 inA0 inA1 inA2 inA3 inA4 inA5 inA6 inA7 out1 sum1a sum2a sum3a sum4a sum5a sum6a sum7a carry1)
    
    ;; 2nd row
    ;; out2 contains 3rd bit answer
    (ripple-carry ground sum1a sum2a sum3a sum4a sum5a sum6a sum7a carry1 inB2 inA0 inA1 inA2 inA3 inA4 inA5 inA6 inA7 out2 sum1b sum2b sum3b sum4b sum5b sum6b sum7b carry2)
    
    ;; 3rd row
    ;; out3 contains 4th bit answer
    (ripple-carry ground sum1b sum2b sum3b sum4b sum5b sum6b sum7b carry2 inB3 inA0 inA1 inA2 inA3 inA4 inA5 inA6 inA7 out3 sum1c sum2c sum3c sum4c sum5c sum6c sum7c carry3)
    
    ;; 4th row
    ;; out4 contains 5th bit answer
    (ripple-carry ground sum1c sum2c sum3c sum4c sum5c sum6c sum7c carry3 inB4 inA0 inA1 inA2 inA3 inA4 inA5 inA6 inA7 out4 sum1d sum2d sum3d sum4d sum5d sum6d sum7d carry4)
    
    ;; 5th row
    ;; out5 contains 6th bit answer
    (ripple-carry ground sum1d sum2d sum3d sum4d sum5d sum6d sum7d carry4 inB5 inA0 inA1 inA2 inA3 inA4 inA5 inA6 inA7 out5 sum1e sum2e sum3e sum4e sum5e sum6e sum7e carry5)
    
    ;; 6th row
    ;; out6 contains 7th bit answer
    (ripple-carry ground sum1e sum2e sum3e sum4e sum5e sum6e sum7e carry5 inB6 inA0 inA1 inA2 inA3 inA4 inA5 inA6 inA7 out6 sum1f sum2f sum3f sum4f sum5f sum6f sum7f carry6)
    
    ;; 7th row (last)
    (ripple-carry ground sum1f sum2f sum3f sum4f sum5f sum6f sum7f carry6 inB7 inA0 inA1 inA2 inA3 inA4 inA5 inA6 inA7 out7 out8 out9 out10 out11 out12 out13 out14 out15)
    
    'multiplier))


;; connecting all the wires to the multiplier.
(multiplier8 inputA0 inputA1 inputA2 inputA3 inputA4 inputA5 inputA6 inputA7 inputB0 inputB1 inputB2 inputB3 inputB4 inputB5 inputB6 inputB7 output0 output1 output2 output3 output4 output5 output6 output7 output8 output9 output10 output11 output12 output13 output14 output15)