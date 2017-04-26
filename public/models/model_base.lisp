
;;;;;;;;;;;;;;;;;;;;;;;;
;;   PURCHASE STAGE   ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *response*)
(defmethod rpm-window-key-event-handler ((win rpm-window) key)
    (cond
      ((string-equal key "l") (setf (nth 0 *response*) "1"))
      ((string-equal key "s") (setf (nth 1 *response*) "1"))
      ((string-equal key "i") (setf (nth 2 *response*) "1"))
      ((string-equal key "c") (setf (nth 3 *response*) "1"))
    )
)

(defun get-response ()
  (format nil "~{~A~^, ~}" *response*)
)

(defun purchase-stage (weather inventory)
  (let ((window (open-exp-window "Purchase Phase" :visible nil)))

      (install-device window)
      (setf *response* '("0" "0" "0" "0"))

      ;; Construct Goal Buffer Contents
      (let ((purchase `(
          isa game-purchase
          score nil
          weather_temperature ,(nth 0 weather)
          weather_condition ,(nth 1 weather)
          inventory_lemons ,(nth 0 inventory)
          inventory_sugar ,(nth 1 inventory)
          inventory_ice ,(nth 2 inventory)
          inventory_cups ,(nth 3 inventory)
        )))

        ;; Insert into Goal Buffer
        (if (buffer-read 'goal)
           (mod-focus-fct purchase)
           (goal-focus-fct (car (define-chunks-fct `(,purchase))))
        )
      )

      ;; Run Model
      (run 10)

      ;; Return Response
      (get-response)

    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;
;;     BASIC MODEL     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(define-model lemonade

  ;;
  ;; GOAL BUFFER DEFINITION
  ;;
  ;; Abstract Game State Type
  (chunk-type game-state state)
  (define-chunks (purchase isa chunk) (learn isa chunk))

  ;; Purchase Stage Type
  (chunk-type (game-purchase (:include game-state))
    (state purchase)
    weather_temperature
    weather_condition
    inventory_lemons
    inventory_sugar
    inventory_ice
    inventory_cups
  )

  ;; Learn Stage Type
  (chunk-type (game-learn (:include game-state))
    (state learn)
    score
  )

  ;; Create Goal Focus
  (declare-buffer-usage goal game-state :all)

  ;; Example Key Press
  (p test-keypress
   =goal>
     isa game-purchase
     state purchase
   ?manual>
     state free
  ==>
   =goal>
     state nil
   +manual>
     cmd press-key
     key "l"
    )

)
