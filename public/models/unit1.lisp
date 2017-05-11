
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;      YOUR MODEL     ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;BEGIN-MODEL
  (define-model lemonade

    ;;
    ;; SET PARAMETERS
    ;;
    (sgp :v nil)

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

    ;;
    ;; PURCHASE STAGE
    ;;
    (p should-buy
     =goal>
       isa game-purchase
       state purchase
    ==>
     =goal>
       state purchase-l
       buy true
    )

    (p purchase-l
     =goal>
       isa game-purchase
       state purchase-l
       buy true
       < inventory_lemons 50
     ?manual>
       state free
    ==>
     =goal>
       state purchase-s
     +manual>
       cmd press-key
       key "l"
      )

    (p no-purchase-l
     =goal>
       isa game-purchase
       state purchase-l
       buy true
       >= inventory_lemons 50
     ?manual>
       state free
    ==>
     =goal>
       state purchase-s
      )

    (p purchase-s
     =goal>
       isa game-purchase
       state purchase-s
       buy true
       < inventory_sugar 25
     ?manual>
       state free
    ==>
     =goal>
       state purchase-i
     +manual>
       cmd press-key
       key "s"
      )

    (p no-purchase-s
     =goal>
       isa game-purchase
       state purchase-s
       buy true
       >= inventory_sugar 25
     ?manual>
       state free
    ==>
     =goal>
       state purchase-i
      )

    (p purchase-i
     =goal>
       isa game-purchase
       state purchase-i
       buy true
     ?manual>
       state free
    ==>
     =goal>
       state purchase-c
     +manual>
       cmd press-key
       key "i"
      )

    (p purchase-c
     =goal>
       isa game-purchase
       state purchase-c
       buy true
      < inventory_cups 100
     ?manual>
       state free
    ==>
     =goal>
       state nil
       buy nil
     +manual>
       cmd press-key
       key "c"
      )

      (p no-purchase-c
       =goal>
         isa game-purchase
         state purchase-c
         buy true
        >= inventory_cups 100
      ==>
       =goal>
         state nil
         buy nil
        )



  )
  ;;END-MODEL

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;   PURCHASE FUNCTION   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (setq *response* (list "0" "0" "0" "0"))

        ;; Add Weather to Outside
        (add-text-to-exp-window :window "Purchase Phase" :x 50 :y 50 :text (nth 0 weather))

        ;; Construct Goal Buffer Contents
        (let ((purchase `(
            state purchase
            weather_condition ,(nth 1 weather)
            inventory_lemons ,(nth 0 inventory)
            inventory_sugar ,(nth 1 inventory)
            inventory_ice ,(nth 2 inventory)
            inventory_cups ,(nth 3 inventory)
          )))

          ;; Insert into Goal Buffer
          (if (buffer-read 'goal)
             (mod-focus-fct purchase)
             (goal-focus-fct (car (define-chunks-fct `(,(append '(isa game-state) purchase)))))
          )
        )

        ;; Run Model
        (run 10)

        ;; Return Response
        (get-response)

      )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;     LEARN FUNCTION    ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun learn-stage (score)
    (let ((window (open-exp-window "Purchase Phase" :visible nil)))

      (install-device window)

      ;; Construct Goal Buffer Contents
      (let ((learn `(
          state learn
          score ,score
        )))

        (if (buffer-read 'goal)
           (mod-focus-fct learn)
           (goal-focus-fct (car (define-chunks-fct `(,(append '(isa game-state) learn)))))
        )
      )

      ;; Run Model
      (run-full-time 10)

      (values)
    )
  )
