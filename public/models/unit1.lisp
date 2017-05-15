  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;      YOUR MODEL     ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-model lemonade
    (sgp :v nil)

    ;;BEGIN-MODEL

    (p test-keypress-l
       =goal>
         isa game-purchase
         state purchase
       ?manual>
         state free
      ==>
       =goal>
         state purchase-l
       +manual>
         cmd press-key
         key "l"
      )

      (p test-keypress-s
       =goal>
         isa game-purchase
         state purchase-l
       ?manual>
         state free
      ==>
       =goal>
         state purchase-s
       +manual>
         cmd press-key
         key "s"
      )

      (p test-keypress-i
       =goal>
         isa game-purchase
         state purchase-s
       ?manual>
         state free
      ==>
       =goal>
         state purchase-i
       +manual>
         cmd press-key
         key "i"
      )

      (p test-keypress-c
       =goal>
         isa game-purchase
         state purchase-i
       ?manual>
         state free
      ==>
       =goal>
         state nil
       +manual>
         cmd press-key
         key "c"
      )

  ;;END-MODEL
  )

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
        (add-text-to-exp-window :window "Purchase Phase" :x 50 :y 50 :text (nth 1 weather))

        ;; Construct Goal Buffer Contents
        (let ((purchase `(
            state purchase
            weather_temperature ,(nth 0 weather)
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
