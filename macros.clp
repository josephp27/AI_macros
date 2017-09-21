(deffacts initial-phase
   (phase set-goal))

(defrule goal-select
   (phase set-goal)
   =>
   (printout t "Are you trying to gain, maintain, or lose weight? (Gain: g "
               "Maintain: m Lose: l)? ")
   (bind ?input (read))
   (assert (goal-set ?input))
   (assert (phase set-weight)))

(defrule bad-goal-choice 
   ?phase <- (phase set-goal)
   ?choice <- (goal-set ?weight&~g&~m&~l)
   =>
   (retract ?phase ?choice)
   (assert (phase set-goal))
   (printout t "Choose g, m, or l." crlf))



(defrule weight-select
   (phase set-weight)
   ?choice <- (goal-set ?goal&g|m|l)
   =>
   (printout t "Bodyweight this week compared from last? Mark same if it within 0.5 lbs. (Above: a "
               "Below: b Same: s)? ")
   (bind ?input (read))
   (assert (weight-set ?input)))

(defrule bad-weight-choice 
   ?phase <- (phase set-weight)
   ?choice <- (weight-set ?weight&~a&~b&~s)
   =>
   (retract ?phase ?choice)
   (assert (phase set-weight))
   (printout t "Choose a, b, or s." crlf))




(defrule above-weight-pro
   (phase set-weight)
   ?choice <- (weight-set ?weight&a)
   =>
   (printout t "Protein: Increase to 1g/1lb of bodyweight" crlf))

(defrule below-weight-pro 
   (phase set-weight)
   ?choice <- (weight-set ?weight&b)
   =>
   (printout t "Protein: Decrease to 1g/1lb of bodyweight" crlf))

(defrule same-weight-pro 
   (phase set-weight)
   ?choice <- (weight-set ?weight&s)
   =>
   (printout t "Protein: Keep same" crlf))



(defrule lose-above
   (phase set-weight)
   ?choice <- (weight-set ?weight&a)
   ?goal_choice <- (goal-set ?goal&l)
   =>
   (printout t "Fat: Decrease by 1g" crlf)
   (printout t "Carbohydrates: Decrease by 15g" crlf))

(defrule lose-same
   (phase set-weight)
   ?choice <- (weight-set ?weight&s)
   ?goal_choice <- (goal-set ?goal&l)
   =>
   (printout t "Fat: Keep same" crlf)
   (printout t "Carbohydrates: Decrease by 10g" crlf))

(defrule lose-below
   (phase set-weight)
   ?choice <- (weight-set ?weight&b)
   ?goal_choice <- (goal-set ?goal&l)
   =>
   (printout t "Fat: Keep same" crlf)
   (printout t "Carbohydrates: Keep same" crlf))


(defrule gain-same
   (phase set-weight)
   ?choice <- (weight-set ?weight&s)
   ?goal_choice <- (goal-set ?goal&g)
   =>
   (printout t "Fat: Keep same" crlf)
   (printout t "Carbohydrates: Increase by 10g" crlf))

(defrule gain-above
   (phase set-weight)
   ?choice <- (weight-set ?weight&a)
   ?goal_choice <- (goal-set ?goal&g)
   =>
   (printout t "Fat: Keep same" crlf)
   (printout t "Carbohydrates: Keep same" crlf))

(defrule gain-below
   (phase set-weight)
   ?choice <- (weight-set ?weight&b)
   ?goal_choice <- (goal-set ?goal&g)
   =>
   (printout t "Fat: Increase by 1g" crlf)
   (printout t "Carbohydrates: Increase by 15g" crlf))


(defrule maintain-same
   (phase set-weight)
   ?choice <- (weight-set ?weight&s)
   ?goal_choice <- (goal-set ?goal&m)
   =>
   (printout t "Fat: Keep same" crlf)
   (printout t "Carbohydrates: Keep same" crlf))

(defrule maintain-above
   (phase set-weight)
   ?choice <- (weight-set ?weight&a)
   ?goal_choice <- (goal-set ?goal&m)
   =>
   (printout t "Fat: Keep same" crlf)
   (printout t "Carbohydrates: Decrease by 10g" crlf))

(defrule maintain-below
   (phase set-weight)
   ?choice <- (weight-set ?weight&b)
   ?goal_choice <- (goal-set ?goal&m)
   =>
   (printout t "Fat: Keep same" crlf)
   (printout t "Carbohydrates: Increase by 10g" crlf))



(reset)
(run)
