
((is (actor (human (male (jack)))) (tense (simple (past)))
  (co_agent (human (female (sue))))
  (location (artifact (dwelling (townville)))) (time (at |time-36|)))
 (is (actor (human (male (jack)))) (object (human (male (jack))))
  (state (marital (val (10)))) (tense (perfect (past)))
  (to (human (female (ann)))) (time (at |time-37|)))
 (ptrans (actor (human (female (ann))))
         (instrument (artifact (transportation (car))))
         (cause (human (female (ann)))) (to (artifact (dwelling (townville))))
         (tense (simple (past))) (co_agent (human (female (lisa))))
         (time (at (time (date (august-1979))))))
 (attend (actor (human (female (ann)))) (object (human (female (ann))))
  (tense (simple (past))) (co_agent (human (female (lisa))))
  (time (before (ptrans (actor (human (male (jack))))))))
 (attend (actor (human (female (ann)))) (object (human (female (ann))))
  (tense (simple (past))) (location (artifact (transportation (car))))
  (time (at |time-38|)))
 (propel (actor (human (female (lisa)))) (cause (human (female (lisa))))
         (object (artifact (opening (door)))) (tense (simple (past)))
         (time (at |time-39|)))
 (plan (name see) (planner (human (female (lisa))))
       (objective
        (is (actor (human (female (sue)))) (object (human (female (sue))))
         (location (artifact (dwelling (house))))))
       (tense (simple (past))) (time (at |time-40|)))
 (mtrans (actor (human (female (ann))))
         (object
          (plan (name get) (planner (human (female (ann))))
                (objective
                 (ptrans (actor (human (male (jack))))
                         (object (human (male (jack))))
                         (to (human (female (ann))))))))
         (to (human (female (lisa)))) (tense (perfect (past)))
         (time (at |time-41|)))
 (propel (actor (human (female (sue)))) (cause (human (female (sue))))
         (object (artifact (opening (door)))) (state (object (open)))
         (tense (simple (past))) (time (at |time-42|)))
 (propel (actor (human (female (ann)))) (cause (human (female (ann))))
         (object (human (female (sue)))) (tense (simple (past)))
         (to (artifact (transportation (car)))) (time (at |time-43|)))
 (speak (actor (human (female (sue)))) (state (intensity (val (10))))
  (tense (simple (past))) (time (at |time-44|)))
 (ptrans (actor (human (female (ann)))) (cause (human (female (ann))))
         (object (artifact (transportation (car)))) (tense (simple (past)))
         (to (state (val (-10)))) (time (at |time-45|)))
 (attend (actor (group (name (neighbors))))
  (object (speak (state (intensity (val (10)))))) (tense (simple (past)))
  (time (at |time-46|)))
 (attend (from (group (name (neighbors)))) (actor (group (law (police))))
  (tense (simple (past))) (time (at |time-47|)))
 (is (actor (human (female (sue)))) (object (human (female (sue))))
  (state (location (val (-10)))) (tense (perfect (present)))
  (time (at |time-48|)))
 (plan (name is_at) (planner (human (male (jack))))
       (object (human (male (jack))))
       (location (artifact (dwelling (townville)))))
 (plan (name is_at) (planner (human (female (sue))))
       (object (human (female (sue))))
       (location (artifact (dwelling (townville)))))
 (plan (name divorced) (planner (human (male (jack))))
       (object (human (female (ann)))))
 (plan (name go_in_group) (planner (human (female (ann))))
       (object (human (female (lisa))))
       (location (artifact (dwelling (townville)))))
 (plan (name do_after_event) (planner (human (female (ann))))
       (object (before (ptrans (actor (human (male (jack))))))))
 (plan (name look_in) (planner (human (female (lisa))))
       (object (artifact (dwelling (house)))))
 (plan (name let_know) (planner (human (female (ann))))
       (to (human (female (lisa))))
       (object
        (plan (name get) (planner (human (female (ann))))
              (objective
               (ptrans (actor (human (male (jack))))
                       (object (human (male (jack))))
                       (to (human (female (ann)))))))))
 (plan (name see_otherside) (planner (human (female (sue))))
       (object (artifact (dwelling (house)))))
 (plan (name force_into) (planner (human (female (ann))))
       (object (human (female (sue)))) (to (artifact (transportation (car)))))
 (plan (name not_agree) (planner (human (female (sue))))
       (object (human (female (ann)))))
 (plan (name attention) (planner (human (female (sue))))
       (object (speak (state (intensity (val (10)))))))
 (plan (name surprised) (planner (human (female (sue))))
       (object (speak (state (intensity (val (10)))))))
 (plan (name run_away) (planner (human (female (ann))))
       (object (artifact (transportation (car)))) (to (state (val (-10)))))
 (plan (name know_event) (planner (group (name (neighbors))))
       (object (speak (state (intensity (val (10)))))))
 (plan (name call_help) (planner (group (name (neighbors))))
       (object (group (law (police)))))
 (plan (name accomplice) (planner (human (female (ann))))
       (object (human (female (lisa)))))
 (plan (name snatch) (planner (human (female (ann))))
       (object (human (female (sue))))
       (instrument (artifact (transportation (car)))))
 (plan (name snatch) (planner (human (female (ann))))
       (object (human (female (sue))))
       (instrument (artifact (transportation (car)))))
 (plan (name wait_for_event) (planner (human (female (ann))))
       (act (before (ptrans (actor (human (male (jack))))))))
 (plan (name ask_help) (planner (human (female (ann))))
       (object (human (female (lisa))))
       (objective
        (plan (name get) (planner (human (female (ann))))
              (objective
               (ptrans (actor (human (male (jack))))
                       (object (human (male (jack))))
                       (to (human (female (ann)))))))))
 (plan (name not_agree) (planner (human (female (sue))))
       (object (human (female (ann)))))
 (goal (name provide_help) (planner (group (law (police))))
  (object (group (name (neighbors)))))
 (plan (name find_opport) (planner (human (female (ann))))
       (act (before (ptrans (actor (human (male (jack))))))))
 (goal (name help) (planner (group (law (police))))
  (object (human (female (sue)))))
 (plan (name check_house) (planner (human (female (lisa))))
       (objective (prox (actor (human (female (sue))))))
       (object (artifact (dwelling (house)))))
 (goal (name abduct) (planner (human (female (ann))))
  (object (human (female (sue)))) (co_planner (human (female (lisa)))))
 (plan (name attention) (planner (human (female (sue))))
       (object (speak (state (intensity (val (10)))))))
 (goal (name help) (planner (group (law (police))))
  (object (group (name (neighbors)))))
 (plan (name find_someone) (planner (human (female (lisa))))
       (object (human (female (sue)))))
 (plan (name find_opport) (planner (human (female (lisa))))
       (object (human (female (sue)))))
 (plan (name find_opport) (planner (human (female (ann))))
       (co_planner (human (female (lisa)))) (object (human (female (sue))))))
