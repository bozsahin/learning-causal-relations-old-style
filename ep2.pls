
((is (actor (human (male (daniel)))) (object (human (male (daniel))))
  (state (marital (val (-10)))) (tense (perfect (past)))
  (co_agent (human (female (mary)))) (time (at |time-1|)))
 (attend (actor (human (male (daniel)))) (object (human (male (daniel))))
  (to (human (female (mary)))) (tense (simple (past)))
  (time (after (action (earning (work))))))
 (propel (actor (human (male (daniel)))) (cause (human (male (daniel))))
         (object (human (female (mary)))) (tense (simple (past)))
         (to (artifact (transportation (car)))) (time (at |time-2|)))
 (mtrans (actor (human (female (mary)))) (object (human (male (daniel))))
         (state (mood (val (-10)))) (time (at |time-3|)))
 (propel (actor (human (male (daniel)))) (cause (human (male (daniel))))
         (object (human (female (mary)))) (state (intensity (val (8))))
         (tense (simple (past))) (to (artifact (transportation (car))))
         (time (at |time-4|)))
 (ptrans (actor (human (male (daniel)))) (cause (human (male (daniel))))
         (object (artifact (transportation (car)))) (tense (simple (past)))
         (to (artifact (dwelling (house)))) (time (at |time-5|)))
 (plan (name threat) (planner (human (male (daniel))))
       (object (human (female (mary))))
       (state (do (actor 
                   (human (female (mary))))
                  (to (action (be_free (escape))))))
       (objective
        (propel (object (human (female (mary)))) (state (health (val (-10))))))
       (time (at |time-6|)))
 (plan (name allow) (planner (human (male (daniel))))
       (object (human (female (mary))))
       (objective
        (attend (from (human (female (mary))))
         (actor (group (family (parents))))))
       (tense (simple (past))) (time (at |time-7|)))
 (ptrans (actor (group (law (police)))) (cause (group (law (police))))
         (tense (simple (past))) (co_agent (group (health (medicine))))
         (to (artifact (dwelling (house)))) (time (at |time-8|)))
 (mtrans (actor (human (male (daniel))))
         (object
          (grasp (actor (human (female (mary))))
                 (object (group (family (child))))))
         (to (group (law (police)))) (tense (simple (past)))
         (to (state (val (-10)))) (time (at |time-9|)))
 (mtrans (actor (human (female (mary))))
         (object
          (ptrans (actor (human (female (mary))))
                  (state (action (val (-10))))))
         (to (human (male (daniel)))) (tense (simple (future)))
         (human (male (daniel))) (time (at |time-10|)))
 (plan (name allow) (planner (human (male (daniel))))
       (object (human (female (mary))))
       (objective
        (ptrans (actor (human (female (mary))))
                (cause (human (female (mary))))))
       (tense (simple (past))) (time (at |time-11|)))
 (ptrans (actor (group (law (police)))) (object (human (male (daniel))))
         (to (artifact (law_enforcement (jail)))) (time (at |time-12|)))
 (plan (name find_someone) (planner (human (male (daniel))))
       (object (human (female (mary)))))
 (plan (name force_into) (planner (human (male (daniel))))
       (object (human (female (mary)))) (to (artifact (transportation (car)))))
 (plan (name not_agree) (planner (human (female (mary))))
       (object (human (male (daniel)))))
 (plan (name not_agree) (planner (human (female (mary))))
       (object (human (male (daniel)))))
 (plan (name force_into) (planner (human (male (daniel))))
       (object (human (female (mary)))) (to (artifact (transportation (car)))))
 (plan (name not_agree) (planner (human (female (mary))))
       (object (human (male (daniel)))))
 (plan (name go_in_group) (planner (group (law (police))))
       (object (group (health (medicine))))
       (location (artifact (dwelling (house)))))
 (plan (name arrest) (planner (group (law (police))))
       (object (human (male (daniel)))))
 (plan (name know_event) (planner (group (family (parents))))
       (objective
        (attend (from (human (female (mary))))
         (actor (group (family (parents)))))))
 (plan (name snatch) (planner (human (male (daniel))))
       (object (human (female (mary))))
       (instrument (artifact (transportation (car)))))
 (plan (name not_agree) (planner (human (female (mary))))
       (object (human (male (daniel)))))
 (plan (name not_agree) (planner (human (female (mary))))
       (object (human (male (daniel)))))
 (goal (name help) (planner (group (law (police))))
  (object (human (female (mary)))))
 (plan (name find_opport) (planner (human (male (daniel))))
       (object (human (female (mary)))))
 (goal (name abduct) (planner (human (male (daniel))))
  (object (human (female (mary))))))