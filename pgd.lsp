
; File name               :    pgd.lsp
;
; Created at              :    6/27/1990 16:57:04 by CL(NX) Indexer
;
; Indexed (source) file   :    pgd.lsp
;
;
; INDEX OF DEFINITIONS:
; --------------------------------------
; *var*............................   16
; base_goal........................   14
; base_plan........................   12
; clear_pgd_globals................   15
; constraint_side..................   20
; find_base_goals..................    3
; find_base_plans..................    2
; find_goals_and_plans.............    1
; higher_level_plans...............    6
; infer_goals......................    5
; infer_plans......................    4
; lhs..............................   17
; lower_level_goals................    7
; match_rules......................    8
; match_side.......................   10
; match_sub_struct.................    9
; pattern_side.....................   19
; remove_patterns..................   23
; rhs..............................   18
; subgoal..........................   13
; subplan..........................   11
; total_bindings...................   21
;
;                                    Common Lisp Indexer, v1.1, by Cem Bozsahin
; -----------------------------------------------------------------------------

;                                  -----
;                                    1
;                                  -----

(defun find_goals_and_plans (cd_list)
  "This is the goal and plan detector for Nexus. It returns a list of
    active goals and plans pursued plus the cd_list." 
  (let ((init_plans (remove-if #'null
                               (mapcar #'(lambda
                                          (cd)
                                          (and
                                           (equal (cd_event_type cd) 'theme)
                                           cd))
                                       cd_list))))
    (clear_pgd_globals)
    (find_base_plans cd_list)
    (find_base_goals cd_list)
    (infer_plans (append *active_base_plans* init_plans) *sub_plans*)
    (infer_goals (append init_plans *active_base_plans* *active_plans*
                         *active_base_goals*)
                 *sub_goals*)
    (append cd_list
            (filter (append *active_base_plans* *active_base_goals*
                            *active_plans* *active_goals*)))))

;                                  -----
;                                    2
;                                  -----

(defun find_base_plans (cd_list)
  "This function returns a list of potentially pursued base plans from the given
   cd primitives." 
  (mapc #'(lambda (cd)
            (setq *active_base_plans* (append *active_base_plans*
                                              (match_rules cd *base_plans*))))
        cd_list))

;                                  -----
;                                    3
;                                  -----

(defun find_base_goals (cd_list)
  "This function returns a list of potentially pursued base goals from cd_list
   and *active_base_plans*." 
  (mapc #'(lambda (cd)
            (setq *active_base_goals* (append *active_base_goals*
                                              (match_rules cd *base_goals*))))
        (append cd_list *active_base_plans*)))

;                                  -----
;                                    4
;                                  -----

(defun infer_plans (input_plan_list plan_list)
  "this function recursively goes up in the plan hierarchy. In each recursion
  a higher level of hierarchy is matched, until no more plans can be 
  inferred." 
  (monitor "entering infer_plans") 
  (cond (plan_list
         (let ((plans (higher_level_plans input_plan_list plan_list)))
           (cond (plans
                  (infer_plans (append input_plan_list plans)
                               (remove_patterns plans plan_list)))
                 (t nil))))
        (t nil)))

;                                  -----
;                                    5
;                                  -----

(defun infer_goals (input_plan_list goal_list)
  "this function recursively goes down in the goal hierarchy. In each recursion
  a lower level of hierarchy is matched, until no more goals can be 
  inferred." 
  (monitor "entering infer_goals") 
  (cond (goal_list
         (let ((goals (lower_level_goals input_plan_list goal_list)))
           (cond (goals
                  (infer_goals (append input_plan_list goals)
                               (remove_patterns goals goal_list)))
                 (t nil))))
        (t nil)))

;                                  -----
;                                    6
;                                  -----

(defun higher_level_plans (plan_list goal_list)
  "This function finds out higher level plans which can be inferred 
   from a given plan_list. It returns the newly found plans.
   Its side effect is to save that list in the *active_plans*. See also
   lower_level_goals for comments." 
  (let ((more_plans nil) 
        (inf_plan nil))
    (mapc #'(lambda (goal)
              (setq inf_plan (match_sub_struct (append more_plans plan_list)
                                               goal)) 
              (and inf_plan (push inf_plan more_plans)))
          goal_list)
    (setq *active_plans* (append *active_plans* more_plans))
    more_plans))

;                                  -----
;                                    7
;                                  -----

(defun lower_level_goals (plan_list goal_list)
  "This function finds out lower level goals which can be inferred 
   from a given plan_list. It returns the newly found goals.
   Its side effect is to save that list in the *active_goals*. Note that
   plans detected in this iteration are also passed to match_sub_struct.
   This allows the detector to match a substructure which contains plan
   segments which are at different levels of plan hierarchy. got that?" 
  (let ((more_goals nil) 
        (inf_goal nil))
    (mapc #'(lambda (goal)
              (setq inf_goal (match_sub_struct (append more_goals plan_list)
                                               goal)) 
              (and inf_goal (push inf_goal more_goals)))
          goal_list)
    (setq *active_goals* (append *active_goals* more_goals))
    more_goals))

;                                  -----
;                                    8
;                                  -----

(defun match_rules (cd rule_list)
  "This function matches plans/subgoals against their goals and plans." 
  (let ((inf_list nil))
    (mapc #'(lambda (rule)
              (let* ((rh_sides (rhs rule)) 
                     (bindings (mapcar #'(lambda
                                          (rhs_rule)
                                          (let
                                           ((bd (match_side rhs_rule cd nil)))
                                           (and (cdr bd) bd)))
                                       rh_sides)))
                (and (and_function bindings)
                     (push (instantiate (pattern_side (lhs rule))
                                        (total_bindings bindings))
                           inf_list))))
          rule_list)
    (remove-if #'null inf_list)))

;                                  -----
;                                    9
;                                  -----

(defun match_sub_struct (plans goal)
  "This function matches plans/subgoals against their goals and plans. If the
   right hand side of a rule contains more than one plan/subgoal, then the
   conjunction has to be satisfied in order for goal to be satisfied. also,
   since lhs is bound to all bindings found, missing links (e.g., one plan
   supplying one slot, another supplying another slot) can be set. Similar
   to match_rules but conjunctions are taken care of." 
  (let ((rh_sides (rhs goal)) 
        (bindings nil) 
        (binding nil))
    (mapc #'(lambda (rhs_rule)
              (setq binding (car (sort (mapcar #'(lambda
                                                  (cd)
                                                  (let
                                                   ((bd
                                                     (match_side
                                                      rhs_rule
                                                      cd
                                                      nil)))
                                                   (and (cdr bd) bd)))
                                               plans)
                                       #'> :key
                                       #'(lambda (b) (list-length (cdr b)))))) 
              (push binding bindings))
          rh_sides)
    (and (and_function bindings)
         (instantiate (pattern_side (lhs goal)) (total_bindings bindings)))))

;                                  -----
;                                    10
;                                  -----

(defun match_side (side const bd)
  (let ((*current_bd* (match (pattern_side side) const bd)))
    (and *current_bd* (eval (constraint_side side)) *current_bd*)))

;                                  -----
;                                    11
;                                  -----

(defun subplan (rule) (push rule *sub_plans*))

;                                  -----
;                                    12
;                                  -----

(defun base_plan (rule) (push rule *base_plans*))

;                                  -----
;                                    13
;                                  -----

(defun subgoal (rule) (push rule *sub_goals*))

;                                  -----
;                                    14
;                                  -----

(defun base_goal (rule) (push rule *base_goals*))

;                                  -----
;                                    15
;                                  -----

(defun clear_pgd_globals ()
  (setq *active_base_plans* nil
        *active_base_goals* nil
        *active_plans* nil
        *active_goals* nil))

;                                  -----
;                                    16
;                                  -----

(defun *var* (l) (filler_role l *current_bd*))

;                                  -----
;                                    17
;                                  -----

(defun lhs (rule) (car rule))

;                                  -----
;                                    18
;                                  -----

(defun rhs (rule) (cdr rule))

;                                  -----
;                                    19
;                                  -----

(defun pattern_side (side) (car side))

;                                  -----
;                                    20
;                                  -----

(defun constraint_side (side) (cond ((cdr side) (cadr side)) (t t)))

;                                  -----
;                                    21
;                                  -----

(defun total_bindings (bindings)
  "This function lumps all bindings into a single bindings list. It is
   assumed that the first bindings is not null." 
  (let ((b_list (list t)))
    (mapc #'(lambda (b) (mapc #'(lambda (bv) (push bv b_list)) (cdr b)))
          bindings)
    (reverse b_list)))

;                                  -----
;                                    22
;                                  -----

(proclaim '(special *sub_plans* *sub_goals* *base_plans* *base_goals*
            *active_base_plans* *active_base_goals* *active_plans*
            *active_goals* *current_bd*))

;                                  -----
;                                    23
;                                  -----

(defun remove_patterns (instances patterns)
  "This function removes the associated patterns of instances from the
   patterns list. Association is by 'name. It returns the patterns whose
   names do not match that of instances." 
  (do* ((in_list instances (cdr in_list)) 
        (inst (car in_list) (car in_list)) 
        (name (filler_role 'name inst) (filler_role 'name inst)) 
        (pats (remove-if #'(lambda (x) (equal name x)) patterns :key
                         #'(lambda (x) (filler_role 'name (caar x))))
              (remove-if #'(lambda (x) (equal name x)) pats :key
                         #'(lambda (x) (filler_role 'name (caar x))))))
       ((endp in_list) pats)))
