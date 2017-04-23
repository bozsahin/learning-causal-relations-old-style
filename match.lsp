
; File name               :    match.lsp
;
; Created at              :    6/27/1990 16:57:58 by CL(NX) Indexer
;
; Indexed (source) file   :    match.lsp
;
;
; INDEX OF DEFINITIONS:
; --------------------------------------
; find_best_match..................   16
; instantiate......................   17
; match............................    1
; match_args.......................    2
; match_constants..................   12
; match_episodes...................    7
; match_events.....................    9
; match_event_patterns.............    8
; match_graph......................    6
; match_hypotheses.................    5
; match_hypothesis.................    4
; match_roles......................   13
; match_var........................    3
; partial_ltm_match................   11
; partial_match....................   10
; recall_role_var_associations.....   14
; role_similarity_factor...........   15
;
;                                    Common Lisp Indexer, v1.1, by Cem Bozsahin
; -----------------------------------------------------------------------------

;                                  -----
;                                    1
;                                  -----

(defun match (pat const bindings)
  "This is the CD pattern matcher (a little extended). " 
  (let ((binding_form (or bindings (list t))))
    (cond ((or (null const) (equal pat const)) binding_form)
          ((is_var pat) (match_var pat const binding_form))
          ((or (atom const) (atom pat)) nil)
          ((equal (header_cd pat) (header_cd const))
           (match_args (roles_cd pat) const binding_form))
          ((and (member (header_cd pat) '(plan goal))
                (member (header_cd const) '(plan goal))
                (equal (filler_role 'name pat) (filler_role 'name const)))
           (match_args (roles_cd pat) const binding_form)))))

;                                  -----
;                                    2
;                                  -----

(defun match_args (pat_args const binding_form)
  (repeat
    (initial pat_arg nil const_val nil) 
    (while (setq pat_arg (pop pat_args))) 
    (do (setq 
         const_val 
         (filler_role (role_pair pat_arg) const))) 
    (while
     (setq binding_form (match (filler_pair pat_arg) const_val binding_form))) 
    (result binding_form)))

;                                  -----
;                                    3
;                                  -----

(defun match_var (pat const binding_form)
  (let ((var_value (filler_role (name_var pat) binding_form)))
    (cond (var_value (match var_value const binding_form))
          (t (cons_end binding_form (list (name_var pat) const))))))

;                                  -----
;                                    4
;                                  -----

(defun match_hypothesis (hypo episode)
  "This function matches the event patterns in hypo against the events 
   in the episode. The matches are saved in the property lists of dynamic
   variables of the hypothesis." 
  (mapc #'(lambda (pat)
            (mapc #'(lambda (ev)
                      (setq m (match (cadr pat) ev nil)) 
                      (cond ((> (list-length m)
                                (list-length (get (eval (car pat)) 'match)))
                             (setf (get (eval (car pat)) 'match) m))))
                  episode))
        (causal_hypothesis-event_list hypo)) 
  t)

;                                  -----
;                                    5
;                                  -----

(defun match_hypotheses (episode)
  "This function finds the hypothesis in the procedural LTM which best
   matches the events in the episode. That hypothesis is returned." 
  (find_hypothesis (car (find_max (mapcar #'(lambda
                                             (h)
                                             (match_graph
                                              (causal_hypothesis-name h)
                                              (causal_hypothesis-event_list h)
                                              episode))
                                          *hypos*)))))

;                                  -----
;                                    6
;                                  -----

(defun match_graph (graph_name graph_e_list episode)
  "This function returns a list of name and number represnting the matched
   episode and its match count." 
  (list graph_name
        (apply #'+
               (mapcar #'(lambda (e) (match_event_patterns e episode))
                       graph_e_list))))

;                                  -----
;                                    7
;                                  -----

(defun match_episodes (new_episode ltm_episode)
  "This function matches the new episode against the episodes in the
   long term memory. It returns a list which
   contains the episode name and the match count for that episode. A list of
   these lists will later form the short term memory (STM)." 
  (cond ((= (list-length (episode-event_list ltm_episode)) 0)
         (list (episode-name ltm_episode) 0.0))
        (t
         (list (episode-name ltm_episode)
               (apply #'+
                      (mapcar #'(lambda (event)
                                  (match_events event
                                                (episode-event_list
                                                 ltm_episode)))
                              new_episode))))))

;                                  -----
;                                    8
;                                  -----

(defun match_event_patterns (event_pattern episode_events)
  "This function matches the event (cd_pattern)  with the events in
   the episode (extracted from LTM). It returns a list of match counts which
   can contain exact(1),partial(0<x<1) and no matches (0). 'Match' is the
   CD pattern matcher." 
  (mapcar #'(lambda (e)
              (partial_match event_pattern (match event_pattern e nil)))
          episode_events))

;                                  -----
;                                    9
;                                  -----

(defun match_events (event episode_events)
  "This function matches the event (a cd constant)  with the events in
   the episode (extracted from LTM). It returns the max match count which
   may be exact(1),partial(0<x<1) and no matches (0). " 
  (apply #'max
         (mapcar #'(lambda (e) (match_constants event e)) episode_events)))

;                                  -----
;                                    10
;                                  -----

(defun partial_match (event ev_list)
  "This function returns the event in ev_list which is partially matched 
   (over threshold) to event. If none, nil is returned. Used for finding
   constant conjunctions among events." 
  (let* ((match_list (mapcar #'(lambda (e) (match_constants event e)) ev_list)) 
         (max_match (apply #'max match_list)) 
         (max_match_index (position max_match match_list)))
    (cond ((> max_match 0.5) (nth max_match_index ev_list)) (t nil))))

;                                  -----
;                                    11
;                                  -----

(defun partial_ltm_match (matched_episode total_events total_events_ltm_ep)
  "This function returns a list of matched episode name plus a match value
    which is explained in chapter 5. It is a weighted match of new events.
    The threshold value is fraction of events (match_count/total_events)
     being > .50. Below this threshold, it returns null list (no match)." 
  (let ((e_name (car matched_episode)) 
        (match_count (cadr matched_episode)))
    (cond ((or (= total_events 0)
               (= total_events_ltm_ep 0)
               (< (/ match_count (* 1.0 total_events)) 0.5))
           nil)
          (t
           (list e_name
                 (/ (* match_count total_events)
                    (* 1.0 total_events_ltm_ep)))))))

;                                  -----
;                                    12
;                                  -----

(defun match_constants (source_cd target_cd)
  "This function does to cd constants what 'match' does to cd patterns. A role
   in source_cd matches the same role in target_cd if its role value is subsumed
   by the value of respective target_cd role. It returns a number 0<=x<=1
   which shows the success of match." 
  (cond ((equal source_cd target_cd) 1.0)
        ((or (atom source_cd) (atom target_cd)) 0.0)
        ((or (and (basic_cd source_cd)
                  (basic_cd target_cd)
                  (requal (header_cd source_cd) (header_cd target_cd)))
             (and (high_level_cd source_cd)
                  (high_level_cd target_cd)
                  (requal (filler_role 'name source_cd)
                          (filler_role 'name target_cd))))
         (match_roles source_cd target_cd))
        (t 0.0)))

;                                  -----
;                                    13
;                                  -----

(defun match_roles (source_role_list target_role_list)
  "This function matches each role in source against target roles. Returns 
   the fraction of role values matched. Tense, time, and cause attributes
   are left out of the count." 
  (let* ((result 0.0) 
         (leave 0.0))
    (and (filler_role 'time source_role_list) (setq leave (+ leave 1)))
    (and (filler_role 'tense source_role_list) (setq leave (+ leave 1)))
    (and (filler_role 'cause source_role_list) (setq leave (+ leave 1)))
    (and (= (list-length (roles_cd source_role_list)) leave) (return 0.0))
    (dolist (source_role (roles_cd source_role_list))
      (cond ((not (member (role_pair source_role) '(tense time cause)))
             (let ((target_value (filler_role (role_pair source_role)
                                              target_role_list)))
               (and target_value
                    (or (subsume target_value (filler_pair source_role))
                        (subsume (filler_pair source_role) target_value))
                    (setq result (+ result 1.0)))))))
    (/ result (- (list-length (roles_cd source_role_list)) leave))))

;                                  -----
;                                    14
;                                  -----

(defun recall_role_var_associations (cd_role_pattern)
  "This function is called solely for its side effects. It makes a symbol
   out of each variable in the cd_role_pattern and assigns it its role type
   as value. E.g., (actor (*var* x)) is turned into dynamic symbol x
   with 'actor as its value. It is needed later to find out the similarity
   of roles. Dynamic symbol names passed by Nexus are internally
   generated hence do not cause name clashes." 
  (cond ((null cd_role_pattern) nil)
        (t
         (cond ((is_var (cadar cd_role_pattern))
                (set (name_var (cadar cd_role_pattern))
                     (caar cd_role_pattern))))
         (recall_role_var_associations (cdr cd_role_pattern)))))

;                                  -----
;                                    15
;                                  -----

(defun role_similarity_factor (cd_constant_roles matched_roles role_var_assoc)
  "This function returns a role similarity factor, a number in unit interval. 
   The ratio is similar roles divided by total number of matched roles. Two
   roles are similar if they are of same role type and their slot values
   subsume each other. This function makes use of the temporary dynamic vars
   created by recall_role_var_associations. Then it disposes of them using
   makunbound. See comments on recall_role_var_associations." 
  (cond ((null (cdr matched_roles)) 0.0)
        (t
         (/ (apply #'+
                   (mapcar #'(lambda (x)
                               (cond ((subsume (filler_pair x)
                                               (second
                                                (assoc
                                                 (eval (role_pair x))
                                                 cd_constant_roles)))
                                      (makunbound (role_pair x))
                                      1)
                                     (t 0)))
                           matched_roles))
            (list-length matched_roles)))))

;                                  -----
;                                    16
;                                  -----

(defun find_best_match (event match_list)
  "This function finds the match value with the max. number of attributes
   matched." 
  (cond ((< (list-length event) 2) 0.0)
        (t
         (/ (- (apply #'max (mapcar #'list-length match_list)) 1)
            (- (list-length event) 1)))))

;                                  -----
;                                    17
;                                  -----

(defun instantiate (cd_form bindings)
  "This is an extended version of the CD instantiator. 2nd clause allows
   lists as values, 3rd clause allows deep nesting of CD forms, and the 4th
    clause allows digging up for a deeply buried variable!" 
  (cond ((atom cd_form) cd_form)
        ((= (list-length cd_form) 1) cd_form)
        ((is_var cd_form)
         (instantiate (filler_role (name_var cd_form) bindings) bindings))
        ((cd_event_type cd_form)
         (cons (header_cd cd_form)
               (foritem (pair in (roles_cd cd_form))
                 (save
                  (list (role_pair pair)
                        (instantiate (filler_pair pair) bindings))))))
        (t (list (car cd_form) (instantiate (cadr cd_form) bindings)))))
