
(#S(CAUSAL_HYPOTHESIS :NAME AFTER-ALL-THESE-YEARS-HYPO :TYPE NIL
    :SOURCES (AFTER-ALL-THESE-YEARS) :EVENT_LIST NIL :ROLES_LIST (NIL)
    :PROGRESSIVE NIL :REGRESSIVE NIL)
 #S(CAUSAL_HYPOTHESIS :NAME "kidnapping" :TYPE NIL
    :SOURCES ("Ann, Lisa, Jack, and Sue" "Daniel and Mary")
    :EVENT_LIST
    (('T150
      (PLAN (NAME GO_IN_GROUP) (PLANNER (*VAR* 'T79)) (OBJECT (*VAR* 'T93))
       (LOCATION (*VAR* 'T96))))
     ('T149
      (PLAN (NAME DO_AFTER_EVENT) (PLANNER (*VAR* 'T79))
       (OBJECT (*VAR* 'T110))))
     ('T148
      (PLAN (NAME LET_KNOW) (PLANNER (*VAR* 'T79)) (TO (*VAR* 'T93))
       (OBJECT (*VAR* 'T135))))
     ('T147
      (PLAN (NAME FORCE_INTO) (PLANNER (*VAR* 'T79)) (OBJECT (*VAR* 'T86))
       (TO (*VAR* 'T80))))
     ('T146
      (GOAL (NAME ABDUCT) (PLANNER (*VAR* 'T79)) (OBJECT (*VAR* 'T86))
       (CO_PLANNER (*VAR* 'T93))))
     ('T145 (DO (ACTOR (*VAR* 'T93)))) ('T144 (DO (ACTOR (*VAR* 'T86))))
     ('T143
      (PTRANS (ACTOR (*VAR* 'T79)) (INSTRUMENT (*VAR* 'T80))
       (CAUSE (*VAR* 'T79)) (TO (*VAR* 'T96)) (TENSE (*VAR* 'T81))
       (CO_AGENT (*VAR* 'T93)) (TIME (*VAR* 'T97))))
     ('T142
      (ATTEND (ACTOR (*VAR* 'T79)) (OBJECT (*VAR* 'T79)) (TENSE (*VAR* 'T81))
       (CO_AGENT (*VAR* 'T93)) (TIME (*VAR* 'T110))))
     ('T141
      (PLAN (NAME WAIT_FOR_EVENT) (PLANNER (*VAR* 'T79)) (ACT (*VAR* 'T110))))
     ('T140
      (PROPEL (ACTOR (*VAR* 'T93)) (CAUSE (*VAR* 'T93)) (OBJECT (*VAR* 'T89))
       (TENSE (*VAR* 'T81)) (TIME (*VAR* 'T94))))
     ('T139
      (MTRANS (ACTOR (*VAR* 'T79)) (OBJECT (*VAR* 'T135)) (TO (*VAR* 'T93))
       (TENSE (*VAR* 'T137)) (TIME (*VAR* 'T138))))
     ('T136
      (PLAN (NAME ASK_HELP) (PLANNER (*VAR* 'T79)) (OBJECT (*VAR* 'T93))
       (OBJECTIVE (*VAR* 'T135))))
     ('T134
      (PROPEL (ACTOR (*VAR* 'T86)) (CAUSE (*VAR* 'T86)) (OBJECT (*VAR* 'T89))
       (STATE (*VAR* 'T90)) (TENSE (*VAR* 'T81)) (TIME (*VAR* 'T91))))
     ('T133
      (PLAN (NAME DRIVE_AWAY) (PLANNER (*VAR* 'T79)) (OBJECT (*VAR* 'T80))
       (TO (*VAR* 'T82))))
     ('T132
      (PROPEL (ACTOR (*VAR* 'T79)) (CAUSE (*VAR* 'T79)) (OBJECT (*VAR* 'T86))
       (TENSE (*VAR* 'T81)) (TO (*VAR* 'T80)) (TIME (*VAR* 'T87))))
     ('T131
      (PLAN (NAME ATTENTION) (PLANNER (*VAR* 'T86)) (OBJECTIVE (*VAR* 'T122))))
     ('T130
      (PLAN (NAME SURPRISED) (PLANNER (*VAR* 'T86)) (OBJECT (*VAR* 'T122))))
     ('T129
      (PLAN (NAME KNOW_EVENT) (PLANNER (*VAR* 'T105))
       (OBJECTIVE (*VAR* 'T122))))
     ('T128
      (SPEAK (ACTOR (*VAR* 'T86)) (STATE (*VAR* 'T126)) (TENSE (*VAR* 'T81))
       (TIME (*VAR* 'T127))))
     ('T125
      (PTRANS (ACTOR (*VAR* 'T79)) (CAUSE (*VAR* 'T79)) (OBJECT (*VAR* 'T80))
       (TENSE (*VAR* 'T81)) (TO (*VAR* 'T82)) (TIME (*VAR* 'T83))))
     ('T124
      (ATTEND (ACTOR (*VAR* 'T105)) (OBJECT (*VAR* 'T122)) (TENSE (*VAR* 'T81))
       (TIME (*VAR* 'T123))))
     ('T121
      (PLAN (NAME CALL_HELP) (PLANNER (*VAR* 'T105)) (OBJECT (*VAR* 'T104))))
     ('T120 (GOAL (NAME HELP) (PLANNER (*VAR* 'T104)) (OBJECT (*VAR* 'T105))))
     ('T119
      (PLAN (NAME NOT_AGREE) (PLANNER (*VAR* 'T86)) (OBJECT (*VAR* 'T79))))
     ('T118
      (PLAN (NAME LOOK_IN) (PLANNER (*VAR* 'T93)) (OBJECT (*VAR* 'T102))))
     ('T117
      (PLAN (NAME SEE_OTHERSIDE) (PLANNER (*VAR* 'T86))
       (OBJECT (*VAR* 'T102))))
     ('T116
      (IS (ACTOR (*VAR* 'T86)) (OBJECT (*VAR* 'T86)) (STATE (*VAR* 'T113))
       (TENSE (*VAR* 'T114)) (TIME (*VAR* 'T115))))
     ('T112
      (PLAN (NAME SNATCH) (PLANNER (*VAR* 'T79)) (OBJECT (*VAR* 'T86))
       (INSTRUMENT (*VAR* 'T80))))
     ('T111
      (PLAN (NAME FIND_OPPORT) (PLANNER (*VAR* 'T79)) (ACT (*VAR* 'T110))))
     ('T109 (GOAL (NAME HELP) (PLANNER (*VAR* 'T104)) (OBJECT (*VAR* 'T86))))
     ('T108
      (PLAN (NAME FIND_OPPORT) (PLANNER (*VAR* 'T93)) (OBJECT (*VAR* 'T86))))
     ('T107
      (PLAN (NAME FIND_OPPORT) (PLANNER (*VAR* 'T79)) (CO_PLANNER (*VAR* 'T93))
       (OBJECT (*VAR* 'T86))))
     ('T106
      (GOAL (NAME PROVIDE_HELP) (PLANNER (*VAR* 'T104))
       (OBJECT (*VAR* 'T105))))
     ('T103
      (PLAN (NAME CHECK_HOUSE) (PLANNER (*VAR* 'T93)) (OBJECTIVE (*VAR* 'T101))
       (OBJECT (*VAR* 'T102))))
     ('T100
      (PLAN (NAME ACCOMPLICE) (PLANNER (*VAR* 'T79)) (OBJECT (*VAR* 'T93))))
     ('T99
      (PLAN (NAME FIND_SOMEONE) (PLANNER (*VAR* 'T93)) (OBJECT (*VAR* 'T86))))
     ('T98
      (PTRANS (ACTOR (*VAR* 'T79)) (INSTRUMENT (*VAR* 'T80)) (TO (*VAR* 'T96))
       (TENSE (*VAR* 'T81)) (CO_AGENT (*VAR* 'T93)) (TIME (*VAR* 'T97))))
     ('T95
      (PROPEL (ACTOR (*VAR* 'T93)) (OBJECT (*VAR* 'T89)) (TENSE (*VAR* 'T81))
       (TIME (*VAR* 'T94))))
     ('T92
      (PROPEL (ACTOR (*VAR* 'T86)) (OBJECT (*VAR* 'T89)) (STATE (*VAR* 'T90))
       (TENSE (*VAR* 'T81)) (TIME (*VAR* 'T91))))
     ('T88
      (PROPEL (ACTOR (*VAR* 'T79)) (OBJECT (*VAR* 'T86)) (TENSE (*VAR* 'T81))
       (TO (*VAR* 'T80)) (TIME (*VAR* 'T87))))
     ('T85
      (PTRANS (ACTOR (*VAR* 'T79)) (OBJECT (*VAR* 'T80)) (TENSE (*VAR* 'T81))
       (TO (*VAR* 'T82)) (TIME (*VAR* 'T83))))
     ('T84 (DO (ACTOR (*VAR* 'T79))))
     ('T218
      (PLAN (NAME THREAT) (PLANNER (*VAR* 'T171)) (OBJECT (*VAR* 'T175))
       (STATE (*VAR* 'T215)) (OBJECTIVE (*VAR* 'T216)) (TIME (*VAR* 'T217))))
     ('T214
      (PLAN (NAME ALLOW) (PLANNER (*VAR* 'T171)) (OBJECT (*VAR* 'T175))
       (OBJECTIVE (*VAR* 'T205)) (TENSE (*VAR* 'T165)) (TIME (*VAR* 'T213))))
     ('T212 (IS (ACTOR (*VAR* 'T171)) (STATE (*VAR* 'T211))))
     ('T210
      (PLAN (NAME FORCE_INTO) (PLANNER (*VAR* 'T171)) (OBJECT (*VAR* 'T175))
       (TO (*VAR* 'T172))))
     ('T209
      (PLAN (NAME GO_IN_GROUP) (PLANNER (*VAR* 'T164)) (OBJECT (*VAR* 'T166))
       (LOCATION (*VAR* 'T167))))
     ('T208
      (PLAN (NAME ARREST) (PLANNER (*VAR* 'T164)) (OBJECT (*VAR* 'T171))))
     ('T207 (DO (ACTOR (*VAR* 'T171))))
     ('T206
      (PLAN (NAME KNOW_EVENT) (PLANNER (*VAR* 'T204))
       (OBJECTIVE (*VAR* 'T205))))
     ('T203
      (MTRANS (ACTOR (*VAR* 'T171)) (OBJECT (*VAR* 'T200)) (TO (*VAR* 'T164))
       (TENSE (*VAR* 'T165)) (TO (*VAR* 'T201)) (TIME (*VAR* 'T202))))
     ('T199
      (GOAL (NAME ABDUCT) (PLANNER (*VAR* 'T171)) (OBJECT (*VAR* 'T175))))
     ('T198
      (ATTEND (ACTOR (*VAR* 'T171)) (OBJECT (*VAR* 'T171)) (TO (*VAR* 'T175))
       (TENSE (*VAR* 'T165)) (TIME (*VAR* 'T197))))
     ('T196
      (PLAN (NAME DRIVE_AWAY) (PLANNER (*VAR* 'T171)) (OBJECT (*VAR* 'T172))
       (TO (*VAR* 'T167))))
     ('T195
      (PROPEL (ACTOR (*VAR* 'T171)) (CAUSE (*VAR* 'T171))
       (OBJECT (*VAR* 'T175)) (TENSE (*VAR* 'T165)) (TO (*VAR* 'T172))
       (TIME (*VAR* 'T179))))
     ('T194
      (MTRANS (ACTOR (*VAR* 'T175)) (OBJECT (*VAR* 'T171))
       (STATE (*VAR* 'T192)) (TIME (*VAR* 'T193))))
     ('T191
      (PROPEL (ACTOR (*VAR* 'T171)) (CAUSE (*VAR* 'T171))
       (OBJECT (*VAR* 'T175)) (STATE (*VAR* 'T176)) (TENSE (*VAR* 'T165))
       (TO (*VAR* 'T172)) (TIME (*VAR* 'T177))))
     ('T190
      (PTRANS (ACTOR (*VAR* 'T171)) (CAUSE (*VAR* 'T171))
       (OBJECT (*VAR* 'T172)) (TENSE (*VAR* 'T165)) (TO (*VAR* 'T167))
       (TIME (*VAR* 'T173))))
     ('T189
      (PTRANS (ACTOR (*VAR* 'T164)) (CAUSE (*VAR* 'T164)) (TENSE (*VAR* 'T165))
       (CO_AGENT (*VAR* 'T166)) (TO (*VAR* 'T167)) (TIME (*VAR* 'T168))))
     ('T188
      (PTRANS (ACTOR (*VAR* 'T164)) (OBJECT (*VAR* 'T171)) (TO (*VAR* 'T186))
       (TIME (*VAR* 'T187))))
     ('T185
      (PLAN (NAME NOT_AGREE) (PLANNER (*VAR* 'T175)) (OBJECT (*VAR* 'T171))))
     ('T184
      (PLAN (NAME FIND_SOMEONE) (PLANNER (*VAR* 'T171))
       (OBJECT (*VAR* 'T175))))
     ('T183
      (PLAN (NAME SNATCH) (PLANNER (*VAR* 'T171)) (OBJECT (*VAR* 'T175))
       (INSTRUMENT (*VAR* 'T172))))
     ('T182 (GOAL (NAME HELP) (PLANNER (*VAR* 'T164)) (OBJECT (*VAR* 'T175))))
     ('T181
      (PLAN (NAME FIND_OPPORT) (PLANNER (*VAR* 'T171)) (OBJECT (*VAR* 'T175))))
     ('T180
      (PROPEL (ACTOR (*VAR* 'T171)) (OBJECT (*VAR* 'T175))
       (TENSE (*VAR* 'T165)) (TO (*VAR* 'T172)) (TIME (*VAR* 'T179))))
     ('T178
      (PROPEL (ACTOR (*VAR* 'T171)) (OBJECT (*VAR* 'T175))
       (STATE (*VAR* 'T176)) (TENSE (*VAR* 'T165)) (TO (*VAR* 'T172))
       (TIME (*VAR* 'T177))))
     ('T174
      (PTRANS (ACTOR (*VAR* 'T171)) (OBJECT (*VAR* 'T172))
       (TENSE (*VAR* 'T165)) (TO (*VAR* 'T167)) (TIME (*VAR* 'T173))))
     ('T170
      (PTRANS (ACTOR (*VAR* 'T164)) (TENSE (*VAR* 'T165))
       (CO_AGENT (*VAR* 'T166)) (TO (*VAR* 'T167)) (TIME (*VAR* 'T168))))
     ('T169 (DO (ACTOR (*VAR* 'T164)))))
    :ROLES_LIST
    (((T138 (AT |time-70|)) (T137 (PERFECT (PAST)))
      (T135
       (PLAN (NAME GET) (PLANNER (HUMAN (FEMALE (ANN))))
        (OBJECTIVE
         (PTRANS (ACTOR (HUMAN (MALE (JACK)))) (OBJECT (HUMAN (MALE (JACK))))
          (TO (HUMAN (FEMALE (ANN))))))))
      (T127 (AT |time-73|)) (T126 (INTENSITY (VAL (10)))) (T123 (AT |time-75|))
      (T122 (SPEAK (STATE (INTENSITY (VAL (10)))))) (T115 (AT |time-77|))
      (T114 (PERFECT (PRESENT))) (T113 (LOCATION (VAL (-10))))
      (T110 (BEFORE (PTRANS (ACTOR (HUMAN (MALE (JACK)))))))
      (T105 (GROUP (NAME (NEIGHBORS)))) (T104 (GROUP (LAW (POLICE))))
      (T102 (ARTIFACT (DWELLING (HOUSE))))
      (T101 (PROX (ACTOR (HUMAN (FEMALE (SUE))))))
      (T97 (AT (TIME (DATE (AUGUST-1979)))))
      (T96 (ARTIFACT (DWELLING (TOWNVILLE)))) (T94 (AT |time-68|))
      (T93 (HUMAN (FEMALE (LISA)))) (T91 (AT |time-71|)) (T90 (OBJECT (OPEN)))
      (T89 (ARTIFACT (OPENING (DOOR)))) (T87 (AT |time-72|))
      (T86 (HUMAN (FEMALE (SUE)))) (T83 (AT |time-74|))
      (T82 (STATE (VAL (-10)))) (T81 (SIMPLE (PAST)))
      (T80 (ARTIFACT (TRANSPORTATION (CAR)))) (T79 (HUMAN (FEMALE (ANN)))))
     ((T217 (AT |time-156|))
      (T216
       (PROPEL (OBJECT (HUMAN (FEMALE (MARY)))) (STATE (HEALTH (VAL (-10))))))
      (T215
       (DO (ACTOR (HUMAN (FEMALE (MARY)))) (TO (ACTION (BE_FREE (ESCAPE))))))
      (T213 (AT |time-157|))
      (T211
       (BELIEF
        (GRASP (ACTOR (HUMAN (FEMALE (MARY))))
         (OBJECT (GROUP (FAMILY (CHILD)))))))
      (T205
       (ATTEND (FROM (HUMAN (FEMALE (MARY))))
        (ACTOR (GROUP (FAMILY (PARENTS))))))
      (T204 (GROUP (FAMILY (PARENTS)))) (T202 (AT |time-159|))
      (T201 (STATE (VAL (-10))))
      (T200
       (GRASP (ACTOR (HUMAN (FEMALE (MARY))))
        (OBJECT (GROUP (FAMILY (CHILD))))))
      (T197 (AFTER (ACTION (EARNING (WORK))))) (T193 (AT |time-153|))
      (T192 (MOOD (VAL (-10)))) (T187 (AT |time-162|))
      (T186 (ARTIFACT (LAW_ENFORCEMENT (JAIL)))) (T179 (AT |time-152|))
      (T177 (AT |time-154|)) (T176 (INTENSITY (VAL (8))))
      (T175 (HUMAN (FEMALE (MARY)))) (T173 (AT |time-155|))
      (T172 (ARTIFACT (TRANSPORTATION (CAR)))) (T171 (HUMAN (MALE (DANIEL))))
      (T168 (AT |time-158|)) (T167 (ARTIFACT (DWELLING (HOUSE))))
      (T166 (GROUP (HEALTH (MEDICINE)))) (T165 (SIMPLE (PAST)))
      (T164 (GROUP (LAW (POLICE))))))
    :PROGRESSIVE
    ((CAUSE (AGENT (OR (AND (T198))))
      (EFFECT
       (OR
        (AND (T203 (MEANS SIMILARITY)) (T195 (MEANS SIMILARITY))
         (T191 (MEANS SIMILARITY))))))
     (CAUSE (AGENT (OR (AND (T190))))
      (EFFECT (OR (AND (T203 (MEANS SIMILARITY)) (T189 (MEANS SIMILARITY))))))
     (CAUSE (AGENT (OR (AND (T218))))
      (EFFECT (OR (AND (T182 (MEANS PLAN-PART)) (T185 (MEANS PLAN-PART))))))
     (CAUSE (AGENT (OR (AND (T214))))
      (EFFECT (OR (AND (T206 (MEANS PLAN-PART))))))
     (CAUSE (AGENT (OR (AND (T189))))
      (EFFECT (OR (AND (T203 (MEANS SIMILARITY))))))
     (CAUSE (AGENT (OR (AND (T212))))
      (EFFECT (OR (AND (T199 (MEANS DOMAIN))))))
     (CAUSE (AGENT (OR (AND (T208))))
      (EFFECT (OR (AND (T182 (MEANS PLAN-PART)) (T188 (MEANS CONSEQUENCE))))))
     (CAUSE (AGENT (OR (AND (T207))))
      (EFFECT
       (OR
        (AND (T174 (MEANS AGENCY)) (T180 (MEANS AGENCY))
         (T178 (MEANS AGENCY))))))
     (CAUSE (AGENT (OR (AND (T169))))
      (EFFECT (OR (AND (T170 (MEANS AGENCY))))))
     (CAUSE (AGENT (OR (AND (T84))))
      (EFFECT
       (OR
        (AND (T85 (MEANS AGENCY)) (T98 (MEANS AGENCY)) (T88 (MEANS AGENCY))))))
     (CAUSE (AGENT (OR (AND (T144)))) (EFFECT (OR (AND (T92 (MEANS AGENCY))))))
     (CAUSE (AGENT (OR (AND (T145)))) (EFFECT (OR (AND (T95 (MEANS AGENCY))))))
     (CAUSE (AGENT (OR (AND (T107))))
      (EFFECT (OR (AND (T99 (MEANS SUB-GOAL)) (T100 (MEANS SUB-GOAL))))))
     (CAUSE (AGENT (OR (AND (T108))))
      (EFFECT (OR (AND (T99 (MEANS SUB-GOAL)) (T100 (MEANS SUB-GOAL))))))
     (CAUSE (AGENT (OR (AND (T99))))
      (EFFECT
       (OR (AND (T198 (MEANS CONSEQUENCE))) (AND (T103 (MEANS SUB-GOAL))))))
     (CAUSE (AGENT (OR (AND (T120))))
      (EFFECT (OR (AND (T106 (MEANS SUB-GOAL))))))
     (CAUSE (AGENT (OR (AND (T146))))
      (EFFECT
       (OR
        (AND (T108 (MEANS SUB-GOAL)) (T111 (MEANS SUB-GOAL))
         (T100 (MEANS SUB-GOAL)) (T116 (MEANS DOMAIN)))
        (AND (T99 (MEANS SUB-GOAL)) (T184 (MEANS SUB-GOAL))
         (T109 (MEANS SUB-GOAL)) (T182 (MEANS SUB-GOAL))
         (T112 (MEANS SUB-GOAL)) (T183 (MEANS SUB-GOAL))
         (T107 (MEANS SUB-GOAL)) (T181 (MEANS SUB-GOAL))))))
     (CAUSE (AGENT (OR (AND (T103))))
      (EFFECT (OR (AND (T117 (MEANS SUB-GOAL)) (T118 (MEANS SUB-GOAL))))))
     (CAUSE (AGENT (OR (AND (T109))))
      (EFFECT (OR (AND (T106 (MEANS SUB-GOAL))))))
     (CAUSE (AGENT (OR (AND (T111))))
      (EFFECT
       (OR (AND (T184 (MEANS SUB-GOAL))) (AND (T100 (MEANS SUB-GOAL))))))
     (CAUSE (AGENT (OR (AND (T136))))
      (EFFECT (OR (AND (T100 (MEANS PLAN-PART))))))
     (CAUSE (AGENT (OR (AND (T141))))
      (EFFECT (OR (AND (T107 (MEANS PLAN-PART)) (T111 (MEANS PLAN-PART))))))
     (CAUSE (AGENT (OR (AND (T112))))
      (EFFECT
       (OR
        (AND (T119 (MEANS PLAN-PART)) (T185 (MEANS PLAN-PART))
         (T109 (MEANS PLAN-PART)) (T182 (MEANS PLAN-PART))))))
     (CAUSE (AGENT (OR (AND (T121))))
      (EFFECT
       (OR
        (AND (T120 (MEANS PLAN-PART)) (T106 (MEANS PLAN-PART))
         (T109 (MEANS PLAN-PART))))))
     (CAUSE (AGENT (OR (AND (T129))))
      (EFFECT (OR (AND (T121 (MEANS PLAN-PART)) (T124 (MEANS CONSEQUENCE))))))
     (CAUSE (AGENT (OR (AND (T133))))
      (EFFECT
       (OR (AND (T125 (MEANS CONSEQUENCE)) (T190 (MEANS CONSEQUENCE))))))
     (CAUSE (AGENT (OR (AND (T130))))
      (EFFECT (OR (AND (T128 (MEANS CONSEQUENCE))))))
     (CAUSE (AGENT (OR (AND (T131))))
      (EFFECT
       (OR
        (AND (T129 (MEANS SUB-GOAL)) (T128 (MEANS CONSEQUENCE))
         (T130 (MEANS SUB-GOAL))))))
     (CAUSE (AGENT (OR (AND (T119))))
      (EFFECT
       (OR (AND (T194 (MEANS CONSEQUENCE)))
        (AND (T131 (MEANS SUB-GOAL)) (T130 (MEANS SUB-GOAL)))
        (AND (T132 (MEANS CONSEQUENCE)) (T195 (MEANS CONSEQUENCE))
         (T191 (MEANS CONSEQUENCE))))))
     (CAUSE (AGENT (OR (AND (T147))))
      (EFFECT
       (OR
        (AND (T133 (MEANS PLAN-PART)) (T196 (MEANS PLAN-PART))
         (T119 (MEANS PLAN-PART)) (T185 (MEANS PLAN-PART))
         (T132 (MEANS CONSEQUENCE)) (T191 (MEANS CONSEQUENCE))
         (T195 (MEANS CONSEQUENCE)) (T112 (MEANS PLAN-PART))
         (T183 (MEANS PLAN-PART))))))
     (CAUSE (AGENT (OR (AND (T117))))
      (EFFECT (OR (AND (T134 (MEANS CONSEQUENCE))))))
     (CAUSE (AGENT (OR (AND (T148))))
      (EFFECT (OR (AND (T136 (MEANS PLAN-PART)) (T139 (MEANS CONSEQUENCE))))))
     (CAUSE (AGENT (OR (AND (T118))))
      (EFFECT (OR (AND (T140 (MEANS CONSEQUENCE))))))
     (CAUSE (AGENT (OR (AND (T149))))
      (EFFECT (OR (AND (T141 (MEANS PLAN-PART)) (T142 (MEANS CONSEQUENCE))))))
     (CAUSE (AGENT (OR (AND (T150))))
      (EFFECT
       (OR (AND (T100 (MEANS PLAN-PART)))
        (AND (T143 (MEANS CONSEQUENCE)) (T189 (MEANS CONSEQUENCE)))))))
    :REGRESSIVE
    ((CAUSE (AGENT (OR (AND (T212 (MEANS DOMAIN)))))
      (EFFECT (OR (AND (T199)))))
     (CAUSE (AGENT (OR (AND (T185 (MEANS CONSEQUENCE)))))
      (EFFECT (OR (AND (T194)))))
     (CAUSE (AGENT (OR (AND (T169 (MEANS AGENCY)))))
      (EFFECT (OR (AND (T170)))))
     (CAUSE (AGENT (OR (AND (T207 (MEANS AGENCY))) (AND (T84 (MEANS AGENCY)))))
      (EFFECT (OR (AND (T85)))))
     (CAUSE (AGENT (OR (AND (T84 (MEANS AGENCY))))) (EFFECT (OR (AND (T88)))))
     (CAUSE (AGENT (OR (AND (T144 (MEANS AGENCY))))) (EFFECT (OR (AND (T92)))))
     (CAUSE
      (AGENT (OR (AND (T207 (MEANS AGENCY))) (AND (T145 (MEANS AGENCY)))))
      (EFFECT (OR (AND (T95)))))
     (CAUSE
      (AGENT (OR (AND (T208 (MEANS CONSEQUENCE))) (AND (T84 (MEANS AGENCY)))))
      (EFFECT (OR (AND (T98)))))
     (CAUSE
      (AGENT
       (OR (AND (T108 (MEANS SUB-GOAL)))
        (AND (T146 (MEANS SUB-GOAL)) (T199 (MEANS SUB-GOAL))
         (T107 (MEANS SUB-GOAL)) (T181 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T99)))))
     (CAUSE
      (AGENT
       (OR
        (AND (T107 (MEANS SUB-GOAL)) (T150 (MEANS PLAN-PART))
         (T136 (MEANS PLAN-PART)) (T111 (MEANS SUB-GOAL))
         (T146 (MEANS SUB-GOAL)) (T108 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T100)))))
     (CAUSE (AGENT (OR (AND (T99 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T103)))))
     (CAUSE
      (AGENT
       (OR
        (AND (T120 (MEANS SUB-GOAL)) (T121 (MEANS PLAN-PART))
         (T109 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T106)))))
     (CAUSE (AGENT (OR (AND (T146 (MEANS SUB-GOAL)) (T141 (MEANS PLAN-PART)))))
      (EFFECT (OR (AND (T107)))))
     (CAUSE (AGENT (OR (AND (T146 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T108)))))
     (CAUSE
      (AGENT
       (OR
        (AND (T146 (MEANS SUB-GOAL)) (T121 (MEANS PLAN-PART))
         (T112 (MEANS PLAN-PART)))))
      (EFFECT (OR (AND (T109)))))
     (CAUSE
      (AGENT
       (OR (AND (T141 (MEANS PLAN-PART)))
        (AND (T146 (MEANS SUB-GOAL)) (T199 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T111)))))
     (CAUSE
      (AGENT
       (OR
        (AND (T147 (MEANS PLAN-PART)) (T210 (MEANS PLAN-PART))
         (T146 (MEANS SUB-GOAL)) (T199 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T112)))))
     (CAUSE (AGENT (OR (AND (T146 (MEANS DOMAIN)))))
      (EFFECT (OR (AND (T116)))))
     (CAUSE (AGENT (OR (AND (T103 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T117)))))
     (CAUSE (AGENT (OR (AND (T103 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T118)))))
     (CAUSE
      (AGENT
       (OR (AND (T218 (MEANS PLAN-PART)))
        (AND (T147 (MEANS PLAN-PART)) (T210 (MEANS PLAN-PART))
         (T112 (MEANS PLAN-PART)) (T183 (MEANS PLAN-PART)))))
      (EFFECT (OR (AND (T119)))))
     (CAUSE
      (AGENT
       (OR
        (AND (T199 (MEANS SUB-GOAL)) (T218 (MEANS PLAN-PART))
         (T208 (MEANS PLAN-PART)) (T183 (MEANS PLAN-PART)))
        (AND (T121 (MEANS PLAN-PART)))))
      (EFFECT (OR (AND (T120)))))
     (CAUSE (AGENT (OR (AND (T129 (MEANS PLAN-PART)))))
      (EFFECT (OR (AND (T121)))))
     (CAUSE (AGENT (OR (AND (T129 (MEANS CONSEQUENCE)))))
      (EFFECT (OR (AND (T124)))))
     (CAUSE
      (AGENT
       (OR (AND (T209 (MEANS CONSEQUENCE)) (T190 (MEANS SIMILARITY)))
        (AND (T133 (MEANS CONSEQUENCE)))))
      (EFFECT (OR (AND (T125)))))
     (CAUSE
      (AGENT (OR (AND (T130 (MEANS CONSEQUENCE)) (T131 (MEANS CONSEQUENCE)))))
      (EFFECT (OR (AND (T128)))))
     (CAUSE
      (AGENT (OR (AND (T214 (MEANS PLAN-PART))) (AND (T131 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T129)))))
     (CAUSE (AGENT (OR (AND (T131 (MEANS SUB-GOAL)) (T119 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T130)))))
     (CAUSE (AGENT (OR (AND (T119 (MEANS SUB-GOAL)))))
      (EFFECT (OR (AND (T131)))))
     (CAUSE
      (AGENT
       (OR (AND (T207 (MEANS AGENCY)))
        (AND (T147 (MEANS CONSEQUENCE)) (T119 (MEANS CONSEQUENCE)))))
      (EFFECT (OR (AND (T132)))))
     (CAUSE
      (AGENT (OR (AND (T147 (MEANS PLAN-PART)) (T210 (MEANS PLAN-PART)))))
      (EFFECT (OR (AND (T133)))))
     (CAUSE
      (AGENT
       (OR
        (AND (T185 (MEANS CONSEQUENCE)) (T198 (MEANS SIMILARITY))
         (T210 (MEANS CONSEQUENCE)))
        (AND (T117 (MEANS CONSEQUENCE)))))
      (EFFECT (OR (AND (T134)))))
     (CAUSE (AGENT (OR (AND (T148 (MEANS PLAN-PART)))))
      (EFFECT (OR (AND (T136)))))
     (CAUSE
      (AGENT
       (OR
        (AND (T189 (MEANS SIMILARITY)) (T198 (MEANS SIMILARITY))
         (T190 (MEANS SIMILARITY)))
        (AND (T148 (MEANS CONSEQUENCE)))))
      (EFFECT (OR (AND (T139)))))
     (CAUSE
      (AGENT
       (OR
        (AND (T185 (MEANS CONSEQUENCE)) (T198 (MEANS SIMILARITY))
         (T210 (MEANS CONSEQUENCE)))
        (AND (T118 (MEANS CONSEQUENCE)))))
      (EFFECT (OR (AND (T140)))))
     (CAUSE (AGENT (OR (AND (T149 (MEANS PLAN-PART)))))
      (EFFECT (OR (AND (T141)))))
     (CAUSE
      (AGENT
       (OR (AND (T184 (MEANS CONSEQUENCE))) (AND (T149 (MEANS CONSEQUENCE)))))
      (EFFECT (OR (AND (T142)))))
     (CAUSE
      (AGENT
       (OR (AND (T196 (MEANS CONSEQUENCE))) (AND (T150 (MEANS CONSEQUENCE)))))
      (EFFECT (OR (AND (T143))))))))