This folder contains all data collected in the survey with the exception of (i)
free-response questions and (ii) the offenders' information other than their race, outcome
(re-arrest or not), and judicial decision (incarcerated or not). 

The corresponding data files are:

* `Demographics.csv`: demographic information of the participants. Each row
  corresponds to an individual participant.
* `Perception.csv`: perception questions. Each row corresponds to a set of
  responses provided by an individual participant. The columns names
  contain the information about which group (3 in total) of perception questions
  the answers correspond to. We have excluded the answers given to free-response
  questions.
* `Predictions.csv`: predictions made by the participants and by the RAI.
  `order_question` indicates the order of the offender's profile among those
  that were shown to the participants (40 in total). `survey_part` indicates the
  part of the survey (either 1,2, or 3). `setting` indicates the setting (anchoring vs
  non-anchoring). `condition` indicates the condition (none, controlled, or
  decreasing). The prefixes `qp` and `qb` indicate likelihood estimates and binary
  predictions (1=predicted re-arrest) respectively. The suffixes `h`, `hr`, `r`
  indicate predictions made by participants alone, by participants assisted by
  the RAI, and by the RAI alone respectively. `time_h` and `time_hr` contain the
  time spent on the offender's assessment for predictions made by participants
  without and with the assistance of the RAI respectively. `outcome` indicates
  whether the offender was rearrested (=1) or not (=0). Each row corresponds to
  either one or two pairs (in non-anchoring, second part of the survey) of
  predictions. 
* `Time_and_payment.csv`: time spent on MTurk, on the survey, and payment made
  to the participant.
* `Offenders_race.csv`: whether the offender has been rearrested and/or
  incarcerated.
* `Other`: the folder contains rates of incarceration and RAI's predictions of re-arrest on the test set. 

The files can be joined at the participant's and offender's levels using the
`id` and `index` columns respectively. 