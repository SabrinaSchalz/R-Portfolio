library(tidyverse)
library(corrr)

daten1<-read_csv("calls.csv")

daten2<-read_csv("parts.csv")

#join datasets
daten<- daten1 %>% 
  left_join(daten2, by = c('call_id' = 'call_id')) 

#there's loads of NA in the daten2 section where a call did not lead to an outcome

#participations achieved
participations<- daten %>%
filter(status %in% c("COMPLETE", "SCREEN", "OVER"))

#need to assign ID for status, target spec, disposition
participations <- participations %>%     
  group_by(status) %>%
  dplyr::mutate(status_ID = cur_group_id())

participations <- participations %>%     
  group_by(target_spec) %>%
  dplyr::mutate(target_ID = cur_group_id())

participations <- participations %>%     
  group_by(disposition) %>%
  dplyr::mutate(disposition_ID = cur_group_id())


cor <- participations %>%
	select(user_id, assigned_staff_id, disposition_ID, target_ID, status_ID, time_spent_min, total_call_time_sec, tasks_succeeded, tasks_failed, survey_id, language_id) %>%
	correlate()
cor

#####calls that lead to nothing
nothing <- daten %>%
filter(is.na(status))

nothing <- nothing %>%     
  group_by(target_spec) %>%
  dplyr::mutate(target_ID = cur_group_id())

nothing <- nothing %>%  
  group_by(disposition) %>%
  dplyr::mutate(disposition_ID = cur_group_id())


cor <- nothing %>%
	select(user_id, assigned_staff_id, disposition_ID, target_ID, time_spent_min, total_call_time_sec, tasks_succeeded, tasks_failed, survey_id, language_id) %>%
	correlate()
cor

#new column for action/inaction in data

daten <- daten %>%
  mutate(Participation = case_when(
	status=="COMPLETE" ~ "YES", 
	status=="OVER" ~ "YES",
	status=="SCREEN" ~ "YES",
	status=="PART" ~ "YES",
    is.na(status) ~ "NO",
    ))

daten <- daten %>%     
  group_by(disposition) %>%
  dplyr::mutate(disposition_ID = cur_group_id())

daten <- daten %>%     
  group_by(Participation) %>%
  dplyr::mutate(participation_ID = cur_group_id())

daten <- daten %>%     
  group_by(resolution) %>%
  dplyr::mutate(resolution_ID = cur_group_id())


cor <- daten %>%
	select(user_id, assigned_staff_id, resolution_ID, disposition_ID, time_spent_min, total_call_time_sec, tasks_succeeded, tasks_failed, participation_ID) %>%
	correlate()
cor


#successful calls only

success <- daten %>%
filter(resolution == "DONE")

cor <- success %>%
	select(user_id, assigned_staff_id, disposition_ID, time_spent_min, total_call_time_sec, tasks_succeeded, tasks_failed, participation_ID) %>%
	correlate()
cor


#spoken to only
spoken <- daten %>%
filter(disposition == "PERSON_LISTED")

cor <- spoken %>%
	select(user_id, assigned_staff_id, time_spent_min, total_call_time_sec, tasks_succeeded, tasks_failed, participation_ID) %>%
	correlate()
cor

#secretary only
sec <- daten %>%
filter(disposition == "SECRETARY")

cor <- sec %>%
	select(user_id, assigned_staff_id, time_spent_min, total_call_time_sec, tasks_succeeded, tasks_failed, participation_ID) %>%
	correlate()
cor


#have each disposition be a unique table

disp <- daten %>%
  mutate(NO_ANSWER = case_when(
	disposition=="NO_ANSWER" ~ 1, 
	disposition!="NO_ANSWER" ~ 0, 
    ))

disp <- disp %>%
  mutate(SPOKEN = case_when(
	disposition=="PERSON_LISTED" ~ 1, 
	disposition!="PERSON_LISTED" ~ 0, 
    ))

disp <- disp %>%
  mutate(VOICE = case_when(
	disposition=="VOICEMAIL" ~ 1, 
	disposition!="VOICEMAIL" ~ 0, 
    ))

disp <- disp %>%
  mutate(SEC = case_when(
	disposition=="SECRETARY" ~ 1, 
	disposition!="SECRETARY" ~ 0, 
    ))

disp <- disp %>%
  mutate(ACCEPT = case_when(
	tasks_succeeded != 0 ~ 1, 
	tasks_succeeded == 0 ~ 0, 
    ))


disp <- disp %>%
  mutate(REFUSE = case_when(
	tasks_succeeded == 0 ~ 1, 
	tasks_failed == 0 ~ 0, 
    ))


cor <- disp %>%
	select(user_id, assigned_staff_id, SPOKEN, VOICE, SEC, NO_ANSWER, ACCEPT, REFUSE, time_spent_min, total_call_time_sec, participation_ID) %>%
	correlate()
cor


#add in day and time
time<-read_csv("calls_time.csv")

#join datasets
disp<- disp %>% 
  left_join(time, by = c('call_id' = 'call_id')) 

disp <- disp %>%     
  group_by(Day) %>%
  dplyr::mutate(Day_ID = cur_group_id())

cor <- disp %>%
	select(user_id, assigned_staff_id, Day_ID, Hour, SPOKEN, VOICE, SEC, NO_ANSWER, ACCEPT, REFUSE, time_spent_min, total_call_time_sec, participation_ID) %>%
	correlate()
cor


write.csv(disp,"call_data.csv")

###break here####

daten<-read_csv("call_data.csv")

daten2<-read_csv("ids_v.csv")

#join datasets
daten<- daten %>% 
  left_join(daten2, by = c('user_id' = 'USER ID')) 


daten <- daten %>%
  mutate(ACTIVE = case_when(
	ISO == "Y" ~ 2, 
	ISO == "N" ~ 0, 
	ISO == "I" ~ 1, 
    ))

daten <- daten %>%     
  group_by(COUNTRY) %>%
  dplyr::mutate(COUNTRY_ID = cur_group_id())

daten <- daten %>%     
  group_by(PROFESSION) %>%
  dplyr::mutate(PROFESSION_ID = cur_group_id())

daten <- daten %>%     
  group_by(SPEC) %>%
  dplyr::mutate(SPEC_ID = cur_group_id())

cor <- daten %>%
	select(user_id, INVITES, ACTIVE, COUNTRY_ID, PROFESSION_ID, SPEC_ID, ACTIONS, COMPLETES, SCREENS, STARTS, SPOKEN, VOICE, SEC, NO_ANSWER, ACCEPT, REFUSE, participation_ID) %>%
	correlate()
cor


write.csv(daten,"full_data.csv")