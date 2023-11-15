#### SET UP ####
# Required packages
library("Hmisc")
library("tidyverse")

# select file ending in "questionnaire-39v2"
consent <- read.csv(file.choose())

# select file ending in "questionnaire-833g"
gender <- read.csv(file.choose())

# select file ending in "questionnaire-slvu"
spa.info <- read.csv(file.choose())

# select file ending in "questionnaire-wi8c"
language <- read.csv(file.choose())

# select file ending in "questionnaire-xyeo"
eng.info <- read.csv(file.choose())

# select file ending in "task-5jp5"
practice <- read.csv(file.choose())

# select file ending in "task-j7kq"
experiment <- read.csv(file.choose())

#### CLEANING ####

#### Consent file ####

# clean up the consent node
consent %>%
  filter(Event.Index != "END OF FILE",
         Question.Key %nin% c("BEGIN QUESTIONNAIRE", "END QUESTIONNAIRE")) %>%
  mutate(Consent.Form = case_when(Response == 1 ~ "yes",
                                  TRUE ~ "no")) %>%
  select(Participant.Private.ID,
         Participant.Device.Type,
         Participant.Device,
         Participant.OS,
         Participant.Browser,
         Consent.Form) -> consent.clean

#### Gender file ####

# clean up the gender node
gender %>%
  filter(Event.Index != "END OF FILE",
         Question.Key %nin% c("BEGIN QUESTIONNAIRE", "END QUESTIONNAIRE")) %>%
  select(Participant.Private.ID,
         Question.Key,
         Response) %>%
  pivot_wider(names_from = "Question.Key", values_from = "Response") %>%
  mutate(Gender.Category.Coarse = case_when(GenderText=="female" ~ "female",
                                     GenderText=="Female" ~ "female",
                                     GenderText=="female " ~ "female",
                                     GenderText=="cis male" ~ "male",
                                     GenderText=="male" ~ "male",
                                     GenderText=="Male" ~ "male",
                                     GenderText=="Female (cis)" ~ "female",
                                     GenderText=="Woman" ~ "female",
                                     GenderText=="woman" ~ "female",
                                     GenderText=="Cis woman" ~ "female",
                                     GenderText=="cis woman" ~ "female",
                                     GenderText=="(cis)female" ~ "female",
                                     GenderText=="cis female" ~ "female",
                                     GenderText=="f" ~ "female",
                                     GenderText=="she" ~ "female",
                                     GenderText=="femile" ~ "female",
                                     TRUE ~ "other")) %>%
  mutate(Gender.Category.Fine = case_when(GenderText=="female" ~ "female",
                                          GenderText=="Female" ~ "female",
                                          GenderText=="female " ~ "female",
                                          GenderText=="cis male" ~ "male",
                                          GenderText=="male" ~ "male",
                                          GenderText=="Male" ~ "male",
                                          GenderText=="Female (cis)" ~ "female",
                                          GenderText=="Woman" ~ "woman",
                                          GenderText=="woman" ~ "woman",
                                          GenderText=="Cis woman" ~ "woman",
                                          GenderText=="cis woman" ~ "woman",
                                          GenderText=="(cis)female" ~ "female",
                                          GenderText=="cis female" ~ "female",
                                          GenderText=="f" ~ "female",
                                          GenderText=="she" ~ "woman",
                                          GenderText=="femile" ~ "female",
                                          GenderText=="I don't know" ~ "questioning",
                                          GenderText=="cis female / questioning nonbinary" ~ "questioning",
                                          GenderText=="feminine" ~ "femme",
                                          GenderText=="femenine" ~ "femme",
                                          GenderText=="Agender" ~ "agender",
                                          GenderText=="agender" ~ "agender",
                                          TRUE ~ "nonbinary"),
         feminine = feminine %>% as.numeric(),
         masculine = masculine %>% as.numeric(),
         agender = agender %>% as.numeric()) -> gender.clean

#### Language file ####

# clean up the first language file
language %>%
  filter(Event.Index != "END OF FILE",
         Question.Key %nin% c("BEGIN QUESTIONNAIRE", "END QUESTIONNAIRE", "L1-quantised")) %>%
  mutate(L1 = Response) %>%
  select(Participant.Private.ID,
         L1) -> language.temp

#### SPA/ENG branch files ####

# clean up the English language file
eng.info %>%
  filter(Event.Index != "END OF FILE",
         Question.Key %nin% c("BEGIN QUESTIONNAIRE", "END QUESTIONNAIRE", "LocL1Eng-quantised")) %>%
  select(Participant.Private.ID,
         Question.Key,
         Response) %>%
  pivot_wider(names_from = "Question.Key", values_from = "Response") -> eng.clean

# clean up the Spanish language file
spa.info %>%
  filter(Event.Index != "END OF FILE",
         Question.Key %nin% c("BEGIN QUESTIONNAIRE", "END QUESTIONNAIRE", "LocL2Eng-quantised","TimeuseEng-quantised")) %>%
  select(Participant.Private.ID,
         Question.Key,
         Response) %>%
  pivot_wider(names_from = "Question.Key", values_from = "Response") -> spa.clean

# merge into acquisition info

spa.clean %>%
  rename(LocEng = LocL2Eng,
         DateEngAcq = `DateL2Eng-inmonths`) %>%
  full_join(eng.clean %>% rename(LocEng = LocL1Eng, DateEngAcq = `DateL1Eng-inmonths`),
            by = c("Participant.Private.ID", "DateEngAcq", "LocEng")) %>%
  mutate(TimeuseEng = case_when(is.na(TimeuseEng) ~ "~100%",
                                TRUE ~ TimeuseEng)) %>%
  full_join(language.temp) -> language.clean

#### Practice file ####

# clean up the trial node: practice questions
practice %>% #pull(Response) %>% unique()
  filter(Event.Index != "END OF FILE",
         Trial.Number %nin% c("BEGIN TASK", "END TASK"),
         display == "trial") %>%
  select(Participant.Private.ID,
         Spreadsheet.Row,
         display,
         Trial.Number,
         Zone.Name,
         Response) %>%
  filter(Zone.Name != "Zone1") %>%
  mutate(Response.Accuracy = case_when(Spreadsheet.Row == 1 &
                                         Zone.Name == "buttonA" &
                                         Response == "Someone" ~ 1,
                                       Spreadsheet.Row == 1 &
                                         Zone.Name == "buttonB" &
                                         Response == "blew bubbles." ~ 1,
                                       Spreadsheet.Row == 3 &
                                         Zone.Name == "buttonA" &
                                         Response == "They" ~ 1,
                                       Spreadsheet.Row == 3 &
                                         Zone.Name == "buttonB" &
                                         Response == "did their hair." ~ 1,
                                       TRUE ~ 0)) %>%
  group_by(Participant.Private.ID) %>% summarise(Practice.Accuracy = sum(Response.Accuracy)) -> practice.clean


#### Target file ####

# clean up the trial node: target questions
experiment %>% #pull(Response) %>% unique()
  filter(Event.Index != "END OF FILE",
         Trial.Number %nin% c("BEGIN TASK", "END TASK"),
         display == "trial") %>%
  select(Participant.Private.ID,
         Spreadsheet.Row,
         Trial.Number,
         Zone.Name,
         Response,
         Image) %>%
  filter(Zone.Name != "Zone1") %>%
  mutate(Response.Type = case_when(Zone.Name == "buttonB" ~ "Subject",
                                   TRUE ~ "Predicate")) %>%
  select(-Zone.Name) %>%
  pivot_wider(names_from = "Response.Type", values_from = "Response") %>%
  select(Participant.Private.ID, Spreadsheet.Row, Trial.Number, Image, Subject, Predicate) -> experiment.clean


#### Combine files ####

consent.clean %>%
  full_join(experiment.clean, by = "Participant.Private.ID") %>%
  full_join(practice.clean, by = "Participant.Private.ID") %>%
  full_join(language.clean, by = "Participant.Private.ID") %>%
  full_join(gender.clean, by = "Participant.Private.ID") -> data.complete


#### Write CLEAN for Cara ####

# write to data file for CARA
write_csv(data.complete %>% select(-c(Gender.Category.Coarse, Gender.Category.Fine)), file = paste0(Sys.Date(),"_data.csv"))

# write to data file for LAUREN
write_csv(data.complete, file = paste0(Sys.Date(),"_data-lma.csv"))
