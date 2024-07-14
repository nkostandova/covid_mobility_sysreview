# mobility metrics
mobility_metrics = survey_file
#names(mobility_metrics) = survey_file[1,]


########################## mobility metrics ###############################################
other_choices_file = read.csv("other_choices.csv")

#### for first dataset
mm1 = mobility_metrics %>% select("What.is.the.paper.ID.number..from.the.spreadsheet..", "Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..", "What.mobility.metrics.are.used.from.the.data..Check.all.that.apply.":"X.63" )
names(mm1) = mm1[1,]
mm1 = mm1[-1,]
mm1 = mm1 %>% filter(`Response`=="Yes") 

mm1<-replace(mm1, mm1=="",NA) 
mm1[, 2:(ncol(mm1)-1)]<-replace(data.frame(lapply(mm1[, 2:(ncol(mm1)-1)], as.character), stringsAsFactors = FALSE), !is.na(mm1[, 2:(ncol(mm1)-1)]), "1")

mm1 = mm1 %>% dplyr::rename("Other" = "Other (please specify. If more than one, separate with a \";\".)")
mm1 = merge(mm1, other_choices_file[, c("other_choices", "classification")], by.x = "Other", by.y = "other_choices", all.x = TRUE)

mm1[, c(3:8)] <- sapply(mm1[, c(3:8)], as.numeric)

# check the following papers:
check = (mm1 %>% filter(classification == "need to check" | classification == "not sure what this is " | classification == "new coding - travel time -- but likely this paper should not be included" | classification == " new coding - number of locations visited"))
#View(mobility_metrics %>% filter(What.is.the.paper.ID.number..from.the.spreadsheet.. %in% check$`Open-Ended Response`))

# recode because of weird spacing
mm1 = mm1 %>% mutate(classification = ifelse(classification == "new coding - other ", "new coding - other", classification))
mm1 = mm1 %>% mutate(classification = ifelse(classification == "origin-destination ", "origin-destination", classification))
mm1 = mm1 %>% mutate(classification = ifelse(classification == "origin-destination", "trip counts between an origin and destination", classification))
mm1 = mm1 %>% mutate(classification = ifelse(classification == "new coding - time spent at work - or we can make this a general commuting category", "new coding - time spent at work", classification))


# recode after checking in papers
mm1 = mm1 %>% mutate(classification = ifelse(`Open-Ended Response`=="340507", "direction requests", classification))
mm1 = mm1 %>% mutate(classification = ifelse(`Open-Ended Response`=="704920", "traffic flows", classification))
mm1 = mm1 %>% mutate(classification = ifelse(`Open-Ended Response`=="486227", "new coding - other", classification))
mm1 = mm1 %>% mutate(classification = ifelse(`Open-Ended Response`=="999927", NA, classification))

# check papers with blanks:  ---- need to come back to this
check = (mm1 %>% filter(is.na(classification) == TRUE))
#View(mobility_metrics %>% filter(What.is.the.paper.ID.number..from.the.spreadsheet.. %in% check$`Open-Ended Response`))

# recode: if classification is an option that already exists
mm1 = mm1 %>% mutate(`Trip counts between an origin and destination`=replace(`Trip counts between an origin and destination`,classification == "trip counts between an origin and destination", 1 ),
                     `Clustering of individuals at a location` = replace(`Clustering of individuals at a location`, classification == "clustering of individuals at a location", 1),
                     `Time spent at home/residence` = replace(`Time spent at home/residence`,classification == "time spent at home / residence", 1),
                     `Time spent at particular locations (like businesses)` = replace(`Time spent at particular locations (like businesses)`, classification == "time spent at particular locations (like businesses)", 1),
                     `Traffic flows` = replace(`Traffic flows`, classification == "traffic flows", 1))

# recode: if classification in "other" is already marked off, remove from other
mm1 = mm1 %>% mutate(classification=ifelse(classification == "clustering of individuals at a location" & is.na(`Clustering of individuals at a location`)==FALSE, NA, classification))
mm1 = mm1 %>% mutate(classification=ifelse(classification == "time spent at home / residence" & is.na(`Time spent at home/residence`)==FALSE, NA, classification))
mm1 = mm1 %>% mutate(classification=ifelse(classification == "time spent at particular locations (like businesses)" & is.na(`Time spent at particular locations (like businesses)`)==FALSE, NA, classification))
mm1 = mm1 %>% mutate(classification=ifelse(classification == "traffic flows" & is.na(`Traffic flows`)==FALSE, NA, classification))
mm1 = mm1 %>% mutate(classification=ifelse(classification == "trip counts between an origin and destination" & is.na(`Trip counts between an origin and destination`)==FALSE, NA, classification))


# make new variables from the "classification" options
mm1 =mm1 %>% mutate(`direction requests`= ifelse(classification == "direction requests", 1, NA),
                    `distance traveled` = ifelse(classification == "new coding - distance traveled", 1, NA),
                    `migration` = ifelse(classification == "new coding - migration", 1, NA),
                    `number of locations visited` = ifelse(classification == "new coding - number of locations visited", 1, NA),
                    `other`=ifelse(classification == "new coding - other", 1, NA),
                    `time spent at work` = ifelse(classification == "new coding - time spent at work", 1, NA),
                    `other roaming/international trips/tourism statistics` = ifelse(classification == "new coding - other roaming/international trips/tourism statistics", 1, NA),
                    `population counts` = ifelse(classification == "new coding - population counts", 1, NA),
                    `radius of gyration` = ifelse(classification == "new coding - radius of gyration ", 1, NA))

## recode for 1 paper with multiple things in other
mm1 = mm1 %>% mutate(`distance traveled` = ifelse(`Open-Ended Response`=="104918", 1, `distance traveled`),
                     `other roaming/international trips/tourism statistics` =ifelse(`Open-Ended Response`=="104918", 1, `other roaming/international trips/tourism statistics`),
                     `Time spent at particular locations (like businesses)` = ifelse(`Open-Ended Response`=="104918", 1, `Time spent at particular locations (like businesses)`),
                     classification = ifelse(`Open-Ended Response`=="104918", NA, classification))


# # make long dataset
# # put in order that's appropriate
# mm1_long = mm1 %>% dplyr::select(`Open-Ended Response`, `Time spent at home/residence`:`Traffic flows`, `direction requests`:`radius of gyration`)
# mm1_long <- gather(mm1_long, answer_option, response, "Time spent at home/residence":"radius of gyration", factor_key=TRUE)
# mm1_long$response = as.numeric(mm1_long$response)
# mm1_sum = mm1_long %>% group_by(answer_option) %>% summarize(count = sum(response, na.rm=TRUE))
# mm1_sum %>% arrange(desc(count))


#### for second dataset
mm2 = mobility_metrics %>% select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.mobility.metrics.are.used.from.the.data..Check.all.that.apply..2":"X.133" )
names(mm2) = mm2[1,]
mm2 = mm2[-1,]
mm2<-replace(mm2, mm2=="",NA) 
mm2[, 2:(ncol(mm2)-1)]<-replace(data.frame(lapply(mm2[, 2:(ncol(mm2)-1)], as.character), stringsAsFactors = FALSE), !is.na(mm2[, 2:(ncol(mm2)-1)]), "1")

mm2 = mm2 %>% dplyr::rename("Other" = `Other (please specify. If more than one, separate with a \";\".)`)
mm2 = merge(mm2, other_choices_file[, c("other_choices", "classification")], by.x = "Other", by.y = "other_choices", all.x = TRUE)

mm2[, c(3:8)] <- sapply(mm2[, c(3:8)], as.numeric)


# check the following papers:
check = (mm2 %>% filter(classification == "" | classification == "this will either be origin/destinations or population counts"))
#View(mobility_metrics %>% filter(What.is.the.paper.ID.number..from.the.spreadsheet.. %in% check$`Open-Ended Response`))

# recode 
mm2 = mm2 %>% mutate(classification = ifelse(`Open-Ended Response`=="938671", "population counts", classification)) # already counted as traffic flow in the first dataset
mm2 = mm2 %>% mutate(classification = ifelse(`Open-Ended Response`=="270959", "time spent at particular locations (like businesses)", classification)) # this is Google Community Mobility data

# recode: if classification is an option that already exists
mm2 = mm2 %>% mutate(`Trip counts between an origin and destination`=replace(`Trip counts between an origin and destination`,classification == "trip counts between an origin and destination", 1 ),
                     `Clustering of individuals at a location` = replace(`Clustering of individuals at a location`, classification == "clustering of individuals at a location", 1),
                     `Time spent at home/residence` = replace(`Time spent at home/residence`,classification == "time spent at home / residence", 1),
                     `Time spent at particular locations (like businesses)` = replace(`Time spent at particular locations (like businesses)`, classification == "time spent at particular locations (like businesses)", 1),
                     `Traffic flows` = replace(`Traffic flows`, classification == "traffic flows", 1))


# recode: if classification in "other" is already marked off, remove from other
mm2 = mm2 %>% mutate(classification=ifelse(classification == "clustering of individuals at a location" & is.na(`Clustering of individuals at a location`)==FALSE, NA, classification))
mm2 = mm2 %>% mutate(classification=ifelse(classification == "time spent at particular locations (like businesses)" & is.na(`Time spent at particular locations (like businesses)`)==FALSE, NA, classification))


# make new variables from the "classification" options
mm2 =mm2 %>% mutate(`direction requests`= ifelse(classification == "direction requests", 1, NA),
                    `distance traveled` = ifelse(classification == "new coding - distance traveled", 1, NA),
                    `migration` = ifelse(classification == "new coding - migration", 1, NA),
                    `number of locations visited` = ifelse(classification == "new coding - number of locations visited", 1, NA),
                    `other`=ifelse(classification == "new coding - other", 1, NA),
                    `time spent at work` = ifelse(classification == "new coding - time spent at work", 1, NA),
                    `other roaming/international trips/tourism statistics` = ifelse(classification == "new coding - other roaming/international trips/tourism statistics", 1, NA),
                    `population counts` = ifelse(classification == "new coding - population counts", 1, NA),
                    `radius of gyration` = ifelse(classification == "new coding - radius of gyration ", 1, NA))

#### for third dataset
mm3 = mobility_metrics %>% select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.mobility.metrics.are.used.from.the.data..Check.all.that.apply..3":"X.168" )
names(mm3) = mm3[1,]
mm3 = mm3[-1,]
mm3<-replace(mm3, mm3=="",NA) 
mm3[, 2:(ncol(mm3)-1)]<-replace(data.frame(lapply(mm3[, 2:(ncol(mm3)-1)], as.character), stringsAsFactors = FALSE), !is.na(mm3[, 2:(ncol(mm3)-1)]), "1")

mm3 = mm3 %>% dplyr::rename("Other" = `Other (please specify. If more than one, separate with a \";\".)`)
mm3 = merge(mm3, other_choices_file[, c("other_choices", "classification")], by.x = "Other", by.y = "other_choices", all.x = TRUE) ### none

mm3[, c(3:8)] <- sapply(mm3[, c(3:8)], as.numeric)


# make new variables from the "classification" options
mm3 =mm3 %>% mutate(`direction requests`= ifelse(classification == "direction requests", 1, NA),
                    `distance traveled` = ifelse(classification == "new coding - distance traveled", 1, NA),
                    `migration` = ifelse(classification == "new coding - migration", 1, NA),
                    `number of locations visited` = ifelse(classification == "new coding - number of locations visited", 1, NA),
                    `other`=ifelse(classification == "new coding - other", 1, NA),
                    `time spent at work` = ifelse(classification == "new coding - time spent at work", 1, NA),
                    `other roaming/international trips/tourism statistics` = ifelse(classification == "new coding - other roaming/international trips/tourism statistics", 1, NA),
                    `population counts` = ifelse(classification == "new coding - population counts", 1, NA),
                    `radius of gyration` = ifelse(classification == "new coding - radius of gyration ", 1, NA))


# mm3_long = mm3 %>% dplyr::select(`Open-Ended Response`, `Time spent at home/residence`:`Traffic flows`, `direction requests`:`radius of gyration`)
# mm3_long <- gather(mm3_long, answer_option, response, "Time spent at home/residence":"radius of gyration", factor_key=TRUE)
# mm3_long$response = as.numeric(mm3_long$response)
# mm3_sum = mm3_long %>% group_by(answer_option) %>% summarize(count = sum(response, na.rm=TRUE))
# mm3_sum %>% arrange(desc(count))

#### for fourth dataset
mm4 = mobility_metrics %>% select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.mobility.metrics.are.used.from.the.data..Check.all.that.apply..4":"X.203" )
names(mm4) = mm4[1,]
mm4 = mm4[-1,]
mm4<-replace(mm4, mm4=="",NA) 
mm4[, 2:(ncol(mm4)-1)]<-replace(data.frame(lapply(mm4[, 2:(ncol(mm4)-1)], as.character), stringsAsFactors = FALSE), !is.na(mm4[, 2:(ncol(mm4)-1)]), "1")

mm4 = mm4 %>% dplyr::rename("Other" = `Other (please specify. If more than one, separate with a \";\".)`)
mm4 = merge(mm4, other_choices_file[, c("other_choices", "classification")], by.x = "Other", by.y = "other_choices", all.x = TRUE) ### none

mm4[, c(3:8)] <- sapply(mm4[, c(3:8)], as.numeric)

# make new variables from the "classification" options
mm4 =mm4 %>% mutate(`direction requests`= ifelse(classification == "direction requests", 1, NA),
                    `distance traveled` = ifelse(classification == "new coding - distance traveled", 1, NA),
                    `migration` = ifelse(classification == "new coding - migration", 1, NA),
                    `number of locations visited` = ifelse(classification == "new coding - number of locations visited", 1, NA),
                    `other`=ifelse(classification == "new coding - other", 1, NA),
                    `time spent at work` = ifelse(classification == "new coding - time spent at work", 1, NA),
                    `other roaming/international trips/tourism statistics` = ifelse(classification == "new coding - other roaming/international trips/tourism statistics", 1, NA),
                    `population counts` = ifelse(classification == "new coding - population counts", 1, NA),
                    `radius of gyration` = ifelse(classification == "new coding - radius of gyration ", 1, NA))

#### for fifth dataset
mm5 = mobility_metrics %>% select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.mobility.metrics.are.used.from.the.data..Check.all.that.apply..5":"X.238" )
names(mm5) = mm5[1,]
mm5 = mm5[-1,]
mm5<-replace(mm5, mm5=="",NA) 
mm5[, 2:(ncol(mm5)-1)]<-replace(data.frame(lapply(mm5[, 2:(ncol(mm5)-1)], as.character), stringsAsFactors = FALSE), !is.na(mm5[, 2:(ncol(mm5)-1)]), "1")

mm5 = mm5 %>% dplyr::rename("Other" = `Other (please specify. If more than one, separate with a \";\".)`)
mm5 = merge(mm5, other_choices_file[, c("other_choices", "classification")], by.x = "Other", by.y = "other_choices", all.x = TRUE) ### none

mm5[, c(3:8)] <- sapply(mm5[, c(3:8)], as.numeric)

# make new variables from the "classification" options
mm5 =mm5 %>% mutate(`direction requests`= ifelse(classification == "direction requests", 1, NA),
                    `distance traveled` = ifelse(classification == "new coding - distance traveled", 1, NA),
                    `migration` = ifelse(classification == "new coding - migration", 1, NA),
                    `number of locations visited` = ifelse(classification == "new coding - number of locations visited", 1, NA),
                    `other`=ifelse(classification == "new coding - other", 1, NA),
                    `time spent at work` = ifelse(classification == "new coding - time spent at work", 1, NA),
                    `other roaming/international trips/tourism statistics` = ifelse(classification == "new coding - other roaming/international trips/tourism statistics", 1, NA),
                    `population counts` = ifelse(classification == "new coding - population counts", 1, NA),
                    `radius of gyration` = ifelse(classification == "new coding - radius of gyration ", 1, NA))


#### for sixth dataset
mm6 = mobility_metrics %>% select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.mobility.metrics.are.used.from.the.data..Check.all.that.apply..6":"X.273" )
names(mm6) = mm6[1,]
mm6 = mm6[-1,]
mm6<-replace(mm6, mm6=="",NA) 
mm6[, 2:(ncol(mm6)-1)]<-replace(data.frame(lapply(mm6[, 2:(ncol(mm6)-1)], as.character), stringsAsFactors = FALSE), !is.na(mm6[, 2:(ncol(mm6)-1)]), "1")

mm6 = mm6 %>% dplyr::rename("Other" = `Other (please specify. If more than one, separate with a \";\".)`)
mm6 = merge(mm6, other_choices_file[, c("other_choices", "classification")], by.x = "Other", by.y = "other_choices", all.x = TRUE) ### none

mm6[, c(3:8)] <- sapply(mm6[, c(3:8)], as.numeric)

# make new variables from the "classification" options
mm6 =mm6 %>% mutate(`direction requests`= ifelse(classification == "direction requests", 1, NA),
                    `distance traveled` = ifelse(classification == "new coding - distance traveled", 1, NA),
                    `migration` = ifelse(classification == "new coding - migration", 1, NA),
                    `number of locations visited` = ifelse(classification == "new coding - number of locations visited", 1, NA),
                    `other`=ifelse(classification == "new coding - other", 1, NA),
                    `time spent at work` = ifelse(classification == "new coding - time spent at work", 1, NA),
                    `other roaming/international trips/tourism statistics` = ifelse(classification == "new coding - other roaming/international trips/tourism statistics", 1, NA),
                    `population counts` = ifelse(classification == "new coding - population counts", 1, NA),
                    `radius of gyration` = ifelse(classification == "new coding - radius of gyration ", 1, NA))

#### for seventh dataset

mm7 = mobility_metrics %>% select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.mobility.metrics.are.used.from.the.data..Check.all.that.apply..7":"X.308" )
names(mm7) = mm7[1,]
mm7 = mm7[-1,]
mm7<-replace(mm7, mm7=="",NA) 
mm7[, 2:(ncol(mm7)-1)]<-replace(data.frame(lapply(mm7[, 2:(ncol(mm7)-1)], as.character), stringsAsFactors = FALSE), !is.na(mm7[, 2:(ncol(mm7)-1)]), "1")

mm7 = mm7 %>% dplyr::rename("Other" = `Other (please specify. If more than one, separate with a \";\".)`)
mm7 = merge(mm7, other_choices_file[, c("other_choices", "classification")], by.x = "Other", by.y = "other_choices", all.x = TRUE) ### none

mm7[, c(3:8)] <- sapply(mm7[, c(3:8)], as.numeric)

# make new variables from the "classification" options
mm7 =mm7 %>% mutate(`direction requests`= ifelse(classification == "direction requests", 1, NA),
                    `distance traveled` = ifelse(classification == "new coding - distance traveled", 1, NA),
                    `migration` = ifelse(classification == "new coding - migration", 1, NA),
                    `number of locations visited` = ifelse(classification == "new coding - number of locations visited", 1, NA),
                    `other`=ifelse(classification == "new coding - other", 1, NA),
                    `time spent at work` = ifelse(classification == "new coding - time spent at work", 1, NA),
                    `other roaming/international trips/tourism statistics` = ifelse(classification == "new coding - other roaming/international trips/tourism statistics", 1, NA),
                    `population counts` = ifelse(classification == "new coding - population counts", 1, NA),
                    `radius of gyration` = ifelse(classification == "new coding - radius of gyration ", 1, NA))

#### for eighth dataset

mm8 = mobility_metrics %>% select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.mobility.metrics.are.used.from.the.data..Check.all.that.apply..8":"X.343" )
names(mm8) = mm8[1,]
mm8 = mm8[-1,]
mm8<-replace(mm8, mm8=="",NA) 
mm8[, 2:(ncol(mm8)-1)]<-replace(data.frame(lapply(mm8[, 2:(ncol(mm8)-1)], as.character), stringsAsFactors = FALSE), !is.na(mm8[, 2:(ncol(mm8)-1)]), "1")

mm8 = mm8 %>% dplyr::rename("Other" = `Other (please specify. If more than one, separate with a \";\".)`)
mm8 = merge(mm8, other_choices_file[, c("other_choices", "classification")], by.x = "Other", by.y = "other_choices", all.x = TRUE) ### none

mm8[, c(3:8)] <- sapply(mm8[, c(3:8)], as.numeric)

# make new variables from the "classification" options
mm8 =mm8 %>% mutate(`direction requests`= ifelse(classification == "direction requests", 1, NA),
                    `distance traveled` = ifelse(classification == "new coding - distance traveled", 1, NA),
                    `migration` = ifelse(classification == "new coding - migration", 1, NA),
                    `number of locations visited` = ifelse(classification == "new coding - number of locations visited", 1, NA),
                    `other`=ifelse(classification == "new coding - other", 1, NA),
                    `time spent at work` = ifelse(classification == "new coding - time spent at work", 1, NA),
                    `other roaming/international trips/tourism statistics` = ifelse(classification == "new coding - other roaming/international trips/tourism statistics", 1, NA),
                    `population counts` = ifelse(classification == "new coding - population counts", 1, NA),
                    `radius of gyration` = ifelse(classification == "new coding - radius of gyration ", 1, NA))

#### for ninth dataset

mm9 = mobility_metrics %>% select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.mobility.metrics.are.used.from.the.data..Check.all.that.apply..9":"X.378" )
names(mm9) = mm9[1,]
mm9 = mm9[-1,]
mm9<-replace(mm9, mm9=="",NA) 
mm9[, 2:(ncol(mm9)-1)]<-replace(data.frame(lapply(mm9[, 2:(ncol(mm9)-1)], as.character), stringsAsFactors = FALSE), !is.na(mm9[, 2:(ncol(mm9)-1)]), "1")

mm9 = mm9 %>% dplyr::rename("Other" = `Other (please specify. If more than one, separate with a \";\".)`)
mm9 = merge(mm9, other_choices_file[, c("other_choices", "classification")], by.x = "Other", by.y = "other_choices", all.x = TRUE) ### none

mm9[, c(3:8)] <- sapply(mm9[, c(3:8)], as.numeric)

# make new variables from the "classification" options
mm9 =mm9 %>% mutate(`direction requests`= ifelse(classification == "direction requests", 1, NA),
                    `distance traveled` = ifelse(classification == "new coding - distance traveled", 1, NA),
                    `migration` = ifelse(classification == "new coding - migration", 1, NA),
                    `number of locations visited` = ifelse(classification == "new coding - number of locations visited", 1, NA),
                    `other`=ifelse(classification == "new coding - other", 1, NA),
                    `time spent at work` = ifelse(classification == "new coding - time spent at work", 1, NA),
                    `other roaming/international trips/tourism statistics` = ifelse(classification == "new coding - other roaming/international trips/tourism statistics", 1, NA),
                    `population counts` = ifelse(classification == "new coding - population counts", 1, NA),
                    `radius of gyration` = ifelse(classification == "new coding - radius of gyration ", 1, NA))



# combine datasets
mm_combo = rbind(mm1 %>% select(-c(Response)), mm2, mm3, mm4, mm5, mm6, mm7, mm8, mm9)
mm_combo = mm_combo %>% dplyr::rename(`Time spent at particular locations / trips (like businesses)` = `Time spent at particular locations (like businesses)`)

mm_combo_short = mm_combo %>% dplyr::select(`Open-Ended Response`, `Time spent at home/residence`:`Traffic flows`, `direction requests`:`radius of gyration`)
mm_combo_short = mm_combo_short %>% dplyr::rename("ID" = "Open-Ended Response")
mm_combo_long <- gather(mm_combo_short, answer_option, response, "Time spent at home/residence":"radius of gyration", factor_key=TRUE)
mm_combo_long$response = as.numeric(mm_combo_long$response)
mm_combo_sum = mm_combo_long %>% group_by(answer_option) %>% summarize(count = sum(response, na.rm=TRUE))
mm_combo_sum %>% arrange(desc(count))
clean_mobility_metrics = mm_combo_short
clean_mobility_metrics2 = mm_combo_long %>% filter(is.na(response)==FALSE) %>% group_by(ID) %>% dplyr::select(ID, answer_option) 
clean_mobility_metrics3 <- data.frame(lapply(clean_mobility_metrics2, as.character), stringsAsFactors=FALSE)
clean_mobility_metrics3 = clean_mobility_metrics3 %>% group_by(ID) %>% summarise_all(funs(mobility_metrics = list(.)))
saveRDS(clean_mobility_metrics3, "Data/clean/clean_mobility_metrics3.Rdata")

clean_mobility_metrics2 = merge(clean_mobility_metrics2, clean_amy[, c("ID", "full_review", "quant_data", "loc_focus", "country_focus", "date")], by = "ID", all.x = TRUE)
clean_mobility_metrics2 = clean_mobility_metrics2 %>% filter(full_review == "Yes" & quant_data == "Yes")

# tables
# first - just the straight up table of frequencies
table(clean_mobility_metrics2$answer_option, useNA = "ifany")
length(unique(clean_mobility_metrics2$ID)) ## number of papers = 236
nrow(clean_mobility_metrics2)  ### number of instances

# make a variable that collapses some of the less common ones
clean_mobility_metrics2 = clean_mobility_metrics2 %>% 
  mutate(answer_option_cat = as.character(answer_option))
clean_mobility_metrics2 = clean_mobility_metrics2 %>%
  mutate(answer_option_cat = ifelse(answer_option_cat %in% 
                                      c("direction requests", "distance traveled",
                                        "number of locations visited",
                                        "time spent at work", "other", "other roaming/international trips/tourism statistics",
                                        "population counts", "radius of gyration"), "Other", answer_option_cat))
clean_mobility_metrics2 = clean_mobility_metrics2 %>%
  mutate(answer_option_cat = ifelse(answer_option_cat %in% "migration", "Migration", answer_option_cat))
# make a table and save
as.data.frame(table(clean_mobility_metrics2$answer_option_cat)) %>% arrange(desc(Freq)) %>%
  mutate(percent = round(Freq / length(unique(clean_mobility_metrics2$ID))*100)) %>% 
  flextable() %>% save_as_docx(path="tables/mobility_metrics.docx")


# barplot by date
metrics_date = clean_mobility_metrics2 %>% group_by(date, answer_option_cat) %>% summarize(count = n())
ggplot(metrics_date, aes(fill = answer_option_cat, y = count, x = date ))+
  geom_bar(position = "fill", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Mobility metrics")+ylab("Percent") + scale_fill_igv()+
  scale_y_continuous(labels = scales::percent)+xlab("Date")
ggsave("figures/mobility_metrics.jpeg", width = 11, height =9)


ggplot(metrics_date, aes(fill = answer_option_cat, y = count, x = date ))+
  geom_bar(stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Mobility metrics")+ylab("Count") + scale_fill_igv()+
xlab("Date")
ggsave("figures/mobility_metrics_count.jpeg", width = 11, height =9)


# barplot by location
metrics_location = clean_mobility_metrics2 %>% mutate(Location = 
                                                        ifelse(loc_focus == "national", country_focus, loc_focus))
metrics_location = metrics_location %>% group_by(Location, answer_option_cat) %>% summarize(count = n())
ggplot(metrics_location, aes(fill = answer_option_cat, y = count, x = Location ))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Mobility metrics") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# check the papers that have all na
mm_combo_short$all_na = apply(mm_combo_short[, 2:16], MARGIN = 1, FUN = function(x) all(is.na(x)))
check = mm_combo_short %>% group_by("Open-Ended Response") %>% slice(1) %>% filter(all_na=="TRUE")




###### SPATIAL RESOLUTION OF MOBILITY DATA USED
#### for first dataset
sp1 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.is.the.spatial.scale.of.the.mobility.data.used.in.the.analysis.":"X.46" )
names(sp1) = c("ID", "spatial_scale", "other")

# clean up the "other" options
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "201028", "Subnational - province/state"),
                     other = replace(other, ID == "201028", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "491992", "Airport to airport"),   ### original data is airport to airport, but they aggregated up to micro region level
                     other = replace(other, ID == "491992", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "250305", "Subnational - province/state"),
                     other = replace(other, ID == "250305", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "469208", "Grid cells"),
                     other = replace(other, ID == "469208", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "374972", "Airport to airport"),   
                     other = replace(other, ID == "374972", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "484275", "Subnational - city/village/metro area"),   
                     other = replace(other, ID == "484275", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "224674", "Other - two levels"),   
                     other = replace(other, ID == "224674", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "486227", "Subnational - smaller than city/village/metro area (e.g. zipcode, census block)"),   
                     other = replace(other, ID == "486227", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "164025", "Subnational - city/village/metro area"),   
                     other = replace(other, ID == "164025", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "426822", "Subnational - county/district"),   
                     other = replace(other, ID == "426822", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "955764", "Subnational - county/district"),   
                     other = replace(other, ID == "955764", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "801271", "Subnational - city/village/metro area"),   # for first dataset; second is longitude / latitude
                     other = replace(other, ID == "801271", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "272890", "Subnational - smaller than city/village/metro area (e.g. zipcode, census block)"),   # using GLEAM
                     other = replace(other, ID == "272890", NA))
sp1 = sp1 %>% mutate(spatial_scale=replace(spatial_scale,ID == "247328", "Longitude/latitude coordinates"),   # transportation stations
                     other = replace(other, ID == "247328", NA))

#### for second dataset
sp2 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.is.the.spatial.scale.of.the.mobility.data.used.in.the.analysis..1":"X.81" )
names(sp2) = c("ID", "spatial_scale", "other")  
#table(sp2$other) # 5 datasets to clean up

sp2 = sp2 %>% mutate(spatial_scale=replace(spatial_scale,ID == "491992", "Subnational - county/district"),   
                     other = replace(other, ID == "491992", NA))
sp2 = sp2 %>% mutate(spatial_scale=replace(spatial_scale,ID == "224674", "Other - two levels"),   
                     other = replace(other, ID == "224674", NA))
sp2 = sp2 %>% mutate(spatial_scale=replace(spatial_scale,ID == "164025", "Subnational - city/village/metro area"),   
                     other = replace(other, ID == "164025", NA))
sp2 = sp2 %>% mutate(spatial_scale=replace(spatial_scale,ID == "426822", "Subnational - county/district"),   
                     other = replace(other, ID == "426822", NA))

#### for third dataset
sp3 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.is.the.spatial.scale.of.the.mobility.data.used.in.the.analysis..2":"X.116" )
names(sp3) = c("ID", "spatial_scale", "other")  

sp3 = sp3 %>% mutate(spatial_scale=replace(spatial_scale,ID == "426822", "Grid cells"),   
                     other = replace(other, ID == "426822", NA))
sp3 = sp3 %>% mutate(spatial_scale=replace(spatial_scale,ID == "164025", "Subnational - city/village/metro area"),   
                     other = replace(other, ID == "164025", NA))
sp3 = sp3 %>% mutate(spatial_scale=replace(spatial_scale,ID == "955764", "Subnational - county/district"),   
                     other = replace(other, ID == "955764", NA))
sp3 = sp3 %>% mutate(spatial_scale=replace(spatial_scale,ID == "520252", "Subnational - city/village/metro area"),   
                     other = replace(other, ID == "520252", NA))

#### for fourth dataset
sp4 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.is.the.spatial.scale.of.the.mobility.data.used.in.the.analysis..3":"X.151" )
names(sp4) = c("ID", "spatial_scale", "other") # none

#### for fifth dataset
sp5 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.is.the.spatial.scale.of.the.mobility.data.used.in.the.analysis..4":"X.186" )
names(sp5) = c("ID", "spatial_scale", "other") # none

#### for sixth dataset
sp6 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.is.the.spatial.scale.of.the.mobility.data.used.in.the.analysis..5":"X.221" )
names(sp6) = c("ID", "spatial_scale", "other") # none

#### for seventh dataset
sp7 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.is.the.spatial.scale.of.the.mobility.data.used.in.the.analysis..6":"X.256" )
names(sp7) = c("ID", "spatial_scale", "other") # none

#### for eigth dataset
sp8 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.is.the.spatial.scale.of.the.mobility.data.used.in.the.analysis..7":"X.291" )
names(sp8) = c("ID", "spatial_scale", "other") # none

#### for ninth dataset
sp9 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.is.the.spatial.scale.of.the.mobility.data.used.in.the.analysis..8":"X.326" )
names(sp9) = c("ID", "spatial_scale", "other") # none

#### for tenth dataset
sp10 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..","What.is.the.spatial.scale.of.the.mobility.data.used.in.the.analysis..9":"X.361" )
names(sp10) = c("ID", "spatial_scale", "other") # none

# combine datasets
sp_combo = rbind(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10)
order = sp_combo %>% group_by(spatial_scale) %>% filter(is.na(spatial_scale)==FALSE) %>% summarize(granularity = n())
order %>% arrange(desc(granularity), na.rm=TRUE)
clean_spatial_granularity = sp_combo %>% dplyr::select(ID, spatial_scale) %>% filter(is.na(spatial_scale)==FALSE)
clean_spatial_granularity2 = clean_spatial_granularity %>% group_by(ID) %>% arrange(spatial_scale) %>%
  summarise(spat_scale = paste(spatial_scale, collapse = ","), times = length(spatial_scale))
clean_spatial_granularity2 = clean_spatial_granularity %>% group_by(ID) %>% summarise_all(funs(spatial_scale = list(.)))

#clean_spatial_granularity2 = clean_spatial_granularity %>% group_by(ID) %>% arrange(spatial_scale) %>%
#  summarise(spat_scale = c(spatial_scale), times = length(spatial_scale))
saveRDS(clean_spatial_granularity2, "Data/clean/clean_spatial_granularity2.Rdata")

clean_spatial_granularity = merge(clean_spatial_granularity, clean_amy[, c("ID", "quant_data", "full_review", "loc_focus", "country_focus", "date")], by = "ID", all.x = TRUE)
clean_spatial_granularity = clean_spatial_granularity %>% filter(full_review == "Yes" & quant_data=="Yes")

# tables
# first - just the straight up table of frequencies
table(clean_spatial_granularity$spatial_scale, useNA = "ifany")
length(unique(clean_spatial_granularity$ID)) ## number of papers = 241
nrow(clean_spatial_granularity)  ### number of instances

as.data.frame(table(clean_spatial_granularity$spatial_scale)) %>% arrange(desc(Freq)) %>%
  mutate(Percent = round(Freq/length(unique(clean_spatial_granularity$ID))*100)) %>%
  flextable() %>% autofit() %>% save_as_docx(path = "tables/spatial_granularity.docx")

# barplot by date
spatial_date = clean_spatial_granularity %>% group_by(date, spatial_scale) %>% summarize(count = n())
ggplot(spatial_date, aes(fill = spatial_scale, y = count, x = date ))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Spatial scale")
ggsave("figures/spatial_scale_count.jpeg", width = 11, height =9)

# barplot by location
spatial_location = clean_spatial_granularity %>% mutate(Location = ifelse(loc_focus == "national", country_focus, loc_focus))
spatial_location = spatial_location %>% group_by(Location, spatial_scale) %>% summarize(count = n())
ggplot(spatial_location, aes(fill = spatial_scale, y = count, x = Location ))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Spatial scale") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



############################# granularity of the data ##################################################
gran = survey_file
write.csv(names(survey_file), "names_survey_file.csv")

gr1 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.granularity.of.the.data.in.the.analysis.":"X.47" )
names(gr1) = c("ID", "granularity", "other") # 9 "other" options

View(gr1 %>% filter(is.na(other)==FALSE))

# clean up the "Other" options
gr1 = gr1 %>% mutate(granularity=replace(granularity,ID == "250305", "Data aggregates multiple individuals' movements"),   
                     other = replace(other, ID == "250305", NA))   # Baidu data
gr1 = gr1 %>% mutate(granularity=replace(granularity,ID == "712350", "Data aggregates multiple individuals' movements"),   
                     other = replace(other, ID == "712350", NA))   # number of flights
gr1 = gr1 %>% mutate(granularity=replace(granularity,ID == "176487", "Data aggregates multiple individuals' movements"),   
                     other = replace(other, ID == "176487", NA))   # Baidu data
gr1 = gr1 %>% mutate(granularity=replace(granularity,ID == "272890", "Data aggregates multiple individuals' movements"),   
                     other = replace(other, ID == "272890", NA))   # GLEAM
gr1 = gr1 %>% mutate(granularity=replace(granularity,ID == "474382", "Data aggregates multiple individuals' movements"),   
                     other = replace(other, ID == "474382", NA))   # passenger flows
gr1 = gr1 %>% mutate(granularity=replace(granularity,ID == "486227", "Measures individual-level movements"),   
                     other = replace(other, ID == "486227", NA))   # from mobile phones; more description at https://reader.elsevier.com/reader/sd/pii/S1201971220303556?token=443C77F5D7CF3F3DC6F4AC58DCB118CCA0FE890C44598724440FA6B21F0C395F8831CA5377A6BB8F229DBFB4A64D66CA&originRegion=us-east-1&originCreation=20220112043459
gr1 = gr1 %>% mutate(granularity=replace(granularity,ID == "498590", "Data aggregates multiple individuals' movements"),   
                     other = replace(other, ID == "498590", NA))   # tourists
gr1 = gr1 %>% mutate(granularity=replace(granularity,ID == "651386", "Data aggregates multiple individuals' movements"),   
                     other = replace(other, ID == "651386", NA))   # travellers
gr1 = gr1 %>% mutate(granularity=replace(granularity,ID == "940429", "Data aggregates multiple individuals' movements"),   
                     other = replace(other, ID == "940429", NA))   



gr2 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.granularity.of.the.data.in.the.analysis..1":"X.82" )
names(gr2) = c("ID", "granularity", "other") # 1 "other" options

#View(gr2 %>% filter(is.na(other)==FALSE))
gr2 = gr2 %>% mutate(granularity=replace(granularity,ID == "147477", "Data aggregates multiple individuals' movements"),   
                     other = replace(other, ID == "147477", NA))   # govt response tracker - "stringency index"


gr3 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.granularity.of.the.data.in.the.analysis..2":"X.117" )
names(gr3) = c("ID", "granularity", "other") # 1 "other" options

#View(gr3 %>% filter(is.na(other)==FALSE))
gr3 = gr3 %>% mutate(granularity=replace(granularity,ID == "164025", "Data aggregates multiple individuals' movements"),   
                     other = replace(other, ID == "164025", NA))   # google community mobility reports


gr4 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.granularity.of.the.data.in.the.analysis..3":"X.152" )
names(gr4) = c("ID", "granularity", "other") # no "other" options

#View(gr4 %>% filter(is.na(other)==FALSE))


gr5 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.granularity.of.the.data.in.the.analysis..4":"X.187" )
names(gr5) = c("ID", "granularity", "other") # no "other" options

gr6 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.granularity.of.the.data.in.the.analysis..5":"X.222" )
names(gr6) = c("ID", "granularity", "other") # no "other" options

gr7 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.granularity.of.the.data.in.the.analysis..6":"X.257" )
names(gr7) = c("ID", "granularity", "other") # no "other" options

gr8 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.granularity.of.the.data.in.the.analysis..7":"X.292" )
names(gr8) = c("ID", "granularity", "other") # no "other" options

gr9 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.granularity.of.the.data.in.the.analysis..8":"X.327" )
names(gr9) = c("ID", "granularity", "other") # no "other" options

gr10 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.granularity.of.the.data.in.the.analysis..9":"X.362" )
names(gr10) = c("ID", "granularity", "other") # no "other" options

# combine datasets
gr_combo = rbind(gr1, gr2, gr3, gr4, gr5, gr6, gr7, gr8, gr9, gr10)
order = gr_combo %>% group_by(granularity) %>% filter(is.na(granularity)==FALSE) %>% summarize(num = n())
order %>% arrange(desc(granularity), na.rm=TRUE)
clean_granularity = gr_combo %>% dplyr::select(ID, granularity) %>% filter(is.na(granularity)==FALSE)
#clean_granularity2 = clean_granularity %>% group_by(ID) %>% summarize(new_gran = c(granularity))
clean_granularity2 = clean_granularity %>% group_by(ID) %>% summarise_all(funs(merged = list((.))))

#clean_spatial_granularity2 = clean_spatial_granularity %>% group_by(ID) %>% arrange(spatial_scale) %>%
#  summarise(spat_scale = c(spatial_scale), times = length(spatial_scale))
saveRDS(clean_granularity2, "Data/clean/clean_granularity2.Rdata")



clean_granularity = merge(clean_granularity, clean_amy[, c("ID", "full_review", "quant_data", "loc_focus", "country_focus", "date")], by = "ID", all.x = TRUE)
clean_granularity = clean_granularity %>% filter(full_review == "Yes" & quant_data == "Yes")

# tables
# first - just the straight up table of frequencies
table(clean_granularity$granularity, useNA = "ifany")
length(unique(clean_granularity$ID)) ## number of papers = 241
nrow(clean_granularity)  ### number of instances = 316

# barplot by date
gran_date = clean_granularity %>% group_by(date, granularity) %>% summarize(count = n())
ggplot(gran_date, aes(fill = granularity, y = count, x = date ))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Granularity of data")

# barplot by location
gran_location = clean_granularity %>% mutate(Location = ifelse(loc_focus == "national", country_focus, loc_focus))
gran_location = gran_location %>% group_by(Location, granularity) %>% summarize(count = n())
ggplot(gran_location, aes(fill = granularity, y = count, x = Location ))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Granularity of data") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

################################## how data is made available
da1 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "How.is.the.data.made.available.":"X.64" )
names(da1) = c("ID", "availability", "other") # 6 "other" options

View(da1 %>% filter(is.na(other)==FALSE))

# clean up the "Other" options
da1 = da1 %>% mutate(availability=replace(availability,ID == "153128", "Made available to authors by a company, government agency, etc."),   
                     other = replace(other, ID == "153128", NA))   # mobile phone data
da1 = da1 %>% mutate(availability=replace(availability,ID == "243005", "Free and open source"),   
                     other = replace(other, ID == "243005", NA))   
da1 = da1 %>% mutate(availability=replace(availability,ID == "272890", "Free and open source"),   
                     other = replace(other, ID == "272890", NA))   # GLEAM
da1 = da1 %>% mutate(availability=replace(availability,ID == "365889", "Free and open source"),   
                     other = replace(other, ID == "365889", NA))   # available from previous pub; though seems they also use OAG?
da1 = da1 %>% mutate(availability=replace(availability,ID == "388998", "Free and open source"),   
                     other = replace(other, ID == "388998", NA))   # though seems it's from public info?
da1 = da1 %>% mutate(availability=replace(availability,ID == "899450", "Available for purchase"),   
                     other = replace(other, ID == "899450", NA))   # makes available for free, but authors used HereMaps (purchase)



da2 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "How.is.the.data.made.available..1":"X.99" )
names(da2) = c("ID", "availability", "other") # 0 "other" options

da3 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "How.is.the.data.made.available..2":"X.134" )
names(da3) = c("ID", "availability", "other") # 0 "other" options

da4 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "How.is.the.data.made.available..3":"X.169" )
names(da4) = c("ID", "availability", "other") # 0 "other" options

da5 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "How.is.the.data.made.available..4":"X.204" )
names(da5) = c("ID", "availability", "other") # 0 "other" options

da6 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "How.is.the.data.made.available..5":"X.239" )
names(da6) = c("ID", "availability", "other") # 0 "other" options

da7 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "How.is.the.data.made.available..6":"X.274" )
names(da7) = c("ID", "availability", "other") # 0 "other" options

da8 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "How.is.the.data.made.available..7":"X.309" )
names(da8) = c("ID", "availability", "other") # 0 "other" options

da9 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "How.is.the.data.made.available..8":"X.344" )
names(da9) = c("ID", "availability", "other") # 0 "other" options

da10 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "How.is.the.data.made.available..9":"X.379" )
names(da10) = c("ID", "availability", "other") # 0 "other" options

# combine datasets
da_combo = rbind(da1, da2, da3, da4, da5, da6, da7, da8, da9, da10)
order = da_combo %>% group_by(availability) %>% filter(is.na(availability)==FALSE) %>% summarize(num = n())
order %>% arrange(desc(availability), na.rm=TRUE)
clean_availability = da_combo %>% dplyr::select(ID, availability) %>% filter(is.na(availability)==FALSE)
#clean_granularity2 = clean_granularity %>% group_by(ID) %>% summarize(new_gran = c(granularity))
clean_availability2 = clean_availability %>% group_by(ID) %>% summarise_all(funs(dat_availability = list((.))))

#clean_spatial_granularity2 = clean_spatial_granularity %>% group_by(ID) %>% arrange(spatial_scale) %>%
#  summarise(spat_scale = c(spatial_scale), times = length(spatial_scale))
saveRDS(clean_availability2, "Data/clean/clean_availability.Rdata")





############### identifiability and privacy
priv1 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.identifiability.or.privacy.of.the.dataset.":"X.65" )
names(priv1) = c("ID", "identifiability", "other") # 2 "other" options

#View(priv1 %>% filter(is.na(other)==FALSE))

# clean up the "Other" options
priv1 = priv1 %>% mutate(identifiability=replace(identifiability,ID == "562500", "Other - flight data"),   
                     other = replace(other, ID == "562500", NA))
priv1 = priv1 %>% mutate(identifiability=replace(identifiability,ID == "712350", "Other - flight data"),   
                     other = replace(other, ID == "712350", NA))


priv2 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.identifiability.or.privacy.of.the.dataset..1":"X.100" )
names(priv2) = c("ID", "identifiability", "other") # 1 "other" option

priv2 = priv2 %>% mutate(identifiability=replace(identifiability,ID == "712350", "Other - train frequency"),   
                         other = replace(other, ID == "712350", NA))

priv3 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.identifiability.or.privacy.of.the.dataset..2":"X.135" )
names(priv3) = c("ID", "identifiability", "other") # 0 "other" options

priv4 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.identifiability.or.privacy.of.the.dataset..3":"X.170" )
names(priv4) = c("ID", "identifiability", "other") # 0 "other" options

priv5 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.identifiability.or.privacy.of.the.dataset..4":"X.205" )
names(priv5) = c("ID", "identifiability", "other") # 0 "other" options

priv6 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.identifiability.or.privacy.of.the.dataset..5":"X.240" )
names(priv6) = c("ID", "identifiability", "other") # 0 "other" options

priv7 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.identifiability.or.privacy.of.the.dataset..6":"X.275" )
names(priv7) = c("ID", "identifiability", "other") # 0 "other" options

priv8 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.identifiability.or.privacy.of.the.dataset..7":"X.310" )
names(priv8) = c("ID", "identifiability", "other") # 0 "other" options

priv9 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.identifiability.or.privacy.of.the.dataset..8":"X.345" )
names(priv9) = c("ID", "identifiability", "other") # 0 "other" options

priv10 = mobility_metrics %>% filter(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`=="Yes") %>% 
  dplyr::select("What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.identifiability.or.privacy.of.the.dataset..9":"X.380" )
names(priv10) = c("ID", "identifiability", "other") # 0 "other" options

# combine datasets
priv_combo = rbind(priv1, priv2, priv3, priv4, priv5, priv6, priv7, priv8, priv9, priv10)
order = priv_combo %>% group_by(identifiability) %>% filter(is.na(identifiability)==FALSE) %>% summarize(num = n())
order %>% arrange(desc(num), na.rm=TRUE)
clean_identifiability = priv_combo %>% dplyr::select(ID, identifiability) %>% filter(is.na(identifiability)==FALSE)
#clean_granularity2 = clean_granularity %>% group_by(ID) %>% summarize(new_gran = c(granularity))
clean_identifiability2 = clean_identifiability %>% group_by(ID) %>% summarise_all(funs(dat_identifiability = list((.))))

#clean_spatial_granularity2 = clean_spatial_granularity %>% group_by(ID) %>% arrange(spatial_scale) %>%
#  summarise(spat_scale = c(spatial_scale), times = length(spatial_scale))
saveRDS(clean_identifiability2, "Data/clean/clean_identifiability.Rdata")




################################## availability of meta data (directly related to the data set)
dm1 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..directly.related.to.the.dataset...Check.all.that.apply.":"X.52" )
names(dm1) = dm1[1,]
dm1 = dm1  %>% filter(Response=="Yes")
dm1 = dm1 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
dm1 = dm1 %>% dplyr::select(-c(Response))

# clean up the "Other" options 
dm1 = dm1 %>% mutate(Other = replace(Other, ID == "204102" | ID == "408459" | ID == "761918" | ID == "808424" | ID == "785805", NA))
dm1 = dm1 %>% mutate(`Socio-economic Information` = replace(`Socio-economic Information`, ID == "649756", "Socio-economic Information"),
                     Other = replace(Other, ID == "649756", NA))
dm1 = dm1 %>% mutate(Other = replace(Other, ID == "711982" | ID == "841109", "Other - clinical"))
dm1 = dm1 %>% mutate(Other = replace(Other, ID == "831221" | ID == "831221", "Other - Means of transport, duration of journey, reason for moving"))



dm2 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..directly.related.to.the.dataset...Check.all.that.apply..1":"X.87" )
names(dm2) = dm2[1,]
dm2 = dm2  %>% filter(Response=="Yes")
dm2 = dm2 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
dm2 = dm2 %>% dplyr::select(-c(Response))

dm2 = dm2 %>% mutate(`Socio-economic Information` = replace(`Socio-economic Information`, ID == "577820", "Socio-economic Information"),
                     Other = replace(Other, ID == "577820", NA))


dm3 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..directly.related.to.the.dataset...Check.all.that.apply..2":"X.122" )
names(dm3) = dm3[1,]
dm3 = dm3  %>% filter(Response=="Yes")
dm3 = dm3 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
dm3 = dm3 %>% dplyr::select(-c(Response))


dm4 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..directly.related.to.the.dataset...Check.all.that.apply..3":"X.157" )
names(dm4) = dm4[1,]
dm4 = dm4  %>% filter(Response=="Yes")
dm4 = dm4 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
dm4 = dm4 %>% dplyr::select(-c(Response))


dm5 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..directly.related.to.the.dataset...Check.all.that.apply..4":"X.192" )
names(dm5) = dm5[1,]
dm5 = dm5  %>% filter(Response=="Yes")
dm5 = dm5 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
dm5 = dm5 %>% dplyr::select(-c(Response))


dm6 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..directly.related.to.the.dataset...Check.all.that.apply..5":"X.227" )
names(dm6) = dm6[1,]
dm6 = dm6  %>% filter(Response=="Yes")
dm6 = dm6 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
dm6 = dm6 %>% dplyr::select(-c(Response))

dm7 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..directly.related.to.the.dataset...Check.all.that.apply..6":"X.262" )
names(dm7) = dm7[1,]
dm7 = dm7  %>% filter(Response=="Yes")
dm7 = dm7 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
dm7 = dm7 %>% dplyr::select(-c(Response))

dm8 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..directly.related.to.the.dataset...Check.all.that.apply..7":"X.297" )
names(dm8) = dm8[1,]
dm8 = dm8  %>% filter(Response=="Yes")
dm8 = dm8 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
dm8 = dm8 %>% dplyr::select(-c(Response))

dm9 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..directly.related.to.the.dataset...Check.all.that.apply..8":"X.332" )
names(dm9) = dm9[1,]
dm9 = dm9  %>% filter(Response=="Yes")
dm9 = dm9 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
dm9 = dm9 %>% dplyr::select(-c(Response))

dm10 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..directly.related.to.the.dataset...Check.all.that.apply..9":"X.367" )
names(dm10) = dm10[1,]
dm10 = dm10  %>% filter(Response=="Yes")
dm10 = dm10 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
dm10 = dm10 %>% dplyr::select(-c(Response))



# combine datasets
dm_combo = rbind(dm1, dm2, dm3, dm4, dm5, dm6, dm7, dm8, dm9, dm10)
dm_combo[, 2:(ncol(dm_combo)-1)]<-replace(data.frame(lapply(dm_combo[, 2:(ncol(dm_combo)-1)], as.character), stringsAsFactors = FALSE), !is.na(dm_combo[, 2:(ncol(dm_combo)-1)]), "1")
dm_combo[, c(2:6)] <- sapply(dm_combo[, c(2:6)], as.numeric)
dm_combo = dm_combo %>% mutate(`Other - clinical` = ifelse(Other == "Other - clinical", 1, NA),
                               `Other - Means of transport, duration of journey, reason for moving` = ifelse(Other == "Other - Means of transport, duration of journey, reason for moving", 1, NA))
dm_combo = dm_combo %>% dplyr::select(-c(Other))
dm_combo_long <- gather(dm_combo, answer_option, response, "Age": "Other - Means of transport, duration of journey, reason for moving", factor_key=TRUE)
dm_combo_long$response = as.numeric(dm_combo_long$response)
dm_combo_sum = dm_combo_long %>% group_by(answer_option) %>% summarize(count = sum(response, na.rm=TRUE))
dm_combo_sum %>% arrange(desc(count))
clean_direct_meta = dm_combo_long %>% filter(is.na(response)==FALSE) %>% group_by(ID) %>% dplyr::select(ID, answer_option) 
clean_direct_meta2 <- data.frame(lapply(clean_direct_meta, as.character), stringsAsFactors=FALSE)
clean_direct_meta2 = clean_direct_meta2 %>% group_by(ID) %>% summarise_all(funs(direct_metadata = list(.)))

saveRDS(clean_direct_meta2, "Data/clean/clean_direct_meta.Rdata")






################################## availability of meta data (ecological)
em1 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..ecologically...Check.all.that.apply.":"X.57" )
names(em1) = em1[1,]
em1 = em1  %>% filter(Response=="Yes")
em1 = em1 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
em1 = em1 %>% dplyr::select(-c(Response))

# clean up the "Other" options 
em1 = em1 %>% mutate(Other = replace(Other, ID == "273118" | ID == "312222" | ID == "686801" |ID == "761918" | ID == "885960" | ID == "987179", NA))
em1 = em1 %>% mutate(`Socio-economic information` = replace(`Socio-economic information`, ID == "766150", "Socio-economic information"),
                     Other = replace(Other, ID == "766150", NA))



em2 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..ecologically...Check.all.that.apply..1":"X.92" )
names(em2) = em2[1,]
em2 = em2  %>% filter(Response=="Yes")
em2 = em2 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
em2 = em2 %>% dplyr::select(-c(Response))


em3 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..ecologically...Check.all.that.apply..2":"X.127" )
names(em3) = em3[1,]
em3 = em3  %>% filter(Response=="Yes")
em3 = em3 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
em3 = em3 %>% dplyr::select(-c(Response))


em4 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..ecologically...Check.all.that.apply..3":"X.162" )
names(em4) = em4[1,]
em4 = em4  %>% filter(Response=="Yes")
em4 = em4 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
em4 = em4 %>% dplyr::select(-c(Response))



em5 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..ecologically...Check.all.that.apply..4":"X.197" )
names(em5) = em5[1,]
em5 = em5  %>% filter(Response=="Yes")
em5 = em5 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
em5 = em5 %>% dplyr::select(-c(Response))


em6 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..ecologically...Check.all.that.apply..5":"X.232" )
names(em6) = em6[1,]
em6 = em6  %>% filter(Response=="Yes")
em6 = em6 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
em6 = em6 %>% dplyr::select(-c(Response))




em7 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..ecologically...Check.all.that.apply..6":"X.267" )
names(em7) = em7[1,]
em7 = em7  %>% filter(Response=="Yes")
em7 = em7 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
em7 = em7 %>% dplyr::select(-c(Response))



em8 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..ecologically...Check.all.that.apply..7":"X.302" )
names(em8) = em8[1,]
em8 = em8  %>% filter(Response=="Yes")
em8 = em8 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
em8 = em8 %>% dplyr::select(-c(Response))


em9 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..ecologically...Check.all.that.apply..8":"X.337" )
names(em9) = em9[1,]
em9 = em9  %>% filter(Response=="Yes")
em9 = em9 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
em9 = em9 %>% dplyr::select(-c(Response))


em10 = mobility_metrics  %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, 
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Are.any.of.the.meta.data.available..ecologically...Check.all.that.apply..9":"X.372" )
names(em10) = em10[1,]
em10 = em10  %>% filter(Response=="Yes")
em10 = em10 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
em10 = em10 %>% dplyr::select(-c(Response))


#View(em10 %>% filter(is.na(Other)==FALSE))

# combine datasets
em_combo = rbind(em1, em2, em3, em4, em5, em6, em7, em8, em9, em10)
em_combo[, 2:(ncol(em_combo)-1)]<-replace(data.frame(lapply(em_combo[, 2:(ncol(em_combo)-1)], as.character), stringsAsFactors = FALSE), !is.na(em_combo[, 2:(ncol(em_combo)-1)]), "1")
em_combo[, c(2:6)] <- sapply(em_combo[, c(2:6)], as.numeric) ### no "other"
em_combo_long <- gather(em_combo, answer_option, response, "Age": "Other", factor_key=TRUE)
em_combo_long$response = as.numeric(em_combo_long$response)
em_combo_sum = em_combo_long %>% group_by(answer_option) %>% summarize(count = sum(response, na.rm=TRUE))
em_combo_sum %>% arrange(desc(count))
clean_eco_meta = em_combo_long %>% filter(is.na(response)==FALSE) %>% group_by(ID) %>% dplyr::select(ID, answer_option) 
clean_eco_meta2 <- data.frame(lapply(clean_eco_meta, as.character), stringsAsFactors=FALSE)
clean_eco_meta2 = clean_eco_meta2 %>% group_by(ID) %>% summarise_all(funs(eco_metadata = list(.)))

saveRDS(clean_eco_meta2, "Data/clean/clean_eco_meta.Rdata")





#################### pre-processing
preproc1 = mobility_metrics %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`, "What.is.the.paper.ID.number..from.the.spreadsheet..", "How.was.the.data.pre.processed..Check.all.that.apply.":"X.71" )
names(preproc1) = preproc1[1,]
preproc1 = preproc1  %>% filter(Response=="Yes")
preproc1 = preproc1 %>% rename("ID" = "Open-Ended Response",
                     "Other" = "Other (please specify)")
preproc1 = preproc1 %>% dplyr::select(-c(Response))

View(preproc1 %>% filter(is.na(Other)==FALSE))




########################## use of Sars-cov-2 data
use_cov_data1 = mobility_metrics %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`,
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "Does.the.paper.use.data.on.SARS.CoV.2.COVID.19..Check.all.that.apply.":"X.402" )
names(use_cov_data1) = use_cov_data1[1,]
use_cov_data1 = use_cov_data1  %>% filter(Response=="Yes")
use_cov_data1 = use_cov_data1 %>% rename("ID" = "Open-Ended Response",
                               "Other" = "Other (please specify)")
use_cov_data1 = use_cov_data1 %>% dplyr::select(-c(Response))

View(use_cov_data1 %>% filter(is.na(Other)==FALSE))

# clean up the "Other" options 
use_cov_data1 = use_cov_data1 %>% mutate(Other = replace(Other, ID == "115596" | ID == "340507", "Other - reproductive number"))
use_cov_data1 = use_cov_data1 %>% mutate(Other = replace(Other, ID == "120428" | ID == "415549" | ID == "463434" | ID == "608894" | ID == "636456" | ID == "647188" | ID == "810337" | ID == "831596" | ID == "914607" | ID == "940429" | ID == "955764", NA))
use_cov_data1 = use_cov_data1 %>% mutate(Other = replace(Other, ID == "208592", "Other - surveillance data"))
use_cov_data1 = use_cov_data1 %>% mutate(Other = replace(Other, ID == "238237" | ID == "255607" | ID == "332085" | ID == "365889" | ID == "469208" | ID == "816704" | ID == "918120", NA), 
                                         `Confirmed cases` = replace(`Confirmed cases`, ID == "238237" | ID == "255607" | ID == "332085" | ID == "365889" | 
                                                                      ID == "469208" | ID == "816704" | ID == "918120", "Confirmed cases"))
use_cov_data1 = use_cov_data1 %>% mutate(Other = replace(Other, ID == "300617", "Other - test positivity"))
use_cov_data1 = use_cov_data1 %>% mutate(Other = replace(Other, ID == "374972" | ID == "414420" | ID == "548207", NA),
                                         `Serological results or virological study` = replace(`Serological results or virological study`, ID == "374972" | ID == "414420" | ID == "548207", "Serological results or virological study"))
use_cov_data1 = use_cov_data1 %>% mutate(Other = replace(Other, ID == "753054", "Other - self-reported test result"))
use_cov_data1 = use_cov_data1 %>% mutate(Other = replace(Other, ID == "753054", "Other - self-reported test result"))


use_cov_data1[, 2:(ncol(use_cov_data1)-1)]<-replace(data.frame(lapply(use_cov_data1[, 2:(ncol(use_cov_data1)-1)], as.character), stringsAsFactors = FALSE), !is.na(use_cov_data1[, 2:(ncol(use_cov_data1)-1)]), "1")
use_cov_data1[, c(2:8)] <- sapply(use_cov_data1[, c(2:8)], as.numeric) ### no "other"
use_cov_data1 = use_cov_data1 %>% mutate(`Other - reproductive number` = ifelse(Other == "Other - reproductive number", 1, NA),
                                         `Other - surveillance data` = ifelse(Other == "Other - surveillance data", 1, NA),
                                         `Other - test positivity` = ifelse(Other == "Other - test positivity", 1, NA))
use_cov_data1 = use_cov_data1 %>% dplyr::select(-c(Other))
use_cov_data1_long <- gather(use_cov_data1, answer_option, response, "Confirmed cases": "Other - test positivity", factor_key=TRUE)
use_cov_data1_long$response = as.numeric(use_cov_data1_long$response)
use_cov_data1_sum = use_cov_data1_long %>% group_by(answer_option) %>% summarize(count = sum(response, na.rm=TRUE))
use_cov_data1_sum %>% arrange(desc(count))
clean_cov_data = use_cov_data1_long %>% filter(is.na(response)==FALSE) %>% group_by(ID) %>% dplyr::select(ID, answer_option) 
clean_cov_data2 <- data.frame(lapply(clean_cov_data, as.character), stringsAsFactors=FALSE)
clean_cov_data2 = clean_cov_data2 %>% group_by(ID) %>% summarise_all(funs(cov_data = list(.)))

saveRDS(clean_cov_data2, "Data/clean/clean_cov_data.Rdata")






###################### source of covid data

source_cov_data = mobility_metrics %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`,
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.source.of.these.data..Check.all.that.apply.":"X.410" )
names(source_cov_data) = source_cov_data[1,]
source_cov_data = source_cov_data  %>% filter(Response=="Yes")
source_cov_data = source_cov_data %>% rename("ID" = "Open-Ended Response",
                                         "Other" = "Other (please specify)")
source_cov_data = source_cov_data %>% dplyr::select(-c(Response))


source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "104918" | ID == "860233", NA),
                                         `Other - non JHU University` = ifelse(ID == "104918" | ID == "860233", 1, NA))
source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "208592" | ID == "285056" | ID == "116795" | ID == "388998" | ID == "520252" | ID == "567600" | ID == "600066", NA),
                                             `Government/public health report` = replace(`Government/public health report`, ID == "208592" | ID == "285056" | ID == "116795" | ID == "388998" | ID == "520252" | ID == "567600" | ID == "600066", "Government/public health report"))
source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "242863" | ID == "153004" | ID == "807236", NA))
source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "115596" | ID == "202559", NA),
                                             `Non-governmental organization report or repository` = replace(`Non-governmental organization report or repository`, ID == "115596" | ID == "202559", "Non-governmental organization report or repository")) # rt.live; DXY
source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "224674", NA),
                                             `Media including repositories (like The New York Times)` = replace(`Media including repositories (like The New York Times)`, ID == "224674" | ID == "785805", "Media including repositories (like The New York Times)")) # covidtracking - The Atlantic; NY Times github
source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "272890" | ID == "785805", NA),
                                             `Non-governmental organization report or repository` = replace(`Non-governmental organization report or repository`, ID == "272890" | ID == "785805", "Non-governmental organization report or repository")) 
source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "272890" | ID == "315644" | ID == "469208", NA),
                                             `Other - previous studies` = ifelse(ID == "272890" | ID == "315644" | ID == "469208", 1, NA))
source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "407692", NA),
                                             `Government/public health report` = replace(`Government/public health report`, ID == "407692" , NA))
source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "421107", NA),
                                             `Non-governmental organization report or repository` = replace(`Non-governmental organization report or repository`, ID == "421107" , "Non-governmental organization report or repository"),  # ourworld in data, ISARIC
                                             `Government/public health report` = replace(`Government/public health report`, ID == "421107", "Government/public health report")) ## cdc, Official UK Coronavirus Dashboard
source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "498590", NA),
                                             `Government/public health report` = replace(`Government/public health report`, ID == "498590", "Government/public health report"),
                                             `JHU COVID-19 Dashboard/JHU Coronavirus Resource Center` = replace(`JHU COVID-19 Dashboard/JHU Coronavirus Resource Center`, ID == "498590", "JHU COVID-19 Dashboard/JHU Coronavirus Resource Center" ),
                                             `Media including repositories (like The New York Times)` = replace(`Media including repositories (like The New York Times)`, ID == "498590", "Media including repositories (like The New York Times)")) # covidtracking - The Atlantic
source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "608894" | ID == "651386" | ID == "562500" | ID == "681039" | ID == "845317", NA),
                                             `Other - not specified` = ifelse(ID == "608894" | ID == "651386" | ID == "562500" | ID == "681039" | ID == "845317", 1, NA))
source_cov_data = source_cov_data %>% mutate(Other = replace(Other, ID == "883231", NA),
                                             `Media including repositories (like The New York Times)` = replace(`Media including repositories (like The New York Times)`, ID == "883231", "Media including repositories (like The New York Times)"),  # NYT
                                             `Non-governmental organization report or repository` = replace(`Non-governmental organization report or repository`, ID == "883231" , "NA"))

#View(source_cov_data %>% filter(is.na(Other)==FALSE))

source_cov_data[, 2:(10)]<-replace(data.frame(lapply(source_cov_data[, 2:(10)], as.character), stringsAsFactors = FALSE), !is.na(source_cov_data[, 2:(10)]), "1")
source_cov_data[, c(2:10)] <- sapply(source_cov_data[, c(2:10)], as.numeric) ### no "other"
source_cov_data = source_cov_data %>% dplyr::select(-c(Other))
source_cov_data_long <- gather(source_cov_data, answer_option, response, "WHO report": "Other - not specified", factor_key=TRUE)
source_cov_data_long$response = as.numeric(source_cov_data_long$response)
source_cov_data_sum = source_cov_data_long %>% group_by(answer_option) %>% summarize(count = sum(response, na.rm=TRUE))
source_cov_data_sum %>% arrange(desc(count))
clean_source_cov_data = source_cov_data_long %>% filter(is.na(response)==FALSE) %>% group_by(ID) %>% dplyr::select(ID, answer_option) 
clean_source_cov_data2 <- data.frame(lapply(clean_source_cov_data, as.character), stringsAsFactors=FALSE)
clean_source_cov_data2 = clean_source_cov_data2 %>% group_by(ID) %>% summarise_all(funs(source_cov_data = list(.)))

saveRDS(clean_source_cov_data2, "Data/clean/clean_source_cov_data2.Rdat")








################################## author affiliation - first author

first_author_aff = mobility_metrics %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`,
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.category.of.the.affiliation.for.the.first.author.listed...If.there.is.joint.first.authorship..use.the.first.author.listed..If.there.are.multiple.affiliations..check.all.that.apply.":"X.10" )
names(first_author_aff) = first_author_aff[1,]
first_author_aff = first_author_aff  %>% filter(Response=="Yes")
first_author_aff = first_author_aff %>% rename("ID" = "Open-Ended Response",
                                             "Other" = "Other (please specify)")
first_author_aff = first_author_aff %>% dplyr::select(-c(Response))


# clean up other choices
# add academic - physics as an option
first_author_aff = first_author_aff %>% mutate(`Other - Academia - physics` = ifelse(ID == "200360" | ID == "224674", 1, NA),
                                               Other = replace(Other, ID == "200360" | ID == "224674", NA),
                                               `Academia â€“ computer science, engineering, geography` = replace(`Academia â€“ computer science, engineering, geography`, ID == "224674", NA))
# classify academia - public policy as social science
first_author_aff = first_author_aff %>% mutate(Other = replace(Other, ID == "312222", NA))
first_author_aff = first_author_aff %>% mutate(`Other - Academia but not clear what department` = ifelse(ID == "159923" | ID == "841939" | ID == "636456", 1, NA),
                                               Other = replace(Other, ID == "159923" | ID == "841939" | ID == "636456", NA))
# architecture / urban development as its own category
first_author_aff = first_author_aff %>% mutate(`Other - Academia - architecture / urban development` = ifelse(ID == "164025" | ID == "510323" | ID == "725594", 1, NA),
                                               Other = replace(Other, ID == "164025" | ID == "510323" | ID == "725594", NA))
# transportation as its own category
first_author_aff = first_author_aff %>% mutate(`Other - Academia - transportation` = ifelse(ID == "115596" | ID == "205379", 1, NA),
                                               Other = replace(Other, ID == "115596" | ID == "205379", NA))
# NGO
first_author_aff = first_author_aff %>% mutate(Other = replace(Other, ID == "272890", NA),
                                               `Non-government agency â€“ not public health` = replace(`Non-government agency â€“ not public health`, ID == "272890", "Non-government agency â€“ not public health"))
# biology lab
first_author_aff = first_author_aff %>% mutate(Other = replace(Other, ID == "498590", NA),
                                               `Academia â€“ biology, ecology` = replace(`Academia â€“ biology, ecology`, ID == "498590", "Academia â€“ biology, ecology"))
# european commission - demography and migration
first_author_aff = first_author_aff %>% mutate(Other = replace(Other, ID == "955764", NA),
                                               `Government agency â€“ not public health` = replace(`Government agency â€“ not public health`, ID == "955764", "Government agency â€“ not public health"))
# polisci, management
first_author_aff = first_author_aff %>% mutate(Other = replace(Other, ID == "964638" | ID == "518763", NA),
                                               `Academia â€“ business, economics, social science` = replace(`Academia â€“ business, economics, social science`, ID == "964638" | ID == "518763", "Academia â€“ business, economics, social science"))
# environmental science
first_author_aff = first_author_aff %>% mutate(Other = replace(Other, ID == "558038", NA),
                                               `Academia â€“ computer science, engineering, geography` = replace(`Academia â€“ computer science, engineering, geography`, ID == "558038", "Academia â€“ computer science, engineering, geography"))
# agriculture
first_author_aff = first_author_aff %>% mutate(`Other - Academia - agriculture` = ifelse(ID == "568688" , 1, NA),
                                               Other = replace(Other, ID == "568688", NA))
# dept of media and communication (web mining laboratory)
# transportation as its own category
first_author_aff = first_author_aff %>% mutate(`Other - Academia - media and communication` = ifelse(ID == "711982", 1, NA),
                                               Other = replace(Other, ID == "711982", NA),
                                               `Academia â€“ computer science, engineering, geography` = replace(`Academia â€“ computer science, engineering, geography`, ID == "224674", NA))
first_author_aff = first_author_aff %>% mutate(Other = replace(Other, ID == "785805", NA),
                                               `Academia â€“ computer science, engineering, geography` = replace(`Academia â€“ computer science, engineering, geography`, ID == "785805", "Academia â€“ computer science, engineering, geography"))

#View(first_author_aff %>% filter(is.na(Other)==FALSE))



first_author_aff[, 2:ncol(first_author_aff)]<-replace(data.frame(lapply(first_author_aff[, 2:ncol(first_author_aff)], as.character), stringsAsFactors = FALSE), !is.na(first_author_aff[, 2:ncol(first_author_aff)]), "1")
first_author_aff[, c(2:ncol(first_author_aff))] <- sapply(first_author_aff[, c(2:ncol(first_author_aff))], as.numeric) ### no "other"
first_author_aff = first_author_aff %>% dplyr::select(-c(Other))
first_author_aff_long <- gather(first_author_aff, answer_option, response, "Academia â€“ epidemiology, medicine, public health": "Other - Academia - media and communication", factor_key=TRUE)
first_author_aff_long$response = as.numeric(first_author_aff_long$response)
first_author_aff_sum = first_author_aff_long %>% group_by(answer_option) %>% summarize(count = sum(response, na.rm=TRUE))
first_author_aff_sum %>% arrange(desc(count))
clean_first_author_aff = first_author_aff_long %>% filter(is.na(response)==FALSE) %>% group_by(ID) %>% dplyr::select(ID, answer_option) 
clean_first_author_aff2 <- data.frame(lapply(clean_first_author_aff, as.character), stringsAsFactors=FALSE)
clean_first_author_aff2 = clean_first_author_aff2 %>% group_by(ID) %>% summarise_all(funs(first_author_affiliation = list(.)))

saveRDS(clean_first_author_aff2, "Data/clean/clean_first_author_aff2.Rdat")



########################################## last author affiliations


last_author_aff = mobility_metrics %>% 
  dplyr::select(`Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end..`,
                "What.is.the.paper.ID.number..from.the.spreadsheet..", "What.is.the.category.of.the.affiliation.for.the.last.author.listed...If.there.is.joint.last.authorship..use.the.last.author.listed..If.there.are.multiple.affiliations..check.all.that.apply.":"X.21" )
names(last_author_aff) = last_author_aff[1,]
last_author_aff = last_author_aff  %>% filter(Response=="Yes")
last_author_aff = last_author_aff %>% rename("ID" = "Open-Ended Response",
                                               "Other" = "Other (please specify)")
last_author_aff = last_author_aff %>% dplyr::select(-c(Response))


View(last_author_aff %>% filter(is.na(Other)==FALSE))

# transportation as its own category
last_author_aff = last_author_aff %>% mutate(`Other - Academia - transportation` = ifelse(ID == "115596" | ID == "205379", 1, NA),
                                               Other = replace(Other, ID == "115596" | ID == "205379", NA))
# add academic - physics as an option
last_author_aff = last_author_aff %>% mutate(`Other - Academia - physics` = ifelse(ID == "200360" | ID == "224674", 1, NA),
                                               Other = replace(Other, ID == "200360" | ID == "224674", NA),
                                               `Academia â€“ computer science, engineering, geography` = replace(`Academia â€“ computer science, engineering, geography`, ID == "224674", NA))
# architecture / urban development as its own category
last_author_aff = last_author_aff %>% mutate(`Other - Academia - architecture / urban development / geo-informatics / land-surveying` = ifelse(ID == "164025" | ID == "510323", 1, NA),
                                               Other = replace(Other, ID == "164025" | ID == "510323", NA))






##################################################### Make a single clean dataset

clean_nat = merge(clean_mobility_metrics3, clean_spatial_granularity2, by= "ID", all = TRUE)
clean_nat = merge(clean_nat, clean_granularity2, by = "ID", all = TRUE)
clean_nat = merge(clean_nat, clean_availability2, by = "ID", all = TRUE)
clean_nat = merge(clean_nat, clean_identifiability2, by = "ID", all = TRUE)
clean_nat = merge(clean_nat, clean_direct_meta2, by = "ID", all = TRUE)
clean_nat = merge(clean_nat, clean_eco_meta2, by = "ID", all = TRUE)
clean_nat = merge(clean_nat, clean_cov_data2, by = "ID", all = TRUE)
clean_nat = merge(clean_nat, clean_source_cov_data2, by = "ID", all = TRUE)
clean_nat = merge(clean_nat, clean_first_author_aff2, by = "ID", all = TRUE)


# pull clean variables from the dataset and add that into the dataframe
datset = survey_file
datset = datset[2:nn,]
datset = datset %>% rename(ID = What.is.the.paper.ID.number..from.the.spreadsheet..,
                           provide_details_on_pop = Does.the.paper.provide.details.on.the.population.from.which.data.was.collected.,
                           info_sampling = Do.the.authors.provide.information.about.the.process.of.sampling.to.identify.individuals.for.which.mobility.data.is.provided.or.discuss.possible.bias.in.who.was.sampled.or.subscribed.to.the.data.collection.mechanism.,
                           match_mobility_disease = Can.you.match.mobility.patterns.to.an.individual.s.disease.status.)
clean_nat = merge(clean_nat, datset[, c("ID", "provide_details_on_pop", "info_sampling", "match_mobility_disease")], by = "ID", all = TRUE)

saveRDS(clean_nat, "CleanSurveyData_nk_20220114.rds")

##### combine amy and natalya's
# load amy's
clean_amy <- readRDS("CleanSurveyData_ap_20210120.rds")
# load natalya's
clean_nat <- readRDS("Data/clean_2024/clean_nat.Rdata")

# combine them 
clean_all = full_join(clean_amy %>% select(-c(mobility_metrics:dat_identifiability, eco_metadata:first_author_affiliation, provide_details_on_pop:match_mobility_disease)), 
                      clean_nat, by = "ID")

# restrict to those included in full review, with quant data
clean_all <- clean_all %>% filter(full_review == 'Yes' & quant_data == 'Yes') 

# restrict to those with date < june
clean_all <- clean_all |> filter(date < "2021-06-01")

# drop the one paper with not really extracted data
clean_all <- clean_all |> filter(!(ID %in% c("851627")))

# write the file
saveRDS(clean_all, "Data/clean_all.RDS")

# make a file with just the metadata of manuscripts included
raw_data = read.csv("Data/Full paper review assignments.csv")

# just pull article information
meta_data = raw_data |> filter(ID %in% clean_all$ID) |> group_by(ID) |> slice(1) |>
  select(Authors.y, Title, Published.Year, Published.Month, Journal.1, Volume, Issue, Pages, DOI.1, Abstract.y)
# convert date
meta_data = meta_data |> rename(Authors = Authors.y,
                                Journal = Journal.1,
                                DOI = DOI.1,
                                Abstract = Abstract.y)
write.csv(meta_data |> ungroup() |> select(-c(ID)), "Data/meta_data.csv")


