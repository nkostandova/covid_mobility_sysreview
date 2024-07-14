library(dplyr)
library(ggplot2)
library(ggsci)
library(flextable)
library(viridis)
library(gtsummary)
library(tidyr)
library(stringr)
library(RColorBrewer)

# load data
clean_all = readRDS("Data/clean_all.RDS")
### get span of dates published:
summary(clean_all$date)

###### do a bit of additional cleaning
# clean number of data sets
clean_numb_data_sets <- function(n_mob_data_sets){
  n_mob_data_sets <- replace(n_mob_data_sets, n_mob_data_sets %in% c('two'), 2)
  n_mob_data_sets <- replace(n_mob_data_sets, n_mob_data_sets %in% c('three'), 3)
  n_mob_data_sets <- replace(n_mob_data_sets, n_mob_data_sets %in% c("Eleven (11)"), 11)
  return(n_mob_data_sets)
}

clean_all$n_mob_data_sets = clean_numb_data_sets(clean_all$n_mob_data_sets)
table(clean_all$n_mob_data_sets)



######################### Author affiliations

##### first author
# first author - make a table
first_auth = clean_all |> select(ID, first_author_aff) |> unnest(first_author_aff) # 260 instances
first_auth_sum = first_auth |> group_by(first_author_aff) |> summarize(count = n())
first_auth_sum = first_auth_sum |> mutate(Percent = round(count / length(unique(first_auth$ID))*100)) |>
  arrange(desc(count))
first_auth_sum |>
  flextable() %>% autofit() %>% save_as_docx(path = "tables/jan2024/first_author_aff.docx")


# # figure by date
# ggplot(clean_all |> select(date, first_author_aff) |> unnest(first_author_aff), 
#        aes(x = date, fill = first_author_aff)) +
#   geom_histogram() + theme_bw() + 
#   #scale_fill_nejm(name="First author affiliation")+
#   xlab("Date") + ylab("Count") +
#   scale_fill_brewer(palette = "Set3", name = "First author affiliation")+
#   theme(text = element_text(size=24)) + theme(legend.position = "none")

first_auth_date = clean_all |> select(date, first_author_aff) |> unnest(first_author_aff) 
first_auth_date = first_auth_date |> mutate(first_author_aff = case_when(
  first_author_aff %in% c("Non-government agency - public health") ~ "Non-government agency - public health",
  first_author_aff %in% c("Non-government agency – not public health") ~ "Non-government agency - not public health",
  TRUE ~ first_author_aff
))
# Fig S2A
first_auth_date |>
  ungroup() |> group_by(date, first_author_aff) |> summarize(count = n()) |>
ggplot(aes(fill = first_author_aff, y = count, x = date ))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "First author affiliation") +
  scale_fill_brewer(palette = "Set3", name = "First author affiliation")+
  xlab("Date") + ylab("Count") + 
  theme(legend.position = "none")+
  theme(text = element_text(size=24))

##### last author
# last author - make a table
last_auth = clean_all |> select(ID, last_author_aff) |> unnest(last_author_aff) # 260 instances
last_auth_sum = last_auth |> group_by(last_author_aff) |> summarize(count = n())
last_auth_sum = last_auth_sum |> mutate(Percent = round(count / length(unique(last_auth$ID))*100)) |>
  arrange(desc(count))
last_auth_sum |>
  flextable() %>% autofit() %>% save_as_docx(path = "tables/jan2024/last_author_aff.docx")

# rename a couple
last_auth = clean_all |> select(date, last_author_aff) |> unnest(last_author_aff) 
last_auth = last_auth |> mutate(last_author_v2 = case_when(
  last_author_aff %in% c("Other - Academia") ~ "Academia - Other",
  last_author_aff %in% c("Non-government agency – public health") ~ "Non-governmental agency - public health",
  last_author_aff %in% c("Non-government agency - not public health") ~ "Non-governmental agency - not public health",
  TRUE ~ last_author_aff
))



# Fig S2B
# align the levels to match previous one
last_auth$last_author_v2 = factor(last_auth$last_author_v2,
                                  levels = c("Academia - Other", "Academia – biology, ecology", "Academia – business, economics, social science",
                                             "Academia – computer science, engineering, geography", "Academia – epidemiology, medicine, public health", "Academia – statistics, biostatistics, mathematics",
                                             "Government agency – not public health", "Government agency – public health", "Non-governmental agency - not public health",
                                             "Non-governmental agency - public health", "Point of care (medical institution/clinical/hospital)", "Other"))
last_auth |>
  ungroup() |> group_by(date, last_author_v2) |> summarize(count = n()) |>
  ggplot(aes(fill = last_author_v2, y = count, x = date ))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Last author affiliation") +
  scale_fill_brewer(palette = "Set3", name = "Last author affiliation")+
  xlab("Date") + ylab("Count") + 
  theme(legend.position = "none")+
  theme(text = element_text(size=24))

#### public health affiliation
table(clean_all$author_aff_ph)

ggplot(clean_all |> select(date, author_aff_ph) |> unnest(author_aff_ph) |>
         group_by(date, author_aff_ph) |> summarize(count = n()), 
       aes(x = date, fill = author_aff_ph, y = count)) +
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  #scale_fill_aaas(name="Public health affiliation")+
  scale_fill_grey(name = "Public health affiliation") +
  xlab("Date") + ylab("Count") +
  #scale_fill_discrete(name = "Public health affiliation")+
  theme(text = element_text(size=24)) #+ theme(legend.position = "none")


########### Area of focus
a_focus = clean_all %>% 
  group_by(date, loc_focus) |> mutate(loc_focus = stringr::str_to_sentence(loc_focus)) |> summarize(count = n()) 
a_focus = a_focus |> mutate(loc_focus_fac = case_when(
  loc_focus %in% c("Import") ~ "National (international\nimportation)",
  loc_focus %in% c("Theory") ~ "Theoretical",
  TRUE ~ loc_focus
))
# specify palette
nejm_p = pal_nejm()(8)
## Figure 1A
a_focus |>  ggplot(aes(x = date, fill = loc_focus_fac, y = count))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Geographical focus") +
  scale_fill_manual(values = c(nejm_p[1], nejm_p[3], nejm_p[2], pal_nejm(alpha = 0.6)(2)[2], nejm_p[4])) + 
  xlab("Publication month") + ylab("Count") + 
  #theme(legend.position = "none")+
  theme(text = element_text(size=24))



# test for change over time 
temp = clean_all %>% select(date, ID, loc_focus) |>
  group_by(date, loc_focus) |> mutate(loc_focus = stringr::str_to_sentence(loc_focus))
temp = temp |> group_by(date) |> mutate(num_papers = length(unique(ID)))
temp = temp |> ungroup() |> group_by(date, loc_focus) |> summarize(count = n(),
                                                                           num_papers = min(num_papers))
# make indicator variables
temp = temp |> mutate(ind_glob = ifelse(grepl("Global", loc_focus), 1, 0),
                      ind_import = ifelse(grepl("Import", loc_focus), 1, 0),
                      ind_multi = ifelse(grepl("Multi", loc_focus),1,0),
                      ind_nat = ifelse(grepl("National", loc_focus), 1, 0),
                      ind_theory = ifelse(grepl("Theory", loc_focus), 1, 0))
mod = glm(count ~ date + ind_glob + ind_import + ind_multi + ind_nat + ind_theory, offset = num_papers, data = temp, family = "poisson")
summary(mod)
# save output
geo_focus = broom::tidy(mod) |> filter(term %in% c("date"))
geo_focus = geo_focus |> mutate(descr = "Geographic focus vs time",
                                     categories = paste(toString(unique(temp$loc_focus)), sep=","))

# try instead doing multinomial regression
temp2 = clean_all %>% select(date, ID, loc_focus) |>
  group_by(date, loc_focus) |> mutate(loc_focus = stringr::str_to_sentence(loc_focus))
library(nnet)
library(car)
test <- multinom(loc_focus ~ date, data = temp2)
summary(test)
# check for significance
Anova(test, type = "II")


library(vcd)
# Chi-sq test for trend
# Create a contingency table
tbl <- table(temp2$date, temp2$loc_focus)

# Run the Chi-square test for trend
chisq.test(tbl) # p-value = 0.0257

########################## Countries of focus - get number of LMIC countries - using World Bank classification
# note: Korea = Republic of Korea
country_foc = clean_all %>% filter(loc_focus %in% c("national"))
table(country_foc$country_focus, useNA="always")
table(clean_all$loc_focus)
country_foc = country_foc %>% mutate(lmic = ifelse(country_focus %in% c("India", "Nigeria"), 1, 0))
table(country_foc$lmic)


###### now look at multi-country analysis
multi_coun_foc = clean_all %>% filter(loc_focus %in% c("multi-country"))
table(multi_coun_foc$country_list)
table(multi_coun_foc$continent_list)
View(multi_coun_foc %>% select(country_list, continent_list))




###### primary goal of paper
prim_goal = data.frame(clean_all$primary_goal_of_paper)
id_frame = data.frame(clean_all$ID)
prim_goal = cbind(id_frame, prim_goal)
# replace X.38 entries with "Other"
prim_goal = prim_goal |> mutate(Other = ifelse(is.na(X.38)==FALSE & `X.38`!="", "Other", NA))
prim_goal = prim_goal |> select(-c(X.38))
prim_goal_long = prim_goal |> ungroup() 
names(prim_goal_long)[1] = c("ID")
prim_goal_long = prim_goal_long |> pivot_longer(cols = -c(ID), names_to = "var", values_to = "primary_g")
prim_goal_long = mutate_all(prim_goal_long, as.character) %>%
  filter(is.na(primary_g)==FALSE & primary_g !="")
prim_goal_long = prim_goal_long %>% 
  group_by(ID) %>% summarize(primary_goal = list(primary_g))
# merge back
clean_all = left_join(clean_all, prim_goal_long, by = join_by(ID))
table(unlist(clean_all$primary_goal))



### check primary goal for multinational countries
prim_multi = clean_all |> select(ID, primary_goal, loc_focus, country_list, continent_list) |> filter(loc_focus %in% "multi-country")
# combine country and continent list
prim_multi = prim_multi |> mutate(country_list = ifelse(is.na(country_list) | country_list == "", continent_list, country_list))
prim_multi = prim_multi |> select(-c(continent_list))
table(prim_multi$country_list)
# make continent lists- using https://worldpopulationreview.com/country-rankings/list-of-countries-by-continent 
africa_countries = c("Algeria|Angola|Benin|Botswana|Burkina Faso|Burundi|Cape Verde|Cameroon|Central African Republic|Chad|Comoros|Congo|Djibouti|Egypt|Guinea|Eritrea|Eswatini|Swaziland|Ethiopia|Gabon|Gambia|Ghana|Ivory Coast|Kenya|Lesotho|Liberia|Libya|Madagascar|Malawi|Mali|Mauritania|Mauritius|Morocco|Mozambique|Namibia|Niger|Rwanda|Sao Tome|Senegal|Seychelles|Sierra Leone|Somalia|Sudan|Tanzania|Togo|Tunisia|Uganda|Zambia|Zimbabwe|Africa")
#mena_countries = c("Egypt|Iran|Saudi Arabia|United Arab Emirates|Israel|Jordan|Kuwait|Morocco|Iraq|Oman|Qatar|Tunisia|Algeria|Bahrain|Yemen|Libya|Lebanon|Syria|Djibouti|Palestine|Sudan|Western Sahara")
asia_countries = c("Afghanistan|Armenia|Azerbaijan|Bahrain|Bangladesh|Bhutan|British Indian Ocean|Brunei|Cambodia|China|Georgia|Hong Kong|India|Indonesia|Iran|Iraq|Israel|Japan|Jordan|Kazakhsatan|Kuwait|Kyrgyzstan|Laos|Lebanon|Macau|Malaysia|Maldives|Mongolia|Myanmar|Burma|Nepal|Korea|Oman|Pakistan|Palestine|Philippines|Qatar|Saudi Arabia|Singapore|Sri Lanka|Syria|Taiwan|Tajikistan|Thailand|Timor|Turkmenistan|Emirates|Uzbekistan|Vietnam|Yemen|Asia")
europe_countries = c("Albania|Andorra|Austria|Belarus|Belgium|Bosnia|Herzegovina|Bulgaria|Croatia|Cyprus|Czhech|Denmark|Estonia|Finland|France|Germany|Greece|Hungary|Iceland|Ireland|Italy|Latvia|Liechtenstein|Lithuania|Luxembourg|Malta|Moldova|Monaco|Montenegro|Netherlands|North Macedonia|Norway|Poland|Portugal|Romania|Russia|San Marino|Serbia|Slovakia|Slovenia|Spain|Sweden|Switzerland|Turkey|Ukraine|United Kingdom|UK|Vatican|Europe")
northam_countries = c("Antigua|Bahamas|BArbados|Belize|Canada|Costa Rica|Cuba|Dominic|El Salvador|Grenada|Guatemala|Haiti|Honduras|Jamaica|Mexico|Nicaragua|Panama|Saint Kitts|Saint Lucia|Saint Vincent|Trinidad|USA|United States of America")
oceania_countries = c("Australia|Fiji|Kiribati|Marshall|Micronesia|Nauru|New Zealand|Palau|Papua New Guinea|Samoa|Solomon|Tonga|Tuvalu|Vanuatu")
southam_countries = c("Argentina|Bolivia|Brazil|Chile|Colombia|Ecuador|Guyana|Paraguay|Peru|Suriname|Uruguay|Venezuela")
prim_multi = prim_multi |> mutate(Africa = ifelse(grepl(africa_countries, country_list), 1, 0),
                                  Asia = ifelse(grepl(asia_countries, country_list), 1, 0),
                                  Europe = ifelse(grepl(europe_countries, country_list), 1, 0),
                                  NorthAm = ifelse(grepl(northam_countries, country_list), 1, 0),
                                  Oceania = ifelse(grepl(oceania_countries, country_list), 1, 0),
                                  SouthAm = ifelse(grepl(southam_countries, country_list), 1, 0))
prim_multi = prim_multi |> rowwise() |> mutate(total_cont = sum(Africa, Asia, Europe, NorthAm, Oceania, SouthAm))
sum(prim_multi$Africa)
sum(prim_multi$Asia)
sum(prim_multi$Europe)
sum(prim_multi$NorthAm)
sum(prim_multi$Oceania)
sum(prim_multi$SouthAm)

# check for primary goal
prim_multi_long = prim_multi |> unnest(primary_goal)

########################### time period analyzed and geographical focus
clean_all$end_date = as.Date(clean_all$end_date)
ggplot(clean_all, aes(x = end_date, fill = loc_focus)) +
  geom_histogram() + theme_bw()
# plot country by end date
ggplot(clean_all %>% filter(loc_focus %in% c("national")), aes(x = end_date, fill = country_focus)) +
  geom_histogram() + theme_bw() + 
  #scale_fill_aaas(name="Country")+
  xlab("End date") + ylab("Studies") + ggtitle("Country of focus in national / sub-national studies, by end date of analysis dataset")


# country by date
# keep top 10 countries only
country_top10 = country_foc |> group_by(country_focus) |> summarize(count = n()) |> arrange(desc(count)) |> slice(1:10)
country_top8 = country_top10 |> slice(1:8)
# Figure 1B
temp_countries = clean_all |> filter(loc_focus %in% c("national"))
# set it up so countries are either in the top 8 or classified as "other"
temp_countries = temp_countries |> mutate(country_focus_wother = case_when(
  country_focus %in% country_top8$country_focus ~ country_focus,
  is.na(country_focus) ~ NA,
  TRUE ~ "Other"
))
con_plot = temp_countries  |>
  group_by(date, country_focus_wother) |> summarize(count = n()) 
# set levels in order
con_plot$country_focus_wother = factor(con_plot$country_focus_wother, 
                                       levels = c(unique(country_top8$country_focus), "Other"))
con_plot |>
  ggplot(aes(x = date, fill = country_focus_wother, y = count))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Country of focus") +
  #scale_x_date(expand = c(1/12, 0))+  #scale_fill_brewer(palette = "Set3")+
  #scale_fill_nejm() + 
  scale_x_date(limits = as.Date(c("2020-01-01",  "2021-05-01")))+
  theme(legend.position = "none")+
  scale_fill_manual(values = c(pal_nejm()(8)[3], pal_nejm()(8)[2], pal_nejm()(8)[5], pal_nejm()(8)[1], 
                               pal_nejm()(8)[8], pal_nejm()(8)[4], pal_nejm()(8)[7], pal_nejm()(8)[6],
                    "azure")) + 
    xlab("Publication month") + ylab("Count") + 
  theme(text = element_text(size=24))



##################### mobility metrics
############ look at the time paper published and type of mobility data used
## variables: date, mobility_metrics
# create new dataframe that unlists mobility_metrics and merges the date
mob_data_dates = clean_all %>% select(date, contains("mobility_metrics"))
# for this one, we can unnest all of the datasets separately
mob_data_dates1 = mob_data_dates %>% select(ID, date, mobility_metrics_1) |> unnest_longer(mobility_metrics_1) |> rename(mobility_metrics = mobility_metrics_1)
mob_data_dates2 = mob_data_dates %>% select(ID, date, mobility_metrics_2) |> unnest_longer(mobility_metrics_2)|> rename(mobility_metrics = mobility_metrics_2)
mob_data_dates3 = mob_data_dates %>% select(ID, date, mobility_metrics_3) |> unnest_longer(mobility_metrics_3)|> rename(mobility_metrics = mobility_metrics_3)
mob_data_dates4 = mob_data_dates %>% select(ID, date, mobility_metrics_4) |> unnest_longer(mobility_metrics_4)|> rename(mobility_metrics = mobility_metrics_4)
mob_data_dates_all = rbind(mob_data_dates1, mob_data_dates2, mob_data_dates3, mob_data_dates4)

# make a factor variable so that "Other" is in the bottom of the plot
mob_data_dates_all = mob_data_dates_all |> 
  mutate(mobility_metrics_fac =factor(mobility_metrics,
                                      levels = c("Trip counts between an origin and destination",
                                                 "Time spent at particular locations (like businesses)",
                                                 "Traffic flows",
                                                 "Time spent at home/residence",
                                                 "Clustering of individuals at a location",
                                                 "Migration",
                                                 "Proportion of time spent away from home",
                                                 "Other")))

### Figure: mobility metrics by date
## Figure 2A
# make a manual palette so "Other" is the same everywhere
fig2a_pal = c(pal_jco()(7), "azure")
mob_data_dates_all %>% group_by(date, mobility_metrics_fac) %>% 
  filter(is.na(mobility_metrics_fac)==FALSE) |>
  summarize(count = n()) %>% 
  ggplot(aes(fill = mobility_metrics_fac, y = count, x = date ))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Mobility metric") +
  #scale_fill_jco()+
  xlab("Date") + ylab("Count") + 
  theme(text = element_text(size=24))+
  scale_fill_jco(labels = function(x) str_wrap(x, width = 25)) +
  scale_fill_manual(values = fig2a_pal, labels = function(x) str_wrap(x, width = 25)) + 
  theme(legend.position = "none")+
  theme(legend.spacing.y = unit(0.25, 'cm')) +
  guides(fill = guide_legend(byrow=TRUE)) 



### Table: mobility metrics (frequency only)
# first - just the straight up table of frequencies
mob_data_dates_all = mob_data_dates_all |> filter(is.na(mobility_metrics)==FALSE)
table(mob_data_dates_all$mobility_metrics, useNA = "ifany")
length(unique(mob_data_dates_all$ID)) ## number of papers = 227
nrow(mob_data_dates_all)  ### number of instances - 331
# make a table 
as.data.frame(table(mob_data_dates_all$mobility_metrics)) %>% arrange(desc(Freq)) |>
  mutate(Percent = paste0(round(Freq/sum(Freq)*100), "%")) %>%
  flextable() |> save_as_docx(path = "tables/jan2024/mob_metrics.docx")






###### Spatial scale
## first, get an updated table of frequencies
spat_scale = clean_all |> select(ID, contains("spatial_scale")) 
spta_scale_long = spat_scale |> pivot_longer(cols = -c(ID), names_to = "dataset", values_to = "spatial_scale")
spta_scale_long = spta_scale_long |> filter(is.na(spatial_scale)==FALSE & spatial_scale !="")
length(unique(spta_scale_long$ID)) ## number of papers = 231
nrow(spta_scale_long)  ### number of instances - 288
# replace "Other - specify" with "Other
spta_scale_long = spta_scale_long |> mutate(spatial_scale = ifelse(grepl("Other", spatial_scale), "Other", spatial_scale))

# make a table 
spta_scale_long |> ungroup() |> group_by(spatial_scale) |> summarize(count = n()) |> 
  arrange(desc(count)) |> mutate(Percent = paste0(round(count / sum(count)*100), "%")) |>
  flextable() |> save_as_docx(path = "tables/jan2024/spatscale.docx")




## Figure - by date
spat_scale = clean_all |> select(ID, date, contains("spatial_scale")) 
spta_scale_long = spat_scale |> pivot_longer(cols = -c(ID, date), names_to = "dataset", values_to = "spatial_scale")
spta_scale_long = spta_scale_long |> filter(is.na(spatial_scale)==FALSE & spatial_scale !="")
# replace "Other - specify" with "Other
spta_scale_long = spta_scale_long |> mutate(spatial_scale = ifelse(spatial_scale %in% c("Other (please specify)"), "Other", spatial_scale))
spta_scale_long = spta_scale_long |> mutate(spatial_scale = ifelse(spatial_scale %in% c("Subnational - smaller than city/village/metro area (e.g. zipcode, census block)"), "Subnational - smaller than city/village/metro area", spatial_scale))
spta_scale_long = spta_scale_long |> mutate(spatial_scale_rev = ifelse(grepl("Subnational", spatial_scale), "Subnational", spatial_scale))
spta_scale_long = spta_scale_long |> mutate(spatial_scale_rev = ifelse(grepl("Other", spatial_scale), "Other", spatial_scale_rev))

# only one type per dataset
spta_scale_long_rev = spta_scale_long |> ungroup() |> group_by(ID, dataset, spatial_scale_rev) |> slice(1)
# arrange so Other is at the end
spta_scale_long_rev = spta_scale_long_rev |> mutate(spatial_scale_rev = factor(spatial_scale_rev,
                                                                               levels = c("Multinational/international", "National", "Subnational", "Grid cells",  "Longitude/latitude coordinates", "Airport to airport", "Other")))

### Figure: spatial by date
spta_scale_long_rev %>% group_by(date, spatial_scale_rev) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(fill = spatial_scale_rev, y = count, x = date ))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Spatial scale") +
  #scale_fill_brewer(palette = "Set2") + 
  scale_fill_tron()+
  #theme(legend.position = "bottom")+
  xlab("Date") + ylab("Count") + 
  theme(text = element_text(size=24))


### Now just make the plot for among subnational ones
spta_subnat = spta_scale_long |> filter(spatial_scale_rev %in% "Subnational")
spta_subnat %>% group_by(date, spatial_scale) %>% 
  summarize(count = n()) %>% 
  ungroup() |> group_by(date) |> mutate(total_date = sum(count),
                                        count_prop = count / total_date) |>
  mutate(spat_scale_sub = gsub("Subnational - ", "", spatial_scale)) |>
  ggplot(aes(fill = spat_scale_sub, y = count_prop, x = date , width = total_date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Spatial scale") +
  #scale_fill_brewer(palette = "Set2") + 
  scale_fill_tron()+
  theme(legend.position = "bottom")+
  xlab("Date") + ylab("Count") + 
  theme(text = element_text(size=20))




#### Figure: spatial by date, normalized
spat_norm = spta_scale_long |> ungroup() |> group_by(date, spatial_scale_rev) |> 
  summarize(count = n())
spat_norm = spat_norm |> ungroup() |> group_by(date) |> mutate(total_count = sum(count))
spat_norm = spat_norm |> ungroup() |> mutate(count_norm = count / total_count)

spat_norm %>% 
  ggplot(aes(fill = spatial_scale_rev, y = count_norm, x = date, width = total_count))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Spatial scale") +
  scale_fill_tron()+
  theme(legend.position = "bottom")+
  xlab("Date") + ylab("Count") + 
  theme(text = element_text(size=20))


##### now extract quarters --- for all levels
spta_scale_long = spta_scale_long |> mutate(spatial_scale_rev = ifelse(spatial_scale_rev %in% c("Airport to airport", "Longitude/latitude coordinates"), "Grid cells", spatial_scale_rev))
spta_scale_long = spta_scale_long |> mutate(spatial_scale_rev = ifelse(spatial_scale_rev %in% c("Grid cells"), "Grid cells / coordinates", spatial_scale_rev))
spta_scale_long = spta_scale_long |> mutate(quarter = quarters(date), yr = lubridate::year(date))
spta_scale_long = spta_scale_long |> mutate(new_date= paste0(yr, " ", quarter))
spta_scale_long_q = spta_scale_long |> ungroup() |> group_by(new_date, spatial_scale_rev) |> 
  summarize(count = n())

spta_scale_long_q %>% 
  ggplot(aes(fill = spatial_scale_rev, y = count, x = new_date ))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Spatial scale") +
  #scale_fill_brewer(palette = "Set2") + 
  scale_fill_tron()+
  #theme(legend.position = "bottom")+
  xlab("Date") + ylab("Count") + 
  theme(text = element_text(size=20))



### Now just make the plot for among subnational ones
spta_subnat2 = spta_scale_long |> filter(spatial_scale_rev %in% "Subnational")
spta_subnat2 %>% group_by(new_date, spatial_scale) %>% 
  summarize(count = n()) %>% 
  ungroup() |> group_by(new_date) |> mutate(total_date = sum(count),
                                        count_prop = count / total_date) |>
  mutate(spat_scale_sub = gsub("Subnational - ", "", spatial_scale)) |>
  ggplot(aes(fill = spat_scale_sub, y = count_prop, x = new_date , width = total_date/40))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Spatial scale") +
  #scale_fill_brewer(palette = "Set2") + 
  scale_fill_tron()+
  theme(legend.position = "bottom")+
  xlab("Date") + ylab("Count") + 
  theme(text = element_text(size=24))





### Figure: proportion plot for top ten locations
spta_scale_long = left_join(spta_scale_long, clean_all |> select(ID, country_focus), by = join_by(ID))
spat_location = spta_scale_long |> filter(country_focus != "") |>
  group_by(country_focus, spatial_scale) |> summarize(count = n())
spat_location = spat_location |> ungroup() |> group_by(country_focus) |> mutate(total_country = sum(count))
# restrict to top ten countries
spat_totals = spat_location |> ungroup() |> group_by(country_focus) |> summarize(total_country = sum(count))
spat_totals = spat_totals |> arrange(desc(total_country))
spat_totals = spat_totals |> slice(1:10)

# normalize data
spat_location = spat_location |> rowwise() |> mutate(count_norm = count / total_country)
spat_totals = spat_totals |> mutate(prop_tot = total_country / sum(total_country))
spat_location = left_join(spat_location, spat_totals %>% select(country_focus, prop_tot), by = join_by(country_focus))

ggplot(spat_location |> filter(country_focus %in% spat_totals$country_focus), 
       aes(fill = spatial_scale, y = count_norm, x = country_focus, width = prop_tot*4))+
  geom_bar(position = "stack", stat = "identity", color = "black", width = 0.95) + theme_bw() + 
  labs(fill = "Spatial scale") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=20)) + xlab("Country of focus") + ylab("Proportion datasets")




##### Unnest types of mobility
type_mobility <- clean_all |> select(ID, mp_mobility) |> unnest_wider(mp_mobility)
# make a longer dataframe so that it keeps track of type by dataset
type_mobility_longer = type_mobility |> pivot_longer(cols = -c(ID), names_to = "dataset", values_to = "mobility_type")
type_mobility_longer = type_mobility_longer |> mutate(dataset = gsub("mp_mobility_", "", dataset))
type_mobility_longer = type_mobility_longer |> filter(is.na(mobility_type)==FALSE & mobility_type !="")

# Recode mobility variable to have less categories
type_mobility_longer = type_mobility_longer |> mutate(mobility_type_rec = case_when(
  grepl("Mobile phone", mobility_type) ~ "Mobile phone",
  mobility_type %in% c("Travel History") ~ "Survey data",
  mobility_type %in% c("transport for London ") ~ "Traffic data including train, road use",
  mobility_type %in% c("GPS data from electric bikes", "https://heat.qq.com/", "news article ", "Stringency Index (on governmental response)", "UMaryland", "Wifi log data") ~ "Other",
  grepl("Google", mobility_type) ~ "Google (app or CMR)",
  mobility_type %in% c("Not specified") ~ NA,
  TRUE ~ mobility_type))

### Plot of mobility type by date
# join date
type_mobility_longer = left_join(type_mobility_longer, clean_all |> select(date, ID), by = join_by(ID))

# Additional categorization
type_mobility_longer = type_mobility_longer |> mutate(mobility_type_rec2 = case_when(
  mobility_type_rec %in% c("Airline/flight data", "Traffic data including train, road use") ~ "Flight / traffic data",
  mobility_type_rec %in% c("Apple App Data", "Facebook data", "Google (app or CMR)", "Twitter") ~ "Apple, Facebook, Google, Twitter",
  #mobility_type_rec %in% c("Baidu", "Mobile phone") ~ "Mobile phone / Baidu",
  mobility_type_rec %in% c("Tourism", "Survey data") ~ "Survey data / tourism",
  TRUE ~ mobility_type_rec
))
# restrict to a single occurrence per type per ID
#type_mobility_longer2 = type_mobility_longer |> ungroup() |> group_by(ID, dataset, mobility_type_rec2) |> slice(1) # same amount

# get counts by date and normalize
type_data_date = type_mobility_longer |> filter(is.na(mobility_type_rec2)==FALSE) |> group_by(date, mobility_type_rec2) |> summarize(count = n())
type_data_date = type_data_date |> ungroup() |> group_by(date) |> mutate(total_date = sum(count))
type_data_date = type_data_date |> ungroup()  |> rowwise() |> mutate(count_norm = count / total_date)
##geomtile didn't work as desired
#ggplot(type_data_date, aes(x = date, y = mobility_type_rec, fill = mobility_type_rec)) + 
#  geom_tile(aes(height = count_norm, width = total_date)) + 
#  theme(text = element_text(size=20)) + xlab("Date") + ylab("")
ggplot(type_data_date |> filter(is.na(mobility_type_rec2)==FALSE), 
       aes(fill = mobility_type_rec2, y = count_norm, x = date, width = total_date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  scale_fill_brewer(palette = "Dark2") +
  #theme(legend.position = "bottom") + 
  labs(fill = "Type of mobility data") +# theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Proportion datasets")

# make factors so that Other is on the bottom
type_data_date$mobility_type_rec2 = factor(type_data_date$mobility_type_rec2,
                                                 levels = c("Apple, Facebook, Google, Twitter",
                                                            "Baidu", "Census/migration", "Flight / traffic data",
                                                            "Mobile phone", "Survey data / tourism", "Other"))
# Figure 1D
# not normalized
ggplot(type_data_date |> filter(is.na(mobility_type_rec2)==FALSE), 
       aes(fill = mobility_type_rec2, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_jco() + 
  #theme(legend.position = "none") + 
  labs(fill = "Type of mobility data") + #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Count")

# make factors so that Other is on the bottom
type_mobility_longer$mobility_type_rec2 = factor(type_mobility_longer$mobility_type_rec2,
                                                 levels = c("Apple, Facebook, Google, Twitter",
                                                            "Baidu", "Census/migration", "Flight / traffic data",
                                                            "Mobile phone", "Survey data / tourism", "Other"))


# get number of papers for each type
type_mobility_longer |> ungroup() |> group_by(ID, mobility_type_rec3) |> slice(1) |>
  ungroup() |> group_by(mobility_type_rec3) |> summarize(count = n()) |>
  flextable()



# check mobility type and country / lmic
check_type_country = left_join(type_mobility_longer, clean_all |> select(ID, country_focus),
                               by = join_by(ID))
table(check_type_country$country_focus, check_type_country$mobility_type)


#### test type of mobility over time
temp = type_mobility_longer |> select(ID, date, mobility_type_rec3)
# combine census, survey, and other
temp = temp |> mutate(mobility_type_rec3 = case_when(
  grepl("Census", mobility_type_rec3) ~ "Other",
  grepl("Survey", mobility_type_rec3) ~ "Other",
  TRUE ~ mobility_type_rec3
))
# keep a single one of each type for each paper
temp = temp |> group_by(ID, mobility_type_rec3) |> slice(1)
temp = temp |> filter(is.na(mobility_type_rec3)==FALSE) |> ungroup() |> group_by(date) |> mutate(num_papers = length(unique(ID)))
temp = temp |> ungroup() |> group_by(date, mobility_type_rec3) |> summarize(count = n(),
                                                                    num_papers = min(num_papers))
# make indicator variables
temp = temp |> mutate(ind_apple = ifelse(grepl("Apple", mobility_type_rec3), 1, 0),
                      ind_baidu = ifelse(grepl("Baidu", mobility_type_rec3), 1, 0),
                      ind_flight = ifelse(grepl("Flight", mobility_type_rec3), 1, 0),
                      ind_cdr = ifelse(grepl("Mobile", mobility_type_rec3), 1, 0),
                      ind_other = ifelse(grepl("Other", mobility_type_rec3), 1, 0)
                    )
mod = glm(count ~ date + ind_apple+ ind_baidu+ind_flight+ind_cdr, offset = num_papers, data = temp, family = "poisson") # 5 categories
summary(mod)
# save output
type_time = broom::tidy(mod) |> filter(term %in% c("date"))
type_time = type_time |> mutate(descr = "Type of mobility data vs time",
                                categories = paste(toString(unique(temp$mobility_type_rec3)), sep=","))

temp2 = type_mobility_longer |> select(ID, date, mobility_type_rec3)
# combine census, survey, and other
temp2 = temp2 |> mutate(mobility_type_rec3 = case_when(
  grepl("Census", mobility_type_rec3) ~ "Other",
  grepl("Survey", mobility_type_rec3) ~ "Other",
  TRUE ~ mobility_type_rec3
))
# keep a single one of each type for each paper
temp2 = temp2 |> group_by(ID, mobility_type_rec3) |> slice(1)

test <- multinom(mobility_type_rec3 ~ date, data = temp2)
summary(test)
# check for significance
Anova(test, type = "II")


### statistical test for type of mobility data and area of focus
# combine census, survey, and other
type_mobility_longer_col = type_mobility_longer |> mutate(mobility_type_rec3 = case_when(
    grepl("Census", mobility_type_rec3) ~ "Other",
    grepl("Survey", mobility_type_rec3) ~ "Other",
    TRUE ~ mobility_type_rec3
  ))
# keep a single one of each type for each paper
type_mobility_longer_col = type_mobility_longer_col |> group_by(ID, mobility_type_rec3) |> slice(1)
type_focus = left_join(type_mobility_longer_col |> select(ID, mobility_type_rec3), clean_all |> select(ID, loc_focus), by = join_by(ID) )
type_focus = type_focus |> mutate(loc_focus = stringr::str_to_sentence(loc_focus)) |> mutate(loc_focus_fac = case_when(
  loc_focus %in% c("Import") ~ "National (international\nimportation)",
  loc_focus %in% c("Theory") ~ "Theoretical",
  TRUE ~ loc_focus
))
#type_focus = type_focus %>% 
#  group_by(mobility_type_rec3, loc_focus) |> summarize(count = n()) 
# drop theoretical
type_focus = type_focus |> filter(loc_focus_fac != "Theoretical")
type_focus_tab = table(type_focus$loc_focus_fac, type_focus$mobility_type_rec3)
chi_type_focus = chisq.test(type_focus_tab)
chi_type_focus = broom::tidy(chi_type_focus)
table(type_focus$mobility_type_rec3)
table(type_focus$loc_focus_fac)

type_focus |> group_by(mobility_type_rec3, loc_focus_fac) |> summarize(count = n()) |> ungroup() |> group_by(mobility_type_rec3) |> mutate(percent = count / sum(count))




### Get mobility metrics by data type
#### first combine mobility metrics and data type 
mob_met1 = clean_all |> select(ID, contains("mobility_metrics")[1]) |> unnest(cols = mobility_metrics_1) |> 
  rename(mobility_metric = mobility_metrics_1) |> mutate(dataset = 1)
mob_met2 = clean_all |> select(ID, contains("mobility_metrics")[2]) |> unnest(cols = mobility_metrics_2) |> 
  rename(mobility_metric = mobility_metrics_2) |> mutate(dataset = 2)
mob_met3 = clean_all |> select(ID, contains("mobility_metrics")[3]) |> unnest(cols = mobility_metrics_3) |> 
  rename(mobility_metric = mobility_metrics_3) |> mutate(dataset = 3)
mob_met4 = clean_all |> select(ID, contains("mobility_metrics")[4]) |> unnest(cols = mobility_metrics_4) |> 
  rename(mobility_metric = mobility_metrics_4) |> mutate(dataset = 4)
mob_met_all = rbind(mob_met1, mob_met2, mob_met3, mob_met4)



metrics_type = full_join(mob_met_all, type_mobility_longer |> select(ID, dataset, mobility_type_rec2) |> mutate(dataset = as.numeric(dataset)),
                         by = join_by(ID, dataset))
metrics_type_sum = metrics_type |> group_by(mobility_type_rec2, mobility_metric) |> summarize(count = n())
metrics_type_sum = metrics_type_sum |> filter(is.na(mobility_metric)==FALSE) |> filter(is.na(mobility_type_rec2)==FALSE)
metrics_type_sum = metrics_type_sum |> ungroup() |> group_by(mobility_metric) |> mutate(total_metric = sum(count))
metrics_type_sum = metrics_type_sum |> rowwise() |> mutate(count_prop = count / total_metric)
#metrics_type_sum = metrics_type_sum |> mutate(mobility_type_rec = case_when(mobility_type_rec %in% c("Traffic data including train, road use") ~ "Traffic data",
#                                                                            TRUE ~ mobility_type_rec))
metrics_type_sum = metrics_type_sum |> mutate(mobility_metric = case_when(
  mobility_metric %in% c("Clustering of individuals at a location") ~ "Clustering of individuals",
  mobility_metric %in% c("Proportion of time spent away from home") ~ "Time spent away from home",
  mobility_metric %in% c("Time spent at home/residence") ~ "Time spent at home",
  mobility_metric %in% c("Time spent at particular locations (like businesses)") ~ "Time spent at particular location",
  grepl("Trip counts", mobility_metric) ~ "Trip counts",
  TRUE ~ mobility_metric
))

# set levels so other is at the bottom
metrics_type_sum = metrics_type_sum |>
  mutate(mobility_metric = factor(mobility_metric,
                                 levels = c("Trip counts",
                                            "Time spent at particular location",
                                            "Traffic flows",
                                            "Time spent at home",
                                            "Clustering of individuals",
                                            "Migration",
                                            "Time spent away from home",
                                            "Other") ))

# Figure 2C
fig2c_pal = c(pal_jama()(6), "azure")
ggplot(metrics_type_sum |> filter(is.na(mobility_type_rec2)==FALSE),
       aes(fill = mobility_type_rec2, y = count_prop, x = mobility_metric))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Type of mobility data") +#scale_x_discrete(guide = guide_axis(n.dodge=2))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  #scale_fill_brewer(palette = "Dark2")+ 
  #scale_fill_d3() + 
  theme(text = element_text(size=24)) + xlab("Mobility metric") + ylab("Proportion")+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))+
  scale_fill_manual(labels = function(x) str_wrap(x, width = 25), values = fig2c_pal) + 
  #theme(legend.position = "none")+
  theme(legend.spacing.y = unit(0.25, 'cm')) +
  guides(fill = guide_legend(byrow=TRUE))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
  


tab1 = metrics_type |> ungroup() |> select(mobility_metric, mobility_type_rec2) |> 
  tbl_summary(by = mobility_type_rec2,
              statistic = list(all_categorical() ~ "{p}%"),
              label = mobility_metric ~ "Mobility metric") 
  save_as_docx(tab1 |> as_flex_table() |> theme_vanilla(), path = "tables/jan2024/metrics_type.docx")

  # check data types for China - 
  dat_china = left_join(metrics_type, clean_all |> select(country_focus, ID), by = join_by(ID) ) |> filter(country_focus %in% c("China"))
  table(dat_china$mobility_type_rec2)
  
  #### Figure: mobility metrics by country, for top ten countries
  # barplot by location
  clean_all = clean_all %>% mutate(Location = 
                                     ifelse(loc_focus == "national", country_focus, loc_focus))
  mob_data_dates_all = left_join(mob_data_dates_all, clean_all |> select(ID, country_focus), by = join_by(ID))
  metrics_location = mob_data_dates_all |> filter(country_focus != "") |>
    group_by(country_focus, mobility_metrics_fac) |> summarize(count = n())
  metrics_location = metrics_location |> ungroup() |> group_by(country_focus) |> mutate(total_country = sum(count))
  # restrict to top ten countries
  metrics_totals = metrics_location |> ungroup() |> group_by(country_focus) |> summarize(total_country = sum(count))
  metrics_totals = metrics_totals |> arrange(desc(total_country))
  metrics_totals = metrics_totals |> slice(1:10)
  
  # normalize data
  metrics_location = metrics_location |> rowwise() |> mutate(count_norm = count / total_country)
  metrics_totals = metrics_totals |> mutate(prop_tot = total_country / sum(total_country))
  metrics_location = left_join(metrics_location, metrics_totals %>% select(country_focus, prop_tot), by = join_by(country_focus))
  
  # let's look at widths for bars based on total number of papers
  temp = metrics_location |> filter(country_focus %in% metrics_totals$country_focus)
  temp |> group_by(country_focus) |> slice(1) |> arrange(desc(total_country)) 
  
  # add the widths
  metrics_location = metrics_location |> mutate(num_pap = case_when(
    total_country <8 ~ "<8 papers",
    total_country <15 ~ "<15 papers",
    total_country < 100 ~ "15+ papers",
    TRUE ~ NA
  ))
  metrics_location = metrics_location |> mutate(width_value = case_when(
    num_pap %in% c("<8 papers") ~ 0.25,
    num_pap %in% c("<15 papers") ~ 0.5,
    num_pap %in% c("15+ papers") ~ 1,
    TRUE ~ NA
  ))
  
  ## Figure 2B
  ggplot(metrics_location |> filter(country_focus %in% metrics_totals$country_focus), 
         aes(fill = mobility_metrics_fac, y = count_norm, x = country_focus, width = width_value))+
    #scale_fill_jco()+
    geom_bar(position = "stack", stat = "identity", color = "black", width = 0.95) + theme_bw() + 
    labs(fill = "Mobility metric") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
    theme(text = element_text(size=24)) + xlab("Country of focus") + ylab("Proportion datasets")+ #theme(legend.position = "none")+
  scale_fill_manual(values = fig2a_pal, labels = function(x) str_wrap(x, width = 25)) + 
    #theme(legend.position = "none")+
    theme(legend.spacing.y = unit(0.25, 'cm')) +
    guides(fill = guide_legend(byrow=TRUE))


  
  

#### Spatial scale by dataset type
spat_tocombine = spta_scale_long_rev |> select(ID, dataset, spatial_scale_rev)
spat_tocombine = spat_tocombine |> mutate(dataset = gsub("spatial_scale_", "", dataset))
metrics_type_spat = full_join(type_mobility_longer |> select(ID, dataset, mobility_type_rec2) |> mutate(dataset = as.numeric(dataset)), 
                              spat_tocombine |> mutate(dataset = as.numeric(dataset)), by = join_by(ID, dataset))

metrics_type_spat$spatial_scale_rev = as.character(metrics_type_spat$spatial_scale_rev)
spat_type = metrics_type_spat |> mutate(spatial_scale_rev = ifelse(spatial_scale_rev %in% c("Airport to airport", "Longitude/latitude coordinates"), "Grid cells", spatial_scale_rev))

spat_type = spat_type |> mutate(spatial_scale_rev = ifelse(spatial_scale_rev %in% c("Grid cells"), "Grid cells / coordinates", spatial_scale_rev))

spat_type = spat_type |> ungroup() |> group_by(ID, dataset, spatial_scale_rev, mobility_type_rec2) |> slice(1)
spat_type$spatial_scale_rev = factor(spat_type$spatial_scale_rev, 
                                     levels = c("Multinational/international", "National", "Subnational", "Grid cells / coordinates", "Other"))


# make a table
tab2 = spat_type |> ungroup() |> select(spatial_scale_rev, mobility_type_rec2) |> 
  tbl_summary(by = mobility_type_rec2,
              statistic = list(all_categorical() ~ "{p}%"),
              label = spatial_scale_rev ~ "Spatial scale") 
save_as_docx(tab2 |> as_flex_table() |> theme_vanilla(), path = "tables/jan2024/spatscale_type.docx")

spat_type = spat_type |> ungroup() |> group_by(mobility_type_rec2, spatial_scale_rev) |> summarize(count = n()) 
spat_type = spat_type |> ungroup() |> filter(is.na(mobility_type_rec2) == FALSE & is.na(spatial_scale_rev)==FALSE) |> 
  group_by(mobility_type_rec2) |> mutate(total_scale = sum(count))
spat_type = spat_type |> rowwise() |> mutate(count_prop = count / total_scale)
#spat_type = spat_type |> mutate(mobility_type_rec = case_when(mobility_type_rec %in% c("Traffic data including train, road use") ~ "Traffic data",
#                                                                            TRUE ~ mobility_type_rec))

spat_type = spat_type |> mutate(mobility_type_rec3 = ifelse(grepl("Apple", as.character(mobility_type_rec2)), "Apple, Facebook,\nGoogle, Twitter", as.character(mobility_type_rec2)))
spat_type$mobility_type_rec3 = factor(spat_type$mobility_type_rec3,
                                      levels = c("Apple, Facebook,\nGoogle, Twitter", "Baidu", "Census/migration", "Flight / traffic data", "Mobile phone", "Survey data / tourism", "Other"))



# Figure 2D
dp = pal_material("deep-purple")(8)
fig2d_pal = c(dp[8], dp[6], dp[4], dp[2], "azure")
ggplot(spat_type |> filter(is.na(spatial_scale_rev)==FALSE),
       aes(fill =  spatial_scale_rev, y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Spatial scale") + #scale_x_discrete(guide = guide_axis(n.dodge=3))+
  theme(text = element_text(size=24)) + xlab("Mobility data type") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 20, angle = 45,  hjust = 1))+
  #scale_fill_brewer(palette = "Blues", direction = -1, labels = function(x) str_wrap(x, width = 25))+
  scale_fill_manual(values = fig2d_pal,labels = function(x) str_wrap(x, width = 25))+
 #   theme(legend.position = "none")+
  theme(legend.spacing.y = unit(0.25, 'cm')) +
  guides(fill = guide_legend(byrow=TRUE))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))




############################ Granularity by dataset type
gran_tocombine = clean_all |> select(ID, contains("granularity"))
gran_tocombine = gran_tocombine |> pivot_longer(cols = -c(ID), names_to = "dataset", values_to = "granularity")  
gran_tocombine = gran_tocombine |> mutate(dataset = gsub("granularity_", "", dataset))
gran_tocombine = gran_tocombine |> filter(is.na(granularity)==FALSE & granularity !="") |> mutate(dataset = as.numeric(dataset))
table(gran_tocombine$granularity) #288 instances; 259 = No, 29 - Yes
length(unique(gran_tocombine$ID)) #231 papers

gran_type = full_join(type_mobility_longer |> select(ID, dataset, mobility_type_rec2) |> mutate(dataset = as.numeric(dataset)), 
                                   gran_tocombine, by = join_by(ID, dataset))
# make a table
tab3 = gran_type |> ungroup() |> select(granularity, mobility_type_rec2) |> 
  tbl_summary(by = mobility_type_rec2,
              statistic = list(all_categorical() ~ "{p}%"),
              label = granularity ~ "Granularity") 
save_as_docx(tab3 |> as_flex_table() |> theme_vanilla(), path = "tables/jan2024/gran_type.docx")

gran_type = gran_type %>% filter(is.na(mobility_type_rec2)==FALSE & is.na(granularity)==FALSE)
gran_type = gran_type |> ungroup() |> group_by(mobility_type_rec2, granularity)  |> summarize(count = n()) 
gran_type = gran_type |> ungroup() |> 
  group_by(mobility_type_rec2) |> mutate(total_gran = sum(count))
gran_type = gran_type |> rowwise() |> mutate(count_prop = count / total_gran)
gran_type = gran_type |> mutate(mobility_type_rec2 = case_when(mobility_type_rec2 %in% c("Traffic data including train, road use") ~ "Traffic data",
                                                              TRUE ~ mobility_type_rec2))
gran_type = gran_type |> mutate(granularity = case_when(grepl("aggregates", granularity) ~ "Aggregates multiple\nindividuals' movements",
                                                        grepl("individual-level", granularity) ~ "Measures individual-level\nmovements"))

gran_type = gran_type |> mutate(mobility_type_rec3 = ifelse(grepl("Apple", as.character(mobility_type_rec2)), "Apple, Facebook,\nGoogle, Twitter", as.character(mobility_type_rec2)))
gran_type$mobility_type_rec3 = factor(gran_type$mobility_type_rec3,
                                      levels = c("Apple, Facebook,\nGoogle, Twitter", "Baidu", "Census/migration", "Flight / traffic data", "Mobile phone", "Survey data / tourism", "Other"))

ggplot(gran_type |> filter(is.na(granularity)==FALSE),
       aes(fill = granularity , y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Granularity") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_uchicago()+
  theme(text = element_text(size=24)) + xlab("Mobility data type") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 20, angle = 45,  hjust = 1))



## granularity by date
gran_date = left_join(gran_tocombine, clean_all |> select(ID, date), by = join_by(ID))
ggplot(gran_date |> filter(is.na(granularity)==FALSE) |> ungroup() |> group_by(date, granularity) |> summarize(count = n()),
       aes(fill = granularity , y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Granularity") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_uchicago()+
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 20, angle = 45,  hjust = 1))



################### Details on population available by data type
detpop_tocombine = clean_all |> select(ID, contains("det_pop"))
detpop_tocombine = detpop_tocombine |> pivot_longer(cols = -c(ID), names_to = "dataset", values_to = "detpop")  
detpop_tocombine = detpop_tocombine |> mutate(dataset = gsub("det_pop_", "", dataset))
detpop_tocombine = detpop_tocombine |> filter(is.na(detpop)==FALSE & detpop !="") |> mutate(dataset = as.numeric(dataset))
table(detpop_tocombine$detpop) #288 instances; 253 = No, 35 - Yes
length(unique(detpop_tocombine$ID)) #231 papers
detpop_tocombine = full_join(type_mobility_longer |> select(ID, dataset, mobility_type_rec2, mobility_type_rec3) |> mutate(dataset = as.numeric(dataset)), 
                        detpop_tocombine, by = join_by(ID, dataset))
# make a table
tab4 = detpop_tocombine |> ungroup() |> select(detpop, mobility_type_rec2) |> 
  tbl_summary(by = mobility_type_rec2,
              statistic = list(all_categorical() ~ "{p}%"),
              label = detpop ~ "Details on population available") 
save_as_docx(tab4 |> as_flex_table() |> theme_vanilla(), path = "tables/jan2024/detpop_type.docx")



detpop_tocombine = detpop_tocombine %>% filter(is.na(mobility_type_rec3)==FALSE & is.na(detpop)==FALSE)
detpop_tocombine = detpop_tocombine |> ungroup() |> group_by(mobility_type_rec3, detpop)  |> summarize(count = n()) 
detpop_tocombine = detpop_tocombine |> ungroup() |> 
  group_by(mobility_type_rec3) |> mutate(total_gran = sum(count))
detpop_tocombine = detpop_tocombine |> rowwise() |> mutate(count_prop = count / total_gran)
#detpop_tocombine = detpop_tocombine |> mutate(mobility_type_rec = case_when(mobility_type_rec %in% c("Traffic data including train, road use") ~ "Traffic data",
#                                                              TRUE ~ mobility_type_rec))
detpop_tocombine$detpop = factor(detpop_tocombine$detpop, levels = c("Yes", "No"))

# Fig S8A
ggplot(detpop_tocombine,
       aes(fill = detpop , y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Details on population\navailable") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  #scale_fill_manual(values=met.brewer("Veronese", 2))+
  #scale_fill_brewer(palette = "Oranges", direction = -1) + 
  scale_fill_grey()+
  theme(text = element_text(size=24)) + xlab("Mobility data type") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 20, angle = 45,  hjust = 1))



############### Metadata available directly
direct_meta_combo = clean_all |> select(ID, contains("direct_meta")) |> select(-c(direct_metadata))
direct_meta_combo = direct_meta_combo |> pivot_longer(cols = -c(ID), names_to = "dataset", values_to = "meta")  
direct_meta_combo = direct_meta_combo |> mutate(dataset = gsub("direct_meta_", "", dataset))
direct_meta_combo = direct_meta_combo |> filter(is.na(meta)==FALSE & meta !="NULL") |> mutate(dataset = as.numeric(dataset))
# list elements so need to unnest
direct_meta_combo = direct_meta_combo |> unnest(meta)
# unique number of datasets
direct_meta_combo = direct_meta_combo |> mutate(ds_unique = paste0(ID, "_", dataset))
length(unique(direct_meta_combo$ds_unique)) #287
# make a table and save
direct_meta_combo |> group_by(meta) |> summarize(count = n()) |> arrange(desc(count)) |>
  mutate(Percent = paste0(round(count / length(unique(direct_meta_combo$ds_unique))*100), "%")) |> flextable()|> 
  save_as_docx(path = "tables/jan2024/direct_meta_all.docx")
table(direct_meta_combo$meta) #303 instances;
length(unique(direct_meta_combo$ID)) #230 papers
direct_meta_combo = right_join(type_mobility_longer |> select(ID, dataset, mobility_type_rec2, mobility_type_rec3) |> mutate(dataset = as.numeric(dataset)), 
                              direct_meta_combo, by = join_by(ID, dataset))

direct_meta_combo = direct_meta_combo |> mutate(meta_fac = factor(meta,
                                                                  levels = c("None", "Age", "Ethnicity/race", "Gender/sex", "Socio-economic Information", "Other - clinical", "Other - Means of transport, duration of journey, reason for moving")))
# make a table

tab5 = direct_meta_combo |> ungroup() |> select(meta_fac, mobility_type_rec2) |> 
  tbl_summary(by = mobility_type_rec2,
              statistic = list(all_categorical() ~ "{p}%"),
              label = meta_fac ~ "Direct meta-data available") 

save_as_docx(tab5 |> as_flex_table() |> theme_vanilla(), path = "tables/jan2024/direct_meta_type.docx")

direct_meta_combo = direct_meta_combo %>% filter(is.na(mobility_type_rec3)==FALSE & is.na(meta)==FALSE)

direct_meta_combo = direct_meta_combo |> ungroup() |> group_by(mobility_type_rec3, meta)  |> summarize(count = n()) 
direct_meta_combo = direct_meta_combo |> ungroup() |> 
  group_by(mobility_type_rec3) |> mutate(total_gran = sum(count))
direct_meta_combo = direct_meta_combo |> rowwise() |> mutate(count_prop = count / total_gran)
#direct_meta_combo = direct_meta_combo |> mutate(mobility_type_rec = case_when(mobility_type_rec %in% c("Traffic data including train, road use") ~ "Traffic data",
   #                                                                         TRUE ~ mobility_type_rec))
direct_meta_combo = direct_meta_combo |> mutate(meta = ifelse(grepl("Other", meta), "Other", meta))
direct_meta_combo$meta_rev = factor(direct_meta_combo$meta, levels = c("None", "Age", "Ethnicity/race", "Gender/sex", "Socio-economic Information", "Other"))

# Figure S8B
ggplot(direct_meta_combo,
       aes(fill = meta_rev , y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Direct meta-data") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  #scale_fill_manual(values=met.brewer("Thomas", 7))+
  scale_fill_manual(values = c("slategray2", "plum4", "seagreen4", "lightcoral", "steelblue4", "seashell4"))+
  theme(text = element_text(size=24)) + xlab("Mobility data type") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 20, angle = 45,  hjust = 1))



############### Metadata available ecologically
eco_meta_combo = clean_all |> select(ID, contains("eco_meta"))
eco_meta_combo = eco_meta_combo |> pivot_longer(cols = -c(ID), names_to = "dataset", values_to = "meta")  
eco_meta_combo = eco_meta_combo |> mutate(dataset = gsub("eco_meta_", "", dataset))
eco_meta_combo = eco_meta_combo |> filter(is.na(meta)==FALSE & meta !="NULL") |> mutate(dataset = as.numeric(dataset))
# list elements so need to unnest
eco_meta_combo = eco_meta_combo |> unnest(meta)
# unique number of datasets
eco_meta_combo = eco_meta_combo |> mutate(ds_unique = paste0(ID, "_", dataset))
length(unique(eco_meta_combo$ds_unique)) #287
table(eco_meta_combo$meta) #313 instances
length(unique(eco_meta_combo$ID)) #230 papers
eco_meta_combo |> group_by(meta) |> summarize(count = n()) |> arrange(desc(count)) |>
  mutate(Percent = paste0(round(count / length(unique(eco_meta_combo$ds_unique))*100), "%")) |> flextable()|> 
  save_as_docx(path = "tables/jan2024/eco_meta_all.docx")

eco_meta_combo = right_join(type_mobility_longer |> select(ID, dataset, mobility_type_rec2, mobility_type_rec3) |> mutate(dataset = as.numeric(dataset)), 
                            eco_meta_combo, by = join_by(ID, dataset))
eco_meta_combo = eco_meta_combo |> mutate(meta_fac = factor(meta,
                                                                  levels = c("None", "Age", "Ethnicity/race", "Gender/sex", "Socio-economic Information")))

tab6 = eco_meta_combo |> ungroup() |> select(meta, mobility_type_rec2) |> 
  tbl_summary(by = mobility_type_rec2,
              statistic = list(all_categorical() ~ "{p}%"),
              label = meta ~ "Ecological meta-data available") 
save_as_docx(tab6 |> as_flex_table() |> theme_vanilla(), path = "tables/jan2024/eco_meta_type.docx")

eco_meta_combo = eco_meta_combo %>% filter(is.na(mobility_type_rec3)==FALSE & is.na(meta)==FALSE)
eco_meta_combo = eco_meta_combo |> ungroup() |> group_by(mobility_type_rec3, meta)  |> summarize(count = n()) 
eco_meta_combo = eco_meta_combo |> ungroup() |> 
  group_by(mobility_type_rec3) |> mutate(total_gran = sum(count))
eco_meta_combo = eco_meta_combo |> rowwise() |> mutate(count_prop = count / total_gran)
#eco_meta_combo = eco_meta_combo |> mutate(mobility_type_rec = case_when(mobility_type_rec %in% c("Traffic data including train, road use") ~ "Traffic data",
#                                                                              TRUE ~ mobility_type_rec))
eco_meta_combo = eco_meta_combo |> mutate(meta_fac = factor(meta,
                                                            levels = c("None", "Age", "Ethnicity/race", "Gender/sex", "Socio-economic information")))
# Figure S8C
ggplot(eco_meta_combo,
       aes(fill = meta_fac , y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Ecological meta-data") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  scale_fill_manual(values=met.brewer("Thomas", 7))+
  theme(text = element_text(size=24)) + xlab("Mobility data type") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 20, angle = 45,  hjust = 1))+
  scale_fill_manual(values = c("slategray2", "plum4", "seagreen4", "lightcoral", "steelblue4", "seashell4"))+
  theme(text = element_text(size=24)) + xlab("Mobility data type") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 20, angle = 45,  hjust = 1))





############# Bias in who is sampled
samp_combo = clean_all |> select(ID, contains("samp_"))
samp_combo = samp_combo |> pivot_longer(cols = -c(ID), names_to = "dataset", values_to = "bias")  
samp_combo = samp_combo |> mutate(dataset = gsub("samp_", "", dataset))
samp_combo = samp_combo |> filter(is.na(bias)==FALSE & bias !="") |> mutate(dataset = as.numeric(dataset))
samp_combo = samp_combo |> unnest(bias)
# unique number of datasets
samp_combo = samp_combo |> mutate(ds_unique = paste0(ID, "_", dataset))
length(unique(samp_combo$ds_unique)) # 287 unique datasets
table(samp_combo$bias) 
length(unique(samp_combo$ID)) #230 papers
samp_combo |> group_by(bias) |> summarize(count = n()) |> arrange(desc(count)) |>
  mutate(Percent = paste0(round(count / length(unique(samp_combo$ds_unique))*100), "%")) |> flextable()|> 
  save_as_docx(path = "tables/jan2024/bias_sample_all.docx")

samp_combo = right_join(type_mobility_longer |> select(ID, dataset, mobility_type_rec2, mobility_type_rec3) |> mutate(dataset = as.numeric(dataset)), 
                        samp_combo, by = join_by(ID, dataset))
tab7 = samp_combo |> ungroup() |> select(bias, mobility_type_rec2) |> 
  tbl_summary(by = mobility_type_rec2,
              statistic = list(all_categorical() ~ "{p}%"),
              label = bias ~ "Information about bias from sampling") 
save_as_docx(tab7 |> as_flex_table() |> theme_vanilla(), path = "tables/jan2024/bias_type.docx")

samp_combo = samp_combo %>% filter(is.na(mobility_type_rec3)==FALSE & is.na(bias)==FALSE)
samp_combo = samp_combo |> ungroup() |> group_by(mobility_type_rec3, bias)  |> summarize(count = n()) 
samp_combo = samp_combo |> ungroup() |> 
  group_by(mobility_type_rec3) |> mutate(total_gran = sum(count))
samp_combo = samp_combo |> rowwise() |> mutate(count_prop = count / total_gran)
#samp_combo = samp_combo |> mutate(mobility_type_rec = case_when(mobility_type_rec %in% c("Traffic data including train, road use") ~ "Traffic data",
#                                                                        TRUE ~ mobility_type_rec))
samp_combo = samp_combo |> mutate(bias_rec = case_when(
  grepl("No,", bias) ~ "No information",
  grepl("but do not use", bias) ~ "Yes, but did not adjust",
  grepl("and use it", bias) ~ "Yes, and adjusted",
  bias %in% c("Not applicable") ~ "Not applicable"
))

# Figure S8D
ggplot(samp_combo |> filter(is.na(mobility_type_rec3)==FALSE),
       aes(fill = bias_rec , y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Bias in who is sampled") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  #scale_fill_manual(values=met.brewer("Tara", 4))+
  scale_fill_manual(values = c("violetred4", "snow4", "lightblue3", "plum4"))+
  theme(text = element_text(size=24)) + xlab("Mobility data type") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 20, angle = 45,  hjust = 1))


##### stack all tables together and save
save_as_docx(tbl_stack(list(tab1, tab2, tab3, tab4, tab5, tab6, tab7)) |> as_flex_table() |> theme_vanilla(), 
             path = "tables/jan2024/tab1_tab7.docx")


########## Match mobility to disease
matc_combo = clean_all |> select(ID, contains("match_disease_"))
matc_combo = matc_combo |> pivot_longer(cols = -c(ID), names_to = "dataset", values_to = "match")  
matc_combo = matc_combo |> mutate(dataset = gsub("match_disease_", "", dataset))
matc_combo = matc_combo |> filter(is.na(match)==FALSE & match !="") |> mutate(dataset = as.numeric(dataset))
matc_combo = matc_combo |> unnest(match)
# unique number of datasets
matc_combo = matc_combo |> mutate(ds_unique = paste0(ID, "_", dataset))
length(unique(matc_combo$ds_unique)) # 288 unique datasets
table(matc_combo$match) #288 datasets
length(unique(matc_combo$ID)) #231 papers

matc_combo = right_join(type_mobility_longer |> select(ID, dataset, mobility_type_rec3) |> mutate(dataset = as.numeric(dataset)), 
                        matc_combo, by = join_by(ID, dataset))
matc_combo = matc_combo %>% filter(is.na(mobility_type_rec3)==FALSE & is.na(match)==FALSE)
matc_combo = matc_combo |> ungroup() |> group_by(mobility_type_rec3, match)  |> summarize(count = n()) 
matc_combo = matc_combo |> ungroup() |> 
  group_by(mobility_type_rec3) |> mutate(total_gran = sum(count))
matc_combo = matc_combo |> rowwise() |> mutate(count_prop = count / total_gran)
#matc_combo = matc_combo |> mutate(mobility_type_rec = case_when(mobility_type_rec %in% c("Traffic data including train, road use") ~ "Traffic data",
#                                                                TRUE ~ mobility_type_rec))
ggplot(matc_combo |> filter(is.na(mobility_type_rec3)==FALSE),
       aes(fill = match , y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Match disease status to mobility") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_grey()+
  theme(text = element_text(size=24)) + xlab("Mobility data type") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 20, angle = 45,  hjust = 1))


########## Availability of mobility data 
dat_avail = clean_all |> select(ID, contains("data_avail_"))
dat_avail = dat_avail |> pivot_longer(cols = -c(ID), names_to = "dataset", values_to = "avail")  
dat_avail = dat_avail |> mutate(dataset = gsub("data_avail_", "", dataset))
dat_avail = dat_avail |> filter(is.na(avail)==FALSE & avail !="") |> mutate(dataset = as.numeric(dataset))
dat_avail = dat_avail |> unnest(avail)
# unique number of datasets
dat_avail = dat_avail |> mutate(ds_unique = paste0(ID, "_", dataset))
length(unique(dat_avail$ds_unique)) # 288 unique datasets
table(dat_avail$avail) #288 datasets
length(unique(dat_avail$ID)) #231 papers
dat_avail |> group_by(avail) |> summarize(count = n()) |> arrange(desc(count)) |>
  mutate(Percent = paste0(round(count / length(unique(dat_avail$ds_unique))*100), "%")) |> flextable()|> 
  save_as_docx(path = "tables/jan2024/dat_avail.docx")

dat_avail = right_join(type_mobility_longer |> select(ID, dataset, mobility_type_rec2, mobility_type_rec3) |> mutate(dataset = as.numeric(dataset)), 
                       dat_avail, by = join_by(ID, dataset))
dat_avail = dat_avail %>% filter(is.na(mobility_type_rec3)==FALSE & is.na(avail)==FALSE)
dat_avail = dat_avail |> ungroup() |> group_by(mobility_type_rec3, avail)  |> summarize(count = n()) 
dat_avail = dat_avail |> ungroup() |> 
  group_by(mobility_type_rec3) |> mutate(total_gran = sum(count))
dat_avail = dat_avail |> rowwise() |> mutate(count_prop = count / total_gran)
#dat_avail = dat_avail |> mutate(mobility_type_rec = case_when(mobility_type_rec %in% c("Traffic data including train, road use") ~ "Traffic data",
#                                                                TRUE ~ mobility_type_rec))
dat_avail = dat_avail |> mutate(avail2 = ifelse(grepl("government", avail), "Company, government agency, etc to authors", avail))
ggplot(dat_avail |> filter(is.na(mobility_type_rec3)==FALSE),
       aes(fill = avail2 , y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Mobility data availability") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  
  theme(text = element_text(size=24)) + xlab("Mobility data type") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 20, angle = 45,  hjust = 1)) #+ theme(legend.position = "none")


# Make a figure for availability by date - standardized
dat_avail = clean_all |> select(ID, contains("data_avail_"))
dat_avail = dat_avail |> pivot_longer(cols = -c(ID), names_to = "dataset", values_to = "avail")  
dat_avail = dat_avail |> mutate(dataset = gsub("data_avail_", "", dataset))
dat_avail = dat_avail |> filter(is.na(avail)==FALSE & avail !="") |> mutate(dataset = as.numeric(dataset))
dat_avail = dat_avail |> unnest(avail)
# merge in date
dat_avail = left_join(dat_avail, clean_all |> select(ID, date), by = join_by(ID))
dat_avail = dat_avail |> 
  group_by(date, avail) |> summarize(count = n())
dat_avail = dat_avail |> ungroup() |> group_by(date) |> mutate(total_date = sum(count))

# normalize data
dat_avail = dat_avail |> rowwise() |> mutate(count_norm = count / total_date)
dat_avail = dat_avail |> mutate(avail = ifelse(grepl("Made available", avail), "Made available to authors by a company,\ngovernment agency, etc.", avail))
ggplot(dat_avail , 
       aes(fill = avail, y = count_norm, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Mobility data availability") + #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Proportion datasets")



######################## Identifiability of data 
dat_ident = clean_all |> select(ID, contains("ident_"))
dat_ident = dat_ident |> pivot_longer(cols = -c(ID), names_to = "dataset", values_to = "ident")  
dat_ident = dat_ident |> mutate(dataset = gsub("ident_", "", dataset))
dat_ident = dat_ident |> filter(is.na(ident)==FALSE & ident !="") |> mutate(dataset = as.numeric(dataset))
dat_ident = dat_ident |> unnest(ident)
# unique number of datasets
dat_ident = dat_ident |> mutate(ds_unique = paste0(ID, "_", dataset))
length(unique(dat_ident$ds_unique)) # 287 unique datasets
table(dat_ident$ident) 
length(unique(dat_ident$ID)) #231 papers
dat_ident |> group_by(ident) |> summarize(count = n()) |> arrange(desc(count)) |>
  mutate(Percent = paste0(round(count / length(unique(dat_ident$ds_unique))*100), "%")) |> flextable()|> 
  save_as_docx(path = "tables/jan2024/dat_ident.docx")

dat_ident_l = dat_ident
dat_ident = right_join(type_mobility_longer |> select(ID, dataset, mobility_type_rec3) |> mutate(dataset = as.numeric(dataset)), 
                       dat_ident, by = join_by(ID, dataset))
dat_ident = dat_ident %>% filter(is.na(mobility_type_rec3)==FALSE & is.na(ident)==FALSE)
dat_ident = dat_ident |> ungroup() |> group_by(mobility_type_rec3, ident)  |> summarize(count = n()) 
dat_ident = dat_ident |> ungroup() |> 
  group_by(mobility_type_rec3) |> mutate(total_gran = sum(count))
dat_ident = dat_ident |> rowwise() |> mutate(count_prop = count / total_gran)
#dat_ident = dat_ident |> mutate(mobility_type_rec = case_when(mobility_type_rec %in% c("Traffic data including train, road use") ~ "Traffic data",
#                                                                TRUE ~ mobility_type_rec))

ggplot(dat_ident |> filter(is.na(mobility_type_rec3)==FALSE),
       aes(fill = ident , y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Data privacy and identifiability") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  scale_fill_viridis(discrete=TRUE, option = "mako")+
  theme(text = element_text(size=24)) + xlab("Mobility data type") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))


# Make a figure for availability by date - standardized

# merge in date
dat_ident_l = left_join(dat_ident_l, clean_all |> select(ID, date), by = join_by(ID))
dat_ident_l = dat_ident_l |> 
  group_by(date, ident) |> summarize(count = n())
dat_ident_l = dat_ident_l |> ungroup() |> group_by(date) |> mutate(total_date = sum(count))

# normalize data
dat_ident_l = dat_ident_l |> rowwise() |> mutate(count_norm = count / total_date)
dat_ident_l = dat_ident_l |> mutate(ident = ifelse(grepl("Other", ident), "Other", ident))
ggplot(dat_ident_l , 
       aes(fill = ident, y = count_norm, x = date))+
  scale_fill_nejm()+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Identifiability and privacy") + #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Proportion datasets")





############# Type of SARS-CoV-2 data

type_cov_data = clean_all |> select(ID, type_cov) 
type_cov_data = type_cov_data |> unnest(type_cov) |> filter(is.na(type_cov)==FALSE)# 315 instances
# unique number of datasets
length(unique(type_cov_data$ID)) # 231 papers
type_cov_data |> group_by(type_cov) |> summarize(count = n()) |> arrange(desc(count)) |>
  mutate(Percent = paste0(round(count / length(unique(type_cov_data$ID))*100), "%")) |> flextable()|> 
  save_as_docx(path = "tables/jan2024/type_cov.docx")

### by date
# merge in date
type_cov_data = left_join(type_cov_data, clean_all |> select(ID, date), by = join_by(ID))
type_cov_data = type_cov_data |> 
  group_by(date, type_cov) |> summarize(count = n())
type_cov_data = type_cov_data |> ungroup() |> group_by(date) |> mutate(total_date = sum(count))

# normalize data
type_cov_data = type_cov_data |> rowwise() |> mutate(count_norm = count / total_date)

# order so that "Other" and "No" are at the bottom
type_cov_data$type_cov_fac = factor(type_cov_data$type_cov,
                                    levels = c("Confirmed cases", "Confirmed deaths", "Hospitalizations", "Suspected cases",
                                               "Suspected deaths", "Serological results or virological study", "Other", "No"))

ggplot(type_cov_data , 
       aes(fill = type_cov_fac, y = count_norm, x = date))+
  scale_fill_nejm()+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Type of SARS-CoV-2 / COVID-19 data") + #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Proportion datasets")


######################## Source of SARS-CoV-2 data
source_cov_data = clean_all |> select(ID, source_cov_data) 
source_cov_data = source_cov_data |> unnest(source_cov_data) |> filter(is.na(source_cov_data)==FALSE)# 248 instances
table(source_cov_data$source_cov_data)
l_unique = length(unique(source_cov_data$ID)) #203 papers
source_cov_data |> group_by(source_cov_data) |> summarize(count = n()) |> arrange(desc(count)) |>
  mutate(Percent = paste0(round(count / l_unique*100), "%")) |> flextable()|> 
  save_as_docx(path = "tables/jan2024/source_cov.docx")

# look to see if varies by location
source_cov_top10 = left_join(source_cov_data, clean_all |> select(ID, country_focus), by = join_by(ID)) |> filter(country_focus %in% c(country_top10$country_focus))
source_cov_top10 = source_cov_top10 |> 
  group_by(country_focus, source_cov_data) |> summarize(count = n())
source_cov_top10 = source_cov_top10 |> ungroup() |> group_by(country_focus) |> mutate(total_country = sum(count))
# normalize data
source_cov_top10 = source_cov_top10 |> rowwise() |> mutate(count_norm = count / total_country)

# combine some categories
source_cov_top10 = source_cov_top10 |> mutate(source_cov_data_fac = case_when(
  grepl("Other", source_cov_data) ~ "Other",
  grepl("Research study", source_cov_data) ~ "Research study",
  grepl("Media", source_cov_data) ~ "Media",
  grepl("JHU", source_cov_data) ~ "JHU",
  grepl("Non-governmental", source_cov_data) ~ "NGO report or repository",
  TRUE ~ source_cov_data
))
ggplot(source_cov_top10 , 
       aes(fill = source_cov_data_fac, y = count_norm, x = country_focus))+
  scale_fill_nejm()+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Source of SARS-CoV-2 /\nCOVID-19 data") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  theme(text = element_text(size=24)) + xlab("Country") + ylab("Proportion datasets")



########### temporal and spatial overlap
sp_overl = clean_all |> select(ID, spatial) |> filter(is.na(spatial)==FALSE & spatial !="")
length(unique(sp_overl$ID)) # 203 datasets
sp_overl |> group_by(spatial) |> summarize(count = n()) |> arrange(desc(count)) |>
  mutate(Percent = paste0(round(count / length(unique(sp_overl$ID))*100), "%")) |> flextable()|> 
  save_as_docx(path = "tables/jan2024/sp_overl.docx")

temp_overl = clean_all |> select(ID, timing) |> filter(is.na(timing)==FALSE & timing !="")
length(unique(temp_overl$ID)) # 203 datasets
temp_overl |> group_by(timing) |> summarize(count = n()) |> arrange(desc(count)) |>
  mutate(Percent = paste0(round(count / length(unique(temp_overl$ID))*100), "%")) |> flextable()|> 
  save_as_docx(path = "tables/jan2024/timing_overl.docx")

# temporal overlap by type of mobility data - for no overlap
temp_overl = full_join(temp_overl, type_mobility_longer |> select(ID, mobility_type_rec3), by = join_by(ID))
temp_overl |> filter(grepl("No temporal overlap", timing)) |> group_by(mobility_type_rec3) |> summarize(count = n()) |>
  mutate(Percent = paste0(round(count / sum(count) * 100), "%")) |> arrange(desc(count)) |>flextable() |>
  save_as_docx(path = "tables/jan2024/temp_overl_nooverlap.docx")
# get total instances
dim(temp_overl |> filter(grepl("No temporal overlap", timing)))[1]
t = temp_overl |> filter(grepl("No temporal overlap", timing))
length(unique(t$ID)) # 33 papers



######################### purpose of paper - by time
prim_df = clean_all |> select(ID, date, primary_goal) # 242 papers
prim_df = prim_df |> unnest(primary_goal) # 374 goals
table(prim_df$primary_goal)

# need to combine intro-case + intro-variant
# first check if there any papers that have both
prim_check = prim_df |> filter(grepl("Intro", primary_goal)) # 44 entries, 44 separate IDs - so no overlap between them
# that means we can just collapse the categories
prim_df = prim_df |> mutate(primary_goal_col = ifelse(grepl("Intro", primary_goal), "Intro - case or variant", primary_goal))
prim_df_date = prim_df |> group_by(date, primary_goal_col) |> summarize(count = n())

# make a table
prim_df |> group_by(primary_goal) |> summarize(count = n()) |>
  mutate(Percent = paste0(round(count / length(unique(prim_df$ID))*100), "%")) |>
  arrange(desc(count)) |> flextable() |> save_as_docx(path = "tables/jan2024/prim_goal.docx")


# make a plot by date
ggplot(prim_df_date,
       aes(fill = primary_goal_col , y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Primary goal of the study") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "rainbow")+
  scale_fill_brewer(palette = "Paired")+
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))

#### make a plot collapsing NPI categories together
prim_df = prim_df |> mutate(primary_goal_col2 = ifelse(grepl("NPI", primary_goal), "NPI", primary_goal_col))
prim_df = prim_df |> mutate(primary_goal_col2 = case_when(
  primary_goal_col2 %in% c("Case report", "Data available") ~ "Other",
  TRUE ~ primary_goal_col2
) )
# only keep the first set of each goal for each ID
prim_df2 = prim_df |> group_by(ID, primary_goal_col2) |> slice(1)
prim_df_date2 = prim_df2 |> ungroup() |> group_by(date, primary_goal_col2) |> summarize(count = n())

# order categories
prim_df_date2 = prim_df_date2 |> mutate(primary_goal_col3 = factor(primary_goal_col2,
                                                                   levels = c("Assoc mobility, trans",
                                                                              "Change mobility",
                                                                              "Intro - case or variant",
                                                                              "NPI",
                                                                              "SES or race ineq",
                                                                              "Other")))
# Figure 1C
# make a plot by date
ggplot(prim_df_date2,
       aes(fill = primary_goal_col3 , y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Primary goal") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "rainbow")+
  #scale_fill_brewer(palette = "Dark2")+
  scale_fill_aaas(alpha = 0.9)+
  #theme(legend.position = "none")+
  theme(text = element_text(size = 24)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 24))


### look at purpose of papers that used more than a single dataset - 62 instances across 41 papers
two_sets = clean_all |> filter(n_mob_data_sets != "1") # 
prim_df2 |> filter(ID %in% two_sets$ID) |> ungroup() |> group_by(primary_goal) |> summarize(count = n()) |> 
  arrange(desc(count)) |> mutate(Percent = paste0(round(count / length(unique(two_sets$ID))*100), "%")) |>
  flextable() |> save_as_docx(path = "tables/jan2024/prim_goal_mult_dat.docx")



# test for change over time 
temp = prim_df2 |> group_by(date) |> mutate(num_papers = length(unique(ID)))
temp = temp |> ungroup() |> group_by(date, primary_goal_col2) |> summarize(count = n(),
                                                                           num_papers = min(num_papers))
# make indicator variables
temp = temp |> mutate(ind_intro = ifelse(grepl("Intro", primary_goal_col2), 1, 0),
                      ind_npi = ifelse(grepl("NPI", primary_goal_col2), 1, 0),
                      ind_ass = ifelse(grepl("Assoc", primary_goal_col2),1,0),
                      ind_change = ifelse(grepl("Change", primary_goal_col2), 1, 0),
                      ind_other = ifelse(grepl("Other", primary_goal_col2), 1, 0),
                      ind_SES = ifelse(grepl("SES", primary_goal_col2), 1, 0))
mod = glm(count ~ date + ind_intro + ind_npi + ind_ass + ind_change + ind_other + ind_SES, offset = num_papers, data = temp, family = "poisson")
summary(mod)
# save output
stat_prim_goal = broom::tidy(mod) |> filter(term %in% c("date"))
stat_prim_goal = type_time |> mutate(descr = "Primary goal vs time",
                                categories = paste(toString(unique(temp$primary_goal_col2)), sep=","))

temp2 = prim_df2

test <- multinom(primary_goal_col2 ~ date, data = temp2)
summary(test)
# check for significance
Anova(test, type = "II")



####### spatial scale by primary goal
spta_scale_long$dataset = gsub("spatial_scale_", "", spta_scale_long$dataset)
prim_spat = full_join(prim_df2 |> select(ID, primary_goal_col2),
                      spta_scale_long |> select(ID, spatial_scale) |> group_by(ID, spatial_scale) |> slice(1),
                      by = join_by(ID))
prim_spat = prim_spat |> mutate(spatial_scale = ifelse(grepl("Other", spatial_scale), "Other", spatial_scale))
prim_spat = prim_spat |> ungroup() |> group_by(primary_goal_col2, spatial_scale) |> summarize(count = n())
prim_spat = prim_spat |> ungroup() |> filter(is.na(spatial_scale)==FALSE & is.na(primary_goal_col2)==FALSE) |>
  group_by(primary_goal_col2) |> mutate(total_goal = sum(count))
prim_spat = prim_spat |> mutate(count_norm = count / total_goal)

# normalized plot
ggplot(prim_spat,
       aes(fill = spatial_scale , y = count_norm, x = primary_goal_col2))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Spatial scale") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Paired") + 
  theme(text = element_text(size=20)) + xlab("Primary goal of paper") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))

# not normalized
ggplot(prim_spat,
       aes(fill = spatial_scale , y = count, x = primary_goal_col2))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Spatial scale") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Paired") + 
  theme(text = element_text(size=20)) + xlab("Primary goal of paper") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))



######## country focus by primary goal
prim_country = left_join(prim_df2 |> select(ID, primary_goal_col2),
                         clean_all |> select(ID, country_focus),
                         by = join_by(ID))
prim_country = prim_country |> filter(is.na(primary_goal_col2)==FALSE) |>
  filter(is.na(country_focus)==FALSE) |>
  group_by(primary_goal_col2, country_focus) |> summarize(count = n())
ggplot(prim_country |> filter(country_focus %in% country_top10$country_focus),
       aes(fill = country_focus , y = count, x = primary_goal_col2))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Country focus") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Paired") + 
  theme(text = element_text(size=20)) + xlab("Primary goal of paper") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))

######################### purpose of paper - by data type
prim_type = full_join(type_mobility_longer |> select(ID, mobility_type_rec) |> mutate(dataset = as.numeric(dataset)), 
                       dat_ident, by = join_by(ID, dataset))
dat_ident = dat_ident %>% filter(is.na(mobility_type_rec)==FALSE & is.na(ident)==FALSE)
dat_ident = dat_ident |> ungroup() |> group_by(mobility_type_rec, ident)  |> summarize(count = n()) 
dat_ident = dat_ident |> ungroup() |> 
  group_by(mobility_type_rec) |> mutate(total_gran = sum(count))
dat_ident = dat_ident |> rowwise() |> mutate(count_prop = count / total_gran)
dat_ident = dat_ident |> mutate(mobility_type_rec = case_when(mobility_type_rec %in% c("Traffic data including train, road use") ~ "Traffic data",
                                                              TRUE ~ mobility_type_rec))
ggplot(dat_ident |> filter(is.na(mobility_type_rec)==FALSE),
       aes(fill = ident , y = count_prop, x = mobility_type_rec))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Data privacy and identifiability") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  scale_fill_viridis(discrete=TRUE, option = "mako")+
  theme(text = element_text(size=20)) + xlab("Mobility data type") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))

######################### temporal scale - by data type
type_mobility_longer =  type_mobility_longer |> mutate(mobility_type_rec3 = ifelse(grepl("Apple", as.character(mobility_type_rec2)), "Apple, Facebook,\nGoogle, Twitter", as.character(mobility_type_rec2)))
type_mobility_longer$mobility_type_rec3 = factor(type_mobility_longer$mobility_type_rec3,
                                      levels = c("Apple, Facebook,\nGoogle, Twitter", "Baidu", "Census/migration", "Flight / traffic data", "Mobile phone", "Survey data / tourism", "Other"))


temp_scale = full_join(type_mobility_longer |> select(ID, mobility_type_rec3), 
                      clean_all |> select(ID, temp_resol), by = join_by(ID))
temp_scale = temp_scale %>% filter(is.na(mobility_type_rec3)==FALSE & is.na(temp_resol)==FALSE & temp_resol!="")
temp_scale = temp_scale |> ungroup() |> group_by(mobility_type_rec3, temp_resol)  |> summarize(count = n()) 
temp_scale = temp_scale |> ungroup() |> 
  group_by(mobility_type_rec3) |> mutate(total_gran = sum(count))
temp_scale = temp_scale |> rowwise() |> mutate(count_prop = count / total_gran)
#temp_scale = temp_scale |> mutate(mobility_type_rec3 = case_when(mobility_type_rec3 %in% c("Traffic data including train, road use") ~ "Traffic data",
#                                                              TRUE ~ mobility_type_rec3))
temp_scale = temp_scale |> mutate(temp_resol_fac = factor(temp_resol,
                                                          levels = c("Monthly or longer", "Weekly", "Daily", "Sub-daily", "Unspecified")))

# Figure S7
ggplot(temp_scale |> filter(is.na(mobility_type_rec3)==FALSE),
       aes(fill = temp_resol_fac , y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Temporal resolution") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  scale_fill_manual(values=met.brewer("Lakota", 5))+
  theme(axis.text.x = element_text(size = 20, angle = 45,  hjust = 1))


## Make a table that has more granular breakdown of mobility data type
temp_scale_gran = full_join(type_mobility_longer |> select(ID, mobility_type_rec), 
                       clean_all |> select(ID, temp_resol), by = join_by(ID))
temp_scale_gran = temp_scale_gran %>% filter(is.na(mobility_type_rec)==FALSE & is.na(temp_resol)==FALSE & temp_resol!="")
#temp_scale_gran = temp_scale_gran |> ungroup() |> group_by(mobility_type_rec, temp_resol)  |> summarize(count = n()) 
#temp_scale_gran = temp_scale_gran |> ungroup() |> 
#  group_by(mobility_type_rec) |> mutate(total_gran = sum(count))
#temp_scale_gran = temp_scale_gran |> rowwise() |> mutate(count_prop = count / total_gran)
temp_scale_gran = temp_scale_gran |> mutate(temp_resol_fac = factor(temp_resol,
                                                          levels = c("Monthly or longer", "Weekly", "Daily", "Sub-daily", "Unspecified")))
tab8 = temp_scale_gran |> ungroup() |> select(temp_resol_fac, mobility_type_rec) |> 
  tbl_summary(by = temp_resol_fac,
              statistic = list(all_categorical() ~ "{p}%"),
              #label = temp_resol_fac ~ "Temporal resolution"
              label = mobility_type_rec ~ "Mobility type",
              percent = "row") 
save_as_docx(tab8 |> as_flex_table() |> theme_vanilla(), path = "tables/jan2024/metrics_type_tempgran.docx")



########### check using multiple data sets and answering different primary questions
goal_numdat = clean_all |> select(ID, primary_goal, n_mob_data_sets)
goal_numdat = goal_numdat |> unnest(primary_goal)
goal_numdat_sum = goal_numdat %>% filter(is.na(primary_goal)==FALSE & is.na(n_mob_data_sets)==FALSE & n_mob_data_sets!="")
goal_numdat_sum = goal_numdat_sum |> filter(n_mob_data_sets !="11") |> group_by(primary_goal, n_mob_data_sets) |>
  summarize(count = n())
goal_numdat_sum = goal_numdat_sum |> ungroup() |> 
  group_by(primary_goal) |> mutate(total_gran = sum(count))
goal_numdat_sum = goal_numdat_sum |> rowwise() |> mutate(count_prop = count / total_gran)
goal_numdat_sum$n_mob_data_sets = factor(goal_numdat_sum$n_mob_data_sets)

ggplot(goal_numdat_sum |> filter(is.na(primary_goal)==FALSE),
       aes(fill = n_mob_data_sets , y = count_prop, x = primary_goal))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Number of mobility datasets") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  scale_fill_viridis(discrete=TRUE, option = "mako")+
  theme(text = element_text(size=20)) + xlab("Primary goal") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))


####### duration of analysis
# calculate duration
clean_all = clean_all |> mutate(duration_num = as.numeric(duration_analysis))
sum(is.na(clean_all$duration_num)==FALSE)

coefs <- coef(lm(duration_num ~ date, data = clean_all))

# make boxplot by date
clean_all |> ggplot(aes(x = date, y = duration_num, group = date)) + 
  geom_boxplot(color = "darkblue", lwd = 1) + 
  geom_abline(intercept = coefs[1], slope = coefs[2], color = "red", lwd=1)+
  theme_bw() + 
  xlab("Date") + ylab("Duration of analysis (days)")+
  theme(text = element_text(size=24))#+
  #theme(axis.text.x = element_text(size = 20))
summary(clean_all$duration_num)

# now do by primary goal
goal_dur = left_join(prim_df2 |> select(ID, primary_goal_col2),
                     clean_all |> select(ID, duration_num),
                     by = join_by(ID))
goal_dur = goal_dur |> ungroup() |> filter(is.na(primary_goal_col2) == FALSE) |> filter(is.na(duration_num)==FALSE)
goal_dur |> ggplot(aes(x = primary_goal_col2, y = duration_num, group = primary_goal_col2)) + 
  geom_boxplot(aes(color = primary_goal_col2), lwd = 1) + 
  theme_bw() + 
  xlab("Primary goal of paper") + ylab("Duration of analysis (days)")+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1)) + 
  theme(legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")


#### journals published - clean up the variable
# start off with 156 journals
# lower case for all
clean_all = clean_all |> mutate(journal_rec = tolower(journal)) # 149 journals
# delete trailing spaces
clean_all$journal_rec = trimws(clean_all$journal_rec) # 142 journals

clean_all = clean_all |> mutate(journal_rec = case_when(
  journal_rec %in% c("travel med infect dis") ~ "travel medicine and infectious disease",
  journal_rec %in% c("trans policy (oxf)") ~ "transport policy",
  journal_rec %in% c("sci rep") ~ "scientific reports",
  journal_rec %in% c("nonlinear dyn") ~ "nonlinear dynamics",
  journal_rec %in% c("nat med") ~ "nature medicine",
  journal_rec %in% c("nat commun") ~ "nature communications",
  journal_rec %in% c("the lancet") ~ "lancet",
  journal_rec %in% c("lancet id") ~ "lancet infectious diseases",
  journal_rec %in% c("j travel med") ~ "journal of travel medicine",
  journal_rec %in% c("int j infect dis", "int j infec disease") ~ "international journal of infectious diseases",
  journal_rec %in% c("chaos, solitons & fractals") ~ "chaos, solitons and fractals",
  journal_rec %in% c("bmc inf disease") ~ "bmc infectious diseases",
  grepl("pnas", journal_rec) ~ "pnas",
  grepl("proc natl acad sci", journal_rec) ~ "pnas",
  grepl("transp policy", journal_rec) ~ "transport policy",
  grepl("journal of preventive", journal_rec) ~ "journal of preventative medicine and public health",
  grepl("jmir public health", journal_rec) ~ "jmir public health and surveillance",
  grepl("int. j. neviron.", journal_rec) ~ "int j environ res public health",
  grepl("health policy plan", journal_rec) ~ "health policy and planning",
  grepl("disaster med public health prep", journal_rec) ~ "disaster medicine and public health preparedness",
  grepl("epidemiol infect", journal_rec) ~ "epidemiology and infection",
  grepl("int. j. environ. res. public health", journal_rec) ~ "international journal of environmental research and public health",
  grepl("int j environ res public health", journal_rec) ~ "international journal of environmental research and public health",
  grepl("j urban econ", journal_rec) ~ "journal of urban economics",
  journal_rec %in% "scientific reports" ~ "nature scientific reports",
  journal_rec %in% "journal of transportation and health" ~ "journal of transport and health",
  journal_rec %in% "journal of the royal society interface" ~ "journal of royal society interface",
  TRUE ~ journal_rec
))
length(unique(clean_all$journal_rec))
# reduced to 112 journals
sort(unique(clean_all$journal_rec))

clean_all = clean_all |> mutate(journal_rec2 = gsub("j ", "journal of ", journal_rec)) # doesn't change anything
length(unique(clean_all$journal_rec2))
sort(unique(clean_all$journal_rec2))

### model used for modeling SARS-CoV-2 transmission
table(clean_all$model_sars_cov_2)
clean_all |> ungroup() |> group_by(model_sars_cov_2) |> summarize(count = n()) |> filter(is.na(model_sars_cov_2)==FALSE & model_sars_cov_2 != "NA") |>
  filter(model_sars_cov_2 != "No") |> mutate(Percent = paste0(round(count / sum(count)*100), "%")) |> mutate(total = sum(count))



######### Figure 3 - integration and results

########### Mobility data integration

# clean the variable a little bit
clean_all = clean_all |> mutate(mobility_data_integrate_rev = case_when(
  grepl("co-location", mobility_data_integrate) ~ "Other",
  grepl("machine learning", mobility_data_integrate) ~ "Other",
  grepl("semi-mechanistically", mobility_data_integrate) ~ "Other",
  TRUE ~ mobility_data_integrate
))

clean_all |> select(mobility_data_integrate_rev) |> group_by(mobility_data_integrate_rev) |> summarize(count = n()) |> filter(mobility_data_integrate_rev != "") |>
  mutate(Percent = paste0(round(count / sum(count)*100), "%")) |> arrange(desc(count)) |> mutate(tot = sum(count)) |> flextable() |>
  save_as_docx(path = "tables/jan2024/mob_integrate.docx")


# make one for mechanistic / statistical
clean_all = clean_all |> mutate(mech_stat = case_when(
  grepl("mechan", mobility_data_integrate_rev)~ "Mechanistically",
  grepl("statistic", mobility_data_integrate_rev) ~ "Statistically",
  TRUE ~ mobility_data_integrate_rev
))

clean_all |> select(mech_stat) |> mutate(mech_stat = ifelse(grepl("Other", mech_stat), "Other", mech_stat)) |> group_by(mech_stat) |> summarize(count = n()) |> filter(mech_stat != "") |>
  mutate(Percent = paste0(round(count / sum(count)*100), "%")) |> arrange(desc(count)) |> mutate(tot = sum(count)) |> flextable() 

# # drop where no sars-cov-2 data
# ggplot(clean_all |> filter(is.na(mobility_data_integrate)==FALSE & mobility_data_integrate !="") |>
#          filter(mobility_data_integrate != "No SARS-CoV-2 data") |> filter(mobility_data_integrate != "Other-case report") |>
#          group_by(date, mobility_data_integrate_rev) |> summarize(count = n()),
#        aes(fill =  mobility_data_integrate_rev, y = count, x = date))+
#   geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
#   labs(fill = "Integration of mobility and SARS-CoV-2 data") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
#   #scale_fill_viridis(discrete=TRUE, option = "mako")+
#   scale_fill_brewer(palette = "Paired") + 
#   theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
#   theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))+
#   theme(legend.position = "none")
# 


clean_all = clean_all |> mutate(mobility_data_integrate_rev2 = case_when(
  mobility_data_integrate_rev %in% c("They are linked statistically via primarily correlation, exploratory (e.g. Pearson's correlation coefficient, rank test)") ~ "Statistically via primarily correlation, exploratory",
  grepl("They are linked statistically via regression", mobility_data_integrate_rev) ~ "Statistically via regression",
  mobility_data_integrate_rev %in% c("Other") ~ "Other",
  grepl("They do not", mobility_data_integrate_rev) ~ "Not linked - data sets analyzed separately",
  mobility_data_integrate_rev %in% c("They are integrated mechanistically via SEIR, SIR etc. ") ~ "Mechanistically via SEIR, SIR etc.",
  grepl("summary stats", mobility_data_integrate_rev) ~ "Statistically via summary stats",
  grepl("spatial structure", mobility_data_integrate_rev) ~ "Mechanistically via SEIR, SIR etc. with spatial structure",
  grepl("network", mobility_data_integrate_rev) ~ "Statistically via network models",
  grepl("contact structure", mobility_data_integrate_rev) ~ "Mechanistically via SEIR, SIR etc. with contact structure",
  grepl("age", mobility_data_integrate_rev) ~ "Mechanistically via SEIR, SIR etc. with age",
  grepl("individual-based", mobility_data_integrate_rev) ~ "Mechanistically via SEIR, SIR etc. - individual-based",
  grepl("non-linear regression", mobility_data_integrate_rev) ~ "Statistically via non-linear regression",
  TRUE ~ mobility_data_integrate_rev
))


# order
clean_all$mobility_data_integrate_rev2 = factor(clean_all$mobility_data_integrate_rev2, levels = c(
  unique(clean_all$mobility_data_integrate_rev2)[grepl("Mechanistically", unique(clean_all$mobility_data_integrate_rev2))==TRUE],
  unique(clean_all$mobility_data_integrate_rev2)[grepl("Statistically", unique(clean_all$mobility_data_integrate_rev2))==TRUE],
  unique(clean_all$mobility_data_integrate_rev2)[grepl("Not linked", unique(clean_all$mobility_data_integrate_rev2))==TRUE],
  unique(clean_all$mobility_data_integrate_rev2)[grepl("Other", unique(clean_all$mobility_data_integrate_rev2))==TRUE]
))

### try with color integration

# palette 1
pal1 = brewer.pal(n=9, name="Purples")[1:9]
pal2 = brewer.pal(n=9, name="Oranges")[1:9]

fig3a_pal = c(pal1[9], pal1[7], pal1[5], pal1[4], pal1[3],
              pal2[9], pal2[7], pal2[5], pal2[3], pal2[3],
              "olivedrab", "azure")

# Figure 3A
ggplot(clean_all |> filter(is.na(mobility_data_integrate)==FALSE & mobility_data_integrate !="") |>
         filter(mobility_data_integrate != "No SARS-CoV-2 data") |> filter(mobility_data_integrate != "Other-case report") |>
         group_by(date, mobility_data_integrate_rev2) |> summarize(count = n()),
       aes(fill =  mobility_data_integrate_rev2, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Integration of mobility and SARS-CoV-2 data") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_manual(values = fig3a_pal) + 
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 20,  hjust = 1)) #+ theme(legend.position = "none")



#### test mech / stat over time
temp = clean_all %>% select(date, ID, mech_stat) 
temp = temp |> mutate(mech_stat2 = case_when(
  grepl("Mechan", mech_stat) ~ "Mechanistically",
  grepl("Statistically", mech_stat) ~ "Statistically",
  grepl("No SARS-CoV-2", mech_stat) ~ NA,
  grepl("Other", mech_stat) ~ NA,
  mech_stat == "" ~ NA,
  grepl("They do not", mech_stat) ~ NA,
  TRUE ~ NA
))
temp = temp |> filter(is.na(mech_stat2)==FALSE) |> group_by(date) |> mutate(num_papers = length(unique(ID)))
temp = temp |> ungroup() |> group_by(date, mech_stat2) |> summarize(count = n(),
                                                                   num_papers = min(num_papers))
# make indicator variables
temp = temp |> mutate(ind_mech = ifelse(grepl("Mechan", mech_stat2), 1, 0),
                      ind_stat = ifelse(grepl("Stat", mech_stat2), 1, 0))
mod = glm(count ~ date + ind_mech , offset = num_papers, data = temp, family = "poisson")
summary(mod)
# save output
mech_stat_t = broom::tidy(mod) |> filter(term %in% c("date"))
mech_stat_t = mech_stat_t |> mutate(descr = "Mechanistic / statistical integration vs time",
                                categories = paste(toString(unique(temp$mech_stat2)), sep=","))


# logistic regression
temp2 = clean_all %>% select(date, ID, mech_stat) 
temp2 = temp2 |> mutate(mech_stat2 = case_when(
  grepl("Mechan", mech_stat) ~ "Mechanistically",
  grepl("Statistically", mech_stat) ~ "Statistically",
  grepl("No SARS-CoV-2", mech_stat) ~ NA,
  grepl("Other", mech_stat) ~ NA,
  mech_stat == "" ~ NA,
  grepl("They do not", mech_stat) ~ NA,
  TRUE ~ NA
))
# make a binary var
temp2 = temp2 |> mutate(mech = case_when(
  mech_stat2 %in% "Mechanistically" ~ 1,
  mech_stat2 %in% "Statistically" ~ 0,
  TRUE ~ NA
))
test = glm(mech ~ date, data = temp2, family = "binomial")
summary(test)


# plot it
ggplot(temp,
       aes(fill =  mech_stat2, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Integration") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_manual(values = c("#f2edee", pal1, pal2, "cornflowerblue")) + 
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 20,  hjust = 1)) #+ theme(legend.position = "none")




#### stat/mech and primary goal
stat_mech_goal = clean_all %>% select(date, ID, mech_stat) 
stat_mech_goal = stat_mech_goal |> mutate(mech_stat2 = case_when(
  grepl("Mechan", mech_stat) ~ "Mechanistically",
  grepl("Statistically", mech_stat) ~ "Statistically",
  grepl("No SARS-CoV-2", mech_stat) ~ NA,
  grepl("Other", mech_stat) ~ NA,
  mech_stat == "" ~ NA,
  grepl("They do not", mech_stat) ~ NA,
  TRUE ~ NA
))
stat_mech_goal = left_join(stat_mech_goal, prim_df2 |> select(ID, primary_goal_col2), by = join_by(ID))
stat_mech_goal = stat_mech_goal |> filter(is.na(mech_stat2)==FALSE)
stat_mech_goal_table = table(stat_mech_goal$mech_stat2, stat_mech_goal$primary_goal_col2)
stat_mech_goal_chi = broom::tidy(chisq.test(stat_mech_goal_table))
stat_mech_goal_chi

############ Mobility data in model

# clean the variable a little bit
clean_all = clean_all |> mutate(mob_in_mod = case_when(
  mobility_data_in_model %in% c("contact rate", "no trans model: other", "visual inspection") ~ "Other",
  grepl("statistical", mobility_data_in_model) ~ "Statistical",
  grepl("do not explicitly model", mobility_data_in_model) ~ "Don't explicitly model",
  grepl("Metapopulation", mobility_data_in_model) ~ "Metapopulation",
  TRUE ~ mobility_data_in_model
))

mob_model_date = clean_all |> select(date, mob_in_mod) |> filter(is.na(mob_in_mod)==FALSE) |>
  filter(!(mob_in_mod %in% c("Not specified", "Not applicable"))) |>
  group_by(date, mob_in_mod) |> summarize(count = n())

ggplot(mob_model_date, aes(fill =  mob_in_mod, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Integration of mobility \ninto transmission model") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  #scale_fill_brewer(palette = "Paired") + 
  scale_fill_tron()+
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))#+
  #theme(legend.position = "none")

# by primary goal
mob_model_goal = left_join(clean_all |> select(ID, mob_in_mod) |>filter(is.na(mob_in_mod)==FALSE) |>
                             filter(!(mob_in_mod %in% c("Not specified", "Not applicable"))),
                           prim_df2 |> select(ID, primary_goal_col2),
                           by = join_by(ID))
mob_model_goal = mob_model_goal |> group_by(primary_goal_col2, mob_in_mod) |> summarize(count = n())
mob_model_goal = mob_model_goal |> filter(is.na(primary_goal_col2)==FALSE) |> filter(is.na(mob_in_mod)==FALSE)
mob_model_goal = mob_model_goal |> ungroup() |> group_by(primary_goal_col2) |> mutate(total_metric = sum(count))
mob_model_goal = mob_model_goal |> rowwise() |> mutate(count_prop = count / total_metric)

ggplot(mob_model_goal ,
       aes(fill = mob_in_mod, y = count_prop, x = primary_goal_col2))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Integration of mobility\ninto transmission model") +#scale_x_discrete(guide = guide_axis(n.dodge=2))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  #scale_fill_brewer(palette = "Dark2")+ 
  scale_fill_tron()+
  #theme(legend.position = "none")+
  theme(text = element_text(size=20)) + xlab("Primary goal of paper") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))


##### Some results - "Do the authors use data to identify a change in mobility over some specified time?"
change_mob = data.frame(clean_all$change_mobility)
id_frame = data.frame(clean_all$ID)
date_frame = data.frame(clean_all$date)
change_mob = cbind(date_frame, id_frame, change_mob)
names(change_mob)[1:2] = c("date", "ID")
#change_mob= clean_all |> select(date, ID, change_mobility)
change_mob = change_mob |> pivot_longer(cols = -c(date, ID), names_to = "quest", values_to = "change_mob")
change_mob = change_mob |> filter(is.na(change_mob)==FALSE  & change_mob !="")
change_mob = change_mob |> mutate(change_mobility = case_when(
  grepl("Mixed", change_mob) ~ "Mixed change",
  grepl("Not applicable", change_mob) ~ "NA",
  grepl("No, did not assess", change_mob) ~ "NA",
  TRUE ~ change_mob
))

change_mob_date = change_mob |> select(date, change_mobility) |> filter(is.na(change_mobility)==FALSE) |>
  filter(!(change_mobility %in% c("NA"))) |>
  group_by(date, change_mobility) |> summarize(count = n())

ggplot(change_mob_date, aes(fill =  change_mobility, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Change in mobility") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Accent") + 
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))#+
#theme(legend.position = "none")

# don't drop NAs
change_mob_date = change_mob |> select(date, change_mobility) |> filter(is.na(change_mobility)==FALSE) |>
  group_by(date, change_mobility) |> summarize(count = n())

ggplot(change_mob_date, aes(fill =  change_mobility, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Change in mobility") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Accent") + 
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))#+
#theme(legend.position = "none")



######################### Results presented
res = clean_all |> select(date, ID, results_presented)
res = res |> unnest(results_presented)
res_ID = res
res = res |> select(date, results_presented) |> filter(is.na(results_presented)==FALSE) |>
  group_by(date, results_presented) |> summarize(count = n())

ggplot(res, aes(fill =  results_presented, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Results presented") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Accent") + 
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))+
theme(legend.position = "none")

# look by primary goal
res_goal = left_join(res_ID, prim_df2, by = join_by(ID))
res_goal = res_goal |> group_by(primary_goal_col2, results_presented) |> summarize(count = n())
res_goal = res_goal |> filter(is.na(primary_goal_col2)==FALSE) |> filter(is.na(results_presented)==FALSE)
res_goal = res_goal |> ungroup() |> group_by(primary_goal_col2) |> mutate(total_metric = sum(count))
res_goal = res_goal |> rowwise() |> mutate(count_prop = count / total_metric)

ggplot(res_goal ,
       aes(fill = results_presented, y = count_prop, x = primary_goal_col2))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Results presented") +#scale_x_discrete(guide = guide_axis(n.dodge=2))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Dark2")+ 
  theme(legend.position = "none")+
  theme(text = element_text(size=20)) + xlab("Primary goal of paper") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))




## look by type of mobility data
# look by primary goal
res_type = left_join(res_ID, type_mobility_longer |> select(ID, mobility_type_rec3), by = join_by(ID))
res_type = res_type |> group_by(mobility_type_rec3, results_presented) |> summarize(count = n())
res_type = res_type |> filter(is.na(mobility_type_rec3)==FALSE) |> filter(is.na(results_presented)==FALSE)
res_type = res_type |> ungroup() |> group_by(mobility_type_rec3) |> mutate(total_metric = sum(count))
res_type = res_type |> rowwise() |> mutate(count_prop = count / total_metric)

ggplot(res_type ,
       aes(fill = results_presented, y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Results presented") +#scale_x_discrete(guide = guide_axis(n.dodge=2))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Dark2")+ 
  theme(legend.position = "none")+
  theme(text = element_text(size=20)) + xlab("Primary goal of paper") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

# look by country - for top ten countries
res_coun = clean_all |> select(date, ID, results_presented, country_focus)
res_coun = res_coun |> unnest(results_presented)
res_coun = res_coun |> select(results_presented, country_focus) |> filter(is.na(results_presented)==FALSE) |>
  filter(country_focus %in% country_top10$country_focus) |>
  group_by(country_focus, results_presented) |> summarize(count = n())

ggplot(res_coun, aes(fill =  results_presented, y = count, x = country_focus))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Results presented") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Accent") + 
  theme(text = element_text(size=20)) + xlab("Country") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))+
  theme(legend.position = "none")





### null hypothesis
table(clean_all$null_hypothesis)

########################### Do the authors evaluate the effectiveness of (measured as a change in cases, deaths, R0, etc. that is causally attributed to) travel restrictions?
#### travel restrictions

causal_mob = data.frame(clean_all$causal_travel)
id_frame = data.frame(clean_all$ID)
date_frame = data.frame(clean_all$date)
causal_mob = cbind(date_frame, id_frame, causal_mob)
names(causal_mob)[1:2] = c("date", "ID")
#change_mob= clean_all |> select(date, ID, change_mobility)
causal_mob = causal_mob |> pivot_longer(cols = -c(date, ID), names_to = "quest", values_to = "causal_mob")
causal_mob = causal_mob |> filter(is.na(causal_mob)==FALSE & causal_mob !="")
causal_mob_ID = causal_mob |> select(date, ID, causal_mob)
causal_mob = causal_mob |> select(date, causal_mob)  |>
  group_by(date, causal_mob) |> summarize(count = n())

ggplot(causal_mob |> filter(causal_mob!=""), aes(fill =  causal_mob, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Effectiveness of travel restrictions") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Accent") + 
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))#+
  #theme(legend.position = "none")



# Let's look at results presented for the papers that looked at lockdown
ass_tr = causal_mob_ID |> filter(grepl("Yes", causal_mob))
nrow(ass_tr) #59 instances
length(unique(ass_tr$ID)) #50 instances
# keep just a unique ID
ass_tr = ass_tr |> ungroup() |> group_by(ID) |> slice(1) # 46 papers
ass_tr = ass_tr |> rename(travel_restrictions = causal_mob)
trav_res = left_join(ass_tr |> select(ID, travel_restrictions), res_ID,  by = join_by(ID)) |> filter(is.na(results_presented)==FALSE)
nrow(trav_res ) #80 instances
length(unique(trav_res$ID)) #48 papers
trav_res_sum = trav_res |> group_by(results_presented) |> summarize(count = n())
trav_res_sum = trav_res_sum |> mutate(Percent = paste0(round(count/(length(unique(trav_res$ID)))*100), "%"))
trav_res_sum = trav_res_sum |> arrange(desc(count))
trav_res_sum |> flextable() |> save_as_docx(path = "tables/jan2024/results_for_travel.docx")




### What are the actual results?
causal_trav = clean_all |> select(date, causal_travel_metric) 
causal_trav = causal_trav |> filter(is.na(causal_travel_metric)==FALSE) |> filter(causal_travel_metric !="NULL")
causal_trav = causal_trav |> unnest(causal_travel_metric)
causal_trav_ID = causal_trav
causal_trav = causal_trav |> ungroup() |> group_by(date, causal_travel_metric) |> summarize(count = n())
# make shorter labels
causal_trav = causal_trav |> mutate(causal_travel_metric = case_when(
  grepl("Decreased cases/deaths/R0 after travel restrictions", causal_travel_metric) ~ "Decreased cases/deaths/R0 after travel restrictions",
  grepl("Decreased cases/deaths/R0 before travel restrictions", causal_travel_metric) ~ "Decreased cases/deaths/R0 before travel restrictions",
  grepl("Increased cases/deaths/R0 after travel restrictions", causal_travel_metric) ~ "Increased cases/deaths/R0 after travel restrictions",
  grepl("Increased cases/deaths/R0 before travel restrictions", causal_travel_metric) ~ "Increased cases/deaths/R0 before travel restrictions",
  grepl("Mixed associations", causal_travel_metric) ~ "Mixed associations",
  TRUE ~ causal_travel_metric
)
)

# palette 1
fig3c_pal = c(pal_material("red")(7)[7], pal_material("red")(7)[4],
              pal_material("blue-grey")(6)[6], pal_material("blue-grey")(6)[3],
              "burlywood3", "purple", "azure")
# Figure 3C
ggplot(causal_trav |> filter(causal_travel_metric!=""), aes(fill =  causal_travel_metric, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Effectiveness of travel restrictions") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  #scale_fill_brewer(palette = "Accent") + 
  scale_fill_manual(values = fig3c_pal)+
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Count")+
  theme(legend.position = "none")




### Look by type of mobility data
trav_mobtype = left_join(causal_trav_ID, type_mobility_longer |> select(ID, mobility_type_rec3), by = join_by(ID))
trav_mobtype = trav_mobtype |> mutate(causal_travel_metric = case_when(
  grepl("Decreased cases/deaths/R0 after travel restrictions", causal_travel_metric) ~ "Decreased cases/deaths/R0 after travel restrictions",
  grepl("Decreased cases/deaths/R0 before travel restrictions", causal_travel_metric) ~ "Decreased cases/deaths/R0 before travel restrictions",
  grepl("Increased cases/deaths/R0 after travel restrictions", causal_travel_metric) ~ "Increased cases/deaths/R0 after travel restrictions",
  grepl("Increased cases/deaths/R0 before travel restrictions", causal_travel_metric) ~ "Increased cases/deaths/R0 before travel restrictions",
  grepl("Mixed associations", causal_travel_metric) ~ "Mixed associations",
  TRUE ~ causal_travel_metric
)
)
trav_mobtype = trav_mobtype |> ungroup() |> group_by(mobility_type_rec3, causal_travel_metric) |> summarize(count = n())
trav_mobtype = trav_mobtype |> ungroup() |> group_by(mobility_type_rec3) |> mutate(total_metric = sum(count))
trav_mobtype = trav_mobtype |> rowwise() |> mutate(count_prop = count / total_metric)

# Figure S16
ggplot(trav_mobtype ,
       aes(fill = causal_travel_metric, y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Effectiveness of travel restrictions") +#scale_x_discrete(guide = guide_axis(n.dodge=2))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_nejm()+
  #theme(legend.position = "none")+
  theme(text = element_text(size=20)) + xlab("Type of mobility data") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))




########################### lockdown --- effectiveness
causal_mob = data.frame(clean_all$causal_lockdown)
id_frame = data.frame(clean_all$ID)
date_frame = data.frame(clean_all$date)
causal_mob = cbind(date_frame, id_frame, causal_mob)
names(causal_mob)[1:2] = c("date", "ID")
#change_mob= clean_all |> select(date, ID, change_mobility)
causal_mob = causal_mob |> pivot_longer(cols = -c(date, ID), names_to = "quest", values_to = "causal_mob")
causal_mob_ID = causal_mob
causal_mob = causal_mob |> select(date, causal_mob) |> filter(is.na(causal_mob)==FALSE) |>
  group_by(date, causal_mob) |> summarize(count = n())

ggplot(causal_mob |> filter(causal_mob!=""), aes(fill =  causal_mob, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Effectiveness of lockdown / social distancing") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Accent") + 
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))#+
#theme(legend.position = "none")




### What are the actual results?
causal_lock= clean_all |> select(date, causal_lock_metric) 
causal_lock = causal_lock |> filter(is.na(causal_lock_metric)==FALSE) |> filter(causal_lock_metric !="NULL")
causal_lock = causal_lock |> unnest(causal_lock_metric)
causal_lock_ID = causal_lock
causal_lock = causal_lock |> ungroup() |> group_by(date, causal_lock_metric) |> summarize(count = n())
# make shorter labels
causal_lock = causal_lock |> mutate(causal_met_fac = case_when(
  grepl("Decreased cases/deaths/R0 after lockdown", causal_lock_metric) ~ "Decreased cases/deaths/R0 after lockdown",
  grepl("Decreased cases/deaths/R0 before lockdown", causal_lock_metric) ~ "Decreased cases/deaths/R0 before lockdown",
  grepl("Increased cases/deaths/R0 after lockdown", causal_lock_metric) ~ "Increased cases/deaths/R0 after lockdown",
  grepl("Increased cases/deaths/R0 before lockdown", causal_lock_metric) ~ "Increased cases/deaths/R0 before lockdown",
  grepl("Mixed associations", causal_lock_metric) ~ "Mixed associations",
  TRUE ~ causal_lock_metric
)
                                      )

# Figure 3B
ggplot(causal_lock |> filter(causal_lock_metric!=""), aes(fill =  causal_met_fac, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Effectiveness of lockdown") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE)+
  #scale_fill_brewer(palette = "Accent") + 
  scale_fill_manual(values = fig3c_pal)+
  scale_x_date(limits = as.Date(c("2020-01-01",  "2021-05-01")))+
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 20,  hjust = 1))+ theme(legend.position = "none")



### lockdown results presented by data type
lock_mobtype = left_join(causal_lock_ID, type_mobility_longer |> select(ID, mobility_type_rec3), by = join_by(ID))
lock_mobtype = lock_mobtype |> mutate(causal_met_fac = case_when(
  grepl("Decreased cases/deaths/R0 after lockdown", causal_lock_metric) ~ "Decreased cases/deaths/R0 after lockdown",
  grepl("Decreased cases/deaths/R0 before lockdown", causal_lock_metric) ~ "Decreased cases/deaths/R0 before lockdown",
  grepl("Increased cases/deaths/R0 after lockdown", causal_lock_metric) ~ "Increased cases/deaths/R0 after lockdown",
  grepl("Increased cases/deaths/R0 before lockdown", causal_lock_metric) ~ "Increased cases/deaths/R0 before lockdown",
  grepl("Mixed associations", causal_lock_metric) ~ "Mixed associations",
  TRUE ~ causal_lock_metric
)
)
lock_mobtype = lock_mobtype |> ungroup() |> group_by(mobility_type_rec3, causal_met_fac) |> summarize(count = n())
lock_mobtype = lock_mobtype |> ungroup() |> group_by(mobility_type_rec3) |> mutate(total_metric = sum(count))
lock_mobtype = lock_mobtype |> rowwise() |> mutate(count_prop = count / total_metric)

# Figure S14
ggplot(lock_mobtype ,
       aes(fill = causal_met_fac, y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Effectiveness of lockdown") +#scale_x_discrete(guide = guide_axis(n.dodge=2))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_nejm()+
  #theme(legend.position = "none")+
  theme(text = element_text(size=20)) + xlab("Type of mobility data") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))





# Let's look at results presented for the papers that looked at lockdown
assessed_lockdown = causal_mob_ID |> filter(grepl("Yes", causal_mob))
nrow(assessed_lockdown) #74 instances
length(unique(assessed_lockdown$ID)) #62 instances
# keep just a unique ID
assessed_lockdown = assessed_lockdown |> ungroup() |> group_by(ID) |> slice(1) # 62 papers
lock_res = left_join(assessed_lockdown |> select(ID, causal_mob), res_ID,  by = join_by(ID)) |> filter(is.na(results_presented)==FALSE)
nrow(lock_res ) #104 instances
length(unique(lock_res$ID)) #58 papers
lock_res_sum = lock_res |> group_by(results_presented) |> summarize(count = n())
lock_res_sum = lock_res_sum |> mutate(Percent = paste0(round(count/(length(unique(lock_res$ID)))*100), "%"))
lock_res_sum = lock_res_sum |> arrange(desc(count))
lock_res_sum |> flextable() |> save_as_docx(path = "tables/jan2024/results_for_lockdown.docx")




################## relationship between mobility and lockdown --- association
mob_lock= clean_all |> select(ID, date, lockdown) 
mob_lock = mob_lock |> filter(is.na(lockdown)==FALSE) |> filter(lockdown !="NULL") 
mob_lock = mob_lock |> unnest(lockdown)

mob_lock = mob_lock |> ungroup() |> group_by(date, lockdown) |> summarize(count = n())
ggplot(mob_lock |> filter(lockdown!=""), aes(fill =  lockdown, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Relationship between change in mobility\nand lockdown") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Accent") + 
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))#+theme(legend.position = "none")


### What are the actual results?
mob_lock_res = clean_all |> select(ID, date, mob_lo_res) 
mob_lock_res = mob_lock_res |> filter(is.na(mob_lo_res)==FALSE) |> filter(mob_lo_res !="NULL")
mob_lock_res = mob_lock_res |> unnest(mob_lo_res)
length(unique(mob_lock_res$ID)) #71
mob_lock_res = mob_lock_res |> ungroup() |> group_by(date, mob_lo_res) |> summarize(count = n())
mob_lock_res = mob_lock_res |> mutate(mob_lo_fac = case_when(
  grepl("Decreased mobility after lockdown", mob_lo_res) ~ "Decreased mobility after lockdown",
  grepl("Decreased mobility before lockdown", mob_lo_res) ~ "Decreased mobility before lockdown",
  grepl("Increased mobility before lockdown", mob_lo_res) ~ "Increased mobility before lockdown",
  grepl("Mixed associations", mob_lo_res) ~ "Mixed associations",
  TRUE ~ mob_lo_res
))
mob_lock_res$mob_lo_fac = factor(mob_lock_res$mob_lo_fac,
                                 levels = c("Decreased mobility after lockdown",
                                            "Decreased mobility before lockdown",
                                            "Increased mobility before lockdown",
                                            "Mixed associations",
                                            "No association", "Other"))

# Figure S15
# make same colors as for travel restrictions
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
(cols <- f("Accent"))                             

ggplot(mob_lock_res |> filter(mob_lo_fac!=""), aes(fill =  mob_lo_fac, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Association") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  #scale_fill_brewer(palette = "Accent") + 
  scale_fill_manual(values = c(cols[1:4], cols[6]))+
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, hjust = 1))#+theme(legend.position = "none")





### results by type of mobility data
mob_lock_res = clean_all |> select(ID, mob_lo_res) 
mob_lock_res = mob_lock_res |> filter(is.na(mob_lo_res)==FALSE) |> filter(mob_lo_res !="NULL")
mob_lock_res = mob_lock_res |> unnest(mob_lo_res)
length(unique(mob_lock_res$ID)) #71
mob_lock_res = mob_lock_res |> mutate(mob_lo_fac = case_when(
  grepl("Decreased mobility after lockdown", mob_lo_res) ~ "Decreased mobility after lockdown",
  grepl("Decreased mobility before lockdown", mob_lo_res) ~ "Decreased mobility before lockdown",
  grepl("Increased mobility before lockdown", mob_lo_res) ~ "Increased mobility before lockdown",
  grepl("Mixed associations", mob_lo_res) ~ "Mixed associations",
  TRUE ~ mob_lo_res
))
mob_lock_res$mob_lo_fac = factor(mob_lock_res$mob_lo_fac,
                                 levels = c("Decreased mobility after lockdown",
                                            "Decreased mobility before lockdown",
                                            "Increased mobility before lockdown",
                                            "Mixed associations",
                                            "No association", "Other"))

mob_lock_res = left_join(mob_lock_res, type_mobility_longer |> select(ID, mobility_type_rec3), by = join_by(ID))
mob_lock_res = mob_lock_res |> ungroup() |> group_by(mobility_type_rec3, mob_lo_res) |> summarize(count = n())
mob_lock_res = mob_lock_res |> ungroup() |> group_by(mobility_type_rec3) |> mutate(total_metric = sum(count))
mob_lock_res = mob_lock_res |> rowwise() |> mutate(count_prop = count / total_metric)

ggplot(mob_lock_res ,
       aes(fill = mob_lo_res, y = count_prop, x = mobility_type_rec3))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Effectiveness of lockdown") +#scale_x_discrete(guide = guide_axis(n.dodge=2))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_nejm()+
  #theme(legend.position = "none")+
  theme(text = element_text(size=20)) + xlab("Type of mobility data") + ylab("Proportion datasets")+
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

########################### change in mobility + transmission of SARS-CoV-2
mob_ass_trans= clean_all |> select(date, ID, mob_trans) 
mob_ass_trans = mob_ass_trans |> filter(is.na(mob_trans)==FALSE) |> filter(mob_trans !="NULL")
mob_ass_trans = mob_ass_trans |> unnest(mob_trans)

mob_ass_trans = mob_ass_trans |> ungroup() |> group_by(date, mob_trans) |> summarize(count = n())


ggplot(mob_ass_trans |> filter(mob_trans!=""), aes(fill =  mob_trans, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Assessed association") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Accent") + 
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))#+
#theme(legend.position = "none")

# how many papers?
mob_ass_trans_yes =  clean_all |> select(date, ID, mob_trans) 
mob_ass_trans_yes = mob_ass_trans_yes |> filter(is.na(mob_trans)==FALSE) |> filter(mob_trans !="NULL") |> filter(grepl("Yes", mob_trans))
length(unique(mob_ass_trans_yes$ID)) #127 papers


### What are the actual results?
mob_ass_find= clean_all |> select(date, mob_trans_res) 
mob_ass_find = mob_ass_find |> filter(is.na(mob_trans_res)==FALSE) |> filter(mob_trans_res !="NULL")
mob_ass_find = mob_ass_find |> unnest(mob_trans_res)

mob_ass_find = mob_ass_find |> ungroup() |> group_by(date, mob_trans_res) |> summarize(count = n())

mob_ass_find = mob_ass_find |> mutate(mob_trans_fac = case_when(
  grepl("Decreased mobility associated with decreased transmission", mob_trans_res) ~ "Decreased mobility, decreased transmission",
  grepl("Decreased mobility associated with increased transmission", mob_trans_res) ~ "Decreased mobility, increased transmission",
  grepl("Increased mobility associated with increased", mob_trans_res) ~ "Increased mobility, increased transmission",
  grepl("Increased mobility associated with decreased", mob_trans_res) ~ "Increased mobility, decreased transmission",
  grepl("Mixed associations", mob_trans_res) ~ "Mixed associations",
  grepl("not associated", mob_trans_res) ~ "No association",
  TRUE ~ mob_trans_res
))
mob_ass_find$mob_trans_fac = factor(mob_ass_find$mob_trans_fac,
                                    levels = c("Decreased mobility, decreased transmission",
                                               "Increased mobility, increased transmission",
                                               "Decreased mobility, increased transmission",
                                               "Increased mobility, decreased transmission",
                                               "Mixed associations",
                                               "No association",
                                               "Other"))
# make into a table
mob_ass_find |> select(mob_trans_fac, count) |> ungroup() |> group_by(mob_trans_fac) |> summarize(count = sum(count)) |> ungroup() |> 
  mutate(Percent = paste0(round(count / length(unique(mob_ass_trans_yes$ID))*100 ), "%")) |> arrange(desc(count)) |>
  flextable() |> save_as_docx(path = "tables/jan2024/transm_mobility.docx")
mob_ass_find |> select(mob_trans_fac, count) |> ungroup()  |> summarize(count = sum(count)) #151 instances


# Figure 3D
ggplot(mob_ass_find |> filter(mob_trans_fac!=""), aes(fill =  mob_trans_fac, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Association between mobility and transmission") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  #scale_fill_brewer(palette = "Accent") + 
  scale_fill_manual(values = c(pal_material("brown")(8)[7], pal_material("brown")(8)[4],
                               pal_material("green")(8)[8], pal_material("green")(8)[2],
                               "burlywood", "purple", "azure"))+
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 20, hjust = 1)) #+theme(legend.position = "none")






#################### mechanistically predict changes
mech_pred= clean_all |> select(date, mobility_data_mechanistic) 
mech_pred = mech_pred |> filter(is.na(mobility_data_mechanistic)==FALSE) |> filter(mobility_data_mechanistic !="NULL") 
mech_pred = mech_pred |> unnest(mobility_data_mechanistic)

mech_pred = mech_pred |> mutate(mob_mech = case_when(
  grepl("AND evaluate the accuracy", mobility_data_mechanistic) ~ "Yes, provide prediction accuracy; evaluate accuracy\nwith and without mobility data",
  grepl("authors provide measures of prediction accuracy", mobility_data_mechanistic) ~ "Yes, provide prediction accuracy",
  grepl("do not provide any measures", mobility_data_mechanistic) ~ "Yes, but don't provide measures of prediction accuracy",
  mobility_data_mechanistic %in% c("No", "Not applicable", "Other") ~ "No; Not applicable; Other",
  TRUE ~ mobility_data_mechanistic
))

mech_pred = mech_pred |> ungroup() |> group_by(date, mob_mech) |> summarize(count = n())

mech_pred = mech_pred |> filter(mob_mech !="")
mech_pred$mob_mech = factor(mech_pred$mob_mech,
                            levels = c("Yes, provide prediction accuracy; evaluate accuracy\nwith and without mobility data",
                                       "Yes, provide prediction accuracy",
                                       "Yes, but don't provide measures of prediction accuracy",
                                       "No; Not applicable; Other"))
# Figure S18B
ggplot(mech_pred |> filter(mob_mech!=""), aes(fill =  mob_mech, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Mechanically predict changes in SARS-CoV-2") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  #scale_fill_brewer(palette = "Accent") + 
  scale_fill_jco()+
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Count")

table(mech_pred$mob_mech) # 2 provided prediction accuracy and evaluate accuracy

################ statistically predict changes
stat_pred= clean_all |> select(date, mobility_data_statistical) 
stat_pred = stat_pred |> filter(is.na(mobility_data_statistical)==FALSE) |> filter(mobility_data_statistical !="NULL") 
stat_pred = stat_pred |> unnest(mobility_data_statistical)
stat_pred = stat_pred |> ungroup() |> group_by(date, mobility_data_statistical) |> summarize(count = n())

stat_pred = stat_pred |> mutate(mob_stat = case_when(
  grepl("AND evaluate the accuracy", mobility_data_statistical) ~ "Yes, provide prediction accuracy; evaluate accuracy\nwith and without mobility data",
  grepl("authors provide measures of prediction accuracy", mobility_data_statistical) ~ "Yes, provide prediction accuracy",
  grepl("do not provide any measures", mobility_data_statistical) ~ "Yes, but don't provide measures of prediction accuracy",
  mobility_data_statistical %in% c("No", "Not applicable") ~ "No; Not applicable; Other",
  TRUE ~ mobility_data_statistical
))

# align factors in the same levels as for mechanistic
stat_pred$mob_stat = factor(stat_pred$mob_stat, levels = levels(mech_pred$mob_mech))
# Figure S18A
ggplot(stat_pred |> filter(mob_stat!=""), aes(fill =  mob_stat, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Statistically predict changes in SARS-CoV-2") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  #scale_fill_brewer(palette = "Accent") + 
  scale_fill_jco()+
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))#+theme(legend.position = "none")

table(stat_pred$mob_stat) # 2 provided prediction accuracy and evaluate accuracy




### Look at whether mobility model was fit to the data
mob_mod = clean_all |> select(ID, mod_mobility)
mob_mod = mob_mod |> unnest(mod_mobility)
mob_mod = mob_mod |> filter(is.na(mod_mobility)==FALSE) # 295 instances (datasets), across 230 papers reporting
# get single entry for each combination
mob_mod = mob_mod |> group_by(ID, mod_mobility) |> slice(1) #241 instances, across 230 papers
# look to see if any had No and another type
mob_mod = mob_mod |> ungroup() |> group_by(ID) |> mutate(count_ID = n())
# drop No when count >1
mob_mod = mob_mod |> filter(!(count_ID>1 & mod_mobility=="No")) #236 instances, 230 papers
length_IDs = length(unique(mob_mod$ID))
mob_mod |> ungroup() |> group_by(mod_mobility) |> summarize(count = n()) |> mutate(Percent = paste0(round(count / length_IDs*100), "%")) |> arrange(desc(count)) |>
  flextable() |> save_as_docx(path = "tables/jan2024/model_mobility.docx")




### Look at whether mobility data were validated
mob_val = clean_all |> select(ID, mod_valid)
mob_val = mob_val |> unnest(mod_valid)
mob_val = mob_val |> filter(is.na(mod_valid)==FALSE) # 288 instances (datasets), across 231 papers reporting
# get single entry for each combination
mob_val = mob_val |> group_by(ID, mod_valid) |> slice(1) #235 instances, across 231 papers
# look to see if any had No and another type
mob_val = mob_val |> ungroup() |> group_by(ID) |> mutate(count_ID = n())
# drop No when count >1
mob_val = mob_val |> filter(!(count_ID>1 & grepl("No,", mod_valid))) #231 instances, 231 papers
length_IDs = length(unique(mob_val$ID))
mob_val |> ungroup() |> group_by(mod_valid) |> summarize(count = n()) |> mutate(Percent = paste0(round(count / length_IDs*100), "%")) |> arrange(desc(count)) |>
  flextable() |> save_as_docx(path = "tables/jan2024/mobility_validate.docx")




################### relationship between mobility and transmission parameters
trans_param= clean_all |> select(ID, date, transmission_param) 
trans_param = trans_param |> filter(is.na(transmission_param)==FALSE) |> filter(transmission_param !="NULL") 
trans_param = trans_param |> unnest(transmission_param)

trans_param = trans_param |> mutate(transmission_param = ifelse(grepl("Other -", transmission_param), "Other", transmission_param))
trans_param = trans_param |> group_by(ID, transmission_param) |> slice(1)
trans_param = trans_param |> ungroup() |> group_by(date, transmission_param) |> summarize(count = n())


ggplot(trans_param |> filter(transmission_param!=""), aes(fill =  transmission_param, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Assess relationship between mobility\nand trans. parameter") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Paired") + 
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))#+theme(legend.position = "none")



################## relationship between mobility and travel restrictions
mob_tr= clean_all |> select(ID, date, travel_restrictions) 
mob_tr = mob_tr |> filter(is.na(travel_restrictions)==FALSE) |> filter(travel_restrictions !="NULL") 
mob_tr = mob_tr |> unnest(travel_restrictions)
mob_tr_ID = mob_tr

mob_tr = mob_tr |> ungroup() |> group_by(date, travel_restrictions) |> summarize(count = n())
ggplot(mob_tr |> filter(travel_restrictions!=""), aes(fill =  travel_restrictions, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Assess relationship between change in mobility\nand travel restrictions") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Accent") + 
  theme(text = element_text(size=20)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 14, angle = 45,  hjust = 1))#+theme(legend.position = "none")


### What are the actual results?
mob_tr_res = clean_all |> select(ID, date, mob_res_res) 
mob_tr_res = mob_tr_res |> filter(is.na(mob_res_res)==FALSE) |> filter(mob_res_res !="NULL")
mob_tr_res = mob_tr_res |> unnest(mob_res_res)
mob_tr_res = mob_tr_res |> ungroup() |> group_by(date, mob_res_res) |> summarize(count = n())
# make shorter labels
mob_tr_res = mob_tr_res |> mutate(mob_res_fac = case_when(
  grepl("Decreased mobility after travel restrictions", mob_res_res) ~ "Decreased mobility after travel restrictions",
  grepl("Decreased mobility before travel restrictions", mob_res_res) ~ "Decreased mobility before travel restrictions",
  grepl("Increased mobility before travel restrictions", mob_res_res) ~ "Increased mobility before travel restrictions",
  grepl("Mixed associations", mob_res_res) ~ "Mixed associations",
  grepl("Other", mob_res_res) ~ "Other",
  grepl("No association", mob_res_res) ~ "No association",
  TRUE ~ mob_res_res
))

ggplot(mob_tr_res |> filter(mob_res_res!=""), aes(fill =  mob_res_fac, y = count, x = date))+
  geom_bar(position = "stack", stat = "identity", color = "black") + theme_bw() + 
  labs(fill = "Association") +#scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #scale_fill_viridis(discrete=TRUE, option = "mako")+
  scale_fill_brewer(palette = "Accent") + 
  theme(text = element_text(size=24)) + xlab("Date") + ylab("Count")+
  theme(axis.text.x = element_text(size = 20,  hjust = 1))#+theme(legend.position = "none")



##########################
# Table 2- regression test for change over time
change_time_table = rbind(geo_focus, stat_prim_goal, mech_stat_t, type_time)
change_time_table = change_time_table |> select(descr, estimate, p.value, categories)
change_time_table = change_time_table |> mutate(estimate = round(estimate, 4),
                                                p.value = round(p.value,4))

change_time_table = change_time_table |> mutate(p.value = ifelse(p.value == 0, "<0.0001", p.value))
as.data.frame(change_time_table) |> flextable() |> save_as_docx(path = "tables/jan2024/stat_test_time.docx")

#############################################################
##### Now look at abstracts
abs = read.csv("Data/COVID mobility review abstracts_AAbstracts_w_meta.csv")

# check why some were excluded after full review
full_exc = mobility_metrics |> filter(Does.this.paper.meet.the.criteria.for.full.review...Note..only.scientific.papers.where.the.primary.focus.is.on.mobility.data.being.used.to.understand.patterns.of.SARS.CoV.2.transmission.should.be.evaluated..If.you.click.no..this.survey.will.end.. == "No")
# pull those IDs from abstracts
abs_exc = abs |> filter(ID %in% as.numeric(full_exc$What.is.the.paper.ID.number..from.the.spreadsheet..))
table(abs_exc$Data)
