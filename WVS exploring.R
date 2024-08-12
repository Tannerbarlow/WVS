setwd("~/WVS")

library(tidyverse)
library(forcats)
load("~/wvs_africa.r")
#wvs_afr includes all the african countries as well as a few variables of interest to 
#me, will likely be edited later with measures the group agrees on


#replacing the numeric codes with N/A
wvs_afr <- wvs_afr %>% naniar::replace_with_na_all(condition = ~.x %in% c(-5, -4, -2, -1))

#creating a new variable for country name which stores each name as a lowercase string
#and deleting the COUNTRY_ALPHA variable from the original data
wvs_afr <- wvs_afr %>% mutate(country = case_when(COUNTRY_ALPHA == "BFA" ~ 'burkina faso',
                                                  COUNTRY_ALPHA == "DZA" ~ 'algeria', 
                                                  COUNTRY_ALPHA == "EGY" ~ 'egypt',
                                                  COUNTRY_ALPHA == "ETH" ~ 'ethiopia',
                                                  COUNTRY_ALPHA == "GHA" ~ 'ghana',
                                                  COUNTRY_ALPHA == "KEN" ~ 'kenya',
                                                  COUNTRY_ALPHA == "LBY" ~ 'libya',
                                                  COUNTRY_ALPHA == "MLI" ~ 'mali',
                                                  COUNTRY_ALPHA == "MAR" ~ 'morocco',
                                                  COUNTRY_ALPHA == "NGA" ~ 'nigeria',
                                                  COUNTRY_ALPHA == "RWA" ~ 'rwanda',
                                                  COUNTRY_ALPHA == "TUN" ~ 'tunisia',
                                                  COUNTRY_ALPHA == "TZA" ~ 'tanzania', 
                                                  COUNTRY_ALPHA == "UGA" ~ 'uganda',
                                                  COUNTRY_ALPHA == "ZAF" ~ 'south africa',
                                                  COUNTRY_ALPHA == "ZMB" ~ 'zambia',
                                                  COUNTRY_ALPHA == "ZWE" ~ 'zimbabwe',
                                                  TRUE ~ 'other')) %>%
  select(-COUNTRY_ALPHA)

wvs_afr <- wvs_afr %>% mutate(weight = case_when(S017 == "0" ~ 'undefined',
                                                 S017 == "1" ~ 'no weighting',
                                                 TRUE ~ 'other')) %>%
  select(-S017)

wvs_afr <- wvs_afr %>% rename(year = S020)

wvs_afr <- wvs_afr %>% mutate(trust = case_when(A165 == "1" ~ 'can trust',
                                                A165 == "2" ~ 'need to be very careful',
                                                TRUE ~ 'N/A')) %>%
  select(-A165)

wvs_afr <- wvs_afr %>% mutate(advantage = case_when(A168 == "1" ~ 'would take advantage',
                                                    A168 == "2" ~ 'would be fair',
                                                    TRUE ~'N/A')) %>%
  select(-A168)

wvs_afr <- wvs_afr %>% mutate(future = case_when(B017 == "1" ~ 'bright',
                                                 B017 == "2" ~ 'bleak', 
                                                 B017 == "3" ~ 'both', 
                                                 B017 == "4" ~ 'neither',
                                                 B017 == "5" ~ 'other', 
                                                 TRUE ~ 'N/A')) %>%
  select(-B017)

wvs_afr <- wvs_afr %>% rename(finan_satis = C006)

wvs_afr <- wvs_afr %>% mutate(indiv = case_when(D079 == "1" ~ 'agree strongly',
                                                D079 == "2" ~ 'agree', 
                                                D079 == "3" ~ 'disgree',
                                                D079 == "4" ~ 'disagree strongly',
                                                TRUE ~ 'N/A')) %>%
  select(-D079)

wvs_afr <- wvs_afr %>% mutate(pol_int = case_when(E023 == "1" ~ 'very interested',
                                                  E023 == "2" ~ 'somewhat interested',
                                                  E023 == "3" ~ 'not very interested',
                                                  E023 == "4" ~ 'not at all interested',
                                                  TRUE ~ 'N/A')) %>%
  select(-E023)

wvs_afr <- wvs_afr %>% rename(polit_id = E033)

wvs_afr <- wvs_afr %>% mutate(church_conf = case_when(E069_01 == "1" ~ 'a great deal',
                                                      E069_01 == "2" ~ 'quite a lot',
                                                      E069_01 == "3" ~ 'not very much',
                                                      E069_01 == "4" ~ 'none at all', 
                                                      TRUE ~ 'N/A')) %>%
  select(-E069_01)

wvs_afr <- wvs_afr %>% mutate(press_conf = case_when(E069_04 == "1" ~ 'a great deal',
                                                     E069_04 == "2" ~ 'quite a lot',
                                                     E069_04 == "3" ~ 'not very much',
                                                     E069_04 == "4" ~ 'none at all',
                                                     TRUE ~ 'N/A')) %>%
  select(-E069_04)

wvs_afr <- wvs_afr %>% mutate(police_conf = case_when(E069_06 == "1" ~ 'a great deal',
                                                      E069_06 == "2" ~ 'quite a lot',
                                                      E069_06 == "3" ~ 'not very much',
                                                      E069_06 == "4" ~ 'none at all', 
                                                      TRUE ~ 'N/A')) %>%
  select(-E069_06)

wvs_afr <- wvs_afr %>% mutate(par_conf = case_when(E069_07 == "1" ~ 'a great deal',
                                                      E069_07 == "2" ~ 'quite a lot',
                                                      E069_07 == "3" ~ 'not very much',
                                                      E069_07 == "4" ~ 'none at all', 
                                                      TRUE ~ 'N/A')) %>%
  select(-E069_07)

wvs_afr <- wvs_afr %>% mutate(civil_serv_conf = case_when(E069_08 == "1" ~ 'a great deal',
                                                      E069_08 == "2" ~ 'quite a lot',
                                                      E069_08 == "3" ~ 'not very much',
                                                      E069_08 == "4" ~ 'none at all', 
                                                      TRUE ~ 'N/A')) %>%
  select(-E069_08)

wvs_afr <- wvs_afr %>% mutate(gov_conf = case_when(E069_11 == "1" ~ 'a great deal',
                                                      E069_11 == "2" ~ 'quite a lot',
                                                      E069_11 == "3" ~ 'not very much',
                                                      E069_11 == "4" ~ 'none at all', 
                                                      TRUE ~ 'N/A')) %>%
  select(-E069_11)

wvs_afr <- wvs_afr %>% mutate(courts_conf = case_when(E069_17 == "1" ~ 'a great deal',
                                                      E069_17 == "2" ~ 'quite a lot',
                                                      E069_17 == "3" ~ 'not very much',
                                                      E069_17 == "4" ~ 'none at all', 
                                                      TRUE ~ 'N/A')) %>%
  select(-E069_17)

wvs_afr <- wvs_afr %>% mutate(democ_view = case_when(E117 == "1" ~ 'very good',
                                                      E117 == "2" ~ 'fairly good',
                                                      E117 == "3" ~ 'fairly bad',
                                                      E117 == "4" ~ 'very bad', 
                                                      TRUE ~ 'N/A')) %>%
  select(-E117)

wvs_afr <- wvs_afr %>% mutate(nat_pol_satis = case_when(E125 == "1" ~ 'very satisfied',
                                                      E125 == "2" ~ 'fairly satisfied',
                                                      E125 == "3" ~ 'fairly dissatisfied',
                                                      E125 == "4" ~ 'very dissatisfied',
                                                      TRUE ~ 'N/A')) %>%
  select(-E125)

wvs_afr <- wvs_afr %>% mutate(pol_news = case_when(E150 == "1" ~ 'every day',
                                                      E150 == "2" ~ 'several times a week',
                                                      E150 == "3" ~ 'once or twice a week',
                                                      E150 == "4" ~ 'less often',
                                                   E150 == "5" ~ 'never', 
                                                      TRUE ~ 'N/A')) %>%
  select(-E150)

wvs_afr <- wvs_afr %>% rename(democ_import = E235)

wvs_afr <- wvs_afr %>% mutate(relig_mem = case_when(F024 == "0" ~ 'no',
                                                    F024 == "1" ~ 'yes',
                                                    TRUE ~ 'N/A')) %>%
  select(-F024)

wvs_afr <- wvs_afr %>% mutate(relig_denom = case_when(F025 == "0" ~ 'none',
                                                      F025 == "1" ~ 'catholic',
                                                      F025 == "2" ~ 'protestant',
                                                      F025 == "3" ~ 'orthodox',
                                                      F025 == "4" ~ 'jewish',
                                                      F025 == "5" ~ 'muslim',
                                                      F025 == "6" ~ 'hindu', 
                                                      F025 == "7" ~ 'buddhist',
                                                      F025 == "8" ~ 'other christian',
                                                      F025 == "9" ~ 'other',
                                                      TRUE ~ 'N/A')) %>%
  select(-F025)

wvs_afr <- wvs_afr %>% mutate(attend = case_when(F028 == "1" ~ 'more than once a week',
                                                 F028 == "2" ~ 'once a week',
                                                 F028 == "3" ~ 'once a month',
                                                 F028 == "4" ~ 'only on christmas/easter',
                                                 F028 == "5" ~ 'only on other holy days',
                                                 F028 == "6" ~ 'once a year', 
                                                 F028 == "7" ~ 'less than once a year',
                                                 F028 == "8" ~ 'never/practically never',
                                                 TRUE ~ 'N/A')) %>%
  select(-F028)

wvs_afr <- wvs_afr %>% mutate(raised_relig = case_when(F029 == "0" ~ 'no',
                                                       F029 == "1" ~ 'yes',
                                                       TRUE ~ 'N/A')) %>%
  select(-F029)

wvs_afr <- wvs_afr %>% mutate(ntl_pride = case_when(G006 == "1" ~ 'very proud',
                                                    G006 == "2" ~ 'quite proud',
                                                    G006 == "3" ~ 'not very proud',
                                                    G006 == "4" ~ 'not at all proud', 
                                                    TRUE ~ 'N/A')) %>%
  select(-G006)

wvs_afr <- wvs_afr %>% mutate(rob_in_hood = case_when(H002_01 == "1" ~ 'very frequent',
                                                  H002_01 == "2" ~ 'quite frequent',
                                                  H002_01 == "3" ~ 'not frequent',
                                                  H002_01 == "4" ~ 'not at all frequent',
                                                  TRUE ~ 'N/A')) %>%
  select(-H002_01)

wvs_afr <- wvs_afr %>% mutate(pol_int_in_hood = case_when(H002_03 == "1" ~ 'very frequent',
                                                      H002_03 == "2" ~ 'quite frequent',
                                                      H002_03 == "3" ~ 'not frequent',
                                                      H002_03 == "4" ~ 'not at all frequent',
                                                      TRUE ~ 'N/A')) %>%
  select(-H002_03)

wvs_afr <- wvs_afr %>% mutate(crime_vict = case_when(H004 == "0" ~ 'no',
                                                     H004 == "1" ~ 'yes', 
                                                     TRUE ~ 'N/A')) %>%
  select(-H004)

wvs_afr <- wvs_afr %>% mutate(sex = case_when(X001 == "1" ~ 'male',
                                              X001 == "2" ~ 'female', 
                                              TRUE ~ 'N/A')) %>%
  select(-X001)

wvs_afr <- wvs_afr %>% mutate(marital = case_when(X007 == "1" ~ 'married', 
                                                  X007 == "2" ~ 'cohabitating',
                                                  X007 == "3" ~ 'divorced',
                                                  X007 == "4" ~ 'separated', 
                                                  X007 == "5" ~ 'widowed',
                                                  X007 == "6" ~ 'single/never married',
                                                  TRUE ~ 'N/A')) %>%
  select(-X007)

wvs_afr <- wvs_afr %>% mutate(children = case_when(X011 == "0" ~ 'none',
                                                   X011 == "1" ~ '1',
                                                   X011 == "2" ~ '2', 
                                                   X011 == "3" ~ '3', 
                                                   X011 == "4" ~ '4', 
                                                   X011 == "5" ~ '5 or more', 
                                                   TRUE ~ 'N/A')) %>%
  select(-X011)

wvs_afr <- wvs_afr %>% mutate(edu = case_when(X025 == "1" ~ 'incomplete elementary',
                                              X025 == "2" ~ 'complete elementary',
                                              X025 == "3" ~ 'incomplete vocational',
                                              X025 == "4" ~ 'complete vocational',
                                              X025 == "5" ~ 'incomplete secondary',
                                              X025 == "6" ~ 'complete secondary',
                                              X025 == "7" ~ 'some university', 
                                              X025 == "8" ~ 'university',
                                              TRUE ~ 'N/A')) %>%
  select(-X025)

wvs_afr <- wvs_afr %>% mutate(employ = case_when(X028 == "1" ~ 'full time',
                                                 X028 == "2" ~ 'part time',
                                                 X028 == "3" ~ 'self employed',
                                                 X028 == "4" ~ 'retired', 
                                                 X028 == "5" ~ 'housewife',
                                                 X028 == "6" ~ 'students',
                                                 X028 == "7" ~ 'unemployed',
                                                 X028 == "8" ~ 'other', 
                                                 TRUE ~ 'N/A')) %>%
  select(-X028)

wvs_afr <- wvs_afr %>% mutate(self_class = case_when(X045 == "1" ~ 'upper class',
                                                     X045 == "2" ~ 'upper middle class',
                                                     X045 == "3" ~ 'lower middle class',
                                                     X045 == "4" ~ 'working class',
                                                     X045 == "5" ~ 'lower class',
                                                     TRUE ~ 'N/A')) %>%
  select(-X045)

wvs_afr <- wvs_afr %>% mutate(age = case_when(X003R == "1" ~ '15-24',
                                              X003R == "2" ~ '25-34',
                                              X003R == "3" ~ '35-44',
                                              X003R == "4" ~ '45-54',
                                              X003R == "5" ~ '55-64',
                                              X003R == "6" ~ '65 or older',
                                              TRUE ~ 'N/A')) %>%
  select(-X003R)


wvs_afr <- wvs_afr %>% mutate(finan_satis = as_factor(finan_satis),
                              polit_id = as_factor(polit_id), democ_import = as_factor(democ_import), 
                              country = as_factor(country), trust = as_factor(trust), 
                              advantage = as_factor(advantage), indiv = as_factor(indiv), 
                              pol_int = as_factor(pol_int), church_conf = as_factor(church_conf),
                              press_conf = as_factor(press_conf), police_conf = as_factor(police_conf), 
                              par_conf = as_factor(par_conf), civil_serv_conf = as_factor(civil_serv_conf),
                              gov_conf = as_factor(gov_conf), courts_conf = as_factor(courts_conf),
                              democ_view = as_factor(democ_view),
                              nat_pol_satis = as_factor(nat_pol_satis), pol_news = as_factor(pol_news),
                              relig_mem = as_factor(relig_mem), relig_denom = as_factor(relig_denom),
                              attend = as_factor(attend), raised_relig = as_factor(raised_relig),
                              ntl_pride = as_factor(ntl_pride), rob_in_hood = as_factor(rob_in_hood), 
                              pol_int_in_hood = as_factor(pol_int_in_hood), crime_vict = as_factor(crime_vict),
                              sex = as_factor(sex), marital = as_factor(marital),
                              children = as_factor(children), 
                              employ = as_factor(employ), self_class = as_factor(self_class), 
                              age = as_factor(age))

wvs_afr <- wvs_afr %>% mutate(year = as.numeric(year))

wvs_afr <- wvs_afr %>% mutate(weight = as.numeric(weight))

wvs_afr <- wvs_afr %>% mutate(wave = case_when(year == "1981" | year == "1982" | year == "1983" |
                                                 year == "1984" ~ '1',
                                               year == "1990" | year == "1991" | year == "1992" |
                                                 year == "1993" | year == "1994" ~ '2',
                                               year == "1995" | year == "1996" | year == "1997" |
                                                 year == "1998" ~ '3', 
                                               year == "1999" | year == "2000" | year == "2001" |
                                                 year == "2002" | year == "2003" | year == "2004" ~ '4',
                                               year == "2005" | year == "2006" | year == "2007" |
                                                 year == "2008" | year == "2009" ~ '5',
                                               year == "2010" | year == "2011" | year == "2012" |
                                                 year == "2013" | year == "2014" ~ '6',
                                               year == "2017" | year == "2018" | year == "2019" |
                                                 year == "2020" | year == "2021" | year == "2022" ~ '7',
                                               TRUE ~ 'N/A'))

#factoring in weights 
options(na.action = "na.pass")

wvs_afr_weights <- wvs_afr %>% drop_na(trust) %>% filter(trust != "iap") %>%
  srvyr::as_survey_design(weights = weight, nest = TRUE)
  
view(wvs_afr)
 #trust by country
no_na_trust <- wvs_afr %>% filter(trust != "N/A")

trust_by_country <- no_na_trust %>% group_by(country, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N) , pct = round((freq*100), 0)) 

p0 <- ggplot(data = trust_by_country, mapping = aes(x = country, y = pct, fill = trust)) 

p1 <- p0 + geom_col(position = "dodge2") + labs(x = "Country", y = "Percent", 
                                                title = "Trust Levels by Country", 
                                                subtitle = "data from WVS, 1982-2022", 
                                                fill = "Can you trust others?")

p1 + theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

#trust by religion faceted by country
no_na_relig <- no_na_trust %>% filter(relig_denom != "N/A")

trust_by_relig <- no_na_relig %>% group_by(relig_denom, country, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100), 0))

p0 <- ggplot(data = trust_by_relig, mapping = aes(x = relig_denom, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2")  + facet_wrap(~country, ncol = 4)

p1 + labs(x = "Country", y = "Percent", title = "Trust by Religion by Country", 
          subtitle = "Data from WVS, 1982-2022", fill = "Can you trust others?") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

#trust by class faceted by country
no_na_class <- no_na_trust %>% filter(self_class != "N/A")

trust_by_class <- no_na_class %>% group_by(self_class, country, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100), 0))

p0 <- ggplot(data = trust_by_class, mapping = aes(x = self_class, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2")  + facet_wrap(~country, ncol = 4) + 
  scale_x_discrete(limits = c("lower class", "working class", "lower middle class", 
                              "upper middle class", "upper class"))

p1 + labs(x = "Country", y = "Percent", title = "Trust by Class by Country", 
          subtitle = "Data from WVS, 1982-2022", fill = "Can you trust others?") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))


#categorizing countries by number of available years
#many years = more than 4
#few years = 3-4
many_years <- c("nigeria", "south africa")
  
few_years <- c("egypt", "morocco", "zimbabwe")
  
two_years <- c("algeria", "ethiopia", "ghana", "libya", "rwanda", "tunisia")
  
one_year <- c("burkina faso", "kenya", "mali", "tanzania", "uganda", "zambia")
#timeseries

#trust over time
#this is a poor representation as countries are going in an out by the year, some data
#points probably only represent one country
#peaks and valleys may have more to do with which countries are entering/exiting than
#a continental trend

trust_over_time <- no_na_trust %>% group_by(year, trust) %>% summarize(N= n()) %>%
  mutate(freq = N/sum(N), pct= round((freq*100), 0))

trust_over_time <- trust_over_time %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_over_time, mapping = aes(x = year, y = pct, color = trust,
                                                   ymax = 50)) 

p1 <- p0 + geom_line(aes(group = trust), linewidth = 2)  

p1 + labs(x = "Year", y = "Percent", title = "Trust over Time in Africa", 
          subtitle = "Data from WVS, 1982-2022", color = "Can you trust others?") +
  theme(legend.position = "top")

#faceted by country for few/many year countries
trust_over_time <- no_na_trust %>% group_by(year, country, trust) %>% summarize(N= n()) %>%
  mutate(freq = N/sum(N), pct= round((freq*100), 0))

trust_over_time <- trust_over_time %>% filter(country %in% many_years| 
                                                country %in% few_years)

p0 <- ggplot(data = trust_over_time, mapping = aes(x = year, y = pct, color = trust)) 

p1 <- p0 + geom_line(aes(group = trust), linewidth = 2) + facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent", title = "Trust over Time in Africa", 
          subtitle = "Data from WVS, 1982-2022", color = "Can you trust others?") +
  theme(legend.position = "top") 


#just South Africa
trust_over_time <- no_na_trust %>% group_by(country, year, trust) %>% summarize(N= n()) %>%
  mutate(freq = N/sum(N), pct= round((freq*100), 0))

south_afr <- trust_over_time %>% filter(country == "south africa") %>% 
  filter(trust != "need to be very careful")

p0 <- ggplot(data = south_afr, mapping = aes(x = year, y = pct, color = trust,
                                             ymax = 40, ymin = 0)) 

p1 <- p0 + geom_line(aes(group = trust), linewidth = 2)

p1 + labs(x = "Year", y = "Percent", title = "Trust over Time in South Africa", 
          subtitle = "Data from WVS, 1982-2022", color = "Can you trust others?") +
  theme(legend.position = "top")

#bar plots for two year countries 
trust_over_time <- no_na_trust %>% group_by(year, country, trust) %>% summarize(N= n()) %>%
  mutate(freq = N/sum(N), pct= round((freq*100), 0))

trust_over_time <- trust_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_over_time, mapping = aes(x = year, y = pct, fill = trust)) 

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent", title = "Trust over Time in Africa", 
          subtitle = "Data from WVS, 1982-2022", fill = "Can you trust others?") +
  theme(legend.position = "top")

#bar plot for one year countries
trust_over_time <- no_na_trust %>% group_by(country, trust) %>% summarize(N= n()) %>%
  mutate(freq = N/sum(N), pct= round((freq*100), 0))

trust_over_time <- trust_over_time %>% filter(country %in% one_year)

p0 <- ggplot(data = trust_over_time, mapping = aes(x = country, y = pct, fill = trust)) 

p1 <- p0 + geom_col(position = "dodge2")  

p1 + labs(x = "Country", y = "Percent", title = "Trust Levels in Africa", 
          subtitle = "Data from WVS, 1982-2022", fill = "Can you trust others?") +
  theme(legend.position = "top") 

#trust and sex over time
#continental time series
no_na_sex <- no_na_trust %>% filter(sex!= "N/A")

trust_by_sex <- no_na_sex %>% group_by(sex, year, trust) %>%
  summarize(N= n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0)) 

trust_by_sex <- trust_by_sex %>% filter(trust != "need to be very careful") 

p0 <- ggplot(data = trust_by_sex, mapping = aes(x = year, y = pct, color = sex))

p1 <- p0 + geom_line(aes(group = sex), linewidth = 2, alpha = 0.6)

p1 + labs(x = "Year", y = "Percent", color = "Sex",
          title = "Percentage of People Who Believe They Can Trust Others", 
          subtitle = "African Countries, 1982-2022", caption = "Data from WVS") +
  theme(legend.position = "top")

#faceted by country for many/few years
trust_by_sex <- no_na_sex %>% group_by(year, country, sex, trust) %>%
  summarize(N= n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0)) 

trust_by_sex <- trust_by_sex %>% filter(country %in% many_years | country %in% few_years) %>%
  filter(trust != "need to be very careful") 

p0 <- ggplot(data = trust_by_sex, mapping = aes(x = year, y = pct, color = sex)) 

p1 <- p0 + geom_line(aes(group = sex), linewidth = 2) + facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent", color = "Sex",
          title = "Percentage of People Who Believe They Can Trust Others", 
          subtitle = "African Countries, 1982-2022", caption = "Data from WVS") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

#faceted bar plots for two years
trust_by_sex <- no_na_sex %>% group_by(year, country, sex, trust) %>%
  summarize(N= n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0)) 

trust_by_sex <- trust_by_sex %>% filter(country %in% two_years) %>%
  filter(trust != "need to be very careful") 

p0 <- ggplot(data = trust_by_sex, mapping = aes(x = year, y = pct, fill = sex))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent", fill = "Sex",
          title = "Percentage of People Who Believe They Can Trust Others", 
          subtitle = "African Countries, 1982-2022", caption = "Data from WVS") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

#bar plot for one year countries
trust_by_sex <- no_na_sex %>% group_by(country, sex, trust) %>%
  summarize(N= n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0)) 

trust_by_sex <- trust_by_sex %>% filter(country %in% one_year) %>%
  filter(trust != "need to be very careful") 

p0 <- ggplot(data = trust_by_sex, mapping = aes(x = country, y = pct, fill = sex))

p1 <- p0 + geom_col(position = "dodge2") 

p1 + labs(x = "Year", y = "Percent", fill= "Sex",
          title = "Percentage of People Who Believe They Can Trust Others", 
          subtitle = "African Countries, 1982-2022", caption = "Data from WVS") +
  theme(legend.position = "top")

#trust and education over time
#continental time series

no_na_edu <- no_na_trust %>% filter(edu != "N/A") %>% 
  mutate(edu = fct_relevel(edu,"incomplete elementary", "elementary", 
                           "incomplete vocational", "complete vocational",
                           "incomplete secondary", "complete secondary",
                           "some university", "university"))

trust_by_edu <- no_na_edu %>% group_by(year, edu, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0))

view(trust_by_edu)

trust_by_edu <- trust_by_edu %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_edu, mapping = aes(x = year, y = pct))

p1 <- p0 + geom_line(aes(group = edu), linewidth = 2, color = "pink") + 
  facet_wrap(~factor(edu, levels = c('incomplete elementary', 'elementary',
                                  'incomplete vocational', 'complete vocational',
                                  'incomplete secondary', 'complete secondary',
                                  'some university', 'university')))

p1 + labs(x = "Year", y = "Percent", color = "Highest Educational Attainment", 
          title = "Percentage of People who Believe they can Trust Others", 
          subtitle = "African Continent, 1982-2022") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#faceted for many/few year countries
trust_by_edu <- no_na_edu %>% group_by(year, country, edu, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0))

trust_by_edu <- trust_by_edu %>% filter(trust != "need to be very careful")

trust_by_edu <- trust_by_edu %>% filter(country %in% many_years | 
                                          country %in% few_years)

p0 <- ggplot(data = trust_by_edu, mapping = aes(x = year, y = pct, color = edu))

p1 <- p0 + geom_line(aes(group = edu), linewidth = 2) + 
  facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent", color = "Highest Educational Attainment", 
          title = "Percentage of People who Believe they can Trust Others", 
          subtitle = "African Continent, 1982-2022")

#faceted bar plots for two year countries
trust_by_edu <- no_na_edu %>% group_by(year, country, edu, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0))

trust_by_edu <- trust_by_edu %>% filter(trust != "need to be very careful")

trust_by_edu <- trust_by_edu %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_by_edu, mapping = aes(x = year, y = pct, fill = edu))

p1 <- p0 + geom_col(position = "dodge2") + 
  facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent", fill = "Highest Educational Attainment", 
          title = "Percentage of People who Believe they can Trust Others", 
          subtitle = "African Continent, 1982-2022")
#just rwanda
trust_by_edu <- no_na_edu %>% group_by(year, country, edu, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0))

trust_by_edu <- trust_by_edu %>% filter(trust != "need to be very careful") %>% 
  filter(country == "rwanda")

p0 <- ggplot(data = trust_by_edu, mapping = aes(x = year, y = pct, fill = edu))

p1 <- p0 + geom_col(position = "dodge2")

p1 + labs(x = "Year", y = "Percent", fill = "Highest Educational Attainment", 
          title = "Percentage of People who Believe they can Trust Others", 
          subtitle = "African Continent, 1982-2022")
#bar plot for one year countries
trust_by_edu <- no_na_edu %>% group_by(country, edu, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0))

trust_by_edu <- trust_by_edu %>% filter(trust != "need to be very careful")

trust_by_edu <- trust_by_edu %>% filter(country %in% one_year)

p0 <- ggplot(data = trust_by_edu, mapping = aes(x = country, y = pct, fill = edu))

p1 <- p0 + geom_col(position = "dodge2")

p1 + labs(x = "Year", y = "Percent", fill = "Highest Educational Attainment", 
          title = "Percentage of People who Believe they can Trust Others", 
          subtitle = "African Continent, 1982-2022")

#trust and attendance over time
#continental time series

no_na_attend <- no_na_trust %>% filter(attend != "N/A") %>% 
  mutate(attend = fct_relevel(attend, "more than once a week", "once a week", 
                              "once a month", "only on christmas/easter", 
                              "only on other holy days", "once a year",
                              "less than once a year", "never/practically never"))

trust_by_attend <- no_na_attend %>% group_by(year, attend, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0)) 

trust_by_attend <- trust_by_attend %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_attend, mapping = aes(x = year, y = pct)) 

p1 <- p0 + geom_line(aes(group = attend), linewidth = 2) + facet_wrap(~attend) 

p1 + labs(x = "Year", y = "Percent", 
          title = "Percentage of People who Believe they can Trust Others and Religious Service Attendance",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS")

#faceted by country for many/few countries
no_na_attend <- no_na_trust %>% filter(attend != "N/A") %>% 
  mutate(attend = fct_relevel(attend, "more than once a week", "once a week", 
                              "once a month", "only on christmas/easter", 
                              "only on other holy days", "once a year",
                              "less than once a year", "never/practically never"))

trust_by_attend <- no_na_attend %>% group_by(year, country, attend, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0)) 

trust_by_attend <- trust_by_attend %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_attend, mapping = aes(x = year, y = pct, color = attend))

p1 <- p0 + geom_line(aes(group = attend), linewidth = 2) + facet_wrap(~country) 

p1 + labs(x = "Year", y = "Percent", 
          title = "Percentage of People who Believe they can Trust Others and Religious Service Attendance",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS", 
          attend = "Religious Service Attendance")

#faceted bar plots for two year countries
no_na_attend <- no_na_trust %>% filter(attend != "N/A") %>% 
  mutate(attend = fct_relevel(attend, "more than once a week", "once a week", 
                              "once a month", "only on christmas/easter", 
                              "only on other holy days", "once a year",
                              "less than once a year", "never/practically never"))

trust_by_attend <- no_na_attend %>% group_by(year, country, attend, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0)) 

trust_by_attend <- trust_by_attend %>% filter(trust != "need to be very careful") %>%
  filter(country %in% two_years)

p0 <- ggplot(data = trust_by_attend, mapping = aes(x = year, y = pct, fill = attend))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country) 

p1 + labs(x = "Year", y = "Percent", 
          title = "Percentage of People who Believe they can Trust Others and Religious Service Attendance",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS", 
          fill = "Religious Service Attendance")

#bar plot for one year countries
no_na_attend <- no_na_trust %>% filter(attend != "N/A") %>% 
  mutate(attend = fct_relevel(attend, "more than once a week", "once a week", 
                              "once a month", "only on christmas/easter", 
                              "only on other holy days", "once a year",
                              "less than once a year", "never/practically never"))

trust_by_attend <- no_na_attend %>% group_by(year, country, attend, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0)) 

trust_by_attend <- trust_by_attend %>% filter(trust != "need to be very careful") %>%
  filter(country %in% one_year)

p0 <- ggplot(data = trust_by_attend, mapping = aes(x = country, y = pct, fill = attend)) 

p1 <- p0 + geom_col(position = "dodge2") 

p1 + labs(x = "Year", y = "Percent", 
          title = "Percentage of People who Believe they can Trust Others and Religious Service Attendance",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS", 
          fill = "Religious Service Attendance")

#trust and marital over time
#removed morocco and libya because they each had one cohabitating respondent for years
#where they were the only countries with data which skewed trends
no_na_marital <- no_na_trust %>% filter(marital != "N/A") %>% 
  filter(country != "libya" & country != "morocco")

trust_by_marital <- no_na_marital %>% group_by(year, marital, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0))

trust_by_marital <- trust_by_marital %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_marital, mapping = aes(x = year, y = pct, color = marital))

p1 <- p0 + geom_line(aes(group = marital), linewidth = 2, alpha = 0.6) 

p1 + labs(x = "Year", y = "Percent",
          title = "People who Believe They can Trust Others by Marital Status", 
          subtitle = "African Continent, 1982-2022", caption = "data from WVS", 
          color = "Marital Status")  + 
  theme(legend.position = "top")

#faceted by country for many/few years
no_na_marital <- no_na_trust %>% filter(marital != "N/A") 

trust_by_marital <- no_na_marital %>% group_by(year, country, marital, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0))

trust_by_marital <- trust_by_marital %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_marital, mapping = aes(x = year, y = pct, color = marital))

p1 <- p0 + geom_line(aes(group = marital), linewidth = 2, alpha = 0.6)  + 
  facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent",
          title = "People who Believe They can Trust Others by Marital Status", 
          subtitle = "African Continent, 1982-2022", caption = "data from WVS", 
          color = "Marital Status")  + 
  theme(legend.position = "top")

#faceted bar plots for two year countries
#filtering out cohabitating because very few respondents selected this option
no_na_marital <- no_na_trust %>% filter(marital != "N/A")  %>% filter(marital != "cohabitating")

trust_by_marital <- no_na_marital %>% group_by(year, country, marital, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0))

trust_by_marital <- trust_by_marital %>% filter(trust != "need to be very careful") %>%
  filter(country %in% two_years)

p0 <- ggplot(data = trust_by_marital, mapping = aes(x = year, y = pct, fill = marital))

p1 <- p0 + geom_col(position = "dodge2")  + 
  facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent",
          title = "People who Believe They can Trust Others by Marital Status", 
          subtitle = "African Continent, 1982-2022", caption = "data from WVS", 
          fill = "Marital Status")  + 
  theme(legend.position = "top")

#bar plot for one year countries
no_na_marital <- no_na_trust %>% filter(marital != "N/A") 

trust_by_marital <- no_na_marital %>% group_by(year, country, marital, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100), 0))

trust_by_marital <- trust_by_marital %>% filter(trust != "need to be very careful") %>%
  filter(country %in% one_year)

p0 <- ggplot(data = trust_by_marital, mapping = aes(x = country, y = pct, fill = marital))

p1 <- p0 + geom_col(position = "dodge2")

p1 + labs(x = "Year", y = "Percent",
          title = "People who Believe They can Trust Others by Marital Status", 
          subtitle = "African Continent, 1982-2022", caption = "data from WVS", 
          fill = "Marital Status")  + 
  theme(legend.position = "top")

