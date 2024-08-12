#trust----
#overall timeseries
trust_over_time <- no_na_trust %>% group_by(wave, trust) %>% summarize(N= n()) %>%
  mutate(freq = N/sum(N), pct= round((freq*100), 0))

trust_over_time <- trust_over_time %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_over_time, mapping = aes(x = wave, y = pct, color = trust,
                                                   ymax = 50)) 

p1 <- p0 + geom_line(aes(group = trust), linewidth = 2)  

p1 + labs(x = "Survey Wave", y = "Percent", title = "Trust over Time in Africa", 
          subtitle = "Data from WVS, 1982-2022", color = "Can you trust others?") +
  theme(legend.position = "top")
#3+ year country timeseries
trust_over_time <- no_na_trust %>% group_by(wave, country, trust) %>% summarize(N= n()) %>%
  mutate(freq = N/sum(N), pct= round((freq*100), 0))

trust_over_time <- trust_over_time %>% filter(country %in% many_years| 
                                                country %in% few_years)

p0 <- ggplot(data = trust_over_time, mapping = aes(x = wave, y = pct, color = trust)) 

p1 <- p0 + geom_line(aes(group = trust), linewidth = 2) + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", title = "Trust over Time in Africa", 
          subtitle = "Data from WVS, 1982-2022", color = "Can you trust others?") +
  theme(legend.position = "top") 

#2 year bar plot
trust_over_time <- no_na_trust %>% group_by(wave, country, trust) %>% summarize(N= n()) %>%
  mutate(freq = N/sum(N), pct= round((freq*100), 0))

trust_over_time <- trust_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_over_time, mapping = aes(x = wave, y = pct, fill = trust)) 

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", title = "Trust over Time in Africa", 
          subtitle = "Data from WVS, 1982-2022", fill = "Can you trust others?") +
  theme(legend.position = "top")


#age----
#overall timeseries
age_over_time <- no_na_age %>% group_by(wave, age) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

p0 <- ggplot(data = age_over_time, mapping = aes(x = wave, y = pct, color = age))

p1 <- p0 + geom_line(aes(group = age_over_time$age), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Age Groups", title = "Age of Respondents over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")

#3+ year country timeseries
age_over_time <- no_na_age %>% group_by(wave, country, age) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

age_over_time <- age_over_time %>% filter(country %in% many_years| country %in% few_years)

p0 <- ggplot(data = age_over_time, mapping = aes(x = wave, y = pct, color = age))

p1 <- p0 + geom_line(aes(group = age_over_time$age), linewidth = 2) + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Age Groups", title = "Age of Respondents over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")

#2 year bar plot
age_over_time <- no_na_age %>% group_by(wave, country, age) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

age_over_time <- age_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = age_over_time, mapping = aes(x = wave,  y = pct, fill = age))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Age Groups", title = "Age of Respondents over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "right")

#edu----
#overall timeseries
edu_over_time <- no_na_edu %>% group_by(wave, edu) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

p0 <- ggplot(data = edu_over_time, mapping = aes(x = wave, y = pct, color = edu))

p1 <- p0 + geom_line(aes(group = edu_over_time$edu), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Highest Educational Attainment", 
          title = "Education over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "left")
#timeseries for just university
edu_over_time <- no_na_edu %>% group_by(wave, edu) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

uni_over_time <- edu_over_time %>% filter(edu == "university")

p0 <- ggplot(data = uni_over_time, mapping = aes(x = wave, y = pct, ymin= 0, ymax = 25))

p1 <- p0 + geom_line(aes(group = uni_over_time$edu), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", 
          title = "Proportion Completing University over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "left")
#timeseries for incomplete elementary
edu_over_time <- no_na_edu %>% group_by(wave, edu) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

inc_elem_over_time <- edu_over_time %>% filter(edu == "incomplete elementary")

p0 <- ggplot(data = inc_elem_over_time, mapping = aes(x = wave, y = pct))

p1 <- p0 + geom_line(aes(group = inc_elem_over_time$edu), linewidth = 2) + ylim(0,25) 

p1 + labs(x = "Year", y = "Percent", 
          title = "Proportion with Less than Elementary Education over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "left")

#3+ year country timeseries
edu_over_time <- no_na_edu %>% group_by(wave, country, edu) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

edu_over_time <- edu_over_time %>% filter(country %in% many_years| country %in% few_years)

p0 <- ggplot(data = edu_over_time, mapping = aes(x = wave, y = pct, color = edu))

p1 <- p0 + geom_line(aes(group = edu_over_time$edu), linewidth = 2) + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Educational Attainment",
          title = "Education over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "left")

#2 year bar plot
edu_over_time <- no_na_edu %>% group_by(wave, country, edu) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

edu_over_time <- edu_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = edu_over_time, mapping = aes(x = wave,  y = pct, fill = edu))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Educational Attainment",
          title = "Education over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "right")


#sex----
#overall timeseries
sex_over_time <- no_na_sex %>% group_by(wave, sex) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

p0 <- ggplot(data = sex_over_time, mapping = aes(x = wave, y = pct, color = sex))

p1 <- p0 + geom_line(aes(group = sex_over_time$sex), linewidth = 2)  + ylim(0,100)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Sex", 
          title = "Sex of Respondents over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")

#3+ year country timeseries
sex_over_time <- no_na_sex %>% group_by(wave, country, sex) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

sex_over_time <- sex_over_time %>% filter(country %in% many_years| country %in% few_years)

p0 <- ggplot(data = sex_over_time, mapping = aes(x = wave, y = pct, color = sex))

p1 <- p0 + geom_line(aes(group = sex_over_time$sex), linewidth = 2) + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Sex",
          title = "Sex of Respondents over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")
#just egypt
sex_over_time <- sex_over_time %>% filter(country == 'egypt')

p0 <- ggplot(data = sex_over_time, mapping = aes(x = wave, y = pct, color = sex))

p1 <- p0 + geom_line(aes(group = sex_over_time$sex), linewidth = 2) + ylim(30, 70) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Sex", 
          title = "Sex of Respondents over Time", 
          subtitle = "Egypt", caption = "data from WVS") + 
  theme(legend.position = "top")
#2 year bar plot
sex_over_time <- no_na_sex %>% group_by(wave, country, sex) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

sex_over_time <- sex_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = sex_over_time, mapping = aes(x = wave,  y = pct, fill = sex))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Sex",
          title = "Sex of Respondents over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")


#immigrant----
#overall timeseries
no_na_immigrant <- wvs_afr_other %>% filter(immigrant != "N/A")

immigrant_over_time <- no_na_immigrant %>% filter(country %in% has_immigrants |
                                                    country %in% no_immigrants)

immigrant_over_time <- no_na_immigrant %>% group_by(wave, immigrant) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round ((freq*100),0))

p0 <- ggplot(data = immigrant_over_time, mapping = aes(x = wave, y = pct, color = immigrant))

p1 <- p0 + geom_line(aes(group = immigrant), linewidth = 2)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Immigrant Status", 
          title = "Immigrant Status Over Time", subtitle = "African Continent, 1982-2022", 
          caption = "data from WVS") + theme(legend.position = "top")

#just immigrants timeseries
no_na_immigrant <- wvs_afr_other %>% filter(immigrant != "N/A")

immigrant_over_time <- no_na_immigrant %>% filter(country %in% has_immigrants |
                                                    country %in% no_immigrants)

immigrant_over_time <- no_na_immigrant %>% group_by(wave, immigrant) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round ((freq*100),0))

immigrant_only <- immigrant_over_time %>% filter(immigrant == "immigrant")

p0 <- ggplot(data = immigrant_only, mapping = aes(x = wave, y = pct, color = immigrant))

p1 <- p0 + geom_line(aes(group = immigrant), linewidth = 2) + ylim(0,5)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Immigrant Status", 
          title = "Immigrant Status Over Time", subtitle = "African Continent, 1982-2022", 
          caption = "data from WVS") + theme(legend.position = "top")

#3+ year country timeseries
no_na_immigrant <- wvs_afr_other %>% filter(immigrant != "N/A")

immigrant_over_time <- no_na_immigrant %>% group_by(wave, country, immigrant) %>% 
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

immigrant_over_time <- immigrant_over_time %>% 
  filter(country %in% has_immigrants | country %in% no_immigrants) 

immigrant_over_time <- immigrant_over_time %>%
  filter(country %in% many_years | country %in% few_years) %>% 
  filter(country != "south africa")

immigrant_over_time <- immigrant_over_time %>% filter(immigrant == "born in country")


p0 <- ggplot(data = immigrant_over_time, mapping = aes(x = wave, y = pct, ymin = 90,
                                                       ymax = 100,
                                                       color = immigrant))

p1 <- p0 + geom_line(aes(group = immigrant), linewidth = 2) + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Immigrant Status", title = "Immigration Over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")


#2 year bar plot
no_na_immigrant <- wvs_afr_other %>% filter(immigrant != "N/A")

immigrant_over_time <- no_na_immigrant %>% group_by(wave, country, immigrant) %>% 
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

immigrant_over_time <- immigrant_over_time %>% 
  filter(country == "libya" | country == "tunisia")

immigrant_over_time <- immigrant_over_time %>%
  filter(country %in% two_years)

p0 <- ggplot(data = immigrant_over_time, mapping = aes(x = wave, y = pct,
                                                       fill = immigrant))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Immigrant Status", title = "Immigration Over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")



#religion----
#overall timeseries
relig_over_time <- no_na_relig_denom %>% 
  group_by(wave, relig_denom) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))


p0 <- ggplot(data = relig_over_time, mapping = aes(x = wave, y = pct, color = relig_denom))

p1 <- p0 + geom_line(aes(group = relig_over_time$relig_denom), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Religion", 
          title = "Religion over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")

#3+ year country timeseries
relig_over_time <- no_na_relig_denom %>% group_by(wave, country, relig_denom) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

relig_over_time <- relig_over_time %>% filter(country %in% many_years| country %in% few_years)

p0 <- ggplot(data = relig_over_time, mapping = aes(x = wave, y = pct, color = relig_denom))

p1 <- p0 + geom_line(aes(group = relig_over_time$relig_denom), linewidth = 2) +
  facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Religion",
          title = "Religion over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "right")

#2 year bar plot
relig_over_time <- no_na_relig_denom %>% group_by(wave, country, relig_denom) %>% 
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

relig_over_time <- relig_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = relig_over_time, mapping = aes(x = wave,  y = pct, fill = relig_denom))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country, ncol = 2)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Religion",
          title = "Religion over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "right")


#attend----
#overall timeseries
attend_over_time <- no_na_attend %>% group_by(wave, attend) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

p0 <- ggplot(data = attend_over_time, mapping = aes(x = wave, y = pct, color = attend))

p1 <- p0 + geom_line(aes(group = attend_over_time$attend), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Religious Service Attendance", 
          title = "Religious Service Attendance over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "left")

#3+ year country timeseries
attend_over_time <- no_na_attend %>% group_by(wave, country, attend) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

attend_over_time <- attend_over_time %>% filter(country %in% many_years| country %in% few_years)

p0 <- ggplot(data = attend_over_time, mapping = aes(x = wave, y = pct, color = attend))

p1 <- p0 + geom_line(aes(group = attend_over_time$attend), linewidth = 2) +
  facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Religious Service Attendance",
          title = "Religion Service Attendance over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "right")

#2 year bar plot
attend_over_time <- no_na_attend %>% group_by(wave, country, attend) %>% 
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

attend_over_time <- attend_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = attend_over_time, mapping = aes(x = wave,  y = pct, fill = attend))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country, ncol = 2)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Religious Service Attendance",
          title = "Religious Service Attendance Over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "right")



#income----
#overall timeseries
no_na_income <- wvs_afr_other %>% filter(income != "N/A")

income_over_time <- no_na_income %>% group_by(wave, income) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

income_over_time <- income_over_time %>% mutate(income = fct_relevel(income, "low", 
                                                                     "medium", "high"))

p0 <- ggplot(data = income_over_time, mapping = aes(x = wave, y = pct, 
                                                    color = income))

p1 <- p0 + geom_line(aes(group = income), linewidth = 2)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Income Level", title = "Income Over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")

#3+ year country timeseries
no_na_income <- wvs_afr_other %>% filter(income != "N/A")

income_over_time <- no_na_income %>% group_by(wave, country, income) %>% 
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

income_over_time <- income_over_time %>% mutate(income = fct_relevel(income, "low", 
                                                                     "medium", "high")) %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = income_over_time, mapping = aes(x = wave, y = pct, 
                                                    color = income))

p1 <- p0 + geom_line(aes(group = income), linewidth = 2) + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Income Level", title = "Income Over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")

#2 year bar plot
no_na_income <- wvs_afr_other %>% filter(income != "N/A")

income_over_time <- no_na_income %>% group_by(wave, country, income) %>% 
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

income_over_time <- income_over_time %>% mutate(income = fct_relevel(income, "low", 
                                                                     "medium", "high")) %>%
  filter(country %in% two_years)

p0 <- ggplot(data = income_over_time, mapping = aes(x = wave, y = pct, 
                                                    fill = income))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Income Level", title = "Income Over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")


#employ----
#overall timeseries
employ_over_time <- no_na_employ %>% group_by(wave, employ) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

p0 <- ggplot(data = employ_over_time, mapping = aes(x = wave, y = pct, color = employ))

p1 <- p0 + geom_line(aes(group = employ_over_time$employ), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Employment Status", 
          title = "Employment Status over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "top")

#3+ year country timeseries
employ_over_time <- no_na_employ %>% group_by(wave, country, employ) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

employ_over_time <- employ_over_time %>% filter(country %in% many_years| country %in% few_years)

p0 <- ggplot(data = employ_over_time, mapping = aes(x = wave, y = pct, color = employ))

p1 <- p0 + geom_line(aes(group = employ_over_time$employ), linewidth = 2) +
  facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Employment Status",
          title = "Employment Status over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "right")

#2 year bar plot
employ_over_time <- no_na_employ %>% group_by(wave, country, employ) %>% 
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

employ_over_time <- employ_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = employ_over_time, mapping = aes(x = wave, y = pct, fill = employ))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country, ncol = 2)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Employment Status",
          title = "Employment Status Over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "right")

#political----
#overall timeseries
political_over_time <- no_na_political %>% group_by(wave, political) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

p0 <- ggplot(data = political_over_time, mapping = aes(x = wave, y = pct, color = political))

p1 <- p0 + geom_line(aes(group = political_over_time$political), linewidth = 2) +
  scale_color_brewer(palette = "RdBu", direction = -1)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Political Identification", 
          title = "Political Identification Over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "left")

#3+ year country timeseries
political_over_time <- no_na_political %>% group_by(wave, country, political) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

political_over_time <- political_over_time %>% filter(country %in% many_years | 
                                                        country %in% few_years)

p0 <- ggplot(data = political_over_time, mapping = aes(x = wave, y = pct, color = political))

p1 <- p0 + geom_line(aes(group = political), linewidth = 2) + facet_wrap(~country) +
  scale_color_brewer(palette = "RdBu", direction = -1)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Political Identification", 
          title = "Political Identification over Time", subtitle = "African Continent, 1982-2022",
          caption = "data from WVS") + theme(legend.position = "top")

#2 year bar plot
political_over_time <- no_na_political %>% group_by(wave, country, political) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

political_over_time <- political_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = political_over_time, mapping = aes(x = wave, y = pct, fill = political))

p1 <- p0 + geom_col(position = "dodge2") + scale_fill_brewer(palette = "RdBu", direction = -1) +
  facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Political Identification",
          title = "Political Identification Over Time", subtitle = "African Continent, 1982-2022", 
          caption = "data from WVS") + theme(legend.position = "top")



#group_pol----
#overall timeseries
group_pol_over_time <- no_na_group_pol %>% group_by(wave, group_pol) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

p0 <- ggplot(data = group_pol_over_time, mapping = aes(x = wave, y = pct, color = group_pol))

p1 <- p0 + geom_line(aes(group = group_pol_over_time$group_pol), linewidth = 2) +
  scale_color_brewer(palette = "RdBu", direction = -1)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Political Identification", 
          title = "Political Identification Over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") + 
  theme(legend.position = "left")

#3+ year country timeseries
group_pol_over_time <- no_na_group_pol %>% group_by(wave, country, group_pol) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

group_pol_over_time <- group_pol_over_time %>% filter(country %in% many_years | 
                                                        country %in% few_years)

p0 <- ggplot(data = group_pol_over_time, mapping = aes(x = wave, y = pct, color = group_pol))

p1 <- p0 + geom_line(aes(group = group_pol), linewidth = 2) + facet_wrap(~country) +
  scale_color_brewer(palette = "RdBu", direction = -1)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Political Identification", 
          title = "Political Identification over Time", subtitle = "African Continent, 1982-2022",
          caption = "data from WVS") + theme(legend.position = "top")

#2 year bar plot
group_pol_over_time <- no_na_group_pol %>% group_by(wave, country, group_pol) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

group_pol_over_time <- group_pol_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = group_pol_over_time, mapping = aes(x = wave, y = pct, fill = group_pol))

p1 <- p0 + geom_col(position = "dodge2") + scale_fill_brewer(palette = "RdBu", direction = -1) +
  facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Political Identification",
          title = "Political Identification Over Time", subtitle = "African Continent, 1982-2022", 
          caption = "data from WVS") + theme(legend.position = "top")


#marital----
#overall timeseries
no_na_marital <- no_na_trust %>% filter(marital != "N/A")

marital_over_time <- no_na_marital %>% group_by(wave, marital) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

p0 <- ggplot(data = marital_over_time, mapping = aes(x = wave, y = pct, color = marital))

p1 <- p0 + geom_line(aes(group = marital), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Marital Status", 
          title = "Marital Status over Time", subtitle = "African Continent, 1982-2022",
          caption = "data from WVS") + theme(legend.position = "top")
             
             
#3+ year country timeseries
marital_over_time <- no_na_marital %>% group_by(wave, country, marital) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

marital_over_time <- marital_over_time %>% filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = marital_over_time, mapping = aes(x = wave, y = pct, color = marital))

p1 <- p0 + geom_line(aes(group = marital), linewidth = 2) + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Marital Status", 
          title = "Marital Status over Time", subtitle = "African Continent, 1982-2022",
          caption = "data from WVS") + theme(legend.position = "top")

#2 year bar plot
marital_over_time <- no_na_marital %>% group_by(wave, country, marital) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

marital_over_time <- marital_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = marital_over_time, mapping = aes(x = wave, y = pct, fill = marital))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Marital Status", 
          title = "Marital Status over Time", subtitle = "African Continent, 1982-2022",
          caption = "data from WVS") + theme(legend.position = "top")

#trust and age----
#overall timeseries
trust_by_age <- no_na_age %>% group_by(wave, age, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_age <- trust_by_age %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_age, mapping = aes(x = wave, y = pct, color = age))

p1 <- p0 + geom_line(aes(group = age), linewidth = 2) 

p1 + labs(x = "Wave", y = "Percent", color = "Age Groups",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#3+ year country timeseries
trust_by_age <- no_na_age %>% group_by(wave, country, age, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_age <- trust_by_age %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_age, mapping = aes(x = wave, y = pct, color = age))

p1 <- p0 + geom_line(aes(group = age), linewidth = 2)  + facet_wrap(~country)

p1 + labs(x = "Wave", y = "Percent", color = "Age Groups",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
trust_by_age <- no_na_age %>% group_by(wave, country, age, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_age <- trust_by_age %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_by_age, mapping = aes(x = wave, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2") + facet_grid(country~age)

p1 + labs(x = "Wave", y = "Percent", fill = "Can you trust others?", 
          title = "Age and Trust Over Time", subtitle = "data from WVS") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))


#trust and edu----
#overall timeseries
trust_by_edu <- no_na_edu %>% group_by(wave, edu, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 


trust_by_edu <- trust_by_edu %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_edu, mapping = aes(x = wave, y = pct, color = edu))

p1 <- p0 + geom_line(aes(group = edu), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Educational Attainment",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#3+ year country timeseries
trust_by_edu <- no_na_edu %>% group_by(wave, country, edu, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_edu <- trust_by_edu %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_edu, mapping = aes(x = wave, y = pct, color = edu))

p1 <- p0 + geom_line(aes(group = edu), linewidth = 2)  + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Education",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
two_years_edu <- c("algeria", "ghana", "rwanda")

trust_by_edu <- no_na_edu %>% group_by(wave, country, edu, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_edu <- trust_by_edu %>% filter(country %in% two_years_edu)

p0 <- ggplot(data = trust_by_edu, mapping = aes(x = wave, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2") + facet_grid(country~edu)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Can you trust others?", 
          title = "Education and Trust Over Time", subtitle = "data from WVS") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

#trust and sex----
#overall timeseries
trust_by_sex <- no_na_sex %>% group_by(wave, sex, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 


trust_by_sex <- trust_by_sex %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_sex, mapping = aes(x = wave, y = pct, color = sex))

p1 <- p0 + geom_line(aes(group = sex), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Sex",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#3+ year country timeseries
trust_by_sex <- no_na_sex %>% group_by(wave, country, sex, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_sex <- trust_by_sex %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_sex, mapping = aes(x = wave, y = pct, color = sex))

p1 <- p0 + geom_line(aes(group = sex), linewidth = 2)  + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Sex",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
trust_by_sex <- no_na_sex %>% group_by(wave, country, sex, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_sex <- trust_by_sex %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_by_sex, mapping = aes(x = wave, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2") + facet_grid(country~sex)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Can you trust others?", 
          title = "Sex and Trust Over Time", subtitle = "data from WVS") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

#trust and immigrant----
#overall timeseries
no_na_trust <- wvs_afr_other %>% filter(trust != "N/A")

no_na_immigrant <- no_na_trust %>% filter(immigrant != "N/A") 

no_na_immigrant <- no_na_immigrant %>% filter(country %in% has_immigrants | 
                                                country %in% no_immigrants)

trust_by_immigrant <- no_na_immigrant %>% group_by(wave, immigrant, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_immigrant <- trust_by_immigrant %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_immigrant, mapping = aes(x = wave, y = pct, color = immigrant))

p1 <- p0 + geom_line(aes(group = immigrant), linewidth = 2)  + ylim(10,20)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Immigration Status",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#3+ year country timeseries
no_na_immigrant <- no_na_immigrant %>% filter(country %in% has_immigrants |
                                                country %in% no_immigrants)


trust_by_immigrant <- no_na_immigrant %>% group_by(wave, country, immigrant, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_immigrant <- trust_by_immigrant %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years) %>% 
  filter(country != "south africa")

p0 <- ggplot(data = trust_by_immigrant, mapping = aes(x = wave, y = pct, color = immigrant))

p1 <- p0 + geom_line(aes(group = immigrant), linewidth = 2)  + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Immigration Status",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
trust_by_immigrant <- no_na_immigrant %>% group_by(wave, country, immigrant, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_immigrant <- trust_by_immigrant %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_by_immigrant, mapping = aes(x = wave, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2") + facet_grid(country~immigrant)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Can you trust others?", 
          title = "Immigration Status and Trust Over Time", subtitle = "data from WVS") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

#trust and religion----
#overall timeseries
no_na_trust <- wvs_afr_other %>% filter(trust != "N/A")

no_na_relig<- no_na_trust %>% filter(relig_denom != "N/A") 

trust_by_relig <- no_na_relig %>% group_by(wave, relig_denom, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_relig <- trust_by_relig %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_relig, mapping = aes(x = wave, y = pct, color = relig_denom))

p1 <- p0 + geom_line(aes(group = relig_denom), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Religion",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#3+ year country timeseries
trust_by_relig <- no_na_relig %>% group_by(wave, country, relig_denom, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_relig <- trust_by_relig %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_relig, mapping = aes(x = wave, y = pct, color = relig_denom))

p1 <- p0 + geom_line(aes(group = relig_denom), linewidth = 2)  + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Religion",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
trust_by_relig <- no_na_relig %>% group_by(wave, country, relig_denom, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_relig <- trust_by_relig %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_by_relig, mapping = aes(x = relig_denom, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2") + facet_grid(country~wave)

p1 + labs(x = "", y = "Percent", fill = "Can you trust others?", 
          title = "Religion and Trust across Survey Waves", subtitle = "data from WVS") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

#trust and attend----
#overall timeseries
no_na_attend <- no_na_attend %>% filter(attend != "N/A") 

trust_by_attend <- no_na_attend %>% group_by(wave, attend, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_attend <- trust_by_attend %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_attend, mapping = aes(x = wave, y = pct, color = attend))

p1 <- p0 + geom_line(aes(group = attend), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Religious Service Attendance",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#3+ year country timeseries
trust_by_attend <- no_na_attend %>% group_by(year, country, attend, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_attend <- trust_by_attend %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_attend, mapping = aes(x = year, y = pct, color = attend))

p1 <- p0 + geom_line(aes(group = attend), linewidth = 2)  + facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent", color = "Religious Service Attendance",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
trust_by_attend <- no_na_attend %>% group_by(wave, country, attend, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_attend <- trust_by_attend %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_by_attend, mapping = aes(x = wave, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2") + facet_grid(country~attend)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Can you trust others?", 
          title = "Religious Service Attendance and Trust Over Time", 
          subtitle = "data from WVS") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))


#trust and income----
#overall timeseries
no_na_income <- no_na_trust %>% filter(income != "N/A") 

no_na_income <- no_na_income %>% mutate(income = fct_relevel(income, "low", "medium", 
                                                             "high"))

trust_by_income <- no_na_income %>% group_by(wave, income, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_income <- trust_by_income %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_income, mapping = aes(x = wave, y = pct, color = income))

p1 <- p0 + geom_line(aes(group = income), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Income",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#3+ year country timeseries
trust_by_income <- no_na_income %>% group_by(wave, country, income, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_income <- trust_by_income %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_income, mapping = aes(x = wave, y = pct, color = income))

p1 <- p0 + geom_line(aes(group = income), linewidth = 2)  + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Income",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
trust_by_income <- no_na_income %>% group_by(wave, country, income, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_income <- trust_by_income %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_by_income, mapping = aes(x = wave, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2") + facet_grid(country~income)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Can you trust others?", 
          title = "Income and Trust Over Time", 
          subtitle = "data from WVS") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))


#trust and employ----
#overall timeseries
no_na_employ <- no_na_employ %>% filter(employ != "N/A") 

trust_by_employ <- no_na_employ %>% group_by(wave, employ, trust) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_employ <- trust_by_employ %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_employ, mapping = aes(x = wave, y = pct, color = employ))

p1 <- p0 + geom_line(aes(group = employ), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Employment Status",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#3+ year country timeseries
trust_by_employ <- no_na_employ %>% group_by(wave, country, employ, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_employ <- trust_by_employ %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_employ, mapping = aes(x = wave, y = pct, color = employ))

p1 <- p0 + geom_line(aes(group = employ), linewidth = 2)  + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Employment Status",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
trust_by_employ <- no_na_employ %>% group_by(wave, country, employ, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_employ <- trust_by_employ %>% filter(country %in% two_years) %>%
  filter(employ != "other")

p0 <- ggplot(data = trust_by_employ, mapping = aes(x = wave, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2") + facet_grid(employ~country)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Can you trust others?", 
          title = "Employment Status and Trust Over Time", 
          subtitle = "data from WVS") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))


#trust and political----
#overall timeseries
no_na_political <- no_na_political %>% filter(political != "N/A") 

trust_by_political <- no_na_political %>% group_by(wave, political, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_political <- trust_by_political %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_political, mapping = aes(x = wave, y = pct, color = political))

p1 <- p0 + geom_line(aes(group = political), linewidth = 2)  + 
  scale_color_brewer(palette = "RdBu", direction = -1)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Political Identification",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#3+ year country timeseries
trust_by_political <- no_na_political %>% group_by(wave, country, political, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_political <- trust_by_political %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_political, mapping = aes(x = wave, y = pct, color = political))

p1 <- p0 + geom_line(aes(group = political), linewidth = 2)  + facet_wrap(~country) +
  scale_color_brewer(palette = "RdBu", direction = -1)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Political Identification",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
trust_by_political <- no_na_political %>% group_by(wave, country, political, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_political <- trust_by_political %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_by_political, mapping = aes(x = wave, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2") + facet_grid(country~political) 

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Can you trust others?", 
          title = "Political Identification and Trust Over Time", 
          subtitle = "data from WVS") + 
  theme(legend.position = "top")

#trust and group pol----
#overall timeseries
no_na_group_pol<- no_na_group_pol %>% filter(group_pol != "N/A") 

trust_by_group_pol <- no_na_group_pol %>% group_by(wave, group_pol, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_group_pol <- trust_by_group_pol %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_group_pol, mapping = aes(x = wave, y = pct, color = group_pol))

p1 <- p0 + geom_line(aes(group = group_pol), linewidth = 2)  + 
  scale_color_brewer(palette = "RdBu", direction = -1)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Political Identification",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#3+ year country timeseries
trust_by_group_pol <- no_na_group_pol %>% group_by(wave, country, group_pol, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_group_pol <- trust_by_group_pol %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_group_pol, mapping = aes(x = wave, y = pct, color = group_pol))

p1 <- p0 + geom_line(aes(group = group_pol), linewidth = 2)  + facet_wrap(~country) +
  scale_color_brewer(palette = "RdBu", direction = -1)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Political Identification",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
trust_by_group_pol <- no_na_group_pol %>% group_by(wave, country,group_pol, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_group_pol <- trust_by_group_pol %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_by_group_pol, mapping = aes(x = wave, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2") + facet_grid(country~group_pol) 

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Can you trust others?", 
          title = "Political Identification and Trust Over Time", 
          subtitle = "data from WVS") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))


#trust and marital----
#overall timeseries
no_na_martial <- no_na_marital %>% filter(marital != "N/A") 

trust_by_marital <- no_na_marital %>% group_by(wave, marital, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_marital <- trust_by_marital %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_by_marital, mapping = aes(x = wave, y = pct, color = marital))

p1 <- p0 + geom_line(aes(group = marital), linewidth = 2)  

p1 + labs(x = "Survey Wave", y = "Percent", color = "Marital Status",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#3+ year country timeseries
trust_by_marital <- no_na_marital %>% group_by(wave, country, marital, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_marital <- trust_by_marital %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_by_marital, mapping = aes(x = wave, y = pct, color = marital))

p1 <- p0 + geom_line(aes(group = marital), linewidth = 2)  + facet_wrap(~country) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Marital Status",
          title = "Percent of People who say they can Trust over Time",
          subtitle = "African Continent, 1982-2022", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
trust_by_marital <- no_na_marital %>% group_by(wave, country, marital, trust) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

trust_by_marital <- trust_by_marital %>% filter(country %in% two_years)

p0 <- ggplot(data = trust_by_marital, mapping = aes(x = wave, y = pct, fill = trust))

p1 <- p0 + geom_col(position = "dodge2") + facet_grid(country~marital) 

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Can you trust others?", 
          title = "Marital Status and Trust Over Time", 
          subtitle = "data from WVS") + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))

