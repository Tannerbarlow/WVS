#data download and set_up----

wvs_relig <- wvs_full %>% select(COUNTRY_ALPHA, S017, S020, A165, F025, F028, A006)

wvs_relig <- wvs_relig %>% filter(COUNTRY_ALPHA %in% afr_countries)

#removing N/As
wvs_relig <- wvs_relig %>% naniar::replace_with_na_all(condition = ~.x %in% c(-5, -4, -2, -1))
#renaming variables
wvs_relig <- wvs_relig %>% mutate(country = case_when(COUNTRY_ALPHA == "BFA" ~ 'burkina faso',
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

wvs_relig <- wvs_relig %>% mutate(weight = case_when(S017 == "0" ~ 'undefined',
                                                             S017 == "1" ~ 'no weighting',
                                                             TRUE ~ 'other')) %>%
  select(-S017)

wvs_relig <- wvs_relig %>% rename(year = S020)

wvs_relig <- wvs_relig %>% mutate(trust = case_when(A165 == "1" ~ 'can trust',
                                                            A165 == "2" ~ 'need to be very careful',
                                                            TRUE ~ 'N/A')) %>%
  select(-A165)
wvs_relig <- wvs_relig %>% mutate(relig_denom = case_when(F025 == "0" ~ 'none',
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

wvs_relig <- wvs_relig %>% mutate(attend = case_when(F028 == "1" ~ 'more than once a week',
                                                             F028 == "2" ~ 'once a week',
                                                             F028 == "3" ~ 'once a month',
                                                             F028 == "4" ~ 'only on christmas/easter',
                                                             F028 == "5" ~ 'only on other holy days',
                                                             F028 == "6" ~ 'once a year', 
                                                             F028 == "7" ~ 'less than once a year',
                                                             F028 == "8" ~ 'never/practically never',
                                                             TRUE ~ 'N/A')) %>%
  select(-F028)

wvs_relig <- wvs_relig %>% mutate(relig_imp = case_when(A006 == "1" ~ 'very important',
                                                        A006 == "2" ~ 'rather important',
                                                        A006 == "3" ~ 'not very important',
                                                        A006 == "4" ~ 'not at all important',
                                                        TRUE ~ 'N/A')) %>%
  select(-A006)


wvs_relig <- wvs_relig %>% mutate(wave = case_when(year == "1981" | year == "1982" | year == "1983" |
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


no_na_trust <- wvs_relig %>% filter(trust != "N/A")

#just relig_imp years----
#years
#overall timeseries
no_na_relig_imp <- no_na_trust %>% filter(relig_imp != "N/A")

relig_imp_over_time <- no_na_relig_imp %>% group_by(year, relig_imp) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

p0 <- ggplot(data = relig_imp_over_time, mapping = aes(x = year, y = pct, color = relig_imp))

p1 <- p0 + geom_line(aes(group = relig_imp), linewidth = 2)

p1 + labs(x = "Year", y = "Percent", color = "Importance of Religion", 
          title = "Importance of Religion to Respondents over Time", 
          subtitle = "African Continent, 1990-2022", caption = "data from WVS") + 
  theme(legend.position = "top")

#3+ year timeseries
relig_imp_over_time <- no_na_relig_imp %>% group_by(year, country, relig_imp) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

relig_imp_over_time <- relig_imp_over_time %>% filter(country %in% many_years |
                                                        country %in% few_years)

p0 <- ggplot(data = relig_imp_over_time, mapping = aes(x = year, y = pct, color = relig_imp))

p1 <- p0 + geom_line(aes(group = relig_imp), linewidth = 2) + facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent", color = "Importance of Religion", 
          title = "Importance of Religion to Respondents over Time", 
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
relig_imp_over_time <- no_na_relig_imp %>% group_by(year, country, relig_imp) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

relig_imp_over_time <- relig_imp_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = relig_imp_over_time, mapping = aes(x = year, y = pct, fill = relig_imp))

p1 <- p0 + geom_col(position = "dodge2", width = 3) + facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent", fill = "Importance of Religion", 
          title = "Importance of Religion to Respondents over Time", 
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#one year countries bar plot
relig_imp_over_time <- no_na_relig_imp %>% group_by (country, relig_imp) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

relig_imp_over_time <- relig_imp_over_time %>% filter(country %in% one_year)

p0 <- ggplot(data = relig_imp_over_time, mapping = aes(x = country, y = pct, fill = relig_imp))

p1 <- p0 + geom_col(position = "dodge2") 

p1 + labs(x = "", y = "Percent", fill = "Importance of Religion", 
          title = "Importance of Religion to Respondents", subtitle = "African Continent", 
          caption = "data from WVS")

#just relig_imp waves----
#waves
#continental timeseries
no_na_relig_imp <- no_na_trust %>% filter(relig_imp != "N/A")

relig_imp_over_time <- no_na_relig_imp %>% group_by(wave, relig_imp) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0)) 

p0 <- ggplot(data = relig_imp_over_time, mapping = aes(x = wave, y = pct, color = relig_imp))

p1 <- p0 + geom_line(aes(group = relig_imp), linewidth = 2)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Importance of Religion", 
          title = "Importance of Religion to Respondents over Time", 
          subtitle = "African Continent, 1990-2022", caption = "data from WVS") + 
  theme(legend.position = "top")

#3+ year timeseries
relig_imp_over_time <- no_na_relig_imp %>% group_by(wave, country, relig_imp) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

relig_imp_over_time <- relig_imp_over_time %>% filter(country %in% many_years |
                                                        country %in% few_years)

p0 <- ggplot(data = relig_imp_over_time, mapping = aes(x = wave, y = pct, color = relig_imp))

p1 <- p0 + geom_line(aes(group = relig_imp), linewidth = 2) + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Importance of Religion", 
          title = "Importance of Religion to Respondents over Time", 
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#2 year bar plot
relig_imp_over_time <- no_na_relig_imp %>% group_by(wave, country, relig_imp) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

relig_imp_over_time <- relig_imp_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = relig_imp_over_time, mapping = aes(x = wave, y = pct, fill = relig_imp))

p1 <- p0 + geom_col(position = "dodge2", width = 3) + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Importance of Religion", 
          title = "Importance of Religion to Respondents over Time", 
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "top")

#one year countries bar plot
relig_imp_over_time <- no_na_relig_imp %>% group_by(country, relig_imp) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

relig_imp_over_time <- relig_imp_over_time %>% filter(country %in% one_year)

p0 <- ggplot(data = relig_imp_over_time, mapping = aes(x = country, y = pct, fill = relig_imp))

p1 <- p0 + geom_col(position = "dodge2") 

p1 + labs(x = "", y = "Percent", fill = "Importance of Religion", 
          title = "Importance of Religion to Respondents", subtitle = "African Continent", 
          caption = "data from WVS")

#relig_imp and trust years----
#overall timeseries
trust_and_relig_imp <- no_na_relig_imp %>% group_by(year, country, relig_imp, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

trust_and_relig_imp <- trust_and_relig_imp %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_and_relig_imp, mapping = aes(x = year, y = pct, color = relig_imp))

p1 <- p0 + geom_line(aes(group = relig_imp), linewidth = 2) 

p1 + labs(x = "Year", y = "Percent", color = "Importance of Religion", 
          title = "People who Believe they can Trust Others", subtitle = "African Continent", 
          daption = "data from WVS") + theme(legend.position = "top")

#3+ year country timeseries
trust_and_relig_imp <- no_na_relig_imp %>% group_by(year, country, relig_imp, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

trust_and_relig_imp <- trust_and_relig_imp %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_and_relig_imp, mapping = aes(x = year, y = pct, color = relig_imp))

p1 <- p0 + geom_line(aes(group = relig_imp), linewidth = 2)  + facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent", color = "Importance of Religion", 
          title = "People who Believe they can Trust Others", subtitle = "African Continent", 
          daption = "data from WVS") + theme(legend.position = "top")

#2 year barplot
trust_and_relig_imp <- no_na_relig_imp %>% group_by(year, country, relig_imp, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

trust_and_relig_imp <- trust_and_relig_imp %>% filter(trust != "need to be very careful") %>%
  filter(country %in% two_years)

p0 <- ggplot(data = trust_and_relig_imp, mapping = aes(x = year, y = pct, fill = relig_imp))

p1 <- p0 + geom_col(position = "dodge2", width = 3)  + facet_wrap(~country)

p1 + labs(x = "Year", y = "Percent", fill = "Importance of Religion", 
          title = "People who Believe they can Trust Others", subtitle = "African Continent", 
          daption = "data from WVS") + theme(legend.position = "top")

#one year barplot
trust_and_relig_imp <- no_na_relig_imp %>% group_by(country, relig_imp, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

trust_and_relig_imp <- trust_and_relig_imp %>% filter(trust != "need to be very careful") %>%
  filter(country %in% one_year)

p0 <- ggplot(data = trust_and_relig_imp, mapping = aes(x = country, y = pct, fill = relig_imp))

p1 <- p0 + geom_col(position = "dodge2")

p1 + labs(x = "", y = "Percent", fill = "Importance of Religion", 
          title = "People who Believe they can Trust Others", subtitle = "African Continent", 
          daption = "data from WVS") + theme(legend.position = "top")

#relig_imp and trust waves----
#waves
#overall timeseries
trust_and_relig_imp <- no_na_relig_imp %>% group_by(wave, relig_imp, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

trust_and_relig_imp <- trust_and_relig_imp %>% filter(trust != "need to be very careful")

p0 <- ggplot(data = trust_and_relig_imp, mapping = aes(x = wave, y = pct, color = relig_imp))

p1 <- p0 + geom_line(aes(group = relig_imp), linewidth = 2)  + ylim(0, 30)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Importance of Religion", 
          title = "People who Believe they can Trust Others", 
          subtitle = "African Continent", caption = "data from WVS") + 
  theme(legend.position = "top")

#3+ year timeseries
trust_and_relig_imp <- no_na_relig_imp %>% group_by(wave, country, relig_imp, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

trust_and_relig_imp <- trust_and_relig_imp %>% filter(trust != "need to be very careful") %>%
  filter(country %in% many_years | country %in% few_years)

p0 <- ggplot(data = trust_and_relig_imp, mapping = aes(x = wave, y = pct, color = relig_imp))

p1 <- p0 + geom_line(aes(group = relig_imp), linewidth = 2)  + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Importance of Religion", 
          title = "People who Believe they can Trust Others", 
          subtitle = "African Continent", caption = "data from WVS") + 
  theme(legend.position = "top")

#2 year bar plot
trust_and_relig_imp <- no_na_relig_imp %>% group_by(wave, country, relig_imp, trust) %>%
  summarize(N = n()) %>% mutate(freq = N/sum(N), pct = round((freq*100),0))

trust_and_relig_imp <- trust_and_relig_imp %>% filter(trust != "need to be very careful") %>%
  filter(country %in% two_years)

p0 <- ggplot(data = trust_and_relig_imp, mapping = aes(x = wave, y = pct, fill = relig_imp))

p1 <- p0 + geom_col(position = "dodge2")  + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", fill = "Importance of Religion", 
          title = "People who Believe they can Trust Others", 
          subtitle = "African Continent", caption = "data from WVS") + 
  theme(legend.position = "top")



#relig_denom filtered----
common_religs <- c("muslim", "catholic", "protestant", "orthodox", "other christian")

dom_relig <- no_na_trust %>% filter(relig_denom %in% common_religs)

#overall bar plot
dom_relig_by_country <- dom_relig %>% group_by(country, relig_denom) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

p0 <- ggplot(data = dom_relig_by_country, mapping = aes(x = country, y = pct, fill = relig_denom))

p1 <- p0 + geom_col()

p1 + labs(x = "", y = "Percent", fill = "Religion", title = "Religion of Respondents by Country", 
          subtitle = "African Continent", caption = "data from WVS") +
  theme(legend.position = "right", axis.text.x = element_text(angle = 30, hjust = 1))

#overall time series
dom_relig_over_time <- dom_relig %>% group_by(wave, relig_denom) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

p0 <- ggplot(data = dom_relig_over_time, mapping = aes(x = wave, y = pct, color = relig_denom))
             
p1 <- p0 + geom_line(aes(group = relig_denom), linewidth = 2) 

p1 + labs(x = "Survey Wave", y = "Percent", color = "Religion", title = "Religion of Respondents over Time", 
          subtitle = "African Continent", caption = "data from WVS") + 
  theme(legend.position = "top")

#3+ year country time series
dom_relig_over_time <- dom_relig %>% group_by(wave, country, relig_denom) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

dom_relig_over_time <- dom_relig_over_time %>% filter(country %in% many_years | 
                                                        country %in% few_years)
p0 <- ggplot(data = dom_relig_over_time, mapping = aes(x = wave, y = pct, color = relig_denom))

p1 <- p0 + geom_line(aes(group = relig_denom), linewidth = 2)  + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Religion", title = "Religion of Respondents over Time", 
          subtitle = "African Continent", caption = "data from WVS") + 
  theme(legend.position = "top")

#2 year country bar plot
dom_relig_over_time <- dom_relig %>% group_by(wave, country, relig_denom) %>% summarize(N = n()) %>%
  mutate(freq = N/sum(N), pct = round((freq*100),0))

dom_relig_over_time <- dom_relig_over_time %>% filter(country %in% two_years)

p0 <- ggplot(data = dom_relig_over_time, mapping = aes(x = wave, y = pct, fill = relig_denom))

p1 <- p0 + geom_col(position = "dodge2") + facet_wrap(~country)

p1 + labs(x = "Survey Wave", y = "Percent", color = "Religion", title = "Religion of Respondents over Time", 
          subtitle = "African Continent", caption = "data from WVS") + 
  theme(legend.position = "top")

#dom_relig and trust----
#overall timeseries
#3+ year time series
#2 barplot
#one year bar plot