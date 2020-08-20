#import tidyverse library, which includes dplyr and ggplot2
library(tidyverse)
library(reshape2)

#import the participant data and experimental data csvs into dataframes
participant_data <- read_csv("participant-info.csv")
experiment_data <- read_csv("ahi-cesd.csv")

#remove the data for individual questions from the two surveys, leaving only the total scores
experiment_data_lite <- subset(experiment_data, select = c(id, occasion, elapsed.days, intervention, ahiTotal, cesdTotal))
head(experiment_data_lite)

#group the data by id, and filter out all participants that only took the baseline survey.
exp_data_multionly <- experiment_data_lite %>% 
    group_by(id)%>%
    filter((max(row_number()) > 1))%>%
    ungroup()

#create a column that contains rounded day values for elapsed time and remove the old col
exp_data_multionly <- exp_data_multionly%>%
    mutate(days_since_start = round(elapsed.days, 0))

exp_data_multionly <- subset(exp_data_multionly, select = -c(elapsed.days))

#convert intervention and occasion to factors
exp_data_multionly$intervention <- as.factor(exp_data_multionly$intervention)
exp_data_multionly$occasion <- as.factor(exp_data_multionly$occasion)

study_summary <- exp_data_multionly %>%
    group_by(intervention, occasion)%>%
    summarise(mean_ahi = mean(ahiTotal), 
              med_ahi = median(ahiTotal),
              ahi_sd = sd(ahiTotal),
              mean_cesd = mean(cesdTotal),
              med_cesd = median(cesdTotal),
              cesd_sd = sd(cesdTotal),
              occasion_count = n())

study_summary

study_summary %>%
    ggplot(., aes(x = occasion, y = med_ahi, color = intervention, group = intervention))+
    geom_point(size = 3)+geom_line()

study_summary %>%
    ggplot(., aes(x = occasion, y = med_cesd, color = intervention, group = intervention))+
    geom_point(size = 3)+geom_line()

ggplot(exp_data_multionly, aes(x = occasion, y = ahiTotal))+
    geom_boxplot()+
    facet_grid(. ~ intervention)

ggplot(exp_data_multionly, aes(x = occasion, y = cesdTotal))+
    geom_boxplot()+
    facet_grid(. ~ intervention)

#usntack the tall dataframe into a wide dataframe
#create columns for difference calculations
exp_data_ahi <- subset(exp_data_multionly, select = c(id, occasion, intervention, ahiTotal))
cols = c('id', 'ob0', 'ob1', 'ob2', 'ob3', 'ob4', 'ob5')

exp_data_ahi_1 <- exp_data_ahi %>% filter(intervention == 1)
exp_data_ahi_1$intervention <- NULL
exp_data_ahi_1 <- dcast(exp_data_ahi_1, id~occasion, fill = NULL, fun.aggregate = mean, , drop = FALSE)
colnames(exp_data_ahi_1) = cols
exp_data_ahi_1 <- exp_data_ahi_1 %>% mutate(d1wk = ob1-ob0, 
                                            d2wk = ob2-ob0, 
                                            d1mo = ob3-ob0,
                                            d3mo = ob4-ob0,
                                            d6mo = ob5-ob0,
                                            intervention = 1)

exp_data_ahi_2 <- exp_data_ahi %>% filter(intervention == 2)
exp_data_ahi_2$intervention <- NULL
exp_data_ahi_2 <- dcast(exp_data_ahi_2, id~occasion, fill = NULL, fun.aggregate = mean, , drop = FALSE)
colnames(exp_data_ahi_2) = cols
exp_data_ahi_2 <- exp_data_ahi_2 %>% mutate(d1wk = ob1-ob0, 
                                            d2wk = ob2-ob0, 
                                            d1mo = ob3-ob0,
                                            d3mo = ob4-ob0,
                                            d6mo = ob5-ob0,
                                            intervention = 2)

exp_data_ahi_3 <- exp_data_ahi %>% filter(intervention == 3)
exp_data_ahi_3$intervention <- NULL
exp_data_ahi_3 <- dcast(exp_data_ahi_3, id~occasion, fill = NULL, fun.aggregate = mean, , drop = FALSE)
colnames(exp_data_ahi_3) = cols
exp_data_ahi_3 <- exp_data_ahi_3 %>% mutate(d1wk = ob1-ob0, 
                                            d2wk = ob2-ob0, 
                                            d1mo = ob3-ob0,
                                            d3mo = ob4-ob0,
                                            d6mo = ob5-ob0,
                                            intervention = 3)

exp_data_ahi_4 <- exp_data_ahi %>% filter(intervention == 4)
exp_data_ahi_4$intervention <- NULL
exp_data_ahi_4 <- dcast(exp_data_ahi_4, id~occasion, fill = NULL, fun.aggregate = mean, , drop = FALSE)
colnames(exp_data_ahi_4) = cols
exp_data_ahi_4 <- exp_data_ahi_4 %>% mutate(d1wk = ob1-ob0, 
                                            d2wk = ob2-ob0, 
                                            d1mo = ob3-ob0,
                                            d3mo = ob4-ob0,
                                            d6mo = ob5-ob0,
                                            intervention = 4)

#row-bind the four dfs into a single df
exp_data_ahi_diffs <- rbind(exp_data_ahi_1, exp_data_ahi_2, exp_data_ahi_3, exp_data_ahi_4)
exp_data_ahi_diffs

#perform anova for each differnce calculation and store the results
ahi_1wk_anova <- aov(d1wk ~ intervention, data = exp_data_ahi_diffs)
ahi_2wk_anova <- aov(d2wk ~ intervention, data = exp_data_ahi_diffs)
ahi_1mo_anova <- aov(d1mo ~ intervention, data = exp_data_ahi_diffs)
ahi_3mo_anova <- aov(d3mo ~ intervention, data = exp_data_ahi_diffs)
ahi_6mo_anova <- aov(d6mo ~ intervention, data = exp_data_ahi_diffs)

summary(ahi_1wk_anova)

summary(ahi_2wk_anova)

summary(ahi_1mo_anova)

summary(ahi_3mo_anova)

summary(ahi_6mo_anova)

#usntack the tall dataframe into a wide dataframe
#create columns for difference calculations
exp_data_cesd <- subset(exp_data_multionly, select = c(id, occasion, intervention, cesdTotal))
cols = c('id', 'ob0', 'ob1', 'ob2', 'ob3', 'ob4', 'ob5')

exp_data_cesd_1 <- exp_data_cesd %>% filter(intervention == 1)
exp_data_cesd_1$intervention <- NULL
exp_data_cesd_1 <- dcast(exp_data_cesd_1, id~occasion, fill = NULL, fun.aggregate = mean, , drop = FALSE)
colnames(exp_data_cesd_1) = cols
exp_data_cesd_1 <- exp_data_cesd_1 %>% mutate(d1wk = ob1-ob0, 
                                            d2wk = ob2-ob0, 
                                            d1mo = ob3-ob0,
                                            d3mo = ob4-ob0,
                                            d6mo = ob5-ob0,
                                            intervention = 1)

exp_data_cesd_2 <- exp_data_cesd %>% filter(intervention == 2)
exp_data_cesd_2$intervention <- NULL
exp_data_cesd_2 <- dcast(exp_data_cesd_2, id~occasion, fill = NULL, fun.aggregate = mean, , drop = FALSE)
colnames(exp_data_cesd_2) = cols
exp_data_cesd_2 <- exp_data_cesd_2 %>% mutate(d1wk = ob1-ob0, 
                                            d2wk = ob2-ob0, 
                                            d1mo = ob3-ob0,
                                            d3mo = ob4-ob0,
                                            d6mo = ob5-ob0,
                                            intervention = 2)

exp_data_cesd_3 <- exp_data_cesd %>% filter(intervention == 3)
exp_data_cesd_3$intervention <- NULL
exp_data_cesd_3 <- dcast(exp_data_cesd_3, id~occasion, fill = NULL, fun.aggregate = mean, , drop = FALSE)
colnames(exp_data_cesd_3) = cols
exp_data_cesd_3 <- exp_data_cesd_3 %>% mutate(d1wk = ob1-ob0, 
                                            d2wk = ob2-ob0, 
                                            d1mo = ob3-ob0,
                                            d3mo = ob4-ob0,
                                            d6mo = ob5-ob0,
                                            intervention = 3)

exp_data_cesd_4 <- exp_data_cesd %>% filter(intervention == 4)
exp_data_cesd_4$intervention <- NULL
exp_data_cesd_4 <- dcast(exp_data_cesd_4, id~occasion, fill = NULL, fun.aggregate = mean, , drop = FALSE)
colnames(exp_data_cesd_4) = cols
exp_data_cesd_4 <- exp_data_cesd_4 %>% mutate(d1wk = ob1-ob0, 
                                            d2wk = ob2-ob0, 
                                            d1mo = ob3-ob0,
                                            d3mo = ob4-ob0,
                                            d6mo = ob5-ob0,
                                            intervention = 4)

#row-bind the four dfs into a single df
exp_data_cesd_diffs <- rbind(exp_data_cesd_1, exp_data_cesd_2, exp_data_cesd_3, exp_data_cesd_4)
exp_data_cesd_diffs

#perform anova for each differnce calculation and store the results
cesd_1wk_anova <- aov(d1wk ~ intervention, data = exp_data_cesd_diffs)
cesd_2wk_anova <- aov(d2wk ~ intervention, data = exp_data_cesd_diffs)
cesd_1mo_anova <- aov(d1mo ~ intervention, data = exp_data_cesd_diffs)
cesd_3mo_anova <- aov(d3mo ~ intervention, data = exp_data_cesd_diffs)
cesd_6mo_anova <- aov(d6mo ~ intervention, data = exp_data_cesd_diffs)

summary(cesd_1wk_anova)

summary(cesd_2wk_anova)

summary(cesd_1mo_anova)

summary(cesd_3mo_anova)

summary(cesd_6mo_anova)

#T test for intervention 1
t.test(exp_data_ahi_1$ob0, exp_data_ahi_1$ob1, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 2
t.test(exp_data_ahi_2$ob0, exp_data_ahi_2$ob1, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 3
t.test(exp_data_ahi_3$ob0, exp_data_ahi_3$ob1, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 4
t.test(exp_data_ahi_4$ob0, exp_data_ahi_4$ob1, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)


#T test for intervention 1
t.test(exp_data_ahi_1$ob0, exp_data_ahi_1$ob2, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 2
t.test(exp_data_ahi_2$ob0, exp_data_ahi_2$ob2, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 3
t.test(exp_data_ahi_3$ob0, exp_data_ahi_3$ob2, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 4
t.test(exp_data_ahi_4$ob0, exp_data_ahi_4$ob2, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)


#T test for intervention 1
t.test(exp_data_ahi_1$ob0, exp_data_ahi_1$ob3, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 2
t.test(exp_data_ahi_2$ob0, exp_data_ahi_2$ob3, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 3
t.test(exp_data_ahi_3$ob0, exp_data_ahi_3$ob3, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 4
t.test(exp_data_ahi_4$ob0, exp_data_ahi_4$ob3, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)


#T test for intervention 1
t.test(exp_data_ahi_1$ob0, exp_data_ahi_1$ob4, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 2
t.test(exp_data_ahi_2$ob0, exp_data_ahi_2$ob4, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 3
t.test(exp_data_ahi_3$ob0, exp_data_ahi_3$ob4, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 4
t.test(exp_data_ahi_4$ob0, exp_data_ahi_4$ob4, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)


#T test for intervention 1
t.test(exp_data_ahi_1$ob0, exp_data_ahi_1$ob5, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 2
t.test(exp_data_ahi_2$ob0, exp_data_ahi_2$ob5, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 3
t.test(exp_data_ahi_3$ob0, exp_data_ahi_3$ob5, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 4
t.test(exp_data_ahi_4$ob0, exp_data_ahi_4$ob5, alternative = "less", mu=0, paired = FALSE, conf.level = 0.95)


#T test for intervention 1
t.test(exp_data_cesd_1$ob0, exp_data_cesd_1$ob1, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 2
t.test(exp_data_cesd_2$ob0, exp_data_cesd_2$ob1, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 3
t.test(exp_data_cesd_3$ob0, exp_data_cesd_3$ob1, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 4
t.test(exp_data_cesd_4$ob0, exp_data_cesd_4$ob1, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)


#T test for intervention 1
t.test(exp_data_cesd_1$ob0, exp_data_cesd_1$ob2, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 2
t.test(exp_data_cesd_2$ob0, exp_data_cesd_2$ob2, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 3
t.test(exp_data_cesd_3$ob0, exp_data_cesd_3$ob2, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 4
t.test(exp_data_cesd_4$ob0, exp_data_cesd_4$ob2, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)


#T test for intervention 1
t.test(exp_data_cesd_1$ob0, exp_data_cesd_1$ob3, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 2
t.test(exp_data_cesd_2$ob0, exp_data_cesd_2$ob3, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 3
t.test(exp_data_cesd_3$ob0, exp_data_cesd_3$ob3, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 4
t.test(exp_data_cesd_4$ob0, exp_data_cesd_4$ob3, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)


#T test for intervention 1
t.test(exp_data_cesd_1$ob0, exp_data_cesd_1$ob4, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 2
t.test(exp_data_cesd_2$ob0, exp_data_cesd_2$ob4, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 3
t.test(exp_data_cesd_3$ob0, exp_data_cesd_3$ob4, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 4
t.test(exp_data_cesd_4$ob0, exp_data_cesd_4$ob4, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)


#T test for intervention 1
t.test(exp_data_cesd_1$ob0, exp_data_cesd_1$ob5, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 2
t.test(exp_data_cesd_2$ob0, exp_data_cesd_2$ob5, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 3
t.test(exp_data_cesd_3$ob0, exp_data_cesd_3$ob5, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)

#T test for intervention 4
t.test(exp_data_cesd_4$ob0, exp_data_cesd_4$ob5, alternative = "greater", mu=0, paired = FALSE, conf.level = 0.95)



