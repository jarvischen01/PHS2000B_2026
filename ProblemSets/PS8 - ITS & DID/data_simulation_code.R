##install.packages("sigmoid")
library(sigmoid)
library(ggplot2)
library(dplyr)
library(fixest)
rm(list = ls())


#########################
### RDD/IV simulation ###
#########################

age <- seq(from=0.5, to=12, by=0.05)
age <- rep(age, 1000)
RDD_IV_sim <- as.data.frame(age)
RDD_IV_sim$under5 <- ifelse(RDD_IV_sim < 5, 1, 0) 
RDD_IV_sim$flu_vacc_prob <- sigmoid(((RDD_IV_sim$age/-10))+ 0.5 + 0.3*RDD_IV_sim$under5)
set.seed(0606)
RDD_IV_sim$flu_vacc <- rbinom(nrow(RDD_IV_sim), 1, RDD_IV_sim$flu_vacc_prob)
table(RDD_IV_sim$flu_vacc) 

#RDD_IV_sim$ED_visit_prob <- sigmoid(((RDD_IV_sim$age/-5)) - 10*RDD_IV_sim$flu_vacc)
RDD_IV_sim$ED_visit_prob <- ((RDD_IV_sim$age)*(-0.005)) + 0.25 - (0.15*RDD_IV_sim$flu_vacc)
RDD_IV_sim$ED_visit <- rbinom(nrow(RDD_IV_sim), 1, RDD_IV_sim$ED_visit_prob)
RDD_IV_sim$age_centered<- RDD_IV_sim$age - 5


write.csv(RDD_IV_sim, "C:/Users/nerah/Documents/PHS2000_24_25/PHS2000B/Exam/Exam 2/rdd_iv_flu_data.csv")

png("rdd_iv_q2.png")
ggplot(RDD_IV_sim, aes(x=age, y=flu_vacc, group=under5)) +
  stat_summary_bin(fun.y='mean', bins=30, size=2, geom='point') +
  geom_vline(xintercept = 5, color = "red") +
  geom_smooth(method='lm', formula=y~x) +
  labs(x = "Age", y = "Pr of Flu Vaccine") +
  theme_bw()
dev.off()

RDD_IV_sim_sub <- subset(RDD_IV_sim, age_centered > -1 & age_centered <  1)
ggplot(RDD_IV_sim, aes(x=age, y=ED_visit, group=under5)) +
  stat_summary_bin(fun.y='mean', bins=30, size=2, geom='point') +
  geom_vline(xintercept = 5, color = "red") +
  geom_smooth(method='lm', formula=y~x) +
  labs(x = "Age", y = "Pr of ED visit") +
  theme_bw()

ggplot(RDD_IV_sim_sub, aes(x=age, y=ED_visit, group=under5)) +
  stat_summary_bin(fun.y='mean', bins=20, size=2, geom='point') +
  geom_vline(xintercept = 5, color = "red") +
  geom_smooth(method='lm', formula=y~x) +
  labs(x = "Age", y = "Pr of ED visit") +
  theme_bw()


rdd_model1 <- feols(flu_vacc ~ age_centered + under5,
                 data = subset(RDD_IV_sim, age_centered > -1 & age_centered <  1),
                 se = "hetero") 

summary(rdd_model1)


rdd_model2 <- feols(ED_visit ~ age_centered | flu_vacc ~ under5,
                    data = subset(RDD_IV_sim, age_centered > -1 & age_centered <  1),
                    se = "hetero") 

summary(rdd_model2)


rdd_model3 <- feols(ED_visit ~ age_centered + under5,
                    data = subset(RDD_IV_sim, age_centered > -1 & age_centered <  1),
                    se = "hetero") 

summary(rdd_model3)

######################
### DiD simulation ###
######################

h <- function(x) {
  peaks <- c(6, 18, 30)
  max_height <- 0
  for (p in peaks) {
    max_height <- max_height + exp(-0.05 * (x - p)^2)
  }
  return(max_height / 20)  
}

x <- seq(0, 36, by=0.1)
y <- h(x)+0.003*x
plot(x, y, type="l", ylim=c(0,0.3), 
     main="Function with Local Maxima at x=6,18,30",
     xlab="x", ylab="f(x)")
abline(v=c(6,18,30), lty=2, col="red")


set.seed(0526)

month<- seq(from=1, to=36, by=1)
month<- rep(month, 10000)
country_A <- as.data.frame(month)
country_A$country <- "Jarvistan"
country_A$post <- ifelse(country_A$month>=16,1,0)
country_A$effective<-ifelse(country_A$month>=16 & country_A$month<=21,1,0)
country_A$ED_visit_prob <- h(country_A$month)+0.05+(0.003*country_A$month)-(0.03*country_A$effective)
country_A$ED_visit <- rbinom(nrow(country_A), 1, country_A$ED_visit_prob)
country_A$treat<- 1


country_B <- as.data.frame(month)
country_B$country <- "McConnelland"
country_B$post <- ifelse(country_B$month>=16,1,0)
country_B$ED_visit_prob <- h(country_B$month)+(0.003*country_A$month)
country_B$ED_visit <- rbinom(nrow(country_B), 1, country_B$ED_visit_prob)
country_B$treat<- 0
country_B$effective<-0

did_data <- rbind(country_A, country_B)
write.csv(did_data, "C:/Users/nerah/Documents/PHS2000_24_25/PHS2000B/Exam/Exam 2/did_data.csv")


its_data <- country_A %>% 
  group_by(month) %>%
  summarize(monthly_ED_visits = sum(ED_visit))
its_data$month_centered <- its_data$month - 16
its_data$inter_first_6mo <- ifelse(its_data$month>=16 & its_data$month<=21,1,0)
its_data$inter_after_6mo <- ifelse(its_data$month>21,1,0)

write.csv(its_data, "C:/Users/nerah/Documents/PHS2000_24_25/PHS2000B/Exam/Exam 2/its_data.csv")

its_model <- feols(monthly_ED_visits ~ month_centered + inter_first_6mo, data=its_data, se = "hetero")
summary(its_model)

did_model <- feols(ED_visit ~ treat*post, data=did_data, se = "hetero")
summary(did_model)





plot1 <- ggplot() + geom_point(data = country_A, aes(x=month, y=ED_visit_prob)) + 
  geom_point(data = country_B, aes(x=month, y=ED_visit_prob)) +
  geom_vline(xintercept = 16, colour="grey", linetype = 2) +
  labs(x = "Time", y = "Pr ED visit")
plot1



its








