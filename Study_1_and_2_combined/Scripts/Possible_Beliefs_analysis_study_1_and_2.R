# MERGE STUDY 1 AN STUDY 2


############################################################################
# PACKAGES
############################################################################
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lme4")
library(readr)
library(dplyr)
library(tidyr)
library(lme4)
options(scipen = 9999)

############################################################################
# DATA
############################################################################
BO.1 <- read.csv("./Study_1/Data/Possible_Beliefs_Study_1_Data.csv")
BO.2 <- read.csv("./Study_2/Data/Possible_Beliefs_Study_2_Data.csv")

# or for results load
# load("./Study_1_and_2_combined/R_Images/study_1_and_2.RData")


BO.2 <- BO.2 %>%
 select(X, ID, Age.3,
        Stimuli,
        Trial,
        Condition,
        Answer,
        Gender)

BO <- rbind(BO.1, BO.2)
View(BO)

############################################################################
# SUBSETTING BELIEF CONDITIONS FOR MAIN ANALYSIS
############################################################################

BO_belief <-
 subset(BO,
         BO$Condition == "No Evidence" |
         BO$Condition == "Strong Evidence" |
         BO$Condition == "Counter Evidence" |
         BO$Condition == "Opinion" |
         BO$Condition == "Moral" |
         BO$Condition == "Immoral")
str(BO_belief)
BO_belief$Condition <-
 droplevels(as.factor(BO_belief$Condition))
str(BO_belief)
BO_belief$Condition <-
 relevel(as.factor(BO_belief$Condition),
         ref = "No Evidence"
 )
View(BO_belief)

############################################################################
# ANALYSIS
############################################################################
source("./Study_2/Functions/diagnostic_fcns.r")

xx.fe.re <-
 fe.re.tab(
  fe.model = "Answer  ~
           Condition*Age.3 + Trial",
  re = "(1|ID)", data = BO_belief
 )
xx.fe.re$summary
t.data <-
  xx.fe.re$data
str(t.data)

# center and z-transform variables that are in the random slopes
t.data$c.Condition.Counter.Evidence <-
 as.numeric(t.data$Condition.Counter.Evidence) -
 mean(as.numeric(t.data$Condition.Counter.Evidence))
t.data$c.Condition.Strong.Evidence <-
 as.numeric(t.data$Condition.Strong.Evidence) -
 mean(as.numeric(t.data$Condition.Strong.Evidence))
t.data$z.Trial <- scale(t.data$Trial)

# center and z-transform variables that are in the random slopes
t.data$c.Condition.Opinion <-
 as.numeric(t.data$Condition.Opinion) -
 mean(as.numeric(t.data$Condition.Opinion))
t.data$c.Condition.Immoral <-
 as.numeric(t.data$Condition.Immoral) -
 mean(as.numeric(t.data$Condition.Immoral))
t.data$c.Condition.Moral <-
 as.numeric(t.data$Condition.Moral) -
 mean(as.numeric(t.data$Condition.Moral))


contr <- glmerControl(
 optimizer = "bobyqa",
 optCtrl = list(maxfun = 10000000)
)

full <-
 glmer(Answer ~
        Condition * Age.3 +
        (1 + (c.Condition.Counter.Evidence +
                c.Condition.Strong.Evidence
              + c.Condition.Opinion +
                c.Condition.Immoral +
               c.Condition.Moral) +
           z.Trial || ID),
       data = t.data,
       family = binomial,
       control = contr
 )

# overdispersion
overdisp.test(full)
summary(full)$varcor
ranef.diagn.plot(full)
# colliniarity
xx <- lm(Answer ~ (Condition + Age.3),
         data = t.data
)
library(car)
vif(xx)

# model stability

summary(full)$coefficients
round(summary(full)$coefficients, 3)

# reduced model
source("./drop1_para.r")
test.1 <- drop1p(
 model.res = full,
 contr = contr
)
test.1$drop1.res

full.2 <- glmer(Answer ~
                 Condition + Age.3 +
                 (1 + (c.Condition.Counter.Evidence +
                        c.Condition.Strong.Evidence +
                         c.Condition.Opinion +
                        c.Condition.Immoral +
                         c.Condition.Moral) +
                    z.Trial || ID),
                data = t.data,
                family = binomial,
                control = contr
)
anova(full, full.2, test = "Chisq")
test.2 <- drop1p(
 model.res = full.2,
 contr = contr
)
round(test.2$drop1.res, 3)

summary(full.2)$coefficients
round(summary(full.2)$coefficients, 3)

# age only
ageonly <-
 glmer(Answer ~ Age.3 +
        (1 + (c.Condition.Counter.Evidence +
               c.Condition.Strong.Evidence) +
           z.Trial || ID),
       data = t.data,
       family = binomial,
       control = contr
 )

# null model
null <-
 glmer(Answer ~ 1 +
        (1 + (c.Condition.Counter.Evidence +
               c.Condition.Strong.Evidence) +
           z.Trial || ID),
       data = t.data,
       family = binomial,
       control = contr
 )

anova(full, null, test = "Chisq")
anova(full.2, null, test = "Chisq")

# belief condition comparisons
library("emmeans")
emm <- emmeans(full.2, ~ Condition)
summary(emm, type = "response")
summary(pairs(emm), type = "response")

save.image("./Study_1_and_2_combined/R_Images/study_1_and_2.RData")
