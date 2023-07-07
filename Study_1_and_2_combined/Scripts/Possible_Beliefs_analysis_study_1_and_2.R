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
source("./Study_2/Functions/drop1_para.r")
options(scipen = 9999)

############################################################################
# DATA
############################################################################
BO.1 <- read.csv("./Study_1/Data/merged.data.Study1.csv")
BO.2 <- read.csv("./Study_2/Data/merged.data.Study2.csv")

# or for results load
# load("./Study_1_and_2_combined/R_Images/study_1_and_2.RData")

BO.1 <- BO.1 %>%
  select(
    X, ID, Age.3,
    Stimuli,
    Trial,
    Condition,
    Answer,
    Gender
  )

BO.2 <- BO.2 %>%
  select(
    X, ID, Age.3,
    Stimuli,
    Trial,
    Condition,
    Answer,
    Gender
  )

BO <- rbind(BO.1, BO.2)

############################################################################
# SUBSETTING BELIEF CONDITIONS FOR MAIN ANALYSIS
############################################################################

BO_belief <-
  subset(
    BO,
      BO$Condition == "Strong Evidence" |
      BO$Condition == "Moral"
  )
str(BO_belief)
BO_belief$Condition <-
  droplevels(as.factor(BO_belief$Condition))
str(BO_belief)
BO_belief$Condition <-
  relevel(as.factor(BO_belief$Condition),
    ref = "Strong Evidence"
  )

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
t.data$z.Trial <- scale(t.data$Trial)

# center and z-transform variables that are in the random slopes
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
    (1 + (c.Condition.Moral) +
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
test.1 <- drop1p(
  model.res = full,
  contr = contr
)
test.1$drop1.res

full.2 <- glmer(Answer ~
  Condition + Age.3 +
  (1 + (c.Condition.Moral) +
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

# belief condition comparisons
library("emmeans")
emm <- emmeans(full.2, ~Condition)
summary(emm, type = "response")
summary(pairs(emm), type = "response")

save.image("./Study_1_and_2_combined/R_Images/study_1_and_2.RData")
