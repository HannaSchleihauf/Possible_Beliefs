# Study Name: Children and adults' intuitions of what beliefs are possible
# Authors: Joshua Confer, Hanna Schleihauf, Jan Engelmann

# Study 1
# Belief-Action analysis
# March 2021

############################################################################
# PACKAGES & FUNCTIONS
############################################################################

# install.packages("readr")
# install.packages("tidyverse")
# install.packages("lme4")
# install.packages("emmeans")
library(readr)

library(tidyverse)
library(lme4)
library(emmeans)
source("./Study_1/Functions/diagnostic_fcns.r")
source("./Study_1/Functions/drop1_para.r")
source("./Study_1/Functions/glmm_stability.r")
source("./Study_1/functions/boot_glmm.r")
options(scipen = 9999)

############################################################################
# DATA
############################################################################

BO <- read.csv("./Study_1/Data/merged.data.Study1.csv")

# or load image for results
# load("./Study_1/R_Images/Possible_Beliefs_Study_1_Belief_Action_Analysis.RData")

############################################################################
# BELIEF ACTION CONDITION COMPARISONS
############################################################################

# No evidence versus possible actions
BO_no.ap <- subset(BO, BO$Condition == "No Evidence" |
  BO$Condition == "Action Possible")
BO_no.ap$Condition <- as.factor(BO_no.ap$Condition)
BO_no.ap$Condition <- droplevels(BO_no.ap$Condition)
xx.fe.re <- fe.re.tab(
  fe.model = "Answer  ~ Condition*Age.3",
  re = "(1|ID)", data = BO_no.ap
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

t.data$c.Condition.No.Evidence <-
  t.data$Condition.No.Evidence -
  mean(t.data$Condition.No.Evidence)

contr <-
  glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 10000000)
  )

full.no.ap <- glmer(Answer ~ Condition * Age.3 +
  (1 + (c.Condition.No.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
null.no.ap <- glmer(Answer ~ 1 +
  (1 + (c.Condition.No.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
anova(full.no.ap, null.no.ap, test = "Chisq")
round(summary(full.no.ap)$coefficients, 3)

post1 <- drop1p(
  model.res = full.no.ap, para = F,
  data = NULL, contr = contr, n.cores = c("all-1", "all")
)
post1$drop1.res

full.no.ap.2 <- glmer(Answer ~ Condition + Age.3 +
  (1 + (c.Condition.No.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
post2 <- drop1p(
  model.res = full.no.ap.2, para = F,
  data = NULL, contr = contr, n.cores = c("all-1", "all")
)
post2$drop1.res

emm.no.ap <- emmeans(full.no.ap, ~Condition)
summary(emm.no.ap, type = "response")
summary(pairs(emm.no.ap), type = "response")

# No evidence versus impossible actions
BO_no.ip <- subset(BO, BO$Condition == "No Evidence" |
  BO$Condition == "Action Impossible")
BO_no.ip$Condition <-
  as.factor(BO_no.ip$Condition)
BO_no.ip$Condition <-
  relevel(BO_no.ip$Condition, ref = "Action Impossible")
BO_no.ip$Condition <-
  droplevels(BO_no.ip$Condition)
xx.fe.re <- fe.re.tab(
  fe.model = "Answer  ~ Condition*Age.3",
  re = "(1|ID)", data = BO_no.ip
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

t.data$c.Condition.No.Evidence <- t.data$Condition.No.Evidence -
  mean(t.data$Condition.No.Evidence)
contr <- glmerControl(
  optimizer = "Nelder_Mead", optCtrl = list(maxfun = 10000000)
)
full.no.ip <- glmer(Answer ~ Condition * Age.3 +
  (1 + (c.Condition.No.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
null.no.ip <- glmer(Answer ~ 1 +
  (1 + (c.Condition.No.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
anova(full.no.ip, null.no.ip, test = "Chisq")
round(summary(full.no.ip)$coefficients, 3)

post1.no.ip <- drop1p(
  model.res = full.no.ip, para = F, data = NULL,
  contr = contr, n.cores = c("all-1", "all")
)
post1.no.ip$drop1.res

contr <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 10000000)
)
full.no.ip.2 <- glmer(Answer ~ Condition + Age.3 +
  (1 + (c.Condition.No.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
post2.no.ip <- drop1p(
  model.res = full.no.ip.2, para = F, data = NULL,
  contr = contr, n.cores = c("all-1", "all")
)
post2.no.ip$drop1.res

emm.no.ip <- emmeans(full.no.ip, ~Condition)
summary(emm.no.ip, type = "response")
summary(pairs(emm.no.ip), type = "response")

# Counter evidence vs possible actions
BO_c.ap <- subset(BO, BO$Condition == "Counter Evidence" |
  BO$Condition == "Action Possible")
BO_c.ap$Condition <- as.factor(BO_c.ap$Condition)
BO_c.ap$Condition <- droplevels(BO_c.ap$Condition)
xx.fe.re <- fe.re.tab(
  fe.model = "Answer  ~ Condition*Age.3",
  re = "(1|ID)", data = BO_c.ap
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

t.data$c.Condition.Counter.Evidence <-
  t.data$Condition.Counter.Evidence -
  mean(t.data$Condition.Counter.Evidence)

full.c.ap <- glmer(Answer ~ Condition * Age.3 +
  (1 + (c.Condition.Counter.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
null.c.ap <- glmer(Answer ~ 1 +
  (1 + (c.Condition.Counter.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
anova(full.c.ap, null.c.ap, test = "Chisq")
round(summary(full.c.ap)$coefficients, 3)

post1.c <- drop1p(
  model.res = full.c.ap, para = F, data = NULL,
  contr = contr, n.cores = c("all-1", "all")
)
post1.c$drop1.res

full.c.ap.2 <- glmer(Answer ~ Condition + Age.3 +
  (1 + (c.Condition.Counter.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
post2.c <- drop1p(
  model.res = full.c.ap.2, para = F,
  data = NULL, contr = contr, n.cores = c("all-1", "all")
)
post2.c$drop1.res

emm.c.ap <- emmeans(full.c.ap, ~Condition)
summary(emm.c.ap, type = "response")
summary(pairs(emm.c.ap), type = "response")

# Counter evidence vs impossible actions
BO_c.ip <- subset(BO, BO$Condition == "Counter Evidence" |
  BO$Condition == "Action Impossible")
BO_c.ip$Condition <- as.factor(BO_c.ip$Condition)
BO_c.ip$Condition <- relevel(BO_c.ip$Condition, ref = "Action Impossible")
BO_c.ip$Condition <- droplevels(BO_c.ip$Condition)

xx.fe.re <- fe.re.tab(
  fe.model = "Answer  ~ Condition*Age.3",
  re = "(1|ID)", data = BO_c.ip
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

t.data$c.Condition.Counter.Evidence <- t.data$Condition.Counter.Evidence -
  mean(t.data$Condition.Counter.Evidence)

full.c.ip <- glmer(Answer ~ Condition * Age.3 +
  (1 + (c.Condition.Counter.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
null.c.ip <- glmer(Answer ~ 1 +
  (1 + (c.Condition.Counter.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
anova(full.c.ap, null.c.ip, test = "Chisq")
round(summary(full.c.ip)$coefficients, 3)

post1.c.i <- drop1p(
  model.res = full.c.ip, para = F,
  data = NULL, contr = contr, n.cores = c("all-1", "all")
)
post1.c.i$drop1.res

full.c.ip.2 <- glmer(Answer ~ Condition + Age.3 +
  (1 + (c.Condition.Counter.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
post2.c.i <- drop1p(
  model.res = full.c.ip.2, para = F,
  data = NULL, contr = contr, n.cores = c("all-1", "all")
)
post2.c.i$drop1.res

emm.c.ip <- emmeans(full.c.ip, ~Condition)
summary(emm.c.ip, type = "response")
summary(pairs(emm.c.ip), type = "response")

# strong evidence vs possible actions
BO_s.p <- subset(BO, BO$Condition == "Strong Evidence" |
  BO$Condition == "Action Possible")
BO_s.p$Condition <- as.factor(BO_s.p$Condition)
BO_s.p$Condition <- relevel(BO_s.p$Condition, ref = "Action Possible")
BO_s.p$Condition <- droplevels(BO_s.p$Condition)
xx.fe.re <- fe.re.tab(
  fe.model = "Answer  ~ Condition*Age.3",
  re = "(1|ID)", data = BO_s.p
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

t.data$c.Condition.Strong.Evidence <- t.data$Condition.Strong.Evidence -
  mean(t.data$Condition.Strong.Evidence)

full.s.p <- glmer(Answer ~ Condition * Age.3 +
  (1 + (c.Condition.Strong.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
null.s.p <- glmer(Answer ~ 1 +
  (1 + (c.Condition.Strong.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
anova(full.s.p, null.s.p, test = "Chisq")
round(summary(full.s.p)$coefficients, 3)

post1.s.p <- drop1p(
  model.res = full.s.p, para = F,
  data = NULL, contr = contr, n.cores = c("all-1", "all")
)
post1.s.p$drop1.res

full.s.p.2 <- glmer(Answer ~ Condition + Age.3 +
  (1 + (c.Condition.Strong.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
post2.s.p <- drop1p(
  model.res = full.s.p.2, para = F,
  data = NULL, contr = contr, n.cores = c("all-1", "all")
)
post2.s.p$drop1.res

emm.s.p <- emmeans(full.s.p, ~Condition)
summary(emm.s.p, type = "response")
summary(pairs(emm.s.p), type = "response")

# strong evdience vs imposisble actions
BO_s.ip <- subset(BO, BO$Condition == "Strong Evidence" |
  BO$Condition == "Action Impossible")
BO_s.ip$Condition <- as.factor(BO_s.ip$Condition)
BO_s.ip$Condition <- relevel(BO_s.ip$Condition, ref = "Action Impossible")
BO_s.ip$Condition <- droplevels(BO_s.ip$Condition)
xx.fe.re <- fe.re.tab(
  fe.model = "Answer  ~ Condition*Age.3",
  re = "(1|ID)", data = BO_s.ip
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

t.data$c.Condition.Strong.Evidence <- t.data$Condition.Strong.Evidence -
  mean(t.data$Condition.Strong.Evidence)

full.s.ip <- glmer(Answer ~ Condition * Age.3 +
  (1 + (c.Condition.Strong.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
null.s.ip <- glmer(Answer ~ 1 +
  (1 + (c.Condition.Strong.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
anova(full.s.ip, null.s.ip, test = "Chisq")
round(summary(full.s.ip)$coefficients, 3)

post1.s <- drop1p(
  model.res = full.s.ip, para = F,
  data = NULL, contr = contr, n.cores = c("all-1", "all")
)
post1.s$drop1.res

full.s.ip.2 <- glmer(Answer ~ Condition + Age.3 +
  (1 + (c.Condition.Strong.Evidence) || ID),
data = t.data, family = binomial, control = contr
)
post2.s <- drop1p(
  model.res = full.s.ip.2, para = F,
  data = NULL, contr = contr, n.cores = c("all-1", "all")
)
post2.s$drop1.res

emm.s.ip <- emmeans(full.s.ip, ~Condition)
summary(emm.s.ip, type = "response")
summary(pairs(emm.s.ip), type = "response")

save.image("./Study_1/R_Images/Possible_Beliefs_Study_1_Belief_Action_Analysis.RData")
