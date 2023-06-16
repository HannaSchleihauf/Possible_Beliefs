# Study Name: Simulation - BeliefOtherwise power analysis -
# binomial GLMM - only experimental conditions minimal"
# Authors: Joshua Confer, Hanna Schleihauf, Jan Engelmann

# Study 1 & 2
# Belief analysis
# March "8/9/2020"

library(tidyverse)
library(dplyr)
library(cowplot)
source("./Study_1/Functions/drop1_para.r")

# For results of the power analysis load
# load("./Power_Simulation/R_Images/Power_Simulation.RData")
# Then scroll down to the evaluation of results

## Expected performance levels
per.no.evidence.4.5 <- 0.6
per.no.evidence.6.7 <- 0.7
per.no.evidence.adults <- 0.9
per.high.evidence.for.4.5 <- 0.35
per.high.evidence.for.6.7 <- 0.20
per.high.evidence.for.adults <- 0.05
per.high.evidence.against.4.5 <- 0.60
per.high.evidence.against.6.7 <- 0.75
per.high.evidence.against.adults <- 0.90

## Generate predictors
set.seed(1)
n.subject <- 120 # number subjects
n.per.subject <- 9 # observations per subject
n.condition <- 3 # number of conditions
n.per.condition <- 3 # observations per subject and condition
subj.id <-
  as.factor(paste("subj", 1:n.subject, sep = "."))

start.data <-
  data.frame(subj.id)
# duplicate rows according to the number obs. per subject:
start.data <-
  start.data[rep(
    x = 1:nrow(start.data),
    times = n.per.subject
  ), ]
start.data <- as.data.frame(start.data)
names(start.data) <- "subj.id"

# add condition and trial number
start.data <-
  data.frame(expand.grid(
    subj.id = subj.id,
    condition = c(
      ".no.evidence",
      ".high.evidence.for",
      ".high.evidence.against"
    ),
    trial.per.condition = c(1:n.per.condition)
  ))

# add age group
start.data$age.group <-
  as.factor(rep(
    x = c(".4.5", ".6.7", ".adults"),
    times = n.subject / 3
  ))[as.numeric(start.data$subj.id)]
# add sex
start.data <-
  start.data[order(start.data$subj.id), ]
start.data$gender <-
  as.factor(rep(
    x = c(".male", ".female"),
    times = n.subject / 2
  ))[as.numeric(start.data$subj.id)]
# add trial number
start.data$trial <-
  as.factor(rep(seq(from = 1, to = 9), times = n.subject))

# check whether it worked
ftable(condition ~
         age.group + gender, start.data) / 2

# z-transformation of covariates
start.data$z.trial <-
  as.vector(scale(as.numeric(start.data$trial)))
start.data$z.trial.per.condition <-
  as.vector(scale(as.numeric(start.data$trial.per.condition)))

levels(start.data$condition)

# dummy code factors and center them for random slopes
start.data$condition.high.evidence.for <-
  as.numeric(start.data$condition == levels(start.data$condition)[2])
start.data$condition.high.evidence.against <-
  as.numeric(start.data$condition == levels(start.data$condition)[3])

# centering
start.data$condition.high.evidence.for.c <-
  as.numeric(start.data$condition.high.evidence.for) -
  mean(as.numeric(start.data$condition.high.evidence.for))
start.data$condition.high.evidence.against.c <-
  as.numeric(start.data$condition.high.evidence.against) -
  mean(as.numeric(start.data$condition.high.evidence.against))

# dummy coding
start.data$age.group.6.7 <-
  as.numeric(start.data$age.group == levels(start.data$age.group)[2])
start.data$age.group.adults <-
  as.numeric(start.data$age.group == levels(start.data$age.group)[3])
start.data$age.group.6.7.c <-
  start.data$age.group.6.7 -
  mean(start.data$age.group.6.7) # centering
start.data$age.group.adults.c <-
  start.data$age.group.adults -
  mean(start.data$age.group.adults) # centering

# dummy coding
start.data$gender.male <-
  as.numeric(start.data$gender == levels(start.data$gender)[2])
# centering
start.data$gender.male.c <-
  start.data$gender.male - mean(start.data$gender.male)

# checks:
# does each subject have only one sex and age?
xx <- table(start.data$subj.id, start.data$gender)
range(apply(X = xx > 0, MARGIN = 1, sum)) # should be 1 and 1

xx <- table(start.data$subj.id, start.data$age.group)
range(apply(X = xx > 0, MARGIN = 1, sum)) # should be 1 and 1

xx <- table(start.data$subj.id, start.data$condition)
range(apply(X = xx > 0, MARGIN = 1, sum)) # should be 3 and 3

xx <- table(start.data$subj.id, start.data$trial)
range(apply(X = xx > 0, MARGIN = 1, sum)) # should be 10 and 10

n.simus <- 100 # small number for testing
r.effects <-
  c(0.1, 0.2, 0.4)
qlogis(per.no.evidence.4.5)

r.slope.high.evidence.for <-
  c(0.25, 0.5)
qlogis(per.high.evidence.for.4.5) -
  qlogis(per.no.evidence.4.5)

r.slope.high.evidence.against <-
  c(0.025, 0.05)
qlogis(per.high.evidence.against.4.5) -
  qlogis(per.no.evidence.4.5)

r.slope.trial <- 0.05

# create object to store the simulation parameters and results:
all.res <-
  data.frame(expand.grid(
    n.per.subject = n.per.subject,
    r.effect = r.effects,
    r.slope.high.evidence.for =
      r.slope.high.evidence.for,
    r.slope.high.evidence.against =
      r.slope.high.evidence.against,
    simu = 1:n.simus
  ))

all.res$icpt <- NA

all.res$condition.high.evidence.for <- NA
all.res$condition.high.evidence.against <- NA
all.res$age.group.6.7 <- NA
all.res$age.group.adults <- NA

all.res$condition.high.evidence.for.6.7 <- NA
all.res$condition.high.evidence.for.adults <- NA
all.res$condition.high.evidence.against.6.7 <- NA
all.res$condition.high.evidence.against.adults <- NA

all.res$re.sd <- NA
all.res$warns.full <- NA
all.res$warns.null <- NA

all.res$lrt.p.condition <- NA
all.res$lrt.p.age.group <- NA
all.res$lrt.p.condition.age.group <- NA
all.res$full.null.p <- NA

# load packages needed:
library(lme4)
# Loading required package: Matrix
library(kyotil) # we want to store info about convergence issues

# define control structure to make convergence more likely:
contr <-
  glmerControl(optimizer = "bobyqa",
               optCtrl = list(maxfun = 10000))
xdata <-
  start.data

m.mat <-
  model.matrix(object = ~ condition * age.group +
                 gender, data = xdata) # create model martix
colnames(m.mat)

per.no.evidence.4.5 <- 0.6
per.no.evidence.6.7 <- 0.7
per.no.evidence.adults <- 0.9
per.high.evidence.for.4.5 <- 0.35
per.high.evidence.for.6.7 <- 0.20
per.high.evidence.for.adults <- 0.05
per.high.evidence.against.4.5 <- 0.60
per.high.evidence.against.6.7 <- 0.75
per.high.evidence.against.adults <- 0.90

coefs <- c(
  "(Intercept)" =
    qlogis(per.no.evidence.4.5),
  "condition.high.evidence.for" =
    qlogis(per.high.evidence.for.4.5) -
    qlogis(per.no.evidence.4.5),
  "condition.high.evidence.against" =
    qlogis(per.high.evidence.against.4.5) -
    qlogis(per.no.evidence.4.5),
  "age.group.6.7" =
    qlogis(per.no.evidence.6.7) -
    qlogis(per.no.evidence.4.5),
  "age.group.adults" =
    qlogis(per.no.evidence.adults) -
    qlogis(per.no.evidence.4.5),
  "gender.male" = 0,
  "condition.high.evidence.for:age.group.6.7" =
    qlogis(per.high.evidence.for.6.7) -
    (qlogis(per.no.evidence.4.5) + # "intercept"
       (qlogis(per.high.evidence.for.4.5) -
          qlogis(per.no.evidence.4.5)) + # "condition.high.evidence.for"
       (qlogis(per.no.evidence.6.7) -
          qlogis(per.no.evidence.4.5))), # "age.group.6.7"
  "condition.high.evidence.against:age.group.6.7" =
    qlogis(per.high.evidence.against.6.7) -
    (qlogis(per.no.evidence.4.5) + # "intercept"
       (qlogis(per.high.evidence.against.4.5) -
          qlogis(per.no.evidence.4.5)) + # "condition.high.evidence.against"
       (qlogis(per.no.evidence.6.7) -
          qlogis(per.no.evidence.4.5))), # "age.group.6.7"
  "condition.high.evidence.for:age.group.adults" =
    qlogis(per.high.evidence.for.adults) -
    (qlogis(per.no.evidence.4.5) + # "intercept"
       (qlogis(per.high.evidence.for.4.5) -
          qlogis(per.no.evidence.4.5)) + # "condition.high.evidence.for"
       (qlogis(per.no.evidence.adults) -
          qlogis(per.no.evidence.4.5))), # "age.group.adults"
  "condition.high.evidence.against:age.group.adults" =
    qlogis(per.high.evidence.against.adults) -
    (qlogis(per.no.evidence.4.5) + # "intercept"
       (qlogis(per.high.evidence.against.4.5) -
          qlogis(per.no.evidence.4.5)) + # "condition.high.evidence.against"
       (qlogis(per.no.evidence.adults) -
          qlogis(per.no.evidence.4.5)))
) # "age.group.adults"

LP <- m.mat[, names(coefs)] %*% coefs # LP wrt fixed effects

# run simulation
for (i in 1:nrow(all.res)) {
  set.seed(i) # allows to later replicate individual simulations

  # add random effect to linear predictor:
  LP <-
    LP + rnorm(
      n = n.subject,
      sd = all.res[i, "r.effect"]
    )[as.numeric(xdata$subj.id)] +
    rnorm(n = n.subject,
          sd = all.res[i,
                       "r.slope.high.evidence.for"])[
                         as.numeric(xdata$subj.id)] *
    xdata$condition.high.evidence.for +
    rnorm(n = n.subject,
          sd = all.res[i,
                       "r.slope.high.evidence.against"])[
                         as.numeric(xdata$subj.id)] *
    xdata$condition.high.evidence.against

  # generate response:
  xdata$coding <-
    rbinom(n = nrow(xdata), size = 1, prob = exp(LP) / (1 + exp(LP)))

  xxdata <-
    subset(xdata, xdata$condition == ".no.evidence" |
             xdata$condition == ".high.evidence.for" |
             xdata$condition == ".high.evidence.against")


  # fit full model:
  full <-
    keepWarnings(glmer(
      coding ~ condition * age.group +
        (1 + (condition.high.evidence.for.c +
                condition.high.evidence.against.c) +
           z.trial || subj.id),
      data = xxdata,
      family = binomial,
      control = contr
    ))

  # fit null model:
  null <- keepWarnings(glmer(
    coding ~ 1 +
      (1 + (condition.high.evidence.for.c +
              condition.high.evidence.against.c) +
         z.trial || subj.id),
    data = xxdata,
    family = binomial,
    control = contr
  ))

  red1 <- keepWarnings(glmer(
    coding ~ condition + age.group +
      (1 + (condition.high.evidence.for.c +
              condition.high.evidence.against.c) +
         z.trial || subj.id),
    data = xxdata,
    family = binomial,
    control = contr
  ))

  # store results:
  all.res[i, c(
    "icpt", "condition.high.evidence.for",
    "condition.high.evidence.against",
    "age.group.6.7", "age.group.adults",
    "condition.high.evidence.for.6.7",
    "condition.high.evidence.for.adults",
    "condition.high.evidence.against.6.7",
    "condition.high.evidence.against.adults"
  )] <-
    fixef(full$value)

  all.res[i, "re.sd"] <-
    as.data.frame(summary(full$value)$varcor)[1, "sdcor"]
  all.res[i, "warns.full"] <-
    nchar(paste(full$warnings, collapse = ""))
  all.res[i, "warns.null"] <-
    nchar(paste(null$warnings, collapse = ""))
  all.res[i, "full.null.p"] <-
    as.data.frame(anova(null$value,
                        full$value,
                        test = "Chisq"
    ))[2, "Pr(>Chisq)"]

  all.res[i, "lrt.p.condition.age.group"] <-
    as.data.frame(
      drop1(
        full$value,
        test = "Chisq"))["condition:age.group", "Pr(Chi)"]

  xx <- drop1(red1$value, test = "Chisq")
  all.res[i, "lrt.p.condition"] <-
    as.data.frame(xx)["condition", "Pr(Chi)"]
  all.res[i, "lrt.p.age.group"] <-
    as.data.frame(xx)["age.group", "Pr(Chi)"]
}

save.image("./Power_Simulation/R_Images/Power_Simulation.RData")
# load("./Power_Simulation/R_Images/Power_Simulation.RData")


## Evaluation of results
# number of warning per combinations of random effects
# Full model:
ftable(r.effect ~
         r.slope.high.evidence.against +
         r.slope.high.evidence.for, all.res)

# full model
tapply(
  X = all.res[, "warns.full"] > 0,
  INDEX = all.res[, c(
    "r.slope.high.evidence.against",
    "r.slope.high.evidence.for", "r.effect"
  )],
  FUN = sum, rm.na = TRUE
)

sum(all.res[, "warns.full"] > 0)
30 / 1200
# warnings in 25% when we had 30

# warning codes:
# 363: unable to evaluate scaled gradient.
# Model failed to converge: degenerate Hessian with 1 negative eigenvalues
# 205: Model is nearly unidentifiable: large eigenvalue ratio -
# Rescale variables?

# Null model:
tapply(
  X = all.res[, "warns.null"] > 0,
  INDEX = all.res[, c(
    "r.slope.high.evidence.against",
    "r.slope.high.evidence.for", "r.effect"
  )],
  FUN = sum
)

# plot estimates
par(mar = c(3, 3, 0.2, 0.2), mgp = c(1.7, 0.3, 0), tcl = -0.15, las = 1)
plot(
  x = as.numeric(as.factor(rep(
    x = c(
      "icpt", "condition.high.evidence.for",
      "condition.high.evidence.against",
      "age.group.6.7", "age.group.adults",
      "condition.high.evidence.for.6.7",
      "condition.high.evidence.for.adults",
      "condition.high.evidence.against.6.7",
      "condition.high.evidence.against.adults",
      "re.sd"
    ),
    each = nrow(all.res)
  ))),
  y = unlist(all.res[, c(
    "icpt", "condition.high.evidence.for",
    "condition.high.evidence.against",
    "age.group.6.7", "age.group.adults",
    "condition.high.evidence.for.6.7",
    "condition.high.evidence.for.adults",
    "condition.high.evidence.against.6.7",
    "condition.high.evidence.against.adults",
    "re.sd"
  )]),
  pch = 19, col = grey(level = 0.2, alpha = 0.2),
  xaxt = "n", xlim = c(0.5, 10.5), ylab = "estimate", xlab = ""
)
mtext(
  text = c(
    "icpt", "condition.high.evidence.for",
    "condition.high.evidence.against",
    "age.group.6.7", "age.group.adults",
    "condition.high.evidence.for.6.7",
    "condition.high.evidence.for.adults",
    "condition.high.evidence.against.6.7",
    "condition.high.evidence.against.adults", "re.sd"
  ),
  side = 1, at = , 1:20, line = 0.2
)

## Only models that converged are evaluated from here on:
all.res2 <- subset(all.res, warns.full == 0)

lrt.data <- all.res2 %>%
  group_by(
    r.effect, r.slope.high.evidence.against,
    r.slope.high.evidence.for
  ) %>%
  summarise(
    lrt.p.condition.mean =
      mean(lrt.p.condition),
    # mean condition p-value of the models that converged
    n.sign.lrt =
      length(lrt.p.condition[lrt.p.condition < 0.05]),
    # number of significant condition effects
    n.lrt =
      n.simus, # length(lrt.p.con), #number of iterations
    proportion.sign.lrt =
      length(lrt.p.condition[lrt.p.condition < 0.05]) / n.simus, # proportion of significant condition effects
    prw.lrt =
      mean(all.res2$lrt.p.condition <= 0.05), # power for interaction effect
    full.null.p.mean =
      mean(full.null.p), # mean full-null model p-value of the models that converged
    n.sign.full.null.p =
      length(full.null.p[full.null.p < 0.05]), # number of significant full-null model comparisons
    n.fn = n.simus, # length(full.null.p), #number of iterations
    proportion.sign.fn =
      length(full.null.p[full.null.p < 0.05]) / n.simus
  ) # proportion of significant full-null model comparisons
lrt.data

lrt.data2 <- all.res2 %>%
  filter(full.null.p < 0.05) %>%
  group_by(
    r.effect,
    r.slope.high.evidence.against,
    r.slope.high.evidence.for
  ) %>%
  summarise(
    lrt.p.condition.mean2 =
      mean(lrt.p.condition),
    n.sign.lrt2 =
      length(lrt.p.condition[lrt.p.condition < 0.05]),
    n.lrt =
      n.simus, # length(lrt.p.condition.age.group),
    pwr =
      mean(lrt.p.condition <= 0.05),
    proportion.sign.lrt2 =
      length(lrt.p.condition[lrt.p.condition <= 0.05]) / n.simus
  )
lrt.data2

lrt.data3 <- all.res2 %>%
  filter(full.null.p < 0.05) %>%
  group_by(
    r.effect, r.slope.high.evidence.against,
    r.slope.high.evidence.for
  ) %>%
  summarise(lrt.p.condition.mean3 =
              mean(lrt.p.condition <= 0.05))
lrt.data3

# save.image("./Power_Simulation/R_Images/Power_Simulation.RData")
