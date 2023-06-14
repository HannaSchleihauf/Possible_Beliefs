# Study Name: Children and adults' intuition of what people can believe
# Authors: Joshua Confer, Hanna Schleihauf, Jan Engelmann

# Study 2
# Belief analysis without random slopes
# February 2022

############################################################################
# PACKAGES & FUNCTIONS
############################################################################

library("lme4")
library("tidyverse")
library("parallel")
library("dfoptim")
library("optimx")
library("emmeans")
library("car")
source("./Study_2/Functions/diagnostic_fcns.r")
source("./Study_2/Functions/drop1_para.r")
source("./Study_2/Functions/glmm_stability.r")
source("./Study_2/Functions/boot_glmm.r")
options(scipen = 9999)

############################################################################
# DATA
############################################################################

BO.2 <- read.csv("./Study_2/Data/merged.data.Study2.csv")
# or for results load
# load("./Study_2/R_Images/comp_sep_belief_analysis_with_adjusted_model.RData")

############################################################################
# ANALYSIS
############################################################################

# preparation for model fitting ------------------------------------
ftable(Answer ~ Condition + Age.3, BO.2)
table(BO.2$Answer, BO.2$Condition, BO.2$Age.3,
  useNA = "ifany"
)
# here we see that we have in a few cells a 0
# --> therefore we might have a complete separation problem

# add variable trial per condition
BO.2$trial.per.condition <-
  ave(BO.2$ID, list(BO.2$ID, BO.2$Condition),
    FUN = seq_along
  )

levels(as.factor(BO.2$Condition))
BO.belief <-
  subset(BO.2, BO.2$Condition == "Opinion" |
    BO.2$Condition == "Moral" |
    BO.2$Condition == "Immoral")
BO.belief <-
  droplevels(BO.belief)
levels(as.factor(BO.belief$Condition))
BO.belief$Condition <-
  factor(BO.belief$Condition, ordered = FALSE)
BO.belief$Condition <-
  factor(BO.belief$Condition,
    levels = c(
      "Opinion",
      "Immoral",
      "Moral"
    )
  )
levels(as.factor(BO.belief$Condition))

# prepare data frame for analysis (dummy-code factors etc.)
xx.fe.re <-
  fe.re.tab(
    fe.model = "Answer ~
    Condition*Age.3",
    re = "(1|ID)", other.vars =
      c(
        "Trial",
        "trial.per.condition"
      ),
    data = BO.belief
  )
xx.fe.re$summary
t.data <-
  xx.fe.re$data
str(t.data)

# center and z-transform variables that are in the random slopes
t.data$c.Condition.Immoral <-
  as.numeric(t.data$Condition.Immoral) -
  mean(as.numeric(t.data$Condition.Immoral))
t.data$c.Condition.Moral <-
  as.numeric(t.data$Condition.Moral) -
  mean(as.numeric(t.data$Condition.Moral))
t.data$c.Age.3.7.8.group <-
  as.numeric(t.data$Age.3.7.8.group) -
  mean(as.numeric(t.data$Age.3.7.8.group))
t.data$c.Age.3.Adult <-
  as.numeric(t.data$Age.3.Adult) -
  mean(as.numeric(t.data$Age.3.Adult))
t.data$z.Trial <-
  scale(t.data$Trial)

# fitting the model as pre-registered
contr <-
  glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 10000000)
  )
full.inital <-
  glmer(
    Answer ~
      Condition * Age.3 +
      (1 + (c.Condition.Immoral +
        c.Condition.Moral) +
        z.Trial || ID),
    data = t.data,
    family = binomial,
    control = contr
  )
summary(full.inital)$coefficients
# large standard errors indicate a complete separation problem

# Changing response to solve the complete separation problem --------------
# function to keep warnings
keepWarnings <-
  function(expr) {
    localWarnings <- list()
    value <- withCallingHandlers(expr,
      warning = function(w) {
        localWarnings[[length(localWarnings) + 1]] <<- w
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = localWarnings)
  }

# make a new variable for the response
t.data$resp <-
  as.numeric(t.data$Answer)

# check again which cells have a complete separation issue (have no 1s in it)
ftable(
  Answer ~ Condition +
    Age.3, t.data
) # immoral 7-8 year olds
table(
  tapply(
    t.data$resp,
    list(
      t.data$ID,
      t.data$Condition
    ), sum
  )
)

# determining in which cells we will be changing one data point at a time
to.change.1 <-
  (1:nrow(t.data))[(t.data$Condition == "Immoral" &
    t.data$Age.3 == "7-8 group")]

# creating empty variables to store the results
all.res <- data.frame(
  n.sim = c(1:length(to.change.1)),
  to.change.1 = NA,
  # full warning
  full.warnings = NA,
  # all.full.coeffs
  all.full.coeffs.intercept = NA,
  all.full.coeffs.ConditionMoral = NA,
  all.full.coeffs.ConditionImmoral = NA,
  all.full.coeffs.Age.37.8.group = NA,
  all.full.coeffs.AgeAdult = NA,
  all.full.coeffs.ConditionMoral.Age.37.8.group = NA,
  all.full.coeffs.ConditionMoral.Age.3Adult = NA,
  all.full.coeffs.ConditionImmoral.Age.37.8.group = NA,
  all.full.coeffs.ConditionImmoral.Age.3Adult = NA,

  # null warnings
  null.warnings = NA,
  # test.full.null
  test.full.null.Chisq = NA,
  test.full.null.Df = NA,
  test.full.null.Pr..Chisq = NA,

  # red warnings
  red.warnings = NA,
  # red coefficients

  all.red.coeffs.intercept = NA,
  all.red.coeffs.ConditionMoral = NA,
  all.red.coeffs.ConditionImmoral = NA,
  all.red.coeffs.Age.37.8.group = NA,
  all.red.coeffs.AgeAdult = NA,

  # red reduced model comparisons
  # Condition*Age
  test.2.way.Condition.Age.3.Chisq = NA,
  test.2.way.Condition.Age.3.Chi.Df = NA,
  test.2.way.Condition.Age.3.Pr..Chisq = NA,
  test.2.way.Condition.Age.3.n.opt.warnings = NA,
  test.2.way.Condition.Age.3.n.fun.warnings = NA,
  test.main.Age.3.Chisq = NA,
  test.main.Age.3.Chi.Df = NA,
  test.main.Age.3.Pr..Chisq = NA,
  test.main.Age.3.n.opt.warnings = NA,
  test.main.Age.3.n.fun.warnings = NA,
  test.main.Condition.Chisq = NA,
  test.main.Condition.Chi.Df = NA,
  test.main.Condition.Pr..Chisq = NA,
  test.main.Condition.n.opt.warnings = NA,
  test.main.Condition.n.fun.warnings = NA,
  # assumptions
  colliniarity.test.Condition = NA,
  colliniarity.test.Age.3 = NA
)
emm.post.hoc.full <- c()
emm.post.hoc.full.cis <- c()
emm.post.hoc.Condition <- c()
emm.post.hoc.Condition.cis <- c()
emm.post.hoc.Age.3 <- c()
emm.post.hoc.Age.3.cis <- c()

contr <-
  glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 10000000)
  )
for (i in 1:(length(to.change.1))) { # i = 1
  set.seed(i)
  t.data$new.resp <-
    t.data$Answer
  xx <- to.change.1[i]
  all.res$to.change.1[i] <-
    xx
  t.data$new.resp[xx] <-
    ifelse(t.data$new.resp[xx] == 0, 1, 0)

  # Full model
  full1 <-
    keepWarnings(
      glmer(new.resp ~
        Condition * Age.3 +
        (1 | ID),
      data = t.data,
      family = binomial,
      control = contr
      )
    )
  red1 <-
    keepWarnings(
      glmer(new.resp ~
        Condition + Age.3 +
        (1 | ID),
      data = t.data,
      family = binomial,
      control = contr
      )
    )

  if (length(full1$warnings) == 0 &
    length(red1$warnings) == 0) {
    full <-
      full1$value
    red <-
      red1$value
    all.res$full.warnings[i] <- "no"
    all.res$all.full.coeffs.intercept[i] <-
      summary(full)$coefficients["(Intercept)", 1]
    all.res$all.full.coeffs.ConditionMoral[i] <-
      summary(full)$coefficients["ConditionMoral", 1]
    all.res$all.full.coeffs.ConditionImmoral[i] <-
      summary(full)$coefficients["ConditionImmoral", 1]
    all.res$all.full.coeffs.Age.37.8.group[i] <-
      summary(full)$coefficients["Age.37-8 group", 1]
    all.res$all.full.coeffs.AgeAdult[i] <-
      summary(full)$coefficients["Age.3Adult", 1]
    all.res$all.full.coeffs.ConditionMoral.Age.37.8.group[i] <-
      summary(full)$coefficients["ConditionMoral:Age.37-8 group", 1]
    all.res$all.full.coeffs.ConditionMoral.Age.3Adult[i] <-
      summary(full)$coefficients["ConditionMoral:Age.3Adult", 1]
    all.res$all.full.coeffs.ConditionImmoral.Age.37.8.group[i] <-
      summary(full)$coefficients["ConditionImmoral:Age.37-8 group", 1]
    all.res$all.full.coeffs.ConditionImmoral.Age.3Adult[i] <-
      summary(full)$coefficients["ConditionImmoral:Age.3Adult", 1]

    # full-red model comparisons
    tests.full.red <-
      drop1p(
        model.res = full, para = F, contr = contr,
        n.cores = c("all-1", "all"), to.del = NULL
      )
    test.2.way.int <-
      as.data.frame(tests.full.red$drop1.res[2, c(
        "Chisq", "Chi.Df",
        "Pr..Chisq.",
        "n.opt.warnings",
        "n.fun.warnings"
      )])
    all.res$test.2.way.Condition.Age.3.Chisq[i] <-
      test.2.way.int$Chisq
    all.res$test.2.way.Condition.Age.3.Chi.Df[i] <-
      test.2.way.int$Chi.Df
    all.res$test.2.way.Condition.Age.3.Pr..Chisq[i] <-
      test.2.way.int$Pr..Chisq.
    all.res$test.2.way.Condition.Age.3.n.opt.warnings[i] <-
      test.2.way.int$n.opt.warnings
    all.res$test.2.way.Condition.Age.3.n.fun.warnings[i] <-
      test.2.way.int$n.fun.warnings

    # assumptions
    # colliniarity
    all.res$colliniarity.test.Condition[i] <-
      vif(red)[1, 3]
    all.res$colliniarity.test.Age.3[i] <-
      vif(red)[2, 3]

    # post hoc pairwise comparisons model with three-way interaction
    emm <-
      emmeans(full, ~ Condition * Age.3)
    emm.post.hoc.full <-
      rbind(
        emm.post.hoc.full,
        as.data.frame(summary(emm, type = "scale"))
      )
    emm.post.hoc.full.cis <-
      rbind(
        emm.post.hoc.full.cis,
        as.data.frame(summary(emm,
          type = "response"
        ))
      )

    emm.pairs.full <-
      summary(contrast(emm, "pairwise")[c(
        1, 2, 3, 6, 9, 11, 14, 18, 21, 22, 23, 24, 29, 33, 34, 35, 36
      )],
      type = "response",
      adjust = "sidak"
      )
  } else {
    all.res$full.warnings[i] <- "yes"
  }

  # Null model
  null1 <-
    keepWarnings(
      glmer(new.resp ~ 1 +
        (1 | ID),
      data = t.data,
      family = binomial,
      control = contr
      )
    )
  if (length(full1$warnings) == 0 &
    length(null1$warnings) == 0) {
    null <- null1$value
    all.res$null.warnings[i] <- "no"

    # full null model comparisons
    test.full.null <-
      as.data.frame(anova(null, full,
        test = "Chisq"
      ))["full", c(
        "Chisq", "Df",
        "Pr(>Chisq)"
      )]
    all.res$test.full.null.Chisq[i] <-
      test.full.null$Chisq
    all.res$test.full.null.Df[i] <-
      test.full.null$Df
    all.res$test.full.null.Pr..Chisq[i] <-
      test.full.null$`Pr(>Chisq)`
  } else {
    all.res$null.warnings[i] <- "yes"
  }

  # Red model
  if (length(red1$warnings) == 0) {
    red <- red1$value
    all.res$red.warnings[i] <- "no"
    all.res$all.red.coeffs.intercept[i] <-
      summary(red)$coefficients["(Intercept)", 1]
    all.res$all.red.coeffs.ConditionMoral[i] <-
      summary(red)$coefficients["ConditionMoral", 1]
    all.res$all.red.coeffs.ConditionImmoral[i] <-
      summary(red)$coefficients["ConditionImmoral", 1]
    all.res$all.red.coeffs.Age.37.8.group[i] <-
      summary(red)$coefficients["Age.37-8 group", 1]
    all.res$all.red.coeffs.AgeAdult[i] <-
      summary(red)$coefficients["Age.3Adult", 1]

    # red-main model comparisons
    tests.red.main <-
      drop1p(
        model.res = red, para = F, contr = contr,
        n.cores = c("all-1", "all"), to.del = NULL
      )
    test.main.Condition <-
      as.data.frame(tests.red.main$drop1.res[2, c(
        "Chisq",
        "Chi.Df",
        "Pr..Chisq.",
        "n.opt.warnings",
        "n.fun.warnings"
      )])
    test.main.Age.3 <-
      as.data.frame(
        tests.red.main$drop1.res[
          3,
          c(
            "Chisq",
            "Chi.Df",
            "Pr..Chisq.",
            "n.opt.warnings",
            "n.fun.warnings"
          )
        ]
      )
    all.res$test.main.Age.3.Chisq[i] <-
      test.main.Age.3$Chisq
    all.res$test.main.Age.3.Chi.Df[i] <-
      test.main.Age.3$Chi.Df
    all.res$test.main.Age.3.Pr..Chisq[i] <-
      test.main.Age.3$Pr..Chisq.
    all.res$test.main.Age.3.n.opt.warnings[i] <-
      test.main.Age.3$n.opt.warnings
    all.res$test.main.Age.3.n.fun.warnings[i] <-
      test.main.Age.3$n.fun.warnings
    all.res$test.main.Condition.Chisq[i] <-
      test.main.Condition$Chisq
    all.res$test.main.Condition.Chi.Df[i] <-
      test.main.Condition$Chi.Df
    all.res$test.main.Condition.Pr..Chisq[i] <-
      test.main.Condition$Pr..Chisq.
    all.res$test.main.Condition.n.opt.warnings[i] <-
      test.main.Condition$n.opt.warnings
    all.res$test.main.Condition.n.fun.warnings[i] <-
      test.main.Condition$n.fun.warnings

    # post hoc pairwise comparisons model with only one two-way interactions
    emm <-
      emmeans(red, ~Condition)
    emm.pairs.red <-
      summary(contrast(emm, "pairwise"),
        type = "response", adjust = "tukey"
      )
    emm.post.hoc.Condition <-
      rbind(emm.post.hoc.Condition, data.frame(
        term = emm.pairs.red$contrast,
        odds.ratio = emm.pairs.red$odds.ratio,
        se = emm.pairs.red$SE,
        p.value = emm.pairs.red$p.value
      ))
    emm <-
      emmeans(red, ~Age.3)
    emm.pairs.red <-
      summary(contrast(emm, "pairwise"),
        type = "response", adjust = "tukey"
      )
    emm.post.hoc.Age.3 <-
      rbind(emm.post.hoc.Age.3, data.frame(
        term = emm.pairs.red$contrast,
        odds.ratio = emm.pairs.red$odds.ratio,
        se = emm.pairs.red$SE,
        p.value = emm.pairs.red$p.value
      ))
  }
  print(i)
}

save.image(
  "./Study_2/R_Images/comp_sep_belief_analysis_with_adjusted_model.RData"
)
# load("./Study_2/R_Images/comp_sep_belief_analysis_with_adjusted_model.RData")

############################################################################
# EVALUATION OF THE RESULTS
############################################################################

str(all.res)
# how many models did converge
# full
sum(all.res$full.warnings == "no")
# null
sum(all.res$null.warnings == "no")
# red
sum(all.res$red.warnings == "no")

# means of full-null-comparisons
# Chisq
round(mean(all.res$test.full.null.Chisq, na.rm = T), 10)
round(range(all.res$test.full.null.Chisq, na.rm = T), 10)
# DF
range(all.res$test.full.null.Df, na.rm = T)
# p-value
round(mean(all.res$test.full.null.Pr..Chisq, na.rm = T), 10)
round(range(all.res$test.full.null.Pr..Chisq, na.rm = T), 10)

# colliniarity
round(mean(all.res$colliniarity.test.Condition, na.rm = T), 10)
round(range(all.res$colliniarity.test.Condition, na.rm = T), 10)
round(mean(all.res$colliniarity.test.Age.3, na.rm = T), 10)
round(range(all.res$colliniarity.test.Age.3, na.rm = T), 10)

# means of two-way interaction
sum(all.res$test.2.way.Condition.Age.3.n.opt.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res$test.2.way.Condition.Age.3.Chisq, na.rm = T), 10)
round(range(all.res$test.2.way.Condition.Age.3.Chisq, na.rm = T), 10)
# DF
range(all.res$test.2.way.Condition.Age.3.Chi.Df, na.rm = T)
# p-value
round(mean(all.res$test.2.way.Condition.Age.3.Pr..Chisq, na.rm = T), 10)
round(mean(all.res$test.2.way.Condition.Age.3.Pr..Chisq[
  all.res$test.2.way.Condition.Age.3.n.opt.warnings == 0 &
    all.res$test.2.way.Condition.Age.3.n.fun.warnings == 0
], na.rm = T), 10)
round(range(all.res$test.2.way.Condition.Age.3.Pr..Chisq, na.rm = T), 10)
hist(all.res$test.2.way.Condition.Age.3.Pr..Chisq)

# Main Effect Age
sum(all.res$test.main.Age.3.n.opt.warnings == 0 &
  all.res$test.main.Age.3.n.fun.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res$test.main.Age.3.Chisq, na.rm = T), 10)
round(range(all.res$test.main.Age.3.Chisq, na.rm = T), 10)
# DF
range(all.res$test.main.Age.3.Chi.Df, na.rm = T)
# p-value
round(mean(all.res$test.main.Age.3.Pr..Chisq, na.rm = T), 10)
round(mean(all.res$test.main.Age.3.Pr..Chisq[
  all.res$test.main.Age.3.n.opt.warnings == 0 &
    all.res$test.main.Age.3.n.fun.warnings == 0
], na.rm = T), 10)
round(range(all.res$test.main.Age.3.Pr..Chisq, na.rm = T), 10)
hist(all.res$test.main.Age.3.Pr..Chisq,
  breaks = seq(from = 0, to = 1, by = 0.05)
)

# Main Effect Condition
sum(all.res$test.main.Condition.n.opt.warnings == 0 &
  all.res$test.main.Condition.n.fun.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res$test.main.Condition.Chisq, na.rm = T), 10)
round(range(all.res$test.main.Condition.Chisq, na.rm = T), 10)
# DF
range(all.res$test.main.Condition.Chi.Df, na.rm = T)
# p-value
round(mean(
  all.res$test.main.Condition.Pr..Chisq,
  na.rm = T
), 10)
round(mean(
  all.res$test.main.Condition.Pr..Chisq[
    all.res$test.main.Condition.n.opt.warnings == 0 &
      all.res$test.main.Condition.n.fun.warnings == 0
  ],
  na.rm = T
), 10)
round(range(all.res$test.main.Condition.Pr..Chisq, na.rm = T), 10)
hist(all.res$test.main.Condition.Pr..Chisq,
  breaks = seq(from = 0, to = 1, by = 0.05)
)

library("tidyselect")
all.coefs <-
  all.res %>%
  select(vars_select(
    names(all.res),
    starts_with("all.full.coeffs", ignore.case = TRUE)
  ))

round(colMeans(all.coefs, na.rm = T), 3)
data.frame(
  min =
    sapply(all.coefs, min, na.rm = T),
  max = sapply(all.coefs, max, na.rm = T)
)

# CIs emmeans
head(emm.post.hoc.full.cis)
emm.post.hoc.full.cis$term <-
  paste(emm.post.hoc.full.cis$Condition, emm.post.hoc.full.cis$Age.3)
mean.prob <-
  round(tapply(emm.post.hoc.full.cis$prob,
    emm.post.hoc.full.cis$term,
    FUN = mean
  ), 3)
min.prob <-
  round(tapply(emm.post.hoc.full.cis$prob,
    emm.post.hoc.full.cis$term,
    FUN = min
  ), 3)
max.prob <-
  round(tapply(emm.post.hoc.full.cis$prob,
    emm.post.hoc.full.cis$term,
    FUN = max
  ), 3)

min.CI <-
  round(tapply(emm.post.hoc.full.cis$asymp.LCL,
    emm.post.hoc.full.cis$term,
    FUN = min
  ), 3)
max.CI <-
  round(tapply(emm.post.hoc.full.cis$asymp.UCL,
    emm.post.hoc.full.cis$term,
    FUN = min
  ), 3)
mean.CI.lower <-
  round(tapply(emm.post.hoc.full.cis$asymp.LCL,
    emm.post.hoc.full.cis$term,
    FUN = mean
  ), 3)
mean.CI.upper <-
  round(tapply(emm.post.hoc.full.cis$asymp.UCL,
    mm.post.hoc.full.cis$term,
    FUN = mean
  ), 3)
cbind(min.CI, max.CI)
cbind(mean.CI.lower, mean.CI.upper)

# CIs emmeans
head(emm.post.hoc.full)
emm.post.hoc.full$term <-
  paste(emm.post.hoc.full$Condition, emm.post.hoc.full$Age.3)
mean.est <-
  round(tapply(emm.post.hoc.full$emmean, emm.post.hoc.full$term, FUN = mean), 3)
min.est <-
  round(tapply(emm.post.hoc.full$emmean, emm.post.hoc.full$term, FUN = min), 3)
max.est <-
  round(tapply(emm.post.hoc.full$emmean, emm.post.hoc.full$term, FUN = max), 3)

min.CI <-
  round(tapply(emm.post.hoc.full$asymp.LCL,
    emm.post.hoc.full$term,
    FUN = min
  ), 3)
max.CI <-
  round(tapply(emm.post.hoc.full$asymp.UCL,
    emm.post.hoc.full$term,
    FUN = min
  ), 3)
mean.CI.lower <-
  round(tapply(emm.post.hoc.full$asymp.LCL,
    emm.post.hoc.full$term,
    FUN = mean
  ), 3)
mean.CI.upper <-
  round(tapply(emm.post.hoc.full$asymp.UCL,
    emm.post.hoc.full$term,
    FUN = mean
  ), 3)
cbind(min.CI, max.CI)
cbind(mean.CI.lower, mean.CI.upper)


############################################################################
# MAIN GRAPH study 2
############################################################################

BO.2 <- read.csv("./Study_2/merged.data.csv")

library("ggplot2")

BO.2$Condition <-
  factor(BO.2$Condition,
    levels = c(
      "Action Possible", "Opinion",
      "Immoral", "Moral", "Action Impossible"
    )
  )

xx <- ftable(Answer ~ Condition + Age.3, BO.2)

## frequency table as I need it, with changed signs
yy <- xx[, 1] + xx[, 2]
xx <- xx / yy

freq <- as.data.frame(xx)
freq$new <- NA
freq$new[freq$Answer == 0] <-
  freq$Freq[freq$Answer == 0] * (-1)
freq$new[freq$Answer == 1] <-
  freq$Freq[freq$Answer == 1]

freq <- freq %>%
  mutate(Condition = dplyr::recode(Condition,
    "Opinion" = "Opinion",
    "Immoral" = "Immoral",
    "Moral" = "Moral",
    "Action Possible" = "Possible Action",
    "Action Impossible" = "Impossible Action"
  ))

pdf("./Study_2/Plots/Study_2_Main_Plot_Adjusted.pdf",
    width = 10, height = 6)
p <- freq %>%
  ggplot(aes(x = Age.3, y = new, fill = factor(Answer))) +
  geom_bar(stat = "identity", width = .65) +
  facet_wrap(~Condition, nrow = 1) +
  theme(strip.background = element_blank()) +
  theme(strip.text.x = element_text(size = 12)) +
  xlab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("5-6 y/o", "7-8 y/o", "Adults")) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key = element_rect(size = 12),
    legend.key.size = unit(4, "cm"),
    legend.key.height = unit(4, "line"),
    legend.key.width = unit(2, "line"),
    legend.text =
      element_text(size = 12, colour = "gray30", family = "sans"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x.bottom =
      element_text(size = 12, colour = "gray30", family = "sans"),
    axis.text.y =
      element_text(size = 12, colour = "gray30", family = "sans"),
    axis.text.x =
      element_text(size = 12, colour = "gray30", family = "sans"),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(colour = "gray30", fill = NA),
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(
    labels =
      c("Belief/action is impossible", "Belief/action is possible"),
    values = c("darkgreen", "goldenrod1"),
    guide = guide_legend(reverse = TRUE)
  )

p
dev.off()

############################################################################
# PLOT WITH ESTIMATES AND CONFIDENCE INTERVALS
############################################################################

xx <-
  data.frame(
    term = c(levels(as.factor(emm.post.hoc.full.cis$term))),
    Condition = c(rep("Immoral", 3), rep("Moral", 3), rep("Opinion", 3)),
    Age.3 = c(rep(c("5-6 group", "7-8 group", "Adult"), 3)),
    prob = mean.prob, asymp.LCL = mean.CI.lower, asymp.UCL = mean.CI.upper
  )
xx$Condition <- factor(xx$Condition, # Reordering group factor levels
  levels = c("Opinion", "Immoral", "Moral")
)
pdf("./Study_2/Plots/Study_2_Estimates_and_CIs_Adjusted.pdf",
    width = 8, height = 6)
p <- ggplot(
  xx,
  aes(
    x = interaction(Age.3),
    y = prob
  )
) +
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL), size = 1) +
  facet_wrap(~Condition, nrow = 1) +
  xlab("") +
  ylab("predicted probability that\nparticipants judge belief possible") +
  scale_x_discrete(
    labels =
      c("5-6 y/o", "7-8 y/o", "Adults")
  ) +
  theme(
    axis.text.x = element_text(
      size = 10, face = NULL,
      margin = margin(
        t = 2, r = 0, b = 0, l = 0,
        unit = "mm"
      )
    ),
    axis.title.x = element_text(
      size = 5, face = NULL,
      margin = margin(
        t = 10, r = 0, b = 0, l = 0,
        unit = "mm"
      )
    ),
    axis.title.y = element_text(
      size = 12, face = NULL,
      margin = margin(
        t = 0, r = 3, b = 0, l = 0,
        unit = "mm"
      )
    )
  )
p
dev.off()
