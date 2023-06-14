# Study Name: Children and adults' intuition of what people can believe
# Authors: Joshua Confer, Hanna Schleihauf, Jan Engelmann

# Study 2
# Belief - Action analysis
# March 2021

############################################################################
# PACKAGES & FUNCTIONS
############################################################################
library("readr")
library("dplyr")
library("tidyr")
library("lme4")
library("emmeans")
library("lme4")
library("tidyverse")
source("./Study_2/Functions/diagnostic_fcns.r")
source("./Study_2/Functions/drop1_para.r")
source("./Study_2/Functions/glmm_stability.r")
source("./Study_2/Functions/boot_glmm.r")
options(scipen = 999)

############################################################################
# DATA
############################################################################
BO.2 <- read.csv("./Study_2/Data/merged.data.Study2.csv")

# or for results load
# load("./Study_2/R_Images/comp_sep_belief_action_analysis.RData")
# then scroll down to evaluate images to look at the results

############################################################################
# BELIEF ACTION CONDITION COMPARISONS
############################################################################

# Fit models for all combinations ---------------------------
# Opinion vs. Action Possible -------------------------------
BO_oo.ap <-
  subset(BO.2, BO.2$Condition == "Opinion" |
    BO.2$Condition == "Action Possible")
BO_oo.ap$Condition <-
  as.factor(BO_oo.ap$Condition)
BO_oo.ap$Condition <-
  droplevels(BO_oo.ap$Condition)
xx.fe.re_oo.ap <-
  fe.re.tab(
    fe.model = "Answer  ~ Condition*Age.3",
    re = "(1|ID)", data = BO_oo.ap
  )
xx.fe.re_oo.ap$summary
t.data_oo.ap <-
  xx.fe.re_oo.ap$data
str(t.data_oo.ap)

# center factors
t.data_oo.ap$c.Condition.Opinion <-
  t.data_oo.ap$Condition.Opinion -
  mean(t.data_oo.ap$Condition.Opinion)

contr <-
  glmerControl(
    optimizer =
      "nlminbwrap",
    optCtrl = list(maxfun = 10000000)
  )
full.oo.ap <-
  glmer(Answer ~ Condition * Age.3 +
    (1 + (c.Condition.Opinion) || ID),
  data = t.data_oo.ap,
  family = binomial,
  control = contr
  )
null.oo.ap <-
  glmer(Answer ~ 1 +
    (1 + (c.Condition.Opinion) || ID),
  data = t.data_oo.ap,
  family = binomial,
  control = contr
  )
anova(full.oo.ap, null.oo.ap, test = "Chisq")
round(summary(full.oo.ap)$coefficients, 3)

post1 <-
  drop1p(
    model.res = full.oo.ap, para = F,
    data = NULL,
    contr = contr,
    n.cores = c("all-1", "all")
  )
round(post1$drop1.res, 3)

full.oo.ap.2 <-
  glmer(Answer ~ Condition + Age.3 +
    (1 + (c.Condition.Opinion) || ID),
  data = t.data_oo.ap,
  family = binomial,
  control = contr
  )
post2 <-
  drop1p(
    model.res = full.oo.ap.2,
    data = NULL,
    contr = contr
  )
round(post2$drop1.res, 3)

emm.oo.ap <-
  emmeans(full.oo.ap, ~Condition)
summary(emm.oo.ap,
  type = "response"
)
summary(pairs(emm.oo.ap),
  type = "response"
)

# Opinion vs. Action Impossible -------------------------------
BO_oo.ip <-
  subset(BO.2, BO.2$Condition == "Opinion" |
    BO.2$Condition == "Action Impossible")
BO_oo.ip$Condition <-
  as.factor(BO_oo.ip$Condition)
BO_oo.ip$Condition <-
  relevel(BO_oo.ip$Condition,
    ref = "Action Impossible"
  )
BO_oo.ip$Condition <-
  droplevels(BO_oo.ip$Condition)
xx.fe.re <-
  fe.re.tab(
    fe.model = "Answer  ~ Condition*Age.3",
    re = "(1|ID)",
    data = BO_oo.ip
  )
xx.fe.re$summary
t.data_oo.ip <-
  xx.fe.re$data
str(t.data_oo.ip)

t.data_oo.ip$c.Condition.Opinion <-
  t.data_oo.ip$Condition.Opinion -
  mean(t.data_oo.ip$Condition.Opinion)

contr <-
  glmerControl(
    optimizer = "nlminbwrap",
    optCtrl = list(maxfun = 10000000)
  )
full.oo.ip <-
  glmer(Answer ~ Condition * Age.3 +
    (1 + (c.Condition.Opinion) || ID),
  data = t.data_oo.ip,
  family = binomial,
  control = contr
  )
null.oo.ip <-
  glmer(Answer ~ 1 +
    (1 + (c.Condition.Opinion) || ID),
  data = t.data_oo.ip,
  family = binomial,
  control = contr
  )
anova(full.oo.ip, null.oo.ip, test = "Chisq")
round(summary(full.oo.ip)$coefficients, 3)

post1.oo.ip <-
  drop1p(
    model.res = full.oo.ip,
    contr = contr
  )
round(post1.oo.ip$drop1.res, 3)

full.oo.ip.2 <-
  glmer(Answer ~ Condition + Age.3 +
    (1 + (c.Condition.Opinion) || ID),
  data = t.data_oo.ip,
  family = binomial,
  control = contr
  )
post2.oo.ip <-
  drop1p(
    model.res = full.oo.ip.2,
    contr = contr
  )
round(post2.oo.ip$drop1.res, 3)

emm.oo.ip <-
  emmeans(full.oo.ip, ~Condition)
summary(emm.oo.ip,
  type = "response"
)
summary(pairs(emm.oo.ip),
  type = "response"
)

# Moral vs. Action Possible -------------------------------
BO_m.p <-
  subset(
    BO.2,
    BO.2$Condition == "Moral" |
      BO.2$Condition == "Action Possible"
  )
BO_m.p$Condition <-
  as.factor(BO_m.p$Condition)
BO_m.p$Condition <-
  relevel(BO_m.p$Condition,
    ref = "Action Possible"
  )
BO_m.p$Condition <-
  droplevels(BO_m.p$Condition)
xx.fe.re <-
  fe.re.tab(
    fe.model = "Answer  ~ Condition*Age.3",
    re = "(1|ID)",
    data = BO_m.p
  )
xx.fe.re$summary
t.data_m.p <-
  xx.fe.re$data
str(t.data_m.p)

t.data_m.p$c.Condition.Moral <-
  t.data_m.p$Condition.Moral -
  mean(t.data_m.p$Condition.Moral)

contr <-
  glmerControl(
    optimizer = "nlminbwrap",
    optCtrl = list(maxfun = 10000000)
  )
full.m.p <-
  glmer(Answer ~ Condition * Age.3 +
    (1 + (c.Condition.Moral) || ID),
  data = t.data_m.p,
  family = binomial,
  control = contr
  )
null.m.p <-
  glmer(Answer ~ 1 +
    (1 + (c.Condition.Moral) || ID),
  data = t.data_m.p,
  family = binomial,
  control = contr
  )
anova(full.m.p, null.m.p, test = "Chisq")
round(summary(full.m.p)$coefficients, 3)

post1.m.p <-
  drop1p(
    model.res = full.m.p,
    contr = contr
  )
round(post1.m.p$drop1.res, 3)

full.m.p.2 <-
  glmer(Answer ~ Condition + Age.3 +
    (1 + (c.Condition.Moral) || ID),
  data = t.data_m.p,
  family = binomial,
  control = contr
  )
post2.m.p <-
  drop1p(
    model.res = full.m.p.2,
    contr = contr
  )
round(post2.m.p$drop1.res, 3)

emm.m.p <-
  emmeans(full.m.p, ~ Condition * Age.3)
summary(emm.m.p, type = "response")
summary(pairs(emm.m.p)[c(1, 4, 7, 9, 10, 11, 14, 15)],
  type = "response"
)

emm.m.p.2 <-
  emmeans(full.m.p.2, ~Condition)
summary(emm.m.p.2, type = "response")
summary(pairs(emm.m.p.2), type = "response")


# Moral vs. Action Impossible -------------------------------
BO_m.ip <-
  subset(
    BO.2,
    BO.2$Condition == "Moral" |
      BO.2$Condition == "Action Impossible"
  )
BO_m.ip$Condition <-
  as.factor(BO_m.ip$Condition)
BO_m.ip$Condition <-
  relevel(BO_m.ip$Condition,
    ref = "Action Impossible"
  )
BO_m.ip$Condition <-
  droplevels(BO_m.ip$Condition)
xx.fe.re <-
  fe.re.tab(
    fe.model = "Answer  ~ Condition*Age.3",
    re = "(1|ID)", data = BO_m.ip
  )
xx.fe.re$summary
t.data_m.ip <-
  xx.fe.re$data
str(t.data_m.ip)

t.data_m.ip$c.Condition.Moral <-
  t.data_m.ip$Condition.Moral -
  mean(t.data_m.ip$Condition.Moral)

full.m.ip <-
  glmer(Answer ~ Condition * Age.3 +
    (1 + (c.Condition.Moral) || ID),
  data = t.data_m.ip,
  family = binomial,
  control = contr
  )
null.m.ip <-
  glmer(Answer ~ 1 +
    (1 + (c.Condition.Moral) || ID),
  data = t.data_m.ip,
  family = binomial,
  control = contr
  )
anova(full.m.ip, null.m.ip, test = "Chisq")
round(summary(full.m.ip)$coefficients, 3)

post1.m <-
  drop1p(
    model.res = full.m.ip,
    contr = contr
  )
post1.m$drop1.res

full.m.ip.2 <-
  glmer(Answer ~ Condition + Age.3 +
    (1 + (c.Condition.Moral) || ID),
  data = t.data_m.ip,
  family = binomial,
  control = contr
  )
post2.m <-
  drop1p(
    model.res = full.m.ip.2,
    contr = contr
  )
post2.m$drop1.res

emm.m.ip <-
  emmeans(full.m.ip, ~Condition)
summary(emm.m.ip, type = "response")
summary(pairs(emm.m.ip), type = "response")

# Comparisons with complete separation problems ---------------------------

# function to keep warnings
keepWarnings <-
  function(expr) {
    localWarnings <-
      list()
    value <-
      withCallingHandlers(expr,
        warning = function(w) {
          localWarnings[[length(localWarnings) + 1]] <<- w
          invokeRestart("muffleWarning")
        }
      )
    list(value = value, warnings = localWarnings)
  }

# Immoral vs. Action Possible -------------------------------

BO_i.ap <-
  subset(
    BO.2,
    BO.2$Condition == "Immoral" |
      BO.2$Condition == "Action Possible"
  )
BO_i.ap$Condition <-
  as.factor(BO_i.ap$Condition)
BO_i.ap$Condition <-
  droplevels(BO_i.ap$Condition)
xx.fe.re <-
  fe.re.tab(
    fe.model = "Answer  ~ Condition*Age.3",
    re = "(1|ID)", data = BO_i.ap
  )
xx.fe.re$summary
t.data_i.ap <-
  xx.fe.re$data
str(t.data_i.ap)

t.data_i.ap$c.Condition.Immoral <-
  t.data_i.ap$Condition.Immoral -
  mean(t.data_i.ap$Condition.Immoral)

# make a new variable for the response
t.data_i.ap$resp <-
  as.numeric(t.data_i.ap$Answer)

# determining in which cells we will be changing one data point at a time
to.change.1 <-
  (1:nrow(t.data_i.ap))[(
    t.data_i.ap$Condition == "Immoral" &
      t.data_i.ap$Age.3 == "7-8 group")]

# creating empty variables to store the results
all.res_i.ap <-
  data.frame(
    n.sim = c(1:length(to.change.1)),
    to.change.1 = NA,
    # full warning
    warnings = NA,
    # all.full.coeffs
    all.full.coeffs.intercept = NA,
    all.full.coeffs.ConditionImmoral = NA,
    all.full.coeffs.Age.37.8.group = NA,
    all.full.coeffs.AgeAdult = NA,
    all.full.coeffs.ConditionImmoral.Age.37.8.group = NA,
    all.full.coeffs.ConditionImmoral.Age.3Adult = NA,

    # test.full.null
    test.full.null.Chisq = NA,
    test.full.null.Df = NA,
    test.full.null.Pr..Chisq = NA,

    # red coefficients
    all.red.coeffs.intercept = NA,
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
    #
    test.main.Age.3.Chisq = NA,
    test.main.Age.3.Chi.Df = NA,
    test.main.Age.3.Pr..Chisq = NA,
    test.main.Age.3.n.opt.warnings = NA,
    test.main.Age.3.n.fun.warnings = NA,
    #
    test.main.Condition.Chisq = NA,
    test.main.Condition.Chi.Df = NA,
    test.main.Condition.Pr..Chisq = NA,
    test.main.Condition.n.opt.warnings = NA,
    test.main.Condition.n.fun.warnings = NA,
    # assumptions
    overdispersion.test.full = NA,
    colliniarity.test.Condition = NA,
    colliniarity.test.Age.3 = NA
  )

emm.post.hoc.full_i.ap <- c()
emm.post.hoc.full.cis_i.ap <- c()
emm.post.hoc.Condition_i.ap <- c()
emm.post.hoc.Condition.cis_i.ap <- c()
emm.post.hoc.Age.3_i.ap <- c()
emm.post.hoc.Age.3.cis_i.ap <- c()

contr <-
  glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 10000000)
  )

for (i in 1:(length(to.change.1))) { # i=1
  set.seed(i)
  t.data_i.ap$new.resp <-
    t.data_i.ap$Answer

  xx <- to.change.1[i]
  t.data_i.ap$new.resp[xx] <-
    ifelse(t.data_i.ap$new.resp[xx] == 0, 1, 0)
  ftable(new.resp ~ Condition + Age.3, t.data_i.ap)

  all.res_i.ap$to.change.1[i] <- xx

  # Full model
  full1 <-
    keepWarnings(glmer(
      new.resp ~ Condition * Age.3 +
        (1 + (c.Condition.Immoral) || ID),
      data = t.data_i.ap,
      family = binomial,
      control = contr
    ))
  red1 <-
    keepWarnings(glmer(
      new.resp ~ Condition + Age.3 +
        (1 + (c.Condition.Immoral) || ID),
      data = t.data_i.ap,
      family = binomial,
      control = contr
    ))
  null <-
    keepWarnings(glmer(
      new.resp ~ 1 +
        (1 + (c.Condition.Immoral) || ID),
      data = t.data_i.ap,
      family = binomial,
      control = contr
    ))

  if (length(full1$warnings) == 0 &
    length(red1$warnings) == 0 &
    length(null$warnings) == 0) {
    full <- full1$value
    red <- red1$value
    null <- null$value

    all.res_i.ap$warnings[i] <- "no"
    summary(full)$coefficients["(Intercept)", 1]
    all.res_i.ap$all.full.coeffs.ConditionImmoral[i] <-
      summary(full)$coefficients["ConditionImmoral", 1]
    all.res_i.ap$all.full.coeffs.Age.37.8.group[i] <-
      summary(full)$coefficients["Age.37-8 group", 1]
    all.res_i.ap$all.full.coeffs.AgeAdult[i] <-
      summary(full)$coefficients["Age.3Adult", 1]

    all.res_i.ap$all.full.coeffs.ConditionImmoral.Age.37.8.group[i] <-
      summary(full)$coefficients["ConditionImmoral:Age.37-8 group", 1]
    all.res_i.ap$all.full.coeffs.ConditionImmoral.Age.3Adult[i] <-
      summary(full)$coefficients["ConditionImmoral:Age.3Adult", 1]

    # full null model comparisons
    test.full.null <-
      as.data.frame(anova(null, full,
        test = "Chisq"
      ))[
        "full",
        c("Chisq", "Df", "Pr(>Chisq)")
      ]
    all.res_i.ap$test.full.null.Chisq[i] <-
      test.full.null$Chisq
    all.res_i.ap$test.full.null.Df[i] <-
      test.full.null$Df
    all.res_i.ap$test.full.null.Pr..Chisq[i] <-
      test.full.null$`Pr(>Chisq)`


    # full-red model comparisons
    tests.full.red <-
      drop1p(
        model.res = full, para = F, contr = contr,
        n.cores = c("all-1", "all"), to.del = NULL
      )
    test.2.way.int <-
      as.data.frame(tests.full.red$drop1.res[2, c(
        "Chisq", "Chi.Df",
        "Pr..Chisq.", "n.opt.warnings",
        "n.fun.warnings"
      )])
    all.res_i.ap$test.2.way.Condition.Age.3.Chisq[i] <-
      test.2.way.int$Chisq
    all.res_i.ap$test.2.way.Condition.Age.3.Chi.Df[i] <-
      test.2.way.int$Chi.Df
    all.res_i.ap$test.2.way.Condition.Age.3.Pr..Chisq[i] <-
      test.2.way.int$Pr..Chisq.
    all.res_i.ap$test.2.way.Condition.Age.3.n.opt.warnings[i] <-
      test.2.way.int$n.opt.warnings
    all.res_i.ap$test.2.way.Condition.Age.3.n.fun.warnings[i] <-
      test.2.way.int$n.fun.warnings

    # assumptions
    all.res_i.ap$overdispersion.test.full[i] <-
      overdisp.test(full)[1, c("dispersion.parameter")]
    # colliniarity
    all.res_i.ap$colliniarity.test.Condition[i] <-
      vif(red)[1, 3]
    all.res_i.ap$colliniarity.test.Age.3[i] <-
      vif(red)[2, 3]

    # post hoc pairwise comparisons model with three-way interaction
    emm <-
      emmeans(full, ~ Condition * Age.3)
    emm.post.hoc.full.cis_i.ap <-
      rbind(
        emm.post.hoc.full.cis_i.ap,
        as.data.frame(emm)
      )

    emm.pairs.full <-
      summary(contrast(emm, "pairwise"),
        type = "response", adjust = "tukey"
      )

    # detect which pairwise comparisons  make sense
    emm.pairs.full$condition.action.possible <-
      str_count(emm.pairs.full$contrast, "Possible")
    emm.pairs.full$condition.immoral <-
      str_count(emm.pairs.full$contrast, "Immoral")
    emm.pairs.full$age.5.6 <-
      str_count(emm.pairs.full$contrast, "5-6 group")
    emm.pairs.full$age.7.8 <-
      str_count(emm.pairs.full$contrast, "7-8 group")
    emm.pairs.full$age.adult <-
      str_count(emm.pairs.full$contrast, "Adult")

    emm.pairs.full$comparisons <- NA

    emm.pairs.full$comparisons <-
      ifelse(emm.pairs.full$condition.action.possible == 2 |
        emm.pairs.full$condition.immoral == 2 |
        emm.pairs.full$age.5.6 == 2 |
        emm.pairs.full$age.7.8 == 2 |
        emm.pairs.full$age.adult == 2,
      1, 0
      )
    comparions.of.interest <-
      which(emm.pairs.full$comparisons == 1)

    emm.pairs.of.interest <-
      summary(contrast(emm, "pairwise")[comparions.of.interest],
        type = "response", adjust = "tukey"
      )

    emm.post.hoc.full_i.ap <-
      rbind(
        emm.post.hoc.full_i.ap,
        data.frame(
          term = emm.pairs.of.interest$contrast,
          odds.ratio = emm.pairs.of.interest$odds.ratio,
          se = emm.pairs.of.interest$SE,
          p.value = emm.pairs.of.interest$p.value
        )
      )

    all.res_i.ap$all.red.coeffs.intercept[i] <-
      summary(red)$coefficients["(Intercept)", 1]
    all.res_i.ap$all.red.coeffs.ConditionImmoral[i] <-
      summary(red)$coefficients["ConditionImmoral", 1]
    all.res_i.ap$all.red.coeffs.Age.37.8.group[i] <-
      summary(red)$coefficients["Age.37-8 group", 1]
    all.res_i.ap$all.red.coeffs.AgeAdult[i] <-
      summary(red)$coefficients["Age.3Adult", 1]

    # red-main model comparisons
    tests.red.main <-
      drop1p(
        model.res = red, para = F,
        contr = contr,
        n.cores = c("all-1", "all"), to.del = NULL
      )
    test.main.Condition <-
      as.data.frame(tests.red.main$drop1.res[2, c(
        "Chisq", "Chi.Df",
        "Pr..Chisq.",
        "n.opt.warnings",
        "n.fun.warnings"
      )])
    test.main.Age.3 <-
      as.data.frame(tests.red.main$drop1.res[3, c(
        "Chisq", "Chi.Df",
        "Pr..Chisq.",
        "n.opt.warnings",
        "n.fun.warnings"
      )])

    all.res_i.ap$test.main.Condition.Chisq[i] <-
      test.main.Condition$Chisq
    all.res_i.ap$test.main.Condition.Chi.Df[i] <-
      test.main.Condition$Chi.Df
    all.res_i.ap$test.main.Condition.Pr..Chisq[i] <-
      test.main.Condition$Pr..Chisq.
    all.res_i.ap$test.main.Condition.n.opt.warnings[i] <-
      test.main.Condition$n.opt.warnings
    all.res_i.ap$test.main.Condition.n.fun.warnings[i] <-
      test.main.Condition$n.fun.warnings

    all.res_i.ap$test.main.Age.3.Chisq[i] <-
      test.main.Age.3$Chisq
    all.res_i.ap$test.main.Age.3.Chi.Df[i] <-
      test.main.Age.3$Chi.Df
    all.res_i.ap$test.main.Age.3.Pr..Chisq[i] <-
      test.main.Age.3$Pr..Chisq.
    all.res_i.ap$test.main.Age.3.n.opt.warnings[i] <-
      test.main.Age.3$n.opt.warnings
    all.res_i.ap$test.main.Age.3.n.fun.warnings[i] <-
      test.main.Age.3$n.fun.warnings
    #
    # post hoc pairwise comparisons model with only one two-way interactions
    emm <-
      emmeans(red, ~Condition)
    emm.pairs.red <-
      summary(contrast(emm, "pairwise"),
        type = "response",
        adjust = "tukey"
      )
    emm.post.hoc.Condition_i.ap <-
      rbind(
        emm.post.hoc.Condition_i.ap,
        data.frame(
          term = emm.pairs.red$contrast,
          odds.ratio = emm.pairs.red$odds.ratio,
          se = emm.pairs.red$SE,
          p.value = emm.pairs.red$p.value
        )
      )

    emm <-
      emmeans(red, ~Age.3)
    emm.pairs.red <-
      summary(contrast(emm, "pairwise"),
        type = "response",
        adjust = "tukey"
      )
    emm.post.hoc.Age.3_i.ap <-
      rbind(
        emm.post.hoc.Age.3_i.ap,
        data.frame(
          term = emm.pairs.red$contrast,
          odds.ratio = emm.pairs.red$odds.ratio,
          se = emm.pairs.red$SE,
          p.value = emm.pairs.red$p.value
        )
      )
  } else {
    all.res_i.ap$warnings[i] <- "yes"
  }

  print(i)
}

save.image("./Study_2/comp_sep_belief_action_analysis.RData")
# load("./Study_2/comp_sep_belief_action_analysis.RData")

# Evaluation of the results -----------------------------------------------
str(all.res_i.ap)
# how many models did converge
sum(all.res_i.ap$warnings == "no")

# means of full-null-comparisons
# Chisq
round(mean(all.res_i.ap$test.full.null.Chisq, na.rm = T), 10)
round(range(all.res_i.ap$test.full.null.Chisq, na.rm = T), 10)
# DF
range(all.res_i.ap$test.full.null.Df, na.rm = T)
# p-value
round(mean(all.res_i.ap$test.full.null.Pr..Chisq, na.rm = T), 10)
round(range(all.res_i.ap$test.full.null.Pr..Chisq, na.rm = T), 10)

# assumption tests
# overdispersion parameter
round(mean(all.res_i.ap$overdispersion.test.full, na.rm = T), 10)
round(range(all.res_i.ap$overdispersion.test.full, na.rm = T), 10)
# colliniarity
round(mean(all.res_i.ap$colliniarity.test.Condition, na.rm = T), 10)
round(range(all.res_i.ap$colliniarity.test.Condition, na.rm = T), 10)
round(mean(all.res_i.ap$colliniarity.test.Age.3, na.rm = T), 10)
round(range(all.res_i.ap$colliniarity.test.Age.3, na.rm = T), 10)

# means of two-way interaction
sum(all.res_i.ap$test.2.way.Condition.Age.3.n.opt.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res_i.ap$test.2.way.Condition.Age.3.Chisq, na.rm = T), 10)
round(range(all.res_i.ap$test.2.way.Condition.Age.3.Chisq, na.rm = T), 10)
# DF
range(all.res_i.ap$test.2.way.Condition.Age.3.Chi.Df, na.rm = T)
# p-value
round(mean(all.res_i.ap$test.2.way.Condition.Age.3.Pr..Chisq, na.rm = T), 10)
round(
  mean(
    all.res_i.ap$test.2.way.Condition.Age.3.Pr..Chisq[
      all.res_i.ap$test.2.way.Condition.Age.3.n.opt.warnings == 0 &
        all.res_i.ap$test.2.way.Condition.Age.3.n.fun.warnings == 0
    ],
    na.rm = T
  ), 10
)
round(range(all.res_i.ap$test.2.way.Condition.Age.3.Pr..Chisq, na.rm = T), 10)
hist(all.res_i.ap$test.2.way.Condition.Age.3.Pr..Chisq)

# Main Effect Age
sum(all.res_i.ap$test.main.Age.3.n.opt.warnings == 0 &
  all.res_i.ap$test.main.Age.3.n.fun.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res_i.ap$test.main.Age.3.Pr..Chisq, na.rm = T), 10)
round(range(all.res_i.ap$test.main.Age.3.Pr..Chisq, na.rm = T), 10)
# DF
range(all.res_i.ap$test.main.Age.3.Chi.Df, na.rm = T)
# p-value
round(mean(all.res_i.ap$test.main.Age.3.Pr..Chisq, na.rm = T), 10)
round(
  mean(all.res_i.ap$test.main.Age.3.Pr..Chisq[
    all.res_i.ap$test.main.Age.3.n.opt.warnings == 0 &
      all.res_i.ap$test.main.Age.3.n.fun.warnings == 0
  ], na.rm = T), 10
)
round(range(all.res_i.ap$test.main.Age.3.Pr..Chisq, na.rm = T), 10)
hist(all.res_i.ap$test.main.Age.3.Pr..Chisq, breaks = seq(
  from = 0, to = 1,
  by = 0.05
))

# Main Effect Condition
sum(all.res_i.ap$test.main.Condition.n.opt.warnings == 0 &
  all.res_i.ap$test.main.Condition.n.fun.warnings == 0, na.rm = T)
# Chisq
round(mean(all.res_i.ap$test.main.Condition.Pr..Chisq, na.rm = T), 10)
round(range(all.res_i.ap$test.main.Condition.Pr..Chisq, na.rm = T), 10)
# DF
range(all.res_i.ap$test.main.Condition.Chi.Df, na.rm = T)
# p-value
round(mean(all.res_i.ap$test.main.Condition.Pr..Chisq, na.rm = T), 10)
round(mean(
  all.res_i.ap$test.main.Condition.Pr..Chisq[
    all.res_i.ap$test.main.Condition.n.opt.warnings == 0 &
      all.res_i.ap$test.main.Condition.n.fun.warnings == 0
  ],
  na.rm = T
), 10)
round(range(all.res_i.ap$test.main.Condition.Pr..Chisq, na.rm = T), 10)
hist(all.res_i.ap$test.main.Condition.Pr..Chisq,
  breaks =
    seq(from = 0, to = 1, by = 0.05)
)

library("tidyselect")
all.coefs <-
  all.res_i.ap %>%
  select(vars_select(
    names(all.res_i.ap),
    starts_with("all.full.coeffs", ignore.case = TRUE)
  ))

round(colMeans(all.coefs, na.rm = T), 3)
data.frame(
  min = sapply(all.coefs, min, na.rm = T),
  max = sapply(all.coefs, max, na.rm = T)
)

# post-hoc full
xx <- mapply(
  FUN = tapply, X = as.data.frame(emm.post.hoc.full_i.ap$p.value),
  MoreArgs = list(INDEX = emm.post.hoc.full_i.ap$term, FUN = mean)
)
round(xx, 3)

# post-hoc red
xx <- mapply(
  FUN = tapply,
  X = as.data.frame(emm.post.hoc.Condition_i.ap$p.value),
  MoreArgs = list(
    INDEX = emm.post.hoc.Condition_i.ap$term,
    FUN = mean
  )
)
round(xx, 3)

xx <- mapply(
  FUN = tapply,
  X = as.data.frame(emm.post.hoc.Age.3_i.ap$p.value),
  MoreArgs = list(
    INDEX = emm.post.hoc.Age.3_i.ap$term,
    FUN = mean
  )
)
round(xx, 3)


# Immoral vs. Action Impossible -------------------------------
BO_i.ip <-
  subset(
    BO.2,
    BO.2$Condition == "Immoral" |
      BO.2$Condition == "Action Impossible"
  )
levels(as.factor(BO_i.ip$Condition))

xx.fe.re <-
  fe.re.tab(
    fe.model = "Answer  ~ Condition*Age.3",
    re = "(1|ID)", data = BO_i.ip
  )
xx.fe.re$summary
t.data_i.ip <-
  xx.fe.re$data
str(t.data_i.ip)

t.data_i.ip$c.Condition.Immoral <-
  t.data_i.ip$Condition.Immoral -
  mean(t.data_i.ip$Condition.Immoral)

# make a new variable for the response
t.data_i.ip$resp <-
  as.numeric(t.data_i.ip$Answer)

# determining in which cells we will be changing one data point at a time
to.change.1 <-
  (1:nrow(t.data_i.ip))[(t.data_i.ip$Condition == "Immoral" &
    t.data_i.ip$Age.3 == "7-8 group")]

# creating empty variables to store the results
all.res_i.ip <-
  data.frame(
    n.sim = c(1:length(to.change.1)),
    to.change.1 = NA,
    # full warning
    warnings = NA,
    # all.full.coeffs
    all.full.coeffs.intercept = NA,
    all.full.coeffs.ConditionImmoral = NA,
    all.full.coeffs.Age.37.8.group = NA,
    all.full.coeffs.AgeAdult = NA,
    all.full.coeffs.ConditionImmoral.Age.37.8.group = NA,
    all.full.coeffs.ConditionImmoral.Age.3Adult = NA,

    # test.full.null
    test.full.null.Chisq = NA,
    test.full.null.Df = NA,
    test.full.null.Pr..Chisq = NA,

    # red coefficients
    all.red.coeffs.intercept = NA,
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
    #
    test.main.Age.3.Chisq = NA,
    test.main.Age.3.Chi.Df = NA,
    test.main.Age.3.Pr..Chisq = NA,
    test.main.Age.3.n.opt.warnings = NA,
    test.main.Age.3.n.fun.warnings = NA,
    #
    test.main.Condition.Chisq = NA,
    test.main.Condition.Chi.Df = NA,
    test.main.Condition.Pr..Chisq = NA,
    test.main.Condition.n.opt.warnings = NA,
    test.main.Condition.n.fun.warnings = NA,
    # assumptions
    overdispersion.test.full = NA,
    colliniarity.test.Condition = NA,
    colliniarity.test.Age.3 = NA
  )

emm.post.hoc.full_i.ip <- c()
emm.post.hoc.full.cis_i.ip <- c()
emm.post.hoc.Condition_i.ip <- c()
emm.post.hoc.Condition.cis_i.ip <- c()
emm.post.hoc.Age.3_i.ip <- c()
emm.post.hoc.Age.3.cis_i.ip <- c()

contr <-
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))

for (i in 1:(length(to.change.1))) { # i = 1
  set.seed(i)
  t.data_i.ip$new.resp <-
    t.data_i.ip$Answer

  xx <- to.change.1[i]
  t.data_i.ip$new.resp[xx] <-
    ifelse(t.data_i.ip$new.resp[xx] == 0, 1, 0)
  ftable(
    new.resp ~
      Condition + Age.3,
    t.data_i.ip
  )

  all.res_i.ip$to.change.1[i] <- xx

  # Full model
  full1 <-
    keepWarnings(glmer(new.resp ~ Condition * Age.3 +
      (1 + (c.Condition.Immoral) || ID),
    data = t.data_i.ip, family = binomial, control = contr
    ))
  red1 <-
    keepWarnings(glmer(new.resp ~ Condition + Age.3 +
      (1 + (c.Condition.Immoral) || ID),
    data = t.data_i.ip, family = binomial, control = contr
    ))
  null <-
    keepWarnings(glmer(new.resp ~ 1 +
      (1 + (c.Condition.Immoral) || ID),
    data = t.data_i.ip, family = binomial, control = contr
    ))

  if (length(full1$warnings) == 0 &
    length(red1$warnings) == 0 &
    length(null$warnings) == 0) {
    full <- full1$value
    red <- red1$value
    null <- null$value

    all.res_i.ip$warnings[i] <- "no"
    all.res_i.ip$all.full.coeffs.intercept[i] <-
      summary(full)$coefficients["(Intercept)", 1]
    all.res_i.ip$all.full.coeffs.ConditionImmoral[i] <-
      summary(full)$coefficients["ConditionImmoral", 1]
    all.res_i.ip$all.full.coeffs.Age.37.8.group[i] <-
      summary(full)$coefficients["Age.37-8 group", 1]
    all.res_i.ip$all.full.coeffs.AgeAdult[i] <-
      summary(full)$coefficients["Age.3Adult", 1]

    all.res_i.ip$all.full.coeffs.ConditionImmoral.Age.37.8.group[i] <-
      summary(full)$coefficients["ConditionImmoral:Age.37-8 group", 1]
    all.res_i.ip$all.full.coeffs.ConditionImmoral.Age.3Adult[i] <-
      summary(full)$coefficients["ConditionImmoral:Age.3Adult", 1]

    # full null model comparisons
    test.full.null <-
      as.data.frame(anova(null, full, test = "Chisq"))[
        "full",
        c(
          "Chisq", "Df",
          "Pr(>Chisq)"
        )
      ]
    all.res_i.ip$test.full.null.Chisq[i] <-
      test.full.null$Chisq
    all.res_i.ip$test.full.null.Df[i] <-
      test.full.null$Df
    all.res_i.ip$test.full.null.Pr..Chisq[i] <-
      test.full.null$`Pr(>Chisq)`


    # full-red model comparisons
    tests.full.red <-
      drop1p(
        model.res = full, para = F,
        contr = contr, n.cores = c("all-1", "all"), to.del = NULL
      )
    test.2.way.int <-
      as.data.frame(tests.full.red$drop1.res[2, c(
        "Chisq", "Chi.Df",
        "Pr..Chisq.",
        "n.opt.warnings",
        "n.fun.warnings"
      )])
    all.res_i.ip$test.2.way.Condition.Age.3.Chisq[i] <-
      test.2.way.int$Chisq
    all.res_i.ip$test.2.way.Condition.Age.3.Chi.Df[i] <-
      test.2.way.int$Chi.Df
    all.res_i.ip$test.2.way.Condition.Age.3.Pr..Chisq[i] <-
      test.2.way.int$Pr..Chisq.
    all.res_i.ip$test.2.way.Condition.Age.3.n.opt.warnings[i] <-
      test.2.way.int$n.opt.warnings
    all.res_i.ip$test.2.way.Condition.Age.3.n.fun.warnings[i] <-
      test.2.way.int$n.fun.warnings

    # assumptions
    all.res_i.ip$overdispersion.test.full[i] <-
      overdisp.test(full)[1, c("dispersion.parameter")]
    # colliniarity
    all.res_i.ip$colliniarity.test.Condition[i] <-
      vif(red)[1, 3]
    all.res_i.ip$colliniarity.test.Age.3[i] <-
      vif(red)[2, 3]

    # post hoc pairwise comparisons model with three-way interaction
    emm <- emmeans(full, ~ Condition * Age.3)
    emm.post.hoc.full.cis_i.ip <-
      rbind(
        emm.post.hoc.full.cis_i.ip,
        as.data.frame(emm)
      )

    emm.pairs.full <-
      summary(contrast(emm, "pairwise"),
        type = "response", adjust = "tukey"
      )

    # detect comparisons that make sense
    emm.pairs.full$condition.action.possible <-
      str_count(emm.pairs.full$contrast, "Possible")
    emm.pairs.full$condition.immoral <-
      str_count(emm.pairs.full$contrast, "Immoral")
    emm.pairs.full$age.5.6 <-
      str_count(emm.pairs.full$contrast, "5-6 group")
    emm.pairs.full$age.7.8 <-
      str_count(emm.pairs.full$contrast, "7-8 group")
    emm.pairs.full$age.adult <-
      str_count(emm.pairs.full$contrast, "Adult")

    emm.pairs.full$comparisons <- NA

    emm.pairs.full$comparisons <-
      ifelse(emm.pairs.full$condition.action.possible == 2 |
        emm.pairs.full$condition.immoral == 2 |
        emm.pairs.full$age.5.6 == 2 |
        emm.pairs.full$age.7.8 == 2 |
        emm.pairs.full$age.adult == 2,
      1, 0
      )
    comparions.of.interest <-
      which(emm.pairs.full$comparisons == 1)

    emm.pairs.of.interest <-
      summary(contrast(emm, "pairwise")[comparions.of.interest],
        type = "response", adjust = "tukey"
      )

    emm.post.hoc.full_i.ip <-
      rbind(
        emm.post.hoc.full_i.ip,
        data.frame(
          term = emm.pairs.of.interest$contrast,
          odds.ratio = emm.pairs.of.interest$odds.ratio,
          se = emm.pairs.of.interest$SE,
          p.value = emm.pairs.of.interest$p.value
        )
      )

    all.res_i.ip$all.red.coeffs.intercept[i] <-
      summary(red)$coefficients["(Intercept)", 1]
    all.res_i.ip$all.red.coeffs.ConditionImmoral[i] <-
      summary(red)$coefficients["ConditionImmoral", 1]
    all.res_i.ip$all.red.coeffs.Age.37.8.group[i] <-
      summary(red)$coefficients["Age.37-8 group", 1]
    all.res_i.ip$all.red.coeffs.AgeAdult[i] <-
      summary(red)$coefficients["Age.3Adult", 1]

    # red-main model comparisons
    tests.red.main <-
      drop1p(
        model.res = red, para = F,
        contr = contr,
        n.cores = c("all-1", "all"), to.del = NULL
      )
    test.main.Condition <-
      as.data.frame(tests.red.main$drop1.res[2, c(
        "Chisq", "Chi.Df",
        "Pr..Chisq.",
        "n.opt.warnings",
        "n.fun.warnings"
      )])
    test.main.Age.3 <-
      as.data.frame(tests.red.main$drop1.res[3, c(
        "Chisq", "Chi.Df",
        "Pr..Chisq.",
        "n.opt.warnings",
        "n.fun.warnings"
      )])

    all.res_i.ip$test.main.Condition.Chisq[i] <-
      test.main.Condition$Chisq
    all.res_i.ip$test.main.Condition.Chi.Df[i] <-
      test.main.Condition$Chi.Df
    all.res_i.ip$test.main.Condition.Pr..Chisq[i] <-
      test.main.Condition$Pr..Chisq.
    all.res_i.ip$test.main.Condition.n.opt.warnings[i] <-
      test.main.Condition$n.opt.warnings
    all.res_i.ip$test.main.Condition.n.fun.warnings[i] <-
      test.main.Condition$n.fun.warnings

    all.res_i.ip$test.main.Age.3.Chisq[i] <-
      test.main.Age.3$Chisq
    all.res_i.ip$test.main.Age.3.Chi.Df[i] <-
      test.main.Age.3$Chi.Df
    all.res_i.ip$test.main.Age.3.Pr..Chisq[i] <-
      test.main.Age.3$Pr..Chisq.
    all.res_i.ip$test.main.Age.3.n.opt.warnings[i] <-
      test.main.Age.3$n.opt.warnings
    all.res_i.ip$test.main.Age.3.n.fun.warnings[i] <-
      test.main.Age.3$n.fun.warnings
    #
    # post hoc pairwise comparisons model with only one two-way interactions
    emm <-
      emmeans(red, ~Condition)
    emm.pairs.red <-
      summary(contrast(emm, "pairwise"),
        type = "response", adjust = "tukey"
      )
    emm.post.hoc.Condition_i.ip <-
      rbind(
        emm.post.hoc.Condition_i.ip,
        data.frame(
          term = emm.pairs.red$contrast,
          odds.ratio = emm.pairs.red$odds.ratio,
          se = emm.pairs.red$SE,
          p.value = emm.pairs.red$p.value
        )
      )

    emm <-
      emmeans(red, ~Age.3)
    emm.pairs.red <-
      summary(contrast(emm, "pairwise"),
        type = "response", adjust = "tukey"
      )
    emm.post.hoc.Age.3_i.ip <-
      rbind(
        emm.post.hoc.Age.3_i.ip,
        data.frame(
          term = emm.pairs.red$contrast,
          odds.ratio = emm.pairs.red$odds.ratio,
          se = emm.pairs.red$SE,
          p.value = emm.pairs.red$p.value
        )
      )
  } else {
    all.res_i.ip$warnings[i] <- "yes"
  }
  print(i)
}

save.image("./Study_2/R_Images/comp_sep_belief_action_analysis.RData")

# Evaluation of the results -----------------------------------------------
str(all.res_i.ip)
# how many models did converge
sum(all.res_i.ip$warnings == "no")

# means of full-null-comparisons
# Chisq
round(mean(all.res_i.ip$test.full.null.Chisq,
  na.rm = T
), 10)
round(range(all.res_i.ip$test.full.null.Chisq,
  na.rm = T
), 10)
# DF
range(all.res_i.ip$test.full.null.Df, na.rm = T)
# p-value
round(mean(all.res_i.ip$test.full.null.Pr..Chisq,
  na.rm = T
), 10)
round(range(all.res_i.ip$test.full.null.Pr..Chisq,
  na.rm = T
), 10)

# assumption tests
# overdispersion parameter
round(
  mean(all.res_i.ip$overdispersion.test.full,
    na.rm = T
  ),
  10
)
round(
  range(all.res_i.ip$overdispersion.test.full,
    na.rm = T
  ),
  10
)
# colliniarity
round(
  mean(all.res_i.ip$colliniarity.test.Condition,
    na.rm = T
  ),
  10
)
round(
  range(all.res_i.ip$colliniarity.test.Condition,
    na.rm = T
  ),
  10
)
round(
  mean(all.res_i.ip$colliniarity.test.Age.3,
    na.rm = T
  ),
  10
)
round(
  range(all.res_i.ip$colliniarity.test.Age.3,
    na.rm = T
  ),
  10
)

# means of two-way interaction
sum(all.res_i.ip$test.2.way.Condition.Age.3.n.opt.warnings == 0,
  na.rm = T
)
# Chisq
round(
  mean(
    all.res_i.ip$test.2.way.Condition.Age.3.Chisq,
    na.rm = T
  ), 10
)
round(
  range(
    all.res_i.ip$test.2.way.Condition.Age.3.Chisq,
    na.rm = T
  ), 10
)
# DF
range(all.res_i.ip$test.2.way.Condition.Age.3.Chi.Df,
  na.rm = T
)
# p-value
round(
  mean(
    all.res_i.ip$test.2.way.Condition.Age.3.Pr..Chisq,
    na.rm = T
  ),
  10
)
round(
  mean(
    all.res_i.ip$test.2.way.Condition.Age.3.Pr..Chisq[
      all.res_i.ip$test.2.way.Condition.Age.3.n.opt.warnings == 0 &
        all.res_i.ip$test.2.way.Condition.Age.3.n.fun.warnings == 0
    ],
    na.rm = T
  ), 10
)
round(
  range(
    all.res_i.ip$test.2.way.Condition.Age.3.Pr..Chisq,
    na.rm = T
  ),
  10
)
hist(all.res_i.ip$test.2.way.Condition.Age.3.Pr..Chisq)

# Main Effect Age
sum(all.res_i.ip$test.main.Age.3.n.opt.warnings == 0 &
  all.res_i.ip$test.main.Age.3.n.fun.warnings == 0,
na.rm = T
)
# Chisq
round(
  mean(
    all.res_i.ip$test.main.Age.3.Pr..Chisq,
    na.rm = T
  ),
  10
)
round(
  range(
    all.res_i.ip$test.main.Age.3.Pr..Chisq,
    na.rm = T
  ),
  10
)
# DF
range(all.res_i.ip$test.main.Age.3.Chi.Df,
  na.rm = T
)
# p-value
round(
  mean(all.res_i.ip$test.main.Age.3.Pr..Chisq,
    na.rm = T
  ),
  10
)
round(mean(all.res_i.ip$test.main.Age.3.Pr..Chisq[
  all.res_i.ip$test.main.Age.3.n.opt.warnings == 0 &
    all.res_i.ip$test.main.Age.3.n.fun.warnings == 0
], na.rm = T), 10)
round(
  range(
    all.res_i.ip$test.main.Age.3.Pr..Chisq,
    na.rm = T
  ),
  10
)
hist(all.res_i.ip$test.main.Age.3.Pr..Chisq,
  breaks = seq(from = 0, to = 1, by = 0.05)
)

# Main Effect Condition
sum(all.res_i.ip$test.main.Condition.n.opt.warnings == 0 &
  all.res_i.ip$test.main.Condition.n.fun.warnings == 0,
na.rm = T
)
# Chisq
round(
  mean(
    all.res_i.ip$test.main.Condition.Pr..Chisq,
    na.rm = T
  ),
  10
)
round(
  range(
    all.res_i.ip$test.main.Condition.Pr..Chisq,
    na.rm = T
  ),
  10
)
# DF
range(all.res_i.ip$test.main.Condition.Chi.Df, na.rm = T)
# p-value
round(mean(all.res_i.ip$test.main.Condition.Pr..Chisq, na.rm = T), 10)
round(mean(all.res_i.ip$test.main.Condition.Pr..Chisq[
  all.res_i.ip$test.main.Condition.n.opt.warnings == 0 &
    all.res_i.ip$test.main.Condition.n.fun.warnings == 0
], na.rm = T), 10)
round(range(all.res_i.ip$test.main.Condition.Pr..Chisq, na.rm = T), 10)
hist(all.res_i.ip$test.main.Condition.Pr..Chisq,
  breaks = seq(from = 0, to = 1, by = 0.05)
)

library("tidyselect")
all.coefs <-
  all.res_i.ip %>%
  select(vars_select(
    names(all.res_i.ip),
    starts_with("all.full.coeffs", ignore.case = TRUE)
  ))

round(colMeans(all.coefs, na.rm = T), 3)
data.frame(
  min = sapply(all.coefs, min, na.rm = T),
  max = sapply(all.coefs, max, na.rm = T)
)

# post-hoc full
xx <- mapply(
  FUN = tapply, X = as.data.frame(emm.post.hoc.full_i.ip$p.value),
  MoreArgs = list(INDEX = emm.post.hoc.full_i.ip$term, FUN = mean)
)
round(xx, 3)

# post-hoc red
xx <- mapply(
  FUN = tapply,
  X = as.data.frame(emm.post.hoc.Condition_i.ip$p.value),
  MoreArgs = list(
    INDEX = emm.post.hoc.Condition_i.ip$term, FUN = mean
  )
)
round(xx, 3)

xx <- mapply(
  FUN = tapply, X = as.data.frame(emm.post.hoc.Age.3_i.ip$p.value),
  MoreArgs = list(INDEX = emm.post.hoc.Age.3_i.ip$term, FUN = mean)
)
round(xx, 3)
