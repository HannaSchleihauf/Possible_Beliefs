# Study Name: Children and adults' intuitions of what beliefs are possible
# Authors: Joshua Confer, Hanna Schleihauf, Jan Engelmann

# Study 1
# Belief analysis
# March 2021

############################################################################
# PACKAGES & FUNCTIONS
############################################################################

# install.packages("readr")
# install.packages("tidyverse")
# install.packages("lme4")
library(readr)
library(tidyverse)
library(lme4)
source("./Study_1/Functions/diagnostic_fcns.r")
source("./Study_1/Functions/drop1_para.r")
source("./Study_1/Functions/glmm_stability.r")
source("./Study_1/functions/boot_glmm.r")
options(scipen = 9999)

############################################################################
# DATA
############################################################################

BO <-
  read.csv("./Study_1/Data/merged.data.Study1.csv")

# or for results load
# load("./Study_1/R_Images/merged.data.Study1.csv")

library(dplyr)
BO.sum <- BO %>%
  group_by(ID, Gender, Age.3, Condition) %>%
  summarise(Answer = sum(Answer))

ftable(Answer ~ Condition + Age.3, BO.sum)

############################################################################
# SUBSETTING BELIEF CONDITIONS FOR MAIN ANALYSIS
############################################################################

BO_belief <- subset(
  BO,
  BO$Condition == "No Evidence" |
    BO$Condition == "Strong Evidence" |
    BO$Condition == "Counter Evidence"
)
BO_belief$Condition <- as.factor(BO_belief$Condition)
BO_belief$Condition <- droplevels(BO_belief$Condition)
BO_belief$Condition <- relevel(BO_belief$Condition, ref = "No Evidence")

############################################################################
# ANALYSIS
############################################################################

# prepare data frame for analysis (dummy-code factors etc.)
xx.fe.re <- fe.re.tab(
  fe.model = "Answer  ~ Condition*Age.3 + Trial",
  re = "(1|ID)", data = BO_belief
)
xx.fe.re$summary # indicates which random effects need to be included
t.data <- xx.fe.re$data
str(t.data)

# center and z-transform variables that are in the random slopes
t.data$c.Condition.Counter.Evidence <- as.numeric(
  t.data$Condition.Counter.Evidence
) -
  mean(as.numeric(t.data$Condition.Counter.Evidence))
t.data$c.Condition.Strong.Evidence <- as.numeric(
  t.data$Condition.Strong.Evidence
) -
  mean(as.numeric(t.data$Condition.Strong.Evidence))
t.data$z.Trial <- scale(t.data$Trial)

contr <-
  glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 10000000)
  )

full <-
  glmer(Answer ~
    Condition * Age.3 +
    (1 + (c.Condition.Counter.Evidence +
      c.Condition.Strong.Evidence) +
      z.Trial || ID),
  data = t.data,
  family = binomial, control = contr
  )

null <-
  glmer(Answer ~ 1 +
    (1 + (c.Condition.Counter.Evidence +
      c.Condition.Strong.Evidence) +
      z.Trial || ID),
  data = t.data,
  family = binomial, control = contr
  )

anova(full, null, test = "Chisq")

# check assumptions
summary(full)$varcor # all random effect identifiable
ranef.diagn.plot(full) # all random effects roughly normally distributed
xx <-
  glmer(Answer ~ Condition + Age.3 +
    (1 + (c.Condition.Counter.Evidence +
      c.Condition.Strong.Evidence) +
      z.Trial || ID),
  data = t.data, family = binomial, control = contr
  )
library(car)
vif(xx) # colliniarity
overdisp.test(full)

# checking model stability
m.stab.b <-
  glmm.model.stab(model.res = full, contr = contr, use = c("ID"))
m.stab.b$detailed$warnings
xx <- as.data.frame(round(m.stab.b$summary[, -1], 3))
dev.off()
m.stab.plot(round(m.stab.b$summary[, -1], 3))

# look at estimates
round(summary(full)$coefficients, 3)

# reduced model
test.1 <- drop1p(
  model.res = full, para = F, data = NULL, contr = contr,
  n.cores = c("all-1", "all")
)
test.1$drop1.res

full.2 <- glmer(Answer ~ Condition + Age.3 +
  (1 + (c.Condition.Counter.Evidence + c.Condition.Strong.Evidence) +
    z.Trial || ID),
data = t.data, family = binomial, control = contr
)

test.2 <- drop1p(
  model.res = full.2, para = F, data = NULL, contr = contr,
  n.cores = c("all-1", "all")
)
round(test.2$drop1.res, 3)

# belief condition comparisons
library("emmeans")
emm <- emmeans(full, ~ Condition)
summary(emm, type = "scale")
summary(emm, type = "response")
summary(pairs(emm), type = "response")

############################################################################
# MAIN GRAPH
############################################################################

library("ggplot2")

BO$Condition <-
  factor(BO$Condition, levels = c(
    "Action Possible",
    "No Evidence",
    "Counter Evidence",
    "Strong Evidence",
    "Action Impossible"
  ))

xx <- ftable(Answer ~ Condition + Age.3, BO)

## frequency table as I need it, with changed signs
yy <- xx[, 1] + xx[, 2]
xx <- xx / yy

freq <- as.data.frame(xx)
freq$new <- NA
freq$new[freq$Answer == 0] <- freq$Freq[freq$Answer == 0] * (-1)
freq$new[freq$Answer == 1] <- freq$Freq[freq$Answer == 1]

freq <- freq %>%
  mutate(Condition = dplyr::recode(Condition,
    "No Evidence" = "No Evidence",
    "Strong Evidence" = "Strong Evidence",
    "Counter Evidence" = "Counterevidence",
    "Action Possible" = "Possible Action",
    "Action Impossible" = "Impossible Action"
  ))

pdf("./Study_1/Plots/Study_1_Main_Plot.pdf", width = 10, height = 6)
p <- freq %>%
  ggplot(aes(x = Age.3, y = new, fill = factor(Answer))) +
  geom_bar(stat = "identity", width = .65) +
  facet_wrap(~Condition, nrow = 1) +
  theme(strip.background = element_blank()) +
  theme(strip.text.x = element_text(size = 12)) +
  xlab("") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(
    "5-6 y/o",
    "7-8 y/o",
    "Adults"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key = element_rect(size = 12),
    legend.key.size = unit(4, "cm"),
    legend.key.height = unit(4, "line"),
    legend.key.width = unit(2, "line"),
    legend.text =
      element_text(
        size = 12,
        colour = "gray30",
        family = "sans"
      ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x.bottom = element_text(
      size = 12,
      colour = "gray30", family = "sans"
    ),
    axis.text.y =
      element_text(
        size = 12,
        colour = "gray30",
        family = "sans"
      ),
    axis.text.x =
      element_text(
        size = 12,
        colour = "gray30",
        family = "sans"
      ),
    axis.ticks.x = element_blank(),
    panel.border =
      element_rect(
        colour = "gray30",
        fill = NA
      ),
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(
    labels =
      c(
        "Belief/action is uncontrollable",
        "Belief/action is controllable"
      ),
    values = c("darkgreen", "goldenrod1"),
    guide = guide_legend(reverse = TRUE)
  )
p
dev.off()
############################################################################
# PLOT WITH ESTIMATES AND CONFIDENCE INTERVALS
############################################################################

xx <- summary(emm, type = "response")

pdf("./Study_1/Plots/Study_1_Estimates_and_CIs1.pdf", width = 8, height = 6)
p <- ggplot(
  xx,
  aes(
    x = interaction(Age.3),
    y = prob
  )
) +
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL),
    size = 1
  ) +
  facet_wrap(~Condition, nrow = 1) +
  xlab("") +
  ylab(
    "predicted probability that\nparticipants judge belief possible"
  ) +
  scale_x_discrete(labels = c(
    "5-6 y/o",
    "7-8 y/o",
    "Adults"
  )) +
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

save.image("./Study_1/Data/Possible_Beliefs_Study_1_Belief_Analysis.RData")
