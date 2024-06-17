#----------------------------#
# Cox regression models in R #
#----------------------------#

# Load necessary libraries
library(survival)
library(tidyverse)
library(survminer)

# load dataset
data(veteran)
head(veteran)

# fit the Cox proportional hazards model
cox_model = coxph(Surv(time, status) ~ age + celltype + trt + karno, data=veteran)
summary(cox_model)

# Create a survival plot
ggsurvplot(survfit(cox_model), data = veteran,
                     conf.int = TRUE,
                     pval = TRUE,
                     risk.table = TRUE) +
  labs(title = 'Cox Regression analysis',
       subtitle = 'Veteran data',
       y="Survival probability", x="Time")

#-----#
# end #
#-----#