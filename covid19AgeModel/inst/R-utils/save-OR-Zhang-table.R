library(data.table)
library(lognorm)

indir = "~/git/R0t"

# 95% CI of OR from https://science.sciencemag.org/content/sci/early/2020/05/04/science.abb8001.full.pdf
CI_OR = data.table(age = c("0-14", "65+"),
                   lower.bound = c(0.24, 1.12),
                   upper.bound = c(0.49, 1.92)) %>%
  mutate(mu = NA_real_, sigma = NA_real_)

CI_OR$mu[1] = as.numeric(getParmsLognormForLowerAndUpper(lower = CI_OR$lower.bound[1], upper = CI_OR$upper.bound[1], sigmaFac = qnorm(0.95))[1])
CI_OR$mu[2] = as.numeric(getParmsLognormForLowerAndUpper(lower = CI_OR$lower.bound[2], upper = CI_OR$upper.bound[2], sigmaFac = qnorm(0.95))[1])

CI_OR$sigma[1] = as.numeric(getParmsLognormForLowerAndUpper(lower = CI_OR$lower.bound[1], upper = CI_OR$upper.bound[1], sigmaFac = qnorm(0.95))[2])
CI_OR$sigma[2] = as.numeric(getParmsLognormForLowerAndUpper(lower = CI_OR$lower.bound[2], upper = CI_OR$upper.bound[2], sigmaFac = qnorm(0.95))[2])

write.csv(CI_OR, file = file.path(indir, "usa", "data", "CI_OR_age_Zhang.csv"), row.names = F)
