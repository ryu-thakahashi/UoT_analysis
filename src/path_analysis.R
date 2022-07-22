library(lavaan)
md_model = "
# direct(label:c)
forgive ~ c*cost.num

# mediator(label:a,b)
sincerity ~ a*cost.num
forgive ~ b*sincerity

# indirect effect(a*b)
ab := a*b

# total effect
total := c + (a*b)
"
fit_med = sem(md_model,data=df,
              estimator = "ML",
              se = "bootstrap",
              bootstrap = 2000)
summary(fit_med,standardized = TRUE,
        fit.measures = TRUE, ci = TRUE)
fitMeasures(fit_med)