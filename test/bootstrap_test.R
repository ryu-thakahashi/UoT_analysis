set.seed(20)
sam = rnorm(20,170,6)
round(sam[1:5],2)

tt = numeric(0)
ME = mean(sam);SD = sd(sam)
for(i in 1:2000){z = rnorm(20,0,1);bx = ME+z*SD;tt=cbind(tt,mean(bx))}

mean(tt)
quantile(tt,p=c(0.025,0.975))

library(simpleboot)
b.mean = one.boot(sam,mean,10)
b.mean$t

library(boot)
b.mean = one.boot(sam,mean,2000)
boot.ci(b.mean)

library(lavaan)
glimpse(df)
path_model  = 
"
sincerity ~ cost.num
forgive ~ sincerity + cost.num
"
CSF.fit = sem(path_model,data=df)
summary(CSF.fit,standardized = TRUE, rsquare = TRUE)
fitmeasures(CSF.fit)

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

fit_med2 = sem(md_model,data=df,
              se = "bootstrap",
              bootstrap = 2000)
summary(fit_med2,standardized = TRUE,
        fit.measures = TRUE, ci = TRUE)
fitMeasures(fit_med2)
