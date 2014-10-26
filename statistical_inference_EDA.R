library(datasets)
library(ggplot2)

# Plot Tooth length vs. Dosage (mg) split by Supplement Type (OJ or VC)
ggplot(data=ToothGrowth, aes(x=as.factor(dose), y=len, fill=supp)) +
     geom_bar(stat="identity",) + facet_grid(. ~ supp) + xlab("Dosage (milligram)") +
     ylab("Tooth length") + guides(fill=guide_legend(title="Supplement type"))

# Validate the correlation results with a linear model
fit <- lm(len ~ dose + supp, data = ToothGrowth)

# Summary of Results of the linear model
summary(fit)

# Confidence Interval of variables in the linear model
confint(fit)