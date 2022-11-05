require(binom)

0.769

1.96

(1+2)/(60+4)

# Use this for the Agresti-Coull method of confidence interval

# Left
0.769-(1.96*sqrt((0.769*(1-0.769))/(169+4)))

#Right
0.769+(1.96*sqrt((0.769*(1-0.769))/(169+4)))

# Left

0.046875-(1.96*sqrt((0.046875*(1-0.046875))/(169+4)))

# Right

0.046875+(1.96*sqrt((0.046875*(1-0.046875))/(169+4)))

# Use binomfit

# standard error

sqrt((1/60)*(1-(1/60))/60)

1/60

binom.confint(x = 1, n = 60, conf.level = 0.95, method = "all")
