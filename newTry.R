library(MASS)

head(shuttle)
shuttle2<-shuttle
shuttle2$use2<-as.numeric(shuttle2$use=='aut
o')
fit<-glm(use2 ~ factor(wind) - 1, family = binomial, data = shuttle2)
fit
fit<-glm(use2 ~ factor(wind) - 1, family = binomial, data = shuttle2)
shuttle2<-shuttle
shuttle2$use2<-as.numeric(shuttle2$use=='auto')
fit<-glm(use2 ~ factor(wind) - 1, family = binomial, data = shuttle2)
summary(fit)$coef
exp(coef(fit))
1.286 / 1.327
# ou
exp(coef(fit))[1] / exp(coef(fit))[2]
exp(cbind(OddsRatio = coef(fit), confint(fit)))

## Consider the previous problem. Give the estimated odds ratio for autoloader use comparing head winds (numerator) to tail winds (denominator) adjusting for wind strength from the variable mag

fit<-glm(use2 ~ factor(wind) + factor(magn) - 1, family = binomial, data = shuttle2)
summary(fit)$coef
exp(coef(fit))
exp(cbind(OddsRatio = coef(fit), confint(fit)))
exp(cbind(OddsRatio = coef(fit), confint(fit)))[1] / exp(cbind(OddsRatio = coef(fit), confint(fit)))[2]

