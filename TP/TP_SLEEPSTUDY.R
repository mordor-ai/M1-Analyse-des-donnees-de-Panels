#import du package lme4
library(lme4)
require(lattice)
## description du dataset
## on remarque 3 variables 
str(sleepstudy)
## affichage des données
sleepstudy
## représente 180 observations de sujets privés de sommeil ( 3 heurs seulement=> on observe les temps de réaction)
? sleepstudy


# 
interaction.plot(Days,Subject,Reaction, col=1:18)




k=length(modele$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(modele$residuals**2)
n=length(modele$residuals)

RSE = sqrt(SSE/(n-(1+k))) #Residual Standard Error

VarBols = (RSE**2)*solve(t(X)%*%X)
StandardError = sqrt(VarBols[1][1])


# regression linéaire simple en focntion du jou

reg=lm(Reaction ~ Days,data = sleepstudy)
summ <- summary(reg)
print (summ)
reg$coefficients
X <- model.matrix(~Days)
print(x)

#
# βOLS = moindres carrés ordinaires :  (X'X)-1X'Y
# dans ce cas ci βOLS=βGLS
beta_ols = solve(t(X)%*%X)%*%t(X)%*%Y
print(beta_ols)
## on b
# t value= beta0 / error 
# 
variance_beta_ols = solve(t(x)%*%x)%*%t(x)
print(variance_beta_ols)
## on rajoute l'effet aléatoire sujet
#model 1 : beta0+beta1*xik + ui+epsilonik
res <-lme4::lmer(Reaction~Days + (1|Subject),sleepstudy,REML = FALSE)
summary(res)
# Modele 2 : beta0+beta1*xik + uixik +epsilonik
# beta
res <-lme4::lmer(Reaction~Days + (Days|Subject),sleepstudy,REML = FALSE)
res <-lme4::lmer(Reaction~Days + (Days-1|Subject),sleepstudy,REML = FALSE)
summary(res)
res <-lme4::lmer(Reaction~Days + (0+Days|Subject),sleepstudy,REML = FALSE)
summary(res)
#modele 3 : beta0+vi+beta1*xik + uixik +epsilonik
# avec ui et vi indépendants

res <-lme4::lmer(Reaction~Days + (1+Days|| Subject),sleepstudy,REML = FALSE)
summary(res)

# modele4beta0+vi+beta1*xik + uixik +epsilonik
# avec ui et vi dépendants
#
res <-lme4::lmer(Reaction~Days + (1+Days|Subject),sleepstudy,REML = FALSE)
summary(res)



# on ajoute 







# from doc
xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)", aspect = "xy")
(fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
(fm2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy))

