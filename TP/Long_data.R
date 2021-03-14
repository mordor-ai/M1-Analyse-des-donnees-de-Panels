#import des données de log_data
setwd("/Users/epellegrin/Desktop/data-science/MIASHS/Analyse des données de Panels/TP")
long.data = read.csv2("long_data.csv",sep = ";")
#
dim(long.data)
head(long.data)
library(ggplot2)
ggplot(long.data, aes(time,bn))+geom_point()
ggplot(long.data, aes(time,bn, group=palmId))+geom_point()+geom_line()
ggplot(long.data, aes(time,bn, group=palmId))+geom_point()+geom_line()+geom_smooth(method ="lm", se= FALSE)
## affiche l'évolution par individu du nombre de bananes dans le temps
## bn :  nombre de régimes en fonction du temps
ggplot(long.data, aes(time,bn))+geom_point()+geom_line()+ facet_wrap(~palmId)
## régression linéaire simple  du nombre de régimes de banane
# y=  beta1+ beta2xi + epsilonij
mod <-lm(bn ~ time, data = long.data)
res <- resid(mod)
summary(mod)
plot(mod)
ggplot(long.data, aes(palmId,res) )+geom_point()
# on réordoen en fonction des résidus
ggplot(long.data, aes(reorder(palmId,res),res) )+geom_point()+geom_hline(yintercept = 0,colour=2)

## on remarque que certains on des résidus tout le temps négatifs et d'autres sont toujours positifs

# l'intercept : 34 ( 34  régimes par an )
# time =  -4 une baisse de 4 régimes par ans


## on itroduit un effet aléatoire au niveau de l'intercept
# modele yij = beat1 + beta2xi + bj + epsilonij

library(lme4)
mod1 <- lmer(bn ~ time + (1|palmId) , data = long.data)

summary(mod1)
summary(mod1)$varcor # variances associées aux effest alétoires 
VarCorr(mod1)# var
fixef(mod1) # extraction des beta1 et beta2
ranef(mod1) # extartion des bj 

#sigmacarrechapeau de 
#Groups   Name        Variance Std.Dev.
#palmId   (Intercept)  2.08    1.442   
#Residual             16.33    4.042   

 ## autre modèle
# modele yij = beta1 + beta2xi + bj + ajxi+ epsilonij
# i= 0..4
# j= 1..72


mod2 <- lmer(bn ~ time + (0+time | palmId) , data = long.data)

summary(mod2)
summary(mod2)$varcor # variances associées aux effest alétoires 
VarCorr(mod2)# variance 
fixef(mod2) # extraction des beta1 et beta2
ranef(mod2) # extartion des bj 



## dans lmer :  time et 1+time sont équivallents

mod3 <- lmer(bn ~ time + (1+time | palmId) , data = long.data)
mod3 <- lmer(bn ~ time + (time | palmId) , data = long.data)

# modele yij = beta1 + beta2xi + bj + ajxi+ epsilonij
summary(mod3)
summary(mod3)$varcor # variances associées aux effest alétoires 
VarCorr(mod3)# variance 
# on voit qu'il ya une covariance entre l'intercept et la pente
fixef(mod3) # extraction des beta1 et beta2
ranef(mod3) # extartion des bj 


# modele avec intercept et pente aléatoire indépencats

mod4 <- lmer(bn ~ time + (1 | palmId) + (0+time | palmId), data = long.data)
## ecriture équivallente
mod4 <- lmer(bn ~ time +  (0+time || palmId), data = long.data)

# modele yij = beta1 + beta2xi + bj + ajxi+ epsilonij
summary(mod4)
summary(mod4)$varcor # variances associées aux effest alétoires 
VarCorr(mod4)# variance 
# on voit qu'il ya une covariance entre l'intercept et la pente
fixef(mod4) # extraction des beta1 et beta2
ranef(mod4) # extartion des bj 

## comparaison des modèles
res <- anova(mod3, mod4) # refitting model with ML
print(res)
# pvalue : 0.9023 => on garde le modèle le plus simple ( mod4)
## test pente aléatoire  /intercept aléatoire

res <- anova(mod4, mod2) # refitting model with ML
print(res)
# pvalue: 0.4085 => on garde le modèle le plus simple

res <- anova(mod4, mod1) # refitting model with ML
print(res)
# pvalue <0.05 => on garde le modèle le plus complet

## esperance conditionnelle  ( intercept aléatoire) / marginale ( pente aléatoire)
y1 <- predict(mod1)
head(y1)
## esperance marginale
y1b <- predict(mod1, re.form=NA)
head(y1b)




y2 <- predict(mod2)
head(y2)
## esperance marginale
# ajouter  re.form = NA pour
y2b <- predict(mod2, re.form=NA)
head(y2b)
y3 <- predict(mod3)
head(y3)
## esperance marginale
y3b <- predict(mod3, re.form=NA)
head(y3b)


long.data$y1 <- y1
long.data$y2 <- y2
long.data$y3 <- y3
long.data$y1b <- y1b

ggplot(sub,aes(time,y1, group =  palmId)) + geom_line(colour =  "red")

ggplot(sub,aes(time,y1b, group =  palmId)) + geom_line(colour =  "black")


ggplot(sub,aes(time,y2, group =  palmId)) + geom_line(colour =  "blue")

ggplot(sub,aes(time,y2, group =  palmId)) + geom_line(colour =  "forestgreen")

library(gridExtra)





sub <- subset(long.data, palmId %in% c ("57_11", "37_5", "56_13", "34_7", "54_13","37_6","37_23"))
sub <- droplevels(sub)
##
p1 <- ggplot(sub, aes(time,y1, group= palmId))+ geom_line(colour="red")
p2 <- ggplot(sub, aes(time,y2, group= palmId))+ geom_line( colour="blue")
p3 <- ggplot(sub, aes(time,y3, group= palmId))+ geom_line(colour="forestgreen")
# la même prédictuon pour tous les individus
p4 <- ggplot(sub, aes(time,y1b, group= palmId))+ geom_line(colour="black")
grid.arrange(p1,p2,p3,p4)







