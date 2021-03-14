library(lme4)
#import des données de log_data
setwd("/Users/epellegrin/Desktop/data-science/MIASHS/Analyse des données de Panels/TP")
lattice.data = read.csv2("lattice.design.csv",sep = ";")
dim(lattice.data)
head(lattice.data)
library(ggplot2)
## affichage des rendements en huile de chaque famille
ggplot(lattice.data, aes(replicate,yield))+geom_point()+geom_line()+ facet_wrap(~family)

ggplot(lattice.data , aes(replicate , yield)) + geom_boxplot(aes(fill = replicate)) + facet_wrap(~family)



#  Rendement en huile en fonction de la famille et de la r ́ep ́etition
#   Plot de l’interaction replicate x family
# on remarque que les répéetitions ont une interaction sur les familles  
# s'il n'y avait pas d'inteacrion, les courbes seraient parallèles
# donc on doit intégrer les répetions dans le modèle
attach( lattice.data )
interaction.plot(family, replicate, yield, las = 1) 
detach()



# on ignore l'effet bloc anova en focntion de lafamille
mod <- lm(yield~family, data = lattice.data )
#  famille est un facteur donc on fait toujours une anova 
anova(mod)
## on affiche les résidus
plot(lattice.data$replicate, resid(mod))
res <- resid(mod)
ggplot(lattice.data,aes(reorder(family, res), res))+geom_point() +geom_hline(yintercept=0,colour=2)



## MODELE 1
# Yijk =μ+βi +bj +εijk,    i=1,...,25,j=1,...6,k=1,...12
# avec bj ∼N(0,σb2),εijk ∼N(0,σ2)



# intéraction replicate / family
# on rajoute un effet aléatoire sur la répétition
mod1 <- lmer(yield~family +(1|replicate), data = lattice.data)
summary(mod1)
summary(mod1)$varcor # variances associées aux effest alétoires 
VarCorr(mod1)# var
fixef(mod1) # extraction des beta1 et beta2
#on remarque que c'est la fmille f18 qui est la plus productive = 2.10478108'
ranef(mod1) # extartion des bj 
# on remarque que c'est la répétion R3 qui influe leplus négativement sur le rendement
#R3 -0.57255984
plot(mo1)
# par défut on la famille F1 est chosi comme famille de réfernce
options()$contrasts
contrasts(lattice.data$family)
mod1 <- lmer(yield~family +(1|replicate), data = lattice.data)
summary(mod1)
fixef(mod1)

plot(lattice.data$replicate, resid(mod1))



##  MODELE 2

## interaction emboitée
# Yijk =μ+βi +bj +bij +εijk    i=1,...,25,j=1,...6,k=1,...12 
# avec bj ∼N(0,σ12),bij ∼N(0,σ2),εijk ∼N(0,σ2)
mod2 <- lmer(yield ~ family + (1|family/replicate ) , data = lattice.data)

summary(mod2)
summary(mod2)$varcor # variances associées aux effest alétoires 
VarCorr(mod2)# variance 
fixef(mod2) # extraction des beta1 et beta2
ranef(mod2) # extartion des bj 
plot(lattice.data$replicate, resid(mod2))

## choix du modèle
anova(mod2,mod1)


## on ajoute l'effet block

mod3 <- lmer(yield ~ family + (1|replicate/block/family), data = lattice.data)
summary(mod3)

anova(mod3)

# doit on le prendre en compte ?
anova(mod3,mod2)

## ezst-ce qu'il y a un effet famille ?
library(lmerTest)
mod2 <- lmer(yield ~ family + (1|replicate/family), data = ldesign)
summary(mod2)
anova(mod2)




