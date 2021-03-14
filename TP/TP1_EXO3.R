# modele d'anova à  effet aléatoire
# yik =β+ai +εik i=1,...,10etk=1,...,10
i= 10
k =  10
beta0 <- runif(1,-3,3)
## epsilon simulation du résidu
sd_epsilon =  1
mean_epsilon=  0
epsilon  <- rnorm (n,mean = mean_epsilon,sd = sd_epsilon)

# creation de la matrice identité
repetition=  rep(x = 1,i)
print(repetition)
Identite <- diag(rep(1,i))
print(Identite)




#- Simulerles ai selonuneloiN(0,σa2) où σa2 =9
sd_ai =  3
mean_ai = 0
ai <-rnorm(k,mean_ai,sd_ai)
print(ai)
#- Simuler les yik avec une variance résiduelle σ02 = 1

# option 1:  en créant le vecteur fac 
n = 100
fac  =  factor(rep(1:10,each=10))
print(fac)

Y_vect = beta0 + ai[fac] +epsilon

# Option 2 :  de façon matricielle
# on utilise kroneker
vect_un <- cbind(rep(1, k))

print(vect_un)
Z = kronecker(Identite,vect_un )
print(Z)
Y_mat= beta0 + Z%*%ai + epsilon
print(Y_mat)

# Que vaut V (matrice de variance-covariance de Y dans ce modèle) ? V −1 ?
V = sd_epsilon*sd_epsilon*diag(rep(1,100)) + sd_ai*sd_ai*Z%*%t(Z)

print (V)
V_moins_1 =solve(V)
print(V_moins_1)

#Construire la matrice X de design des effets fixes.

X <- model.matrix(~fac)
print(X)

print(Y_vect)

print(as.vector(Y_mat))

beta_gls = solve(t(X)%*%V_moins_1%*%X)%*%t(X)%*%V_moins_1%*%Y_mat
print(beta_gls) 

beta_ols = solve(t(X)%*%X)%*%t(X)%*%Y_mat
print(beta_ols)
# ce sont les mêmes valeurs => théorème Kruskal

# simulation lme4

anova_lme4 <- lme4::lmer(Y_vect~1+(1|fac),REML = FALSE)
summary(anova_lme4)
# maximum de vrauisemblance restreint
anova_lme4 <- lme4::lmer(Y_vect~1+(1|fac),REML = TRUE)
summary(anova_lme4)


summ <- summary(anova_lme4)
# on compare les sigma
print(sigmacarrechapeau_ols)
print(summ$sigma*summ$sigma)
# simulation nlme


anova_nlme <- nlme::lme(Y_vect~1, random = ~1|fac, method =  "ML" )
summary(anova_nlme)


anova_nlme <- nlme::lme(Y_vect~1, random = ~1|fac, method =  "REML" )
summary(anova_nlme)


# todo simuler un vecteur de dimension 1
vecteur_extrait<- rbinom(i*k,1,0.7)
print(vecteur_extrait )
# ensuite extraire aléatoirement les lignes de Y , 
Y_extrait=  Y_vect[which(vecteur_extrait==1)]
fac_extrait= fac[which(vecteur_extrait==1)]
epsilon_extrait =  epsilon[which(vecteur_extrait==1)]

print( fac)
dim(Y_extrait)


Z_extrait = Z[which(vecteur_extrait==1),]
print(Z_extrait)
dimension_extraite = dim(Z_extrait)[1]
Identite_extrait=  diag(rep(1,dimension_extraite))

print(Identite_extrait)
V_extrait = sd_epsilon*sd_epsilon*Identite_extrait + sd_ai*sd_ai*Z_extrait%*%t(Z_extrait)

print (V_extrait)
print(V)
V_extrait_moins_1 =solve(V_extrait)
print(V_extrait_moins_1)

#Construire la matrice X de design des effets fixes.

X_extrait <- model.matrix(~fac_extrait)
print(X_extrait)


#beta_gls = solve(t(X)%*%V_moins_1%*%X)%*%t(X)%*%V_moins_1%*%Y_mat
beta_gls_extrait = solve(t(X_extrait)%*%V_extrait_moins_1%*%X_extrait)%*%t(X_extrait)%*%V_extrait_moins_1%*%Y_extrait
print(beta_gls_extrait) 

beta_ols_extrait = solve(t(X_extrait)%*%X_extrait)%*%t(X_extrait)%*%Y_extrait
print(beta_ols_extrait)
# ce sont les mêmes valeurs => théorème Kruskal

# simulation lme4

anova_lme4_extrait <- lme4::lmer(Y_extrait~1+(1|fac_extrait),REML = FALSE)
summary(anova_lme4_extrait)
# maximum de vrauisemblance restreint
anova_lme4_extrait <- lme4::lmer(Y_extrait~1+(1|fac_extrait),REML = TRUE)
summary(anova_lme4_extrait)


summ <- summary(anova_lme4)
# on compare les sigma
print(sigmacarrechapeau_ols)
print(summ$sigma*summ$sigma)
# simulation nlme


anova_nlme <- nlme::lme(Y_vect~1, random = ~1|fac, method =  "ML" )
summary(anova_nlme)


anova_nlme <- nlme::lme(Y_vect~1, random = ~1|fac, method =  "REML" )
summary(anova_nlme)


