# TP fournées
# i:  temperatures
i=3
#j:  fournée
j=6
#k: pain
k=4

# 6 fournées
# 4 pains


# vecteurs
## Temperatures cl : veut dire classe

T_cl  <- as.factor(rep(1:i,each=8))
print(T_cl)
## Fournées 
F_cl <- as.factor(rep(1:j,each=4))
print(F_cl)

# 3 temperatures
TEMPERATURES <- c(180,150,130)



# creation du vecteur BETA
beta1 <- runif(1,-3,3)
beta2 <- runif(1,-3,3)
beta3 <- runif(1,-3,3)
BETA <-c(beta1,beta2,beta3)
print (BETA)

# simulation de U
sd_u =  3
mean_u = 0
U <-rnorm(j,mean_u,sd_u)
print(U)

# simulation de Epsilon
sd_epsilon =  1
mean_epsilon=  0
EPSILON  <- rnorm (j*k,mean = mean_epsilon,sd = sd_epsilon)
print(EPSILON)
# Objectif :  obtenir : 
# Y= X*BETA+ Z*U + EPSILON


# creation de X :  matrice de design des effets fixe
X <- cbind(rep(1,j*k),c(rep(0,8),rep(1,8),rep(0,8)),c(rep(0,16),rep(1,8)))
print(X)

X=model.matrix(~T_cl)
## option 1:  en créant le vecteur fac 


# Option 2 :  de façon matricielle
## on utilise kroneker

## creation de la matrice identité
repetition=  rep(x = 1,j)
print(repetition)
Identite <- diag(repetition)
print(Identite)
## vecteur des 1 
vect_un <- cbind(rep(1, k))

print(vect_un)
## creation de la matrice Z
Z = kronecker(Identite,vect_un )
print(Z)

#Z=model.matrix(~F_cl-1)
Y= X%*%BETA + Z%*%U + EPSILON
print(Y)


# Que vaut V (matrice de variance-covariance de Y dans ce modèle) ? V −1 ?
V = sd_epsilon*sd_epsilon*diag(rep(1,j*k)) + sd_ai*sd_ai*Z%*%t(Z)
print (V)
V_moins_1 =solve(V)
print(V_moins_1)



beta_gls = solve(t(X)%*%V_moins_1%*%X)%*%t(X)%*%V_moins_1%*%Y
print(beta_gls) 

beta_ols = solve(t(X)%*%X)%*%t(X)%*%Y
print(beta_ols)


anova_lme4 <- lme4::lmer(Y~T_cl,REML = FALSE)
summary(anova_lme4)
# maximum de vrauisemblance restreint
anova_lme4 <- lme4::lmer(Y~T_cl+(1|F_cl),REML = TRUE)
summary(anova_lme4)

print(T_cl)
# ajout d'une pente sur la temperature
T <-rnorm(j*k,0,2)

res <- lme4::lmer(Y~T_cl+(T|F_cl),REML = TRUE)
summary(res)
# avec effet de l'intercept
res<- lme4::lmer(Y~T_cl+(1+T|F_cl),REML = TRUE)
summary(res)
# yijk = BETA+ GAMMA + DELTA + EPSILON

# suppression de la correlation entre les niveaux d'effets aleatoires fournée sur l'intercpt
#et fouenré sur la pente 

res <-lme4::lmer(Y~T_cl + (1|F_cl)+ (T-1|F_cl))
summary(res)
res <-lme4::lmer(Y~T_cl + (1|F_cl)+ (0+T|F_cl))
summary(res)

res <-lme4::lmer(Y~T_cl + (1+T||F_cl))
summary(res)



