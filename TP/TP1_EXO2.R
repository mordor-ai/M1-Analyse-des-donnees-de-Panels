# Modèle d’ANCOVA

#yij=β0+xijβ1+aj+εij j=1,...10,i=1,...Nj
# - Simuler l’appartenance de chaque donnée à l’un des 10 niveaux du facteur fac.

# niveaux 
j=seq(1,10)
print(j)
# nombre d'observations dans le niveau j
n = 100
fac  =  factor(sample(j,n,replace= TRUE ))
print(fac)

#- Simuler l’effet aj de chacun des niveaux selon une loi gaussienne de variance 3.
a= runif(j,0,3)
print(a)

#- Simuler les yij selon le modèle décrit.

# todo génererBeta0 et beta1 
## β0,β1 ∼Unif[−3;3]
beta0 <- runif(1,-3,3)
beta1 <- runif(1,-3,3)
print( beta0)
sd_epsilon =  2
## epsilon
mean_epsilon=  0
epsilon  <- rnorm (n,mean = mean_epsilon,sd = sd_epsilon)
## les valeurs du regresseur x entre 0 et 1
#xi ∼Unif[0;1]
xi= runif(n,0,1)


#yij=β0+xijβ1+aj+εij j=1,...10,i=1,...Nj

Y = beta0 + beta1*x +a[fac] +epsilon

print(Y)

#Construire la matrice X de design des effets fixes.

X_intercept <- model.matrix(~xi+fac)
X_ss_intercept <- model.matrix(~xi+fac-1)

#X <- X_intercept
print(X_intercept)
print(X_ss_intercept)

X<-X_intercept
print(X)

#Calculer les estimations de β, de a = (a1, ..., a10)′ et de la variance résiduelle.


# βOLS = moindres carrés ordinaires :  (X'X)-1X'Y
# dans ce cas ci βOLS=βGLS
beta_ols = solve(t(X)%*%X)%*%t(X)%*%Y
print(beta_ols)

#BETA <-cbind(beta0,beta1)
p =  length(beta_ols)
sigmacarrechapeau_ols = t(Y-X%*%beta_ols)%*%(Y-X%*%beta_ols)/(n-p)
print(sigmacarrechapeau_ols)

#Obtenir la valeur de la matrice de variance-covariance de l’estimateur β.

variance_beta_chapeau_ols <- sigmacarrechapeau_ols[1,1] * solve( (t(X) %*% X) )
print (variance_beta_chapeau_ols)

ancova =lm(Y ~ xi +fac)
summ <- summary(ancova)
# on compare les sigma
print(sigmacarrechapeau_ols)
print(summ$sigma*summ$sigma)
# on compare les matrices variance-covariance
variance_beta_chapeau_ols
# instruction variance covariance :  vcov
vcov(ancova)
