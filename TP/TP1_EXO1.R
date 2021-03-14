# Modèle de régression linéaire
#i =  1 à  100
n=  10

# todo génererBeta0 et beta1 
#β0,β1 ∼Unif[−3;3]
beta0 <- runif(1,-3,3)
beta1 <- runif(1,-3,3)

# todo générere les valeurs du regresseur x entre 0 et 1
#xi ∼Unif[0;1]
#runif(n, min=a, max=b)
xi= runif(n,0,1)

# simuler epsilon selon une loi gaussienne d'ecart type 2
# simule epsilon avec la loi centrée réduite
# rnorm(n,mean=μ,sd=σ)
#εi ∼N(0,2)
sd_epsilon =  2
mean_epsilon=  0
epsilon  <- rnorm (n,mean = mean_epsilon,sd = sd_epsilon)


#yi =β0 +xiβ1 +εi
Y = beta0 + beta1*xi + epsilon
print(Y)
# creation de la matrice identité
repetition=  rep(x = 1,n)
print(repetition)
Identite <- diag(rep(1,n))
print(Identite)
# Que vaut V (matrice de variance-covariance de Y dans ce modèle) ? V −1 ?
V = sd_epsilon*sd_epsilon*Identite

# attention R n'arrive pas à  calculer au delà  de 10000
#V_moins_1 =solve(V)
V_moins_1 = 1/(sd_epsilon*sd_epsilon)*Identite
print(V_moins_1)
#print(V_moins_1_alt)

# matrice X
X <- cbind(rep(1,n),xi)
print(X)

#Donner l’expression de βOLS et βGLS. Calculer leurs valeurs numériquement dans R.
print(beta0)
print(beta1)
# βOLS = moindres carrés ordinaires :  (X'X)-1X'Y
beta_ols = solve(t(X)%*%X)%*%t(X)%*%Y
print(beta_ols)
#  βGLS (X'Vexp(-1)X)exp(-1)X'Vexp(-1)Y
beta_gls = solve(t(X)%*%V_moins_1%*%X)%*%t(X)%*%V_moins_1%*%Y
print(beta_gls)    

#BETA <-cbind(beta0,beta1)
p =  length(beta_ols)
sigmacarrechapeau_ols = t(Y-X%*%beta_ols)%*%(Y-X%*%beta_ols)/(n-p)
print(sigmacarrechapeau_ols[1,1])

p =  length(beta_gls)
sigmacarrechapeau_gls = t(Y-X%*%beta_gls)%*%(Y-X%*%beta_gls)/(n-p)
print(sigmacarrechapeau_gls)

#Obtenir la valeur de la matrice de variance-covariance de l’estimateur β.

variance_beta_chapeau_ols = sigmacarrechapeau_ols[1,1]*solve( (t(X) %*% X) )
print (variance_beta_chapeau_ols)

variance_beta_chapeau_gls <- solve(t(X)%*%V_moins_1%*%X)
print (variance_beta_chapeau_gls)
#À l’aide de la fonction lm de R, retrouver ces informations.
# on compare les beta
print(beta_gls)  
reg=lm(Y ~ xi)
summ <- summary(reg)
# on compare les sigma
print(sigmacarrechapeau_gls)
print(summ$sigma*summ$sigma)
# on compare les matrices variance-covariance
variance_beta_chapeau_gls
variance_beta_chapeau_ols
# instruction variance covariance :  vcov
vcov(reg)



