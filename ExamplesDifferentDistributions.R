 
 

#### Options for links and distributions:
#Distribution: gaussian, links= identity, log, sqrt
#Distribution: binomial, links= logit, lot, probit
#Distribution: poisson, links= log, sqrt
#Distribution: Gamma, links= inverse2
#
#

#R CMD INSTALL CMLbeta_0.1.0.tar.gz
library(CMLbeta)

#######################################
############## NORMAL MODEL
######################################


set.seed(1093183)

N <- 5000
n<-500
Za <- abs(cbind(1, rnorm(N), rnorm(N), rbinom(N, 1, 0.3)))
Xa <- cbind(1, Za[,c(  4)])

 
## REsponse variables 
bet <-  c(1.5,1,1,1)   
YaNormal <- rnorm(N,  ( Za%*%bet), 1);  mean(YaNormal)




########################################
########################################
### Sample and models for Normal variable 
########################################
########################################
Ya    <- YaNormal
bet.PI <-  c(-1,-0.5)  
pi0a  <-   1/(1+exp(-cbind(1,Ya)%*%bet.PI  )); sum(pi0a)  ;    
R     <-   R.PI   <-   rbinom(N, 1, pi0a ) ; sum(R)
pi0M  <-   glm(R~   Ya , family=binomial )  

   
X.PI.TRUE   <- cbind( 1, Ya ) 
X.PI.TRUENoy   <- cbind( Xa[,1]  ) 
colnames(X.PI.TRUE) <- c('Int', 'Ya')
colnames(X.PI.TRUENoy) <- c('Int' )
bet.PITRUE  <- pi0M$coefficients
## Formula for  the sampling weights :
fX.PI0      <- formula(~0+   Int+   Ya  )
## Fomula  and covariates for the calibrated/estimated weights:
X.PINoy   <-     cbind( Xa  ) 
colnames(X.PINoy)  <- c('Int', 'x2' )
fX.PI  <- formula(~0+ Int+x2*Ya  )

## Epsilon for estimation process:
eps=1E-10
ids=ids.PI=1:length(R.PI)
## Distribution and link for ESTIMATED WEIGHTS
dist.PI='binomial'; link.PI='logit'

## Distribution and link for OUTCOME Ya
dist= 'gaussian' ; link='identity'
ids.PI=1:length(Ya)
simulta=FALSE
bet.PI=  bet.PITRUE
 
 

modSc_AlpEst  <-  CMLest(    ids=1:length(Ya), Ya,   pi0a,bet = bet,   Za, dist=dist, link=link,   
                        R.PI,  X.PINoy,  dist.PI=dist.PI, link.PI=link.PI , eps=1E-10 ,  bet.PI =NULL,   fX.PI =  fX.PI ) 
  


bet.PIy   <- bet.PITRUE[2]  
bet.PIx   <- bet.PITRUE[1]


pFUN0   <-  function(x.pi, yi,  ...){
  x <-  cbind(rbind(x.pi)  , yi)
  b <- c(bet.PIx, bet.PIy )
  #x <-  cbind(rbind(x.pi)   )
  #b <- c(bet.PIx  )
  pii0 <-  1/(1+exp( -x%*%b   ) ) 
  pii0}

# This fucntion allows interaction in the weights
 
bet   <- bet
distance<- 'poisson'
modSc_AlpCal <-  CMLcal(  ids=1:length(Ya),  Ya, pi0a, bet =bet,  Za, dist=dist, link=link,
                      R.PI,  X.PINoy , distance=distance,    eps=1E-10,    bet.PI =NULL,     fX.PI=formula(~ 1),   pFUN0 ,X.PI.TRUENoy) 
 
## Conditional Max. Lik. calibration
modSc_AlpCal$coeffCL 

# Weighted Lik. calibration
modSc_AlpCal$coeffWL
 

## Conditional Max. Lik. estimated weg
modSc_AlpEst$coeffCL 

# Weighted Lik.  estimated weg
modSc_AlpEst$coeffWL

 
########################################
########################################
### Sample and models for Binary variable  - log link
########################################
########################################
bet <- - c(1,1,1,1) /2  
YaBinRR <- rbinom(N, 1,  exp(Za%*%bet));   mean(YaBinRR)
 


Ya     <- YaBinRR
bet.PI <-  c(-2.5,-1)  
pi0a  <-   1/(1+exp(-cbind(1,Ya)%*%bet.PI  )); sum(pi0a)  ;    
R     <-   R.PI   <-   rbinom(N, 1, pi0a ) ; sum(R)
pi0M  <-   glm(R~   Ya , family=binomial )  


X.PI.TRUE   <- cbind( 1, Ya ) 
X.PI.TRUENoy   <- cbind( Xa[,1]  ) 
colnames(X.PI.TRUE) <- c('Int', 'Ya')
colnames(X.PI.TRUENoy) <- c('Int' )
bet.PITRUE  <- pi0M$coefficients
## Formula for  the sampling weights :
fX.PI0      <- formula(~0+   Int+   Ya  )
## Fomula  and covariates for the calibrated/estimated weights:
X.PINoy   <-     cbind( Xa  ) 
colnames(X.PINoy)  <- c('Int', 'x2' )
fX.PI  <- formula(~0+ Int+x2*Ya  )

## Epsilon for estimation process:
eps=1E-10
ids=ids.PI=1:length(R.PI)
## Distribution and link for ESTIMATED WEIGHTS
dist.PI='binomial'; link.PI='logit'

## Distribution and link for OUTCOME Ya
dist= 'binomial' ; link='log'
ids.PI=1:length(Ya)
simulta=FALSE
bet.PI=  bet.PITRUE

 


modSc_AlpEst  <-  CMLest(    ids=1:length(Ya), Ya,   pi0a,bet = bet,   Za, dist=dist, link=link,   
                             R.PI,  X.PINoy,  dist.PI=dist.PI, link.PI=link.PI , eps=1E-10 ,  bet.PI =NULL,   fX.PI =  fX.PI ) 



bet.PIy   <- bet.PITRUE[2]  
bet.PIx   <- bet.PITRUE[1]


pFUN0   <-  function(x.pi, yi,  ...){
  x <-  cbind(rbind(x.pi)  , yi)
  b <- c(bet.PIx, bet.PIy )
  #x <-  cbind(rbind(x.pi)   )
  #b <- c(bet.PIx  )
  pii0 <-  1/(1+exp( -x%*%b   ) ) 
  pii0}

# This fucntion allows interaction in the weights

bet   <- bet
modSc_AlpCal <-  CMLcal(  ids=1:length(Ya),  Ya, pi0a, bet =bet,  Za, dist=dist, link=link,
                          R.PI,  X.PINoy , distance=distance,    eps=1E-10,    bet.PI =NULL,     fX.PI=formula(~ 1),   pFUN0 ,X.PI.TRUENoy) 

## Conditional Max. Lik. calibration
modSc_AlpCal$coeffCL 

# Weighted Lik. calibration
modSc_AlpCal$coeffWL


## Conditional Max. Lik. estimated weg
modSc_AlpEst$coeffCL 

# Weighted Lik.  estimated weg
modSc_AlpEst$coeffWL




########################################
########################################
### Sample and models for Poisson variable  - log link
########################################
########################################
 a <- 0.5
bet <-  c(1,a,a,a) 
YaPoisson <- rpois(N,   exp( Za%*%bet) ) ;  mean(YaPoisson)
 

dist='gaussian'; link='identity'
dist='Gamma'; link='inverse2' ;



Ya     <- YaPoisson
bet.PI <-  c(-1.5,-0.3)  
YaAux   <- sqrt(Ya)
pi0a  <-   1/(1+exp(-cbind(1,YaAux)%*%bet.PI  )); sum(pi0a)  ;    
R     <-   R.PI   <-   rbinom(N, 1, pi0a ) ; sum(R)
pi0M  <-   glm(R~   Ya , family=binomial )  


X.PI.TRUE   <- cbind( 1, Ya ) 
X.PI.TRUENoy   <- cbind( Xa[,1]  ) 
colnames(X.PI.TRUE) <- c('Int', 'Ya')
colnames(X.PI.TRUENoy) <- c('Int' )
bet.PITRUE  <- pi0M$coefficients
## Formula for  the sampling weights :
fX.PI0      <- formula(~0+   Int+   sqrt(Ya) )
## Fomula  and covariates for the calibrated/estimated weights:
X.PINoy   <-     cbind( Xa  ) 
colnames(X.PINoy)  <- c('Int', 'x2' )
fX.PI  <- formula(~0+ Int+x2* sqrt(Ya)  )



## Epsilon for estimation process:
eps=1E-8
ids=ids.PI=1:length(R.PI)
## Distribution and link for ESTIMATED WEIGHTS
dist.PI='binomial'; link.PI='logit'

## Distribution and link for OUTCOME Ya
dist= 'poisson' ; link='log'
ids.PI=1:length(Ya)
simulta=FALSE
bet.PI=  bet.PITRUE
X.PINoy  <-  X.PI.TRUENoy
fX.PI   <- fX.PI0

model.matrix(fX.PI0   , data.frame(Int=1, Ya=Ya)   ) 
 

modSc_AlpEst  <-  CMLest(    ids=1:length(Ya), Ya,   pi0a,bet = bet,   Za, dist=dist, link=link,   
                             R.PI,  X.PINoy,  dist.PI=dist.PI, link.PI=link.PI , eps=1E-10 ,  bet.PI =NULL,   fX.PI =  fX.PI ) 



bet.PIy   <- bet.PITRUE[2]  
bet.PIx   <- bet.PITRUE[1]


pFUN0   <-  function(x.pi, yi,  ...){
  x <-  cbind(rbind(x.pi)  , yi)
  b <- c(bet.PIx, bet.PIy )
  #x <-  cbind(rbind(x.pi)   )
  #b <- c(bet.PIx  )
  pii0 <-  1/(1+exp( -x%*%b   ) ) 
  pii0}

# This fucntion allows interaction in the weights

bet   <- bet
modSc_AlpCal <-  CMLcal(  ids=1:length(Ya),  Ya, pi0a, bet =bet,  Za, dist=dist, link=link,
                          R.PI,  X.PINoy , distance=distance,    eps=1E-10,    bet.PI =NULL,     fX.PI=formula(~ 1),   pFUN0 ,X.PI.TRUENoy) 

## Conditional Max. Lik. calibration
modSc_AlpCal$coeffCL 

# Weighted Lik. calibration
modSc_AlpCal$coeffWL


## Conditional Max. Lik. estimated weg
modSc_AlpEst$coeffCL 

# Weighted Lik.  estimated weg
modSc_AlpEst$coeffWL


########################################
########################################
### Sample and models for Gamma variable  - inverse link
########################################
######################################## 
bet <-     c(4,1,1,1)
YaGamma <- rgamma(N,   scale=as.vector(1/( Za%*%bet )),shape=1 ) ;  mean(YaGamma)





Ya     <- YaGamma 
bet.PI <-  c(-1.6,-2)  
pi0a  <-   1/(1+exp(-cbind(1,Ya)%*%bet.PI  )); sum(pi0a)  ;    
R     <-   R.PI   <-   rbinom(N, 1, pi0a ) ; sum(R)
pi0M  <-   glm(R~   Ya , family=binomial )  


X.PI.TRUE   <- cbind( 1, Ya ) 
X.PI.TRUENoy   <- cbind( Xa[,1]  ) 
colnames(X.PI.TRUE) <- c('Int', 'Ya')
colnames(X.PI.TRUENoy) <- c('Int' )
bet.PITRUE  <- pi0M$coefficients
## Formula for  the sampling weights :
fX.PI0      <- formula(~0+   Int+   Ya  )
## Fomula  and covariates for the calibrated/estimated weights:
X.PINoy   <-     cbind( Xa  ) 
colnames(X.PINoy)  <- c('Int', 'x2' )
fX.PI  <- formula(~0+ Int+x2*Ya  )

## Epsilon for estimation process:
eps=1E-10
ids=ids.PI=1:length(R.PI)
## Distribution and link for ESTIMATED WEIGHTS
dist.PI='binomial'; link.PI='logit'

## Distribution and link for OUTCOME Ya
 
dist='Gamma'; link='inverse2' ;
ids.PI=1:length(Ya)
simulta=FALSE
bet.PI=  bet.PITRUE

 


modSc_AlpEst  <-  CMLest(    ids=1:length(Ya), Ya,   pi0a,bet = bet,   Za, dist=dist, link=link,   
                             R.PI,  X.PINoy,  dist.PI=dist.PI, link.PI=link.PI , eps=1E-10 ,  bet.PI =NULL,   fX.PI =  fX.PI ) 



bet.PIy   <- bet.PITRUE[2]  
bet.PIx   <- bet.PITRUE[1]


pFUN0   <-  function(x.pi, yi,  ...){
  x <-  cbind(rbind(x.pi)  , yi)
  b <- c(bet.PIx, bet.PIy )
  #x <-  cbind(rbind(x.pi)   )
  #b <- c(bet.PIx  )
  pii0 <-  1/(1+exp( -x%*%b   ) ) 
  pii0}

# This fucntion allows interaction in the weights

bet   <- bet
modSc_AlpCal <-  CMLcal(  ids=1:length(Ya),  Ya, pi0a, bet =bet,  Za, dist=dist, link=link,
                          R.PI,  X.PINoy , distance=distance,    eps=1E-10,    bet.PI =NULL,     fX.PI=formula(~ 1),   pFUN0 ,X.PI.TRUENoy) 

## Conditional Max. Lik. calibration
modSc_AlpCal$coeffCL 

# Weighted Lik. calibration
modSc_AlpCal$coeffWL


## Conditional Max. Lik. estimated weg
modSc_AlpEst$coeffCL 

# Weighted Lik.  estimated weg
modSc_AlpEst$coeffWL



