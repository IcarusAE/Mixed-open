library(lavaan)
library(tidyverse)
library(gridExtra)



###############################################################################
###############################################################################
##                                                                           ##
##                     Mediator models als Path model                        ##
##                                                                           ##
###############################################################################
###############################################################################


# TPB-model mit subj. Wissen als Mediator
medmod3 <- '
  csefX.t3 ~ csefX.t1 + i*treatment  + b*knwlslfX.t3 
  attbmgX.t3 ~ attbmgX.t1 + e*treatment 
  snbmgX.t3 ~ snbmgX.t1 + g*treatment
  knwlslfX.t3 ~ knwlslfX.t1 + a*treatment
  eeaX.t3 ~ eeaX.t1 + d*csefX.t3 + f*attbmgX.t3 + h*snbmgX.t3  + c*knwlslfX.t3
  
  attbmgX.t3 ~~ csefX.t3
  csefX.t3 ~~ snbmgX.t3
  snbmgX.t3 ~~attbmgX.t3
  
  #Specific indirect effects
  sie.int.kn.sef := a*b
  sie.int.kn.sef.eea := a*b*d 
  sie.int.kn.eea := a*c
  sie.kn.sef.eea:= b*d
  sie.int.att.eea := e*f
  sie.int.sn.eea := g*h
  
  # total indirect effect (tie) and total effect (tot)
  tie.int.kn.eea := a*b*d + a*c
  tot.int.sef := a*b + i
  tot.kn.eea:= b*d + c
  tot.int.eea := a*b*d + a*c + i*d + e*f + g*h
'
#Sobel
fit.medmod3 <- sem(medmod3, data=data, estimator="MLR", missing="FIML", fixed.x=TRUE)
summary(fit.medmod3, standardized=TRUE, modindices=F)

#Bootstrapping
fit.medmod3 <- sem(medmod3, data=data, estimator="ML", missing="FIML", fixed.x=TRUE, test="Yuan-Bentler", se="boot")
summary(fit.medmod3, standardized=TRUE, modindices=F)
resid(fit.medmod3, "standardized")  

parameterEstimates(fit.medmod3, ci=TRUE)






###############################################################################
###############################################################################
##                                                                           ##
##                     Mediator models alles latent                          ##
##                                                                           ##
###############################################################################
###############################################################################



# F I N A L   M O D E L
#TPB-model mit subj. Wissen als Mediator
medmod3 <- '
  ATT_t1 =~ atbmg01.t1  + x*atbmg03.t1
  ATT_t3 =~ atbmg01.t3  + x*atbmg03.t3
  SN_t1 =~ sn01.t1 + y*sn02.t1
  SN_t3 =~ sn01.t3 + y*sn02.t3
  CSEF_t1 =~ csef01.t1 + z*csef03.t1
  CSEF_t3 =~ csef01.t3 + z*csef03.t3
  PercKnow_t1 =~ knwlslfX.t1
  PercKnow_t3 =~ knwlslfX.t3
  EEA_t1 =~ eeaX.t1
  EEA_t3 =~ eeaX.t3
  
  atbmg01.t1 ~~atbmg01.t3
  atbmg03.t1 ~~atbmg03.t3
  #sn01.t1 ~~ sn01.t3
  sn02.t1 ~~ sn02.t3
  csef01.t1 ~~csef01.t3
  csef03.t1 ~~csef03.t3
  
  #fixed errors
  eeaX.t1 ~~.201*eeaX.t1
  eeaX.t3 ~~.189*eeaX.t3
  knwlslfX.t1 ~~.192*knwlslfX.t1
  knwlslfX.t3 ~~.23*knwlslfX.t3
  
  # Structural model
  
  CSEF_t3 ~ CSEF_t1 + d*treatment  + b*PercKnow_t3 
  ATT_t3 ~ ATT_t1 + e*treatment 
  SN_t3 ~ SN_t1 + f*treatment
  PercKnow_t3 ~ PercKnow_t1 + a*treatment + j*ATT_t3
  EEA_t3 ~ EEA_t1 + c*CSEF_t3 + g*ATT_t3 + h*SN_t3  + i*PercKnow_t3
  
  #Inter mediator error covs
  ATT_t3 ~~ CSEF_t3
  CSEF_t3 ~~ SN_t3
  SN_t3 ~~ ATT_t3
  
  #Specific indirect effects
  sie.int.pk.eea := a*i
  sie.int.pk.se.eea := a*b*c
  sie.int.se.eea := d*c
  sie.int.att.eea := e*g
  sie.int.sn.eea := f*h
  
  # total indirect effect (tie) and total effect (tot)
  tot.int.eea := a*b*c + d*c + e*g + f*h + a*i + e*j*i + e*j*b*c
  
'
#Sobel
fit.medmod3 <- sem(medmod3, data=data, estimator="MLR", missing="FIML", fixed.x=TRUE)
summary(fit.medmod3, standardized=TRUE, modindices=F)
resid(fit.medmod3, "standardized")  


#Bootstrapping
fit.medmod3 <- sem(medmod3, data=data, estimator="ML", missing="FIML", fixed.x=TRUE, test="Yuan-Bentler", se="boot")


parameterEstimates(fit.medmod3, ci=TRUE)

