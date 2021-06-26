#### This is the analysis script for the book chapter in the latest version ######
## Wir br?uchten also die study success and satisfaction Indikatoren f?r die Verl?ufe 
## und eine Liste von Variablen f?r die wir Subgruppen finden wollen.

library(subgroupsem)
library(lavaan)
library(tidyverse)

## Variablen
# ID_t	Personen_ID
# wave	Erhebungswelle

## AV: Studienabbruchgedanken
# Item: tg53221	oft an Studienabbruch gedacht	1 (gar nicht) - 4 (v?llig)
# Item: tg53222	Wenn ich nochmals w?hlen k?nnte, w?rde ich mich f?r ein anderes 
#       Studienfachentscheiden.	1 (gar nicht) - 4 (v?llig)
# Item: tg53223 ernsthafter Gedanke an Studienabbruch mit	1 (gar nicht) - 4 (v?llig)

## Potentielle Subgruppen-Merkmale
# Item: t700001	Geschlecht	1=m 2=w
# tx29003	Muttersprache: deutsch	1=ja, 2=nein
# tx29060	Momentan Berufst?tig	0=nein, 1=ja
# t242001	Nutzung eines Beratungsangebots	1=genutzt, 2=nicht genutzt
# Beratung zur Immatrikulation/Zulassung
# tx29062 berufliche Stellung (aktuell)	Nominal, 9 Auspr?gungen

d <- readRDS("../data/SC5_SubgroupSEM.rds")
d <- subset(d,
            subset=wave %in% c(2,4,6,8),
            select=c(ID_t,wave,
                     tg53221,tg53222,tg53223,
                     t70000y, t700001,
                     t731351_g1, t731301_g1
                     ))
names(d) <- c("id", "wave", 
              "abbruch1", "abbruch2", "abbruch3", 
              "geburtsjahr", "gender", 
              "bildung_vater", "bildung_mutter")

d <- within(d, {
  geburtsjahr <- as.numeric(geburtsjahr)
  gender <- as.factor(gender)
  bildung_vater <- as.factor(bildung_vater)
  bildung_mutter <- as.factor(bildung_mutter)
})
summary(d)

## wide data format
dwide <- reshape(d,
                 v.names=c("abbruch1", "abbruch2", "abbruch3"),
                 timevar="wave",
                 idvar="id",
                 direction="wide")
summary(dwide)

## RIASEC is only measured at wave 1
d <- readRDS("../data/SC5_SubgroupSEM.rds")
dinterest <- subset(d,
                    subset=wave==1,
                    select=c(ID_t,t66207a_g1, t66207b_g1, t66207c_g1, t66207d_g1, t66207e_g1, t66207f_g1, tg02001))
names(dinterest) <- c("id", "R", "I", "A", "S", "E", "C", "ang_abschluss")

dinterest <- within(dinterest, {
  R <- as.numeric(R)
  I <- as.numeric(I)
  A <- as.numeric(A)
  S <- as.numeric(S)
  E <- as.numeric(E)
  C <- as.numeric(C)
  ang_abschluss <- as.factor(ang_abschluss)
})

#rm(d)

## merge dwide and beratung
dwide <- merge(dwide, dinterest, by="id", all=TRUE)
head(dwide)

# reliability
library(psych)
dAbbruch <- data.frame(d$abbruch1, d$abbruch2, d$abbruch3)
omegah(dAbbruch)


# descriptives
summary(dwide)


####### lavaan model #############
dwide$subg <- as.numeric(dwide$gender==1)

model <- '
eta1 =~ 1*abbruch1.2 + c(la2,la2)*abbruch2.2 + c(la3,la3)*abbruch3.2
eta2 =~ 1*abbruch1.4 + c(la2,la2)*abbruch2.4 + c(la3,la3)*abbruch3.4
eta3 =~ 1*abbruch1.6 + c(la2,la2)*abbruch2.6 + c(la3,la3)*abbruch3.6
eta4 =~ 1*abbruch1.8 + c(la2,la2)*abbruch2.8 + c(la3,la3)*abbruch3.8

eta1 ~ c(m11,m12)*1
eta2 ~ c(m21,m22)*1
eta3 ~ c(m31,m32)*1
eta4 ~ c(m41,m42)*1

abbruch1.2 ~ 0*1
abbruch1.4 ~ 0*1
abbruch1.6 ~ 0*1
abbruch1.8 ~ 0*1

abbruch2.2 ~ c(nu2,nu2)*1
abbruch2.4 ~ c(nu2,nu2)*1
abbruch2.6 ~ c(nu2,nu2)*1
abbruch2.8 ~ c(nu2,nu2)*1

abbruch3.2 ~ c(nu3,nu3)*1
abbruch3.4 ~ c(nu3,nu3)*1
abbruch3.6 ~ c(nu3,nu3)*1
abbruch3.8 ~ c(nu3,nu3)*1


MF1 =~ abbruch2.2 + 1*abbruch2.4 + 1*abbruch2.6 + 1*abbruch2.8
MF2 =~ abbruch3.2 + 1*abbruch3.4 + 1*abbruch3.6 + 1*abbruch3.8

eta1 + eta2 + eta3 + eta4 ~~ 0*MF1 + 0*MF2
MF1 ~~ 0*MF2 
'

model2 <- '
eta1 =~ 1*abbruch1.2 + c(la2,la2)*abbruch2.2 + c(la3,la3)*abbruch3.2
eta2 =~ 1*abbruch1.4 + c(la2,la2)*abbruch2.4 + c(la3,la3)*abbruch3.4
eta3 =~ 1*abbruch1.6 + c(la2,la2)*abbruch2.6 + c(la3,la3)*abbruch3.6
eta4 =~ 1*abbruch1.8 + c(la2,la2)*abbruch2.8 + c(la3,la3)*abbruch3.8

pi1 =~ 1.00*eta1 + 1.00*eta2 + 1.00*eta3 + 1.00*eta4
pi2 =~ 0.00*eta1 + -0.5*eta2 + -0.5*eta3 + 1.00*eta4
pi3 =~ -.25*eta1 + 0.25*eta2 + 0.25*eta3 + -.25*eta4
pi4 =~ 0.00*eta1 + -0.5*eta2 + 0.50*eta3 + 0.00*eta4


eta1 ~ 0*1
eta2 ~ 0*1
eta3 ~ 0*1
eta4 ~ 0*1

pi1 ~ c(m11,m12)*1
pi2 ~ c(m21,m22)*1
pi3 ~ c(m31,m32)*1
pi4 ~ c(m41,m42)*1

abbruch1.2 ~ 0*1
abbruch1.4 ~ 0*1
abbruch1.6 ~ 0*1
abbruch1.8 ~ 0*1

abbruch2.2 ~ c(nu2,nu2)*1
abbruch2.4 ~ c(nu2,nu2)*1
abbruch2.6 ~ c(nu2,nu2)*1
abbruch2.8 ~ c(nu2,nu2)*1

abbruch3.2 ~ c(nu3,nu3)*1
abbruch3.4 ~ c(nu3,nu3)*1
abbruch3.6 ~ c(nu3,nu3)*1
abbruch3.8 ~ c(nu3,nu3)*1

MF1 =~ abbruch2.2 + 1*abbruch2.4 + 1*abbruch2.6 + 1*abbruch2.8
MF2 =~ abbruch3.2 + 1*abbruch3.4 + 1*abbruch3.6 + 1*abbruch3.8

eta1 + eta2 + eta3 + eta4 ~~ 0*MF1 + 0*MF2
pi1 + pi2 + pi3 + pi4 ~~ 0*MF1 + 0*MF2
eta1 ~~ 0*eta1 + 0*eta2 + 0*eta3 + 0*eta4
eta2 ~~ 0*eta2 + 0*eta3 + 0*eta4
eta3 ~~ 0*eta3 + 0*eta4
eta4 ~~ 0*eta4
MF1 ~~ 0*MF2 
'



m1 <- sem(model, data=dwide, group="subg", missing="fiml",
          group.label=c("0","1"))
m2 <- sem(model2, data=dwide, group="subg", missing="fiml",
          group.label=c("0","1"))
summary(m1)
summary(m2)
fitmeasures(m1)[c("rmsea", "cfi", "tli", "srmr", "chisq", "df", "pvalue")] %>% round(3)
fitmeasures(m2)[c("rmsea", "cfi", "tli", "srmr", "chisq", "df", "pvalue")] %>% round(3)

con = '
m11==m12
m21==m22
m31==m32
m41==m42
'           

wald <- lavTestWald(m1, constraints=con)
wald

####### subroupsem model #############

#columns <- c("geburtsjahr", "gender",   
#             "bildung_vater", "bildung_mutter", "ang_abschluss", 
#             "R", "I", "A", "S", "E", "C")

columns <- c("geburtsjahr", "gender",
             "bildung_vater", "bildung_mutter", "ang_abschluss")


#dwide_omit <- dwide[complete.cases(dwide[,columns]),]

C <- matrix(c(0.25, 0.25, 0.25, 0.25,
              -1.0, 0.00, 0.00, 1.00,
              -3.0, 1.00, 1.00, 1.00,
              0.00, -1.00, 1.00, 0.00), ncol = 4, byrow = T)

solve(C) %>% t() 



model <- '
eta1 =~ 1*abbruch1.2 + c(la2,la2)*abbruch2.2 + c(la3,la3)*abbruch3.2
eta2 =~ 1*abbruch1.4 + c(la2,la2)*abbruch2.4 + c(la3,la3)*abbruch3.4
eta3 =~ 1*abbruch1.6 + c(la2,la2)*abbruch2.6 + c(la3,la3)*abbruch3.6
eta4 =~ 1*abbruch1.8 + c(la2,la2)*abbruch2.8 + c(la3,la3)*abbruch3.8

pi1 =~ 1.00*eta1 + 1.00*eta2 + 1.00*eta3 + 1.00*eta4
pi2 =~ 0.00*eta1 + -0.5*eta2 + -0.5*eta3 + 1.00*eta4
pi3 =~ -.25*eta1 + 0.25*eta2 + 0.25*eta3 + -.25*eta4
pi4 =~ 0.00*eta1 + -0.5*eta2 + 0.50*eta3 + 0.00*eta4


eta1 ~ 0*1
eta2 ~ 0*1
eta3 ~ 0*1
eta4 ~ 0*1

pi1 ~ c(m11,m12)*1
pi2 ~ c(m21,m22)*1
pi3 ~ c(m31,m32)*1
pi4 ~ c(m41,m42)*1

abbruch1.2 ~ 0*1
abbruch1.4 ~ 0*1
abbruch1.6 ~ 0*1
abbruch1.8 ~ 0*1

abbruch2.2 ~ c(nu2,nu2)*1
abbruch2.4 ~ c(nu2,nu2)*1
abbruch2.6 ~ c(nu2,nu2)*1
abbruch2.8 ~ c(nu2,nu2)*1

abbruch3.2 ~ c(nu3,nu3)*1
abbruch3.4 ~ c(nu3,nu3)*1
abbruch3.6 ~ c(nu3,nu3)*1
abbruch3.8 ~ c(nu3,nu3)*1

MF1 =~ abbruch2.2 + 1*abbruch2.4 + 1*abbruch2.6 + 1*abbruch2.8
MF2 =~ abbruch3.2 + 1*abbruch3.4 + 1*abbruch3.6 + 1*abbruch3.8

eta1 + eta2 + eta3 + eta4 ~~ 0*MF1 + 0*MF2
pi1 + pi2 + pi3 + pi4 ~~ 0*MF1 + 0*MF2
eta1 ~~ 0*eta1 + 0*eta2 + 0*eta3 + 0*eta4
eta2 ~~ 0*eta2 + 0*eta3 + 0*eta4
eta3 ~~ 0*eta3 + 0*eta4
eta4 ~~ 0*eta4
MF1 ~~ 0*MF2 

diff := m11 - m12
'

con = '
m11==m12
m21==m22
m31==m32
m41==m42
'        

f_fit <- function(sg, dat) {
  ### Transform subgroup from logical to numerical and add to dataset
  sg <- as.numeric(sg)
  dat$subgroup <- sg
  
  ###### Try to fit lavaan model
  rval <- tryCatch({
    fit <- sem(model, data = dat, group='subgroup', missing="fiml")
    
    no_warns <- lavaan:::lav_object_post_check(fit)
    if(!no_warns){return(-1)}
    
    # interestingness measure: Wald test
    #wald <- lavTestWald(fit, constraints=con)
    #res <- wald$stat
    
    # interestingness measure: model fit
    #rmsea <- fitmeasures(fit)["rmsea"]
    #res <- 1/rmsea
    
    # # interestingness measure: absolute mean difference first growth component
    est <- parameterEstimates(fit)
    res <- abs(unlist(subset(est, lhs=="diff", select="est")))
    
    print(res)
    res
    
  }, error = function(e) -1)
  
  if(!is.numeric(rval) | length(rval) > 1){rval <- -1}
  
  return(rval)
}

task <- subgroupsem(f_fit = f_fit,
                    dat = dwide,
                    columns = columns,
                    search_depth = 3,
                    max_n_subgroups = 10,
                    generalization_aware=TRUE)
summary(task)
plot(task)
saveRDS(task, "resultsNEPS2.rds")
task <- readRDS("resultsNEPS2.rds")
saveRDS(dwide, "dwide.rds")


# columns <- c("geburtsjahr", "gender",
#              "bildung_vater", "bildung_mutter", "ang_abschluss")

## Post-analysis
# select subgroup
# get description and add selectors to NA selection

task$subgroups$Subgroup5$description
na_descriptors <- c("geburtsjahr", "bildung_mutter")
sg <- task$subgroups$Subgroup5$cases
has_na <- sapply(columns, function(column) is.na(dwide[, column]))
has_na_selectors <- apply(has_na[, na_descriptors, drop = F], 
                          1, any)
sg <- ifelse(has_na_selectors, NA, sg)
summary(sg)

dwide$subgroup <- sg

###### Try to fit lavaan model
fit <- sem(model, data = dwide, group='subgroup', missing="fiml")
summary(fit)
  
# interestingness measure: Wald test
wald <- lavTestWald(fit, constraints=con)
res <- wald$stat
