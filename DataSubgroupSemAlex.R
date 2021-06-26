library(foreign)
library(dplyr)
library(haven)

##Verlaufsvariabeln Studienerfolg
dpTargetCAWI <- read.spss("SC5_pTargetCAWI_D_12-0-0.sav", to.data.frame = T, use.value.labels = F)
head(dpTargetCAWI)
dVS <- dpTargetCAWI %>% select(ID_t, ID_i, wave, tg52011, tg52015, tg52020,tg53111:tg53236)
head(dVS)


##sozioÃ¶konomischer Status --> kein Zugriff
##dMicrom <- read.spss("SC5_pTargetMicrom_D_12-0-0.sav", to.data.frame = T, use.value.labels = F)


##Subgruppenvariablen
dBasics <- read.spss("SC5_Basics_D_12-0-0.sav", to.data.frame = T, use.value.labels = F)
head(dBasics)
dPerson <- dBasics %>% select (ID_t,t70000y, t700001,t405000_g1,tx29003,tx29004,t741001,tx29060,tx29062,t751001_g1,tx29505,t731301_g1,t731403_g9,tx29705,t731351_g1,t731453_g9)


#merge and write
d <- merge(dVS,dPerson, by="ID_t")
write.table(d, file = "SC_5_SubgroupSem_data.csv", sep = ",",
            row.names = TRUE, col.names = NA)


