library(tidyverse)
getwd()
setwd("/Users/bangho/Documents/please")
load('newtfc_swine_20220412.rda')
dat <- newtfc 

#FCR calc
dat$FCR <- dat$ADFI/dat$ADG

#bw (g->kg)
dat$bw <- dat$bw/1000

head(dat)
sidAAFun <- function(x){
  x %>% mutate(sidThrLys = Thr.SID/Lys.SID,
               sidMetLys = Met.SID/Lys.SID,
               sidCysLys = Cys.SID/Lys.SID,
               sidMetCysLys = MetCys.SID/Lys.SID,
               sidTrpLys = Trp.SID/Lys.SID,
               sidIleLys = Ile.SID/Lys.SID,
               sidValLys = Val.SID/Lys.SID,
               sidLeuLys = Leu.SID/Lys.SID,
               sidPheLys = Phe.SID/Lys.SID,
               sidTyrLys = Tyr.SID/Lys.SID,
               sidHisLys = His.SID/Lys.SID,
               sidArgLys = Arg.SID/Lys.SID,
               sidAlaLys = Ala.SID/Lys.SID,
               sidAspLys = Asp.SID/Lys.SID,
               sidGluLys = Glu.SID/Lys.SID,
               sidSerLys = Ser.SID/Lys.SID,
               sidProLys = Pro.SID/Lys.SID)
}    

dat %>% filter(FCR != 'NA') -> FCR
sidAAFun(FCR) -> FCR
FCR %>% select(study, bw, ADG, ADFI, FCR, CrudeProtein,
               sidThrLys, sidMetLys, sidCysLys, sidMetCysLys, sidTrpLys,
               sidIleLys, sidValLys, sidLeuLys, sidPheLys, sidTyrLys, 
               sidHisLys, sidArgLys, sidAlaLys, sidAspLys, sidGluLys, 
               sidSerLys, sidProLys) -> a

#----EDA----
boxplot(a$FCR)
boxplot(a$FCR)$stats

eda <- subset(a, a$FCR>=boxplot(a$FCR)$stats[1,])
eda <- subset(eda, eda$FCR<=boxplot(a$FCR)$stats[5,])

boxplot(eda$FCR)
summary(eda$FCR)

#----bw10----
##bw 이산형으로 변환 -> bw10
eda %>% filter(bw<120) ->eda


foo1 <- function(x) {
  a <- round(x/10,digits = 0)*10
  return(a)
}
eda$bw10 <- lapply(eda$bw, foo1)
as.numeric(eda$bw10)->eda$bw10

#INPUT -> BW
#OUTPUT -> FCR, ADG, AA

eda %>% 
  ggplot(aes(x=bw, y=FCR)) +
  geom_point(aes(color = as.factor(study)))+
  #stat_smooth(method=lm, col ="red", formula  = y ~ x) +
  stat_smooth(method=lm, col ="blue", formula  = y ~ poly(x, 2)) +
  theme(legend.position = 'none')

lm(FCR ~ bw, data = eda) %>% summary # report
lm(FCR ~ poly(bw, 2), data = eda) %>% summary() # report

modelPoly <- lm(FCR ~ poly(bw, 2), data = eda)

pred <- predict(modelPoly, newdata = eda)

eda %>% 
  mutate(pred = pred) %>% 
  mutate(binN = ifelse(FCR >= pred, 1, 0)) -> eda

eda %>% 
  ggplot(aes(x=bw, y=FCR)) +
  geom_point(aes(color = as.factor(binN)))+
  #stat_smooth(method=lm, col ="red", formula  = y ~ x) +
  stat_smooth(method=lm, col ="blue", formula  = y ~ poly(x, 2)) +
  labs(color = "FCR binning") +
  theme(legend.position = 'bottom')

#write.csv(eda, file="binDataFcr.csv", row.names = F)\