library(data.table)
library(stats)
eda <- read.csv(file= "binDataFcr.csv")
eda$binN <- as.factor(eda$binN)
df <- subset(eda, select = c(2, 6:24)) #input = BW, CP, AAs

#----data production setting, data.table::CJ function----
aCol <- c(1, 2, 3)
bCol <- c(4, 5, 6)
cCol <- c(7, 8, 9)
data.table::CJ(aCol,bCol,cCol)

ab <- data.frame(aCol,bCol,cCol)
CJ(ab$aCol,ab$bCol,ab$cCol)

#----data partitioning----
bwN <- c(1:12)*10

foo1 <- function(x) {
  df %>% filter(bw10==x) -> out
  return(out)
}

lapply(bwN,foo1) -> bwB

#----min, median, max----
colnames(df) -> colN

foo2 <- function(y) {
  bwB[[1]] %>% select(y) -> a
  as.numeric(unlist(a)) -> b
  c(min(b), median(b), max(b)) -> out
  return(out)
}

bind_cols(lapply(colN, foo2)) -> db
as.data.frame(db) ->db
colnames(db) <- colN
CJ(db$bw, db$CrudeProtein, db$sidThrLys, db$sidMetLys, 
   db$sidCysLys, db$sidMetCysLys, db$sidTrpLys, db$sidIleLys,
   db$sidValLys,db$sidLeuLys, db$sidPheLys, db$sidTyrLys,
   db$sidHisLys,db$sidArgLys, db$sidAlaLys, db$sidAspLys,
   db$sidGluLys, db$sidSerLys, db$sidProLys, db$bw10)


#Error in CJ(db$bw, db$CrudeProtein, db$sidThrLys, db$sidMetLys, db$sidCysLys,  : 
#Cross product of elements provided to CJ() would result in 3486784401 rows
#which exceeds .Machine$integer.max == 2147483647

#----data randomizing, and extraction----

r1 <- c(1,2,3)
CJ(r1,r1,r1,r1,r1) -> rN

rN[sample(nrow(rN), 100),] ->rN1
rN[sample(nrow(rN), 100),] ->rN2
rN[sample(nrow(rN), 100),] ->rN3
rN[sample(nrow(rN), 100),] ->rN4
cbind(rN1, rN2, rN3, rN4)-> rND
colnames(rND) <- colN

rND %>% select(bw) -> a
db %>% select(bw) -> b 
ifelse(a[,1]==1, b[1,1],
       ifelse(a[,1]==2,b[2,1],b[3,1]))

dbMaking <- function(x) {
  rND %>% select(x) -> a
  db %>% select(x) -> b 
  ifelse(a[,1]==1, b[1,1],
         ifelse(a[,1]==2,b[2,1],b[3,1]))
}

lapply(colN,dbMaking)->testData
bind_cols(testData)-> tD #randomized sample data


#----making sample data_ each bw10 phase-----

colnames(df) <- colN

foo3 <- function(y) {
  bwB[[1]] %>% select(y) -> a #bwB[[i]] change! bw10 =bwB[[1]]
  as.numeric(unlist(a)) -> b
  c(min(b), median(b), max(b)) -> out
  return(out)
}
as.data.frame(bind_cols(lapply(colN, foo3))) -> db10 #dataName change!
colnames(db10) <- colN #dataName change!

MakingRepeat <- function(x) {
  rND %>% select(x) -> a
  db120 %>% select(x) -> b #dataName change!
  ifelse(a[,1]==1, b[1,1],
         ifelse(a[,1]==2,b[2,1],b[3,1]))
}
lapply(colN,MakingRepeat)-> testData
bind_cols(testData)-> tD120 #dataName change!

bind_rows(tD10, tD20, tD30, tD40, tD50, tD60,
          tD70, tD80, tD90, tD100, tD110, tD120) ->tD #Total sampleData

#bw10
foo4 <- function(x) {
  a <- round(x/10,digits = 0)*10
  return(a)
}
tD$bw10 <-as.numeric(lapply(tD$bw, foo4))

save(tD, file = "tD.rda")
