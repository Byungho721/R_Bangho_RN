library(data.table)
library(stats)
eda <- read.csv(file= "binDataFcr.csv")
eda$binN <- as.factor(eda$binN)
df <- subset(eda, select = c(2, 6:24, 26)) #input = BW, CP, AAs

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
colnames(db) <- colN
CJ(db$bw, db$CrudeProtein, db$sidThrLys, db$sidMetLys, 
   db$sidCysLys, db$sidMetCysLys, db$sidTrpLys, db$sidIleLys,
   db$sidValLys,db$sidLeuLys, db$sidPheLys, db$sidTyrLys,
   db$sidHisLys,db$sidArgLys, db$sidAlaLys, db$sidAspLys,
   db$sidGluLys, db$sidSerLys, db$sidProLys, db$bw10)


#Error in CJ(db$bw, db$CrudeProtein, db$sidThrLys, db$sidMetLys, db$sidCysLys,  : 
#Cross product of elements provided to CJ() would result in 3486784401 rows
#which exceeds .Machine$integer.max == 2147483647