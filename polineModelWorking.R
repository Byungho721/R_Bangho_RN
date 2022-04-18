#poline model

testData <- tD[14,]

modelPoly <- lm(FCR ~ poly(bw, 2), data = eda)


poline %>% predict(testData, "prob") %>% as.data.frame() -> m
predict(modelPoly, newdata = data.frame(bw = testData$bw)) -> a
eda$FCR[eda$bw == testData$bw] -> b
mean(b[b>=a]) -> c
mean(b[b < a]) -> d

data.frame(bw = testData$bw,
           bw10 = testData$bw10, 
           HFCR = round(c, digits = 2),
           HFCRp = m[1, 1],
           LFCR = round(d, digits = 2),
           LFCRp = m[1, 2])

polineWorking <- function(x) {
  testData <- tD[x,]
  poline %>% predict(testData, "prob") %>% as.data.frame() -> m
  predict(modelPoly, newdata = data.frame(bw = testData$bw)) -> a
  eda$FCR[eda$bw == testData$bw] -> b
  mean(b[b>=a]) -> c
  mean(b[b < a]) -> d
  data.frame(bw = testData$bw,
             bw10 = testData$bw10, 
             HFCR = round(c, digits = 2),
             HFCRp = m[1, 1],
             LFCR = round(d, digits = 2),
             LFCRp = m[1, 2]) -> out
  return(out)
} 
tDList <- c(1:1200)
lapply(tDList, polineWorking) -> tDResult
bind_rows(tDResult) ->tDF

save(tDF, file = "testDataResult.rda")
