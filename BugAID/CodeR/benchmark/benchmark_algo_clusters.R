library(rbenchmark)
source("model/DbScanClustering.R")
source("model/OpticsClustering.R")

resultbenchmark <- benchmark("DbScan" = {
  b <- dbScanClustering(db, datasetBugId)
},
"Optics" = {
  b <- opticsClustering(db, datasetBugId)
},
replications = 20,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))

autoplot(resultbenchmark)

#------------------------------


n <- 20
time.DBScan <- system.time(for (i in 1:n) b <- dbScanClustering(db, datasetBugId)) / length(n)
time.Optics <- system.time(for (i in 1:n) b <- opticsClustering(db, datasetBugId)) / length(n)



#------------------------------


library(microbenchmark)
library(ggplot2)

n <- 50
set.seed(2017)

mbm3 <- microbenchmark("DbScan" = {
  b <- dbScanClustering(db, datasetBugId)
}, "Optics" = {
  b <- opticsClustering(db, datasetBugId)
}, times = n)

autoplot(mbm2) + 
  ggtitle("Tempo de execução dos algoritmos") + 
  geom_line(aes(color = expr))


mbm2_Optics <- mbm2[mbm2$expr == "Optics",]
mbm2_DbScan <- mbm2[mbm2$expr == "DbScan",]

resul_final <- rbind(mbm2_Optics, mbm2_DbScan)

ggplot(resul_final, aes(x=interation, y=time)) +
  geom_line(aes(color = expr)) +
  ggtitle("Tempo de execução dos algoritmos")


