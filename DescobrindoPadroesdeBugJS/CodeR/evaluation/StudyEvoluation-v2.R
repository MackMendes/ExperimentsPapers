# ===========
# Objetivo: 
#   Realizar a avaliacao do estudo do Hanan, com os resultados obtidos pelo BugAID.
# 
# Entendimento do certo ou errado: 
#   * VERDADEIRO-POSITIVO: Commit que foi agrupado (clusterizado) no cluster que contenha um dos BCTs do Commit;
#   * VERDADEIRO-NEGATIVO: Commit que NAO foi agrupado no cluster com BCTs que NAO teria;
#   * FALSO-POSTIVO: Commit que foi agrupado no cluster com BCT que NAO possui;
#   * FALSO-NEGATIVO: Commit que NAO foi agrupado ao cluster que deveria ter sido agrupado.
# 
# Descricao Geral:
# ===========
# Desenvolver a implementacao para calcular os BCTs gerados errados, com base
# no que o Hanam disse: "If two commits that have the same repair type are 
# clustered together by DBScan, we consider those commits true positives. 
# All other unrelated commits clustered with commits from the oracle are 
# false positives. All commits in the oracle not clustered together are false 
# negatives". 
# ===========

setwd("G:/Mestrado/Meus experimentos/Meu GitHub/ExperimentsPapers/DescobrindoPadroesdeBugJS/CodeR/evaluation")

# ===========
# Obter o dataset de classificacoes feita pelo estudo do Hanan (BugAID)
# ===========
languageconstruct <- read.csv(file="original dataset/language_construct_database.csv", 
                              header=TRUE, sep=";")

names(languageconstruct)[1] <- "IDCommit"

# ===========
# Obter o dataset que o Hanan informou ter as informacoes de {Reparo, Label}
# ===========
datasetLabelReparoCommit <- read.csv(file="original dataset/oracle.csv", 
                                     header=TRUE, sep=";")
names(datasetLabelReparoCommit)

# ===========
# Realizando um Join entre os dois datasets 
# ===========
library(dplyr)
datasetOracleCompleted <- (datasetLabelReparoCommit  %>% 
                             inner_join(languageconstruct, by = c("IDCommit" = "IDCommit"))) 

# ===========
# Resultado obtido
# ===========
resultlanguageconstructWithDBScan <- 
  read.csv(file="dataset job/BugAID/resultado_language_construct_to_tuning.csv", 
                              header=TRUE, sep=";")


ncolRowOracle <- integer()
quantidadeBasicChanges <- integer()
iClusterID <- integer()
instancesIdCommitsByCluster <- array()
iBCTs <- data.frame()

matrixconfusion <- datasetOracleCompleted[,c("IDRepair", "IDCommit", "ProjectID", "Cluster")]
matrixconfusion$IDClustertByCluster <- 0
matrixconfusion$IDRepairByCluster <- 0
matrixconfusion$QuantidadeBasicChanges <- 0 
matrixconfusion$QuantidadeBCTsMath <- 0


quantidadeMaxBCTS <- resultlanguageconstructWithDBScan %>% 
                        filter(BasicChanges == max(BasicChanges)) %>%
                        top_n(1)

# ===========
# Montando as colunas de resultado de forma dinamica
# ===========
for(r in 1:as.integer(quantidadeMaxBCTS$BasicChanges)) { 
  # Resultado do Cluster
  matrixconfusion[1, paste("BCT", r, "_ByCluster", sep="")] <- NA
  
  # BCT que deu Math (sao iguais). 
  matrixconfusion[1, paste("BCT", r, "_ByCluster_MathWith", sep="")] <- NA
}


for(i in 1:nrow(resultlanguageconstructWithDBScan))  # Para cada linha do resultado obtido
{
  # Obtendo os commits de cada agrupamento, na coluna com as instancias (sperados por virgula)
  instancesIdCommitsByCluster <-
    as.array(unlist(strsplit(as.character(resultlanguageconstructWithDBScan[i,]$ContainsInstances), ',')))
  
  # Obtendo todos os BCTs de cada agrupamento/cluster, na coluna com os BTCs (seperados por virgula)
  iBCTs <-
    as.data.frame(unlist(strsplit(as.character(resultlanguageconstructWithDBScan[i,]$BCTs), ',')))
  
  quantidadeBasicChanges <- as.integer(resultlanguageconstructWithDBScan[i,]$BasicChanges)
  iClusterID <- as.integer(resultlanguageconstructWithDBScan[i,]$ClusterID)

  for(j in 1:dim(instancesIdCommitsByCluster))  # Para cada IdCommit agrupado no cluster percorrido
  {
      # Procurar o registro dentro do dataset principal, o Commit que foi Clusterizado (agrupado) no resultado
      rowOracle <- datasetOracleCompleted[datasetOracleCompleted$IDCommit == (as.integer(instancesIdCommitsByCluster[j])),]

      if(nrow(rowOracle) != 0) { # Se encontrou um registro com tenha sido agrupado no resultado

        # Setar a quantidade de BCTs agrupadas no Cluster resultante
        matrixconfusion[matrixconfusion$IDCommit == (as.integer(instancesIdCommitsByCluster[j])),
                        "QuantidadeBasicChanges"] <- quantidadeBasicChanges
        
        # Setar o ID do Cluster que este item foi agrupado
        matrixconfusion[matrixconfusion$IDCommit == (as.integer(instancesIdCommitsByCluster[j])),
                        "IDClustertByCluster"] <- iClusterID
        
        ncolRowOracle <- ncol(rowOracle[, 11: ncol(rowOracle)]) # Obtendo a quantidade de BCTs contidas no DataSet
        
        for(b in 1:nrow(iBCTs)) {
          
          # Pegando o resultado do Cluster e jogado na matriz de confusao (montando as colunas de resultado de forma dinÃ¢mica)
          matrixconfusion[matrixconfusion$IDCommit == (as.integer(instancesIdCommitsByCluster[j])), 
                          paste("BCT", b, "_ByCluster", sep="")] <- trimws(as.character(iBCTs[b,]))

          for (x in 1:ncolRowOracle) {
            
            # Se o valor da coluna de BCT que o Commit possui (antes do cluster), NAO for NA ou se NAO for string vazia
            if(!is.na(rowOracle[,paste("BCT", x, sep="")]) && 
               trimws(as.character(rowOracle[,paste("BCT", x, sep="")])) != "") {
            
              # EntÃ£o, armazeno na Matriz de Confusao este BCT
              matrixconfusion[matrixconfusion$IDCommit == (as.integer(instancesIdCommitsByCluster[j])), 
                              paste("BCT", x, sep="")] <- trimws(paste0(as.character(rowOracle[,paste("BCT", x, sep="")]), ".0"))
              
              
              # So se os BCTs estao iguais, que gravamos na Matriz de Confusao, o nome da coluna que deu Math (igualou) com o 
              # BCT agrupado no resultado do DBScan
              if(trimws(paste0(as.character(rowOracle[,paste("BCT", x, sep="")]), ".0")) == 
                 trimws(as.character(iBCTs[b,]))){
                
                matrixconfusion[matrixconfusion$IDCommit == (as.integer(instancesIdCommitsByCluster[j])), 
                                paste("BCT", b, "_ByCluster_MathWith", sep="")] <- paste("BCT", x, sep="")

                matrixconfusion[matrixconfusion$IDCommit == (as.integer(instancesIdCommitsByCluster[j])), 
                                "QuantidadeBCTsMath"] <- (1 + as.integer(matrixconfusion[matrixconfusion$IDCommit ==
                                 (as.integer(instancesIdCommitsByCluster[j])), "QuantidadeBCTsMath"]))
                
              }
            }
            
            
          }
          
        }
        
      }
      
  }
}

# ===========
# Obtendo os BCTs com os Reparos
# ===========
source("GetBCTsWithRepair.R")
padroesDefeitosPorConjutoBCTs <- getBCTsWithRepair()

# ===========
# Obtendo as Labels do Tipo de Reparo (BCTs por tipo de Reparo)
# ===========

padroesDefeitosPorConjutoBCTs_Job <- padroesDefeitosPorConjutoBCTs %>%
  inner_join(matrixconfusion, by = c("IDRepair" = "IDRepair", "CountBCTs" = "QuantidadeBasicChanges")) %>%
  select(IDRepair, Fault, Repair, BCT1_Label, BCT2_Label, BCT3_Label, BCT4_Label, IdCCT, AvgModifiedStatements, CountBCTs)



# =========================================
# ==== Montando a matrix da confusÃ£o ======
# =========================================

matrixconfusion$VerdadeiroPositivo <- 0
matrixconfusion$VerdadeiroNegativo <- 0
matrixconfusion$FalsoNegativo <- 0
matrixconfusion$FalsoPositivo <- 0

library("plyr")

matrixMaths <- data.frame(BCT1_Math=integer(),
                          BCT2_Math=integer(),
                          BCT3_Math=integer(),
                          BCT4_Math=integer(),
                          stringsAsFactors=FALSE)

for (mc in 1:nrow(matrixconfusion)) 
{
  
  if(matrixconfusion[mc,]$IDClustertByCluster == 0) { # O commit nao foi agrupado em nenhum cluster
    matrixconfusion[mc,"FalsoNegativo"] <- 1
  }
  else {

    mcFilterLabels <- padroesDefeitosPorConjutoBCTs_Job[(padroesDefeitosPorConjutoBCTs_Job$IDRepair == 
                        (as.integer(matrixconfusion[mc,"IDRepair"])) && 
                         padroesDefeitosPorConjutoBCTs_Job$CountBCTs == matrixconfusion[mc,"QuantidadeBCTsMath"]),]

    if(nrow(mcFilterLabels) > 0) {
      
      for(bc in 1:as.integer(matrixconfusion[mc,"QuantidadeBasicChanges"]))
      {
        for (lb in 1:nrow(mcFilterLabels)) 
        {
          for(bctsLb in 1:4) { # Todos os BCTs das Labels, nesse caso sao 4 (BCT1_Label, BCT2_Label, BCT3_Label, BCT4_Label)

            if(trimws(gsub(":global", "", as.character(matrixconfusion[mc,paste0("BCT", bc)]))) == 
               trimws(paste0(as.character(mcFilterLabels[lb,paste0("BCT", bctsLb, "_Label")]), ".0"))) {
              matrixMaths[mc,paste0("BCT", bctsLb, "_Math")] <- 1
              break()
            }
          }
        }
      }

      
      if(nrow(matrixMaths) > 0) {
               
        matrixMaths[mc, "BCT1_Math"] <- if (is.na(matrixMaths[mc, "BCT1_Math"])) 0 else as.integer(matrixMaths[mc, "BCT1_Math"]) 
        matrixMaths[mc, "BCT2_Math"] <- if (is.na(matrixMaths[mc, "BCT2_Math"])) 0 else as.integer(matrixMaths[mc, "BCT2_Math"]) 
        matrixMaths[mc, "BCT3_Math"] <- if (is.na(matrixMaths[mc, "BCT3_Math"])) 0 else as.integer(matrixMaths[mc, "BCT3_Math"]) 
        matrixMaths[mc, "BCT4_Math"] <- if (is.na(matrixMaths[mc, "BCT4_Math"])) 0 else as.integer(matrixMaths[mc, "BCT4_Math"]) 
  
        sumAllMatrixMath <- sum(matrixMaths[mc,1:4])
  
        if(sumAllMatrixMath >= matrixconfusion[mc,"QuantidadeBCTsMath"]) {
          matrixconfusion[mc,"VerdadeiroPositivo"] <- 1
        }
        else{
          matrixconfusion[mc,"FalsoPositivo"] <- 1
        }
      }
      else {
        matrixconfusion[mc,"FalsoPositivo"] <- 1
      }
    }
    else{
      matrixconfusion[mc,"FalsoPositivo"] <- 1
    }
  }
  
}

# =========================================
# ======== Obtendo os resultados ==========
# =========================================


matrixConfusionFinal <- data.frame(Precision=numeric(),
                                   Recall=numeric(),
                                   NPV=numeric(),
                                   Accuracy=numeric(),
                                   Fmeasure=numeric(),
                          stringsAsFactors=FALSE)


matrixMathsSum <- colSums(matrixconfusion[,15:18], na.rm = FALSE)

# Precision
matrixConfusionFinal[1, "Precision"] <- matrixMathsSum["VerdadeiroPositivo"] / (matrixMathsSum["VerdadeiroPositivo"] + matrixMathsSum["FalsoPositivo"])

# Recall
matrixConfusionFinal[1,"Recall"] <- matrixMathsSum["VerdadeiroPositivo"] / (matrixMathsSum["VerdadeiroPositivo"] + matrixMathsSum["FalsoNegativo"]) 

# Negative Predictive Value
matrixConfusionFinal[1,"NPV"] <- matrixMathsSum["VerdadeiroNegativo"] / (matrixMathsSum["VerdadeiroNegativo"] + matrixMathsSum["FalsoNegativo"])

# Accuracy
matrixConfusionFinal[1,"Accuracy"] <- (matrixMathsSum["VerdadeiroPositivo"] + matrixMathsSum["VerdadeiroNegativo"]) / ncol(matrixconfusion)

# F-measure
matrixConfusionFinal[1,"Fmeasure"] <- 2 * ((matrixConfusionFinal[1,"Recall"] * matrixConfusionFinal[1, "Precision"]) / (matrixConfusionFinal[1,"Recall"] + matrixConfusionFinal[1, "Precision"]))
