
setwd("G:/Mestrado/Meus experimentos/Meu GitHub/ExperimentsPapers/DescobrindoPadroesdeBugJS/CodeR/evaluation")

# ===========
# Obter todos os conjuntos b�sicos de mudan�a
# ===========
# Leitura de CSV
# CSV com todos os Reparos classificados pelos Hanan, com os seus repectivos BCTs
conjuntosBCTs <- read.csv(file="original dataset/tablecct.csv", header=TRUE, sep=";")

# Obtendo cada BCT dentro das assinaturas do conjunto de mudan�as b�sicas de cada corre��o
library(stringr)
quebraBcts <- as.data.frame(str_split(conjuntosBCTs$Signature, "<br>", simplify = TRUE))

# Renomear as colunas 
colnames(quebraBcts) <- c("bct1", "bct2", "bct3", "bct4")

# Atribuindo os IDs, M�dia de Modfica��es e a quantidade de BCTs da corre��o na matriz com os BCTs por coluna
quebraBcts$IdCCT <- conjuntosBCTs$ID
quebraBcts$Avg.Modified.Statements <- conjuntosBCTs$Avg.Modified.Statements
quebraBcts$CountBCTs <- conjuntosBCTs$BCTs

# ===========

# ===========
# Obter o dataset com o Grupo de Padr�es de Defeitos
# ===========
padroesDefeitos <- read.csv(file="original dataset/tablebugpatterns.csv", header=TRUE, sep=";")
padroesDefeitos$Change.Types.Groups.List <- str_split(padroesDefeitos$Change.Types.Groups, ",")

PadroesDefeitosPorConjutoBCTs <- data.frame(IDBugPatterns=integer(),
                 Fault=character(),
                 Repair=character(),
                 Bct1=character(),
                 Bct2=character(),
                 Bct3=character(),
                 Bct4=character(),
                 IdCCT = integer(),
                 AvgModifiedStatements = integer(),
                 CountBCTs = integer(),
                 stringsAsFactors=FALSE)


idChangeType <- integer(0L)

for(i in 1:dim(padroesDefeitos)[1]) {
  for (j in 1:length(padroesDefeitos[i,]$Change.Types.Groups.List[[1]])) {
    idChangeType <- as.integer(padroesDefeitos[i,]$Change.Types.Groups.List[[1]][j])
    
    PadroesDefeitosPorConjutoBCTs <- 
      rbind(PadroesDefeitosPorConjutoBCTs, as.data.frame(c(padroesDefeitos[i,c(1:3)],
                              quebraBcts[quebraBcts$IdCCT==idChangeType, ])))
  }
}

# ===========
# Obter o dataset de classifica��es feita pelo estudo do Hanan (BUgAID)
# ===========

languageconstruct <- read.csv(file="original dataset/language_construct_database.csv", 
                                      header=TRUE, sep=";")

names(languageconstruct)[1] <- "IDCommit"

# ===========
# Obter o dataset que o Hanan informou ter as informa��es de {Reparo, Label}
# ===========

datasetLabelReparoCommit <- read.csv(file="original dataset/oracle.csv", 
                                     header=TRUE, sep=";")
names(datasetLabelReparoCommit)

# ===========
# Realizando um Join entre os tr�s datasets 
# ===========

library(dplyr)

datasetOracleCompleted <- (languageconstruct %>% 
                          inner_join(datasetLabelReparoCommit, by = c("IDCommit" = "IDCommit"))) %>%
                          inner_join(padroesDefeitos[1:3], by = c("IDRepair" = "ID"))

# ===========
# Desenvolver a implementa��o para calcular os BCTs gerados errados, com base no que estava sendo 
# esperado no ID Repair 
# ===========


