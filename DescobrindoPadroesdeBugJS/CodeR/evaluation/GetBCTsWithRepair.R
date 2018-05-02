getBCTsWithRepair <- function () {
    # ===========
    # Obter todos os conjuntos basicos de mudanca
    # ===========
    # Leitura de CSV
    # CSV com todos os Reparos classificados pelos Hanan, com os seus repectivos BCTs
    conjuntosBCTs <- read.csv(file="original dataset/tablecct.csv", header=TRUE, sep=";")

    # Obtendo cada BCT dentro das assinaturas do conjunto de mudancas basicas de cada correcao
    library(stringr)
    quebraBcts <- as.data.frame(str_split(conjuntosBCTs$Signature, "<br>", simplify = TRUE))

    # Renomear as colunas 
    colnames(quebraBcts) <- c("BCT1_Label", "BCT2_Label", "BCT3_Label", "BCT4_Label")

    # Atribuindo os IDs, Media de Modficacoes e a quantidade de BCTs da correcao na matriz com os BCTs por coluna
    quebraBcts$IdCCT <- conjuntosBCTs$ID
    quebraBcts$AvgModifiedStatements <- conjuntosBCTs$Avg.Modified.Statements
    quebraBcts$CountBCTs <- conjuntosBCTs$BCTs


    # ===========
    # Obter o dataset com o Grupo de Padroes de Defeitos
    # ===========
    padroesDefeitos <- read.csv(file="original dataset/tablebugpatterns.csv", header=TRUE, sep=";")
    padroesDefeitos$Change.Types.Groups.List <- str_split(padroesDefeitos$Change.Types.Groups, ",")

    PadroesDefeitosPorConjutoBCTs <- data.frame(IDBugPatterns=integer(),
                                                Fault=character(),
                                                Repair=character(),
                                                BCT1_Label=character(),
                                                BCT2_Label=character(),
                                                BCT3_Label=character(),
                                                BCT4_Label=character(),
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

    names(PadroesDefeitosPorConjutoBCTs) <- c("IDRepair", names(PadroesDefeitosPorConjutoBCTs[2:ncol(PadroesDefeitosPorConjutoBCTs)]))

    return(PadroesDefeitosPorConjutoBCTs)
}