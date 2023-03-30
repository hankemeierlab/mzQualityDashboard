"assay<-" <- function(exp, name, value){
    SummarizedExperiment::`assay<-`(exp, name, value = value)
}

"assayNames<-" <- function(exp, value){
    SummarizedExperiment::`assayNames<-`(exp, value = value)
}

"assays<-" <- function(exp, name, value){
    SummarizedExperiment::`assays<-`(exp, name, value = value)
}

"rowData<-" <- function(exp, name, value){
    SummarizedExperiment::`rowData<-`(exp, name, value = value)
}

SummarizedExperiment <- function(assays = list(), rowData = data.frame(),
                                 colData = data.frame(), metadata = list()) {
    SummarizedExperiment::SummarizedExperiment(
        assays = assays, rowData = rowData,
        colData = colData, metadata = metadata
    )
}

assay <- function(x, i){
    SummarizedExperiment::assay(x = x, i = i)
}

assayNames <- function(x){
    SummarizedExperiment::assayNames(x)
}

assays <- function(x){
    SummarizedExperiment::assays(x)
}

colData <- function(x){
    SummarizedExperiment::colData(x)
}

rowData <- function(x){
    SummarizedExperiment::rowData(x)
}


"metadata<-" <- function(x, value){
    S4Vectors::`metadata<-`(x = x, value = value)
}

metadata <- function(x){
    S4Vectors::metadata(x)
}
