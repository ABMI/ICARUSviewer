#' code for reading query and calling the data (using SqlRender)
#' @param sqlquery
#' @param Resultschema
#' @param CDMschema
#' @param connectionDetails
#' @param connection
#' @export

sql_reader <- function(sqlquery,
                       Resultschema,
                       CDMschema,
                       connectionDetails,
                       connection){

    resultDatabaseSchema <- paste0(Resultschema,".dbo")
    CDMDatabaseSchema <- paste0(CDMschema,".dbo")
    connectionDetails <-connectionDetails
    connection <- connection

    Sys.setlocale(category="LC_CTYPE", locale="C")

    ###load demographic data
    sql <- SqlRender::readSql(sqlquery)
    sql <- SqlRender::renderSql(sql,
                                resultDatabaseSchema = resultDatabaseSchema)$sql
    out<-DatabaseConnector::querySql(connection, sql)
    colnames(out)<-SqlRender::snakeCaseToCamelCase(colnames(out))

    return(out)
}

#' code for loading data
#' @import SqlRender
#' @import DatabaseConnector
#' @param Resultschema
#' @param CDMschema
#' @param connectionDetails
#' @param connection
#' @export
call_dataList<- function(connectionDetails,
                         connection,
                         Resultschema,
                         CDMschema){

    resultDatabaseSchema <- paste0(Resultschema,".dbo")
    CDMDatabaseSchema <- paste0(CDMschema,".dbo")
    connectionDetails <-connectionDetails
    connection <- connection

    Sys.setlocale(category="LC_CTYPE", locale="C")

    ###load demographic data
    sql <- SqlRender::readSql("SQL/loadDemographic.sql")
    sql <- SqlRender::renderSql(sql,
                                resultDatabaseSchema = resultDatabaseSchema)$sql
    demographic_data<-DatabaseConnector::querySql(connection, sql)
    colnames(demographic_data)<-SqlRender::snakeCaseToCamelCase(colnames(demographic_data))

    ###load PFT longitudinal data
    sql <- SqlRender::readSql("SQL/loadAsthma_Measure.sql")
    sql <- SqlRender::renderSql(sql,
                                resultDatabaseSchema = resultDatabaseSchema)$sql
    measure_data<-DatabaseConnector::querySql(connection, sql)
    colnames(measure_data)<-SqlRender::snakeCaseToCamelCase(colnames(measure_data))

    ##load comorbidity data
    sql <- SqlRender::readSql("SQL/loadComorbidity.sql")
    sql <- SqlRender::renderSql(sql,
                                resultDatabaseSchema = resultDatabaseSchema)$sql
    comorbidity_data<-DatabaseConnector::querySql(connection, sql)
    colnames(comorbidity_data)<-SqlRender::snakeCaseToCamelCase(colnames(comorbidity_data))

    ##load exacerbation data
    sql <- SqlRender::readSql("SQL/loadExacerbation.sql")
    sql <- SqlRender::renderSql(sql,
                                resultDatabaseSchema = resultDatabaseSchema)$sql
    exacerbatuib_data<-DatabaseConnector::querySql(connection, sql)
    colnames(exacerbatuib_data)<-SqlRender::snakeCaseToCamelCase(colnames(exacerbatuib_data))

    result<-list(demographic_data,
                 measure_data,
                 comorbidity_data,
                 exacerbatuib_data)

    return(result)

}

