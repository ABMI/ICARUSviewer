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
#' @param connectionDetails
#' @param connection
#' @param CDMschema
#' @param cohortTable
#' @export
call_dataList<- function(connectionDetails,
                         connection,
                         Resultschema,
                         CDMschema,
                         cohortTable){
  
    connectionDetails <-connectionDetails
    connection <- connection

    Sys.setlocale(category="LC_CTYPE", locale="C")

    ###load demographic data
    sql <- SqlRender::readSql( paste0(.libPaths()[1],"/ICARUSviewer","/SQL/loadDemographic.sql") )
    sql <- SqlRender::render(sql = sql,
                             resultDatabaseSchema = Resultschema,
                             cdmDatabaseSchema = CDMschema,
                             cohortTable = cohortTable)
    demographic_data<-DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)

    ##load asthma_cohort data
    sql <- SqlRender::readSql( paste0(.libPaths()[1],"/ICARUSviewer","/SQL/loadAsthma_cohort.sql") )
    sql <- SqlRender::render(sql = sql,
                             resultDatabaseSchema = Resultschema,
                             cohortTable = cohortTable)
    asthmacohort_data<-DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
    
    result<-list(demographic_data,
                 asthmacohort_data)

    return(result)

}

