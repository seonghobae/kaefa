# init.R

#' Initalize aefa engine,
#' This function initalise the aefa cluster.
#' If someone have Google Computing Engine informaiton, put the information in the argument.
#' @param GCEvms insert google computing engine virtual machine information.
#' @param debug run with debug mode. default is FALSE
#'
#' @return nothing to return, just hidden variable to set to run parallelism.
#' @export
#'
#' @examples
#' \dontrun{
#' .conn <- aefaInit()
#' 
#'}
aefaInit <- function(GCEvms = NULL, debug = F) {
    options(future.debug = debug)
    
    # setting up cluster
    if (!is.null(GCEvms)) {
        parallel::mclapply(future::plan(list(conn <- future::tweak(future::cluster, workers = future::as.cluster(GCEvms)), 
            future::multiprocess)), mc.cores = length(GCEvms))
    } else if (NROW(future::plan("list")) == 1) {
        if (length(grep("openblas", extSoftVersion()["BLAS"])) > 0) {
            conn <- future::plan(future::multiprocess)
        } else if (length(future::availableWorkers()) == 1) {
            conn <- future::plan(future::sequential)
        } else {
            conn <- suppressMessages(try(future::plan(strategy = list(future::tweak(future::cluster), 
                future::multiprocess)), silent = T))
        }
        
    }
    
    return(conn)
    
}
