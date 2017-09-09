# init.R

#' aefaInit
#'
#' @param GCEvms insert google computing engine virtual machine information.
#' @param debug run with debug mode. default is FALSE
#'
#' @return
#' @export
#'
#' @examples
aefaInit <- function(GCEvms = NULL, debug = F) {
  options(future.debug = debug)

  # setting up cluster
  if (!is.null(GCEvms)) {
    parallel::mclapply(future::plan(list(
      future::tweak(future::cluster, workers = future::as.cluster(GCEvms)),
      future::multiprocess
    )), mc.cores = length(GCEvms))
  } else if (NROW(future::plan("list")) == 1) {
    if (length(grep('openblas', extSoftVersion()["BLAS"])) > 0) {
      future::plan(future::multiprocess)
    } else if (length(future::availableWorkers()) == 1) {
      future::plan(future::sequential)
    } else {
      suppressMessages(try(future::plan(strategy = list(
        future::tweak(future::cluster), future::multiprocess
      )), silent = T)
      )
    }

  }
}
