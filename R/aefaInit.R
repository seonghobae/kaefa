# init.R
aefaInit <- function(GCEvms = NULL, debug = F){

  options(future.debug = debug)

  if (!require('future')) {
    install.packages('future', repos = 'http://cloud.r-project.org')
  }
  require('future') # GET RID OFF WHEN PRODUCTION

  if (!require('mirt')) {
    install.packages('mirt', repos = 'http://cloud.r-project.org')
  }
  require('mirt') # GET RID OFF WHEN PRODUCTION

  if (!require('progress')) {
    install.packages('progress', repos = 'http://cloud.r-project.org')
  }
  require('progress') # GET RID OFF WHEN PRODUCTION

  if (!require('plyr')) {
    install.packages('plyr', repos = 'http://cloud.r-project.org')
  }
  require('plyr') # GET RID OFF WHEN PRODUCTION

  if (!require('psych')) {
    install.packages('psych', repos = 'http://cloud.r-project.org')
  }
  require('psych') # GET RID OFF WHEN PRODUCTION

  # setting up cluster
  if (!is.null(GCEvms)) {
    parallel::mclapply(future::plan(list(
      future::tweak(future::cluster, workers = future::as.cluster(GCEvms)),
      future::multiprocess
    )), mc.cores = length(GCEvms))
  } else if (NROW(future::plan("list")) == 1) {
    if (length(grep('openblas', extSoftVersion()["BLAS"])) > 0) {
      future::plan(future::multiprocess)
      # mirt::mirtCluster()
    } else if (length(future::availableWorkers()) == 1) {
      future::plan(future::sequential)
    } else {
      suppressMessages(try(future::plan(strategy = list(
        future::tweak(future::cluster), future::multiprocess
      )), silent = T))
    }

  }
}
