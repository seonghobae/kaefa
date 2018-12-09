# kaefa.R

#' Initalize aefa engine,
#' This function initalise the aefa cluster.
#' If someone have Remote Cluster informaiton with SSH, put the information in the argument.
#'
#' @import NCmisc
#' @import future
#' @import progress
#' @param RemoteClusters insert google computing engine virtual machine information. If you want to use MPI address, please insert addresses here.
#' @param debug run with debug mode. default is FALSE
#' @param sshKeyPath provide the SSH key path, NA is the placeholder.
#'
#' @return nothing to return, just hidden variable to set to run parallelism.
#' @export
#'
#' @examples
#' \dontrun{
#' # remote cluster with key -- actually used in aefa() function.
#' aefaInit(RemoteClusters = c('localhost', 's1', 's2'), sshKeyPath = c(NA, '~/pub.pem', NA))
#'
#'}
aefaInit <- function(RemoteClusters = getOption("kaefaServers"), debug = F, sshKeyPath = NULL) {
    # options(future.debug = debug)

    assignClusterNodes <- function(serverList, loadPercentage = 50, freeRamPercentage = 30,
        requiredMinimumClusters = max(c(1, round(sqrt(NROW(serverList))))), sshKeyPath = NULL) {


        STOP <- F
        while (!STOP) {
          pb <- progress::progress_bar$new(
            format = " initialising [:bar] :percent eta: :eta",
            total = length(serverList), clear = F, width = 300)
          # invisible(gc())
            statusList <- list()
            decisionList <- list()
            for (i in serverList) {
              suppressWarnings(pb$tick())
                if (i == "localhost") {
                  # localhost side
                  statusList$localhost <- tryCatch(system(paste("uptime | awk '{print $8}' &&",
                    "cat /proc/cpuinfo | grep processor | wc -l &&", "free | grep Mem | awk '{print $4/$2 * 100}'"),
                    intern = TRUE), error = function(e) {
                  })  # CentOS
                  if (length(grep("load", statusList[[i]][1])) > 0 | length(grep("average",
                    statusList[[i]][1])) > 0) {
                    Sys.sleep(10)
                    statusList$localhost <- tryCatch(system(paste("uptime | awk '{print $11}' &&",
                      "cat /proc/cpuinfo | grep processor | wc -l &&", "free | grep Mem | awk '{print $4/$2 * 100}'"),
                      intern = TRUE), error = function(e) {
                    })  # Ubuntu
                  }
                } else {
                  # SSH side if key is provided
                  if (!is.null(sshKeyPath)) {
                    for (jj in 1:length(serverList)) {
                      if (names(serverList)[[jj]] %in% names(serverList) && (length(grep(c("pem"),
                        sshKeyPath[[jj]])) > 0 | length(grep(c("key"), sshKeyPath[[jj]])) >
                        0)) {
                        statusList[[i]] <- tryCatch(system(paste("ssh", i, "-i",
                          sshKeyPath[[jj]], "uptime | awk '{print $8}' &&", "ssh",
                          i, "-i", jj, "cat /proc/cpuinfo | grep processor | wc -l &&",
                          "ssh", i, "-i", jj, "free | grep Mem | awk '{print $4/$2 * 100}'"),
                          intern = TRUE), error = function(e) {
                        })  # CentOS
                        if (length(grep("load", statusList[[i]][1])) > 0 | length(grep("average",
                          statusList[[i]][1])) > 0) {
                          Sys.sleep(10)

                          statusList[[i]] <- tryCatch(system(paste("ssh", i, "-i",
                            sshKeyPath[[jj]], "uptime | awk '{print $11}' &&", "ssh",
                            i, "-i", jj, "cat /proc/cpuinfo | grep processor | wc -l &&",
                            "ssh", i, "-i", jj, "free | grep Mem | awk '{print $4/$2 * 100}'"),
                            intern = TRUE), error = function(e) {
                          })  # Ubuntu
                        }

                      } else {
                        statusList[[i]] <- tryCatch(system(paste("ssh", i, "uptime | awk '{print $8}' &&",
                          "ssh", i, "cat /proc/cpuinfo | grep processor | wc -l &&",
                          "ssh", i, "free | grep Mem | awk '{print $4/$2 * 100}'"),
                          intern = TRUE), error = function(e) {
                        })  # CentOS
                        if (length(grep("load", statusList[[i]][1])) > 0 | length(grep("average",
                          statusList[[i]][1])) > 0) {
                          Sys.sleep(10)

                          statusList[[i]] <- tryCatch(system(paste("ssh", i, "uptime | awk '{print $11}' &&",
                            "ssh", i, "cat /proc/cpuinfo | grep processor | wc -l &&",
                            "ssh", i, "free | grep Mem | awk '{print $4/$2 * 100}'"),
                            intern = TRUE), error = function(e) {
                          })  # Ubuntu
                        }
                      }
                    }
                  } else {
                    statusList[[i]] <- tryCatch(system(paste("ssh", i, "uptime | awk '{print $8}' &&",
                      "ssh", i, "cat /proc/cpuinfo | grep processor | wc -l &&",
                      "ssh", i, "free | grep Mem | awk '{print $4/$2 * 100}'"), intern = TRUE),
                      error = function(e) {
                      })  # CentOS
                    if (length(grep("load", statusList[[i]][1])) > 0 | length(grep("average",
                      statusList[[i]][1])) > 0) {
                      Sys.sleep(5)
                      statusList[[i]] <- tryCatch(system(paste("ssh", i, "uptime | awk '{print $11}' &&",
                        "ssh", i, "cat /proc/cpuinfo | grep processor | wc -l &&",
                        "ssh", i, "free | grep Mem | awk '{print $4/$2 * 100}'"),
                        intern = TRUE), error = function(e) {
                      })  # Ubuntu
                    }
                  }
                }
                # evaluation
                statusList[[i]][1] <- gsub(",", "", statusList[[i]][1])
                decisionList[[i]] <- tryCatch(as.numeric(statusList[[i]][1])/
                                                as.numeric(statusList[[i]][2]) * 100 < loadPercentage
                  ,
                  error = function(e) {
                  })
            }
            availableCluster <- names(decisionList)[which(unlist(decisionList))]

            if (requiredMinimumClusters > length(availableCluster)) {
              ## Elapsed time
              pb <- progress_bar$new(
                format = "  Wait for 100 seconds for connection stabilise [:bar] :percent in :elapsed",
                total = 100, clear = F, width = 300)
              for (i in 1:100) {
                suppressWarnings(pb$tick())
                Sys.sleep(1)
              }
            } else {
                nCores <- 0
                for (jj in which(unlist(decisionList))) {
                  nCores <- nCores + as.numeric(statusList[[jj]][2])
                }
                decisionTable <- (cbind.data.frame(statusList, stringsAsFactors =F)[which(unlist(decisionList))])
                print(decisionTable)

                servNames <- as.character(colnames(decisionTable))
                servThreads <- as.numeric((decisionTable[2,]))

                connList <- vector()

                for(i in 1:length(servNames)){
                  maxP <- round((servThreads[i]*.7*.5))
                  if(maxP >= 4){
                    maxP <- round(maxP*.7) # add attunation factor for a high-performance computing machine
                  }

                  if(maxP >= 6){
                    maxP <- round(maxP*.7) # add attunation factor for a high-performance computing machine
                  }
                  connList <- c(connList, rep(servNames[i], max(c(1,maxP))))
                }
                connList <- as.character(connList)

                message("get ", length(availableCluster)," clusters")
                STOP <- T
            }
        }
        # connList <- connList[sample(x = 1:length(connList), size = length(connList))]
        return(availableCluster)
    }



    if (is.null(suppressWarnings(NCmisc::top()$CPU$idle))) {
        parallelProcessors <- round(parallel::detectCores(all.tests = FALSE, logical = FALSE)/2)
        if (2 >= parallelProcessors) {
            parallelProcessors <- 2
        }
    } else if (suppressWarnings(NCmisc::top()$CPU$idle) > 50) {
        parallelProcessors <- round(parallel::detectCores(all.tests = FALSE, logical = FALSE))
        if (2 >= parallelProcessors) {
            parallelProcessors <- 2
        }
    } else if (suppressWarnings(NCmisc::top()$CPU$idle) <= 50) {
        parallelProcessors <- round(parallel::detectCores(all.tests = FALSE, logical = FALSE)/2)
        if (2 >= parallelProcessors) {
            parallelProcessors <- 2
        }
    } else if (suppressWarnings(NCmisc::top()$CPU$idle) < 30) {
        parallelProcessors <- round(parallel::detectCores(all.tests = FALSE, logical = FALSE)/3)
        if (2 >= parallelProcessors) {
            parallelProcessors <- 2
        }
    } else if (suppressWarnings(NCmisc::top()$CPU$idle) < 10) {
        parallelProcessors <- 2
    }

    # setting up cluster
    if (!is.null(RemoteClusters)) {
      halfCores <- function() { max(1, round(0.5 * future::availableCores()))}
      try(future::plan(list(
        future::tweak(future::cluster, workers = assignClusterNodes(RemoteClusters), gc = T),
        future::tweak(future::multiprocess, workers = halfCores, gc = T)
      )))
        # try(future::plan(future::tweak("future::cluster",
        #                                     workers = tryCatch(assignClusterNodes(RemoteClusters), error = function(e){assignClusterNodes(RemoteClusters)}), gc = T
        #                  )))
    } else if (NROW(future::plan("list")) == 1) {
        if (length(grep("openblas|microsoft", extSoftVersion()["BLAS"])) > 0) {
            options(aefaConn = future::plan("future::multiprocess", workers = parallelProcessors),
                gc = T)
        } else if (parallel::detectCores(logical = F) == 1) {
            options(aefaConn = future::plan(future::sequential), gc = T)
        } else {
            options(aefaConn = (tryCatch(future::plan(strategy = list(future::tweak(future::cluster(workers = parallelProcessors)),
                future::multiprocess(workers = parallelProcessors)), gc = T), error = function(e) {
            })))
        }
    }
}

#' assessment of fit indices of the calibrated model
#'
#' @param mirtModel insert estimated \code{mirt::mirt} or \code{mirt::mixedmirt} model.
#' @param RemoteClusters insert google computing engine virtual machine information.
#' @param rotate set the rotate critera if mirt model is exploratory model. default is bifactorQ
#' @param PV_Q1 Do you want to get PV_Q1 (Chalmers & Ng, 2017) if can get it? default is TRUE.
#' @param S_X2 Do you want to get S_X2 if can get it? default is TRUE.
#'
#' @return return item fit estimates
#' @export
#'
#' @examples
#' \dontrun{
#' testModel1 <- engineAEFA(mirt::Science)
#' testItemFit1 <- evaluateItemFit(testModel1)
#' }
evaluateItemFit <- function(mirtModel, RemoteClusters = NULL, rotate = "bifactorQ",
    PV_Q1 = T, S_X2 = T) {
    # if (is.null(getOption('aefaConn'))) { getOption('aefaConn',
    # aefaInit(RemoteClusters = RemoteClusters, debug = F)) }

    options(future.globals.maxSize = 500 * 1024^3)
    if (class(mirtModel) == "aefa") {
        mirtModel <- mirtModel$estModelTrials[[NROW(mirtModel$estModelTrials)]]
    }

    # convert mixedclass to singleclass temporary
    if (class(mirtModel)[1] == "MixedClass") {
      mirtModel <- .exportParmsEME(mirtModel)
    }

    if (attr(class(mirtModel), "package") == "mirt") {
        # item fit evaluation

        if(sum('lca' %in% mirtModel@Model$itemtype) == 0){
          modFit_Zh <- listenv()
          modFit_Zh %<-% suppressWarnings(tryCatch(mirt::itemfit(mirtModel, rotate = rotate,
                                                                 fit_stats = "Zh", QMC = T, method = if(mirtModel@Model$nfact == 1) 'EAP' else 'MAP', impute = if (sum(is.na(mirtModel@Data$data)) >
                                                                                                                         0)
                                                                   100 else 0), error = function(e) {NULL}))
        }

        if(S_X2){
          modFit_SX2 <- listenv()
          modFit_SX2 %<-% suppressWarnings(tryCatch(mirt::itemfit(mirtModel, rotate = rotate,
                                                                  fit_stats = "S_X2", QMC = T, method = if(mirtModel@Model$nfact == 1) 'EAP' else 'MAP', impute = if (sum(is.na(mirtModel@Data$data)) >
                                                                                                                            0)
                                                                    100 else 0), error = function(e) {NULL}))
        }


        if (mirtModel@Model$nfact == 1 && PV_Q1 && sum('lca' %in% mirtModel@Model$itemtype) != 0) {
            modFit_PVQ1 <- listenv()
            modFit_PVQ1 %<-% suppressWarnings(tryCatch(mirt::itemfit(mirtModel, rotate = rotate,
                fit_stats = "PV_Q1*", QMC = T, method = if(mirtModel@Model$nfact == 1) 'EAP' else 'MAP'), error = function(e) {NULL}))

        }

        if (sum(mirtModel@Model$itemtype %in% "Rasch") > 0 && mirtModel@Model$nfact ==
            1) {
            modFit_infit <- listenv()
            modFit_infit %<-% tryCatch(mirt::itemfit(mirtModel, rotate = rotate,
                fit_stats = "infit", QMC = T, method = "WLE", impute = if (sum(is.na(mirtModel@Data$data)) >
                  0)
                  100 else 0), error = function(e) {NULL})
        }

        # check item fit indices are exists

        if (exists("modFit_Zh")) {
            if (!class(modFit_Zh)[1] == "mirt_df") {
                rm(modFit_Zh)
            } else {
              modFit_Zh <- plyr::rbind.fill(modFit_Zh)
            }
        }

        if (exists("modFit_SX2")) {
            if (!class(modFit_SX2)[1] == "mirt_df") {
                rm(modFit_SX2)
            } else {
              modFit_SX2 <- plyr::rbind.fill(modFit_SX2)
            }
        }

        if (exists("modFit_PVQ1")) {
            if (!class(modFit_PVQ1)[1] == "mirt_df") {
                rm(modFit_PVQ1)
            } else {
              modFit_PVQ1 <- plyr::rbind.fill(modFit_PVQ1)
            }
        }

        if (exists("modFit_infit")) {
            if (!class(modFit_infit)[1] == "mirt_df") {
                rm(modFit_infit)
            } else {
              modFit_infit <- plyr::rbind.fill(modFit_infit)
            }
        }

        itemFitList <- c("modFit_Zh", "modFit_SX2", "modFit_PVQ1", "modFit_infit")[c(exists("modFit_Zh"),
            exists("modFit_SX2"), exists("modFit_PVQ1"), exists("modFit_infit"))]

        if(length(itemFitList) != 0){

          fitList <- list()
          for (i in 1:length(itemFitList)) {
            fitList[[i]] <- (eval(parse(text = itemFitList[i])))
          }
          itemfitList <- invisible(suppressWarnings(suppressMessages(plyr::join_all(fitList))))
          itemfitList <- itemfitList[itemfitList$item %in% colnames(mirtModel@Data$data),]
          return(itemfitList)
        } else {
          itemfitList <- plyr::rbind.fill(mirt::itemfit(mirtModel, impute = if (sum(is.na(mirtModel@Data$data)) > 0) 100 else 0))
          itemfitList <- itemfitList[itemfitList$item %in% colnames(mirtModel@Data$data),]
          return(itemfitList)
        }

    } else {
        message("That's seems not MIRT model, so that trying to estimate new model with default settings")
        estModel <- engineAEFA(data = mirtModel)
        return(estModel)
    }
}

#' doing automated exploratory factor analysis (aefa) for research capability to identify unexplained factor structure with complexly cross-classified multilevel structured data in R environment
#' @param data insert \code{data.frame} object.
#' @param model specify the mirt model if you have want to calibrate. default is NULL to run exploratory models, but accepting \code{mirt::mirt.model} object.
#' @param minExtraction specify the minimum number of factors to calibrate. defaults is 1 but can change this. if model is not NULL, aefa will ignoring this.
#' @param maxExtraction specify the maximum number of factors to calibrate. defaults is 10 but can change this. if model is not NULL, aefa will ignoring this.
#' @param RemoteClusters insert google computing engine virtual machine information.
#' @param sshKeyPath provide the SSH key path, NA is the placeholder.
#'
#' @param GenRandomPars Try to generate Random Parameters? Default is TRUE
#' @param NCYCLES N Cycles of Robbin Monroe stage (stage 3). Default is 4000.
#' @param BURNIN N Cycles of Metro-hastings burnin stage (stage 1). Default is 1500.
#' @param SEMCYCLES N Cycles of Metro-hastings burnin stage (stage 2). Default is 1000.
#' @param covdata insert covariate data frame where use to fixed and random effect term. if not inserted, ignoring fixed and random effect estimation.
#' @param fixed a right sided R formula for specifying the fixed effect (aka 'explanatory') predictors from covdata and itemdesign.
#' @param random a right sided formula or list of formulas containing crossed random effects of the form \code{v1 + ... v_n | G}, where \code{G} is the grouping variable and \code{v_n} are random numeric predictors within each group. G may contain interaction terms, such as group:items to include cross or person-level interactions effects.
#' @param key item key vector of multiple choices test.
#' @param accelerate a character vector indicating the type of acceleration to use. Default is  'squarem' for the SQUAREM procedure (specifically, the gSqS3 approach)
#' @param symmetric force S-EM/Oakes information matrix to be symmetric? Default is FALSE to detect solutions that have not reached the ML estimate.
#'
#' @param saveModelHistory Will you save the model calibration historys? default is TRUE
#' @param filename If you want to save the model calibration historys, Which one want you save file name? default is aefa.RDS, However can change this.
#' @param printItemFit Will you printing item fit indices during the calibrations? default is TRUE.
#' @param rotate set the rotate critera if mirt model is exploratory model. default is bifactorQ, however you can change this what you want to, like 'geominQ', 'bifactorT', 'geominT'. In current, Target rotation not supporting.
#'
#' @param resampling Do you want to do resampling with replace? default is TRUE and activate nrow is over samples argument.
#' @param samples Specify the number samples with resampling. default is 5000.
#' @param printDebugMsg Do you want to see the debugging messeages? default is FALSE
#' @param modelSelectionCriteria Which critera want to use model selection work? 'DIC' (default), 'AIC', 'AICc', 'BIC', 'saBIC' available. AIC and DIC will be identical when no prior parameter distributions are included.
#' @param saveRawEstModels Do you want to save raw estimated models before model selection work? default is FALSE
#' @param fitEMatUIRT Do you want to fit the model with EM at UIRT? default is FALSE
#' @param ranefautocomb Do you want to find global-optimal random effect combination? default is TRUE
#' @param PV_Q1 Do you want to get PV_Q1 (Chalmers & Ng, 2017) if can get it? default is TRUE.
#' @param tryLCA Do you want to try calibrate LCA model if avaliable? default is TRUE
#' @param forcingQMC Do you want to forcing the use QMC estimation instead MHRM? default is FALSE
#' @param turnOffMixedEst Do you want to turn off mixed effect (multilevel) estimation? default is FALSE
#' @param fitIndicesCutOff Specify item assessment cutoff. default is p < .005
#' @param anchor Set the anchor item names If you want to consider DIF detection. default is NULL.
#' @importFrom stats qnorm
#' @importFrom stats sd
#'
#' @return automated exploratory factor analytic models
#' @export
#'
#' @aliases efa
#'
#' @examples
#' \dontrun{
#' testMod1 <- aefa(mirt::Science, minExtraction = 1, maxExtraction = 2)
#'
#' }
aefa <- efa <- function(data, model = NULL, minExtraction = 1, maxExtraction = if (is.data.frame(data)) if (ncol(data) <=
    10) max(c(1, (round(sqrt(ncol(data)))+2))) else max(c(1, round(sqrt(ncol(data))))) else if (class(data) %in% c("SingleGroupClass", "MixedClass",
    "DiscreteClass", "MultipleGroupClass")) max(c(1, round(sqrt(ncol(data@Data$data))))) else if (class(data) %in% "aefa") max(c(1, round(sqrt(ncol(data$estModelTrials[[NROW(data$estModelTrials)]]@Data$data))))) else stop("Please provide data correctly."),
    RemoteClusters = getOption("kaefaServers"), sshKeyPath = NULL, GenRandomPars = T, NCYCLES = 4000,
    BURNIN = 1500, SEMCYCLES = 1000, covdata = NULL,
    fixed = kaefa:::.covdataFixedEffectComb(covdata),
    random = lapply(c(c(kaefa:::.covdataClassifieder(covdata)$random, 'items'),
                      if(length(kaefa:::.covdataClassifieder(covdata)$random) != 0) paste0(kaefa:::.covdataClassifieder(covdata)$random, ':items')),
                    FUN = function(X){eval(parse(text = paste0('as.formula(',paste0('~1|',X), ')')))}),
    key = NULL, accelerate = "squarem", symmetric = F, saveModelHistory = T,
    filename = "aefa.RDS", printItemFit = T, rotate = c("bifactorQ","geominQ", "geominT", "bentlerQ", "bentlerT",
                                                        "oblimin", "oblimax", "simplimax", "tandemII",
                                                        "tandemI", "entropy", "quartimax"), resampling = T, samples = 5000,
    printDebugMsg = F, modelSelectionCriteria = "DIC", saveRawEstModels = F, fitEMatUIRT = F,
    ranefautocomb = T, PV_Q1 = T, tryLCA = F, forcingQMC = F, turnOffMixedEst = F,
    fitIndicesCutOff = 0.005, anchor = colnames(data)) {

  workDirectory <- getwd()
  message(paste0('work directory: ', workDirectory))
  if(length(.covdataClassifieder(covdata)$fixed) != 0){
    if(sum(class(covdata) %in% 'tbl_df') != 0){
      covdata <- as.data.frame(covdata)
    }
    for(i in which(colnames(covdata) %in% .covdataClassifieder(covdata)$fixed)){
      covdata[,i] <- as.factor(covdata[,i])
    }
  }

    TimeStart <- Sys.time()
    options(future.globals.maxSize = 500 * 1024^3)

    # prepare for bad item detection
    badItemNames <- vector()  # make new null vector
    DIFitems <- vector() # DIF in anchor items
    checkDIF <- TRUE

    # prepare for save model history
    modelHistoryCount <- 0
    if (saveModelHistory) {
        if (saveRawEstModels) {
            modelHistory <- list(rawEstModels = list(), estModelTrials = list(),
                itemFitTrials = list(), rotationTrials = list())
        } else {
            modelHistory <- list(estModelTrials = list(), itemFitTrials = list(),
                rotationTrials = list())
        }
    }

    calibModel <- as.list(maxExtraction:minExtraction)
    if (!is.null(model)) {
        # user specified EFA or CFA
        j <- maxExtraction
        model <- unlist(list(model))
        for (i in 1:NROW(model)) {
            if (class(model[[i]]) == "mirt.model" | class(model[[i]]) == "numeric") {
                calibModel[[j + i]] <- tryCatch(model[[i]], error = function(e) {NULL})
            }
        }
    }

    model <- calibModel

    # prepare for do aefa work
    STOP <- F

    while (!STOP) {
      # invisible(gc())
      if(workDirectory != getwd()){
        setwd('/tmp')
        setwd(workDirectory)
      }

        # estimate run exploratory IRT and confirmatory IRT
        if ((is.data.frame(data) | is.matrix(data))) {
            if (exists("estModel")) {
                tryCatch(rm(estModel), error = function(e) {NULL})
            }
            modelDONE <- FALSE
            while (!modelDONE) {
              if(workDirectory != getwd()){
                setwd('/tmp')
                setwd(workDirectory)
              }
              tryCatch(aefaInit(RemoteClusters = RemoteClusters, debug = printDebugMsg,
                  sshKeyPath = sshKeyPath), error = function(e) {NULL})
                # general condition
                estModel <- tryCatch(engineAEFA(data = data.frame(data[, !colnames(data) %in%
                  badItemNames]), model = model, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES,
                  BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, covdata = covdata, fixed = fixed,
                  random = random, key = key, accelerate = accelerate, symmetric = symmetric,
                  resampling = resampling, samples = samples, printDebugMsg = printDebugMsg,
                  fitEMatUIRT = fitEMatUIRT, ranefautocomb = ranefautocomb, tryLCA = tryLCA,
                  forcingQMC = forcingQMC, turnOffMixedEst = turnOffMixedEst, anchor = anchor[!anchor %in% DIFitems]), error = function(e) {
                })
                if (exists("estModel")) {
                  modelDONE <- TRUE
                }
            }

        } else if (is.list(data) && !is.data.frame(data)) {
            # Some weird condition: user specified pre-calibrated model or list of data.frame
            # in data

            estModel <- list()
            for (i in 1:NROW(data)) {
                if (sum(c("MixedClass", "SingleGroupClass", "DiscreteClass", "MultipleGroupClass") %in%
                  class(data[[i]])) != 0) {
                  # then first, searching pre-calibrated model in data argument
                  estModel[[NROW(estModel) + 1]] <- data[[i]]
                  if (!modelFound) {
                    # set modelFound flag
                    modelFound <- T
                  }
                } else if (is.data.frame(data[[i]]) | is.matrix(data[[i]])) {
                  # if list contains dataframe, try to estimate them anyway; even this behaviour
                  # seems weird
                  estModel[[NROW(estModel) + 1]] <- tryCatch(engineAEFA(data = data.frame(data[[i]][,
                    !colnames(data[[i]]) %in% badItemNames]), model = model, GenRandomPars = GenRandomPars,
                    NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, covdata = covdata,
                    fixed = fixed, random = random, key = key, accelerate = accelerate,
                    symmetric = symmetric, resampling = resampling, samples = samples,
                    printDebugMsg = printDebugMsg, fitEMatUIRT = fitEMatUIRT, ranefautocomb = ranefautocomb,
                    tryLCA = tryLCA, forcingQMC = forcingQMC, turnOffMixedEst = turnOffMixedEst, anchor = anchor[!anchor %in% DIFitems]),
                    error = function(e) {
                    })
                  if (!dfFound) {
                    # set dfFound flag
                    dfFound <- T
                  }
                }
            }
            estModel <- unlist(estModel)  # tidy up
        } else if (sum(c("MixedClass", "SingleGroupClass", "DiscreteClass", "MultipleGroupClass") %in% class(data)) !=
            0) {
            # if data is pre-calibrated mirt model, simply switch them
            estModel <- data
            data <- estModel@Data$data
        }

        # if estModel is not NULL, count modelHistoryCount plus one
        if (exists("estModel")) {
            if (!is.null(estModel)) {
                modelHistoryCount <- modelHistoryCount + 1
            }
        }

        # save model history (raw model, before model selection)
        if (exists("estModel")) {
            if (!is.null(estModel) && saveModelHistory && saveRawEstModels) {
              if(workDirectory != getwd()){
                setwd('/tmp')
                setwd(workDirectory)
              }
              modelHistory$rawEstModels[[modelHistoryCount]] <- estModel
                tryCatch(saveRDS(modelHistory, filename), error = function(e) {
                })
            }
        }

        if (exists("estModel")) {
            if (!is.null(estModel)) {
                # check ncol(estModel@Data$data) > 3 / Model Selection: if estModel is not NULL,
                # run this procedure

                if (class(estModel) == "list") {
                  # model fit evaluation
                  modModelFit <- vector()
                  for (i in 1:NROW(estModel)) {
                    if (sum(c("MixedClass", "SingleGroupClass", "DiscreteClass", "MultipleGroupClass") %in%
                      class(estModel[[i]])) > 0) {
                      if (estModel[[i]]@OptimInfo$secondordertest) {
                        # heywood case filter
                        if(class(estModel[[i]]) %in% "MixedClass"){
                          if(sum(invisible(.exportParmsEME(estModel[[i]], quiet = T))@Fit$h2 > 1) > 0){
                            next()
                          }
                        } else if(class(estModel[[i]]) %in% "SingleGroupClass"){
                          if(sum(estModel[[i]]@Fit$h2 > 1) > 0){
                            next()
                          }
                        }
                        if (toupper(modelSelectionCriteria) %in% c("DIC")) {
                          modModelFit[[length(modModelFit) + 1]] <- estModel[[i]]@Fit$DIC
                        } else if (toupper(modelSelectionCriteria) %in% c("AIC")) {
                          modModelFit[[length(modModelFit) + 1]] <- estModel[[i]]@Fit$AIC
                        } else if (toupper(modelSelectionCriteria) %in% c("AICC", "CAIC")) {
                          modModelFit[[length(modModelFit) + 1]] <- estModel[[i]]@Fit$AICc
                        } else if (toupper(modelSelectionCriteria) %in% c("BIC")) {
                          modModelFit[[length(modModelFit) + 1]] <- estModel[[i]]@Fit$BIC
                        } else if (toupper(modelSelectionCriteria) %in% c("SABIC")) {
                          modModelFit[[length(modModelFit) + 1]] <- estModel[[i]]@Fit$SABIC
                        } else {
                          stop("please specify model fit type correctly: DIC (default), AIC, BIC, AICc (aka cAIC), saBIC")
                        }
                      } else {
                        modModelFit[[length(modModelFit) + 1]] <- NA  # prevent unexpected event
                      }
                    }
                  }

                  # select model
                  if (exists("modModelFit")) {
                    if (length(which(modModelFit == min(modModelFit[is.finite(modModelFit)],
                      na.rm = T))[1]) > 0) {
                      estModel <- estModel[[which(modModelFit == min(modModelFit[is.finite(modModelFit)],
                        na.rm = T))[1]]]
                    } else {
                      message("Can not find any optimal model")
                      STOP <- T
                    }
                  } else {
                    message("Can not find any optimal model")
                    STOP <- T
                  }
                }

                if (exists("modelFound")) {
                  data <- estModel@Data$data
                } else if (exists("dfFound")) {
                  data <- estModel@Data$data
                }

                if (class(estModel) %in% c("MixedClass", "SingleGroupClass", "DiscreteClass", "MultipleGroupClass")) {

                  if (class(estModel) %in% "MixedClass") {
                    if (is.numeric(estModel@Model$model)) {
                      if (estModel@Model$model > 1 && !estModel@Options$exploratory) {
                        estModel@Options$exploratory <- TRUE
                      }
                    }
                  }
                  # save model history of DIC evaluated model
                  if (saveModelHistory) {
                    if(workDirectory != getwd()){
                      setwd('/tmp')
                      setwd(workDirectory)
                    }
                    modelHistory$estModelTrials[[modelHistoryCount]] <- estModel
                    tryCatch(saveRDS(modelHistory, filename), error = function(e) {
                    })
                  }

                  if (exists("estItemFit")) {
                    tryCatch(rm(estItemFit), error = function(e) {
                    })
                  }
                  fitDONE <- FALSE
                  while (!fitDONE) {
                    if(workDirectory != getwd()){
                      setwd('/tmp')
                      setwd(workDirectory)
                    }
                    # reconnect
                    # tryCatch(aefaInit(RemoteClusters = RemoteClusters, debug = printDebugMsg,
                    #   sshKeyPath = sshKeyPath), error = function(e) {
                    # })

                    # rotation search
                    if (estModel@Model$model > 1 && sum(estModel@Model$itemtype ==
                      "lca") == 0) {
                      searchDone <- FALSE

                      # step 1: calculate Zh
                      while (!searchDone) {
                        estItemFitRotationSearchTmp <- listenv::listenv()
                        for (rotateTrial in rotate) {
                          estItemFitRotationSearchTmp[[rotateTrial]] %<-% tryCatch(evaluateItemFit(estModel,
                            RemoteClusters = RemoteClusters, rotate = rotateTrial, PV_Q1 = F, S_X2 = F),
                            error = function(e) {NULL})
                        }
                        if (!is.null(as.list(estItemFitRotationSearchTmp))) {
                          estItemFitRotationSearchTmp <- as.list(estItemFitRotationSearchTmp)
                          searchDone <- TRUE
                        }
                      }

                      rotateCandidate1 <- names(estItemFitRotationSearchTmp)
                      rotateCandidate2 <- vector()
                      estItemFitRotationSearch <- list()
                      for(i in 1:NROW(estItemFitRotationSearchTmp)){
                        if(!is.null(estItemFitRotationSearchTmp[[i]])){
                          rotateCandidate2[length(rotateCandidate2) + 1] <- rotateCandidate1[i]
                          estItemFitRotationSearch[[NROW(estItemFitRotationSearch) + 1]] <- estItemFitRotationSearchTmp[[i]]
                        }
                      }

                      names(estItemFitRotationSearch) <- rotateCandidate2

                      # print(estItemFitRotationSearch)

                      # step 2: count Zh < cutoff
                      countZh <- vector()

                      for(countZh_iter in estItemFitRotationSearch){
                        if(!is.null(countZh_iter)){
                          countZh[length(countZh) + 1] <- sum((countZh_iter$Zh)+abs(qnorm(.025))/sqrt(nrow(data)) <
                                                                qnorm(fitIndicesCutOff/2))
                        } else {
                          countZh[length(countZh) + 1] <- NA
                        }
                      }

                      # step 3: decision; countZh is placeholder of is.na(Zh)
                      rotateCandidates <- names(estItemFitRotationSearch)[which(countZh[is.finite(countZh)] == min(countZh[is.finite(countZh)], na.rm = T))]

                      # check best candidates rotations
                      if (length(rotateCandidates) > 1) {
                        Zh_min <- vector()
                        estItemFitRotationSearchTmp <- list()
                        rotateCandidate3 <- vector()
                        for(i in which(names(estItemFitRotationSearch) %in% rotateCandidates)){
                          rotateCandidate3[[length(rotateCandidate3) + 1]] <- names(estItemFitRotationSearch)[i]
                          estItemFitRotationSearchTmp[[NROW(estItemFitRotationSearchTmp) + 1]] <- estItemFitRotationSearch[[i]]
                        }
                        names(estItemFitRotationSearchTmp) <- rotateCandidate3
                        for(i in estItemFitRotationSearchTmp){
                          Zh_min[length(Zh_min)+1] <- min(abs(i$Zh))
                        }
                        rotateCandidates <- rotateCandidate3[which(Zh_min == min(Zh_min[is.finite(Zh_min)], na.rm = T))]
                      }

                      # decision again if models aren't have any differences
                      if (length(rotateCandidates) > 1) {
                        rotateCandidates <- rotateCandidates[1]
                      } else if (length(rotateCandidates) == 0){
                        rotateCandidates <- 'none'
                      }

                      # estimate item fit measures
                      estItemFit <- tryCatch(evaluateItemFit(estModel, RemoteClusters = RemoteClusters,
                        rotate = rotateCandidates, PV_Q1 = PV_Q1), error = function(e) {NULL})

                    } else {
                      # estimate item fit measures
                      rotateCandidates <- 'none'
                      estItemFit <- tryCatch(evaluateItemFit(estModel, RemoteClusters = RemoteClusters,
                        rotate = rotateCandidates, PV_Q1 = PV_Q1), error = function(e) {NULL})
                    }

                    if (exists("estItemFit")) {
                      if (length(estItemFit) != 0) {
                        fitDONE <- TRUE
                      }
                    }
                  }
                  if (printItemFit) {
                    tryCatch(print(estItemFit), error = function(e) {NULL})
                  }

                  # save model
                  if (saveModelHistory) {
                    if(workDirectory != getwd()){
                      setwd('/tmp')
                      setwd(workDirectory)
                    }
                    modelHistory$itemFitTrials[[modelHistoryCount]] <- estItemFit
                    modelHistory$rotationTrials[[modelHistoryCount]] <- rotateCandidates
                    tryCatch(saveRDS(modelHistory, filename), error = function(e) {
                    })
                  }

                  # checkup conditions
                  if ("Zh" %in% colnames(estItemFit)) {
                    ZhCond <- sum(estItemFit$Zh+abs(qnorm(.025))/sqrt(nrow(data)) < qnorm(fitIndicesCutOff/2), na.rm = T) !=
                      0  # p < .005
                  } else {
                    ZhCond <- FALSE
                  }

                  if ("df.PV_Q1" %in% colnames(estItemFit)) {
                    if (sum(is.na(estItemFit$df.PV_Q1), na.rm = T) == length(estItemFit$df.PV_Q1)) {
                      PVCond1 <- FALSE
                      PVCond2 <- FALSE
                      PVCond3 <- FALSE
                    } else {
                      PVCond1 <- sum(is.na(estItemFit$df.PV_Q1), na.rm = T) != 0
                      PVCond2 <- length(which(estItemFit$df.PV_Q1 == 0)) > 0
                      PVCond3 <- sum(estItemFit$p.PV_Q1_star < fitIndicesCutOff, na.rm = T) !=
                        0  # https://osf.io/preprints/psyarxiv/mky9j/
                      if (sum(estItemFit$p.PV_Q1_star < fitIndicesCutOff, na.rm = T) ==
                        length(estItemFit$p.PV_Q1_star)) {
                        # turn off when all p-values are p<.005; that may wrong
                        PVCond3 <- FALSE
                      }
                    }
                  } else {
                    PVCond1 <- FALSE
                    PVCond2 <- FALSE
                    PVCond3 <- FALSE
                  }


                  if ("df.S_X2" %in% colnames(estItemFit)) {
                    if (sum(is.na(estItemFit$df.S_X2), na.rm = T) == length(estItemFit$df.S_X2)) {
                      S_X2Cond1 <- FALSE
                      S_X2Cond2 <- FALSE
                      S_X2Cond3 <- FALSE
                    } else {
                      S_X2Cond1 <- sum(is.na(estItemFit$df.S_X2), na.rm = T) != 0
                      S_X2Cond2 <- length(which(estItemFit$df.S_X2 == 0)) > 0
                      S_X2Cond3 <- sum(estItemFit$p.S_X2 < fitIndicesCutOff, na.rm = T) !=
                        0  # https://osf.io/preprints/psyarxiv/mky9j/
                      if (sum(estItemFit$p.S_X2 < fitIndicesCutOff, na.rm = T) ==
                        length(estItemFit$p.S_X2)) {
                        # turn off when all p-values are p<.005; that may wrong
                        S_X2Cond3 <- FALSE
                      }

                      # RMSEA Based
                      S_X2Cond4 <- sum(estItemFit$RMSEA.S_X2 > .05/length(estItemFit$RMSEA.S_X2), na.rm = T) > 0
                    }
                  } else {
                    S_X2Cond1 <- FALSE
                    S_X2Cond2 <- FALSE
                    S_X2Cond3 <- FALSE
                    S_X2Cond4 <- FALSE
                  }

                  # flagging bad item
                  if (ZhCond) {
                    badItemNames <- c(badItemNames, as.character(estItemFit$item[which(estItemFit$Zh ==
                      min(estItemFit$Zh[is.finite(estItemFit$Zh)], na.rm = T))]))
                  } else if (PVCond1) {
                    badItemNames <- c(badItemNames, as.character(estItemFit$item[which(is.na(estItemFit$df.PV_Q1))]))
                  } else if (PVCond2) {
                    badItemNames <- c(badItemNames, as.character(estItemFit$item[which(estItemFit$df.PV_Q1 ==
                      0)]))
                  } else if (PVCond3) {
                    badItemNames <- c(badItemNames, as.character(estItemFit$item[which(estItemFit$PV_Q1/estItemFit$df.PV_Q1 ==
                      max(estItemFit$PV_Q1[is.finite(estItemFit$PV_Q1)]/estItemFit$df.PV_Q1[is.finite(estItemFit$df.PV_Q1)],
                        na.rm = T))]))
                  } else if (S_X2Cond1) {
                    badItemNames <- c(badItemNames, as.character(estItemFit$item[which(is.na(estItemFit$df.S_X2))]))
                  } else if (S_X2Cond2) {
                    badItemNames <- c(badItemNames, as.character(estItemFit$item[which(estItemFit$df.S_X2 ==
                      0)]))
                  } else if (S_X2Cond3) {
                    badItemNames <- c(badItemNames, as.character(estItemFit$item[which(estItemFit$S_X2/estItemFit$p.S_X2 ==
                      max(estItemFit$S_X2[is.finite(estItemFit$S_X2)]/estItemFit$p.S_X2[is.finite(estItemFit$p.S_X2)],
                        na.rm = T))]))
                  } else if (S_X2Cond4){
                    badItemNames <- c(badItemNames, as.character(estItemFit$item[which(estItemFit$RMSEA.S_X2 ==
                                                                                         max(estItemFit$RMSEA.S_X2[is.finite(estItemFit$RMSEA.S_X2)],
                                                                                             na.rm = T))]))
                  } else if(class(estModel) %in% "MultipleGroupClass" && checkDIF){ # DIF part (fixed effect)
                    if (toupper(modelSelectionCriteria) %in% c("DIC")) { # find out
                      seqStat <- 'AIC'
                    } else if (toupper(modelSelectionCriteria) %in% c("AIC")) {
                      seqStat <- 'AIC'
                    } else if (toupper(modelSelectionCriteria) %in% c("AICC", "CAIC")) {
                      seqStat <- 'AICc'
                    } else if (toupper(modelSelectionCriteria) %in% c("BIC")) {
                      seqStat <- 'BIC'
                    } else if (toupper(modelSelectionCriteria) %in% c("SABIC")) {
                      seqStat <- 'SABIC'
                    }
                    if(exists('stepdown')){
                      try(rm(stepdown))
                    }
                    stepdown <- tryCatch(suppressMessages(mirt::DIF(MGmodel = estModel,
                                                                    which.par = unique(mirt::mod2values(estModel)$name[grep("^a|^d",
                                                                                                                            mirt::mod2values(estModel)$name)]),
                                                                    scheme = 'drop_sequential', seq_stat = seqStat)), error = function(e){NULL})
                    DIFsearch <- vector()
                    if(!is.null(stepdown)){ # If DIF exists in trial,
                      for(effsize in stepdown){ # check the effect size of DIF,
                        DIFsearch[length(DIFsearch) + 1] <- effsize[2,]$X2 / effsize[2,]$df
                      }
                      DIFitems <- c(DIFitems, names(stepdown)[which(max(DIFsearch))[1]]) # flagging which one is not common (DIF)
                      if (is.null(DIFitems)){ # However, DIFitems is NULL with unknown reasons,
                        checkDIF <- FALSE # stop the DIF checking
                      }
                    } else { # If DIF not exists in trial
                      checkDIF <- FALSE # stop DIF checking
                    }

                  } else if (length(estItemFit$item) <= 3) {
                    STOP <- TRUE
                  } else if (!estModel@Options$exploratory && estModel@Model$itemtype[1] != 'Rasch' && class(estModel) != 'MultipleGroupClass') {
                    is.between <- function(x, a, b) {
                      (x - a) * (b - x) > 0
                    }
                    try(invisible(suppressWarnings(suppressMessages(mirt::mirtCluster(parallel::detectCores(logical = F))))),
                      silent = T)
                    try(modCI <- mirt::PLCI.mirt(estModel), silent = T)
                    try(invisible(suppressMessages(suppressMessages(mirt::mirtCluster(remove = T)))),
                      silent = T)
                    if (exists("modCI")) {
                      includeZero <- vector()
                      diffValues <- vector()
                      for (i in 1:length(grep("^a", modCI$parnam))) {
                        includeZero[i] <- is.between(0, modCI[grep("^a", modCI$parnam),
                          ]$lower_2.5[i], modCI[grep("^a", modCI$parnam), ]$upper_97.5[i])
                        diffValues[i] <- diff(c(modCI[grep("^a", modCI$parnam), ]$lower_2.5[i],
                          modCI[grep("^a", modCI$parnam), ]$upper_97.5[i]))
                      }

                      if (sum(includeZero, na.rm = T) == 0) {
                        STOP <- TRUE
                      } else {
                        badItemNames <- c(badItemNames, as.character(estItemFit$item[which(max(diffValues[is.finite(diffValues)],
                                                                                               na.rm = T) == diffValues)[1]]))
                        if (length(badItemNames) == 0) {
                          STOP <- TRUE
                        }
                      }
                      try(rm(modCI))


                    } else {
                      STOP <- TRUE
                    }

                  } else {
                    STOP <- TRUE
                  }

                  # adjust model if supplied model is confirmatory model
                  if (!is.null(model) && (!is.numeric(model) | !is.integer(model)) &&
                    "Zh" %in% colnames(estItemFit)) {
                    for (i in 1:NROW(model)) {
                      if (class(model[[i]]) == "mirt.model") {
                        for (j in 1:nrow(model[[i]])) {
                          if (!model[[i]]$x[j, 1] %in% c("COV", "MEAN", "FREE", "NEXPLORE")) {

                            # convert elements # FIXME
                            model[[i]]$x[j, 2] <- eval(parse(text = paste0("c(",
                              gsub("-", ":", model[[i]]$x[j, 2]), ")")))[!eval(parse(text = paste0("c(",
                              gsub("-", ":", model[[i]]$x[j, 2]), ")"))) %in% estItemFit$item[which(estItemFit$Zh ==
                              min(estItemFit$Zh[is.finite(estItemFit$Zh)], na.rm = T))]]  ## FIXME

                            for (k in length(model[[i]]$x[j, 2])) {
                              if (is.numeric(model[[i]]$x[j, 2][k]) | is.integer(model[[i]]$x[j,
                                2][k])) {
                                model[[i]]$x[j, 2][k] <- which(colnames(data.frame(data[,
                                  !colnames(data) %in% badItemNames])) == colnames(data.frame(data[,
                                  !colnames(data) %in% badItemNames]))[model[[i]]$x[j,
                                  2][k]])
                              }
                            }

                            # FIXME
                            model[[i]]$x[j, 2] <- paste(as.character(model[[i]]$x[j,
                              2]), collapse = ", ", sep = "")

                          }
                        }
                      }
                    }
                  }

                }

                # if (ncol(estModel@Data$data) > 3) {} else { message('model is not fit well')
                # STOP <- T # set flag of 'stop while loop' }

            }
        } else {
            stop("estimations were failed. please retry with check your data or models")
        }
    }  # the end of while loop


    TimeEnd <- Sys.time()
    TimeTotal <- TimeEnd - TimeStart


    if (saveModelHistory) {
        class(modelHistory) <- "aefa"
        modelHistory$TimeTotal <- TimeTotal
        modelHistory$TimeStart <- TimeStart
        modelHistory$TimeEnd <- TimeEnd
        modelHistory$workPath <- workDirectory
        return(modelHistory)
    } else {
        return(estModel)
    }
}

#' summarise AEFA results
#'
#' @param mirtModel estimated aefa model
#' @param rotate rotation method. Default is NULL, kaefa will be automatically select the rotation criteria using aefa calibrated model.
#' @param suppress cutoff of rotated coefs. Generally .30 is appropriate but .10 will be good in practically
#' @param which.inspect which you are want to inspect of a calibration trial? If NULL, the last model will return.
#' @param printRawCoefs print the raw IRT coefs.
#' @param simplifyRawCoefs print the simplified raw IRT coefs if available when printRawCoefs = TRUE.
#' @return summary of aefa results
#' @export
#'
#' @examples
#' \dontrun{
#' testMod1 <- aefa(mirt::Science, minExtraction = 1, maxExtraction = 2)
#' aefaResults(testMod1)
#' }
aefaResults <- function(mirtModel, rotate = NULL, suppress = 0, which.inspect = NULL, printRawCoefs = F, simplifyRawCoefs = T) {

    if (class(mirtModel) == "aefa") {
        if(is.null(which.inspect)){
          inspectModelNumber <- NROW(mirtModel$estModelTrials)
        } else {
          inspectModelNumber <- which.inspect
        }
        if(is.null(rotate)){
          automatedRotation <- mirtModel$rotationTrials[[inspectModelNumber]]
        } else {
          automatedRotation <- rotate
        }

        message(paste0("aefa results: aefa has ", NROW(mirtModel$estModelTrials),
            " automated internal validation trials."))
        mirtModel <- mirtModel$estModelTrials[[inspectModelNumber]]

    } else {
      automatedRotation <- rotate
    }

    # convert mixedclass to singleclass temporary
    if (class(mirtModel)[1] == "MixedClass") {
      mirtModel <- .exportParmsEME(mirtModel)
    }

    resultM2 <- tryCatch(mirt::M2(mirtModel, QMC = T), error = function(e) {NULL})
    if (exists("resultM2") && !is.null(resultM2)) {
        message("M2 statistic")
        print(resultM2)
        if(resultM2$CFI > .9 && resultM2$TLI > .9 && resultM2$RMSEA_5 < .05){
          message('The calibrated model is seems reasonable')
        } else if(resultM2$CFI > .9 && resultM2$TLI > .9 && resultM2$RMSEA_5 > .05){
          message('The calibrated model is seems reasonable,')
          message('but RMSEA is inappropriate so that you might increase the number of factor extraction.')
        } else if(resultM2$CFI < .9 && resultM2$TLI > .9){
          message('The calibrated model is seems good,')
          message('but CFI is inappropriate so that you might increase the number of factor extraction.')
        } else if(resultM2$CFI > .9 && resultM2$TLI < .9){
          message('The calibrated model is seems good,')
          message('but TLI is inappropriate so that you might reconsider the model specification.')
        } else if(resultM2$CFI < .9 && resultM2$TLI < .9 && resultM2$RMSEA_5 < .05){
          message('The calibrated model is seems good,')
          message('but CFI and TLI is inappropriate so that you might increase the number of factor extraction.')
        } else if(resultM2$CFI < .9 && resultM2$TLI < .9 && resultM2$RMSEA_5 > .05){
          message('The calibrated model may have some issues,')
          message('please consider to add demographic data in covdata argument to inspection')
        }
        message("\n")
    }

    resultMarginalReliability <- tryCatch(mirt::empirical_rxx(mirt::fscores(mirtModel, QMC = T, method = if(mirtModel@Model$nfact == 1) 'EAP' else 'MAP', rotate = automatedRotation, full.scores.SE = T)), error = function(e) {NULL})

    if(is.null(automatedRotation)){
      automatedRotation <- 'oblimin' # mirt default
    } else {
      if(automatedRotation == 'none'){
        message(paste0("Item Factor Model loadings: ", mirtModel@Model$itemtype[1], ' model'))
      } else {
        message(paste0("Item Factor Model loadings: ", mirtModel@Model$itemtype[1], ' model', ' and\n', automatedRotation, ' rotation as optimal in probability perspectives.'))
      }
      if(automatedRotation %in% c('oblimin', 'quartimin', 'oblimax', 'simplimax', 'bentlerQ', 'geominQ', 'cfQ', 'infomaxQ', 'bifactorQ') & !is.null(rotate)){
        message(paste0('The ', automatedRotation, 'rotation is oblique rotation method.'))
        message('That might have correlational relationships between calibrated factors.')
      }
      if(automatedRotation %in% c('entropy', 'quartimax', 'varimax', 'bentlerT', 'tandemI', 'tandemII', 'geominT', 'cfT', 'infomaxT', 'mccammon', 'bifactorT')& !is.null(rotate)){
        message(paste0('The ', automatedRotation, 'rotation is oblique rotation method.'))
        message('That might not have correlational relationships between calibrated factors.')
      }
      if(automatedRotation %in% c('bifactorQ', 'bifactorT') & !is.null(rotate)){
        message('Moreover, your model has general factor (as known as g-factor) at F1. Therefore, another factors might be subfactor or method factor.')
      }
      if(mirtModel@Model$itemtype[1] %in% c('grsm', 'grsmIRT')){
        message('Sadly, This is a bad news: Your items seems too hard to read or understand to response well, interpret carefully!')
      }
    }

    mirt::summary(mirtModel, rotate = automatedRotation, suppress = suppress, maxit = 1e+05)

    if(printRawCoefs){
      mirt::coef(mirtModel, rotate = automatedRotation, simplify = simplifyRawCoefs)
    }

    if (exists("resultMarginalReliability") && !is.null(resultMarginalReliability)) {
      message("\nMarginal (Empirical) Reliability statistic from Item Information Function")
      print(resultMarginalReliability)
      message("\n")
    }
}

#' return Recursive Score into raw response scale or return the theta itself
#'
#' @param mirtModel estimated aefa model
#' @param mins logical; include the minimum value constants in the dataset. If FALSE, the expected values for each item are determined from the scoring 0:(ncat-1)
#' @param devide logical; devide into the number of items. default is FALSE.
#' @param rotate rotation method. Default is NULL, kaefa will be automatically select the rotation criteria using aefa calibrated model.
#' @param individual logical; return tracelines for individual items?
#' @param extractThetaOnly logical; return the theta only without recursive score? if TRUE, theta will return.
#' @return recursively expected test score
#' @export
#'
#' @examples
#' \dontrun{
#' testMod1 <- aefa(mirt::Science, minExtraction = 1, maxExtraction = 2)
#' aefaResults(testMod1)
#' recursiveScore <- recursiveFormula(testMod1)
#' }
recursiveFormula <- function(mirtModel, mins = F, devide = F, rotate = NULL, individual = F, extractThetaOnly = F){
  if (class(mirtModel) == "aefa") {

    if(is.null(rotate)){
      automatedRotation <- mirtModel$rotationTrials[[NROW(mirtModel$estModelTrials)]]
    } else {
      automatedRotation <- rotate
    }

    message(paste0("aefa results: aefa has ", NROW(mirtModel$estModelTrials),
                   " automated internal validation trials."))
    mirtModel <- mirtModel$estModelTrials[[NROW(mirtModel$estModelTrials)]]

  }

  # convert mixedclass to singleclass temporary
  if (class(mirtModel)[1] == "MixedClass") {
    ThetaRand <- mirt::randef(mirtModel)$Theta
    mirtModel <- .exportParmsEME(mirtModel)
  }

  if(exists('ThetaRand')){
    ThetaExpected <- ThetaRand
  } else {
    ThetaExpected <- mirt::fscores(mirtModel, QMC = T, method = if(mirtModel@Model$nfact == 1) 'EAP' else 'MAP', rotate = automatedRotation)
  }

  if(extractThetaOnly){
    return(ThetaExpected)
  } else {
    resultRecursive <- tryCatch(mirt::expected.test(mirtModel, ThetaExpected, mins = mins, individual = individual), error = function(e) {NULL})

    if(exists('resultRecursive') && !is.null(resultRecursive)){
      if(devide){
        ret <- resultRecursive / ncol(mirtModel@Data$data)
      } else {
        ret <- resultRecursive
      }
      return(ret)
    } else {
      stop('please check your model')
    }
  }

}
