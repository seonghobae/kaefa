# kaefa.R

#' Initalize aefa engine,
#' This function initalise the aefa cluster.
#' If someone have Remote Cluster informaiton with SSH, put the information in the argument.
#'
#' @import NCmisc
#' @import future
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
aefaInit <- function(RemoteClusters = NULL, debug = F, sshKeyPath = NULL) {
    options(future.debug = debug)

    assignClusterNodes <- function(serverList, loadPercentage = 50, freeRamPercentage = 50, requiredMinimumClusters = round(NROW(serverList)/3), sshKeyPath = NULL) {
        STOP <- F
        while (!STOP) {
            statusList <- list()
            decisionList <- list()
            for (i in serverList) {
                if(i == 'localhost'){ # localhost side
                  statusList$localhost <- tryCatch(system(paste("uptime | awk '{print $8}' &&",
                                                                "cat /proc/cpuinfo | grep processor | wc -l &&",
                                                                "free | grep Mem | awk '{print $4/$2 * 100}'"),
                                                intern = TRUE), error=function(e){}) # CentOS
                  if(length(grep('load', statusList[[i]][1])) > 0 | length(grep('average', statusList[[i]][1])) > 0){
                    Sys.sleep(30)
                    statusList$localhost <- tryCatch(system(paste("uptime | awk '{print $11}' &&",
                                                                  "cat /proc/cpuinfo | grep processor | wc -l &&",
                                                                  "free | grep Mem | awk '{print $4/$2 * 100}'"),
                                                       intern = TRUE), error=function(e){}) # Ubuntu
                  }
                } else { # SSH side
                  if(!is.null(sshKeyPath)){ # if key is provided
                    for(jj in 1:length(serverList)){
                      if(names(serverList)[[jj]] %in% names(serverList) &&
                         (length(grep(c('pem'), sshKeyPath[[jj]])) > 0 | length(grep(c('key'), sshKeyPath[[jj]])) > 0)){
                        statusList[[i]] <- tryCatch(system(paste("ssh", i, '-i', sshKeyPath[[jj]], "uptime | awk '{print $8}' &&",
                                                                 "ssh", i, '-i', jj, "cat /proc/cpuinfo | grep processor | wc -l &&",
                                                                 "ssh", i, '-i', jj, "free | grep Mem | awk '{print $4/$2 * 100}'"),
                                                      intern = TRUE), error=function(e){}) # CentOS
                        if(length(grep('load', statusList[[i]][1])) > 0 | length(grep('average', statusList[[i]][1])) > 0){
                          Sys.sleep(30)

                          statusList[[i]] <- tryCatch(system(paste("ssh", i, '-i', sshKeyPath[[jj]], "uptime | awk '{print $11}' &&",
                                                                   "ssh", i, '-i', jj, "cat /proc/cpuinfo | grep processor | wc -l &&",
                                                                   "ssh", i, '-i', jj, "free | grep Mem | awk '{print $4/$2 * 100}'"),
                                                        intern = TRUE), error=function(e){}) # Ubuntu
                        }

                      } else {
                      statusList[[i]] <- tryCatch(system(paste("ssh", i, "uptime | awk '{print $8}' &&",
                                                               "ssh", i, "cat /proc/cpuinfo | grep processor | wc -l &&",
                                                               "ssh", i, "free | grep Mem | awk '{print $4/$2 * 100}'"),
                                                    intern = TRUE), error=function(e){}) # CentOS
                      if(length(grep('load', statusList[[i]][1])) > 0 | length(grep('average', statusList[[i]][1])) > 0){
                        Sys.sleep(30)

                        statusList[[i]] <- tryCatch(system(paste("ssh", i, "uptime | awk '{print $11}' &&",
                                                                 "ssh", i, "cat /proc/cpuinfo | grep processor | wc -l &&",
                                                                 "ssh", i, "free | grep Mem | awk '{print $4/$2 * 100}'"),
                                                      intern = TRUE), error=function(e){}) # Ubuntu
                      }
                      }
                    }
                  } else {
                    statusList[[i]] <- tryCatch(system(paste("ssh", i, "uptime | awk '{print $8}' &&",
                                                             "ssh", i, "cat /proc/cpuinfo | grep processor | wc -l &&",
                                                             "ssh", i, "free | grep Mem | awk '{print $4/$2 * 100}'"),
                                                  intern = TRUE), error=function(e){}) # CentOS
                    if(length(grep('load', statusList[[i]][1])) > 0 | length(grep('average', statusList[[i]][1])) > 0){
                      Sys.sleep(30)
                      statusList[[i]] <- tryCatch(system(paste("ssh", i, "uptime | awk '{print $11}' &&",
                                                               "ssh", i, "cat /proc/cpuinfo | grep processor | wc -l &&",
                                                               "ssh", i, "free | grep Mem | awk '{print $4/$2 * 100}'"),
                                                    intern = TRUE), error=function(e){}) # Ubuntu
                    }
                  }
                }
              # evaluation
                statusList[[i]][1] <- gsub(",", "", statusList[[i]][1])
                decisionList[[i]] <- tryCatch(as.numeric(statusList[[i]][1])/as.numeric(statusList[[i]][2]) * 100 < loadPercentage &&
                                                statusList[[i]][3] > freeRamPercentage, error=function(e){})
            }
            availableCluster <- names(decisionList)[which(unlist(decisionList))]

            if (requiredMinimumClusters > length(availableCluster)) {
                # print(statusList)

                message("All clusters are busy now. Wait for 60 seconds.")
                Sys.sleep(60)
            } else {
                nCores <- 0
                for (jj in which(unlist(decisionList))) {
                  nCores <- nCores + as.numeric(statusList[[jj]][2])
                }
                # print(statusList)

                message("get ", nCores, " threads successfully from ", length(availableCluster), " clusters")
                STOP <- T
            }
        }
        unique(names(statusList)[which(unlist(decisionList))])
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
        tryCatch(future::plan(list(future::tweak(future::cluster,
                                                 workers = assignClusterNodes(RemoteClusters)),
                                   future::multiprocess), gc = T), error=function(e){})
    } else if (NROW(future::plan("list")) == 1) {
        if (length(grep("openblas", extSoftVersion()["BLAS"])) > 0) {
            options(aefaConn = future::plan(future::multiprocess,
                                            workers = parallelProcessors), gc = T)
        } else if (length(future::availableWorkers()) == 1) {
            options(aefaConn = future::plan(future::sequential), gc = T)
        } else {
            options(aefaConn = (tryCatch(future::plan(strategy = list(future::tweak(future::cluster(workers = parallelProcessors)),
                                                                      future::multiprocess(workers = parallelProcessors)),
                gc = T), error=function(e){})))
        }
    }
}


#' fit appropriate multilevel item response model automatically with \code{mirt::mixedmirt}
#'
#' @param data insert \code{data.frame} object.
#' @param model specify the mirt model if want to calibrate. accepting \code{mirt::mirt.model} object.
#' @param GenRandomPars Try to generate Random Parameters? Default is TRUE
#' @param NCYCLES N Cycles of Robbin Monroe stage (stage 3). Default is 4000.
#' @param BURNIN N Cycles of Metro-hastings burnin stage (stage 1). Default is 1500.
#' @param SEMCYCLES N Cycles of Metro-hastings burnin stage (stage 2). Default is 1000.
#' @param covdata insert covariate data frame where use to fixed and random effect term. if not inserted, ignoring fixed and random effect estimation.
#' @param fixed a right sided R formula for specifying the fixed effect (aka 'explanatory') predictors from covdata and itemdesign.
#' @param random a right sided formula or list of formulas containing crossed random effects of the form \code{v1 + ... v_n | G}, where \code{G} is the grouping variable and \code{v_n} are random numeric predictors within each group. G may contain interaction terms, such as group:items to include cross or person-level interactions effects.
#' @param accelerate a character vector indicating the type of acceleration to use. Default is  'squarem' for the SQUAREM procedure (specifically, the gSqS3 approach)
#' @param symmetric force S-EM/Oakes information matrix to be symmetric? Default is FALSE to detect solutions that have not reached the ML estimate.
#' @param itemtype set the calibration item type
#'
#' @return appropriate \code{mirt::mixedeffect} models in list vector
#' @export
#'
#' @examples
#' \dontrun{
#' testModel1 <- fitMLIRT(mirt::Science, covdata = mirt::Science, random = list())
#'
#'}
fitMLIRT <- function(data = data, model = 1, itemtype = NULL,
                     accelerate = 'squarem', GenRandomPars = T,
                     NCYCLES = 4000, BURNIN = 1500, SEMCYCLES = 1000,
                     symmetric = F, covdata = NULL, fixed = ~1, random = NULL) {

    options(future.globals.maxSize = 500 * 1024^3)

    modMLIRT_itemLevel <- listenv::listenv()
    modMLIRT_latentLevel <- listenv::listenv()

    modMLIRT_itemLevel %<-% tryCatch(mirt::mixedmirt(data = data, model = model, accelerate = accelerate,
                                                     itemtype = itemtype, SE = T,
                                                     GenRandomPars = GenRandomPars, covdata = covdata,
                                                     fixed = fixed, random = random, calcNull = T,
                                                     technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN,
                                                                      SEMCYCLES = SEMCYCLES, symmetric = symmetric)),
                                     error=function(e){})
    modMLIRT_latentLevel %<-% tryCatch(mirt::mixedmirt(data = data, model = model, accelerate = accelerate,
                                                       itemtype = itemtype, SE = T, GenRandomPars = GenRandomPars,
                                                       covdata = covdata, lr.fixed = fixed, lr.random = random,
                                                       calcNull = T, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN,
                                                                                      SEMCYCLES = SEMCYCLES,
                                                                                      symmetric = symmetric)),
                                       error=function(e){})

    # evaluate model
    if (exists("modMLIRT_itemLevel")) {
      modMLIRT_itemLevel <- unlist(as.list(modMLIRT_itemLevel))[[1]]
        if (class(modMLIRT_itemLevel) == "MixedClass") {
            if (!modMLIRT_itemLevel@OptimInfo$secondordertest) {
                rm(modMLIRT_itemLevel)
            }
        } else {
            rm(modMLIRT_itemLevel)
        }

    }


    if (exists("modMLIRT_latentLevel")) {
      modMLIRT_latentLevel <- unlist(as.list(modMLIRT_latentLevel))[[1]]
        if (class(modMLIRT_latentLevel) == "MixedClass") {
            if (!modMLIRT_latentLevel@OptimInfo$secondordertest) {
                rm(modMLIRT_latentLevel)
            }
        } else {
            rm(modMLIRT_latentLevel)
        }

    }

    # decision
    if (exists("modMLIRT_itemLevel") && exists("modMLIRT_latentLevel")) {
        if (modMLIRT_itemLevel@Fit$DIC < modMLIRT_latentLevel@Fit$DIC) {
            return(modMLIRT_itemLevel)
        } else {
            return(modMLIRT_latentLevel)
        }
    } else if (exists("modMLIRT_itemLevel") && !exists("modMLIRT_latentLevel")) {
        return(modMLIRT_itemLevel)
    } else if (!exists("modMLIRT_itemLevel") && exists("modMLIRT_latentLevel")) {
        return(modMLIRT_latentLevel)
    } else {
        # stop('no solution')
    }

}


#' assessment of fit indices of the calibrated model
#'
#' @param mirtModel insert estimated \code{mirt::mirt} or \code{mirt::mixedmirt} model.
#' @param RemoteClusters insert google computing engine virtual machine information.
#' @param rotate set the rotate critera if mirt model is exploratory model. default is bifactorQ
#' @param PV_Q1 Do you want to get PV_Q1 (Chalmers & Ng, 2017) if can get it? default is TRUE.
#'
#' @return return item fit estimates
#' @export
#'
#' @examples
#' \dontrun{
#' testModel1 <- engineAEFA(mirt::Science)
#' testItemFit1 <- evaluateItemFit(testModel1)
#' }
evaluateItemFit <- function(mirtModel, RemoteClusters = NULL, rotate = "bifactorQ", PV_Q1 = T) {
    # if (is.null(getOption('aefaConn'))) { getOption('aefaConn', aefaInit(RemoteClusters = RemoteClusters, debug = F)) }

    options(future.globals.maxSize = 500 * 1024^3)


    # convert mixedclass to singleclass temporary
    if (class(mirtModel)[1] == "MixedClass") {
        modMLM <- mirt::mirt(data = mirtModel@Data$data, model = mirtModel@Model$model,
                             SE = T, itemtype = mirtModel@Model$itemtype, pars = "values")
        modMLM_original <- mirt::mod2values(mirtModel)
        if (sum(modMLM_original$name == "(Intercept)") != 0) {
            modMLM_original <- modMLM_original[!modMLM_original$name == "(Intercept)", ]

        }
        modMLM$value[which(modMLM$item %in% colnames(mirtModel@Data$data))] <- modMLM_original$value[which(modMLM_original$item %in% colnames(mirtModel@Data$data))]
        modMLM$est <- F

        mirtModel <- mirt::mirt(data = mirtModel@Data$data, model = mirtModel@Model$model,
                                itemtype = mirtModel@Model$itemtype, pars = modMLM, method = "QMCEM", SE = F, calcNull = T)
    }

    if (attr(class(mirtModel), "package") == "mirt") {
        # item fit evaluation
        modFit_Zh <- listenv()
        modFit_SX2 <- listenv()
        modFit_Zh %<-% suppressWarnings(tryCatch(mirt::itemfit(mirtModel, rotate = rotate,
                                                               fit_stats = "Zh", QMC = T, method = "MAP",
                                                               impute = if (sum(is.na(mirtModel@Data$data)) > 0)
                                                                 100 else 0), error=function(e){}))

        modFit_SX2 %<-% suppressWarnings(tryCatch(mirt::itemfit(mirtModel, rotate = rotate,
                                                                fit_stats = "S_X2", QMC = T, method = "MAP",
                                                                impute = if (sum(is.na(mirtModel@Data$data)) > 0)
                                                                  100 else 0), error=function(e){}))

        if (mirtModel@Model$nfact == 1 && PV_Q1) {
            modFit_PVQ1 <- listenv()
            modFit_PVQ1 %<-% suppressWarnings(tryCatch(mirt::itemfit(mirtModel, rotate = rotate,
                                                                     fit_stats = "PV_Q1", QMC = T,
                                                                     method = "MAP"), error=function(e){}))

        }

        if (sum(mirtModel@Model$itemtype %in% "Rasch") > 0 && mirtModel@Model$nfact == 1) {
            modFit_infit <- listenv()
            modFit_infit %<-% tryCatch(mirt::itemfit(mirtModel, rotate = rotate,
                                                     fit_stats = "infit", QMC = T,
                                                     method = "WLE", impute = if (sum(is.na(mirtModel@Data$data)) > 0) 100 else 0),
                                       error=function(e){})
        }

        # check item fit indices are exists

        if (exists("modFit_Zh")) {
            if (!class(modFit_Zh)[1] == "mirt_df") {
                rm(modFit_Zh)
            }
        }

        if (exists("modFit_SX2")) {
            if (!class(modFit_SX2)[1] == "mirt_df") {
                rm(modFit_SX2)
            }
        }

        if (exists("modFit_PVQ1")) {
            if (!class(modFit_PVQ1)[1] == "mirt_df") {
                rm(modFit_PVQ1)
            }
        }

        if (exists("modFit_infit")) {
            if (!class(modFit_infit)[1] == "mirt_df") {
                rm(modFit_infit)
            }
        }

        itemFitList <- c("modFit_Zh", "modFit_SX2", "modFit_PVQ1", "modFit_infit")[c(exists("modFit_Zh"),
                                                                                     exists("modFit_SX2"),
                                                                                     exists("modFit_PVQ1"),
                                                                                     exists("modFit_infit"))]

        fitList <- list()
        for (i in 1:length(itemFitList)) {
            fitList[[i]] <- (eval(parse(text = itemFitList[i])))
        }
        return(suppressMessages(plyr::join_all(fitList)))

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
#' @param samples specify the number samples with resampling. default is 5000.
#' @param printDebugMsg Do you want to see the debugging messeages? default is FALSE
#' @param modelSelectionCriteria Which critera want to use model selection work? 'DIC' (default), 'AIC', 'AICc', 'BIC', 'saBIC' available.
#' @param saveRawEstModels Do you want to save raw estimated models before model selection work? default is FALSE
#' @param fitEMatUIRT Do you want to fit the model with EM at UIRT? default is FALSE
#' @param ranefautocomb Do you want to find global-optimal random effect combination? default is TRUE
#' @param PV_Q1 Do you want to get PV_Q1 (Chalmers & Ng, 2017) if can get it? default is TRUE.
#' @param tryLCA Do you want to try calibrate LCA model if avaliable? default is TRUE
#'
#' @return automated exploratory factor analytic models
#' @export
#'
#' @examples
#' \dontrun{
#' testMod1 <- aefa(mirt::Science, minExtraction = 1, maxExtraction = 2)
#'
#' }
aefa <- function(data, model = NULL, minExtraction = 1, maxExtraction = if (ncol(data) < 10) ncol(data) else 10, RemoteClusters = NULL, sshKeyPath = NULL, GenRandomPars = T, NCYCLES = 4000,
    BURNIN = 1500, SEMCYCLES = 1000, covdata = NULL, fixed = ~1, random = list(), key = NULL, accelerate = "squarem", symmetric = F, saveModelHistory = T, filename = "aefa.RDS",
    printItemFit = T, rotate = "bifactorQ", resampling = T, samples = 5000, printDebugMsg = F, modelSelectionCriteria = "DIC", saveRawEstModels = F, fitEMatUIRT = F, ranefautocomb = T, PV_Q1 = T, tryLCA = T) {
    # if ('sequential' %in% class(future::plan('list')[[1]]) | 'sequential' %in% class(getOption('aefaConn')) | is.null(getOption('aefaConn'))) { getOption('aefaConn',
    # aefaInit(RemoteClusters = RemoteClusters, debug = printDebugMsg)) }

    options(future.globals.maxSize = 500 * 1024^3)

    # prepare for bad item detection
    badItemNames <- vector()  # make new null vector

    # prepare for save model history
    modelHistoryCount <- 0
    if (saveModelHistory) {
        if (saveRawEstModels) {
            modelHistory <- list(rawEstModels = list(), estModelTrials = list(), itemFitTrials = list())
        } else {
            modelHistory <- list(estModelTrials = list(), itemFitTrials = list())
        }
    }

    calibModel <- as.list(minExtraction:maxExtraction)
    if (!is.null(model)) {
        # user specified EFA or CFA
        j <- maxExtraction
        model <- unlist(list(model))
        for (i in 1:NROW(model)) {
            if (class(model[[i]]) == "mirt.model" | class(model[[i]]) == "numeric") {
                calibModel[[j + i]] <- tryCatch(model[[i]], error=function(e){})
            }
        }
    }

    model <- calibModel

    # prepare for do aefa work
    STOP <- F

    while (!STOP) {
        # estimate run exploratory IRT and confirmatory IRT
        if ((is.data.frame(data) | is.matrix(data))) {

          if(exists('estModel')){
            tryCatch(rm(estModel), error=function(e){})
          }
          modelDONE <- FALSE
          while(!modelDONE){
            tryCatch(aefaInit(RemoteClusters = RemoteClusters, debug = printDebugMsg, sshKeyPath = sshKeyPath), error=function(e){})
            # general condition
            estModel <- tryCatch(engineAEFA(data = data.frame(data[, !colnames(data) %in% badItemNames]), model = model, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN,
                                       SEMCYCLES = SEMCYCLES, covdata = covdata, fixed = fixed, random = random, key = key, accelerate = accelerate, symmetric = symmetric, resampling = resampling,
                                       samples = samples, printDebugMsg = printDebugMsg, fitEMatUIRT = fitEMatUIRT, ranefautocomb = ranefautocomb, tryLCA = tryLCA), error=function(e){})
            if(exists('estModel')){
              modelDONE <- TRUE
            }
          }

        } else if (is.list(data) && !is.data.frame(data)) {
            # Some weird condition: user specified pre-calibrated model or list of data.frame in data

            estModel <- list()
            for (i in 1:NROW(data)) {
                if (sum(c("MixedClass", "SingleGroupClass", "DiscreteClass") %in% class(data[[i]])) != 0) {
                  # then first, searching pre-calibrated model in data argument
                  estModel[[NROW(estModel) + 1]] <- data[[i]]
                  if (!modelFound) {
                    # set modelFound flag
                    modelFound <- T
                  }
                } else if (is.data.frame(data[[i]]) | is.matrix(data[[i]])) {
                  # if list contains dataframe, try to estimate them anyway; even this behaviour seems weird
                  estModel[[NROW(estModel) + 1]] <- tryCatch(engineAEFA(data = data.frame(data[[i]][, !colnames(data[[i]]) %in% badItemNames]), model = model, GenRandomPars = GenRandomPars,
                    NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, covdata = covdata, fixed = fixed, random = random, key = key, accelerate = accelerate, symmetric = symmetric,
                    resampling = resampling, samples = samples, printDebugMsg = printDebugMsg, fitEMatUIRT = fitEMatUIRT, ranefautocomb = ranefautocomb, tryLCA = tryLCA), error=function(e){})
                  if (!dfFound) {
                    # set dfFound flag
                    dfFound <- T
                  }
                }
            }
            estModel <- unlist(estModel)  # tidy up
        } else if (sum(c("MixedClass", "SingleGroupClass", "DiscreteClass") %in% class(data)) != 0) {
            # if data is pre-calibrated mirt model, simply switch them
            estModel <- data
            data <- estModel@Data$data
        }

        # save model history (raw model, before model selection)
        if (saveModelHistory)
            modelHistoryCount <- modelHistoryCount + 1
        if (exists("estModel")) {
            if (saveModelHistory && saveRawEstModels) {
                modelHistory$rawEstModels[[modelHistoryCount]] <- estModel
                tryCatch(saveRDS(modelHistory, filename), error=function(e){})
            }
        }




        if (exists("estModel")) {
            # check ncol(estModel@Data$data) > 3 Model Selection: if estModel is not NULL, run this procedure

            # model fit evaluation
            modModelFit <- vector()
            for (i in 1:NROW(estModel)) {
                if (sum(c("MixedClass", "SingleGroupClass", "DiscreteClass") %in% class(estModel[[i]])) > 0) {
                  if (estModel[[i]]@OptimInfo$secondordertest) {
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
            if(length(which(modModelFit == min(modModelFit, na.rm = T))[1]) > 0){
              estModel <- estModel[[which(modModelFit == min(modModelFit, na.rm = T))[1]]]
            } else {
              message('Can not find any optimal model')
              STOP <- T
            }


            if (exists("modelFound")) {
                data <- estModel@Data$data
            } else if (exists("dfFound")) {
                data <- estModel@Data$data
            }

            if (class(estModel) %in% c("MixedClass", "SingleGroupClass", "DiscreteClass")) {
                # evaluate model save model
                if (saveModelHistory) {
                  modelHistory$estModelTrials[[modelHistoryCount]] <- estModel
                  tryCatch(saveRDS(modelHistory, filename), error=function(e){})
                }

              if(exists('estItemFit')){
                tryCatch(rm(estItemFit), error=function(e){})
              }
              fitDONE <- FALSE
              while(!fitDONE){
                tryCatch(aefaInit(RemoteClusters = RemoteClusters, debug = printDebugMsg, sshKeyPath = sshKeyPath))
                estItemFit <- tryCatch(evaluateItemFit(estModel, RemoteClusters = RemoteClusters, rotate = rotate, PV_Q1 = PV_Q1), error=function(e){})
                if(exists('estItemFit')){
                  fitDONE <- TRUE
                }
              }
                if (printItemFit) {
                  tryCatch(print(estItemFit), error=function(e){})
                }

                # save model
                if (saveModelHistory) {
                  modelHistory$itemFitTrials[[modelHistoryCount]] <- estItemFit
                  tryCatch(saveRDS(modelHistory, filename), error=function(e){})
                }

              # checkup conditions
              if ("Zh" %in% colnames(estItemFit)){
                ZhCond <- sum(estItemFit$Zh < -1.96, na.rm = T) != 0
              } else {
                ZhCond <- FALSE
              }

              if ("df.PV_Q1" %in% colnames(estItemFit)){
                PVCond1 <- sum(is.na(estItemFit$df.PV_Q1), na.rm = T) != 0
                PVCond2 <- length(which(estItemFit$df.PV_Q1 == 0)) > 0
                PVCond3 <- sum(estItemFit$p.PV_Q1 < 0.005, na.rm = T) != 0 # https://osf.io/preprints/psyarxiv/mky9j/
                if(sum(estItemFit$p.PV_Q1 < 0.005, na.rm = T) == length(estItemFit$p.PV_Q1)){ # turn off when all p-values are p<.005; that may wrong
                  PVCond3 <- FALSE
                }
              } else {
                PVCond1 <- FALSE
                PVCond2 <- FALSE
                PVCond3 <- FALSE
              }


              if ("df.S_X2" %in% colnames(estItemFit)){
                S_X2Cond1 <- sum(is.na(estItemFit$df.S_X2), na.rm = T) != 0
                S_X2Cond2 <- length(which(estItemFit$df.S_X2 == 0)) > 0
                S_X2Cond3 <- sum(estItemFit$p.S_X2 < 0.005, na.rm = T) != 0 # https://osf.io/preprints/psyarxiv/mky9j/
                if(sum(estItemFit$p.S_X2 < 0.005, na.rm = T) == length(estItemFit$p.S_X2)){ # turn off when all p-values are p<.005; that may wrong
                  S_X2Cond3 <- FALSE
                }
              } else {
                S_X2Cond1 <- FALSE
                S_X2Cond2 <- FALSE
                S_X2Cond3 <- FALSE
              }

              # plagging bad item
              if(ZhCond){
                badItemNames <- c(badItemNames, as.character(estItemFit$item[which(estItemFit$Zh == min(estItemFit$Zh, na.rm = T))]))
              } else if (PVCond1){
                badItemNames <- c(badItemNames, as.character(estItemFit$item[which(is.na(estItemFit$df.PV_Q1))]))
              } else if (PVCond2){
                badItemNames <- c(badItemNames, as.character(estItemFit$item[which(estItemFit$df.PV_Q1 == 0)]))
              } else if (PVCond3){
                badItemNames <- c(badItemNames, as.character(estItemFit$item[which(estItemFit$PV_Q1/estItemFit$df.PV_Q1 == max(estItemFit$PV_Q1/estItemFit$df.PV_Q1, na.rm = T))]))
              } else if (S_X2Cond1){
                badItemNames <- c(badItemNames, as.character(estItemFit$item[which(is.na(estItemFit$df.S_X2))]))
              } else if (S_X2Cond2){
                badItemNames <- c(badItemNames, as.character(estItemFit$item[which(estItemFit$df.S_X2 == 0)]))
              } else if (S_X2Cond3){
                badItemNames <- c(badItemNames, as.character(estItemFit$item[which(estItemFit$S_X2/estItemFit$p.S_X2 == max(estItemFit$S_X2/estItemFit$p.S_X2, na.rm = T))]))
              } else if (length(estItemFit$item) <= 3) {
                STOP <- TRUE
              } else {
                STOP <- TRUE
              }

                # adjust model if supplied model is confirmatory model
                if (!is.null(model) && (!is.numeric(model) | !is.integer(model)) && "Zh" %in% colnames(estItemFit)) {
                  for (i in 1:NROW(model)) {
                    if (class(model[[i]]) == "mirt.model") {
                      for (j in 1:nrow(model[[i]])) {
                        if (!model[[i]]$x[j, 1] %in% c("COV", "MEAN", "FREE", "NEXPLORE")) {

                          # convert elements # FIXME
                          model[[i]]$x[j, 2] <- eval(parse(text = paste0("c(", gsub("-", ":", model[[i]]$x[j, 2]), ")")))[!eval(parse(text = paste0("c(", gsub("-", ":",
                            model[[i]]$x[j, 2]), ")"))) %in% estItemFit$item[which(estItemFit$Zh == min(estItemFit$Zh, na.rm = T))]]  ## FIXME

                          for (k in length(model[[i]]$x[j, 2])) {
                            if (is.numeric(model[[i]]$x[j, 2][k]) | is.integer(model[[i]]$x[j, 2][k])) {
                              model[[i]]$x[j, 2][k] <- which(colnames(data.frame(data[, !colnames(data) %in% badItemNames])) == colnames(data.frame(data[, !colnames(data) %in%
                                badItemNames]))[model[[i]]$x[j, 2][k]])
                            }
                          }

                          # FIXME
                          model[[i]]$x[j, 2] <- paste(as.character(model[[i]]$x[j, 2]), collapse = ", ", sep = "")

                        }
                      }
                    }
                  }
                }

            }

            # if (ncol(estModel@Data$data) > 3) {} else { message('model is not fit well') STOP <- T # set flag of 'stop while loop' }



        } else {
            stop("estimations were failed. please retry with check your data or models")
        }
    }  # the end of while loop


    if (saveModelHistory) {
        return(modelHistory)
    } else {
        return(estModel)
    }
}
