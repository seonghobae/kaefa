# kaefa.R

#' Initalize aefa engine,
#' This function initalise the aefa cluster.
#' If someone have Google Computing Engine informaiton, put the information in the argument.
#' @import rzmq
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
        options(aefaConn = future::plan(list(future::tweak(future::cluster, workers = future::as.cluster(GCEvms)), future::multiprocess)))
    } else if (NROW(future::plan("list")) == 1) {
        if (length(grep("openblas", extSoftVersion()["BLAS"])) > 0) {
            options(aefaConn = future::plan(future::multiprocess))
        } else if (length(future::availableWorkers()) == 1) {
            options(aefaConn = future::plan(future::sequential))
        } else {
            options(aefaConn = (try(future::plan(strategy = list(future::tweak(future::cluster), future::multiprocess)), silent = T)))
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
fitMLIRT <- function(data = data, model = model, itemtype = NULL, accelerate = accelerate, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric,
    covdata = covdata, fixed = fixed, random = random) {
    modMLIRT_itemLevel <- mirt::mixedmirt(data = data, model = model, accelerate = accelerate, itemtype = itemtype, SE = T, GenRandomPars = GenRandomPars, covdata = covdata, fixed = fixed,
        random = random, calcNull = T, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric))
    modMLIRT_latentLevel <- mirt::mixedmirt(data = data, model = model, accelerate = accelerate, itemtype = itemtype, SE = T, GenRandomPars = GenRandomPars, covdata = covdata, lr.fixed = fixed,
        lr.random = random, calcNull = T, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric))

    # evaluate model
    if (exists("modMLIRT_itemLevel")) {
        if (class(modMLIRT_itemLevel) != "list") {
            if (!modMLIRT_itemLevel@OptimInfo$secondordertest) {
                rm(modMLIRT_itemLevel)
            }
        } else {
            rm(modMLIRT_itemLevel)
        }

    }


    if (exists("modMLIRT_latentLevel")) {
        if (class(modMLIRT_latentLevel) != "list") {
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
#' @param GCEvms insert google computing engine virtual machine information.
#' @param rotate set the rotate critera if mirt model is exploratory model. default is bifactorQ
#'
#' @return return item fit estimates
#' @export
#'
#' @examples
#' \dontrun{
#' testModel1 <- estIRT(mirt::Science)
#' testItemFit1 <- evaluateItemFit(testModel1)
#' }
evaluateItemFit <- function(mirtModel, GCEvms = NULL, rotate = "bifactorQ") {
    if (is.null(getOption("aefaConn"))) {
        getOption("aefaConn", aefaInit(GCEvms = GCEvms, debug = F))
    }

    if (attr(class(mirtModel), "package") == "mirt") {
        # item fit evaluation
        modFit_Zh <- listenv()
        modFit_Zh %<-% try(mirt::itemfit(mirtModel, rotate = rotate, fit_stats = "Zh", QMC = T, method = "MAP", impute = if (sum(is.na(mirtModel@Data$data)) > 0)
            100 else 0), silent = T)

        modFit_SX2 <- listenv()
        modFit_SX2 %<-% try(mirt::itemfit(mirtModel, rotate = rotate, fit_stats = "S_X2", QMC = T, method = "MAP", impute = if (sum(is.na(mirtModel@Data$data)) > 0)
            100 else 0), silent = T)

        if (mirtModel@Model$nfact == 1) {
            modFit_PVQ1 <- listenv()
            modFit_PVQ1 %<-% try(mirt::itemfit(mirtModel, rotate = rotate, fit_stats = "PV_Q1", QMC = T, method = "MAP"), silent = T)

        }

        if (sum(mirtModel@Model$itemtype %in% "Rasch") > 0 && mirtModel@Model$nfact == 1) {
            modFit_infit <- listenv()
            modFit_infit %<-% try(mirt::itemfit(mirtModel, rotate = rotate, fit_stats = "infit", QMC = T, method = "MAP", impute = if (sum(is.na(mirtModel@Data$data)) > 0)
                100 else 0), silent = T)
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

        itemFitList <- c("modFit_Zh", "modFit_SX2", "modFit_PVQ1", "modFit_infit")[c(exists("modFit_Zh"), exists("modFit_SX2"), exists("modFit_PVQ1"), exists("modFit_infit"))]

        fitList <- list()
        for (i in 1:length(itemFitList)) {
            # fitList[[i]] <-
            fitList[[i]] <- (eval(parse(text = itemFitList[i])))
        }
        return(suppressMessages(plyr::join_all(fitList)))

    } else {
        message("That's seems not MIRT model, so that trying to estimate new model")
        estModel <- exploratoryIRT(data = mirtModel)
        return(estModel)
    }
}


#' estimate full-information item factor analysis models with combinating random effects
#'
#' @importFrom utils combn
#' @import listenv
#' @import future
#' @import mirt
#' @import psych
#' @import plyr
#' @import parallel
#' @param data insert \code{data.frame} object.
#' @param model specify the mirt model if want to calibrate. accepting \code{mirt::mirt.model} object.
#' @param GCEvms insert google computing engine virtual machine information.
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
#' @return possible optimal combinations of models in list
#' @export
#'
#' @examples
#' \dontrun{
#' testMod1 <- estIRT(mirt::Science, model = 1)
#'
#' }
estIRT <- function(data, model = 1, GCEvms = NULL, GenRandomPars = T, NCYCLES = 4000, BURNIN = 1500, SEMCYCLES = 1000, covdata = NULL, fixed = ~1, random = list(), key = NULL, accelerate = "squarem",
    symmetric = F) {
    if (is.null(getOption("aefaConn"))) {
        getOption("aefaConn", aefaInit(GCEvms = GCEvms, debug = F))
    }

    combine <- function(x, y) {
        combn(y, x, paste, collapse = ", ")
    }

    randomEffectCandidates <- paste0("list(", unlist(lapply(0:NROW(random), combine, random)), ")")

    # config
    if (is.numeric(model)) {
        if (model == 1) {
            # UIRT
            if (max(psych::describe(data)$range) != 1) {
                # poly UIRT
                if (!is.null(key)) {
                  # with key
                  estItemtype <- c("4PLNRM", "3PLNRM", "3PLNRMu", "2PLNRM", "nominal", "gpcm", "gpcmIRT", "graded", "grsm", "grsmIRT", "Rasch", "rsm")
                } else {
                  # without key
                  estItemtype <- c("nominal", "gpcm", "gpcmIRT", "graded", "grsm", "grsmIRT", "Rasch", "rsm")
                }
            } else {
                # dich UIRT
                estItemtype <- c("4PL", "3PL", "3PLu", "2PL", "ideal", "Rasch")
            }
        } else {
            # MIRT
            if (max(psych::describe(data)$range) != 1) {
                # poly MIRT
                if (!is.null(key)) {
                  # poly MIRT with key
                  estItemtype <- c("4PLNRM", "3PLNRM", "3PLNRMu", "2PLNRM", "nominal", "gpcm", "graded", "grsm")
                } else {
                  # poly MIRT without key
                  estItemtype <- c("nominal", "gpcm", "graded", "grsm")
                }
            } else {
                # dich MIRT
                estItemtype <- c("4PL", "3PL", "3PLu", "2PL", "ideal")
            }
        }
    } else if (class(model) == "mirt.model") {
        # CFA
        if (max(psych::describe(data)$range) != 1) {
            # poly CFA
            if (!is.null(key)) {
                # with key
                estItemtype <- c("4PLNRM", "3PLNRM", "3PLNRMu", "2PLNRM", "nominal", "gpcm", "graded", "Rasch")
            } else {
                # without key
                estItemtype <- c("nominal", "gpcm", "graded", "Rasch")
            }
        } else {
            # dich
            estItemtype <- c("4PL", "3PL", "3PLu", "2PL", "PC3PL", "PC2PL", "ideal", "Rasch")
        }
    } else {
        stop("model is not correctly provided")
    }

    modConditional <- listenv::listenv()
    modUnConditional <- listenv::listenv()
    k <- 0

    # Conditional Model
    if (!is.null(covdata)) {
        for (i in 1:length(randomEffectCandidates)) {
            for (j in estItemtype) {
                if (!is.null(key) && sum(c("4PLNRM", "3PLNRM", "3PLNRMu", "2PLNRM") %in% j) > 0) {
                  k <- k + 1
                  modConditional[[k]] %<-% try(fitMLIRT(accelerate = accelerate, data = mirt::key2binary(data, key), model = model, itemtype = if (j == "4PLNRM")
                    "4PL" else if (j == "3PLNRM")
                    "3PL" else if (j == "3PLuNRM")
                    "3PLu" else if (j == "2PLNRM")
                    "2PL" else j, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric, covdata = covdata, fixed = fixed, random = eval(parse(text = randomEffectCandidates[i]))))
                } else {
                  k <- k + 1
                  modConditional[[k]] %<-% try(fitMLIRT(accelerate = accelerate, data = data, model = model, itemtype = j, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN,
                    SEMCYCLES = SEMCYCLES, symmetric = symmetric, covdata = covdata, fixed = fixed, random = eval(parse(text = randomEffectCandidates[i]))))
                }
            }
        }
    }

    l <- 0
    # UnConditional Model
    for (j in estItemtype) {
        l <- l + 1
        modUnConditional[[l]] %<-% try(mirt::mirt(data = data, model = model, method = "MHRM", itemtype = j, accelerate = accelerate, SE = T, GenRandomPars = GenRandomPars, key = key, calcNull = T,
            technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric)))
    }



    estModels <- list()

    # solve results
    if (!is.null(covdata)) {
        modConditional <- future::values(modConditional)
        if (NROW(modConditional) != 0) {
            for (j in 1:NROW(modConditional)) {
                estModels[[NROW(estModels) + 1]] <- modConditional[[j]]
            }
        }
    }

    modUnConditional <- future::values(modUnConditional)
    if (NROW(modUnConditional) != 0) {
        for (k in 1:NROW(modUnConditional)) {
            estModels[[NROW(estModels) + 1]] <- modUnConditional[[k]]
        }
    }

    finalEstModels <- list()
    noNullEstModels <- list()

    if (NROW(estModels) != 0) {
        for (i in 1:NROW(estModels)) {
            if (!is.null(estModels[[i]]) | length(estModels[[i]]) != 0) {
                noNullEstModels[[NROW(noNullEstModels) + 1]] <- estModels[[i]]
            }
        }

        if (NROW(noNullEstModels) != 0) {
            for (i in 1:NROW(noNullEstModels)) {
                if (sum(c("MixedClass", "SingleGroupClass") %in% class(noNullEstModels[[i]])) > 0) {
                  if (noNullEstModels[[i]]@OptimInfo$secondordertest) {
                    finalEstModels[[NROW(finalEstModels) + 1]] <- noNullEstModels[[i]]
                  }

                }
            }
        }

    }

    return(finalEstModels)
}

# exploratoryIRT.R
#' estimate appropriate exploratory full-information item factor analysis models with combinating random effects by number of factors
#'
#' @param data insert \code{data.frame} object.
#' @param minExtraction specify the minimum number of factors to calibrate. defaults is 1 but can change this.
#' @param maxExtraction specify the maximum number of factors to calibrate. defaults is 10 but can change this.
#' @param GCEvms insert google computing engine virtual machine information.
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
#'
#' @return possible optimal combinations of models in list
#' @export
#'
#' @examples
#' \dontrun{
#' testMod1 <- exploratoryIRT(mirt::Science, minExtraction = 1, maxExtraction = 2)
#'
#' }
exploratoryIRT <- function(data, minExtraction = 1, maxExtraction = if (ncol(data) < 10) ncol(data) else 10, GCEvms = NULL, GenRandomPars = T, NCYCLES = 4000, BURNIN = 1500, SEMCYCLES = 1000,
    covdata = NULL, fixed = ~1, random = list(), key = NULL, accelerate = "squarem", symmetric = F) {
    if (is.null(getOption("aefaConn"))) {
        getOption("aefaConn", aefaInit(GCEvms = GCEvms, debug = F))
    }

    estModels <- listenv::listenv()
    for (i in minExtraction:maxExtraction) {
        estModels[[i]] %<-% estIRT(data = data, model = i, GCEvms = GCEvms, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, covdata = covdata, fixed = fixed,
            random = random, key = key, accelerate = accelerate, symmetric = symmetric)
    }

    estModels <- unlist(as.list(estModels))

    finalEstModels <- list()
    noNullEstModels <- list()

    if (NROW(estModels) != 0) {
        for (i in 1:NROW(estModels)) {
          if (!is.null(estModels[[i]]) | length(estModels[[i]]) != 0) {
              noNullEstModels[[NROW(noNullEstModels) + 1]] <- estModels[[i]]
          }

        }

        if (NROW(noNullEstModels) != 0) {
            for (i in 1:NROW(noNullEstModels)) {
                if (sum(c("MixedClass", "SingleGroupClass") %in% class(noNullEstModels[[i]])) > 0) {
                  if (noNullEstModels[[i]]@OptimInfo$secondordertest) {
                    finalEstModels[[NROW(finalEstModels) + 1]] <- noNullEstModels[[i]]
                  }

                }
            }
        }

    }

    return(finalEstModels)

}

#' doing automated exploratory factor analysis (aefa) for research capability to identify unexplained factor structure with complexly cross-classified multilevel structured data in R environment
#' @param data insert \code{data.frame} object.
#' @param model specify the mirt model if you have want to calibrate. default is NULL to run exploratory models, but accepting \code{mirt::mirt.model} object.
#' @param minExtraction specify the minimum number of factors to calibrate. defaults is 1 but can change this. if model is not NULL, aefa will ignoring this.
#' @param maxExtraction specify the maximum number of factors to calibrate. defaults is 10 but can change this. if model is not NULL, aefa will ignoring this.
#' @param GCEvms insert google computing engine virtual machine information.
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
#' @return automated exploratory factor analytic models
#' @export
#'
#' @examples
#' \dontrun{
#' testMod1 <- aefa(mirt::Science, minExtraction = 1, maxExtraction = 2)
#'
#' }
aefa <- function(data, model = NULL, minExtraction = 1, maxExtraction = if (ncol(data) < 10) ncol(data) else 10, GCEvms = NULL, GenRandomPars = T, NCYCLES = 4000, BURNIN = 1500, SEMCYCLES = 1000,
    covdata = NULL, fixed = ~1, random = list(), key = NULL, accelerate = "squarem", symmetric = F, saveModelHistory = T, filename = "aefa.RDS", printItemFit = T, rotate = "bifactorQ") {
    # if (is.null(getOption("aefaConn"))) {
    #     getOption("aefaConn", aefaInit(GCEvms = GCEvms, debug = F))
    # }

    badItemNames <- c()

    modelHistoryCount <- 0
    if (saveModelHistory) {
        modelHistory <- list(estModelTrials = NULL, itemFitTrials = NULL)
    }

    STOP <- F

    while (!STOP) {
        # estimate
        if (is.null(model) && class(data) == "data.frame") {
            # run exploratory IRT
            try(estModel <- exploratoryIRT(data = data[!colnames(data) %in% badItemNames], minExtraction = minExtraction, maxExtraction = maxExtraction, GCEvms = GCEvms, GenRandomPars = GenRandomPars,
                NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, covdata = covdata, fixed = fixed, random = random, key = key, accelerate = accelerate, symmetric = symmetric))

        } else if (attr(class(model), "class") == "mirt.model" && class(data) == "data.frame") {
            # run confirmatory IRT
            try(estModel <- estIRT(data = data, model = model, GCEvms = GCEvms, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, covdata = covdata,
                fixed = fixed, random = random, key = key, accelerate = accelerate, symmetric = symmetric))
        }

      # Model Selection if model exists in data
        if (class(data) == "list" && NROW(data) > 0) {
            if (sum(c("MixedClass", "SingleGroupClass") %in% class(data[[1]])) > 0) {
                modDIC <- vector()
                for (i in 1:NROW(data)) {
                  modDIC[i] <- data[[i]]@Fit$DIC
                }
                estModel <- data[[which(modDIC == min(modDIC, na.rm = T))]]
                data <- estModel@Data$daata
            }
        } else if (class(estModel) == "list" && NROW(estModel) > 0) {
            if (sum(c("MixedClass", "SingleGroupClass") %in% class(estModel[[1]])) > 0) {
                modDIC <- vector()
                for (i in 1:NROW(estModel)) {
                  modDIC[i] <- estModel[[i]]@Fit$DIC
                }
                estModel <- estModel[[which(modDIC == min(modDIC, na.rm = T))]]
                data <- estModel@Data$daata
            }
        } else if(sum(c("MixedClass", "SingleGroupClass") %in% class(data)) > 0) {
          estModel <- data
          data <- estModel@Data$data
        }

        if (exists("estModel") && sum(c("MixedClass", "SingleGroupClass") %in% class(estModel)) > 0) {
            # evaluate model save model
            if (saveModelHistory) {
                modelHistoryCount <- modelHistoryCount + 1
                modelHistory$estModelTrials[modelHistoryCount] <- estModel
                saveRDS(modelHistory, filename)
            }

            estItemFit <- evaluateItemFit(estModel, GCEvms = GCEvms, rotate = rotate)
            if (printItemFit) {
                print(estItemFit)
            }

            # save model
            if (saveModelHistory) {
                modelHistory$itemFitTrials[modelHistoryCount] <- estItemFit
                saveRDS(modelHistory, filename)
            }

            # find bad item
            if (sum(colnames(estItemFit) %in% "Zh")) {
                if (sum(estItemFit$Zh < -1.96)) {
                  badItemNames <- c(badItemNames, estItemFit$item[which(estItemFit$Zh == min(estItemFit$Zh, na.rm = T))])
                } else {
                  STOP <- T
                }
            }

            # adjust model if supplied model is confirmatory model
            if (attr(class(model), "class") == "mirt.model" && class(data) == "data.frame") {
                for (i in 1:NROW(model)) {
                  model$x[i, 2] <- eval(parse(text = paste0("c(", gsub("-", ":", model$x[i, 2]), ")")))[!eval(parse(text = paste0("c(", gsub("-", ":", model$x[i, 2]), ")"))) %in% estItemFit$item[which(estItemFit$Zh ==
                    min(estItemFit$Zh, na.rm = T))]]  # convert elements
                }

            }

        } else {
            stop("estimations were failed. please retry with check your data or models")
        }

        if (ncol(estModel@Data$data) < 3) {
            message("model is not fit well")
            STOP <- T
        }

    }


    if(saveModelHistory){
      return(modelHistory)
    } else {
      return(estModel)
    }
}
