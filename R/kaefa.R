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
fitMLIRT <- function(data = data, model = model, itemtype = NULL, accelerate = accelerate, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES,
    symmetric = symmetric, covdata = covdata, fixed = fixed, random = random) {
    modMLIRT_itemLevel <- mirt::mixedmirt(data = data, model = model, accelerate = accelerate, itemtype = itemtype, SE = T, GenRandomPars = GenRandomPars, covdata = covdata, fixed = fixed,
        random = random, calcNull = T, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric))
    modMLIRT_latentLevel <- mirt::mixedmirt(data = data, model = model, accelerate = accelerate, itemtype = itemtype, SE = T, GenRandomPars = GenRandomPars, covdata = covdata,
        lr.fixed = fixed, lr.random = random, calcNull = T, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric))

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

    # convert mixedclass to singleclass temporary
    if (class(mirtModel)[1] == "MixedClass") {
        modMLM <- mirt::mirt(data = mirtModel@Data$data, model = mirtModel@Model$model, SE = T, itemtype = mirtModel@Model$itemtype, pars = "values")
        modMLM_original <- mirt::mod2values(mirtModel)
        if (sum(modMLM_original$name == "(Intercept)") != 0) {
            modMLM_original <- modMLM_original[!modMLM_original$name == "(Intercept)", ]

        }
        modMLM$value[which(modMLM$item %in% colnames(mirtModel@Data$data))] <- modMLM_original$value[which(modMLM_original$item %in% colnames(mirtModel@Data$data))]
        modMLM$est <- F

        mirtModel <- mirt::mirt(data = mirtModel@Data$data, model = mirtModel@Model$model, itemtype = mirtModel@Model$itemtype, pars = modMLM, method = "QMCEM", SE = F, calcNull = T)
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
#' @import future
#' @import listenv
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
#' @param resampling Do you want to do resampling with replace? default is TRUE, and it will be activate under unconditional model only.
#' @param samples specify the number samples with resampling. default is 5000.
#' @param printDebugMsg Do you want to see the debugging messeages? default is FALSE
#'
#' @return possible optimal combinations of models in list
#' @export
#'
#' @examples
#' \dontrun{
#' testMod1 <- estIRT(mirt::Science, model = 1)
#'
#' }
estIRT <- function(data, model = 1, GCEvms = NULL, GenRandomPars = T, NCYCLES = 4000, BURNIN = 1500, SEMCYCLES = 1000, covdata = NULL, fixed = ~1, random = list(), key = NULL,
    accelerate = "squarem", symmetric = F, resampling = T, samples = 5000, printDebugMsg = F) {
    # data management: resampling
    if (resampling && is.null(covdata)) {
        resampleCaseNumber <- sample(1:nrow(data), samples, replace = T)
        data <- data[resampleCaseNumber, ]
        if (!is.null(covdata)) {
            covdata <- covdata[resampleCaseNumber, ]
        }
    }

    # data management: exclude range == 0
    data <- data[psych::describe(data)$range != 0]

    # aefaConn
    if (is.null(getOption("aefaConn")) && is.null(GCEvms)) {
        getOption("aefaConn", aefaInit(GCEvms = GCEvms, debug = printDebugMsg))
    }

    # tools
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
                estItemtype <- c("4PL", "3PL", "3PLu", "2PL", "ideal", "Rasch", "spline")
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
    modDiscrete <- listenv::listenv()
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
                  if (sum(c("grsmIRT", "gpcmIRT", "spline", "rsm") %in% j) == 0) {
                    modConditional[[k]] %<-% try(fitMLIRT(accelerate = accelerate, data = data, model = model, itemtype = j, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN,
                      SEMCYCLES = SEMCYCLES, symmetric = symmetric, covdata = covdata, fixed = fixed, random = eval(parse(text = randomEffectCandidates[i]))))
                  } else {
                    # Skipping, see https://github.com/philchalmers/mirt/issues/122#issuecomment-329969581
                  }

                }
            }
        }
    }

    l <- 0
    # UnConditional Model
    for (j in estItemtype) {
        l <- l + 1
        if (sum(c("grsmIRT", "gpcmIRT", "spline", "rsm") %in% j) == 0) {
            modUnConditional[[l]] %<-% try(mirt::mirt(data = data, model = model, method = "MHRM", itemtype = j, accelerate = accelerate, SE = T, GenRandomPars = GenRandomPars,
                key = key, calcNull = T, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric)))
        } else {
            modUnConditional[[l]] %<-% try(mirt::mirt(data = data, model = model, method = "EM", itemtype = j, accelerate = accelerate, SE = T, GenRandomPars = GenRandomPars, key = key,
                calcNull = T, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric)))
        }

    }


    if (class(model) == "numeric") {
        mm <- 0
        for (m in c("sandwich", "Oakes", "sandwich.Louis", "SEM")) {
            for (n in c(T, F)) {
                mm <- mm + 1
                modDiscrete[[mm]] %<-% try(mirt::mdirt(data = data, model = model, SE = T, SE.type = m, accelerate = accelerate, SE = T, GenRandomPars = GenRandomPars, empiricalhist = n,
                  calcNull = T, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric, covdata = covdata, formula = if (fixed == ~1) NULL else fixed)))
            }
        }
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

    modDiscrete <- future::values(modDiscrete)
    if (NROW(modDiscrete) != 0) {
        for (k in 1:NROW(modDiscrete)) {
            estModels[[NROW(estModels) + 1]] <- modDiscrete[[k]]
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
                if (class(noNullEstModels[[i]]) %in% c("MixedClass", "SingleGroupClass", "DiscreteClass")) {
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
#' @param resampling Do you want to do resampling with replace? default is TRUE, and it will be activate under unconditional model only.
#' @param samples specify the number samples with resampling. default is 5000.
#' @param printDebugMsg Do you want to see the debugging messeages? default is FALSE

#' @return possible optimal combinations of models in list
#' @export
#'
#' @examples
#' \dontrun{
#' testMod1 <- exploratoryIRT(mirt::Science, minExtraction = 1, maxExtraction = 2)
#'
#' }
exploratoryIRT <- function(data, model = NULL, minExtraction = 1, maxExtraction = if (ncol(data) < 10) ncol(data) else 10, GCEvms = NULL, GenRandomPars = T, NCYCLES = 4000, BURNIN = 1500,
    SEMCYCLES = 1000, covdata = NULL, fixed = ~1, random = list(), key = NULL, accelerate = "squarem", symmetric = F, resampling = T, samples = 5000, printDebugMsg = F) {
    if (is.null(getOption("aefaConn"))) {
        getOption("aefaConn", aefaInit(GCEvms = GCEvms, debug = printDebugMsg))
    }

    estModels <- listenv::listenv()
    for (i in minExtraction:maxExtraction) {
        # EFA
        estModels[[i]] %<-% try(estIRT(data = data, model = i, GCEvms = GCEvms, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, covdata = covdata,
            fixed = fixed, random = random, key = key, accelerate = accelerate, symmetric = symmetric, resampling = resampling, samples = samples, printDebugMsg = printDebugMsg))
    }

    if (!is.null(model)) {
        # user specified EFA or CFA
        j <- maxExtraction
        model <- unlist(list(model))
        for (i in 1:NROW(model)) {
            if (class(model[[i]]) == "mirt.model" | class(model[[i]]) == "numeric") {
                estModels[[j + i]] %<-% try(estIRT(data = data, model = model[[i]], GCEvms = GCEvms, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES,
                  covdata = covdata, fixed = fixed, random = random, key = key, accelerate = accelerate, symmetric = symmetric, resampling = resampling, samples = samples, printDebugMsg = printDebugMsg))
            }
        }
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
                if (class(noNullEstModels[[i]]) %in% c("MixedClass", "SingleGroupClass", "DiscreteClass")) {
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
#' @param resampling Do you want to do resampling with replace? default is TRUE, and it will be activate under unconditional model only.
#' @param samples specify the number samples with resampling. default is 5000.
#' @param printDebugMsg Do you want to see the debugging messeages? default is FALSE
#' @param modelSelectionCriteria Which critera want to use model selection work? 'DIC' (default), 'AIC', 'AICc', 'BIC', 'saBIC' available.
#' @param saveRawEstModels Do you want to save raw estimated models before model selection work? default is FALSE
#'
#' @return automated exploratory factor analytic models
#' @export
#'
#' @examples
#' \dontrun{
#' testMod1 <- aefa(mirt::Science, minExtraction = 1, maxExtraction = 2)
#'
#' }
aefa <- function(data, model = NULL, minExtraction = 1, maxExtraction = if (ncol(data) < 10) ncol(data) else 10, GCEvms = NULL, GenRandomPars = T, NCYCLES = 4000, BURNIN = 1500,
    SEMCYCLES = 1000, covdata = NULL, fixed = ~1, random = list(), key = NULL, accelerate = "squarem", symmetric = F, saveModelHistory = T, filename = "aefa.RDS", printItemFit = T,
    rotate = "bifactorQ", resampling = T, samples = 5000, printDebugMsg = F, modelSelectionCriteria = "DIC", saveRawEstModels = F) {
    # if (is.null(getOption('aefaConn'))) { getOption('aefaConn', aefaInit(GCEvms = GCEvms, debug = F)) }

    badItemNames <- c()

    modelHistoryCount <- 0
    if (saveModelHistory) {
        modelHistory <- list(rawEstModels = list(), estModelTrials = list(), itemFitTrials = list())
    }

    STOP <- F

    while (!STOP) {
        # estimate run exploratory IRT and confirmatory IRT
        if ((is.data.frame(data) | is.matrix(data))) {
            # general condition
            try(estModel <- exploratoryIRT(data = data.frame(data[, !colnames(data) %in% badItemNames]), model = model, minExtraction = minExtraction, maxExtraction = maxExtraction,
                GCEvms = GCEvms, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, covdata = covdata, fixed = fixed, random = random, key = key,
                accelerate = accelerate, symmetric = symmetric, resampling = resampling, samples = samples, printDebugMsg = printDebugMsg))
        } else if (is.list(data)) {
            # Some weird condition: user specified pre-calibrated model or list of data.frame in data
            modelFound <- F
            dfFound <- F
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
                  estModel[[NROW(estModel) + 1]] <- try(exploratoryIRT(data = data.frame(data[[i]][, !colnames(data[[i]]) %in% badItemNames]), model = model, minExtraction = minExtraction,
                    maxExtraction = maxExtraction, GCEvms = GCEvms, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, covdata = covdata,
                    fixed = fixed, random = random, key = key, accelerate = accelerate, symmetric = symmetric, resampling = resampling, samples = samples, printDebugMsg = printDebugMsg))
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
                try(saveRDS(modelHistory, filename))
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
          estModel <- estModel[[(which(modModelFit == min(modModelFit, na.rm = T))[1])]]
          if (modelFound | dfFound) {
            data <- estModel@Data$data  # to switch general condition
          } # End Of Model selection



          if (class(estModel) %in% c("MixedClass", "SingleGroupClass", "DiscreteClass")) {
            # evaluate model save model
            if (saveModelHistory) {
              modelHistory$estModelTrials[[modelHistoryCount]] <- estModel
              try(saveRDS(modelHistory, filename))
            }

            estItemFit <- evaluateItemFit(estModel, GCEvms = GCEvms, rotate = rotate)
            if (printItemFit) {
              try(print(estItemFit))
            }

            # save model
            if (saveModelHistory) {
              modelHistory$itemFitTrials[[modelHistoryCount]] <- estItemFit
              try(saveRDS(modelHistory, filename))
            }

            # find bad item
            if ("Zh" %in% colnames(estItemFit)) {
              # set Zh as default item fit index
              if (sum(estItemFit$Zh < -1.96) != 0) {
                badItemNames <- c(badItemNames, estItemFit$item[which(estItemFit$Zh == min(estItemFit$Zh, na.rm = T))])
              } else {
                STOP <- T
              }
            } else if ("p.PV_Q1" %in% colnames(estItemFit)) {
              # prevent unexpected situation
              if (sum(is.na(estItemFit$p.PV_Q1)) != 0) {
                badItemNames <- c(badItemNames, estItemFit$item[which(is.na(estItemFit$p.PV_Q1))])
              } else if (sum(estItemFit$p.PV_Q1 < 0.025) != 0) {
                badItemNames <- c(badItemNames, estItemFit$item[which(estItemFit$p.PV_Q1 == min(estItemFit$p.PV_Q1, na.rm = T))])
              } else {
                STOP <- T
              }
            } else if ("p.S_X2" %in% colnames(estItemFit)) {
              # prevent unexpected situation and DiscreteClass evaluation
              if (sum(is.na(estItemFit$p.S_X2)) != 0) {
                badItemNames <- c(badItemNames, estItemFit$item[which(is.na(estItemFit$p.S_X2))])
              } else if (sum(estItemFit$p.S_X2 < 0.025) != 0) {
                badItemNames <- c(badItemNames, estItemFit$item[which(estItemFit$p.S_X2 == min(estItemFit$p.S_X2, na.rm = T))])
              } else {
                STOP <- T
              }
            }

            # adjust model if supplied model is confirmatory model
            if (!is.null(model) && (!is.numeric(model) | !is.integer(model))) {
              for (i in 1:NROW(model)) {
                for (j in 1:nrow(model[[i]])) {
                  if (!model[[i]]$x[j, 1] %in% c("COV", "MEAN", "FREE", "NEXPLORE")) {
                    model[[i]]$x[j, 2] <- eval(parse(text = paste0("c(", gsub("-", ":", model[[i]]$x[j, 2]), ")")))[!eval(parse(text = paste0("c(", gsub("-", ":", model[[i]]$x[j,
                                                                                                                                                                                2]), ")"))) %in% estItemFit$item[which(estItemFit$Zh == min(estItemFit$Zh, na.rm = T))]]  # convert elements
                  }
                }
              }
            }

          }

            # if (ncol(estModel@Data$data) > 3) {} else {
            #     message("model is not fit well")
            #     STOP <- T  # set flag of 'stop while loop'
            # }



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
