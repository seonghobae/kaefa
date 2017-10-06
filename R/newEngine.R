# newEngine.R
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
#' @param resampling Do you want to do resampling with replace? default is FALSE, and it will be activate under unconditional model only.
#' @param samples specify the number samples with resampling. default is 5000.
#' @param printDebugMsg Do you want to see the debugging messeages? default is FALSE
#' @param fitEMatUIRT Do you want to fit the model with EM at UIRT? default is FALSE
#' @param ranefautocomb Do you want to find global-optimal random effect combination? default is TRUE
#'
#' @return possible optimal combinations of models in list
#' @export
#'
#' @examples
#' \dontrun{
#' testMod1 <- engineAEFA(mirt::Science, model = 1)
#'
#' }
engineAEFA <- function(data, model = 1, GenRandomPars = T, NCYCLES = 4000, BURNIN = 1500, SEMCYCLES = 1000, covdata = NULL, fixed = ~1,
    random = list(), key = NULL, accelerate = "squarem", symmetric = F, resampling = F, samples = 5000, printDebugMsg = F, fitEMatUIRT = F,
    ranefautocomb = T) {

    # data management: resampling
    if (resampling && is.null(covdata)) {
        resampleCaseNumber <- sample(1:nrow(data), samples, replace = F)
        data <- data[resampleCaseNumber, ]
        if (!is.null(covdata)) {
            covdata <- covdata[resampleCaseNumber, ]
        }
    }

    # data management: exclude range == 0
    data <- data[psych::describe(data)$range != 0]

    # data management: exclude k > 30
    testLength <- vector()
    nK <- vector()
    for (i in 1:ncol(data)) {
        nK[i] <- length(attributes(factor(data[, i]))$levels)
        testLength[i] <- nK[i] > 30
    }
    data <- data[!testLength]
    nK <- nK[!testLength]

    # aefaConn if (is.null(getOption('aefaConn')) && is.null(GCEvms)) { getOption('aefaConn', aefaInit(GCEvms = GCEvms, debug =
    # printDebugMsg)) }

    # tools
    combine <- function(x, y) {
        combn(y, x, paste, collapse = ", ")
    }

    if (ranefautocomb) {
        randomEffectCandidates <- paste0("list(", unlist(lapply(0:NROW(random), combine, random)), ")")
    } else {
        randomEffectCandidates <- paste0("list(", unlist(lapply(NROW(random):NROW(random), combine, random)), ")")
    }

    # start engine

    exploratoryModels <- listenv::listenv()
    for (i in model) {
        # exploratory i th model

        # config
        if (is.numeric(i)) {
            if (i == 1) {
                # UIRT
                if (max(nK) != 1) {
                  # poly UIRT
                  if (!is.null(key)) {
                    # with key
                    estItemtype <- c("4PLNRM", "3PLNRM", "3PLNRMu", "2PLNRM", "nominal", "gpcm", "gpcmIRT", "graded", "grsm", "grsmIRT",
                      "Rasch", "rsm")
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
                if (max(nK) != 1) {
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

        } else if (class(i) == "mirt.model") {
            # CFA
            if (max(nK) != 1) {
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


      if (sum(max(nK) == nK) != length(nK)) {
        if (length(grep("rsm", estItemtype)) > 0) {
          estItemtype <- estItemtype[-grep("rsm", estItemtype)]
        }
      }

        exploratoryModels[[i]] %<-% {
            modConditional <- listenv::listenv()
            modUnConditional <- listenv::listenv()
            modDiscrete <- listenv::listenv()

            for (j in estItemtype) {
                # itemtype j for model i
                modUnConditional[[j]] %<-% {
                  if (sum(c("grsmIRT", "gpcmIRT", "spline", "rsm") %in% j) == 0 | (!fitEMatUIRT && i != 1)) {
                    try(mirt::mirt(data = data, model = i, method = "MHRM", itemtype = j, accelerate = accelerate, SE = T, GenRandomPars = GenRandomPars,
                      key = key, calcNull = T, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric)))
                  } else {
                    try(mirt::mirt(data = data, model = i, method = "EM", itemtype = j, accelerate = accelerate, SE = T, GenRandomPars = GenRandomPars,
                      key = key, calcNull = T, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric)))
                  }
                }
                modConditional[[j]] %<-% {
                  if (!is.null(covdata)) {
                    modConditionalTemp <- listenv::listenv()
                    for (k in 1:randomEffectCandidates) {
                      # and
                      modConditionalTemp[[k]] %<-% {
                        if (!is.null(key) && sum(c("4PLNRM", "3PLNRM", "3PLNRMu", "2PLNRM") %in% j) > 0) {
                          try(fitMLIRT(accelerate = accelerate, data = mirt::key2binary(data, key), model = i, itemtype = if (j ==
                            "4PLNRM")
                            "4PL" else if (j == "3PLNRM")
                            "3PL" else if (j == "3PLuNRM")
                            "3PLu" else if (j == "2PLNRM")
                            "2PL" else j, GenRandomPars = GenRandomPars, NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric,
                            covdata = covdata, fixed = fixed, random = eval(parse(text = k))))
                        } else {
                          if (sum(c("grsmIRT", "gpcmIRT", "spline", "rsm") %in% j) == 0) {
                            try(fitMLIRT(accelerate = accelerate, data = data, model = model, itemtype = j, GenRandomPars = GenRandomPars,
                              NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric, covdata = covdata,
                              fixed = fixed, random = eval(parse(text = k))))
                          } else {
                            # Skipping at Conditional Model, see https://github.com/philchalmers/mirt/issues/122#issuecomment-329969581
                          }
                        }
                      }
                    }
                    unlist(as.list(modConditionalTemp))  # unlist k
                  }
                }
            }

            # LCA
            if (class(model) == "numeric") {
                modDiscrete %<-% {
                  modDiscreteTemp <- listenv::listenv()
                  for (m in c("sandwich", "Oakes")) {
                    for (n in c(T, F)) {
                      modDiscreteTemp[[NROW(as.list(modDiscreteTemp)) + 1]] %<-%
                        try(mirt::mdirt(data = data, model = i, SE = T, SE.type = m, accelerate = accelerate, GenRandomPars = GenRandomPars,
                          empiricalhist = n, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric),
                          covdata = covdata, formula = if (fixed == ~1)
                            NULL else fixed))

                    }
                  }
                  unlist(as.list(modDiscreteTemp))
                }
            }

            # wrap up i th model
            estModels <- list()

            # solve results
            if (!is.null(covdata)) {
                modConditional <- try(as.list(modConditional))
                if (exists("modConditional")) {
                  if (NROW(modConditional) != 0) {
                    for (jj in 1:NROW(modConditional)) {
                      estModels[[NROW(estModels) + 1]] <- modConditional[[jj]]
                    }
                  }
                }
            }

            modUnConditional <- try(as.list(modUnConditional))
            if (exists("modUnConditional")) {
                if (NROW(modUnConditional) != 0) {
                  for (kk in 1:NROW(modUnConditional)) {
                    estModels[[NROW(estModels) + 1]] <- modUnConditional[[kk]]
                  }
                }
            }

            modDiscrete <- try(as.list(modDiscrete))
            if (exists("modDiscrete")) {
                if (NROW(modDiscrete) != 0) {
                  for (kk in 1:NROW(modDiscrete)) {
                    estModels[[NROW(estModels) + 1]] <- modDiscrete[[kk]]
                  }
                }
            }

            finalEstModels <- list()
            noNullEstModels <- list()

            if (NROW(estModels) != 0) {
                for (ii in 1:NROW(estModels)) {
                  if (!is.null(estModels[[ii]]) | length(estModels[[ii]]) != 0) {
                    noNullEstModels[[NROW(noNullEstModels) + 1]] <- estModels[[ii]]
                  }
                }

                if (NROW(noNullEstModels) != 0) {
                  for (i in 1:NROW(noNullEstModels)) {
                    if (class(noNullEstModels[[ii]]) %in% c("MixedClass", "SingleGroupClass", "DiscreteClass")) {
                      if (noNullEstModels[[ii]]@OptimInfo$secondordertest) {
                        finalEstModels[[NROW(finalEstModels) + 1]] <- noNullEstModels[[ii]]
                      }

                    }
                  }
                }
            }
            finalEstModels
        }  # EOF of exploratoryModels i
    }  # EOF of for loop

    exploratoryModels <- unlist(as.list(exploratoryModels))
    finalEstModels <- list()
    noNullEstModels <- list()

    if (NROW(exploratoryModels) != 0) {
      for (i in 1:NROW(exploratoryModels)) {
        if (!is.null(exploratoryModels[[i]]) | length(exploratoryModels[[i]]) != 0) {
          noNullEstModels[[NROW(noNullEstModels) + 1]] <- exploratoryModels[[i]]
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
