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
#' @param resampling Do you want to do resampling with replace? default is TRUE and activate nrow is over samples argument.
#' @param samples specify the number samples with resampling. default is 5000.
#' @param printDebugMsg Do you want to see the debugging messeages? default is FALSE
#' @param fitEMatUIRT Do you want to fit the model with EM at UIRT? default is FALSE
#' @param ranefautocomb Do you want to find global-optimal random effect combination? default is TRUE
#' @param tryLCA Do you want to try calibrate LCA model if avaliable? default is TRUE
#' @param forcingMixedModelOnly Do you want to forcing the Mixed model calibration? default is FALSE
#' @param forcingQMC Do you want to forcing the use QMC estimation instead MHRM? default is FALSE
#' @param turnOffMixedEst Do you want to turn off mixed effect (multilevel) estimation? default is FALSE
#'
#' @return possible optimal combinations of models in list
#' @export
#'
#' @examples
#' \dontrun{
#' testMod1 <- engineAEFA(mirt::Science, model = 1)
#'
#' }
engineAEFA <- function(data, model = 1, GenRandomPars = T, NCYCLES = 4000, BURNIN = 1500,
    SEMCYCLES = 1000, covdata = NULL, fixed = c(~1, ~0, ~-1), random = list(~1 |
        items), key = NULL, accelerate = "squarem", symmetric = F, resampling = T,
    samples = 5000, printDebugMsg = F, fitEMatUIRT = F, ranefautocomb = T, tryLCA = T,
    forcingMixedModelOnly = F, forcingQMC = F, turnOffMixedEst = F) {

    # data management: resampling
    if (resampling && nrow(data) > samples) {
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

    # aefaConn if (is.null(getOption('aefaConn')) && is.null(GCEvms)) {
    # getOption('aefaConn', aefaInit(GCEvms = GCEvms, debug = printDebugMsg)) }

    # tools
    combine <- function(x, y) {
        combn(y, x, paste, collapse = ", ")
    }

    if (ranefautocomb) {
        randomEffectCandidates <- paste0("list(", unlist(lapply(0:NROW(random), combine,
            random)), ")")
    } else {
        randomEffectCandidates <- paste0("list(", unlist(lapply(NROW(random):NROW(random),
            combine, random)), ")")
    }

    # start engine

    exploratoryModels <- listenv::listenv()

    modConditional1 <- listenv::listenv()
    modConditional2 <- listenv::listenv()
    modUnConditional <- listenv::listenv()
    modDiscrete <- listenv::listenv()

    for (i in model) {
        # exploratory i th model

      if(is.numeric(i)){
        if(i > ncol(data)){
          break()
        }
      }

        # config
        if (is.numeric(i)) {

            if (i == 1) {
                # UIRT
                if (max(nK[is.finite(nK)], na.rm = T) > 2) {
                  # poly UIRT
                  if (!is.null(key)) {
                    # with key
                    estItemtype <- c("4PLNRM", "3PLNRM", "3PLNRMu", "2PLNRM", "nominal",
                      "gpcm", "gpcmIRT", "graded", "grsm", "grsmIRT", "Rasch", "rsm",
                      "monopoly", "ggum")
                  } else {
                    # without key
                    estItemtype <- c("nominal", "gpcm", "gpcmIRT", "graded", "grsm",
                      "grsmIRT", "Rasch", "rsm", "monopoly", "ggum")
                  }
                } else {
                  # dich UIRT
                  estItemtype <- c("4PL", "3PL", "3PLu", "2PL", "ideal", "Rasch",
                    "spline", "monopoly")
                }
            } else {
                # MIRT
                if (max(nK[is.finite(nK)], na.rm = T) > 2) {
                  # poly MIRT
                  if (!is.null(key)) {
                    # poly MIRT with key
                    estItemtype <- c("4PLNRM", "3PLNRM", "3PLNRMu", "2PLNRM", "nominal",
                      "gpcm", "graded", "grsm", "ggum")
                  } else {
                    # poly MIRT without key
                    estItemtype <- c("nominal", "gpcm", "graded", "grsm", "ggum")
                  }
                } else {
                  # dich MIRT
                  estItemtype <- c("4PL", "3PL", "3PLu", "2PL", "ideal")
                }
            }

        } else if (class(i) == "mirt.model") {
            # CFA
            if (max(nK[is.finite(nK)], na.rm = T) > 2) {
                # poly CFA
                if (!is.null(key)) {
                  # with key
                  estItemtype <- c("4PLNRM", "3PLNRM", "3PLNRMu", "2PLNRM", "nominal",
                    "gpcm", "graded", "Rasch", "ggum")
                } else {
                  # without key
                  estItemtype <- c("nominal", "gpcm", "graded", "Rasch", "ggum")
                }
            } else {
                # dich
                estItemtype <- c("4PL", "3PL", "3PLu", "2PL", "PC3PL", "PC2PL", "ideal",
                  "Rasch")
            }
        } else {
            stop("model is not correctly provided")
        }


        if (sum(max(nK[is.finite(nK)], na.rm = T) == nK) != length(nK[is.finite(nK)])) {
            if (length(grep("rsm", estItemtype)) > 0) {
                estItemtype <- estItemtype[-grep("rsm", estItemtype)]
            }
        }

      for (j in estItemtype) {
        # itemtype j for model i

        message("\ncalibrating ", j, ' model ', ': ', if(is.numeric(i)) as.character(i) else ('User specified CFA model'))
        if (!forcingMixedModelOnly) {

          message("mirt::mirt calibration (normal MIRT)\n")
          modUnConditional[[paste(paste0(as.character(i), collapse = ""),
                                  j, collapse = " ")]] %<-% {

            if (sum(c("grsmIRT", "gpcmIRT", "spline", "rsm", "monopoly") %in%
                    j) == 0) {
              if (forcingQMC) {
                estMethod <- "QMCEM"
              } else {
                estMethod <- "MHRM"
              }
              tryCatch(mirt::mirt(data = data, model = i, method = estMethod,
                                  itemtype = j, accelerate = accelerate, SE = T, GenRandomPars = GenRandomPars,
                                  key = key, calcNull = T, technical = list(NCYCLES = NCYCLES,
                                                                            BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric)),
                       error = function(e) {
                       })
            } else {
              tryCatch(mirt::mirt(data = data, model = i, method = "EM",
                                  itemtype = j, accelerate = accelerate, SE = T, GenRandomPars = GenRandomPars,
                                  key = key, calcNull = T, technical = list(NCYCLES = NCYCLES,
                                                                            BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric)),
                       error = function(e) {
                       })
            }
          }
        }
        # FIXME: CONSIDER TO REMOVE THIS: TO SPEED UP; ESTIMATE RANDOM EFFECTS DIRECTLY
        # if (!is.null(covdata)) {} # try to calibrate mixed-effect even covdata is null
        # anyway -- 2017. 11. 10
        if (!turnOffMixedEst && sum(c("grsmIRT", "gpcmIRT", "spline", "rsm", "monopoly") %in%
                                    j) == 0) {
          message("\nmirt::mixedmirt calibration (multilevel/mixed-effect MIRT)\n")
          for (k in randomEffectCandidates) {
            # and
            for (k_fixed in fixed) {
              modConditional1[[paste(paste0(as.character(i), collapse = ""),
                                    j, paste0(as.character(k_fixed), collapse = ""),
                                    k, collapse = " ")]] %<-% {
                                      if (!is.null(key) && sum(c("4PLNRM", "3PLNRM", "3PLNRMu",
                                                                 "2PLNRM") %in% j) > 0) {
                                        tryCatch(mirt::mixedmirt(data = mirt::key2binary(data,
                                                                                         key), model = i,
                                                                 accelerate = accelerate, itemtype = if (j == "4PLNRM")
                                                                   "4PL" else if (j == "3PLNRM")
                                                                     "3PL" else if (j == "3PLuNRM")
                                                                       "3PLu" else if (j == "2PLNRM")
                                                                         "2PL" else j, SE = T, GenRandomPars = GenRandomPars,
                                                                 covdata = covdata, fixed = k_fixed, random = eval(parse(text = k)),
                                                                 calcNull = T, technical = list(NCYCLES = NCYCLES,
                                                                                                BURNIN = BURNIN,
                                                                                                SEMCYCLES = SEMCYCLES,
                                                                                                symmetric = symmetric)),
                                                 error = function(e) {
                                                 })

                                      } else {
                                        tryCatch(mirt::mixedmirt(data = data, model = i,
                                                                 accelerate = accelerate, itemtype = j, SE = T, GenRandomPars = GenRandomPars,
                                                                 covdata = covdata, fixed = k_fixed, random = eval(parse(text = k)),
                                                                 calcNull = T, technical = list(NCYCLES = NCYCLES,
                                                                                                BURNIN = BURNIN,
                                                                                                SEMCYCLES = SEMCYCLES,
                                                                                                symmetric = symmetric)), error = function(e) {
                                                                                                })
                                      }
                                    }
              modConditional2[[paste(paste0(as.character(i), collapse = ""),
                                     j, paste0(as.character(k_fixed), collapse = ""),
                                     k, collapse = " ")]] %<-% {
                                       if (!is.null(key) && sum(c("4PLNRM", "3PLNRM", "3PLNRMu",
                                                                  "2PLNRM") %in% j) > 0) {
                                         tryCatch(mirt::mixedmirt(data = mirt::key2binary(data,
                                                                                          key), model = i,
                                                                  accelerate = accelerate, itemtype = if (j == "4PLNRM")
                                                                    "4PL" else if (j == "3PLNRM")
                                                                      "3PL" else if (j == "3PLuNRM")
                                                                        "3PLu" else if (j == "2PLNRM")
                                                                          "2PL" else j, SE = T, GenRandomPars = GenRandomPars,
                                                                  covdata = covdata, lr.fixed = k_fixed, lr.random = eval(parse(text = k)),
                                                                  calcNull = T, technical = list(NCYCLES = NCYCLES,
                                                                                                 BURNIN = BURNIN,
                                                                                                 SEMCYCLES = SEMCYCLES,
                                                                                                 symmetric = symmetric)),
                                                  error = function(e) {
                                                  })

                                       } else {
                                         tryCatch(mirt::mixedmirt(data = data, model = i,
                                                                  accelerate = accelerate, itemtype = j, SE = T, GenRandomPars = GenRandomPars,
                                                                  covdata = covdata, lr.fixed = k_fixed, lr.random = eval(parse(text = k)),
                                                                  calcNull = T, technical = list(NCYCLES = NCYCLES,
                                                                                                 BURNIN = BURNIN,
                                                                                                 SEMCYCLES = SEMCYCLES,
                                                                                                 symmetric = symmetric)), error = function(e) {
                                                                                                 })
                                       }
                                     }
            }
          }
        }
      }

      # LCA
      if (is.numeric(i) && tryLCA) {
        message("Latent Class Model calibration")

        for (m in c("sandwich", "Oakes")) {
          for (n in c(T, F)) {
            for (k_fixed in fixed) {
              modDiscrete[[paste(paste0(as.character(i), collapse = ""),
                                     m, paste0(as.character(n), collapse = ""),
                                     as.character(k), collapse = " ")]] %<-%
                tryCatch(mirt::mdirt(data = data, model = i, SE = T, SE.type = m,
                                     accelerate = accelerate, GenRandomPars = GenRandomPars,
                                     empiricalhist = n, technical = list(NCYCLES = NCYCLES,
                                                                         BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric),
                                     covdata = covdata, formula = if (k_fixed == ~1)
                                       NULL else k_fixed), error = function(e) {
                                       })
            }
          }
        }
      }
    }  # EOF of for loop

    exploratoryModels <- unlist(list(as.list(modUnConditional), as.list(modConditional1), as.list(modConditional2), as.list(modDiscrete)))
    # exploratoryModels # for debug random effect model

    # # improper solution filter
    finalEstModels <- list()
    noNullEstModels <- list()

    if (NROW(exploratoryModels) != 0) {
        for (i in 1:NROW(exploratoryModels)) {
            if (!is.null(exploratoryModels[[i]]) | length(exploratoryModels[[i]]) !=
                0) {
                noNullEstModels[[NROW(noNullEstModels) + 1]] <- exploratoryModels[[i]]
            }
        }

        if (NROW(noNullEstModels) != 0) {
            for (i in 1:NROW(noNullEstModels)) {
                if (class(noNullEstModels[[i]]) %in% c("MixedClass", "SingleGroupClass",
                  "DiscreteClass")) {
                  if (noNullEstModels[[i]]@OptimInfo$secondordertest) {
                    if (is.numeric(noNullEstModels[[i]]@Model$model) && class(noNullEstModels[[i]]) ==
                      "MixedClass") {
                      if (noNullEstModels[[i]]@Model$model > 1) {
                        noNullEstModels[[i]]@Options$exploratory <- TRUE
                      }
                    }
                    finalEstModels[[NROW(finalEstModels) + 1]] <- noNullEstModels[[i]]
                  }

                }
            }
        }
    }
    return(finalEstModels)
}
