# classify fixed and random effect variables
#' @export
  .covdataClassifieder <- function(a){
    decimalplaces <- function(x) {
      if ((x %% 1) != 0) {
        nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
      } else {
        return(0)
      }
    }

    if (!is.null(a)) {
      if ("tbl_df" %in% class(a)) {
        a_tmp_colnames <- colnames(a)
        a <- as.data.frame(a)
        colnames(a) <- a_tmp_colnames
      }

      # marking integers: NEED TO FIX! (reserved)
      markInt <- vector()
      markNum <- vector()
      markCat <- vector()
      for (i in 1:ncol(a)) {
        if (is.numeric(a[[i]]) | is.integer(a[[i]])) {
          if(!is.null(attr(attr(a[[i]], 'labels'),'names'))){ # for SPSS label support
            markCat[length(markCat) + 1] <- i
          } else if(sum(sapply(a[[i]], decimalplaces) > 0) == 0){
            markInt[length(markInt) + 1] <- i
          } else if(sum(sapply(a[[i]], decimalplaces) > 0) >= 0){
            markNum[length(markNum) + 1] <- i
          }
        }
        else {
          markCat[length(markCat) + 1] <- i
        }
      }

      # change as factor in temporal
      for(i in 1:ncol(a)){
        a[[i]] <- as.factor(a[[i]])
      }

      # classify number, fixed and random
      fixedVars <- vector()
      randomVars <- vector()
      numericVars <- vector() # age, number of team members, ..., etc.

      # convert categorical variables into factor
      if(!is.null(markCat)){
        for (i in markCat) {
          a[[i]] <- as.factor(a[[i]])
        }
      }
        # classify fixed and random first
        for (i in markCat) {
          if(length(levels(a[[i]])) <= 50){ # if k <= 50 (group level <= 50)
            fixedVars <- c(fixedVars, i)
          } else {
            randomVars <- c(randomVars, i) # if k > 50
          }
        }

        # make a decision which variable to move fixed to random by group size balancing
        if(length(fixedVars) != 0){
          gotoRandom <- vector()
          for(i in markCat){
            if(max(table(a[[i]]))/min(table(a[[i]])) < 2){

            } else {  # if fixed group has unbalanced levels (group a has 4 members, group b has 60...)
              gotoRandom[length(gotoRandom) + 1] <- i
            }
          }

          if(length(gotoRandom) != 0){
            fixedVars <- fixedVars[!fixedVars %in% gotoRandom]
            randomVars <- c(randomVars, gotoRandom)
          }
        }

        # elemenate random vars if group size under 2 -- that may numeric / integer?
        if(length(randomVars) != 0){
          excludeRandomVars <- vector()

          for(i in randomVars){
            if(sum(table(a[[i]]) == 1) > sqrt(length(a[[i]]))){ # if a lot of individual cases, do not execlude

            } else {
              if(min(table(a[[i]])) > 1){

              } else {
                excludeRandomVars <- c(excludeRandomVars, i)
              }
            }
          }
          randomVars <- randomVars[!randomVars %in% excludeRandomVars]
          fixedVars <- c(fixedVars, excludeRandomVars)
        }

      # numeric and int
      numericVars <- colnames(a)[c(markInt, markNum)]
      fixedVars <- c(fixedVars, numericVars)
      randomVars <- randomVars[!randomVars %in% numericVars]

      retFixed <- tryCatch(colnames(a[unique(fixedVars)]), error = function(e){NULL})
      retRandom <- tryCatch(colnames(a[unique(randomVars)]), error = function(e){NULL})
      list(fixed = retFixed, random = retRandom, categorical = colnames(a)[markCat])
    } else {
      list(fixed = NULL, random = NULL, categorical = NULL)
    }
  }

# fixed effect combination
#' @export
  .covdataFixedEffectComb <- function(a){
    combine <- function(x, y) {
      combn(y, x, paste, collapse = " + ")
    }

    if(length(.covdataClassifieder(a)$fixed) != 0){
      fixedVarsComb <- paste0(unlist(lapply(0:NROW(.covdataClassifieder(a)$fixed), combine,
                                            .covdataClassifieder(a)$fixed)))
      # fixedVarsComb <- c(c(paste0('~1', paste0(' + ', fixedVarsComb)), paste0('~0', paste0(' + ', fixedVarsComb)), paste0('~-1', paste0(' + ', fixedVarsComb)))[!c(paste0('~1', paste0(' + ', fixedVarsComb)), paste0('~0', paste0(' + ', fixedVarsComb)), paste0('~-1', paste0(' + ', fixedVarsComb))) %in% c("~1 + ", "~0 + " ,  "~-1 + ")], '~1', '~0', '~-1')
      fixedVarsComb <- c(c(paste0('~1', paste0(' + ', fixedVarsComb)))[!c(paste0('~1', paste0(' + ', fixedVarsComb))) %in% c("~1 + ")], '~1')
    } else {
      # fixedVarsComb <- c(~1, ~0, ~-1)
      fixedVarsComb <- c(~1)
    }

    ret <- list()
    for(i in 1:length(fixedVarsComb)){
      ret[[i]] <- as.character(fixedVarsComb[i])
    }

    unlist(ret)
  }

# parameter linking Mixed-Effect to SingleClass Class temporaly
#' @export
  .exportParmsEME <- function(mirtModel, quiet = F){
    if (class(mirtModel)[1] == "MixedClass") {
      if(quiet){

      } else {
        message("\n")
        mirt::summary(mirtModel)
        message("\n")
      }
      modMLM <- mirt::mirt(data = mirtModel@Data$data, model = mirtModel@Model$model,
                           SE = T, itemtype = mirtModel@Model$itemtype, pars = "values")
      modMLM_original <- mirt::mod2values(mirtModel)
      if (sum(modMLM_original$name == "(Intercept)") != 0) {
        modMLM_original <- modMLM_original[!modMLM_original$name == "(Intercept)",]

      }
      modMLM_original <- modMLM_original[modMLM_original$name %in% intersect(modMLM_original$name, modMLM$name),]
      modMLM$value[which(modMLM$item %in% colnames(mirtModel@Data$data))] <- modMLM_original$value[which(modMLM_original$item %in%
                                                                                                           colnames(mirtModel@Data$data))]
      modMLM$est <- F

      if ("grsm" %in% mirtModel@Model$itemtype) {
        mirtModel <- mirt::mirt(data = mirtModel@Data$data, model = mirtModel@Model$model,
                                itemtype = mirtModel@Model$itemtype, pars = modMLM, method = "QMCEM",
                                SE = F, calcNull = F, technical = list(internal_constraints = FALSE))
      } else {
        mirtModel <- mirt::mirt(data = mirtModel@Data$data, model = mirtModel@Model$model,
                                itemtype = mirtModel@Model$itemtype, pars = modMLM, method = "QMCEM",
                                SE = F, calcNull = F)
      }
      if (is.numeric(mirtModel@Model$model)) {
        if (mirtModel@Model$model > 1) {
          mirtModel@Options$exploratory <- TRUE
        }
      }
    }
    mirtModel
  }

# MIRT wrapper
#' @export
#'
  .mirt <- function(data = NULL, model = 1, method = "EM",
                    itemtype = "graded", accelerate = "squarem", SE = T, GenRandomPars = T,
                    key = NULL, calcNull = T, NCYCLES = 4000, BURNIN = 1000, SEMCYCLES = 1500, symmetric = F,
                    group = NULL, anchor = colnames(data), leniency = F){
    invisible(gc())
    if(is.null(group)){
      mod <- mirt::mirt(data = data, model = model, method = method,
                        itemtype = itemtype, accelerate = accelerate, SE = SE, GenRandomPars = GenRandomPars,
                        key = key, calcNull = calcNull, technical = list(NCYCLES = NCYCLES,
                                                                         BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric))
    } else {
      mod <- mirt::multipleGroup(data = data, model = model, method = method,
                        itemtype = itemtype, accelerate = accelerate, SE = SE, GenRandomPars = GenRandomPars,
                        key = key, calcNull = calcNull, technical = list(NCYCLES = NCYCLES,
                                                                         BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric),
                        invariance = anchor, group = group)
    }

    if(exists('mod')){
      if(mod@OptimInfo$converged){
        if(leniency){
          mod
        } else {
          if(mod@OptimInfo$secondordertest){
            mod
          } else {
            NULL
          }
        }
      } else {
        NULL
      }
    }
  }

# EMEIRT wrapper
#' @export
#'
  .mixedmirt <- function(data = NULL, model = 1,
                    itemtype = "graded", accelerate = "squarem", SE = T, GenRandomPars = T, covdata = NULL,
                    fixed = ~1, random = NULL, lr.fixed = ~1, lr.random = NULL,
                    calcNull = T, NCYCLES = 4000, BURNIN = 1000, SEMCYCLES = 1500, symmetric = F, leniency = F){
    invisible(gc())
    mod <- mirt::mixedmirt(data = data, model = model,
                           accelerate = accelerate, itemtype = itemtype, SE = SE, GenRandomPars = GenRandomPars,
                           covdata = covdata, fixed = fixed, random = random, lr.fixed = lr.fixed, lr.random = lr.random,
                           calcNull = calcNull, technical = list(NCYCLES = NCYCLES,
                                                          BURNIN = BURNIN,
                                                          SEMCYCLES = SEMCYCLES,
                                                          symmetric = symmetric))
    if(exists('mod')){
      if(mod@OptimInfo$converged){
        if(leniency){
          mod
        } else {
          if(mod@OptimInfo$secondordertest){
            mod
          } else {
            NULL
          }
        }
      } else {
        NULL
      }
    }
  }
