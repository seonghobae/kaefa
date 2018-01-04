# classify fixed and random effect variables
#' @export
  .covdataClassifieder <- function(a){
    if(!is.null(a)){
      if(sum(class(a) %in% "tbl_df") != 0){
        a <- as.data.frame(a)
      }
      # change as factor
      for(i in 1:ncol(a)){
        a[,i] <- as.factor(a[,i])
      }

      # classify fixed and random
      fixedVars <- vector()
      randomVars <- vector()
      for(i in 1:ncol(a)){
        if(length(levels(a[,i])) <= 30){
          fixedVars <- c(fixedVars, i)
        } else {
          randomVars <- c(randomVars, i)
        }
      }

      # test group size to move random
      if(length(fixedVars) != 0){
        gotoRandom <- vector()
        for(i in 1:ncol(a)){
          if(max(table(a[,i]))/min(table(a[,i])) < 2){

          } else {
            gotoRandom[length(gotoRandom) + 1] <- i
          }
        }
        if(length(randomVars) != 0){
          fixedVars <- fixedVars[!fixedVars %in% gotoRandom]
          randomVars <- c(randomVars, gotoRandom)
        }
      }

      # elemenate random vars if group size under 2
      if(length(randomVars)){
        excludeRandomVars <- vector()
        for(i in randomVars){
          if(min(table(a[,i])) > 1){

          } else {
            excludeRandomVars <- c(excludeRandomVars, i)
          }
        }
        randomVars <- randomVars[!randomVars %in% excludeRandomVars]
      }

      list(fixed = colnames(a[unique(fixedVars)]), random = colnames(a[unique(randomVars)]))
    } else {
      list(fixed = NULL, random = NULL)
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
      fixedVarsComb <- c(c(paste0('~1', paste0(' + ', fixedVarsComb)), paste0('~0', paste0(' + ', fixedVarsComb)), paste0('~-1', paste0(' + ', fixedVarsComb)))[!c(paste0('~1', paste0(' + ', fixedVarsComb)), paste0('~0', paste0(' + ', fixedVarsComb)), paste0('~-1', paste0(' + ', fixedVarsComb))) %in% c("~1 + ", "~0 + " ,  "~-1 + ")], '~1', '~0', '~-1')
    } else {
      fixedVarsComb <- c(~1, ~0, ~-1)
    }

    ret <- list()
    for(i in 1:length(fixedVarsComb)){
      ret[[i]] <- as.character(fixedVarsComb[i])
    }

    ret
  }

# parameter linking Mixed-Effect to SingleClass Class temporaly
#' @export
  .exportParmsEME <- function(mirtModel){
    if (class(mirtModel)[1] == "MixedClass") {
      message("\n")
      mirt::summary(mirtModel)
      message("\n")
      modMLM <- mirt::mirt(data = mirtModel@Data$data, model = mirtModel@Model$model,
                           SE = T, itemtype = mirtModel@Model$itemtype, pars = "values")
      modMLM_original <- mirt::mod2values(mirtModel)
      if (sum(modMLM_original$name == "(Intercept)") != 0) {
        modMLM_original <- modMLM_original[!modMLM_original$name == "(Intercept)",
                                           ]

      }
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
