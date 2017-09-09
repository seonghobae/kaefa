# tools.R

#' fit appropriate multilevel item response model automatically with mirt::mixedmirt
#'
#' @param data insert data.frame object.
#' @param model specify the mirt model if want to calibrate. accepting mirt::mirt.model() object.
#' @param GenRandomPars Try to generate Random Parameters? Default is TRUE
#' @param NCYCLES N Cycles of Robbin Monroe stage (stage 3). Default is 4000.
#' @param BURNIN N Cycles of Metro-hastings burnin stage (stage 1). Default is 1500.
#' @param SEMCYCLES N Cycles of Metro-hastings burnin stage (stage 2). Default is 1000.
#' @param covdata insert covariate data frame where use to fixed and random effect term. if not inserted, ignoring fixed and random effect estimation.
#' @param fixed a right sided R formula for specifying the fixed effect (aka 'explanatory') predictors from covdata and itemdesign.
#' @param random a right sided formula or list of formulas containing crossed random effects of the form v1 + ... v_n | G, where G is the grouping variable and v_n are random numeric predictors within each group. G may contain interaction terms, such as group:items to include cross or person-level interactions effects.
#' @param accelerate a character vector indicating the type of acceleration to use. Default is  'squarem' for the SQUAREM procedure (specifically, the gSqS3 approach)
#' @param symmetric force S-EM/Oakes information matrix to be symmetric? Default is FALSE to detect solutions that have not reached the ML estimate.
#' @param itemtype set the calibration item type
#'
#' @return appropriate mirt::mixedeffect models in list vector
#' @export
#'
#' @examples
#' \dontrun{
#' testModel1 <- fitMLIRT(mirt::Science, covdata = mirt::Science, random = list())
#'
#'}
fitMLIRT <- function(data = data, model = model, itemtype = NULL, accelerate = accelerate, GenRandomPars = GenRandomPars, 
    NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric, covdata = covdata, 
    fixed = fixed, random = random) {
    modMLIRT_itemLevel <- mirt::mixedmirt(data = data, model = model, accelerate = accelerate, itemtype = itemtype, 
        SE = T, GenRandomPars = GenRandomPars, covdata = covdata, fixed = fixed, random = random, calcNull = T, 
        technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric))
    modMLIRT_latentLevel <- mirt::mixedmirt(data = data, model = model, accelerate = accelerate, itemtype = itemtype, 
        SE = T, GenRandomPars = GenRandomPars, covdata = covdata, lr.fixed = fixed, lr.random = random, 
        calcNull = T, technical = list(NCYCLES = NCYCLES, BURNIN = BURNIN, SEMCYCLES = SEMCYCLES, symmetric = symmetric))
    
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
#' @param mirtModel insert estimated mirt::mirt or mirt::mixedmirt model.
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

    if (!exists(".conn")) {
        stop('please run the aefaInit() first')
    }
    
    if (attr(class(mirtModel), "package") == "mirt") {
        # item fit evaluation
        modFit_Zh <- listenv()
        modFit_Zh %<-% try(mirt::itemfit(mirtModel, rotate = rotate, fit_stats = "Zh", QMC = T, method = "MAP", 
            impute = if (sum(is.na(mirtModel@Data$data)) > 0) 
                100 else 0), silent = T)
        
        modFit_SX2 <- listenv()
        modFit_SX2 %<-% try(mirt::itemfit(mirtModel, rotate = rotate, fit_stats = "S_X2", QMC = T, method = "MAP", 
            impute = if (sum(is.na(mirtModel@Data$data)) > 0) 
                100 else 0), silent = T)
        
        if (mirtModel@Model$nfact == 1) {
            modFit_PVQ1 <- listenv()
            modFit_PVQ1 %<-% try(mirt::itemfit(mirtModel, rotate = rotate, fit_stats = "PV_Q1", QMC = T, 
                method = "MAP"), silent = T)
            
        }
        
        if (sum(mirtModel@Model$itemtype %in% "Rasch") > 0 && mirtModel@Model$nfact == 1) {
            modFit_infit <- listenv()
            modFit_infit %<-% try(mirt::itemfit(mirtModel, rotate = rotate, fit_stats = "infit", QMC = T, 
                method = "MAP", impute = if (sum(is.na(mirtModel@Data$data)) > 0) 
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
        
        itemFitList <- c("modFit_Zh", "modFit_SX2", "modFit_PVQ1", "modFit_infit")[c(exists("modFit_Zh"), 
            exists("modFit_SX2"), exists("modFit_PVQ1"), exists("modFit_infit"))]
        
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
