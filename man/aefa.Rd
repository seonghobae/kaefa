% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kaefa.R
\name{aefa}
\alias{aefa}
\alias{efa}
\title{doing automated exploratory factor analysis (aefa) for research capability to identify unexplained factor structure with complexly cross-classified multilevel structured data in R environment}
\usage{
aefa(data, model = NULL, minExtraction = 1, maxExtraction = if
  (is.data.frame(data)) if (ncol(data) <= 10) max(c(1,
  (round(sqrt(ncol(data))) + 2))) else max(c(1, round(sqrt(ncol(data)))))
  else if (class(data) \%in\% c("SingleGroupClass", "MixedClass",
  "DiscreteClass", "MultipleGroupClass")) max(c(1,
  round(sqrt(ncol(data@Data$data))))) else if (class(data) \%in\% "aefa")
  max(c(1,
  round(sqrt(ncol(data$estModelTrials[[NROW(data$estModelTrials)]]@Data$data)))))
  else stop("Please provide data correctly."),
  RemoteClusters = getOption("kaefaServers"), sshKeyPath = NULL,
  GenRandomPars = T, NCYCLES = 4000, BURNIN = 1500,
  SEMCYCLES = 1000, covdata = NULL,
  fixed = kaefa:::.covdataFixedEffectComb(covdata),
  random = lapply(c(c(kaefa:::.covdataClassifieder(covdata)$random,
  "items"), if (length(kaefa:::.covdataClassifieder(covdata)$random) != 0)
  paste0(kaefa:::.covdataClassifieder(covdata)$random, ":items")), FUN =
  function(X) {     eval(parse(text = paste0("as.formula(", paste0("~1|",
  X), ")"))) }), key = NULL, accelerate = "squarem", symmetric = F,
  saveModelHistory = T, filename = "aefa.RDS", printItemFit = T,
  rotate = c("bifactorQ", "geominQ", "geominT", "bentlerQ", "bentlerT",
  "oblimin", "simplimax", "tandemII", "tandemI", "entropy", "quartimax"),
  resampling = T, samples = 5000, printDebugMsg = F,
  modelSelectionCriteria = "DIC", saveRawEstModels = F,
  fitEMatUIRT = F, ranefautocomb = T, PV_Q1 = T, tryLCA = F,
  forcingQMC = F, turnOffMixedEst = F, fitIndicesCutOff = 0.005,
  anchor = colnames(data), skipggum = F, powertest = F, idling = 0,
  leniency = F)
}
\arguments{
\item{data}{insert \code{data.frame} object.}

\item{model}{specify the mirt model if you have want to calibrate. default is NULL to run exploratory models, but accepting \code{mirt::mirt.model} object.}

\item{minExtraction}{specify the minimum number of factors to calibrate. defaults is 1 but can change this. if model is not NULL, aefa will ignoring this.}

\item{maxExtraction}{specify the maximum number of factors to calibrate. defaults is 10 but can change this. if model is not NULL, aefa will ignoring this.}

\item{RemoteClusters}{insert google computing engine virtual machine information.}

\item{sshKeyPath}{provide the SSH key path, NA is the placeholder.}

\item{GenRandomPars}{Try to generate Random Parameters? Default is TRUE}

\item{NCYCLES}{N Cycles of Robbin Monroe stage (stage 3). Default is 4000.}

\item{BURNIN}{N Cycles of Metro-hastings burnin stage (stage 1). Default is 1500.}

\item{SEMCYCLES}{N Cycles of Metro-hastings burnin stage (stage 2). Default is 1000.}

\item{covdata}{insert covariate data frame where use to fixed and random effect term. if not inserted, ignoring fixed and random effect estimation.}

\item{fixed}{a right sided R formula for specifying the fixed effect (aka 'explanatory') predictors from covdata and itemdesign.}

\item{random}{a right sided formula or list of formulas containing crossed random effects of the form \code{v1 + ... v_n | G}, where \code{G} is the grouping variable and \code{v_n} are random numeric predictors within each group. G may contain interaction terms, such as group:items to include cross or person-level interactions effects.}

\item{key}{item key vector of multiple choices test.}

\item{accelerate}{a character vector indicating the type of acceleration to use. Default is  'squarem' for the SQUAREM procedure (specifically, the gSqS3 approach)}

\item{symmetric}{force S-EM/Oakes information matrix to be symmetric? Default is FALSE to detect solutions that have not reached the ML estimate.}

\item{saveModelHistory}{Will you save the model calibration historys? default is TRUE}

\item{filename}{If you want to save the model calibration historys, Which one want you save file name? default is aefa.RDS, However can change this.}

\item{printItemFit}{Will you printing item fit indices during the calibrations? default is TRUE.}

\item{rotate}{set the rotate critera if mirt model is exploratory model. default is bifactorQ, however you can change this what you want to, like 'geominQ', 'bifactorT', 'geominT'. In current, Target rotation not supporting.}

\item{resampling}{Do you want to do resampling with replace? default is TRUE and activate nrow is over samples argument.}

\item{samples}{Specify the number samples with resampling. default is 5000.}

\item{printDebugMsg}{Do you want to see the debugging messeages? default is FALSE}

\item{modelSelectionCriteria}{Which critera want to use model selection work? 'DIC' (default), 'AIC', 'AICc', 'BIC', 'saBIC' available. AIC and DIC will be identical when no prior parameter distributions are included.}

\item{saveRawEstModels}{Do you want to save raw estimated models before model selection work? default is FALSE}

\item{fitEMatUIRT}{Do you want to fit the model with EM at UIRT? default is FALSE}

\item{ranefautocomb}{Do you want to find global-optimal random effect combination? default is TRUE}

\item{PV_Q1}{Do you want to get PV_Q1 (Chalmers & Ng, 2017) if can get it? default is TRUE.}

\item{tryLCA}{Do you want to try calibrate LCA model if avaliable? default is TRUE}

\item{forcingQMC}{Do you want to forcing the use QMC estimation instead MHRM? default is FALSE}

\item{turnOffMixedEst}{Do you want to turn off mixed effect (multilevel) estimation? default is FALSE}

\item{fitIndicesCutOff}{Specify item assessment cutoff. default is p < .005}

\item{anchor}{Set the anchor item names If you want to consider DIF detection. default is NULL.}

\item{skipggum}{Set the skipping ggum fitting procedure to speed up. default is FALSE.}

\item{powertest}{Set power test mode. default is FALSE.}

\item{idling}{Set seconds to idle. default is 0.}

\item{leniency}{skip second order test. default is FALSE}
}
\value{
automated exploratory factor analytic models
}
\description{
doing automated exploratory factor analysis (aefa) for research capability to identify unexplained factor structure with complexly cross-classified multilevel structured data in R environment
}
\examples{
\dontrun{
testMod1 <- aefa(mirt::Science, minExtraction = 1, maxExtraction = 2)

}
}
