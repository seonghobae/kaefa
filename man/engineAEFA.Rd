% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newEngine.R
\name{engineAEFA}
\alias{engineAEFA}
\title{estimate full-information item factor analysis models with combinating random effects}
\usage{
engineAEFA(data, model = 1, GenRandomPars = T, NCYCLES = 4000,
  BURNIN = 1500, SEMCYCLES = 1000, covdata = NULL, fixed = c(~1,
  ~0, ~-1), random = list(~1 | items), key = NULL,
  accelerate = "squarem", symmetric = F, resampling = T,
  samples = 5000, printDebugMsg = F, fitEMatUIRT = F,
  ranefautocomb = T, tryLCA = T, forcingMixedModelOnly = F,
  forcingQMC = F, turnOffMixedEst = F, anchor = NULL,
  skipggumInternal = F, powertest = F, idling = 60, leniency = F)
}
\arguments{
\item{data}{insert \code{data.frame} object.}

\item{model}{specify the mirt model if want to calibrate. accepting \code{mirt::mirt.model} object.}

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

\item{resampling}{Do you want to do resampling with replace? default is TRUE and activate nrow is over samples argument.}

\item{samples}{specify the number samples with resampling. default is 5000.}

\item{printDebugMsg}{Do you want to see the debugging messeages? default is FALSE}

\item{fitEMatUIRT}{Do you want to fit the model with EM at UIRT? default is FALSE}

\item{ranefautocomb}{Do you want to find global-optimal random effect combination? default is TRUE}

\item{tryLCA}{Do you want to try calibrate LCA model if avaliable? default is TRUE}

\item{forcingMixedModelOnly}{Do you want to forcing the Mixed model calibration? default is FALSE}

\item{forcingQMC}{Do you want to forcing the use QMC estimation instead MHRM? default is FALSE}

\item{turnOffMixedEst}{Do you want to turn off mixed effect (multilevel) estimation? default is FALSE}

\item{anchor}{Set the anchor item names If you want to consider DIF detection. default is NULL.}

\item{skipggumInternal}{Set the skipping ggum fitting procedure to speed up. default is FALSE.}

\item{powertest}{Set power test mode. default is FALSE.}

\item{idling}{Set seconds to idle. default is 60.}

\item{leniency}{skip second order test. default is FALSE}
}
\value{
possible optimal combinations of models in list
}
\description{
estimate full-information item factor analysis models with combinating random effects
}
\examples{
\dontrun{
testMod1 <- engineAEFA(mirt::Science, model = 1)

}
}
