#' kaefa: kwangwoon automated exploratory factor analysis
#'
#' This library stands for improving research capability to identify
#' unexplained factor structure with complexly cross-classified
#' multilevel structured data in R environment
#' in the automated exploratory factor analysis framework.
#'
#' In practice of applied psychological resarch, so much researcher ignoring
#' the impact of the MMMM (Multiple Membership Multilevel Model) and MM
#' in exploratory factor analysis from unfamiliar with statistical innovations
#' who noted in Sharpe (2013) and Foster, Min, and Zickar (2017).
#'
#' Moreover, A lot of researcher do not know what is the improper solution
#' in the exploratory factor analysis. That may lead wrong conclusion to
#' research society, The kaefa avoid possible improper solutions during
#' the exploratory factor analysis.
#'
#' The kaefa may inspect this issues from the MMMM or MM
#' in statistical learning theory perspectives using model
#' selection criteria like the DIC (Kang, 2008; Kang, Cohen, & Sung, 2009;
#' Jiao, Kamata, Wang, & Jin, 2012; Jiao & Zhang, 2015).
#'
#' This software can be pallelise to multiple computers via LAN
#' even heterogeneous environment, so that applied researchers may expand
#' their research capability more easy with kaefa
#' even data has too complicated structure.
#'
#' If researcher provide of demographical information in kaefa,
#' kaefa will inspect the optimal number of factor and optimal IRT model,
#' and possible error variances or latent differences from
#' demographic information of respondents.
#'
#' After the \emph{n-th} calibration, kaefa do the item appropriateness test
#' for check which item contribute to explain conceptual criterion with
#' robustness of aberrant response using \emph{Zh, S-X2, PV-Q1}.
#'
#' During the calibration, kaefa consider the these item response models:
#' Rasch, 2PL, 3PL, 3PLu, 4PL, ideal (for dichotomous)
#' nominal, gpcm, graded, grsm, ggum, pcm, rsm, monopoly (for polytomous).
#'
#' Moreover, factor rotation will decide automatically using \emph{Zh} for
#' minimizing potential outage of the item as actual criteria.
#' As the default, "bifactorQ","geominQ", "geominT", "bentlerQ", "bentlerT",
#' "oblimin", "oblimax", "simplimax", "tandemII", "tandemI", "entropy",
#' and "quartimax" will try to inspect the structure of actual criteria
#' reflect to the conceptual criterion.
#'
#' This project started in 2013, and restructured in 2017.
#'
#' @name kaefa
#' @title kwangwoon automated exploratory factor analysis.
#' @author Seongho Bae \email{seongho@kw.ac.kr}
#' @references
#' Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory
#' Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' \doi{10.18637/jss.v048.i06}
#' @references
#' Chalmers, R. P. (2015). Extended Mixed-Effects Item Response Models
#' with the MH-RM algorithm. \emph{Journal of Educational Measurement, 52}(2), 200–222.
#' \doi{10.1111/jedm.12072}
#' @references
#' Foster, G. C., Min, H., & Zickar, M. J. (2017). Review of Item Response Theory
#' Practices in Organizational Research.
#' \emph{Organizational Research Methods, 20}(3), 465–486.
#' \doi{10.1177/1094428116689708}
#' @references
#' Jennrich, R. I., & Bentler, P. M. (2011). Exploratory Bi-Factor Analysis.
#' \emph{Psychometrika, 76}(4), 537–549.
#' \doi{10.1007/s11336-011-9218-4}
#' @references
#' Jiao, H., Kamata, A., Wang, S., & Jin, Y. (2012). A Multilevel Testlet Model
#' for Dual Local Dependence. \emph{Journal of Educational Measurement, 49}(1), 82-100.
#' \doi{10.1111/j.1745-3984.2011.00161.x}
#' @references
#' Jiao, H., & Zhang, Y. (2015). Polytomous multilevel testlet models for testlet-based
#' assessments with complex sampling designs.
#' \emph{British Journal of Mathematical and Statistical Psychology, 68}(1), 65–83.
#' \doi{10.1111/bmsp.12035}
#' @references
#' Kang, T. (2008). Application of Statistical Model Selection Methods to Assessing
#' Test Dimensionality. \emph{Journal of Educational Evaluation, 21}(4), 153–175.
#' Retrieved from \url{http://scholar.dkyobobook.co.kr/searchDetail.laf?barcode=4010022701731}
#' @references
#' Kang, T., Cohen, A. S., & Sung, H.-J. (2009). Model Selection Indices for
#' Polytomous Items. \emph{Applied Psychological Measurement, 33}(7), 499–518.
#' \doi{10.1007/s00330-011-2364-3}
#' @references
#' Mansolf, M., & Reise, S. P. (2016). Exploratory Bifactor Analysis:
#' The Schmid-Leiman Orthogonalization and Jennrich-Bentler Analytic Rotations.
#' \emph{Multivariate Behavioral Research, 51}(5), 698–717.
#' \doi{10.1080/00273171.2016.1215898}
#' @references
#' Preacher, K. J., Zhang, G., Kim, C., & Mels, G. (2013). Choosing the optimal
#' number of factors in exploratory factor analysis: A model selection perspective.
#' \emph{Multivariate Behavioral Research, 48}(1), 28–56.
#' \doi{10.1080/00273171.2012.710386}
#' @references
#' Reise, S. P., & Waller, N. G. (2009). Item Response Theory and Clinical Measurement.
#' \emph{Annual Review of Clinical Psychology, 5}(1), 27–48.
#' \doi{10.1146/annurev.clinpsy.032408.153553}
#' @references
#' Reise, S. P. (2012). The Rediscovery of Bifactor Measurement Models.
#' \emph{Multivariate Behavioral Research, 47}(5), 667–696.
#' \doi{10.1080/00273171.2012.715555}
#' @references
#' Sharpe, D. (2013). Why the resistance to statistical innovations?
#' Bridging the communication gap. \emph{Psychological Methods, 18}(4), 572–582.
#' \doi{10.1037/a0034177}
#' @docType package
NULL
