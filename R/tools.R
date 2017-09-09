# tools.R

fitMLIRT <-
  function(data = data,
           model = model,
           itemtype = NULL,
           accelerate = accelerate,
           GenRandomPars = GenRandomPars,
           NCYCLES = NCYCLES,
           BURNIN = BURNIN,
           SEMCYCLES = SEMCYCLES,
           symmetric = symmetric,
           covdata = covdata,
           fixed = fixed,
           random = random) {
    modMLIRT_itemLevel <- mirt::mixedmirt(
      data = data,
      model = model,
      accelerate = accelerate,
      itemtype = itemtype,
      SE = T,
      GenRandomPars = GenRandomPars,
      covdata = covdata,
      fixed = fixed,
      random = random,
      calcNull = T,
      technical = list(
        NCYCLES = NCYCLES,
        BURNIN = BURNIN,
        SEMCYCLES = SEMCYCLES,
        symmetric = symmetric
      )
    )
    modMLIRT_latentLevel <- mirt::mixedmirt(
      data = data,
      model = model,
      accelerate = accelerate,
      itemtype = itemtype,
      SE = T,
      GenRandomPars = GenRandomPars,
      covdata = covdata,
      lr.fixed = fixed,
      lr.random = random,
      calcNull = T,
      technical = list(
        NCYCLES = NCYCLES,
        BURNIN = BURNIN,
        SEMCYCLES = SEMCYCLES,
        symmetric = symmetric
      )
    )

    # evaluate model
    if (exists('modMLIRT_itemLevel')) {
      if (class(modMLIRT_itemLevel) != 'list') {
        if (!modMLIRT_itemLevel@OptimInfo$secondordertest) {
          rm(modMLIRT_itemLevel)
        }
      } else {
        rm(modMLIRT_itemLevel)
      }

    }


    if (exists('modMLIRT_latentLevel')) {
      if (class(modMLIRT_latentLevel) != 'list') {
        if (!modMLIRT_latentLevel@OptimInfo$secondordertest) {
          rm(modMLIRT_latentLevel)
        }
      } else {
        rm(modMLIRT_latentLevel)
      }

    }

    # decision
    if (exists('modMLIRT_itemLevel') &&
        exists('modMLIRT_latentLevel')) {
      if (modMLIRT_itemLevel@Fit$DIC < modMLIRT_latentLevel@Fit$DIC) {
        return(modMLIRT_itemLevel)
      } else {
        return(modMLIRT_latentLevel)
      }
    } else if (exists('modMLIRT_itemLevel') &&
               !exists('modMLIRT_latentLevel')) {
      return(modMLIRT_itemLevel)
    } else if (!exists('modMLIRT_itemLevel') &&
               exists('modMLIRT_latentLevel')) {
      return(modMLIRT_latentLevel)
    } else {
      # stop('no solution')
    }

  }


evaluateItemFit <-
  function(mirtModel,
           GCEvms = NULL,
           rotate = 'bifactorQ') {
    if (attr(class(mirtModel), 'package') == 'mirt') {
      # check mirtModel is mirt object
      if (!require('future')) {
        install.packages('future', repos = 'http://cloud.r-project.org')
      }
      require('future') # GET RID OFF WHEN PRODUCTION

      if (!require('mirt')) {
        install.packages('mirt', repos = 'http://cloud.r-project.org')
      }
      require('mirt') # GET RID OFF WHEN PRODUCTION

      if (!require('progress')) {
        install.packages('progress', repos = 'http://cloud.r-project.org')
      }
      require('progress') # GET RID OFF WHEN PRODUCTION

      if (!require('plyr')) {
        install.packages('plyr', repos = 'http://cloud.r-project.org')
      }
      require('plyr') # GET RID OFF WHEN PRODUCTION

      if (!require('psych')) {
        install.packages('psych', repos = 'http://cloud.r-project.org')
      }
      require('psych') # GET RID OFF WHEN PRODUCTION

      # load cluster
      if (!is.null(GCEvms)) {
        parallel::mclapply(1:length(GCEvms),
                           future::plan(list(
                             future::tweak(future::cluster, workers = future::as.cluster(GCEvms)),
                             future::multiprocess
                           )),
                           mc.cores = length(GCEvms))
      } else {
        if (length(grep('openblas', extSoftVersion()["BLAS"])) > 0) {
          future::plan(future::multiprocess)
        } else if (length(future::availableWorkers()) == 1) {
          future::plan(future::sequential)
        } else {
          suppressMessages(try(future::plan(strategy = list(
            future::tweak(future::cluster),
            future::multiprocess
          )), silent = T))
          # mirt::mirtCluster()

        }

      }

      # item fit evaluation
      modFit_Zh %<-% try(mirt::itemfit(
        mirtModel,
        rotate = rotate,
        fit_stats = 'Zh',
        QMC = T,
        method = 'MAP',
        impute = if (sum(is.na(mirtModel@Data$data)) > 0)
          100
        else
          0
      ),
      silent = T)

      modFit_SX2 %<-% try(mirt::itemfit(
        mirtModel,
        rotate = rotate,
        fit_stats = 'S_X2',
        QMC = T,
        method = 'MAP',
        impute = if (sum(is.na(mirtModel@Data$data)) > 0)
          100
        else
          0
      ),
      silent = T)

      if (mirtModel@Model$nfact == 1) {
        modFit_PVQ1 %<-% try(mirt::itemfit(
          mirtModel,
          rotate = rotate,
          fit_stats = 'PV_Q1',
          QMC = T,
          method = 'MAP'
        ),
        silent = T)

      }

      if (sum(mirtModel@Model$itemtype %in% 'Rasch') > 0 &&
          mirtModel@Model$nfact == 1) {
        modFit_infit %<-% try(mirt::itemfit(
          mirtModel,
          rotate = rotate,
          fit_stats = 'infit',
          QMC = T,
          method = 'MAP',
          impute = if (sum(is.na(mirtModel@Data$data)) > 0)
            100
          else
            0
        ),
        silent = T)
      }

      # check item fit indices are exists
      itemFitList <-
        c('modFit_Zh',
          'modFit_SX2',
          'modFit_PVQ1',
          'modFit_infit')[c(
            exists('modFit_Zh'),
            exists('modFit_SX2'),
            exists('modFit_PVQ1'),
            exists('modFit_infit')
          )]

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
