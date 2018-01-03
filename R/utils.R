# classify fixed and random effect variables
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

      # elemenate random vars if group size under 3
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
