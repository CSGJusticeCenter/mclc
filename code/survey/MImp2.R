#######
###aggregated data: national level
###IMPUTED DATA m=1,2,3,4,5
micelist <- c("mice_imputed_data1","mice_imputed_data2","mice_imputed_data3","mice_imputed_data4","mice_imputed_data5")
for (h in 1:5) {
  for (i in tablevals) {
    natmice <- setNames(aggregate(get(micelist[h])[[i]], 
                                  by=list(Category=get(micelist[h])$year), FUN=sum)[2],i)
    assign(paste0(i,h),natmice,envir = .GlobalEnv)
  }
}
for (i in 1:5) {
  nat <- do.call(cbind,mget(paste0(tablevals,i))) %>%
    mutate(across(where(is.numeric),formattable::comma,1)) %>%
    cbind(year)
  assign(paste0(micelist[i],".nat"),nat,envir = .GlobalEnv)
}
for (i in 1:5) {
  test <- get(paste0(micelist[i],".nat"))
  test = upData(get(paste0(micelist[i],".nat")), labels = var.labels)
  assign(paste0(micelist[i],".nat"),test,envir = .GlobalEnv)
}
#calculate 95% CI for all counts across all years
#assume Poisson Distribution using count data
for (h in 1:5) {
  for (i in tablevals) {
    for (j in yearnum) { 
      test<-data.frame(CI95=poisson.test(get(paste0(micelist[h],".nat"))[j,i], conf.level = 0.95)$conf.int[1:2])
      colnames(test)[colnames(test)=="CI95"] <- paste0(i,"1.",j)
      assign(paste0(i,"1.",j,".m",h),test,envir = .GlobalEnv)
    }
  }
}
for (i in 1:5) {
  CI <- do.call(cbind,mget(c(paste0(tablevals,"1.1.m",i),paste0(tablevals,"1.2.m",i),paste0(tablevals,"1.3.m",i)))) %>%
    mutate(across(where(is.numeric),formattable::comma,1))
  assign(paste0(micelist[i],".natCI"),CI,envir = .GlobalEnv)
}
#rename columns to indicate which imputed data frame
for (i in 1:5) {
  nameit <- get(paste0(micelist[i],".nat"))
  names(nameit)[names(nameit) == "overall_admissions"]                         <- "overall_admissions1"
  names(nameit)[names(nameit) == "probation_violation_admissions"]             <- "probation_violation_admissions1"
  names(nameit)[names(nameit) == "parole_violation_admissions"]                <- "parole_violation_admissions1"
  names(nameit)[names(nameit) == "total_technical_violation_admissions"]       <- "total_technical_violation_admissions1"
  names(nameit)[names(nameit) == "technical_probation_violation_admissions"]   <- "technical_probation_violation_admissions1"
  names(nameit)[names(nameit) == "technical_parole_violation_admissions"]      <- "technical_parole_violation_admissions1"
  names(nameit)[names(nameit) == "total_new_offense_violation_admissions"]     <- "total_new_offense_violation_admissions1"
  names(nameit)[names(nameit) == "new_offense_probation_violation_admissions"] <- "new_offense_probation_violation_admissions1"
  names(nameit)[names(nameit) == "new_offense_parole_violation_admissions"]    <- "new_offense_parole_violation_admissions1"
  
  names(nameit)[names(nameit) == "overall_population"]                         <- "overall_population1"
  names(nameit)[names(nameit) == "probation_violation_population"]             <- "probation_violation_population1"
  names(nameit)[names(nameit) == "parole_violation_population"]                <- "parole_violation_population1"
  names(nameit)[names(nameit) == "total_technical_violation_population"]       <- "total_technical_violation_population1"
  names(nameit)[names(nameit) == "technical_probation_violation_population"]   <- "technical_probation_violation_population1"
  names(nameit)[names(nameit) == "technical_parole_violation_population"]      <- "technical_parole_violation_population1"
  names(nameit)[names(nameit) == "total_new_offense_violation_population"]     <- "total_new_offense_violation_population1"
  names(nameit)[names(nameit) == "new_offense_probation_violation_population"] <- "new_offense_probation_violation_population1"
  names(nameit)[names(nameit) == "new_offense_parole_violation_population"]    <- "new_offense_parole_violation_population1"
  
  assign(paste0(micelist[i],".nat"),nameit,envir = .GlobalEnv)
}


##########################
#CALCULATE FINAL ESTIMATES, m=1,2,3,4,5 (i.e., calculate average)
#sum of theta estimates divided by total number of imputations
for (j in yearnum){
  parest <- data.frame(
    overall_admissions                         = floor(as.numeric((mice_imputed_data1.nat[j,1] + mice_imputed_data2.nat[j,1] + mice_imputed_data3.nat[j,1] + mice_imputed_data4.nat[j,1] + mice_imputed_data5.nat[j,1])/5)),
    probation_violation_admissions             = floor(as.numeric((mice_imputed_data1.nat[j,2] + mice_imputed_data2.nat[j,2] + mice_imputed_data3.nat[j,2] + mice_imputed_data4.nat[j,2] + mice_imputed_data5.nat[j,2])/5)),
    parole_violation_admissions                = floor(as.numeric((mice_imputed_data1.nat[j,3] + mice_imputed_data2.nat[j,3] + mice_imputed_data3.nat[j,3] + mice_imputed_data4.nat[j,3] + mice_imputed_data5.nat[j,3])/5)),
    total_technical_violation_admissions       = floor(as.numeric((mice_imputed_data1.nat[j,4] + mice_imputed_data2.nat[j,4] + mice_imputed_data3.nat[j,4] + mice_imputed_data4.nat[j,4] + mice_imputed_data5.nat[j,4])/5)),    
    technical_probation_violation_admissions   = floor(as.numeric((mice_imputed_data1.nat[j,5] + mice_imputed_data2.nat[j,5] + mice_imputed_data3.nat[j,5] + mice_imputed_data4.nat[j,5] + mice_imputed_data5.nat[j,5])/5)),
    technical_parole_violation_admissions      = floor(as.numeric((mice_imputed_data1.nat[j,6] + mice_imputed_data2.nat[j,6] + mice_imputed_data3.nat[j,6] + mice_imputed_data4.nat[j,6] + mice_imputed_data5.nat[j,6])/5)),
    total_new_offense_violation_admissions     = floor(as.numeric((mice_imputed_data1.nat[j,7] + mice_imputed_data2.nat[j,7] + mice_imputed_data3.nat[j,7] + mice_imputed_data4.nat[j,7] + mice_imputed_data5.nat[j,7])/5)),    
    new_offense_probation_violation_admissions = floor(as.numeric((mice_imputed_data1.nat[j,8] + mice_imputed_data2.nat[j,8] + mice_imputed_data3.nat[j,8] + mice_imputed_data4.nat[j,8] + mice_imputed_data5.nat[j,8])/5)),
    new_offense_parole_violation_admissions    = floor(as.numeric((mice_imputed_data1.nat[j,9] + mice_imputed_data2.nat[j,9] + mice_imputed_data3.nat[j,9] + mice_imputed_data4.nat[j,9] + mice_imputed_data5.nat[j,9])/5)),
    
    overall_population                         = floor(as.numeric((mice_imputed_data1.nat[j,10] + mice_imputed_data2.nat[j,10] + mice_imputed_data3.nat[j,10] + mice_imputed_data4.nat[j,10] + mice_imputed_data5.nat[j,10])/5)),
    probation_violation_population             = floor(as.numeric((mice_imputed_data1.nat[j,11] + mice_imputed_data2.nat[j,11] + mice_imputed_data3.nat[j,11] + mice_imputed_data4.nat[j,11] + mice_imputed_data5.nat[j,11])/5)),
    parole_violation_population                = floor(as.numeric((mice_imputed_data1.nat[j,12] + mice_imputed_data2.nat[j,12] + mice_imputed_data3.nat[j,12] + mice_imputed_data4.nat[j,12] + mice_imputed_data5.nat[j,12])/5)),
    total_technical_violation_population       = floor(as.numeric((mice_imputed_data1.nat[j,13] + mice_imputed_data2.nat[j,13] + mice_imputed_data3.nat[j,13] + mice_imputed_data4.nat[j,13] + mice_imputed_data5.nat[j,13])/5)),    
    technical_probation_violation_population   = floor(as.numeric((mice_imputed_data1.nat[j,14] + mice_imputed_data2.nat[j,14] + mice_imputed_data3.nat[j,14] + mice_imputed_data4.nat[j,14] + mice_imputed_data5.nat[j,14])/5)),
    technical_parole_violation_population      = floor(as.numeric((mice_imputed_data1.nat[j,15] + mice_imputed_data2.nat[j,15] + mice_imputed_data3.nat[j,15] + mice_imputed_data4.nat[j,15] + mice_imputed_data5.nat[j,15])/5)),
    total_new_offense_violation_population     = floor(as.numeric((mice_imputed_data1.nat[j,16] + mice_imputed_data2.nat[j,16] + mice_imputed_data3.nat[j,16] + mice_imputed_data4.nat[j,16] + mice_imputed_data5.nat[j,16])/5)),    
    new_offense_probation_violation_population = floor(as.numeric((mice_imputed_data1.nat[j,17] + mice_imputed_data2.nat[j,17] + mice_imputed_data3.nat[j,17] + mice_imputed_data4.nat[j,17] + mice_imputed_data5.nat[j,17])/5)),
    new_offense_parole_violation_population    = floor(as.numeric((mice_imputed_data1.nat[j,18] + mice_imputed_data2.nat[j,18] + mice_imputed_data3.nat[j,18] + mice_imputed_data4.nat[j,18] + mice_imputed_data5.nat[j,18])/5))
  )
  assign(paste0("final.parest.imp",year[j]),parest,envir = .GlobalEnv)
}

#rbind final estimates
final.parest.imp <- data.frame()
for (i in year) {
  temp <- get(paste0("final.parest.imp",i))
  final.parest.imp  <- rbind(final.parest.imp,temp)
}

final.parest.imp$year <- year

##########################
#CALCULATE FINAL CONFIDENCE INTERVALS, m=1,2,3,4,5 (i.e., calculate TOTAL variance)
# equation: w + (1+1/m)*b, where w=within-imputation variance, b=between-imputation variance, m=number of imputations
# w = sum of variance estimates divided by total number of imputations
# b = sum of the squared difference of the combined estimate (theta hat) from each theta estimate divided by the total number of imputations minus 1

#LOOP OVER ALL METRICS = 'tablevals' vector
for (h in yearnum){
  for (i in numvar) {
    #WITHIN-IMPUTATION VARIANCE
    #POISSON DISTRIBUTION - mean of POISSON is also the variance
    v1 <- mice_imputed_data1.nat[h,i]
    v2 <- mice_imputed_data2.nat[h,i]
    v3 <- mice_imputed_data3.nat[h,i]
    v4 <- mice_imputed_data4.nat[h,i]
    v5 <- mice_imputed_data5.nat[h,i]
    w  <- sum(v1,v2,v3,v4,v5)/5
    #BETWEEN-IMPUTATION VARIANCE
    b  <- (((mice_imputed_data1.nat[h,i] - final.parest.imp[h,i])^2) + 
             ((mice_imputed_data2.nat[h,i] - final.parest.imp[h,i])^2) + 
             ((mice_imputed_data3.nat[h,i] - final.parest.imp[h,i])^2) + 
             ((mice_imputed_data4.nat[h,i] - final.parest.imp[h,i])^2) + 
             ((mice_imputed_data5.nat[h,i] - final.parest.imp[h,i])^2))/4
    #TOTAL VARIANCE, then calculate standard error
    s  <- sqrt(w + (1 + (1/5))*b)
    #final CI
    natCI <- c(floor(as.numeric(final.parest.imp[h,i] - (s*1.96))), 
               floor(as.numeric(final.parest.imp[h,i] + (s*1.96))))
    assign(paste0("natCI.",year[h],".",i),natCI,envir = .GlobalEnv)
  }
}

#append confidence intervals for ease of output
for (i in year) {
  test <- data.frame()
  for (j in numvar) {
    temp <- get(paste0("natCI.",i,".",j))
    test  <- rbind(test,temp)
  }
  colnames(test) <- c("V1","V2")
  assign(paste0("natCI.",i,"all"),test,envir = .GlobalEnv)
}

#manipulate estimates for merging with CIs
for (i in year) {
  natest <- as.data.frame(t(matrix(as.numeric(unlist(final.parest.imp[which(year==i),])),
                                   nrow=nrow(final.parest.imp[which(year==i),]))
                            )) %>%
    slice(numvar)
  rownames(natest) <- numvar
  colnames(natest) <- "Estimate"
  assign(paste0("nat.",i,"all"),natest,envir = .GlobalEnv)
}