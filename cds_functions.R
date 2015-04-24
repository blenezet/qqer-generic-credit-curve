#YielcCurve
setClass("YieldCurve",
         representation(
           time = "vector",
           discountfactor = "vector")
)

#CreditDefaultSwap
setClass("CreditDefaultSwap", 
         representation(
           maturity = "numeric", 
           marketprice = "numeric" #market spread
         )
)

#CreditDefaultSwapISDA
setClass("CreditDefaultSwapISDA", 
         representation(
           tradedate = "character", 
           startdate = "character", 
           enddate = "character", 
           marketprice = "numeric" #market spread
         )
)

#CreditCurve
setClass("CreditCurve",
         representation(
           time = "vector",
           spread = "vector",
           survivalprobability = "vector",
           hazardrate = "vector")
)

setGeneric( name="BootstrapLimitIndex", def=function(object){standardGeneric("BootstrapLimitIndex")} ) 
setMethod("BootstrapLimitIndex", "CreditCurve", 
          function(object){
            #Find max(i) for which we know hazard rate / survival probability
            if (length(object@time) == 0) #empty credit curve
            {
              max_index = 0
            }  
            else if (!is.na(object@hazardrate[length(object@hazardrate)])) {
              max_index = length(object@hazardrate)
            }
            else {
              for (i in seq(1,length(object@time))) {
                if (is.na(object@hazardrate[i])) {
                  break
                }
              }
              max_index = i - 1
            }
            return (max_index)
          }
) 




#retrieve survival probability for t
GetSurvivalProbability = function(CreditCurve,t) {
  SurvivalProbability = NA
  
  #Find max(i) for which we know hazard rate / survival probability
  max_index = BootstrapLimitIndex(CreditCurve)
  #cat("max_index:",max_index,"t=",t,"\n")
  #print(CreditCurve)
  
  #Calculate survival probability
  if (t<0) {
    warning("Request survival probability for negative time")
  }
  # SP for t=0
  else if (t==0) {
    SurvivalProbability = 1
  }
  # SP for a time before the "max_time" in credit curve (i.e we know all the hazard rates required)  
  else if (t>0 && t<= CreditCurve@time[max_index]) {
    LambdaTime = 0
    i=1
    while (CreditCurve@time[i] <= t && i<=length(CreditCurve@time)) {
      if (i == 1) {
        LambdaTime = LambdaTime + (CreditCurve@time[i]-0) * CreditCurve@hazardrate[i]  
      }
      else {
        LambdaTime = LambdaTime + (CreditCurve@time[i]-CreditCurve@time[i-1]) * CreditCurve@hazardrate[i] 
      }
      #cat(CreditCurve@time[i], "<=t ==>",LambdaTime,"\n")
      i = i + 1
    }
    
    if (t<CreditCurve@time[i] && !is.na(CreditCurve@hazardrate[i]) ) {
      if (i == 1) {
        LambdaTime = LambdaTime + (t-0) * CreditCurve@hazardrate[i] 
      }
      else {
        LambdaTime = LambdaTime + (t-CreditCurve@time[i-1]) * CreditCurve@hazardrate[i] 
      }
      #cat("t lower than pillar[",i,"]=",CreditCurve@time[i]," ==>",LambdaTime,"\n")
    }
    #calculate SP = exp( - sum [lambda_i*tau_i])
    SurvivalProbability = exp(-LambdaTime)    
  }
  else {
    warning("Request survival probability - No hazard rate available for t")
  }

  return (SurvivalProbability)
}

#premium leg function
CalculatePremiumLeg = function (CDS,Frequency,YieldCurve,CreditCurve,Lambda) { #e.g. 0.5/50bp/12
  #index of last cashflow (if no default prior to that)
  N = CDS@maturity*Frequency
  #index of last know survival probability / hazard rate
  BootstrapLimit = BootstrapLimitIndex(CreditCurve)
  if (BootstrapLimit == 0) {
    BootstrapLimitIndex = 0
  }
  else {
    BootstrapLimitIndex = CreditCurve@time[BootstrapLimit] * Frequency  
  }
  #cat("BootstrapLimitIndex:",BootstrapLimitIndex,"\n")
  
  #Bootstap limit lower than maturity of CDS
  if (BootstrapLimitIndex == 0) {
    #cat("**** Premium Leg between 1 and ",N,"(first pillar)\n")
    annuity = 0
    for (n in seq(1,N)) {
      #survvival probability[0,i] = surv prob[0,i-1]*surv prob[i-1,i]
      tmp = (1/Frequency)*GetDiscountFactor(YieldCurve,n/Frequency)*(1)*exp(-Lambda*(n/Frequency))
      annuity = annuity + tmp
      #cat("n =",n,"==> add (use lambda)",tmp, " annuity = ",annuity,"\n")
    }
  }
  else if (BootstrapLimitIndex < N) {
    #No accrued premium for now
    #Until BootstrapLimitIndex
    #cat("**** Premium Leg between 1 and ",BootstrapLimitIndex,"\n")
    annuity = 0
    for (n in seq(1,BootstrapLimitIndex)) {
      tmp = (1/Frequency)*GetDiscountFactor(YieldCurve,n/Frequency)*GetSurvivalProbability(CreditCurve,n/Frequency)
      annuity = annuity + tmp
      #cat("n =",n,"==> add ",tmp," annuity = ",annuity,"\n")
    }
    
    #Until N
    #if N > BootstrapLimitIndex, use Lambda passed as parameter
    if (N >= (BootstrapLimitIndex+1)) {
      #cat("**** Premium Leg between ", BootstrapLimitIndex+1, " and ",N,"\n")
      for (n in seq((BootstrapLimitIndex+1),N)) {
        #survvival probability[0,i] = surv prob[0,i-1]*surv prob[i-1,i]
        tmp = (1/Frequency)*GetDiscountFactor(YieldCurve,n/Frequency)*(GetSurvivalProbability(CreditCurve,BootstrapLimitIndex/Frequency)*exp(-Lambda*((n-BootstrapLimitIndex)/Frequency)))
        annuity = annuity + tmp
        #cat("n =",n,"==> add (use lambda)",tmp, " annuity = ",annuity,"\n")
      }
    }     
  }
  else {
    warning("N >= BootstrapLimitIndex; Lambda parameter not use")
  }
 
  return(CDS@marketprice/10000*annuity)
}

#default leg function
CalculateDefaultLeg = function(CDS,Frequency,YieldCurve,CreditCurve,RecoveryRate,Lambda) {
  #index of last cashflow (if no default prior to that)
  M = CDS@maturity*Frequency
  
  #index of last know survival probability / hazard rate
  BootstrapLimit = BootstrapLimitIndex(CreditCurve)
  if (BootstrapLimit == 0) {
    BootstrapLimitIndex = 0
  }
  else {
    BootstrapLimitIndex = CreditCurve@time[BootstrapLimit] * Frequency  
  }
  #cat("BootstrapLimitIndex:",BootstrapLimitIndex,"\n")
  
  if (BootstrapLimitIndex == 0) {
    #cat("**** Default Leg between 1 and ",M,"(first pillar)\n")
    annuity = 0
    for (m in seq((BootstrapLimitIndex+1),M)) {
      #(Q_m-1-Q_m) = Q_max * exp(- (sum[,m-1] lambda_i*tau_i -sum[,m] lambda_i*tau_i) )
      # = Q_max * exp(lambda_m * tau)
      SurvivalProbability_m = GetSurvivalProbability(CreditCurve,BootstrapLimitIndex/Frequency) * exp(-Lambda*((m-BootstrapLimitIndex)/Frequency))
      SurvivalProbability_m_minus_1 = GetSurvivalProbability(CreditCurve,BootstrapLimitIndex/Frequency) * exp(-Lambda*(((m-1)-BootstrapLimitIndex)/Frequency))
      tmp = GetDiscountFactor(YieldCurve,m/Frequency) * (SurvivalProbability_m_minus_1-SurvivalProbability_m)
      
      annuity = annuity + tmp
      #cat("m =",m,"==> add (use lambda)",tmp, " annuity = ",annuity,"\n")
    }
  }
  else if (BootstrapLimitIndex < M) {
    #Until BootstrapLimitIndex
    annuity = 0
    for (m in seq(1,min(BootstrapLimitIndex,M))) {
      annuity = annuity + GetDiscountFactor(YieldCurve,m/Frequency) * (GetSurvivalProbability(CreditCurve,(m-1)/Frequency)-GetSurvivalProbability(CreditCurve,m/Frequency)) 
      #cat("m=",m,"==> annuity = ",annuity,"\n")
    }
    
    #Until M
    if (M >= (BootstrapLimitIndex+1)) {
      for (m in seq((BootstrapLimitIndex+1),M)) {
        #(Q_m-1-Q_m) = Q_max * exp(- (sum[,m-1] lambda_i*tau_i -sum[,m] lambda_i*tau_i) )
        # = Q_max * exp(lambda_m * tau)
        SurvivalProbability_m = GetSurvivalProbability(CreditCurve,BootstrapLimitIndex/Frequency) * exp(-Lambda*((m-BootstrapLimitIndex)/Frequency))
        SurvivalProbability_m_minus_1 = GetSurvivalProbability(CreditCurve,BootstrapLimitIndex/Frequency) * exp(-Lambda*(((m-1)-BootstrapLimitIndex)/Frequency))
        
        annuity = annuity + GetDiscountFactor(YieldCurve,m/Frequency) * (SurvivalProbability_m_minus_1-SurvivalProbability_m)
        #cat("m=",m,"==> (lambda) annuity = ",annuity,"\n")
      }
    }
  }
  else {
    warning("M >= BootstrapLimitIndex; Lambda parameter not use")
  }
  
  return((1-RecoveryRate)*annuity)
}



# Credit Curve Bootstrapping
BootstrapCreditCurve = function (CDSCollection,RecoveryRate,YieldCurve) {
  #Credit Curve initialization
  CreditCurve = new ("CreditCurve", time = rep(NA,length(CDSCollection)), survivalprobability = rep(NA,length(CDSCollection)), hazardrate=rep(NA,length(CDSCollection)))
  
  for (i in seq(1,length(CDSCollection))) {
    CreditCurve@time[i] = CDSCollection[[i]]@maturity
    CreditCurve@spread[i] = CDSCollection[[i]]@marketprice
  }
  
  #print(CreditCurve)
  
  #Iterate through the list of CDS
  for (i in seq(1,length(CDSCollection))) {
    #quaterly coupounding for premium leg (freq=4)
    #monthly payment for default leg (freq=12)
    tmpfunc = function(lambda) CalculatePremiumLeg(CDSCollection[[i]],4,YieldCurve,CreditCurve,lambda) - CalculateDefaultLeg(CDSCollection[[i]],12,YieldCurve,CreditCurve,RecoveryRate, lambda)
    
    
    #cat("-- i=",i, "lambda=",root,"\n")
    #root = uniroot(tmpfunc,lower=0,upper=0.4,tol=10^-16)$root
    root = NA
    try(root <- uniroot(tmpfunc,lower=0,upper=10.0,tol=10^-16)$root , silent=T)
      
    CreditCurve@hazardrate[i] = root
    CreditCurve@survivalprobability[i] = GetSurvivalProbability(CreditCurve,CDSCollection[[i]]@maturity)
  }
    
  return(CreditCurve)
}

#Calculate ParSwap rate
FindParSwapRate = function(CDS,YieldCurve,CreditCurve,RecoveryRate,Lambda) {
  
  objectivefunc = function(parswaprate) {
    CDS@marketprice = parswaprate
    result = CalculatePremiumLeg(CDS,4,YieldCurve,CreditCurve,Lambda) - CalculateDefaultLeg(CDS,12,YieldCurve,CreditCurve,RecoveryRate, Lambda)
    #cat("rate:",parswaprate," --> ",result,"\n")
    return(result)
  }
  
  root = Inf
  try(root <- uniroot(objectivefunc,lower=0,upper=10^9,tol=10^-16)$root , silent=T)
  return(root)
}

#retrieve discount factor for a specific date
GetDiscountFactor = function(YieldCurve,t) {
  min_time = min(YieldCurve@time)
  min_time_index = which.min(YieldCurve@time)
  max_time = max(YieldCurve@time)
  max_time_index = which.max(YieldCurve@time)
  
  result = NA
  
  if (length(t)==1) {
    if (t < 0) {
      cat("Warning: t is negative. discountfactor not calculated for this case")
    }
    else if (t == 0) {
      result = 1 #df of t=0 is 1
    }
    else if (t>0 && t<min_time) {
      #df of t=0 is 1
      result = 1 + (YieldCurve@discountfactor[min_time_index]-1)*(t/min_time)
    }
    else if (t >= max_time) {
      result = YieldCurve@discountfactor[max_time_index]
    }
    else {
      #i.e t falls between 2 maturity for which we have the discount factor
      for (i in seq(1,length(YieldCurve@time)-1)) {
        if (t>= YieldCurve@time[i] && t<YieldCurve@time[i+1]) {
          result = YieldCurve@discountfactor[i] + (YieldCurve@discountfactor[i+1]-YieldCurve@discountfactor[i])*((t-YieldCurve@time[i])/(YieldCurve@time[i+1]-YieldCurve@time[i]))
        }                                                                                                                 
      }
    }  
  }
  return (result)  
}



