# EVPI

output$text2intro = renderPrint({ 
  paramsa = params2a()
  paramsb = params2b()
  paramsc = params2c()
  paramsd = params2d()
  
  out   = df2()  
  df    = out[[1]]
  Bmax  = max(df$B)
  tmax  = paramsa$tmax
  
  B11 = df %>% filter(a=="A1" & m=="M1") %>% select(B) %>% first() / Bmax
  B12 = df %>% filter(a=="A2" & m=="M1") %>% select(B) %>% first() / Bmax
  B21 = df %>% filter(a=="A1" & m=="M2") %>% select(B) %>% first() / Bmax
  B22 = df %>% filter(a=="A2" & m=="M2") %>% select(B) %>% first() / Bmax
   
  str0 = sprintf("<i>R</i><sub>0</sub> = %.1f, <i>p</i> = %.0f%%",paramsb$R0,paramsb$p*365*100)
  str1 = sprintf("<i>R</i><sub>0</sub> = %.1f, <i>p</i> = %.0f%%",paramsa$R0,paramsa$p*365*100)
  str2 = sprintf("1/<i>&omega;</i> = %.2f years",paramsa$id/365)
  str3 = sprintf("1/<i>&omega;</i> = %.2f years",paramsc$id/365)

  M = matrix(c(B11,B12,B21,B22),nrow=2,byrow=TRUE)
  
  avgs = colMeans(M)
  opts = apply(M,1,min)           
  evpi = min(avgs)-mean(opts)

  R0max   = max(df$R0)
  betamax = df %>% filter(R0==R0max) %>% select(beta) %>% first()  
  
  table(title="SEIRS parameters",class="bold",rows=makerows(c(        
    "R0",varfmt(value=R0max),
    "beta",varfmt(value=betamax[1],prec=2,units="/day"),
    "gamma",varfmt(value=paramsa$gamma,prec=2,units="/day"),
    "sigma",varfmt(value=paramsa$sigma,prec=2,units="/day"),
    "mu",varfmt(value=365*paramsa$mu,prec=3,units="/year"),
    "alpha",varfmt(value=paramsa$alpha,prec=3,units="/day"),
    "B<sub>max</sub>",sprintf("%s at %s",varfmt(value=Bmax,prec=2),varfmt(value=tmax/365,prec=1,units="years"))    
  )))

  cat(paste("<p>Uncertainty in the parameters of the model of an outbreak can confound forecasting and policy-making. In this example, we show how we can decide between two mitigation actions",varfmt("A1"),sprintf("(%s)",str0),"and",varfmt("A2"),sprintf("(%s)",str1),"in the face of two possible immunity duration models",varfmt("M1"),sprintf("(short, %s)",varfmt("1/omega",paramsa$id/365,prec=2,unit="years")),"and",varfmt("M2"),sprintf("(long, %s).",varfmt("1/omega",paramsc$id/365,prec=2,unit="years"),"of an outbreak with",varfmt("R0.",R0max,prec=1))))


  if( avgs[1] < avgs[2] ) {
    Ab = "A1"
    avgsb = avgs[1]
  } else {
    Ab = "A2"
    avgsb = avgs[2]
  }

  cat(paste("<h4>key observations</h4>"))

  cat(paste("<p>If we cannot decide whether",varfmt("M1"),"or",varfmt("M2"),"are right, we would average the burdens across models for each action and pick one that had the lower value. We would pick",varfmt(Ab),"because it has the lower average burden",varfmt("Bavg.",avgsb,prec=1,percent=1)))
  
  cat(paste("<p>Could we make a better choice if we knew whether",varfmt("M1"),"or",varfmt("M2"),"were correct? To determine the value of accurately knowing the immunity duration, we calculate the expected value of perfect information (EVPI). The EVPI is the difference between the optimum of burden averages across actions",sprintf("opt(%s,%s) = %s",varfmt(value=avgs[1],prec=1,percent=1),varfmt(value=avgs[2],prec=1,percent=1),varfmt(value=min(avgs),prec=1,percent=1)),"and the average of optimums across models",sprintf("avg(%s,%s) = %s.",varfmt(value=opts[1],prec=1,percent=1),varfmt(value=opts[2],prec=1,percent=1),varfmt(value=mean(opts),prec=1,percent=1)),"In our case",varfmt("EVPI.",evpi,prec=1,percent=1)))
 
  if(evpi==0) {
    cat(paste("<p>When the EVPI is zero, then we do not need to resolve uncertainty about immunity duration because one of the actions has a better outcome for models. In this case, we would always pick",sprintf("%s.",varfmt(Ab))))
  }
  if(evpi>0) {
    cat(paste("<p>The fact that the EVPI is not zero suggests that our choice of",varfmt(Ab),"we made under no knowledge about immunity wouldn't be ideal for both models."))
  }

  cat(paste("<p>If there are more than two actions or models, the EVPI calculations are analogous, except now the average is taken over several models and the optimum over several actions. If the outcome of an action is worst for all models, such an action is called a 'dominated alterative' and it can be rejected as an option outright. In such a case, its EVPI with every other action will be zero. It is also possible to have situations where one action is never the best, but nevertheless performs relatively well in most cases and can be a strong bet-hedging strategy."))
})