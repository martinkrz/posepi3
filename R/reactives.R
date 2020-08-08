
# Panel 1: position of peaks
calculate1 = function(params1a,params1b,params1c,params1d) {
  t0   = Sys.time()
  out  = data.frame()

  df1a = seirs(params1a)
  df1b = seirs(params1b)
  df1c = seirs(params1c)
  df1d = seirs(params1d)

  out   = rbind(out,df1a,df1b,df1c,df1d) # R0max idmin
  peaks = data.frame()
  peaks = rbind(peaks,find_peaks(df1a),find_peaks(df1b),find_peaks(df1c),find_peaks(df1d))  
  # peaks
  for(R0 in seq(1.25,R0_max,by=0.25)) {
    if(R0 > params1b$R0 & R0 < params1a$R0) {
      p      = params1a
      p$R0   = R0      
      p$beta = get_beta(p)
      peaks  = rbind(peaks,find_peaks(seirs(p)))
      p      = params1c
      p$R0   = R0      
      p$beta = get_beta(p)
      peaks  = rbind(peaks,find_peaks(seirs(p)))
    }
  }
  peaks = peaks %>% arrange(i,R0)  
  report_timing(t0,"calculate1")
  return(list(out,peaks))
}

# Panel 2: EVPI
calculate2 = function(params2a,params2b,params2c,params2d) {
  t0  = Sys.time()
  R0_max = 4 + (params2a$R0 > 4)
  out    = data.frame()
  #params2a0   = params2a
  #params2a0$p = 0
  #params2c0   = params2c
  #params2c0$p = 0
  pmin  = min(params2a$p,params2b$p,params2c$p,params2d$p)
  pmax  = max(params2a$p,params2b$p,params2c$p,params2d$p)
  idmin = min(params2a$id,params2b$id,params2c$id,params2d$id)
  idmax = max(params2a$id,params2b$id,params2c$id,params2d$id)
  models  = c("M1","M1","M2","M2")
  plevel  = c("p3","p2","p3","p2")
  i       = 1
  #for(p in list(params2a0,params2a,params2b,params2c0,params2c,params2d)) {
  for(p in list(params2a,params2b,params2c,params2d)) {
    params      = p
    for(R0 in sort(unique(c(params2a$R0,params2b$R0,R0_max,seq(R0_min,R0_max,by=R0_step2))))) {
      params$R0   = R0
      params$beta = get_beta(params)
      seirs       = seirs(params,steps=sir_system_steps)
      B           = seirs[sir_system_steps,]$B
      a           = FALSE
      if(R0 == params2b$R0 && params$p == pmin) {
        a = "A1"
      } else if (R0 == params2a$R0 && params$p == pmax) {
        a = "A2"
      }
      this        = data.frame(B=B,R0=R0,beta=params$beta,tmax=params$tmax,p=params$p,id=params$id,pstr=plevel[i],m=models[i],a=a)
      #print(this,i)
      out         = rbind(out,this)
    }
    i = i + 1
  }  
  report_timing(t0,"calculate2")
  return(list(out))
}

# vaccination
calculate3 = function(params3) {
  t0 = Sys.time()
  out = data.frame()
  for(p in seq(p3_min,p3_max,by=p3_step)) {
    params   = params3
    params$p = p/365
    seirs1   = seirs(params,steps=sir_system_steps) 
    out      = rbind(out,seirs1)
  }  
  report_timing(t0,"calculate3")
  return(out)
}

calculate4 = function(paramsa,paramsb,paramsc,paramsd) {
  t0 = Sys.time()
  out = data.frame()  
  R0min = paramsb$R0
  R0max = paramsa$R0
  idmin = paramsa$id
  idmax = paramsc$id
  for(id in seq(idmin/365,idmax/365,by=id_step2)) {    
    for(R0 in sort(unique(c(R0min,R0max,seq(R0min,R0max,by=R0_step2))))) {      
      p   = get_params(R0, paramsa$ip, paramsa$lp, id, paramsa$le/365, paramsa$al, paramsa$p*365, paramsa$tmax/365)
      df  = seirs(p)
      out = rbind(out,df[nrow(df),])
    }
  }      
  report_timing(t0,"calculate4")  
  return(out)
}
