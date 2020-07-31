plots1 = function(params1a,params1b,params1c,params1d) { #R0,ip,lp,id,le,alpha=0,p=0) {

  data   = df1()

  out    = data[[1]]
  peaks  = data[[2]]

  df1a   = out %>% filter(R0==params1a$R0 & id==params1a$id) # R0max idmin
  df1b   = out %>% filter(R0==params1b$R0 & id==params1b$id) # R0min idmin
  df1c   = out %>% filter(R0==params1c$R0 & id==params1c$id) # R0max idmax
  df1d   = out %>% filter(R0==params1d$R0 & id==params1d$id) # R0min idmax

  tmax   = params1a$tmax
  Bmax   = max(out$B)
  
  plot1  = ggplot() # I, B
  plot2  = ggplot() # I, B
  plot3  = ggplot() # I vs S
  plot4  = ggplot() # I vs S

  log       = input$log1
  plot1ymax = min(1,ceilToFraction(max(out$I),0.05))

  t0   = min(out[out$I > sir_init_i,]$time)
  smin = min(out[out$time > t0,]$S)
  if(log) {
    imin = min(out[out$time > t0,]$I)
  } else {
    imin = 0
  }

  # flu bands
  for(t1 in seq(tflu_start,tmax,by=365)) {
    t2 = min(tmax,t1 + tflu_duration)
    plot1 = plot1 + geom_ribbon(data=data.frame(x=c(t1,t2)),mapping=aes(x=x/365,ymax=plot1ymax,ymin=imin/1.5),fill=palette["C1"],alpha=0.15)
  }
  
  plot1 = plot1 + geom_ribbon(data=df1a, mapping=aes(x=time/365,ymax=I,ymin=imin/1.5,fill="I"),size=plot_line_width,alpha=0.7)
  plot1 = plot1 + geom_ribbon(data=df1b, mapping=aes(x=time/365,ymax=I,ymin=imin/1.5,fill="R"),size=plot_line_width,alpha=0.7)
  plot1 = plot1 + geom_line(  data=df1a, mapping=aes(x=time/365,y=plot1ymax*B/Bmax,color="I"), size=plot_line_width)
  plot1 = plot1 + geom_line(  data=df1b, mapping=aes(x=time/365,y=plot1ymax*B/Bmax,color="R"), size=plot_line_width)
  
  plot2 = plot2 + geom_ribbon(data=df1c, mapping=aes(x=time/365,ymax=I,ymin=imin/1.5,fill="I"),size=plot_line_width,alpha=0.7)
  plot2 = plot2 + geom_ribbon(data=df1d, mapping=aes(x=time/365,ymax=I,ymin=imin/1.5,fill="R"),size=plot_line_width,alpha=0.7)
  plot2 = plot2 + geom_line(  data=df1c, mapping=aes(x=time/365,y=plot1ymax*B/Bmax,color="I"), size=plot_line_width)
  plot2 = plot2 + geom_line(  data=df1d, mapping=aes(x=time/365,y=plot1ymax*B/Bmax,color="R"), size=plot_line_width)

  plot1 = plot1 + geom_line( data=peaks %>% filter(i < 3 & id == params1a$id), mapping=aes(x=time/365,y=y,group=i),
  size=plot_line_width/2) 
  plot1 = plot1 + geom_point(data=peaks %>% filter(i < 3 & id == params1a$id & R0 ==
  trunc(R0)), mapping=aes(x=time/365,y=y,group=i), shape=21, fill="white", color="black",size=3,stroke=1)
  plot1 = plot1 + geom_point(data=peaks %>% filter(i < 3 & id == params1a$id & R0 !=
  trunc(R0)), mapping=aes(x=time/365,y=y,group=i), shape=21, fill="white", color="black",size=2,stroke=1)

  plot2 = plot2 + geom_line( data=peaks %>% filter(i < 3 & id == params1c$id), mapping=aes(x=time/365,y=y,group=i),
  size=plot_line_width/2) 
  plot2 = plot2 + geom_point(data=peaks %>% filter(i < 3 & id == params1c$id & R0 ==
  trunc(R0)), mapping=aes(x=time/365,y=y,group=i), shape=21, fill="white", color="black",size=3,stroke=1)
  plot2 = plot2 + geom_point(data=peaks %>% filter(i < 3 & id == params1c$id & R0 !=
  trunc(R0)), mapping=aes(x=time/365,y=y,group=i), shape=21, fill="white", color="black",size=2,stroke=1)
 
  plot1   = plot1 + geom_point(data=peaks %>% filter(i < 3 & id == params1a$id & R0 == params1a$R0),mapping=aes(time/365,y),color="black",fill=palette["I"],size=3,stroke=1,shape=21)
  plot1   = plot1 + geom_point(data=peaks %>% filter(i < 3 & id == params1b$id & R0 == params1b$R0),mapping=aes(time/365,y),color="black",fill=palette["R"],size=3,stroke=1,shape=21)
  plot2   = plot2 + geom_point(data=peaks %>% filter(i < 3 & id == params1c$id & R0 == params1c$R0),mapping=aes(time/365,y),color="black",fill=palette["I"],size=3,stroke=1,shape=21)
  plot2   = plot2 + geom_point(data=peaks %>% filter(i < 3 & id == params1d$id & R0 == params1d$R0),mapping=aes(time/365,y),color="black",fill=palette["R"],size=3,stroke=1,shape=21)
  
  plot3 = plot3 + geom_path(data=df1a,aes(x=S,y=I),colour=palette["I"],size=plot_line_width)
  plot3 = plot3 + geom_path(data=df1b,aes(x=S,y=I),colour=palette["R"],size=plot_line_width)
  plot3 = plot3 + geom_hline(yintercept=params1a$stars$I,colour=palette["I"],size=plot_line_width/2,linetype="dashed")
  plot3 = plot3 + geom_vline(xintercept=params1a$stars$S,colour=palette["I"],size=plot_line_width/2,linetype="dashed")
  plot3 = plot3 + geom_hline(yintercept=params1b$stars$I,colour=palette["R"],size=plot_line_width/2,linetype="dashed")
  plot3 = plot3 + geom_vline(xintercept=params1b$stars$S,colour=palette["R"],size=plot_line_width/2,linetype="dashed")
  
  plot4 = plot4 + geom_path(data=df1c,aes(x=S,y=I),colour=palette["I"],size=plot_line_width)
  plot4 = plot4 + geom_path(data=df1d,aes(x=S,y=I),colour=palette["R"],size=plot_line_width)
  plot4 = plot4 + geom_hline(yintercept=params1c$stars$I,colour=palette["I"],size=plot_line_width/2,linetype="dashed")
  plot4 = plot4 + geom_vline(xintercept=params1c$stars$S,colour=palette["I"],size=plot_line_width/2,linetype="dashed")
  plot4 = plot4 + geom_hline(yintercept=params1d$stars$I,colour=palette["R"],size=plot_line_width/2,linetype="dashed")
  plot4 = plot4 + geom_vline(xintercept=params1d$stars$S,colour=palette["R"],size=plot_line_width/2,linetype="dashed")
  
  param_text1 = sprintf("<i>R</i><sub>0</sub> = %.1f&ndash;%.1f, 1/<i>&beta;</i> = %.2f&ndash;%.2f days, 1/<i>&gamma;</i> = %d days, 1/<i>&sigma;</i> = %d days, 1/<i>&omega;</i> = %.1f years, 1/<i>&mu;</i> = %d years and <i>&alpha;</i> = %.3f/day with %.0f%% annual vaccination rate.",
                       params1b$R0,
                       params1a$R0,
                       params1a$beta,
                       params1b$beta,
                       params1a$ip,
                       params1a$lp,
                       params1a$id/365,
                       params1a$le/365,
                       params1a$alpha,
                       params1a$p*100*365)
  param_text2 = sprintf("<i>R</i><sub>0</sub> = %.1f&ndash;%.1f, 1/<i>&beta;</i> = %.2f&ndash;%.2f days, 1/<i>&gamma;</i> = %d days, 1/<i>&sigma;</i> = %d days, 1/<i>&omega;</i> = %.1f years, 1/<i>&mu;</i> = %d years and <i>&alpha;</i> = %.3f/day with %.0f%% annual vaccination rate.",
                        params1b$R0,
                        params1a$R0,
                        params1a$beta,
                        params1b$beta,
                        params1b$ip,
                        params1b$lp,
                        params1c$id/365,
                        params1b$le/365,
                        params1b$alpha,
                        params1b$p*100*365)

  title1 = sprintf("Infected fraction and disease burden for short duration immunity. %s",param_text1)
  title2 = sprintf("Infected fraction and disease burden for long duration immunity. %s",param_text2)
  title3 = sprintf("Phase plane of <i>I</i> vs <i>S</i> for short duration immunity. %s",param_text1)
  title4 = sprintf("Phase plane of <i>I</i> vs <i>S</i> for long duration immunity. %s",param_text2)
 
  plot1 = plot1 + my.plot_legend + my.plot_axis(xlab="years",
                                                ylab="infected fraction (%)",
                                                xmin=0,
                                                xmax=tmax/365,
                                                ymin=imin,
                                                ylog10min=imin/1.5,
                                                ymax=plot1ymax,
                                                ysec=1,
                                                log10=log)
  
  plot3 = plot3 + my.plot_legend + my.plot_axis(xlab="susceptible fraction (%)",ylab="infected fraction (%)",
                                                xmin      = smin,
                                                xlog10min = smin,
                                                xmax      = 1,
                                                ylog10min = imin,
                                                ymax      = plot1ymax,
                                                ypercent=1,
                                                xpercent=1,
                                                dlog10=log)
  
  
  plot2 = plot2 + my.plot_legend + my.plot_axis(xlab="years",
                                                ylab="infected fraction (%)",
                                                xmin=0,
                                                xmax=tmax/365,
                                                ymin=imin,
                                                ylog10min=imin/1.5,
                                                ymax=plot1ymax,
                                                ysec=1,
                                                log10=log)
  
  plot4 = plot4 + my.plot_legend + my.plot_axis(xlab="susceptible fraction (%)",ylab="infected fraction (%)",
                                                xmin=smin,
                                                xlog10min = smin,
                                                xmax=1,
                                                ylog10min=imin,                                                
                                                ymax=plot1ymax,
                                                ypercent=1,
                                                xpercent=1,
                                                dlog10=log)
  
  caption1 = paste("The SEIRS infected fraction and cumulative disease burden for",param_text1," Burden is normalized to",varfmt("B",Bmax,prec=2),"at",varfmt("t",tmax/365,prec=1,units="years for the scenario with short immunity and high infectivity."))
  caption2 = paste("The SEIRS infected fraction and cumulative disease burden for",param_text2," Burden is normalized to",varfmt("B",Bmax,prec=2),"at",varfmt("t",tmax/365,prec=1,units="years for the scenario with short immunity and high infectivity."))
  
  caption3 = paste("The phase plane of",varfmt("I(t)"),"vs",varfmt("S(t)"),"for the SEIRS model with",param_text1,"Horizontal dashed lines represent endemic equilibrium values.")
  caption4 = paste("The phase plane of",varfmt("I(t)"),"vs",varfmt("S(t)"),"for the SEIRS model with",param_text2,"Horizontal dashed lines represent endemic equilibrium values.")
  
  caption1 = paste(caption1,sir_caption(tmax,params1a$p))
  caption2 = paste(caption2,sir_caption(tmax,params1a$p))
  caption3 = paste(caption3,sir_caption(tmax,params1a$p))
  caption4 = paste(caption4,sir_caption(tmax,params1a$p))

  return(list(
    list(plot_theme(plot1),plot_theme(plot2),plot_theme(plot3),plot_theme(plot4)),
    list(1,2,3,4),
    list(title1,title2,title3,title4),
    list(caption1,caption2,caption3,caption4)))
}
