plots1 = function(params1a,params1b,params1c,params1d) { #R0,ip,lp,id,le,alpha=0,p=0) {

  out     = df1()
  df1a    = out[[1]]  # R0max idmin
  df1b    = out[[2]]  # R0min idmin
  
  df2a    = out[[3]]  # R0max idmin
  df2b    = out[[4]]  # R0min idmin

  plot1  = ggplot()  # I, B
  plot2  = ggplot()  # I, B
  plot3  = ggplot()  # I vs S
  plot4  = ggplot()
  
  tmax    = max(df1a$time)
  
  # minimum I that is above S(0), this is for setting minimum y-axis values for log plots
  t0      = df[df1a$I > sir_init_i,]$time[1] 
  imin    = min(df1a[df1a$time > t0,]$I)

  # minimum S for phase plane
  smin    = min(df1a$S)

  # max burden
  Bmax = max(df1a$B)
  
  plot1ymax = min(1,ceilToFraction(max(df1a$I),0.05))
    
  plot1 = plot1 + geom_area(data=df1a, mapping=aes(x=time/365,y=I,fill="I"),size=plot_line_width,alpha=0.7)
  plot1 = plot1 + geom_area(data=df1b, mapping=aes(x=time/365,y=I,fill="R"),size=plot_line_width,alpha=0.7)

  plot1 = plot1 + geom_line(data=df1a, mapping=aes(x=time/365,y=plot1ymax*B/Bmax,color="I"),size=plot_line_width)
  plot1 = plot1 + geom_line(data=df1b, mapping=aes(x=time/365,y=plot1ymax*B/Bmax,color="R"),size=plot_line_width)
  
  plot2 = plot2 + geom_area(data=df2a, mapping=aes(x=time/365,y=I,fill="I"),size=plot_line_width,alpha=0.7)
  plot2 = plot2 + geom_area(data=df2b, mapping=aes(x=time/365,y=I,fill="R"),size=plot_line_width,alpha=0.7)
  
  plot2 = plot2 + geom_line(data=df2a, mapping=aes(x=time/365,y=plot1ymax*B/Bmax,color="I"),size=plot_line_width)
  plot2 = plot2 + geom_line(data=df2b, mapping=aes(x=time/365,y=plot1ymax*B/Bmax,color="R"),size=plot_line_width)
  
  peaks1a = find_peaks(df1a)
  peaks1b = find_peaks(df1b)
  plot1 = plot1 + geom_point(data=peaks1a,mapping=aes(x/365,y),color="black",fill=palette["I"],size=3,stroke=1,shape=21)
  plot1 = plot1 + geom_point(data=peaks1b,mapping=aes(x/365,y),color="black",fill=palette["R"],size=3,stroke=1,shape=21)

  peaks2a = find_peaks(df2a)
  peaks2b = find_peaks(df2b)
  plot2 = plot2 + geom_point(data=peaks2a,mapping=aes(x/365,y),color="black",fill=palette["I"],size=3,stroke=1,shape=21)
  plot2 = plot2 + geom_point(data=peaks2b,mapping=aes(x/365,y),color="black",fill=palette["R"],size=3,stroke=1,shape=21)
  
  #plot1 = plot1 + geom_hline(data=df,yintercept=params1$stars$I,colour=palette["I"],size=plot_line_width/2,linetype="dashed")

  plot3 = plot3 + geom_path(data=df1a,aes(x=S,y=I),colour=palette["I"],size=plot_line_width)
  plot3 = plot3 + geom_path(data=df1b,aes(x=S,y=I),colour=palette["R"],size=plot_line_width)
  plot3 = plot3 + geom_hline(data=df1a,yintercept=params1a$stars$I,colour=palette["I"],size=plot_line_width/2,linetype="dashed")
  plot3 = plot3 + geom_vline(data=df1a,xintercept=params1a$stars$S,colour=palette["I"],size=plot_line_width/2,linetype="dashed")
  plot3 = plot3 + geom_hline(data=df1b,yintercept=params1b$stars$I,colour=palette["R"],size=plot_line_width/2,linetype="dashed")
  plot3 = plot3 + geom_vline(data=df1b,xintercept=params1b$stars$S,colour=palette["R"],size=plot_line_width/2,linetype="dashed")
  
  plot4 = plot4 + geom_path(data=df2a,aes(x=S,y=I),colour=palette["I"],size=plot_line_width)
  plot4 = plot4 + geom_path(data=df2b,aes(x=S,y=I),colour=palette["R"],size=plot_line_width)
  plot4 = plot4 + geom_hline(data=df2a,yintercept=params1a$stars$I,colour=palette["I"],size=plot_line_width/2,linetype="dashed")
  plot4 = plot4 + geom_vline(data=df2a,xintercept=params1a$stars$S,colour=palette["I"],size=plot_line_width/2,linetype="dashed")
  plot4 = plot4 + geom_hline(data=df2b,yintercept=params1b$stars$I,colour=palette["R"],size=plot_line_width/2,linetype="dashed")
  plot4 = plot4 + geom_vline(data=df2b,xintercept=params1b$stars$S,colour=palette["R"],size=plot_line_width/2,linetype="dashed")
  
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
                       params1a$p*100)
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
                        params1b$p*100)

  title1 = sprintf("Infected fraction and disease burden for short duration immunity. %s",param_text1)
  title2 = sprintf("Infected fraction and disease burden for long duration immunity. %s",param_text2)
  title3 = sprintf("Phase plane of <i>I</i> vs <i>S</i> for short duration immunity. %s",param_text1)
  title4 = sprintf("Phase plane of <i>I</i> vs <i>S</i> for long duration immunity. %s",param_text2)
  
  plot1 = plot1 + my.plot_legend + my.plot_axis(xlab="years",
                                                ylab="infected fraction (%)",
                                                ylog10min=min(sir_init_i,imin),
                                                xmin=0,
                                                xmax=tmax/365,
                                                ymin=0,
                                                ymax=plot1ymax,
                                                ysec=1,
                                                log10=input$log1)
  plot2 = plot2 + my.plot_legend + my.plot_axis(xlab="years",
                                                ylab="infected fraction (%)",
                                                ylog10min=min(sir_init_i,imin),
                                                xmin=0,
                                                xmax=tmax/365,
                                                ymin=0,
                                                ymax=plot1ymax,
                                                ysec=1,
                                                log10=input$log1)
  plot3 = plot3 + my.plot_legend + my.plot_axis(xlab="susceptible fraction (%)",ylab="infected fraction (%)",
                                                xmin=smin,xmax=1,
                                                ylog10min=min(sir_init_i,imin),
                                                dlog10=input$log1,
                                                ysec=0,xpercent=1)
  
  caption1 = paste("The SEIRS infected fraction and cumulative disease burden for",param_text1," Burden is normalized to",varfmt("B",Bmax,prec=2),"at",varfmt("t",tmax/365,prec=1,units="years for the scenario with short immunity and high infectivity."))
  caption2 = paste("The SEIRS infected fraction and cumulative disease burden for",param_text2," Burden is normalized to",varfmt("B",Bmax,prec=2),"at",varfmt("t",tmax/365,prec=1,units="years for the scenario with short immunity and high infectivity."))
  
  caption3 = paste("The phase plane of",varfmt("I(t)"),"vs",varfmt("S(t)"),"for the SEIRS model with",param_text1,"Horizontal dashed lines represent endemic equilibrium values.")
  caption4 = paste("The phase plane of",varfmt("I(t)"),"vs",varfmt("S(t)"),"for the SEIRS model with",param_text2,"Horizontal dashed lines represent endemic equilibrium values.")
  
  if(input$points1 == TRUE) {
    #caption1 = paste(caption1,"Points on the trajectory of",varfmt("I"),"indicate time in steps of 1 year (solid) or one quarter (hollow) over the first",n_periods,"inter-epidemic intervals",varfmt("T_E.",params$period/365,units="years",prec=2))
    #caption2 = paste(caption2,"Points on the trajectory indicate time in steps of 1 year (solid) or one quarter (hollow) over the first",n_periods,"inter-epidemic intervals",varfmt("T_E.",params$period/365,units="years",prec=2))
  }

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
