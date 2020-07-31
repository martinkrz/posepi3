plots4 = function(paramsa,paramsb,paramsc,paramsd) {

  df    = df4()  
  tmax  = paramsa$tmax
  Bmax  = max(df$B)
  df$B  = df$B/Bmax
  log   = input$log4
  plot1 = ggplot()
  plot2 = ggplot()
 
  R0max = paramsa$R0
  plot1 = plot1 + geom_line(data=df,aes(x=R0,y=B,group=id,color=id/365),size=plot_line_width) + scale_color_distiller(palette = "Spectral")
  plot1 = plot1 + geom_label(data=df %>% filter(R0==R0max),aes(x=R0,y=B,label=varfmt(value=id/365,prec=2)),hjust="left",fill="white",size=4,nudge_x=0.05)

  plot2 = plot1
  
  param1_text = sprintf("<i>R</i><sub>0</sub> = %.1f&ndash;%.1f, 1/<i>&beta;</i> = %.2f&ndash;%.2f days, 1/<i>&gamma;</i>
  = %d days, 1/<i>&sigma;</i> = %d days, 1/<i>&omega;</i> = %.1f years, 1/<i>&mu;</i> = %d years and <i>&alpha;</i> = %.3f/day with %.0f%% annual vaccination rate.", paramsa$R0, paramsb$R0, paramsa$beta, paramsb$beta, paramsa$ip, paramsa$lp,
  paramsa$id/365, paramsa$le/365, paramsa$alpha, paramsa$p*365*100)

  title1 = sprintf("Effect of <i>R</i><sub>0</sub> and immunity duration on the cumulative disease burden. %s",param1_text)
  plot1 = plot1 + my.plot_axis(xlab="R0",ylab="cumulative burden, B",
                               xmin=paramsb$R0,
                               xmax=paramsa$R0 + 0.25,
                               ymin=0,
                               ylog10min=min(df$B),
                               ymax=1,
                               #log10=log,
                               ypercent=1)
  
  caption1 = paste("Caption.")
  caption1 = paste(caption1,sir_caption(tmax,params$p))

  return(list(
    list(plot_theme(plot1)),
    list(5),
    list(title1),
    list(caption1)))
  
}
