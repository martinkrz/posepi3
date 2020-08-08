server = function(input, output, session) {

  source("R/models.R",local=TRUE)
  source("R/reactives.R",local=TRUE)
  source("R/1.plot.R",local=TRUE)
  source("R/1.text.R",local=TRUE)
  source("R/2.text.R",local=TRUE)
  source("R/2.evpi.R",local=TRUE)
  source("R/2.plot.R",local=TRUE)
  source("R/3.text.R",local=TRUE)
  source("R/3.plot.R",local=TRUE)
  source("R/4.text.R",local=TRUE)
  source("R/4.plot.R",local=TRUE)
  
  ### UI QOL settings
  toggleText = function(value) {
    updateCheckboxInput(session,"text1",value=value) 
    updateCheckboxInput(session,"text2",value=value) 
    updateCheckboxInput(session,"text3",value=value) 
    updateCheckboxInput(session,"text4",value=value) 
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy1", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy2", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy3", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy4", condition = value)
  }
  toggleCaptions = function(value) {
    updateCheckboxInput(session,"captions1",value=value) 
    updateCheckboxInput(session,"captions2",value=value) 
    updateCheckboxInput(session,"captions3",value=value) 
    updateCheckboxInput(session,"captions4",value=value) 
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption1", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption2", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption3", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption4", condition = value)
  }  
  # These used to be local to a pane. I think they work better globally. Clunky kludge.
  observeEvent(input$text1,     { toggleText(input$text1) })
  observeEvent(input$text2,     { toggleText(input$text2) })
  observeEvent(input$text3,     { toggleText(input$text3) })
  observeEvent(input$text4,     { toggleText(input$text4) })
  observeEvent(input$captions1, { toggleCaptions(input$captions1) })
  observeEvent(input$captions2, { toggleCaptions(input$captions2) })
  observeEvent(input$captions3, { toggleCaptions(input$captions3) })
  observeEvent(input$captions4, { toggleCaptions(input$captions4) })
    
  observeEvent(input$refresh1, { shinyjs::reset("form1") })
  observeEvent(input$refresh2, { shinyjs::reset("form2") })
  observeEvent(input$refresh3, { shinyjs::reset("form3") })
  observeEvent(input$refresh4, { shinyjs::reset("form4") })
  
  #observeEvent(input$captions1, { toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption1", condition = input$captions1) })
  #observeEvent(input$captions2,{ toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".caption2", condition = input$captions2) })
  #observeEvent(input$captions3,{ toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".caption3", condition = input$captions3) })
  #observeEvent(input$captions4,{ toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".caption4", condition = input$captions4) })
 
  #observeEvent(input$text2,    { toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".copy2", condition = input$text2) })  
  #observeEvent(input$text3,    { toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".copy3", condition = input$text3) })
  #observeEvent(input$text4,    { toggle(anim = TRUE, animType = "slide", time = 0.5,  selector =".copy4", condition = input$text4) })
    
  #Make sure that vaccination slider maxes out at p_c  
  #observe(updateSliderInput(session, "R0max2", max = 4+(input$R02[2]>4) floor(100*(1-1/input$R01))))

 # calculate parameters from all inputs
  params1a = reactive(get_params(input$R01[2], input$ip1, input$lp1, input$id1[1], input$le1, input$al1, input$p1/100, input$tmax1))
  params1b = reactive(get_params(input$R01[1], input$ip1, input$lp1, input$id1[1], input$le1, input$al1, input$p1/100, input$tmax1))
  params1c = reactive(get_params(input$R01[2], input$ip1, input$lp1, input$id1[2], input$le1, input$al1, input$p1/100, input$tmax1))
  params1d = reactive(get_params(input$R01[1], input$ip1, input$lp1, input$id1[2], input$le1, input$al1, input$p1/100, input$tmax1))
  
  params2a = reactive(get_params(input$R02[2],input$ip2,input$lp2, input$id2[1],   input$le2,input$al2,input$p2[2]/100, input$tmax2))
  params2b = reactive(get_params(input$R02[1],input$ip2,input$lp2, input$id2[1],   input$le2,input$al2,input$p2[1]/100, input$tmax2))
  params2c = reactive(get_params(input$R02[2],input$ip2,input$lp2, input$id2[2],   input$le2,input$al2,input$p2[2]/100, input$tmax2))
  params2d = reactive(get_params(input$R02[1],input$ip2,input$lp2, input$id2[2],   input$le2,input$al2,input$p2[1]/100, input$tmax2))
  
  params3  = reactive(get_params(input$R03, input$ip3, input$lp3, input$id3, input$le3, input$al3, 0, input$tmax3))

  params4a = reactive(get_params(input$R04[2], input$ip4, input$lp4, input$id4[1], input$le4, input$al4, input$p4/100, input$tmax4))
  params4b = reactive(get_params(input$R04[1], input$ip4, input$lp4, input$id4[1], input$le4, input$al4, input$p4/100, input$tmax4))
  params4c = reactive(get_params(input$R04[2], input$ip4, input$lp4, input$id4[2], input$le4, input$al4, input$p4/100, input$tmax4))
  params4d = reactive(get_params(input$R04[1], input$ip4, input$lp4, input$id4[2], input$le4, input$al4, input$p4/100, input$tmax4))

  #df4 = reactiveVal()
  #p4  = reactiveVal()
  #observeEvent(input$tabs, {
  #  if(input$tabs == 2){    
  #    message("Tab 2 code is run")     
  #    df4 = calculate4(params4a(),params4b(),params4c(),params4d())
  #    p4  = plots4(params4a(),params4b(),params4c(),params4d())
  #    message(df4)
  #  }
  #})

  df1 = reactive(calculate1(params1a(),params1b(),params1c(),params1d()))
  p1  = reactive(    plots1(params1a(),params1b(),params1c(),params1d()))

  df2 = reactive(calculate2(params2a(),params2b(),params2c(),params2d()))
  p2  = reactive(    plots2(params2a(),params2b(),params2c(),params2d()))
  
  df3 = reactive(calculate3(params3()))
  p3  = reactive(    plots3(params3()))

  df4 = reactive(calculate4(params4a(),params4b(),params4c(),params4d()))
  p4  = reactive(    plots4(params4a(),params4b(),params4c(),params4d()))
  
  #df4 = observe({if(req(input$tabs)==2) calculate4(params4a(),params4b(),params4c(),params4d())})
  #p4  = observe({if(req(input$tabs)==2) plots4(params4a(),params4b(),params4c(),params4d())    })
  
  #observe({
  #      if (req(input$tabs) == 1)
  #        message("Traj has been selected")
  #      if (req(input$tabs) == 4)
  #        message("EVPI has been selected")
  #    })

  report_timing = function(t0,msg="timing") {
    t1 = Sys.time()
    if(do_timing) {
      message(sprintf("%s %.3f seconds",msg,t1-t0))
    }
  }
  
  source("R/titles.R",local=TRUE)
  source("R/masthead.captions.R",local=TRUE)
  
}