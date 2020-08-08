read_latex = function(file) {
  str = readChar(file, file.info(file)$size)
  #str = str_replace_all(str,"/","//")
  return(str)
}

eq_seirs = read_latex("latex/seirs.tex")
eq_param = read_latex("latex/seirs.parameters.tex")
eq_r0    = read_latex("latex/seirs.r0.tex")
eq_evpi  = read_latex("latex/evpi.tex")

#eq_equil = read_latex("latex/seirs.equilibrium.tex")
#eq_age   = read_latex("latex/seirs.age-dependent.tex")
#eq_period= read_latex("latex/seirs.period.tex")
#eq_endemic_age= read_latex("latex/seirs.endemic-age.tex")
#eq_jacobian= read_latex("latex/seirs.jacobian.tex")
#eq_eigenvalue= read_latex("latex/seirs.eigenvalue.tex")

ui = fluidPage( theme=("css/style.css"),
                shinyjs::useShinyjs(),
                htmlOutput("masthead"),
                navbarPage("Uncertainty and management",id="tabs",
                  #tabsetPanel(id="tabs",
                           tabPanel(HTML("Uncertainty in 1/<i>&omega;</i> and <i>R</i><sub>0</sub>"),value=1,id=1,
                                    fluidRow(
                                      column(3,id="sidebar1",
                                        div(id="form1",
                                        HTML("<p class='normal grey small'>Uncertainty in <i>R</i><sub>0</sub> and 1/<i>&omega;</i> will be modeled by simulating minimum and maximum values.</p>"),
                                        sliderInput("R01", HTML("Basic reproduction number, <i>R</i><sub>0</sub>"),
                                                        value = c(R0M_default,R0_default),
                                                        min = R0_min, max = R0_max, step = R0_step),
                                        sliderInput("id1", HTML("Immunity duration, 1/<i>&omega;</i> (years)"),
                                                    value = c(idshort_default,idlong_default),
                                                    min = id_step, max = id_max, step = id_step),
                                        HTML("<p class='normal grey small'>Remaining parameters are assumed to be accurately known and will remain fixed.</p>"),
                                        sliderInput("ip1", HTML("Infectious period, 1/<i>&gamma;</i> (days)"),
                                                    value = ip_default,
                                                    min = 1, max = ip_max, step = ip_step),
                                        sliderInput("lp1", HTML("Latent period, 1/<i>&sigma;</i> (days)"),
                                                    value = lp_default,
                                                    min = lp_min, max = lp_max, step = lp_step),
                                        sliderInput("le1", HTML("Life expectancy, 1/<i>&mu;</i> (years)"),
                                                    value = le_default,
                                                    min = 1, max = le_max, step = le_step),
                                        sliderInput("al1", HTML("Death onset, 1/<i>&alpha;</i> (days)"),
                                                    value = al_default,
                                                    min = 0, max = al_max, step = al_step),
                                        sliderInput("p1", HTML("annual vaccination rate, <i>p</i> (% per year)"), 
                                                    value = 0,
                                                    min = 0, max = 99, step = 1),
                                        sliderInput("tmax1", HTML("time, <i>t</i><sub>max</sub> (years)"), 
                                                    value = tmax_default,
                                                    min = tmax_min, max = tmax_max, step = tmax_step),
                                        fluidRow(
                                          column(8,offset=2,id="buttons",
                                        checkboxInput("log1", HTML("log<sub>10</sub> axes"), FALSE),
                                        #checkboxInput("points1", HTML("show time points"), TRUE),
                                        #checkboxInput("sir1",HTML("show SIR <i>I</i>(<i>t</i>) trajectory"),TRUE),
                                        checkboxInput("text1",HTML("interpretive text"),interpretive_default),
                                        checkboxInput("captions1",HTML("figure captions"),captions_default),
                                        actionButton("refresh1","Reset")
                                          )
                                        )
                                        )
                                      ),
                                      column(8,id="main1",
                                      #mainPanel(
                                        div(HTML("Timing of epidemic peaks is impacted by uncertainty in <i>R</i><sub>0</sub> and immunity duration"),class="paneltitle"),
                                        div(htmlOutput("text1intro"),class="copy copy1"),
                                        tabsetPanel(
                                          tabPanel("Trajectories",
                                                #div(htmlOutput("text1a"),class="copy copy1"),
                                                div(
                                                  div(htmlOutput("title1a"),class="title"),
                                                  div(plotOutput("plot1a",height=400,width="auto"),class="plot"),
                                                  div(htmlOutput("caption1a"),class="caption caption1"),
                                                  div(htmlOutput("title1b"),class="title"),
                                                  div(plotOutput("plot1b",height=400,width="auto"),class="plot"),
                                                  div(htmlOutput("caption1b"),class="caption caption1"),
                                                  class="plotbox")
                                          ),
                                          tabPanel("Phase plane",
                                                  #div(htmlOutput("text1b"),class="copy copy1"),
                                                div(
                                                  div(htmlOutput("title1c"),class="title"),
                                                  div(plotOutput("plot1c",height=500,width="auto"),class="plot"),
                                                  div(htmlOutput("caption1c"),class="caption caption1"),
                                                  div(htmlOutput("title1d"),class="title"),
                                                  div(plotOutput("plot1d",height=500,width="auto"),class="plot"),
                                                  div(htmlOutput("caption1d"),class="caption caption1"),
                                                class="plotbox")
                                          )
                                        )
                                      )
                                    )),
                           
                           tabPanel("Cumulative burden",value=2,id=2,
                                    fluidRow(
                                      column(3,id="sidebar4",
                                             #sidebarPanel(                                             
                                             div(id="form4",
                                                 HTML("<p class='normal grey small'>Uncertainty in <i>R</i><sub>0</sub> and 1/<i>&omega;</i> will be modeled by simulating minimum and maximum values.</p>"),
                                                 sliderInput("R04", HTML("Basic reproduction number, <i>R</i><sub>0</sub>"), 
                                                             value = c(R0M_default,R0_default),                                                                                                                     
                                                             min = R0_min, max = R0_max, step = R0_step),
                                                 sliderInput("id4", HTML("Immunity duration, 1/<i>&omega;</i> (years)"),
                                                             value = c(idshort_default,idlong_default),
                                                             min = 0.25, max = id_max, step = 0.25),
                                                 HTML("<p class='normal grey small'>Remaining parameters are assumed to be accurately known and will remain fixed.</p>"),
                                                 sliderInput("ip4", HTML("Infectious period, 1/<i>&gamma;</i> (days)"), 
                                                             value = ip_default,
                                                             min = 1, max = ip_max, step = ip_step),
                                                 sliderInput("lp4", HTML("Latent period, 1/<i>&sigma;</i> (days)"),
                                                             value = lp_default,
                                                             min = lp_min, max = lp_max, step = lp_step),
                                                 sliderInput("le4", HTML("Life expectancy, 1/<i>&mu;</i> (years)"),
                                                             value = le_default,
                                                             min = 1, max = le_max, step = le_step),
                                                 sliderInput("al4", HTML("Death onset, 1/<i>&alpha;</i> (days)"),
                                                             value = al_default,
                                                             min = 0, max = al_max, step = al_step),
                                                 sliderInput("p4", HTML("annual vaccination rate, <i>p</i> (% per year)"), 
                                                    value = 0,
                                                    min = 0, max = 99, step = 1),
                                                 sliderInput("tmax4", HTML("time horizon, <i>t</i><sub>max</sub> (years)"), 
                                                             value = tmax_default,
                                                             min = tmax_min, max = tmax_max, step = tmax_step),
                                                 fluidRow(
                                                   column(8,offset=2,id="buttons",
                                                          #checkboxInput("log4", HTML("log<sub>10</sub> axes"), FALSE),
                                                          #checkboxInput("points1", HTML("show time points"), TRUE),
                                                          #checkboxInput("sir1",HTML("show SIR <i>I</i>(<i>t</i>) trajectory"),TRUE),
                                                          checkboxInput("text4",HTML("interpretive text"),interpretive_default),
                                                          checkboxInput("captions4",HTML("figure captions"),captions_default),
                                                          actionButton("refresh4","Reset")
                                                   )
                                                 )
                                             )
                                      ),
                                      column(8,id="main4",
                                             #mainPanel(
                                             div(HTML("Cumulative disease burden"),class="paneltitle"),
                                             div(htmlOutput("text4intro"),class="copy copy4"),
                                             #tabsetPanel(
                                             #  tabPanel("Trajectories",
                                                        #div(htmlOutput("text1a"),class="copy copy1"),
                                                        div(
                                                          div(htmlOutput("title4a"),class="title"),
                                                          div(plotOutput("plot4a",height=500,width="auto"),class="plot"),
                                                          div(htmlOutput("caption4a"),class="caption caption4"),
                                                          class="plotbox")
                                              # )
                                               #tabPanel("Phase plane",
                                               #         #div(htmlOutput("text1b"),class="copy copy1"),
                                               #         div(
                                               #           div(htmlOutput("title4b"),class="title"),
                                               #           div(plotOutput("plot4b",height=500,width="auto"),class="plot"),
                                               #           div(htmlOutput("caption4b"),class="caption caption4"),
                                               #           class="plotbox")
                                               #)
                                             
                                      
                                    ))),
                           
                           
                           tabPanel("Vaccination",value=3,id=3,
                                    fluidRow(
                                      column(3,id="sidebar3",
                                             #sidebarPanel(
                                             div(id="form3",
                                                 sliderInput("R03", HTML("Basic reproduction number, <i>R</i><sub>0</sub>"), 
                                                             value = R0M_default,
                                                             min = R0_min, max = R0_max, step = R0_step),
                                                 sliderInput("ip3", HTML("Infectious period, 1/<i>&gamma;</i> (days)"), 
                                                             value = ip_default,
                                                             min = 1, max = ip_max, step = ip_step),
                                                 sliderInput("lp3", HTML("Latent period, 1/<i>&sigma;</i> (days)"),
                                                             value = lp_default,
                                                             min = lp_min, max = lp_max, step = lp_step),
                                                 sliderInput("id3", HTML("Immunity duration, 1/<i>&omega;</i> (years)"),
                                                             value = idshort_default,
                                                             min = id_step, max = id_max, step = id_step),
                                                 sliderInput("le3", HTML("Life expectancy, 1/<i>&mu;</i> (years)"),
                                                             value = le_default,
                                                             min = 1, max = le_max, step = le_step),
                                                 sliderInput("al3", HTML("Death onset, 1/<i>&alpha;</i> (days)"),
                                                             value = al_default,
                                                             min = 0, max = al_max, step = al_step),
                                                 sliderInput("tmax3", HTML("time horizon, <i>t</i><sub>max</sub> (years)"), 
                                                             value = tmax_default,
                                                             min = tmax_min, max = tmax_max, step = tmax_step),
                                                 fluidRow(
                                                   column(8,offset=2,id="buttons",
                                                          #checkboxInput("log1", HTML("log<sub>10</sub> axes"), FALSE),
                                                          #checkboxInput("points1", HTML("show time points"), TRUE),
                                                          #checkboxInput("sir1",HTML("show SIR <i>I</i>(<i>t</i>) trajectory"),TRUE),
                                                          checkboxInput("text3",HTML("interpretive text"),interpretive_default),
                                                          checkboxInput("captions3",HTML("figure captions"),captions_default),
                                                          actionButton("refresh3","Reset")
                                                   )
                                                 )
                                             )
                                      ),
                                      column(8,id="main3",
                                             #mainPanel(
                                             div(HTML("Effect of vaccination"),class="paneltitle"),
                                             div(htmlOutput("text3intro"),class="copy copy3"),
                                             tabsetPanel(
                                               tabPanel("Trajectories",
                                                        #div(htmlOutput("text1a"),class="copy copy1"),
                                                        div(
                                                          div(htmlOutput("title3a"),class="title"),
                                                          div(plotOutput("plot3a",height=500,width="auto"),class="plot"),
                                                          div(htmlOutput("caption3a"),class="caption caption3"),
                                                          class="plotbox")
                                               ),
                                               tabPanel("Phase plane",
                                                        #div(htmlOutput("text1b"),class="copy copy1"),
                                                        div(
                                                          div(htmlOutput("title3b"),class="title"),
                                                          div(plotOutput("plot3b",height=500,width="auto"),class="plot"),
                                                          div(htmlOutput("caption3b"),class="caption caption3"),
                                                          class="plotbox")
                                               )
                                             )
                                      )
                                    )),
                           
                           tabPanel("EVPI",value=4,id=4,
                                    fluidRow(
                                      column(3,id="form2",
                                      #fluidRow(
                                      #  column(6,HTML("<h5 class=t1>Scenario 1</h5>")),
                                      #  column(6,HTML("<h5 class=t2>Scenario 2</h5>"))
                                      #),
                                      sliderInput("R02", HTML("Basic reproduction number, <i>R</i><sub>0</sub>"), 
                                                  value = c(R0_A1,R0_A2),
                                                  min = R0_min, max = R0_max, step = R0_step),
                                      sliderInput("p2", HTML("annual vaccination rate, <i>p</i> (% per year)"), 
                                                  value = c(p_A1,p_A2),
                                                  min = p_min, max = p_max, step = p_step),
                                      sliderInput("id2", HTML("Immunity duration, 1/<i>&omega;</i> (years)"),
                                                  value = c(idshort_default,idlong_default),
                                                  min = id_step, max = id_max, step = id_step),
                                      sliderInput("ip2", HTML("Infectious period, 1/<i>&gamma;</i> (days)"), 
                                                  value = ip_default,
                                                  min = 1, max = ip_max, step = ip_step),
                                      sliderInput("lp2", HTML("Latent period, 1/<i>&sigma;</i> (days)"),
                                                  value = lp_default,
                                                  min = lp_min, max = lp_max, step = lp_step),
                                     
                                      sliderInput("le2", HTML("Life expectancy, 1/<i>&mu;</i> (years)"),
                                                  value = le_default,
                                                  min = 1, max = le_max, step = le_step),
                                      sliderInput("al2", HTML("Death onset, 1/<i>&alpha;</i> (days)"),
                                                  value = al_default,
                                                  min = 0, max = al_max, step = al_step),
                                     
                                      sliderInput("tmax2", HTML("time horizon, <i>t</i><sub>max</sub> (years)"), 
                                                  value = tmax_default,
                                                  min = tmax_min, max = tmax_max, step = tmax_step),                  
                                      #sliderInput("R0max2", HTML("<i>R</i><sub>0,max</sub>"), 
                                      #            value = R0_Amax,
                                      #            min = R0_min, max = R0_max, step = R0_step),    
                                      fluidRow(
                                        column(8,offset=2,id="buttons",
                                            #checkboxInput("log2", HTML("log<sub>10</sub> axes"), TRUE),
                                            #checkboxInput("points2", HTML("show time points"),   TRUE),
                                            #checkboxInput("sir2",HTML("show SIR <i>I</i>(<i>t</i>) trajectory"),TRUE),
                                            checkboxInput("text2",HTML("interpretive text"),     interpretive_default),
                                            checkboxInput("captions2",HTML("figure captions"),   captions_default),
                                            actionButton("refresh2","Reset")
                                        )
                                      )
                                      ),
                                      column(8,id="main2",
                                                div(HTML("Expected value of perfect information"),class="paneltitle"),
                                                div(htmlOutput("text2intro"),class="copy copy2"),                                                
                                                div(                                                                                                                                                      
                                                  div(htmlOutput("title2a"),class="title"),
                                                  div(plotOutput("plot2a",height=500,width="auto"),class="plot"),
                                                  div(htmlOutput("caption2a"),class="caption caption2"),
                                                  div(htmlOutput("title2b"),class="title"),
                                                  div(htmlOutput("evpi"),class="copy"),
                                                  div(htmlOutput("caption2b"),class="caption caption2"),
                                                  class="plotbox")
                                      )
                                    )),
                           
                           tabPanel("Equations",value=5,id=5,
                                    p(HTML("SEIRS system with continuous vaccination (dot indicates time derivative)")),
                                    withMathJax(paste("$$",eq_seirs,"$$")),
                                    p(HTML("SEIRS Parameters (mean values)")),
                                    withMathJax(paste("$$",eq_param,"$$")),
                                    p(HTML("Basic reproduction number")),
                                    withMathJax(paste("$$",eq_r0,"$$")),
                                    p(HTML("Expected value of perfect information (EVPI) for the cumulative burden <i>B<sub>ij</sub></i> for model <i>i</i> and action <i>j</i>")),
                                    withMathJax(paste("$$",eq_evpi,"$$")),                                    
                                    p(paste("Infection trajectories show a numerical solution to the SEIRS equations with",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps and initial parameters",sep=" ")),
                                    withMathJax(
                                      sprintf("$$S(0) = %s$$",1-sir_init_i),
                                      "$$E(0) = R(0) = 0$$",
                                      sprintf("$$I(0) = B(0) = %s$$",sir_init_i),
                                      "$$S + E + I + R = N = 1$$"),
                                    p(HTML("where <i>B</i> is the cumulative disease burden."))
                          ),
                           
                           tabPanel("Download & Credits",value=6,id=6,
                                    mainPanel(
                                      h3("Points of Significance: The SEIRS model for infectious disease dynamics"),
                                      p(HTML("Ottar Bjørnstad<sup>1,2</sup>, Katriona Shea<sup>1</sup>, Martin Krzywinski<sup>3*</sup>, Naomi Altman<sup>4</sup>")),
                                      div(
                                      p("1. Department of Biology, The Pennsylvania State University, State College, PA, USA."),
                                      p("2. Department of Entomology, The Pennsylvania State University, State College, PA, USA."),
                                      p("3. Canada’s Michael Smith Genome Sciences Center, Vancouver, British Columbia, Canada."),
                                      p("4. Department of Statistics, The Pennsylvania State University, State College, PA, USA."),
                                        class="affiliations"),
                                      p("*",tags$a(href="mailto:martink@bcgsc.ca",tags$i("martink@bcgsc.ca"))),

                                      hr(),
                                      h4("Download code"),
                                      p(tags$a(href="https://github.com/martinkrz/posepi2","https://github.com/martinkrz/posepi3")),
                                      
                                      hr(),
                                      h4("Citation"),
                                      p(HTML("Shea, K., Bjørnstad, O., Krzywinski, M. & Altman, N. <a href='http://www.nature.com/articles/s41592-020-0856-2'>Points of Significance: Uncertainty and the modelling and management of infectious epidemics</a>. (2020) <i>Nature Methods</i> <b>17</b>:in press.")),
                                      hr(),
                                      h4("Version history"),
                                      HTML("<p>27 July 2020 v1.0.0 &mdash; initial public release</p>"),
                                      hr(),
                                      h4("Related columns"),
                                      p(HTML("Bjørnstad, O., Shea, K., Krzywinski, M. & Altman, N. <a href='https://www.nature.com/articles/s41592-020-0822-z'>Points of Significance: Modelling infectious epidemics.</a> (2020) <i>Nature Methods</i> <b>17</b>:455&ndash;456. (<a href='http://shiny.bcgsc.ca/posepi1'>interactive figures</a>, <a href='https://github.com/martinkrz/posepi1'>download code</a>)")),
                                      p(HTML("Bjørnstad, O., Shea, K., Krzywinski, M. & Altman, N. <a href='https://www.nature.com/articles/s41592-020-0856-2'>Points of Significance: The SEIRS model for infectious disease dynamics.</a> (2020) <i>Nature Methods</i> <b>17</b>:557&ndash;558. (<a href='http://shiny.bcgsc.ca/posepi2'>interactive figures</a>, <a href='https://github.com/martinkrz/posepi2'>download code</a>).")),
                                      
                                      width=16
                                      
                                    ))
                          # )
                ))
