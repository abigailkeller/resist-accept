library(tidyverse)
library(shinycssloaders)
library(shinyWidgets)

# Source helper functions -----
source('create_objectives.R')
source('create_utility.R')
source('create_stationary.R')
source('create_nonstationary.R')
source('setSliderColor.R')

### 
# params
#define states
K <- 100000 # carrying capacity
states <- seq(0,1.4*K,by=1000)
states_scale <- states/K

#vector of actions: units of effort, ranging from 0 to 1
actions <- seq(0, 1, 0.01)

#loss params -- sigmoidal
sig_loss_params <- list(
  loss_a=-0.2,
  loss_b=15,
  loss_c=0.4
)
#loss params -- exponential
exp_loss_params <- list(
  loss_a=0.204,
  loss_b=4,
  loss_c=1
)


ui <- navbarPage("Resist-Accept Decision Boundary",
                 id = 'nav-page',
                 
                 theme = 'shiny.css',
                 #Home page
                 tabPanel('Home',
                          fluidPage(
                            mainPanel(fluidRow(column(10, align = 'center', offset = 1,
                                                      div(style = "text-align:center,bottom-padding: 10px",
                                                          h1('Navigating the decision boundary between ',strong('resistance'),' and ',
                                                          strong('acceptance'),' in marine invasive species management.',
                                                             style='color:#660033')))),
                                      width = 12,
                                      tags$hr(),
                                      fluidRow(column(6,
                                                    div(style = "text-align:center; border-style: solid; border-color: black;",
                                                        h2('Expressing decision-maker values',style='color:#006699'),
                                                        h3('Customize your own utility function and discounting of future rewards 
                                                           to find optimal removal efforts in a system with stationary dynamics.'))),
                                               column(6,
                                                      div(style = "text-align:center; border-style: solid; border-color: black",
                                                          h2('Accounting for non-stationarity',style='color:#006699'),
                                                          h3('Describe non-stationary system dynamics to find how accounting for non-stationarity
                                                             impacts the optimal removal effort.')))
                                      ),
                                      tags$hr(),
                                      fluidRow(column(12, align="center",
                                                      img(src='diagram_effort_colonization.png',
                                                                height = '60%',
                                                                width = '60%'))
                                      ),
                                      tags$hr()
                            )
                          )),
                 #Values page
                 tabPanel('Expressing decision-maker values',
                          h2('Expressing decision-maker values',align = 'center',style='color:#660033'),
                          tags$hr(),
                          setSliderColor(c('#660033','#660033','#660033','#660033'), 
                                         c(1,2,3,4)),
                          fluidPage(
                            fluidRow(column(12,
                                            h3('Customize your own utility function and discounting of future rewards 
                                                           to find optimal removal efforts in a system with stationary dynamics.')
                                            )),
                            tags$hr(),
                            sidebarPanel(
                              h4('1. Create your utility function.'),
                              radioButtons('eco_type',
                                           'Select an ecological change function:',
                                           choices = c('Both','Exponential','Sigmoidal'),
                                           inline = TRUE,selected = character(0)),
                              radioButtons('penalty_type',
                                           'Select a removal cost function:',
                                           choices = c('Both','Nonlinear','Linear'),
                                           inline = TRUE,selected = character(0)),
                              conditionalPanel(
                                condition = "(input.penalty_type == 'Nonlinear' || input.penalty_type == 'Both')",
                                h5(strong('Select an exponent for the nonlinear removal cost function:')),
                                actionLink('help_nonlinear_exp', label = 'Help'),
                                sliderInput(inputId = 'nonlinear_exp',
                                            label = '',
                                            min = 0.001,
                                            max = 10,
                                            value = 4,
                                            step = 0.5),
                                tags$hr()
                              ),
                              h5(strong('Choose objective weights:')),
                              actionLink('help_obj_weights', label = 'Help'),
                              sliderInput(inputId = "weight_eco", 
                                          label = 'Minimize ecological change:',
                                          min = 0, max = 1, value = 0.98, step = 0.01),
                              sliderInput(inputId = "weight_cost", 
                                          label = 'Minimize removal cost:',
                                          min = 0, max = 1, value = 0.02, step = 0.01),
                              tags$hr(),
                              h4('2. Choose a discount factor.'),
                              actionLink('help_discount_factor', label = 'Help'),
                              sliderInput(inputId = 'discount_factor',
                                          label = '',
                                          min = 0.75,
                                          max = 0.99,
                                          value = 0.99,
                                          step = 0.01),
                              tags$hr(),
                              h4('3. Choose a mean source population(s) size.'),
                              actionLink('help_sourcepop', label = 'Help'),
                              selectInput(inputId = 'source_pop',
                                          label = NULL,
                                          selected = '0.05K',
                                          choices = c('0.005K','0.05K','0.2K','0.5K','0.75K',
                                                      '1.0K','1.25K','1.5K','2.0K','5.0K')),
                              actionButton(inputId = 'AB_stationary',
                                           label = 'Get stationary policy'),
                              tags$style('#AB_stationary{background-color: #660033;color:#FFFFFF;border-color:#000000;}
                                                                   #AB_stationary:hover{background-color:#006699;}')
                              ),
                            
                            mainPanel(
                              h4('Utility function components:'),
                              tags$hr(),
                              conditionalPanel(
                                condition = "(input.eco_type == 'Both' || input.eco_type == 'Exponential' || 
                                input.eco_type == 'Sigmoidal') && (input.penalty_type == 'Both' || input.penalty_type == 'Linear' || input.penalty_type == 'Nonlinear')",
                                fluidRow(
                                  #plot
                                  splitLayout(withSpinner(plotOutput('eco_plot'), type = 4,
                                                          color = '#660033'),
                                              withSpinner(plotOutput('penalty_plot'), type = 4,
                                                          color = '#660033'))),
                                tags$hr(),
                                h4('Stationary optimal removal effort:')
                              ),
                              withSpinner(plotOutput('stationary_plot'), type = 4,
                                          color = '#660033'),
                              tags$hr()
                              
                            )
                           )
                          ),
                 
                 # non-stationary page
                 tabPanel('Accounting for non-stationarity',
                          h2('Accounting for non-stationarity',align = 'center',style='color:#660033'),
                          tags$hr(),
                          
                          fluidPage(
                            fluidRow(column(12,
                                            h3('Describe non-stationary system dynamics to find how accounting for non-stationarity
                          impacts the optimal removal effort.')
                                            )),
                            tags$hr(),
                            sidebarPanel(
                              h4('1. Create your utility function.'),
                              actionLink('note', label = 'Note'),
                              radioButtons('eco_type_nonstat',
                                           'Select an ecological change function:',
                                           choices = c('Exponential','Sigmoidal'),
                                           inline = TRUE,selected = character(0)),
                              tags$hr(),
                              h5(strong('Select an exponent for the nonlinear removal cost function:')),
                              actionLink('help_nonlinear_exp_nonstat', label = 'Help'),
                              sliderInput(inputId = 'nonlinear_exp_nonstat',
                                          label = '',
                                          min = 0.001,
                                          max = 10,
                                          value = 4,
                                          step = 0.5),
                              tags$hr(),
                              h5(strong('Choose objective weights:')),
                              actionLink('help_obj_weights_nonstat', label = 'Help'),
                              sliderInput(inputId = "weight_eco_nonstat", 
                                          label = 'Minimize ecological change:',
                                          min = 0, max = 1, value = 0.98, step = 0.01),
                              sliderInput(inputId = "weight_cost_nonstat", 
                                          label = 'Minimize removal cost:',
                                          min = 0, max = 1, value = 0.02, step = 0.01),
                              tags$hr(),
                              h4('2. Choose a discount factor.'),
                              actionLink('help_discount_factor_nonstat', label = 'Help'),
                              sliderInput(inputId = 'discount_factor_nonstat',
                                          label = '',
                                          min = 0.75,
                                          max = 0.99,
                                          value = 0.99,
                                          step = 0.01),
                              tags$hr(),
                              h4('3. Create non-stationary dynamics.'),
                              radioButtons('regime_no',
                                           'Choose the number of invasion regimes:',
                                           choices = c('2','3','4'),
                                           inline = TRUE,selected = character(0)),
                              # two regimes
                              conditionalPanel(
                                condition = "input.regime_no == '2'",
                                tags$hr(),
                                h5(strong('Choose a source population(s) size for each regime:')),
                                actionLink('help_sourcepop_2a', label = 'Help'),
                                fixedRow(
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'sourcepop_2a',
                                                         label = 'Regime 1',
                                                         selected = '0.2K',
                                                         choices = c('0.005K','0.05K','0.2K','0.5K','0.75K',
                                                                     '1.0K','1.25K','1.5K','2.0K','5.0K')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'sourcepop_2b',
                                                         label = 'Regime 2:',
                                                         selected = '0.2K',
                                                         choices = c('0.005K','0.05K','0.2K','0.5K','0.75K',
                                                                     '1.0K','1.25K','1.5K','2.0K','5.0K'))))
                                  )
                                ),
                              # three regimes
                              conditionalPanel(
                                condition = "input.regime_no == '3'",
                                tags$hr(),
                                h5(strong('Choose a source population(s) size for each regime:')),
                                actionLink('help_sourcepop_3a', label = 'Help'),
                                fixedRow(
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'sourcepop_3a',
                                                         label = 'Regime 1',
                                                         selected = '0.2K',
                                                         choices = c('0.005K','0.05K','0.2K','0.5K','0.75K',
                                                                     '1.0K','1.25K','1.5K','2.0K','5.0K')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'sourcepop_3b',
                                                         label = 'Regime 2:',
                                                         selected = '0.2K',
                                                         choices = c('0.005K','0.05K','0.2K','0.5K','0.75K',
                                                                     '1.0K','1.25K','1.5K','2.0K','5.0K')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'sourcepop_3c',
                                                         label = 'Regime 3:',
                                                         selected = '0.2K',
                                                         choices = c('0.005K','0.05K','0.2K','0.5K','0.75K',
                                                                     '1.0K','1.25K','1.5K','2.0K','5.0K'))))
                                )
                              ),
                              # four regimes
                              conditionalPanel(
                                condition = "input.regime_no == '4'",
                                tags$hr(),
                                h5(strong('Choose a source population(s) size for each regime:')),
                                actionLink('help_sourcepop_4a', label = 'Help'),
                                fixedRow(
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'sourcepop_4a',
                                                         label = 'Regime 1',
                                                         selected = '0.2K',
                                                         choices = c('0.005K','0.05K','0.2K','0.5K','0.75K',
                                                                     '1.0K','1.25K','1.5K','2.0K','5.0K')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'sourcepop_4b',
                                                         label = 'Regime 2:',
                                                         selected = '0.2K',
                                                         choices = c('0.005K','0.05K','0.2K','0.5K','0.75K',
                                                                     '1.0K','1.25K','1.5K','2.0K','5.0K')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'sourcepop_4c',
                                                         label = 'Regime 3:',
                                                         selected = '0.2K',
                                                         choices = c('0.005K','0.05K','0.2K','0.5K','0.75K',
                                                                     '1.0K','1.25K','1.5K','2.0K','5.0K')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'sourcepop_4d',
                                                         label = 'Regime 4:',
                                                         selected = '0.2K',
                                                         choices = c('0.005K','0.05K','0.2K','0.5K','0.75K',
                                                                     '1.0K','1.25K','1.5K','2.0K','5.0K'))))
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.regime_no == '1' || input.regime_no == '2' || input.regime_no == '3' || input.regime_no == '4'",
                                actionButton(inputId = 'AB_nonstationary',
                                             label = 'Get non-stationary policy'),
                                tags$style('#AB_nonstationary{background-color: #660033;color:#FFFFFF;border-color:#000000;}
                                                                   #AB_nonstationary:hover{background-color:#006699;}')
                              )
                            ),
                            
                            mainPanel(
                              h4('Utility function components:'),
                              tags$hr(),
                              conditionalPanel(
                                condition = "input.eco_type_nonstat == 'Exponential' || 
                                input.eco_type_nonstat == 'Sigmoidal'",
                                fluidRow(
                                  #plot
                                  splitLayout(withSpinner(plotOutput('eco_plot_nonstat'), type = 4,
                                                          color = '#660033'),
                                              withSpinner(plotOutput('penalty_plot_nonstat'), type = 4,
                                                          color = '#660033'))),
                                tags$hr(),
                                h4('Δ optimal removal effort:'),
                              ),
                              withSpinner(plotOutput('nonstationary_plot',
                                                     click = 'nonstat_hover'), type = 4,
                                          color = '#660033'),
                              tags$hr(),
                              htmlOutput('output_check'),
                              tags$head(tags$style("
                               #output_check {
                                color: #FFFFFF; font-size: 2px}")),
                              conditionalPanel(condition = "output.output_check == 'yes'",
                                               h4('Click anywhere on the above plot to see the stationary and non-stationary policies for the associated time point.'),
                                               tags$hr()),
                              conditionalPanel(condition = "input.nonstat_hover != null",
                                               plotOutput('hover_plot'))
                                               # tags$hr())
                              
                            )
                          )
                 )
                 
)

server <- function(input, output, session) {
  
  ########
  # popups
  
  # Note about not having linear removal cost function
  observeEvent(input$note, {
    showModal(modalDialog(
      h5(HTML(paste0("With the linear removal cost function, the optimal removal effort is often equal to a",tags$sub("max"),
                     ", or the maximum possible removal effort in the optimization. This makes comparisons to stationary policies challenging
                     if both the stationary and non-stationary policies are equal to a",tags$sub("max"),"."))),
      title = "In this section, only a nonlinear removal cost function is used.",
      easyClose = TRUE, footer = NULL)
    )
  })
  
  observeEvent(input$help_nonlinear_exp, {
    showModal(modalDialog(
      h5("With the nonlinear removal cost function, the reward scales nonlinearly with action (removal effort)."),
      h5("If the exponent is greater than one, the per unit change in reward is greater at higher actions."),
      h5("If the exponent is between zero and one, the per unit change in reward is greater at lower actions."),
      h5("For reference, an exponent of 4 is used in the main manuscript."),
      tags$hr(),
      tags$img(
        src = base64enc::dataURI(file = "www/penalty_plot.jpg", mime = "image/jpg"),
        height = '320px',width = '640px'
      ),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_nonlinear_exp_nonstat, {
    showModal(modalDialog(
      h5("With the nonlinear removal cost function, the reward scales nonlinearly with action (removal effort)."),
      h5("If the exponent is greater than one, the per unit change in reward is greater at higher actions."),
      h5("If the exponent is between zero and one, the per unit change in reward is greater at lower actions."),
      h5("For reference, an exponent of 4 is used in the main manuscript."),
      tags$hr(),
      tags$img(
        src = base64enc::dataURI(file = "www/penalty_plot.jpg", mime = "image/jpg"),
        height = '320px',width = '640px'
      ),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_obj_weights, {
    showModal(modalDialog(
      h5("This decision problem is a multi-objective programming problem, where the goal is to find the optimal level 
      of removal effort that both minimizes the cost of removal and minimizes ecological change. "),
      h5("The problem is solved using a weighted-sum method, 
         where relevant weights are assigned to objectives and are used to develop a single criterion."),
      h5("These weights indicate the relative importance of the different objectives and must sum to 1."),
      tags$hr(),
      h5(HTML("The utility associated with action <i>a</i>, and state <i>s</i>, is the weighted sum of 
                  the values associated with removal cost, <i>V<sub>c</sub></i>, and ecological damage, <i>V<sub>d</sub></i>.")),
      withMathJax("$$U(s_t,a_t) = w_cV_c(a_t) + w_dV_d(s_t)$$"),
      h5(HTML("The relative importance of minimizing removal cost and minimizing ecological damage 
                  are indicated through weights, <i>w<sub>c</sub></i> and <i>w<sub>d</sub></i>")),
      tags$hr(),
      h5(HTML("For reference, objective weights of 0.02 and 0.98 for <i>w<sub>c</sub></i> and <i>w<sub>d</sub></i>, respectively, are 
                  used in the main manuscript. These weights indicate that minimizing ecological damage is much more important
                  than minimizing removal cost.")),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_obj_weights_nonstat, {
    showModal(modalDialog(
      h5("This decision problem is a multi-objective programming problem, where the goal is to find the optimal level 
      of removal effort that both minimizes the cost of removal and minimizes ecological change. "),
      h5("The problem is solved using a weighted-sum method, 
         where relevant weights are assigned to objectives and are used to develop a single criterion."),
      h5("These weights indicate the relative importance of the different objectives and must sum to 1."),
      tags$hr(),
      h5(HTML("The utility associated with action <i>a</i>, and state <i>s</i>, is the weighted sum of 
                  the values associated with removal cost, <i>V<sub>c</sub></i>, and ecological damage, <i>V<sub>d</sub></i>.")),
      withMathJax("$$U(s_t,a_t) = w_cV_c(a_t) + w_dV_d(s_t)$$"),
      h5(HTML("The relative importance of minimizing removal cost and minimizing ecological damage 
                  are indicated through weights, <i>w<sub>c</sub></i> and <i>w<sub>d</sub></i>")),
      tags$hr(),
      h5(HTML("For reference, objective weights of 0.02 and 0.98 for <i>w<sub>c</sub></i> and <i>w<sub>d</sub></i>, respectively, are 
                  used in the main manuscript. These weights indicate that minimizing ecological damage is much more important
                  than minimizing removal cost.")),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_discount_factor, {
    showModal(modalDialog(
      h5("The discount factor is a value between 0 and 1 that balances the importance of immediate rewards vs. future rewards."),
      tags$hr(),
      h5("For reference, a discount factor of 0.99 is used in the main manuscript, which means that the importance of future rewards is nearly equal to the importance of immediate rewards."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_discount_factor_nonstat, {
    showModal(modalDialog(
      h5("The discount factor is a value between 0 and 1 that balances the importance of immediate rewards vs. future rewards."),
      tags$hr(),
      h5("For reference, a discount factor of 0.99 is used in the main manuscript, which means that the importance of future rewards is nearly equal to the importance of immediate rewards."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_sourcepop, {
    showModal(modalDialog(
      h5("The source population(s) size, \u03B1, is stochastic, with a mean and variance that reflects the frequency and magnitude of connectivity of source population(s) to the local population."),
      tags$hr(),
      h5(paste0("The figure below depicts the source population(s) size propability density associated with the expected (mean) source population size. ",
                "The source population sizes are scaled relative to the local carrying capacity, K.")),
      tags$img(
        src = base64enc::dataURI(file = "www/sourcepop.jpg", mime = "image/jpg"),
        height = '700px',width = '400px'
      ),
      h5("Note: To aid visualization, the probability densities for the lowest and highest source population(s) sizes are shown in separate panels."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_sourcepop_2a, {
    showModal(modalDialog(
      h5("The source population(s) size, \u03B1, is stochastic, with a mean and variance that reflects the frequency and magnitude of connectivity of source population(s) to the local population."),
      tags$hr(),
      h5(paste0("The figure below depicts the source population(s) size propability density associated with the expected (mean) source population size. ",
                "The source population sizes are scaled relative to the local carrying capacity, K.")),
      tags$img(
        src = base64enc::dataURI(file = "www/sourcepop.jpg", mime = "image/jpg"),
        height = '700px',width = '400px'
      ),
      h5("Note: To aid visualization, the probability densities for the lowest and highest source population(s) sizes are shown in separate panels."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_sourcepop_3a, {
    showModal(modalDialog(
      h5("The source population(s) size, \u03B1, is stochastic, with a mean and variance that reflects the frequency and magnitude of connectivity of source population(s) to the local population."),
      tags$hr(),
      h5(paste0("The figure below depicts the source population(s) size propability density associated with the expected (mean) source population size. ",
                "The source population sizes are scaled relative to the local carrying capacity, K.")),
      tags$img(
        src = base64enc::dataURI(file = "www/sourcepop.jpg", mime = "image/jpg"),
        height = '700px',width = '400px'
      ),
      h5("Note: To aid visualization, the probability densities for the lowest and highest source population(s) sizes are shown in separate panels."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_sourcepop_4a, {
    showModal(modalDialog(
      h5("The source population(s) size, \u03B1, is stochastic, with a mean and variance that reflects the frequency and magnitude of connectivity of source population(s) to the local population."),
      tags$hr(),
      h5(paste0("The figure below depicts the source population(s) size propability density associated with the expected (mean) source population size. ",
                "The source population sizes are scaled relative to the local carrying capacity, K.")),
      tags$img(
        src = base64enc::dataURI(file = "www/sourcepop.jpg", mime = "image/jpg"),
        height = '700px',width = '400px'
      ),
      h5("Note: To aid visualization, the probability densities for the lowest and highest source population(s) sizes are shown in separate panels."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  ############
  # stationary
  ############
  
  observeEvent(input$weight_eco, {
    updateSliderInput(session, "weight_cost", value = 1 - input$weight_eco)
  })
  
  observeEvent(input$weight_cost, {
    updateSliderInput(session, "weight_eco", value = 1 - input$weight_cost)
  })
  
  # ecological change plot
  output$eco_plot <- renderPlot({
    req(input$penalty_type)
    req(input$eco_type)
    create_eco_objective_plots(input$eco_type,states_scale,sig_loss_params,
                               exp_loss_params,input$weight_eco)
  })
  
  # removal cost plot
  output$penalty_plot <- renderPlot({
    req(input$penalty_type)
    req(input$eco_type)
    create_cost_objective_plots(input$penalty_type,actions,
                                input$nonlinear_exp,input$weight_cost)
  })
  
  # create stationary df
  stationary_df <- eventReactive(input$AB_stationary, {
    req(input$penalty_type)
    req(input$eco_type)

    df <- create_stationary_df(input$eco_type,input$penalty_type,
                               c(input$weight_eco,input$weight_cost),
                           states,actions,sig_loss_params,
                           exp_loss_params,input$nonlinear_exp,
                           input$discount_factor,input$source_pop)
    
    create_stationary_plot(df,isolate(input$penalty_type),isolate(input$eco_type))

  })
  
  # create stationary plot
  output$stationary_plot <- renderPlot({
    stationary_df()
  })
  
  
  ################
  # non-stationary
  ################
  
  observeEvent(input$weight_eco_nonstat, {
    updateSliderInput(session, "weight_cost_nonstat", 
                      value = 1 - input$weight_eco_nonstat)
  })
  
  observeEvent(input$weight_cost_nonstat, {
    updateSliderInput(session, "weight_eco_nonstat", 
                      value = 1 - input$weight_cost_nonstat)
  })
  
  # ecological change plot
  output$eco_plot_nonstat <- renderPlot({
    req(input$eco_type_nonstat)
    create_eco_objective_plots(input$eco_type_nonstat,states_scale,sig_loss_params,
                               exp_loss_params,input$weight_eco_nonstat)
  })
  
  # removal cost plot
  output$penalty_plot_nonstat <- renderPlot({
    req(input$eco_type_nonstat)
    create_cost_objective_plots('Nonlinear',actions,
                                input$nonlinear_exp_nonstat,
                                input$weight_cost_nonstat)
  })
  
  # create stationary df -- for nonstationary plot
  nonstat_statdf <- eventReactive(input$AB_nonstationary, {
    req(input$eco_type_nonstat)
    req(input$regime_no)
    
    if(input$regime_no == '2'){
      alpha <- c(input$sourcepop_2a,input$sourcepop_2b)
    } else if(input$regime_no == '3'){
      alpha <- c(input$sourcepop_3a,input$sourcepop_3b,
                 input$sourcepop_3c)
    } else if(input$regime_no == '4'){
      alpha <- c(input$sourcepop_4a,input$sourcepop_4b,
                 input$sourcepop_4c,input$sourcepop_4d)
    } 
    
    stat_df <- get_nonstat_stationary(alpha,input$eco_type_nonstat,input$regime_no,
                                      input$discount_factor_nonstat,
                                      c(input$weight_eco_nonstat,
                                        input$weight_cost_nonstat),
                                      input$nonlinear_exp_nonstat,
                                      states,actions,sig_loss_params,
                                      exp_loss_params)
    
    nonstat_df <- get_nonstat_nonstationary(alpha,input$eco_type_nonstat,
                                            input$regime_no,
                                            c(input$weight_eco_nonstat,
                                              input$weight_cost_nonstat),
                                            input$nonlinear_exp_nonstat,
                                            input$discount_factor_nonstat,
                                            states,actions,sig_loss_params,
                                            exp_loss_params)
    
    combine_stat_nonstat_tile(stat_df,nonstat_df,
                              alpha,input$regime_no,states)
    
  })
  
  # output check
  output$output_check <- renderText({
    req(input$eco_type_nonstat)
    req(input$regime_no)
    
    if(length(nonstat_statdf()[[1]]$time) > 1){
      return('yes')
    }
  })
  
  # create non-stationary plot
  output$nonstationary_plot <- renderPlot({
    req(nonstat_statdf())
    
    if(input$regime_no == '2'){
      alpha <- c(input$sourcepop_2a,input$sourcepop_2b)
    } else if(input$regime_no == '3'){
      alpha <- c(input$sourcepop_3a,input$sourcepop_3b,
                 input$sourcepop_3c)
    } else if(input$regime_no == '4'){
      alpha <- c(input$sourcepop_4a,input$sourcepop_4b,
                 input$sourcepop_4c,input$sourcepop_4d)
    } 
    
    create_plot_deltaoptim(nonstat_statdf()[[1]],
                           alpha,input$regime_no)
  })
  
  #create plot hover
  output$hover_plot <- renderPlot({
    req(input$eco_type_nonstat)
    req(input$regime_no)
    
    create_hoverplot(nonstat_statdf()[[2]],round(input$nonstat_hover$x))
      
  })
  
}

shinyApp(ui = ui, server = server)