
# dependency --------------------------------------------------------------

library(shiny)
library(RPushbullet)

library(shinyFiles)
library(shinyWidgets)
library(shinypop)
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)

library(ggplot2)
library(ggplotify)


dir_gallery <- '/workspace/share/shiny/Cooplot/gallery'

# static UI ---------------------------------------------------------------
dB <- dashboardBody(
  tabItems(
    tabItem(tabName = 'init',class = "active"),
    uiOutput('dynamicBody')
  )
)

dS <- dashboardSidebar(
  shinyDirButton('gallery', 
                 'Select a gallery',
                 title ='Please select a file'),
  br(),
  awesomeCheckboxGroup('plot_list',
                       label = 'Plots',
                       choices = NULL),
  hr(),
  sidebarMenu('Loaded plots',
              uiOutput('dynamicMenu'))
)

dP <- shinydashboardPlus::dashboardPage(
  header = dashboardHeader(title = 'urBoard'),
  sidebar = dS,
  body = dB
)

# dynamic UI --------------------------------------------------------------
sbUI <- function(id){
  ns <- NS(id)
  
  tmptag <- menuItem(id,tabName = ns('plotTab'))
  tmptag$attribs <- list(id = ns('sidebar_li'))
  tmptag
}

bodyUI <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = ns('plotTab'),
    column(
      width = 2,
      box(width = 12,
          title = 'Parameters',
          fluidRow(
            column(
              width = 12,
              numericInput(ns('plot_width'), 'Width', 800, min = 1),
              numericInput(ns('plot_height'), 'Height', 800, min = 1),
              textInput(ns('download_filename'),
                        'Download as ...',
                        value = 'plot'),
              downloadBttn(ns('download')),
              actionBttn(ns('save'),'Save object')
            )
          ))
    ),
    column(10,
           box(
             width = 12,
             title = paste('Plot',id),
             uiOutput(ns('render_box'))
           ))
  )
}


# module ------------------------------------------------------------------
singular_server <- function(input,output,session,SelDir,id2mod) {
  print('in module')
  showNotification('Loading the plot',id = 'tmpNoti',duration = NULL)
  g <- readRDS(paste0(SelDir,'/',id2mod))
  removeNotification(id = 'tmpNoti')
  id <- gsub('\\..*','',id2mod)
  print('---------')
  

  
  output$render_box <- renderUI({
    plotOutput(session$ns("render"),
               width = input$plot_width,
               height = input$plot_height)
  })
  
  output$render <- renderPlot({
    g
  })
  
  output$download <- downloadHandler(
    filename = paste0(input$download_filename,'.png'),
    content = function(file) {
      ggsave(file,
             g,
             width = input$plot_width*(25/6),
             height = input$plot_height*(25/6),
             units = 'px')
    }
  )
  
  observeEvent(input$save,{
    showNotification('Saving the object',id = 'tmpNoti',duration = NULL)
    saveRDS(g,file = paste0(SelDir,'/',id2mod))
    removeNotification(id = 'tmpNoti')
  })
}

# server ------------------------------------------------------------------
server <- function(input,output,session){
  reactiveG <- reactiveValues()
  
  ## select menu 
  shinyDirChoose(
    input,
    'gallery',
    root = c(Gallery = dir_gallery),
    session = session,
    allowDirCreate = F
  )
  
  observe({
    reactiveG$selectedDir <- parseDirPath(c(Gallery = dir_gallery), 
                                          input$gallery)
    reactiveG$list <- list.files(reactiveG$selectedDir)
    
    updateAwesomeCheckboxGroup(session,
                               'plot_list',
                               choices = reactiveG$list)
    
    if(length(reactiveG$selectedDir) > 0) {
      print(paste0('Dir:',reactiveG$selectedDir,' with plots:'))
      print(reactiveG$list)
      SelDir <- reactiveG$selectedDir
      print(paste('SelDir:',SelDir))
    }
  })
  
  
  # lazy render
  change <- reactiveVal('')
  observeEvent(input$plot_list,{
    print(paste('new:',paste(input$plot_list,collapse = ',')))
    print(paste('old:',paste(change(),collapse = ',')))
    print(paste('add:',setdiff(input$plot_list,change())))
    print(paste('rm:',setdiff(change(),input$plot_list)))
    
    Add <- setdiff(input$plot_list,change())
    Rm <- setdiff(change(),input$plot_list)
    

    if(length(Add) > 0) {
      insertUI(
        selector = '#dynamicMenu',
        ui = sbUI(gsub('\\..*','',Add))
      )
      insertUI(
        selector = '#shiny-tab-init',
        ui = bodyUI(gsub('\\..*','',Add)),
        where = 'afterEnd'
      )
      callModule(singular_server,
                 id = gsub('\\..*','',Add),
                 SelDir = reactiveG$selectedDir,
                 id2mod = Add)
    }
    
    if(length(Rm) > 0) {
      removeUI(
        paste0('#',NS(gsub('\\..*','',Rm))('sidebar_li'))
      )
      removeUI(
        paste0('#shiny-tab-',
               gsub('\\..*','',Rm),
               '-plotTab')
      )
    }
    
    
    
    # refresh
    change(input$plot_list)
  })
}

shinyApp(ui = dP,server = server)

