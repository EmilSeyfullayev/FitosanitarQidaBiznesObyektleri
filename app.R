library(googledrive)
library(googlesheets4) # I am using the developing version 0.1.0.9000
library(shiny); library(tidyverse); library(DT)

# You want to deploy an app in Shinyapps.io or other server
# FIRST STEP----
# Get the token an store it in a cache folder embedded in your app directory
# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
# options(gargle_quiet = FALSE) # So you can know what is happening
# Authenticate in interactive mode (run the app) ONCE and check if the token 
# has been stored inside the .secrets folder, after that just comment this line
# drive_auth() # Authenticate to produce the token in the cache folder
# Grant permission to googlesheets to access to the token produced
# sheets_auth(token = drive_token())

# SECOND STEP----
# Comment lines 10, 13 and 15 and uncomment lines 21 and 22
# You tell gargle to search the token in the secrets folder and to look
# for an auth given to a certain email (enter your email linked to googledrive!)
drive_auth(cache = ".secrets", email = "aqti.gov.az@gmail.com")
sheets_auth(token = drive_token())

# THIRD STEP---
# Now you can deploy your app in shinyapps.io!!
# Test if your app runs properly in the local version
# Authenticate in ShinyApps.io
# rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
# setwd() in your App directory
# library(rsconnect)
# deployApp()
# Enjoy your new App!!

df1 <- read_sheet("https://docs.google.com/spreadsheets/d/1vhy4x3O3-0Ew7k_8xw8nM9aEa6UwW963SpBY9W6oB1g/edit#gid=0")

df2 <- read_sheet("https://docs.google.com/spreadsheets/d/1vhy4x3O3-0Ew7k_8xw8nM9aEa6UwW963SpBY9W6oB1g/edit#gid=0", 
                  sheet = "mehsuldarliq_ixrac_hecmi")
labels <- read_sheet("https://docs.google.com/spreadsheets/d/1vhy4x3O3-0Ew7k_8xw8nM9aEa6UwW963SpBY9W6oB1g/edit#gid=0", 
                     sheet = "labels")

df_proq <- read_sheet("https://docs.google.com/spreadsheets/d/1vhy4x3O3-0Ew7k_8xw8nM9aEa6UwW963SpBY9W6oB1g/edit#gid=0", 
                      sheet = "proqnoz")

#df1[' '] <- paste0("<button>", names(labels)[6], "</button>")

esl_adlar_df1 <- c(names(df1), " ")
esl_adlar_df2 <- names(df2)

english_adlar <- c(
  "regional_bolme", 
  "inzibati_erazi_vahidi", 
  "subyektin_adi", 
  "teskilati_huquqi_forma",
  "huquqi_unvan", 
  "voen",
  "elaqe_nomresi",
  "E_poct",
  "obyektin_qeydiyyat_nomresi", 
  "fealiyyet_sahesi",
  "obyektin_yerlesdiyi_unvan", 
  "obyektin_umumi_sahesi",
  "mehsulun_novu" ,
  "secilmis_mehsulun_becerildiyi_sahe"
)

names(df1) <- english_adlar

english_adlar_2 <- c(
  "subyektin_adi",
  "obyektin_unvani",
  "mehsulun_novu",
  "mehsulun_sortlari",
  "obyektin_umumi_sahesi",
  "secilmis_mehsulun_becerildiyi_sahe",
  "mehsuldarliq_ilini_secin",
  "mehsuldarliq_ayini_secin",
  "istehsal_hecmi",
  "ixrac_hecmi"
)

names(df2) <- english_adlar_2

names(df_proq) -> esl_adlar_df_proq

english_adlar_df_proq <- c(
  "subyektin_adi",
  "obyektin_unvani",
  "mehsulun_novu",
  "obyektin_umumi_sahesi",
  "secilmis_mehsulun_becerildiyi_sahe",
  "ili_secin",
  "mehsulun_istehsal_hecmi",
  "ixrac_hecmi"
)

names(df_proq) <- english_adlar_df_proq

ui <- fluidPage(
  fluidPage(
    fluidRow(
      column(12, br())
    )
  ),
  fluidPage(
    fluidRow(
      column(
        12, align = 'right', h4(labels[[1]][3],
                                style="color:#0C0FDF")
      )
    )
  ),
  fluidRow(hr(),br(),column(12, align="center", titlePanel(title = names(labels)[4]))),
  fluidRow(column(12, align="center", titlePanel(title = names(labels)[5]))),
  fluidPage(fluidRow(column(12, br(), br()))),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      ########################################### region filteri-----
      
      selectInput('regional_bolme', esl_adlar_df1[1], choices = as.character(unique(df1[[names(df1)[1]]])), 
                  selected = as.character(unique(df1[[names(df1)[1]]])[1])),
      
      ########################################## erazi vahid filteri-----
      hr(),
      htmlOutput('second'),
      ######################################### feal_sahesi-----
      hr(),  
      selectInput(
        'fealiyyet_sahesi', esl_adlar_df1[10], 
        selected=unique(df1[[names(df1)[10]]]), multiple = T, 
        choices = unique(df1[[names(df1)[10]]])
      ),
      ################################# 
      hr(),  
      selectInput(
        'teskilati_huquqi_forma', esl_adlar_df1[4], 
        selected=unique(df1[[names(df1)[4]]]), multiple = T, 
        choices = unique(df1[[names(df1)[4]]])
      ),
      ####################################### mehsulun_novu----
      hr(),    
      selectInput(
        "mehsulun_novu", esl_adlar_df2[3], 
        choices = unique(df2$mehsulun_novu), selected = unique(df2$mehsulun_novu)[1]
      ),
      ###################################### indicators------
      
      br(),
      dataTableOutput("subyekt_sayi"),
      hr(),
      br(),
      dataTableOutput("obyekt_sayi"),
      hr(),
      br(),
      dataTableOutput("mehsulun_umumi_sahesi")
    ),
    
    mainPanel(
      width = 10,
      tabsetPanel(
        type='tab',
        tabPanel(names(labels)[2], br(), br(), dataTableOutput("main_table")),
        tabPanel(
          names(labels)[3], 
          br(),
          fluidPage(
            fluidRow(
              column(3),
              column(
                3,
                align = 'center',
                selectInput(
                  "mehsuldarliq_ilini_secin", 
                  shiny::HTML(paste0("<p><span style='color: grey'>", esl_adlar_df2[7], "</span></p>")),
                  #esl_adlar_df2[7], 
                  choices = unique(df2$mehsuldarliq_ilini_secin),
                  selected = min(df2$mehsuldarliq_ilini_secin)
                )
              ),
              column(
                4,
                align = 'center',
                selectInput(
                  "mehsuldarliq_ayini_secin", 
                  shiny::HTML(paste0("<p><span style='color: grey'>", esl_adlar_df2[8], "</span></p>")),
                  # esl_adlar_df2[8], 
                  choices = unique(df2$mehsuldarliq_ayini_secin),
                  selected = unique(df2$mehsuldarliq_ayini_secin)[1], multiple = T, width = "335px"
                )
              ),
              column(2)
            )
          ),
          
          hr(), br(),
          dataTableOutput("secondary_table")),
        tabPanel(
          "Proqnoz göstəricilər",
          fluidPage(fluidRow(column(12, br(), br()))),
          fluidRow(
            column(
              3,
              selectInput(
                "ili_secin", 
                esl_adlar_df_proq[6], 
                choices = unique(df_proq$ili_secin)
              )
            )
          ),
          br(), 
          br(), 
          dataTableOutput("proqnoz")
        )
      ),
      fluidPage(
        fluidRow(
          column(12, br(), br())
        )
      )
    )
  )
)



server <- function(input, output, session) {
  
  output$second <- renderUI({
    available <- unique(df1[df1$regional_bolme %in% input$regional_bolme, english_adlar[2]] %>% .[[1]])
    selectInput('inzibati_erazi_vahidi', esl_adlar_df1[2], choices = available, selected=available, multiple = T
    )
  })
  
  
  ################# secondary table ----
  
  output$secondary_table <- renderDataTable({
    
    df1 %>% filter(
      regional_bolme %in% input$regional_bolme,
      inzibati_erazi_vahidi %in% input$inzibati_erazi_vahidi,
      fealiyyet_sahesi %in% input$fealiyyet_sahesi,
      teskilati_huquqi_forma %in% input$teskilati_huquqi_forma
    ) -> df1_next
    
    df2 %>% filter(
      subyektin_adi %in% df1_next$subyektin_adi,
      obyektin_unvani %in% df1_next$obyektin_yerlesdiyi_unvan,
      mehsulun_novu %in% input$mehsulun_novu,
      mehsuldarliq_ilini_secin %in% input$mehsuldarliq_ilini_secin,
      mehsuldarliq_ilini_secin %in% input$mehsuldarliq_ilini_secin
    ) -> df2_next
    
    # df2_next$yigimin_baslama_tarixi %>% format("%d.%m.%Y") -> df2_next$yigimin_baslama_tarixi
    # df2_next$yigimin_bitme_tarixi %>% format("%d.%m.%Y") -> df2_next$yigimin_bitme_tarixi
    # 
    df2_next
    
    names(df2_next) <- esl_adlar_df2
    
    
    df2_next %>% select(-c(3,4,7,8))
    
    
  }, selection='single', escape=F, 
  
  class = 'cell-border stripe',
  options = list(#dom= "tpr",
    lengthChange = FALSE, 
    language = list(zeroRecords=names(labels)[12],
                    search = "Axtar",
                    paginate = 
                      list('next'="Növbəti", 
                           'previous'="Əvvəlki",
                           'All' = "Axtar")),
    pageLength = 10, 
    columnDefs = list(
      list(
        className = 'dt-center', 
        targets = '_all'
      ), 
      list(
        width="150px", targets = c(3,4,5,6)
      )
    )
  )) 
  ############################## proqnoz ----
  output$proqnoz <- renderDataTable({
    
    df1 %>% filter(
      regional_bolme %in% input$regional_bolme,
      inzibati_erazi_vahidi %in% input$inzibati_erazi_vahidi,
      fealiyyet_sahesi %in% input$fealiyyet_sahesi,
      teskilati_huquqi_forma %in% input$teskilati_huquqi_forma
    ) -> df1_next
    
    df_proq %>% filter(
      subyektin_adi %in% df1_next$subyektin_adi,
      obyektin_unvani %in% df1_next$obyektin_yerlesdiyi_unvan,
      mehsulun_novu %in% input$mehsulun_novu,
      ili_secin %in% input$ili_secin
    ) -> df2_next
    
    names(df2_next) <- esl_adlar_df_proq
    
    df2_next %>% select(-c(3, 6))
    
    
    
  }, selection='single', escape=F, 
  
  class = 'cell-border stripe',
  options = list(#dom= "tpr",
    lengthChange = FALSE, 
    language = list(zeroRecords=names(labels)[12],
                    search = "Axtar",
                    paginate = 
                      list('next'="Növbəti", 
                           'previous'="Əvvəlki",
                           'All' = "Axtar")),
    pageLength = 10, columnDefs = list(
      list(
        className = 'dt-center', 
        targets = '_all'
      ), 
      list(
        width="150px", targets = c(3,4,5,6)
      )
    )
    
  ))
  
  
  ########################### indicator 1 subyekt sayi ----
  
  output$subyekt_sayi <- DT::renderDataTable({
    
    df1 %>% filter(
      regional_bolme %in% input$regional_bolme,
      inzibati_erazi_vahidi %in% input$inzibati_erazi_vahidi,
      fealiyyet_sahesi %in% input$fealiyyet_sahesi,
      mehsulun_novu %in% input$mehsulun_novu,
      teskilati_huquqi_forma %in% input$teskilati_huquqi_forma
    ) -> df1_next
    
    unique(df1_next$subyektin_adi) %>% length() -> subyekt_sayi
    
    as.data.frame(subyekt_sayi) -> subyekt_sayi_table
    
    names(subyekt_sayi_table) <- names(labels)[9]
    
    subyekt_sayi_table
    
  }, selection='single', escape=F, rownames=F,
  class = 'cell-border stripe',
  options = list(dom= "t",
                 language = list(zeroRecords=names(labels)[12]),
                 columnDefs = list(list(className = 'dt-center', targets = '_all'))
  ))
  
  ################# 2 nd indicator obyekt_sayi ----
  
  output$obyekt_sayi <- DT::renderDataTable({
    
    df1 %>% filter(
      regional_bolme %in% input$regional_bolme,
      inzibati_erazi_vahidi %in% input$inzibati_erazi_vahidi,
      fealiyyet_sahesi %in% input$fealiyyet_sahesi,
      mehsulun_novu %in% input$mehsulun_novu,
      teskilati_huquqi_forma %in% input$teskilati_huquqi_forma
    ) -> df1_next
    
    unique(df1_next$obyektin_yerlesdiyi_unvan) %>% length() -> obyekt_sayi
    
    as.data.frame(obyekt_sayi) -> obyekt_sayi_table
    
    names(obyekt_sayi_table) <- names(labels)[10]
    
    obyekt_sayi_table
    
  }, selection='single', escape=F, rownames=F,
  class = 'cell-border stripe',
  options = list(dom= "t",
                 language = list(zeroRecords=names(labels)[12]),
                 columnDefs = list(list(className = 'dt-center', targets = '_all'))
  ))
  
  ################################ indicator_3 mehsulun_umumi_sahesi -----
  
  output$mehsulun_umumi_sahesi <- DT::renderDataTable({
    
    df1 %>% filter(
      regional_bolme %in% input$regional_bolme,
      inzibati_erazi_vahidi %in% input$inzibati_erazi_vahidi,
      fealiyyet_sahesi %in% input$fealiyyet_sahesi,
      mehsulun_novu %in% input$mehsulun_novu,
      teskilati_huquqi_forma %in% input$teskilati_huquqi_forma
    ) -> df1_next
    
    df2 %>% filter(
      subyektin_adi %in% df1_next$subyektin_adi,
      obyektin_unvani %in% df1_next$obyektin_yerlesdiyi_unvan,
      mehsulun_novu %in% input$mehsulun_novu
    ) -> df2_next
    
    sum(df2_next$secilmis_mehsulun_becerildiyi_sahe) -> secilmis_mehsulun_sahesi
    
    as.data.frame(secilmis_mehsulun_sahesi) -> secilmis_mehsulun_sahesi_table
    
    names(secilmis_mehsulun_sahesi_table) <- names(labels)[8]
    
    secilmis_mehsulun_sahesi_table
    
  }, selection='single', escape=F, rownames=F, class = 'cell-border stripe',
  options = list(dom= "t",
                 language = list(zeroRecords=names(labels)[12]),
                 columnDefs = list(list(className = 'dt-center', targets = '_all'))
  ))
  
  ########################## action_button_function ----
  
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  df1[' ']  = shinyInput(actionButton, 
                         nrow(df1) , 
                         'button_', 
                         label = names(labels)[6], 
                         onclick = 'Shiny.setInputValue(\"select_button\",  this.id, {priority:\"event\"})')
  
  
  #################################################### main_table_in actions ----
  
  output$main_table <- renderDataTable({
    
    df1 %>% filter(
      regional_bolme %in% input$regional_bolme,
      inzibati_erazi_vahidi %in% input$inzibati_erazi_vahidi,
      fealiyyet_sahesi %in% input$fealiyyet_sahesi,
      mehsulun_novu %in% input$mehsulun_novu,
      teskilati_huquqi_forma %in% input$teskilati_huquqi_forma
    ) %>% select(-c(1,2, 10, 13)) -> df1_next
    
    names(df1_next) <- esl_adlar_df1[c(3:9, 11:12, 14:15)]
    
    df1_next
  }, selection='single', escape=F,
  # filter = list(position='top',
  #               placeholder = "Axtar"),
  class = 'cell-border stripe',
  options = list(dom= "tpr",
                 lengthChange = FALSE,
                 language = list(zeroRecords=names(labels)[12],
                                 search = "Axtar",
                                 paginate =
                                   list('next'="Növbəti",
                                        'previous'="Əvvəlki",
                                        'All' = "Axtar")),
                 pageLength = 10,
                 columnDefs = list(
                   list(
                     className = 'dt-center',
                     targets = '_all'
                   ),
                   list(
                     width="10px", targets = c(9,10)
                   ),
                   list(
                     width="50px", targets = c(11)
                   ),
                   list(
                     width="20px", targets = c(2)
                   ),
                   list(
                     width="40px", targets = c(7)
                   ),
                   list(
                     width="190px", targets = c(8)
                   )
                 )
                 
  ))
  
  
  ######################## acilan pencere esas-----
  
  observeEvent(input$select_button, {
    
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    showModal(modalDialog(
      title = names(labels)[7],
      footer = tagList(modalButton(names(labels)[11])),
      DT::renderDataTable({
        
        obyektin_yerlesdiyi_unvan = df1[selectedRow, 11][[1]][1]
        df2[df2$obyektin_unvani == obyektin_yerlesdiyi_unvan, c(3, 4, 6)]-> df_next_modal
        
        names(df_next_modal) <-  c(esl_adlar_df2[c(3, 4)], 
                                   labels[[1]][2])
        
        df_next_modal
        
        
      },
      selection='single', escape=F, class = ' cell-border stripe',
      options = list(
        language= list(zeroRecords=names(labels)[12]), dom = "t", 
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(
            width="20px", targets = c(3))
          
          
        )
      ))
      
    ))
  })
}


shinyApp(ui, server)


