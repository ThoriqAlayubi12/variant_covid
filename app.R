library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(readxl)
library(DT)
library(writexl)
library(plotly)
library(tidyverse)


datvat <- read_excel("data WGS 23 juli 2023.xlsx",sheet = "datvarian")
datvat$minggu <- week(ymd(datvat$`Collection date`))
datvat$bulan <- format(as.Date(datvat$`Collection date`,format="%Y-%m-%d"),"%m")
datvat$tahun <- format(as.Date(datvat$`Collection date`,format="%Y-%m-%d"),"%Y")
datvat$bulan <- as.numeric(datvat$bulan)
datvat$tahun <- as.numeric(datvat$tahun)
datvat$kode <- ifelse(datvat$Gender=="Female",yes = 1,no=1)

datvat$grup_lineage <- ifelse(str_detect(datvat$Lineage, "BA.2.75"), "BA.2.75", 
                           ifelse(str_detect(datvat$Lineage, "BA.4"), "BA.4", 
                                  ifelse(str_detect(datvat$Lineage, "BA.5"), "BA.5",
                                         ifelse(str_detect(datvat$Lineage, "BA.1"), "BA.1",
                                                ifelse(str_detect(datvat$Lineage, "BA.2"), "BA.2", 
                                                       ifelse(str_detect(datvat$Lineage, "BQ.1"), "BQ.1", 
                                                              ifelse(str_detect(datvat$Lineage, "BF.7"), "BF.7",
                                                                     ifelse(str_detect(datvat$Lineage, "XBB.1.5"), "XBB.1.5", 
                                                                            ifelse(str_detect(datvat$Lineage, "XBB.1.9"), "XBB.1.9", 
                                                                                   ifelse(str_detect(datvat$Lineage, "XBB.1.16"), "XBB.1.16",
                                                                                          ifelse(str_detect(datvat$Lineage, "XBB.1.22"), "XBB.1.22",
                                                                                                 ifelse(str_detect(datvat$Lineage, "EG.2"), "EG.2",
                                                                                                        ifelse(str_detect(datvat$Lineage, "EG.5.1"), "EG.5.1",
                                                                                          ifelse(str_detect(datvat$Lineage, "BA.3"), "BA.3", "lainnya"))))))))))))))

datvat$grup_lineage <- as.factor(datvat$grup_lineage)

data_singgah <- datvat %>% group_by(tahun,minggu,grup_lineage) %>%
  summarize("jumlah Spesimen" = sum(kode))
data_singgah$waktu <- paste0("M",data_singgah$minggu,"_",data_singgah$tahun)
tambahan <- datvat %>% group_by(tahun,minggu) %>%
  summarize("penyebut" = sum(kode))
tambahan$waktu <- paste0("M",tambahan$minggu,"_",tambahan$tahun)
data2 <- merge(x=data_singgah,y=tambahan,by='waktu')
data2 <- select(data2,c("tahun.x","minggu.x","waktu","grup_lineage","jumlah Spesimen","penyebut"))
data2$persentase <- data2$`jumlah Spesimen`/data2$penyebut*100
data2$minggu <- data2$minggu.x
data2$tahun <- data2$tahun.x
data2 <- select(data2,c("tahun","minggu","waktu","grup_lineage","jumlah Spesimen","penyebut","persentase"))
untuk_public <- select(datvat,c("Location","Gender","Patient age","minggu","bulan",
                                "tahun","grup_lineage"))

untuk_tabel1 <- datvat %>% group_by(tahun,grup_lineage) %>%
  summarize("Total_Spesimen" = sum(kode))
untuk_tabel2 <- datvat %>% group_by(tahun,bulan,minggu,grup_lineage) %>%
  summarize("Total_Spesimen" = sum(kode))

fungsitabel1 <- function(data, kriteria) {
  header <- c("Tahun Pemeriksaan" = "tahun", "lineage" = "grup_lineage","Jumlah Spesimen" = "Total_Spesimen")
  tbl_oi <- data %>%
    filter(tahun == kriteria) %>%
    rename(!!header)
    return(tbl_oi)
  }

fungsitabel2 <- function(data,kriteria,variant) {
  header <- c("Tahun Pemeriksaan" = "tahun","Bulan" = "bulan","Minggu" = "minggu","Lineage"="grup_lineage","Jumlah Spesimen"="Total_Spesimen")
  jadi <- data %>%
    filter(grup_lineage == variant & between(bulan,kriteria[1],kriteria[2])) %>%
    rename(!!header)
  return(jadi)
}

ui <- bootstrapPage(
  navbarPage(theme = shinytheme("united"),collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">DASHBOARD VARIANT COVID-19 DI INDONESIA</a>'),id="nav",
             windowTitle = "Dashboard Trend Variant Covid-19",
             tabPanel(
               "TREND VARIANT COVID-19",tags$div(
                 tags$h1("Trend Variant Covid-19 Nasional Mulai Juli 2022"),
               ),
               tags$br(),
               sidebarPanel(
                 selectInput(
                   inputId = "AA",
                   label = "Pilih Tahun pemeriksaan",
                   choices = unique(data2$tahun),
                   selected = 2022
                 ),
                 tags$br(),tags$br(),
                 h5("Built with",
                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                    "by",
                    img(src = "https://logos-download.com/wp-content/uploads/2020/06/RStudio_Logo.png", height = "30px"),
                    ".")
               ),
               mainPanel(
                 plotlyOutput(
                   outputId = "areaa"
                 ),
                 tags$br(),
                 tags$br(),
                 plotlyOutput(outputId = "absolut"),
                 tags$br(),
                 "NB: Hasil variant didasarkan pada Variant of Concern (VoC) dan Variant of interest (VoI) yang dirilis WHO dalam weekly situation Report",
                 tags$br(),
                 tags$br(),
                 plotlyOutput(outputId = "graph")
               )
             ),
             tabPanel("TABEL REKAP",
                      fluidRow(
                        column(5,
                               wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 775px;",
                                         fluidRow(style = "margin-top: 25px;",
                                                  column(12, 
                                                         p(tags$b("Jumlah Spesimen Kumulatif", style = "font-size: 150%; font-family:Helvetica; color:#4c4c4c; text-align:left;")))),
                                         hr(),
                                         fluidRow(column(4,
                                                         selectInput(
                                                           inputId = "kumuu",
                                                           label = "Pilih Tahun",
                                                           choices = sort(unique(untuk_tabel1$tahun), decreasing = T),
                                                           selected = 2022))),
                                                  DTOutput("tabelkumuvar")
                                                         )),
                        column(7,
                               wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 775px;",
                                         fluidRow(style = "margin-top: 25px; margin-bottom: -10px;",
                                                  column(5, 
                                                         p(tags$b("Kumulatif Berdasarkan Bulan", style = "font-size: 150%; font-family:Helvetica; color:#4c4c4c; text-align:left;"))),
                                                  column(2, style = "margin-top: 7px;", align = "right", p("dari Bulan:")),
                                                  column(2, align = "left",
                                                         selectInput(inputId = "tabelwaktu_awal",
                                                                     label = NULL,
                                                                     choices = sort(unique(untuk_tabel2$bulan)), 
                                                                     selected = 1)),
                                                  column(1, style = "margin-top: 7px;", align = "center", p("sampai")),
                                                  column(2, align = "left",
                                                         selectInput(inputId = "tabelwaktu_akhir",
                                                                     label = NULL,
                                                                     choices = sort(unique(untuk_tabel2$bulan)), 
                                                                     selected = 12))),
                                         hr(),
                                         fluidRow(column(5,
                                                         selectizeInput(inputId = "pil_var", 
                                                                        label = NULL,
                                                                        choices = sort(unique(untuk_tabel2$grup_lineage)), 
                                                                        selected = "BA.1",
                                                                        width = "85%"))),
                                         br(),
                                         DTOutput("lb_historical_table"))))),
             tabPanel("DOWNLOAD DATA",
                      tags$div(
                        tags$h4()
                      ),
                      sidebarPanel(selectInput("dataset",
                                               label = "Pilih Data Yang ingin di-download",
                                               choices = c("linelist WGS (for public)")),
                                   radioButtons("formatdownload","tipe file yang diinginkan",
                                                choices = c("csv" = ".csv",
                                                            "Excel" = ".xlsx",
                                                            "tsv" = ".tsv"),
                                                inline = TRUE),
                                   downloadButton("unduh","Download"),
                                   tags$br(),tags$br(),
                                   h5("Built with",
                                      img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                      "by",
                                      img(src = "https://logos-download.com/wp-content/uploads/2020/06/RStudio_Logo.png", height = "30px"),
                                      ".")),
                      mainPanel(
                        tableOutput("tabelunduh")
                      )
             ),
             tabPanel("TENTANG DASHBOARD",
                      tags$div(
                        tags$br(),tags$br(),tags$h4("Tentang Dahshboard"),
                        "Dashboard menampilkan grafik 100% stacked area chart hasil proporsi pemeriksaan WGS variant covid-19",
                        tags$br(), tags$br(), tags$h4("Tentang Data"),
                        "Data dalam dashboard ini diperoleh dari laman ",
                        tags$a(href="https://www.epicov.org/epi3/frontend","GISAID"),
                        tags$br(),tags$br(),tags$h4("Pengembang"),
                        tags$a(href="https://www.linkedin.com/in/moch-thoriq-assegaf-al-ayubi-320966201/","Moch. Thoriq Assegaf Al-Ayubi, SKM (Surveillance Data Assistant WHO Indonesia 2021-2023)"),
                        tags$br(),tags$br(),
                        h5("Built with",
                           img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                           "by",
                           img(src = "https://logos-download.com/wp-content/uploads/2020/06/RStudio_Logo.png", height = "30px"),
                           ".")),
                      )
             )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "linelist WGS" = untuk_public
    )
  })
  output$unduh <- downloadHandler(
    filename = function() {
      paste0(input$dataset,"_unduh",input$formatdownload)
    },
    content = function(fname) {
      if (input$formatdownload == ".csv"){
        write.csv(datasetInput(),fname,row.names = FALSE)
      } else if (input$formatdownload == ".xlsx") {
        write_xlsx(datasetInput(),fname)
      } else if (input$formatdownload == ".tsv") {
        write.table(datasetInput(),fname, quote = FALSE,sep = '\t',row.names = FALSE)
      }
    }
  )
  tbl_lb_yearly <- reactive({
    fungsitabel1(untuk_tabel1,input$kumuu)
    })
  output$tabelkumuvar <- renderDT({
    datatable(tbl_lb_yearly())
    })
  tbl_lb_historical <- reactive({
    fungsitabel2(untuk_tabel2,
                 c(as.numeric(input$tabelwaktu_awal),as.numeric(input$tabelwaktu_akhir)),
                 input$pil_var)
                 })
  output$lb_historical_table <- renderDT({
    datatable(tbl_lb_historical(),
              options = list(info = F,paging = F,searching = T,
                             stripeClasses = F,lengthChange = F,
                             orderMulti = T,
                             scrollY = "445px",scrollCollapse = T))})
  
  output$graph <- renderPlotly({
    
    filtered2 <- filter(data2, data2$tahun == input$AA)
    filtered2 <- filtered2 %>% group_by(minggu) %>% summarize("Total Spesimen" = sum(`jumlah Spesimen`))
    
    ggplotly(ggplot(filtered2,aes(x=minggu,y=`Total Spesimen`))+
               geom_bar(stat = "identity",fill="#FF9999")+
               labs(title=paste("Trend Spesimen yang diperiksa Tahun ",input$AA,sep = "")) +
               labs(x="Minggu",y="Jumlah Spesimen") +
               theme_minimal()
    )
  })
  output$areaa <- renderPlotly({
    filtered <- filter(data2, data2$tahun == input$AA)

    ggplotly(ggplot(filtered,aes(x=minggu,y=persentase,fill=grup_lineage))+
               geom_bar(stat = "identity")+
               labs(title=paste("Trend Mingguan Proporsi Variant tahun ",input$AA,sep = "")) +
               labs(x="Minggu",y="Proporsi Variant") +
               theme_minimal()
             )
  })
  output$absolut <- renderPlotly({
    filtered3 <- filter(data2, data2$tahun == input$AA)
    
    ggplotly(ggplot(filtered3,aes(x=minggu,y=`jumlah Spesimen`,fill=grup_lineage))+
               geom_bar(stat = "identity")+
               labs(title=paste("Trend Mingguan Variant Tahun ",input$AA,sep = "")) +
               labs(x="Minggu",y="Jumlah Spesimen") +
               theme_minimal()
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)