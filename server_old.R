library(tidyverse)
library(gt)
library(janitor)
library(shiny)
library(shinydashboard)
library(googlesheets4)
library(rmarkdown)
library(DT)
library(scales)
library(scico)
library(plyr)
library(gtExtras)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(shinythemes)
library(gtExtras)
library(shinyvalidate)
library(lubridate)
library(purrr)
library(ggtext)
library(extrafont)
library(reactable)
library(reactablefmtr)
library(janitor)
library(tidyr)
library(htmlTable)
library(htmltools)
library(fontawesome)
library(shinycssloaders)
library(shinyShortcut)
library(ggcharts)
library(htmltools)
library(svglite)
library(styler)


chs_chn <- readr::read_csv('https://raw.githubusercontent.com/DenimSimbhu/CHS_CHN/main/CHS_CHN2.csv')
chs_chn_ip <- readr::read_csv('https://raw.githubusercontent.com/DenimSimbhu/CHS_CHN/main/CHS_CHN_IP.csv')
sp_tkt <- readr::read_csv('https://raw.githubusercontent.com/DenimSimbhu/CHS_CHN/main/CHS_CHN.csv')
pr_percent <- readr::read_csv('https://raw.githubusercontent.com/DenimSimbhu/CHS_CHN/main/pr_percent.csv')

#chs_chn_ip <- read_csv("CHS_CHN_IP.csv")

chs_chn_ip <- chs_chn_ip %>%
  clean_names() %>%
  dplyr::select(
    Task = issue_key ,
    Summary = summary,
    Assignee = assignee,
    Sprint = sprint,
    Status = status
  ) %>%
  filter(Status != "4_Review") %>%
  dplyr::mutate(
    Status = case_when(
      Status == "1_Requirements" ~ "Yet to Start",
      Status == "3_In Progress" ~ "In Progress",
      Status == "4_Review" ~ "In Review",
      TRUE ~ "Completed"
    )
  )

pal1 = scico::scico(100, palette = 'nuuk')


##Sprint wise data transformation

sp_tkt_Tf <- sp_tkt %>%
  clean_names() %>%
  dplyr::select(
    Task = issue_key ,
    Summary = summary,
    Assignee = assignee,
    Actual_Start = `custom_field_chart_date_of_first_response`,
    Actual_Finish = updated,
    Sprint = sprint,
    Status = resolution
  ) %>%
  drop_na()





format_dmy_af  <- as.Date(sp_tkt_Tf$Actual_Finish, format = "%d-%m-%Y %M:%S")
format_dmy2_af <- as.Date(sp_tkt_Tf$Actual_Finish, format = "%d/%b/%y %H:%M %p")
format_dmy  <- as.Date(sp_tkt_Tf$Actual_Start, format = "%d-%m-%Y %M:%S")
format_dmy2 <- as.Date(sp_tkt_Tf$Actual_Start, format = "%m/%d/%Y %H:%M:%S")

##################

chs_chn_tf <- chs_chn %>%
  dplyr::select(
    "Ticket Summary" = Summary,
    Issue_ID = `Issue key`,
    Type = `Issue key`,
    Status,
    Priority,
    Assignee,
    Created,
    Updated
  ) %>%
  dplyr::mutate(Created =  ymd(as.Date(Created, format = "%d/%B/%Y %H:%M"))) %>%
  dplyr::mutate(Updated =  ymd(as.Date(Updated, format = "%d/%B/%Y %H:%M"))) %>%
  
  dplyr::mutate(Days = as.integer((Sys.Date() - Created))) %>%
  dplyr::mutate(
    Status = case_when(
      Status == "1_Requirements" ~ "Yet to Start",
      Status == "3_In Progress" ~ "In Progress",
      Status == "4_Review" ~ "In Review",
      TRUE ~ "Completed"
    )
  )



chs_chn_tfd <- chs_chn_tf %>%
  clean_names() %>%
  dplyr::select(ticket_summary,
                issue_id,
                status,
                assignee,
                priority,
                created,
                updated,
                days) %>%
  filter(status != "Completed")

chs_chn_tfc <- chs_chn_tf %>%
  clean_names() %>%
  dplyr::select(ticket_summary, issue_id, status, assignee, priority)


orange_pal <- function(x)
  rgb(colorRamp(c("#6DB9EF", "#F3B664"))(x), maxColorValue = 255)


ui <- navbarPage(
  "Denim Health CHS Dash",
  theme = shinythemes::shinytheme("flatly"),
  tabPanel("OverallSummary"
           , shinycssloaders::withSpinner(plotOutput("OverallPlot"))),
  
  
  tabPanel("Completed Tasks"
           , shinycssloaders::withSpinner(gt_output("CompletedReport"))),
  tabPanel(title = "Actual vs Target (Task Duration)", sidebarLayout(
    sidebarPanel(
      selectInput(
        "dataset1",
        label = "Resources",
        choices = unique(chs_chn_tf$Assignee),
        selected = "Himanshu Pant"
      )
    ), mainPanel(gt_output(outputId = "ResPR"))
  )),
  tabPanel("PR Report" , shinycssloaders::withSpinner(reactableOutput("PR1"))),
  tabPanel("On-Going Tasks"
           , shinycssloaders::withSpinner(gt_output(
             "OverallSummaryReport"
           ))),
  navbarMenu(
    "More",
    tabPanel(
      "MQ-Report",
      "Monthly / Quarterly Report Which Contains Data from Jan to Till Date as a set of different visuals across this page",
      value = 4,
      reactableOutput("People"),
      height = px(1600),
      width = px(1600)
    ),
    tabPanel(
      "Download Data",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "dataset",
            "Choose a dataset:",
            choices = c("PR_Details", "PR_DrillDown")
          ),
          radioButtons("filetype", "File type:", choices = c("csv", "tsv")),
          downloadButton('downloadData', 'Download')
        ),
        mainPanel(tableOutput('table'))
      ),
      value = 5,
      gt_output(outputId = "Download"),
      height = px(1600),
      width = px(1600)
    )
  )
)



server <- function(input, output, session) {
  datasetname <- reactiveVal(unique(chs_chn_tf$Assignee)[1])
  
  observeEvent(input$dataset1, {
    updateSelectInput(session, "dataset1", selected = input$dataset1)
    datasetname(input$dataset1)
  })
  
  
  output$OverallPlot <-   renderPlot({
    chs_chn_tf %>%
      mutate(Status = as.factor(Status)) %>%
      dplyr::summarize(prop = n() / nrow(chs_chn_tf), .by = Status) |>
      dplyr::mutate(Status = forcats::fct_reorder(Status, prop)) |>
      dplyr::mutate(prop = as.double(format(prop * 100, digits = 2))) |>
      mutate(label = sprintf("%1.1f%%", prop)) %>%
      bar_chart(Status, prop) +
      #  geom_text(aes(label = label, hjust = -0.1), size = 5)+
      geom_label(aes(label = label, hjust = 1.2)) +
      labs(
        x = NULL,
        y = "% Completed",
        title = "Overall Status of the Sprint - Chennai ",
        caption = "Source: Denim Confluence"
      )
    
    
  })
  
  
  output$OverallSummaryReport <-   render_gt({
    #Added march 7th for filtering only status with open
    chs_chn_ip %>%
      group_by(Assignee) %>%
      gt() %>%
      tab_stubhead("label") %>%
      gt_theme_538() %>%
      data_color(
        # Update cell colors...
        columns = c('Assignee'),
        # ...for supp column!
        colors = scales::col_factor(
          # <- bc it's a factor
          palette = c("#9AC8CD", "#1679AB"),
          # Two factor levels, two colors
          domain = c("Divyum", "Sandeep Rao")# Levels
        )
      ) %>%
      tab_style(
        style = cell_text(color = "black", weight = "bold"),
        locations = list(cells_row_groups(), cells_column_labels(everything()))
      ) %>%
      tab_style(style = cell_fill(color = "bisque"),
                locations = cells_row_groups(groups = 1)) |>
      tab_style(
        style = cell_fill(color = "lightgreen"),
        locations = cells_row_groups(groups = "Himanshu Pant")
      ) %>%
      tab_style(
        style = cell_fill(color = "lightgreen"),
        locations = cells_row_groups(groups = "Sandeep Rao")
      )
    
    
  })
  
  
  
  output$CompletedReport <-   render_gt({
    #Added march 7th for filtering only status with open
    tb1 <- chs_chn_tfc %>%
      dplyr::filter(status == "Completed") %>%
      gt() %>%
      gt_theme_nytimes() %>%
      data_color(
        # Update cell colors...
        columns = c('assignee'),
        # ...for supp column!
        colors = scales::col_factor(
          # <- bc it's a factor
          palette = c("#9AC8CD", "#1679AB"),
          # Two factor levels, two colors
          domain = c("Divyum", "Sandeep Rao")# Levels
        )
      )
    
    
  })
  
  
  ###ACtar
  
  output$AcTar <-   render_gt({
    #Added march 7th for filtering only status with open
    sp_tkt_Tf %>%
      mutate(Actual_Start = as.Date(ifelse(
        is.na(format_dmy2), format_dmy, format_dmy2
      ), origin = "1970-01-01")) %>%
      mutate(Actual_Finish = as.Date(ifelse(
        is.na(format_dmy2_af), format_dmy_af, format_dmy2_af
      ), origin = "1970-01-01")) %>%
      mutate(Duration = as.numeric(Actual_Finish - Actual_Start),
             Target = 10) %>%
      #dplyr::mutate(Total_Days = Total_Days/max(Total_Days) * 100)%>%
      mutate(Total_Days = Duration) %>%
      dplyr::select(Task, Assignee, Duration, Status, Total_Days, Target) %>%
      #group_by(Assignee)%>%
      #  gt() %>%
      gt(groupname_col = "Assignee") %>%
      gt_plt_bullet(
        column = Total_Days,
        target = Target,
        width = 45,
        # color = "#ffb200", FF004D = "#00008B")%>%
        palette = c("#2D3250", "#FF004D")
      ) %>%
      gt_theme_nytimes() %>%
      #  gt(groupname_col = "Assignee")%>%
      cols_label(
        Total_Days = html(
          "<span style='color:#2D3250;'>Actual</span> vs <span style='color:#FF004D;'><b>Target</b></span>"
        )
      ) %>%
      tab_header(
        title = "Total Duration of the stories completed in the sprint",
        subtitle = md(
          "<span style='color:#FF004D;'><b>Sprint Target</b></span> which is 10 days "
        )
      )
    
  })
  
  output$ResPR <-  render_gt({
    #Added march 7th for filtering only status with open
    sp_tkt_Tf %>%
      
      mutate(Actual_Start = as.Date(Actual_Start, format = "%d-%m-%Y %M:%S")) %>%
      mutate(Actual_Finish = as.Date(Actual_Finish, format = "%d-%m-%Y %M:%S")) %>%
      
      # mutate(Actual_Start = as.Date(ifelse(is.na(format_dmy2),
      #                                      format_dmy,format_dmy2), origin = "01-01-1970"))%>%
      # mutate(Actual_Finish = as.Date(ifelse(is.na(format_dmy2_af),
      #                                       format_dmy_af,format_dmy2_af), origin = "01-01-1970"))%>%
      mutate(Duration = as.numeric(Actual_Finish - Actual_Start),
             Target = 10) %>%
      #dplyr::mutate(Total_Days = Total_Days/max(Total_Days) * 100)%>%
      mutate(Total_Days = Duration) %>%
      dplyr::select(Task, Assignee, Duration, Status, Total_Days, Target) %>%
      filter(Assignee == input$dataset1) %>%
      #group_by(Assignee)%>%
      gt() %>%
      #gt(groupname_col = "Assignee")%>%
      gt_plt_bullet(
        column = Total_Days,
        target = Target,
        width = 45,
        # color = "#ffb200", FF004D = "#00008B")%>%
        palette = c("#2D3250", "#FF004D")
      ) %>%
      gt_theme_nytimes() %>%
      #  gt(groupname_col = "Assignee")%>%
      cols_label(
        Total_Days = html(
          "<span style='color:#2D3250;'>Actual</span> vs <span style='color:#FF004D;'><b>Target</b></span>"
        )
      ) %>%
      tab_header(
        title = "Total Duration of the stories completed in the sprint",
        subtitle = md(
          "<span style='color:#FF004D;'><b>Sprint Target</b></span> which is 10 days "
        )
      )
    
  })
  
  
  
  output$PR1 <-  renderReactable({
    pr_percent %>%
      dplyr::select(Assignee, Sprint, "# Stories Worked" = Tickets_Completed, everything()) %>%
      reactable(
        theme = fivethirtyeight(),
        columnGroups = list(colGroup(
          name = "PR Categories",
          columns = c("ReWork_PR", "Clean_PR")
        )),
        defaultColDef = colDef(maxWidth = 150, format = colFormat(digits = 0)),
        columns = list(
          "Assignee" = colDef(maxWidth = 110),
          "Success_Rate" = colDef(
            format = colFormat(
              digits = 2,
              suffix = " %",
              separators = TRUE
            ),
            style = color_scales(., colors = pal1)
          ),
          #"Donated to Pride"=colDef(maxWidth = 100, align="center", cell=pill_buttons(.,color_ref = "color_don", opacity = 0.8)),
          "# Stories Worked" = colDef(
            format = colFormat(
              digits = 0,
              suffix = " Stories",
              separators = TRUE
            ),
            style = list(borderRight = "1px solid #777")
          ),
          color_don = colDef(show = FALSE),
          color_hrc = colDef(show = FALSE),
          # "Clean_PR"=colDef(align="center", cell=icon_assign(.,icon="circle",fill_color = "black")),
          "Clean_PR" = colDef(cell = icon_assign(
            ., icon = "circle-check", fill_color = "#8B322C"
          )),
          "ReWork_PR" = colDef(align = "center", cell = data_bars(., force_outside = c(0, 30)))
        )
      ) %>%
      google_font("Roboto", font_weight = 300) %>%
      add_title(title = "Peer Review Report", font_size = 20) %>%
      add_subtitle(
        subtitle = "Report shows the success rate of the PRs without re-work - Our Target is to achieve minimal or no Re-work",
        font_size = 14,
        font_weight = "normal",
        margin = reactablefmtr::margin(
          t = 10,
          r = 20,
          b = 15,
          l = 0
        )
      )
    
  })
  
}

shinyApp(ui = ui, server = server)
