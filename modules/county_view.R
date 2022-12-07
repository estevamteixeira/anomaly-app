# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
# import("plotly")
import("DT")
import("rintrojs")
import("shiny")
import("shinydashboard")
import("utils")

# Define which objects from the module you make available to a user.
# All other objects are kept private, local, to the module.
export("ui")
export("init_server")

# Use and/or register a module as dependency.
# 'use' is similar to 'import' but instead of importing from packages,
# we import from a module.
consts <- use("constants/constants.R")

# It is a variation of 'use' where instead of returning a module 
# as return value, the elements are copied to the calling environment.
expose("utilities/getDataByTimeRange.R")


ui <- function(id) {
  # This ns <- NS() structure creates a 
  # "namespacing" function, that will 
  # prefix all ids with a string
  ns <- NS(id)
  introBox(data.step = 5, data.intro = consts$intro$text[5],
  box(
    title = "Top Surveillance Counties",
    status = "primary",
    collapsible = FALSE,
    solidHeader = FALSE,
    width = 12,
    # This looks the same as your usual piece of code, 
    # except that the id is wrapped into 
    # the ns() function we defined before
    DT::DTOutput(ns("countytable"))
  ))
}


init_server <- function(id, df, y1, y2, q){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # county_data is a reactive expression whose results will depend on
    # the periods (initial, final years), and condition selected
    county_data <- reactive({
      if(is.null(q()) || q() == "0"){
        merge(unique(
          getCountyData(df,
                        y1(),
                        y2())[, c("CD_UID", "total_cases")
                              ]),
          unique(getSubsetByTimeRange(consts$cd_birth,
                                      y1(),
                                      y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                                         c("CD_UID", "cd.count_dlv")
                                         ][,
                                           `:=` (total_lvb = sum(cd.count_dlv,
                                                                 na.rm = TRUE)),
                                           by = c("CD_UID")
                                           ]),
          by = c("CD_UID"))[,
                           `:=` (rate = 1000*total_cases/total_lvb),
                           by = c("CD_UID")
              ][
                order(-rate)
              ][,
                `:=` (total_cases = ifelse(total_cases < 5,
                                           "< 5",
                                           as.character(scales::comma(total_cases,
                                                                      accuracy = 1))),
                      total_lvb = ifelse(total_lvb < 5,
                                         "< 5",
                                         as.character(scales::comma(total_lvb,
                                                                    accuracy = 1))))
              ] %>%
          merge(consts$cd_names, by = c("CD_UID")) %>%
          .[, .(CD_UID, cd_full, total_cases, total_lvb, rate)] %>%
          .[order(-rate)] %>% 
          unique()
      } else{
        merge(unique(
          getCountyDataByCase(df, y1(), y2(), q())[,
                                                   c("CD_UID", "cat_tier2", "total_cases")
                                                   ]),
              unique(getSubsetByTimeRange(consts$cd_birth,
                                          y1(),
                                          y2())[tolower(dlv) %in% c("lvb", "stillbirth"),
                                                c("CD_UID", "cd.count_dlv")
                                          ][,
                                            `:=` (total_lvb = sum(cd.count_dlv,
                                                                  na.rm = TRUE)),
                                            by = c("CD_UID")
                                          ]),
              by = c("CD_UID"))[,
                               `:=` (rate = 1000*total_cases/total_lvb),
                               by = c("CD_UID")
              ][
                order(-rate)
              ][,
             `:=` (total_cases = ifelse(total_cases < 5,
                                        "< 5",
                                        as.character(scales::comma(total_cases, accuracy = 1))),
                   total_lvb = ifelse(total_lvb < 5,
                                      "< 5",
                                      as.character(scales::comma(total_lvb, accuracy = 1))))
           ] %>%
          merge(consts$cd_names, by = c("CD_UID")) %>%
          .[,c("CD_UID", "cd_full", "total_cases", "total_lvb", "rate")] %>%
          .[order(-rate)] %>% 
          unique()
      }
      })
    
    # This allows to add footer with totals
    # Here we need to make use of the isolate() function
    # Othw, this object should be put inside the DT::renderDT() call
    
    sketch <- htmltools::withTags(table(
      tableHeader(c("County ID", "County", "Total cases",
                    "Total total birth", "Prevalence <br> (* cases per 1,000 total births)"),
                  escape = FALSE),
      # tableFooter(
      #   c("", "Grand Total",
      #     format(
      #       c(sum(unique(consts$cd_stats[,.(BrthYear, CD_UID, cd.count_anom, cat_tier2)])$cd.count_anom),
      #         sum(unique(consts$cd_stats[,.(BrthYear, total_lvb, CD_UID)])$total_lvb),
      #         round(1000*sum(unique(consts$cd_stats[,.(BrthYear, CD_UID, cd.count_anom, cat_tier2)])$cd.count_anom)/sum(unique(consts$cd_stats[,.(BrthYear, total_lvb, CD_UID)])$total_lvb),
      #               0)
      #       ),
      #       big.mark = ","
      #     )
      #   )
      # )
    ))
    
    # DataTable object
    output$countytable <- DT::renderDT({
      
      
      validate(need(nrow(county_data()) > 0 ||
                      !all(is.na(county_data()$total_cases)),
            "Sorry, there is no data available for the selected options.
            \nPlease, choose different years and/or conditions."))
      
      DT::datatable(
        county_data(),
        container = sketch,
        rownames = FALSE,
        style = "bootstrap",
        selection = 'single',
        caption = "Select a county to examine it in more detail",
        # width = "10px",
        extensions = "Buttons",
        options = list(
          dom = 'B<t>ftp',
          extensions = "Buttons",
          search = list(regex = TRUE, caseInsensitive = TRUE),
          paging = TRUE,
          pageLength = 6,
          ordering = TRUE,
          stateSave = TRUE,
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          columnDefs = list(list(visible = FALSE,
                                 targets = c(0)))
          # language = list(zeroRecords = "")
        )
      ) |> 
        formatRound(
          columns = c("rate"),
          digits = 1,
          mark = ","
        )
    }, server = FALSE)
    
    # DataTable proxy object
    DTproxy <- DT::dataTableProxy("countytable")
    
    return(list(cd_selected = reactive({county_data()[input$countytable_rows_selected,]})))
    
  })
 }
