# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
# import("plotly")
import("data.table")
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
# consts <- use("constants/constants.R")
intro <- readr::read_csv("data/intro.csv")

# It is a variation of 'use' where instead of returning a module 
# as return value, the elements are copied to the calling environment.
expose("utilities/getDataByTimeRange.R")


ui <- function(id) {
  # This ns <- NS() structure creates a 
  # "namespacing" function, that will 
  # prefix all ids with a string
  ns <- NS(id)
  introBox(data.step = 5, data.intro = intro$text[5],
  box(
    title = "Surveillance Table",
    status = "primary",
    collapsible = FALSE,
    solidHeader = FALSE,
    width = 12,
    # This looks the same as your usual piece of code, 
    # except that the id is wrapped into 
    # the ns() function we defined before
    DT::DTOutput(ns("geotable"))
  ))
}


init_server <- function(id, df1, df2, y1, y2, q, lim){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # geo_data is a reactive expression whose results will depend on
    # the periods (initial, final years), and condition selected
    geo_data <- reactive({
      dta <- buildGeoDataByCase(df1 = df1,
                         df2 = df2,
                         y1 = y1(),
                         y2 = y2(),
                         q = q(),
                         geo = lim())[,
                                      `:=` (total_cases = ifelse(
                                        total_cases < 5,
                                        "< 5",
                                        as.character(
                                          scales::comma(
                                            total_cases,
                                            accuracy = 1))),
                                        total_lvb_geo = ifelse(
                                          total_lvb_geo < 5,
                                          "< 5",
                                          as.character(
                                            scales::comma(
                                              total_lvb_geo,
                                              accuracy = 1)))
                                      )]
      # For urban rural the logic is different
      # because we are using the CSD's to build the maps
      if(tolower(lim()) %in% "urb"){
        dta <- unique(dta, by = c("area"))[,
                CSDuid := as.numeric(
                  factor(area,
                         levels = c("Urban","Rural")))
                ]
      }
      
      setnames(dta,
               old = colnames(dta)[1:2],
               new = c("GeoUID", "Name"))
      
      return(dta)
    })
    
    # This allows to add footer with totals
    # Here we need to make use of the isolate() function
    # Othw, this object should be put inside the DT::renderDT() call
    
    sketch <- htmltools::withTags(table(
      tableHeader(c("Geography ID", "Name", "Total cases",
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
    output$geotable <- DT::renderDT({
      
      
      validate(need(nrow(geo_data()) > 0 ||
                      !all(is.na(geo_data()$total_cases)),
            "Sorry, there is no data available for the selected options.
            \nPlease, choose different years and/or conditions."))
      
      DT::datatable(
        geo_data(),
        container = sketch,
        rownames = FALSE,
        style = "bootstrap",
        selection = 'single',
        caption = paste("Select a",
                        ifelse(tolower(lim()) %in% "csd","municipality",
                         ifelse(tolower(lim()) %in% "cd","county",
                          ifelse(tolower(lim()) %in% "clus","community cluster",
                           ifelse(tolower(lim()) %in% "zn","management zone",
                            ifelse(tolower(lim()) %in% "urb","urban-rural area",
                                   "community health network"))))),"to examine it in more detail"),
        # width = "10px",
        extensions = "Buttons",
        options = list(
          dom = 'B<t>ftp',
          extensions = "Buttons",
          search = list(regex = TRUE, caseInsensitive = TRUE),
          paging = TRUE,
          pageLength = 5,
          width = c("30px","100px","30px","30px","100px"),
          ordering = TRUE,
          stateSave = TRUE,
          # buttons = list('copy', 'print', list(
          #   extend = 'collection',
          #   buttons = list(
          #     list(extend = 'csv', filename = "geo_view")
          #      # list(extend = 'excel', filename = "trend_results"),
          #      # list(extend = 'pdf', filename = "trend_results")
          #   ),
          #   text = 'Download'
          # )),
          columnDefs = list(list(visible = FALSE,
                                 targets = c(0,3)))
          # language = list(zeroRecords = "")
        )
      ) |> 
        DT::formatRound(
          columns = c("rate"),
          digits = 2,
          mark = ","
        )
    }, server = FALSE)
    
    # DataTable proxy object
    DTproxy <- DT::dataTableProxy("geotable")
    
    return(
      list(geo_selected = reactive({
        
        geo_data()[input$geotable_rows_selected,]
      
        })
      ))
    
    
    
  })
 }
