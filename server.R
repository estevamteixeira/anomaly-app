# Use 'import' from the 'modules' package.
# These listed imports are made available inside the module scope.
import("rintrojs")
import("shiny")

# 'use' is similar to 'import' but instead of importing from packages,
# we import from a module.
consts <- use("constants/constants.R")


function(input, output, session){
  
  ## Show intro modal
  
  observeEvent("", {
    showModal(
      modalDialog(
        includeHTML("intro_text.html"),
        easyClose = TRUE,
        footer = tagList(
          actionButton(inputId = "intro",
                       label = "Introduction Tour",
                       icon = icon("info-circle"))
        )
      ))
  })
  
  ## Remove modal when clicking the button
  
  # observeEvent(input$intro,{
  #   removeModal()
  # })
  # 
  # show intro tour
  observeEvent(input$intro,{
    ## Remove modal when clicking the button
    removeModal()
    ## Start intro tour
    introjs(session,
            options = list("nextLabel" = "Continue",
                           "prevLabel" = "Previous",
                           "doneLabel" = "Alright. Let's go",
                           "skipLabel" = "Skip",
                           showStepNumbers = TRUE))
  })
  
  # show intro tour when pressing the introduction button
  # on the sidebar
  observeEvent(input$intro_btn,
               introjs(session,
                       options = list("nextLabel" = "Continue",
                                      "prevLabel" = "Previous",
                                      "doneLabel" = "Alright. Let's go",
                                      "skipLabel" = "Skip",
                                      showStepNumbers = TRUE))
  )
  
  
  
  # Using a "server-side selectize" option that massively improves 
  # performance and efficiency (for large numbers of choices)
  updateSelectInput(
    session,
    inputId = "icd10",
    choices = consts$icd10_opts,
    selected = c("0")
  )
  
  ## Update the options for the initial year selectInput
  ## The values should reflect the data availability
  observeEvent(input$icd10,{
    
    if (!input$icd10 %in% 0 &
        !is.na(stringr::str_extract(input$icd10, pattern = "\\(.*\\)"))){
      updateSelectInput(
        session,
        inputId = "init_time",
        choices = c(sort(unique(consts$cd_anom$BrthYear[
          consts$cd_anom$cat_tier4 %in% input$icd10]))),
        selected = c(min(consts$cd_anom$BrthYear[
          consts$cd_anom$cat_tier4 %in% input$icd10]))
      )} else if (!input$icd10 %in% 0 &
                  is.na(stringr::str_extract(input$icd10, pattern = "\\(.*\\)"))){
        updateSelectInput(
          session,
          inputId = "init_time",
          choices = c(sort(unique(consts$cd_anom$BrthYear[
            consts$cd_anom$cat_tier3 %in% input$icd10]))),
          selected = c(min(consts$cd_anom$BrthYear[
            consts$cd_anom$cat_tier3 %in% input$icd10]))
        )} else {
          updateSelectInput(
            session,
            inputId = "init_time",
            choices = c(sort(unique(consts$cd_anom$BrthYear))),
            selected = c(min(consts$cd_anom$BrthYear))
          )
        }
  })
  
  ## Make the final year option greater than or equal the
  ## initial year option
  observeEvent(c(input$icd10, input$init_time),{
    
    if (!input$icd10 %in% 0 &
        !is.na(stringr::str_extract(input$icd10, pattern = "\\(.*\\)"))){
      updateSelectInput(
        session,
        inputId = "end_time",
        choices = c(sort(unique(consts$cd_anom$BrthYear[
          consts$cd_anom$cat_tier4 %in% input$icd10])))[
            c(sort(unique(consts$cd_anom$BrthYear[
              consts$cd_anom$cat_tier4 %in% input$icd10]))) >= input$init_time],
        selected = c(max(consts$cd_anom$BrthYear[
          consts$cd_anom$cat_tier4 %in% input$icd10]))
      )
    } else if (!input$icd10 %in% 0 &
               is.na(stringr::str_extract(input$icd10, pattern = "\\(.*\\)"))){
      updateSelectInput(
        session,
        inputId = "end_time",
        choices = c(sort(unique(consts$cd_anom$BrthYear[
          consts$cd_anom$cat_tier3 %in% input$icd10])))[
            c(sort(unique(consts$cd_anom$BrthYear[
              consts$cd_anom$cat_tier3 %in% input$icd10]))) >= input$init_time],
        selected = c(max(consts$cd_anom$BrthYear[
          consts$cd_anom$cat_tier3 %in% input$icd10]))
      )} else {
        updateSelectInput(
          session,
          inputId = "end_time",
          choices = c(sort(unique(consts$cd_anom$BrthYear)))[
            c(sort(unique(consts$cd_anom$BrthYear))) >= input$init_time],
          selected = c(max(consts$cd_anom$BrthYear))
        )
      }
    
  })
  
  initial_year <- reactive({ input$init_time})
  final_year <- reactive({ input$end_time})
  condition <- reactive({ input$icd10})
  geography <- reactive({ input$geo})
  
  session$userData$table_view <- table_view$init_server(
    "table_advanced_view",
    df1 = consts$cd_anom,
    df2 = consts$cd_birth,
    y1 = initial_year,
    y2 = final_year,
    q = condition,
    lim = geography
  )
  
  # global_metrics_view$init_server("global_metrics_advanced_view")
  
  # local_metrics_view$init_server("local_metrics_advanced_view")
  
  session$userData$local_metrics_view <- local_metrics_view$init_server(
    "local_metrics_advanced_view",
    df1 = consts$cd_anom,
    df2 = consts$cd_birth,
    y1 = initial_year,
    y2 = final_year,
    q = condition,
    lim = geography
  )
  
  session$userData$map_view <- map_view$init_server(
    "map_advanced_view",
    df1 = consts$cd_anom,
    df2 = consts$cd_birth,
    y1 = initial_year,
    y2 = final_year,
    q = condition,
    lim = geography
  )
  
  session$userData$line_view <- line_view$init_server(
    "line_advanced_view",
    df1 = consts$cd_anom,
    df2 = consts$cd_birth,
    y1 = initial_year,
    y2 = final_year,
    q = condition,
    lim = geography
  )
  
  session$userData$upset_view <- upset_view$init_server(
    "bar_advanced_view",
    df1 = consts$cd_anom,
    y1 = initial_year,
    y2 = final_year,
    q = condition,
    lim = geography
  )
  
}