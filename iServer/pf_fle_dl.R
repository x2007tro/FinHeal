# Reactive value for selected dataset ----
chosen_dataset <- reactive({
  switch(input$pf_fle_dl_dspick,
         "current month" = transdata_cm(),
         "ytd" = transdata_ytd(),
         "max" = transdata_full())
})

output$pf_fle_dl_dsfeat1 <- renderUI({
  tagList(
    dateInput("pf_fle_dl_dsfeat_begd", label = 'begin date', value = min(chosen_dataset()$transaction_date), width = entry_wid_m),
    dateInput("pf_fle_dl_dsfeat_endd", label = 'end date', value = max(chosen_dataset()$transaction_date), width = entry_wid_m)
  )
  
})

output$pf_fle_dl_dsfeat2 <- renderUI({
  tagList(
    numericInput("pf_fle_dl_dsfeat_ncol", label = 'no. of data fields', value = ncol(chosen_dataset()), width = entry_wid_m),
    numericInput("pf_fle_dl_dsfeat_nrow", label = 'no. of records', value = nrow(chosen_dataset()), width = entry_wid_m)
    
  )
  
})

output$pf_fle_dl_dsfeat3 <- renderUI({
  tagList(
    selectInput("pf_fle_dl_dsfeat_dfdesc", label = 'data fields description', choices = colnames(chosen_dataset()), 
                selected = colnames(chosen_dataset()), selectize = TRUE, multiple = TRUE, width = entry_wid_xl)
  )
  
})

# Table of selected dataset ----
output$pf_fle_dl_dssnip <- DT::renderDataTable({
  df <- head(chosen_dataset(), 10)
  DT::datatable(
    df,
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = TRUE
    )
  ) %>% 
    DT::formatRound('amount', digits = 0) %>% 
    DT::formatStyle(
      c("hyper_category"),
      #fontWeight = "bold",
      #color = "white",
      backgroundColor = DT::styleEqual(
        unique(df$hyper_category),
        brewed_colors[1:length(unique(df$hyper_category))]
      )
    )
})

# Downloadable csv of selected dataset ----
output$pf_fle_dl_dsdl <- downloadHandler(
  filename = function() {
    paste(input$pf_fle_dl_dspick, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(chosen_dataset(), file, row.names = FALSE)
  }
)