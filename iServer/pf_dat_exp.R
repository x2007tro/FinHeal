##
# render data from DB
output$pf_dat_exp_cm_trans <- DT::renderDataTable({
  df <- transdata_cm()
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

output$pf_dat_exp_ytd_trans <- DT::renderDataTable({
  df <- transdata_ytd()
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

output$pf_dat_exp_max_trans <- DT::renderDataTable({
  df <- transdata_full()
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