##
# tab
output$bz_rpt_exp4buz_opt <- renderUI({
  
  cats <- unique(exp4buz_tax_rpt_actnshow()$category)
  
  tagList(
    
    fluidRow(
      column(
        12,
        tags$h4(class = 'block_title', paste0("For time period ", input$bz_ipt_par_begdt, " to ", input$bz_ipt_par_enddt))
      )
    ),
    
    fluidRow(
      
      lapply(1:length(cats), function(i){
        itms_per_cat <- exp4buz_tax_rpt_actnshow() %>% dplyr::filter(category == cats[i])
        col_width <- itms_per_cat$column_width[1]
        
        column(
          col_width,
          
          fluidRow(
            column(
              12,
              
              tags$div(
                class = 'block_inner_frame',
                
                tags$h4(class = 'block_title', cats[i]),
                
                lapply(1:nrow(itms_per_cat), function(j){
                  tags$div(
                    class = "bz_rpt_exp4buz_opt_div",
                    
                    if(itms_per_cat$further_processing[j] == 0){
                      textInput(paste0("bz_rpt_exp4buz_opt_",itms_per_cat$id[j]), label = itms_per_cat$name[j], value = itms_per_cat$value[j], width = entry_wid_m)
                    } else {
                      # process value
                      my_sql <- itms_per_cat$value[j]
                      my_sql <- gsub("%beg_date%", paste0(input$bz_ipt_par_begdt, ' 00:00:00'), my_sql)
                      my_sql <- gsub("%end_date%", paste0(input$bz_ipt_par_enddt, ' 23:59:59'), my_sql)
                      
                      amt <- GetQueryResFromSS(db_obj, my_sql)
                      sum_amt <- sum(amt$amount)
                      
                      textInput(paste0("bz_rpt_exp4buz_opt_",itms_per_cat$id[j]), label = itms_per_cat$name[j], value = scales::comma(sum_amt, accuracy = 2), width = entry_wid_m)
                    }
                    
                  )
                })
                
              )
            )
          )
          
        )
      })
      
    )
  )
  
  
})