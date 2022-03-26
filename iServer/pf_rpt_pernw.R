# render net worth page

output$pr_rpt_pernw_ast_prop <- renderUI({
  tagList(
    tags$h5("Properties")
  )
})

output$pr_rpt_pernw_ast_land <- renderUI({
  tagList(
    tags$h5("Land")
  )
})

output$pr_rpt_pernw_ast_oth <- renderUI({
  tagList(
    tags$h5("Other")
  )
})