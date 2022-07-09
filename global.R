##
# Global parameter
##
theme_set(theme_light())
options(scipen = 66666)

entry_wid_ty <- "50px"
entry_wid_s <- "105px"
entry_wid_m <- "180px"
entry_wid_l <- "275px"
entry_wid_xl <- "500px"
plot_font_color <- "#808080"
brewed_colors <- rep(RColorBrewer::brewer.pal(n = 12, name = "Set3"), 100)
get_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, name = 'Pastel1'))

#
# connection for database
#
db_obj <- list(
  srv = "192.168.2.200",
  prt = 3307,
  dbn = "FinHeal",
  id = "dspeast2",
  pwd = "yuheng"
)

tasks <- list(100)
