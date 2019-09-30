for(cat in 1:5){
  categoria <- names(main_dict)[cat]
  cat_tot <- get_category_total(categoria,person)
  text(-1.5,y_coords_category[cat],
       categoria)
  text(x_coord_category,y_coords_category[cat],
       cat_tot$total_category,cex=2)
}

person <- 122
categoria <- names(main_dict)[5]
cat_tot <- get_category_total(categoria,person)

person <- 123
categoria <- 'Entorno organizacional'
get_category_total(categoria,person)


x <- c(1:9, 8:1)
y <- c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9)
op <- par(mfcol = c(3, 1))
for(xpd in c(FALSE, TRUE, NA)) {
  plot(1:10, main = paste("xpd =", xpd))
  box("figure", col = "pink", lwd = 3)
  polygon(x, y, xpd = xpd, col = "orange", lty = 2, lwd = 2, border = "red")
}
par(op)


color <- threshold_colors[which(threshold_labels==cat_tot$evaluation)]
polygon(c(-.65,-.65,-.35,-.35,3,3,4,4), c(0.2,1.8,1.8,0.2,NA, 1,2,1),
   density = c(10, 35), angle = c(90, 45), col=color, lwd=c(2,3), lty=c(2,4))




person <- person
categoria <- names(main_dict)[5]
cat_tot <- get_category_total(categoria,person)
text(-1.5,y_coords_category[cat],
     categoria)
text(x_coord_category,y_coords_category[cat],
     cat_tot$total_category,cex=2)