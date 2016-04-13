# Visualisation

## ggplot basic

### Left align two plots

require(gridExtra)
gA <- ggplotGrob(A)
gB <- ggplotGrob(B)
gtable <- rbind(gA,gB,size="last")
id <- gtable$layout$t[gtable$layout$name == "panel"]
gtable$heights[id] <- lapply(c(10,5), "unit", "null")

grid::grid.draw(gtable)

## Plot sparklines

## mapping basic

## bar basic

## point basic

## mixed
