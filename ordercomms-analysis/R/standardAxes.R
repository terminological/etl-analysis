library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpubr)

plot_weekday_xaxis <- function(plot) {
  return(plot +
           geom_vline(xintercept = 0, colour='grey')+
           geom_vline(xintercept = 24, colour='grey')+
           geom_vline(xintercept = 48, colour='grey')+
           geom_vline(xintercept = 72, colour='grey')+
           geom_vline(xintercept = 96, colour='grey')+
           geom_vline(xintercept = 120, colour='grey')+
           geom_vline(xintercept = 144, colour='grey')+
           geom_vline(xintercept = 168, colour='grey')+
           xlab("hours from midnight sunday") +
           scale_x_continuous(breaks = seq(0, 24*7, 12))
  );
}

plot_day_xaxis <- function(plot) {
  return(plot +
    xlab("hours from midnight") +
    scale_x_continuous(breaks = seq(0, 24, 1))
  );
}

plot_year_xaxis <- function(plot) {
  return(plot +
           xlab("month of year")+
           scale_x_discrete(breaks = 
                c('01-01','02-01','03-01','04-01','05-01','06-01','07-01','08-01','09-01','10-01','11-01','12-01'),
                   labels = c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))
  );
}

plot_year_xaxis_2 <- function(plot) {
  return(plot +
           xlab("month of year")+
           scale_x_discrete(breaks = 
              c('01','02','03','04','05','06','07','08','09','10','11','12'),
                            labels = c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))
  );
}

theme_Publication <- function(base_size=14, base_family="helvetica") {
	library(grid)
	library(ggthemes)
	(theme_foundation(base_size=base_size, base_family=base_family)
				+ theme(plot.title = element_text(face = "bold",
								size = rel(1.2), hjust = 0.5),
						text = element_text(),
						panel.background = element_rect(colour = NA),
						plot.background = element_rect(colour = NA),
						panel.border = element_rect(colour = NA),
						axis.title = element_text(face = "bold",size = rel(1)),
						axis.title.y = element_text(angle=90,vjust =2),
						axis.title.x = element_text(vjust = -0.2),
						axis.text = element_text(), 
						axis.line = element_line(colour="black"),
						axis.ticks = element_line(),
						panel.grid.major = element_line(colour="#f0f0f0"),
						panel.grid.minor = element_blank(),
						legend.key = element_rect(colour = NA),
						legend.position = "bottom",
						legend.direction = "horizontal",
						legend.key.size= unit(0.2, "cm"),
						legend.margin = unit(0, "cm"),
						legend.title = element_text(face="italic"),
						plot.margin=unit(c(10,5,5,5),"mm"),
						strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
						strip.text = element_text(face="bold")
				))
	
}

scale_fill_Publication <- function(...){
	library(scales)
	discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
	
}

scale_colour_Publication <- function(...){
	library(scales)
	discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
	
}