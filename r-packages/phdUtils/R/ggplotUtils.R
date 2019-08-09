
#' A standard X axis with hour from a sunday
#'
#' @keywords axes
#' @import ggplot2
#' @import cowplot
#' @import ggpubr
#' @export
#' @examples
#' ggplot()+weekXaxis()
weekXaxis <- function() {
  return(geom_vline(xintercept = 0, colour='grey')+
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

#' A standard X axis with hour from midnight
#'
#' @keywords axes
#' @import ggplot2
#' @import cowplot
#' @import ggpubr
#' @export
#' @examples
#' ggplot()+dayXaxis()
dayXaxis <- function() {
  return(
    xlab("hours from midnight") +
    scale_x_continuous(breaks = seq(0, 24, 1))
  );
}

#' A standard X axis of months of year where data is a 'MM-DD' formatted date object
#'
#' @keywords axes
#' @import ggplot2
#' @import ggpubr
#' @export
#' @examples
#' ggplot()+dayXaxis()
monthsXaxis <- function() {
  return(
    xlab("month of year")+
    scale_x_discrete(breaks =
        c('01-01','02-01','03-01','04-01','05-01','06-01','07-01','08-01','09-01','10-01','11-01','12-01'),
        labels = c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))
  );
}

#' A standard X axis of months of year where data is a 'MM' formatted date object
#'
#' @keywords axes
#' @import ggplot2
#' @import cowplot
#' @import ggpubr
#' @export
#' @examples
#' ggplot()+dayXaxis()
monthsXaxis2 <- function(plot) {
  return(plot +
           xlab("month of year")+
           scale_x_discrete(breaks =
                c('01','02','03','04','05','06','07','08','09','10','11','12'),
                labels = c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))
  );
}

# theme(axis.text.x = element_text(angle=90,size=9))+

# remove text labels
# +rremove("x.text")+rremove("ylab")+rremove("xlab")
# remove guides
# guides(fill=FALSE, size=FALSE)+

#' A standard publication shaped plotting
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param print_aspect_ratio svg aspect ratio
#' @param ... passed to cowplot
#' @keywords axes
#' @import ggplot2
#' @import cowplot
#' @import ggpubr
#' @export
#' @examples
#' savePubSmall("the_filename",ggplot())
savePubSmall = function(filename, plot = last_plot(), print_aspect_ratio=1.1,...) {
  cowplot::save_plot(paste0(filename,".svg"), plot+
          cowplot::theme_cowplot(font_size = 8)+
              theme(title = element_blank(), legend.position = "bottom", legend.direction = "horizontal"),
              base_height = NULL,base_width = 89,base_aspect_ratio = print_aspect_ratio, unit="mm",...
  )
  cowplot::save_plot(paste0(filename,".png"), plot+theme_cowplot(font_size = 12), base_width = 5, base_height = 5, dpi=1500/5,...)
}

#' A standard publication shaped plotting
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param print_aspect_ratio svg aspect ratio
#' @param ... passed to cowplot
#' @keywords axes
#' @import ggplot2
#' @import ggpubr
#' @export
#' @examples
#' savePubBig("the_filename",ggplot())
savePubBig = function(filename, plot = last_plot(), print_aspect_ratio=1.1,...) {
  cowplot::save_plot(paste0(filename,".svg"), plot+
              cowplot::theme_cowplot(font_size = 8)+
              theme(title = element_blank(), legend.position = "bottom", legend.direction = "horizontal"),
            base_height = NULL,base_width = 183, base_aspect_ratio = print_aspect_ratio,unit="mm",...
  )
  cowplot::save_plot(paste0(filename,".png"), plot+theme_cowplot(font_size = 12), base_width = 10, base_height = 5, dpi=1500/5,...)
}

# print sizes - width between 8.3 cms to 17.35 cms - BMJ
# nature: 89mm (single) 183mm (double) and max depth 247mm


#' A standard plot for publication
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param maxWidth maximum width in inches
#' @param maxHeight maximum height in inches
#' @param aspectRatio defaults to maxWidth/maxHeight
#' @keywords plot
#' @import ggplot2
#' @export
#' @examples
#' saveThesis("the_filename",maxWidth=4,maxHeight=4,plot=ggplot())
saveThesis <- function(filename,maxWidth,maxHeight,plot = last_plot(),aspectRatio=NULL) {
  if (is.null(aspectRatio)) aspectRatio=maxWidth/maxHeight;
  ggplot2::ggsave(normalizePath(paste0(filename,".pdf"),mustWork = FALSE), plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio));
  ggplot2::ggsave(normalizePath(paste0(filename,".png"),mustWork = FALSE), plot, width = min(maxWidth,maxHeight*aspectRatio), height = min(maxHeight,maxWidth/aspectRatio), dpi=300);
  embedFonts(normalizePath(paste0(filename,".pdf"),mustWork = FALSE));
}

#' A standard 6x8 plot size for a full page
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param aspectRatio defaults to maxWidth/maxHeight
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' saveThesisFullPage("the_filename",ggplot())
saveThesisFullPage = function(filename,plot = last_plot(), aspectRatio=NULL) {
  saveThesis(filename, plot=plot, maxWidth=5.9, maxHeight=8, aspectRatio=aspectRatio)
}

#' A standard max 6x4 plot size for a half page
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param aspectRatio defaults to maxWidth/maxHeight
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' saveThesisFullPage("the_filename",ggplot())
saveThesisHalfPage = function(filename,plot = last_plot(), aspectRatio=NULL) {
  saveThesis(filename, plot=plot,maxWidth=5.9, maxHeight=4, aspectRatio=aspectRatio)
}

#' A standard max 6x3 plot size for a third page
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param aspectRatio defaults to maxWidth/maxHeight
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' saveThesisFullPage("the_filename",ggplot())
saveThesisThirdPage = function(filename,plot = last_plot(), aspectRatio=NULL) {
  saveThesis(filename, plot=plot,maxWidth=5.9, maxHeight=3, aspectRatio=aspectRatio)
}

#' A standard max 3x3 plot size for a page
#'
#' @param filename base of target filename (excuding extension).
#' @param plot a GGplot object or none
#' @param aspectRatio defaults to maxWidth/maxHeight
#' @keywords axes
#' @import ggplot2
#' @export
#' @examples
#' saveThesisSixthPage("the_filename",ggplot())
saveThesisSixthPage = function(filename,plot = last_plot(), aspectRatio=NULL) {
  saveThesis(filename, plot=plot,maxWidth=3, maxHeight=3, aspectRatio=aspectRatio)
}


# aspect ratios: screen 16/9

# print sizes - width between 8.3 cms to 17.35 cms - BMJ
# nature: 89mm (single) 183mm (double) and max depth 247mm

# sudo apt install ttf-mscorefonts-installer
# sudo apt-get install ttf2ufm
# cd /usr/share/fonts/truetype/msttcorefonts
# sudo ttf2ufm Arial*.ttf
# sudo cp Arial.* ../../type1/
# sudo fc-cache -f -v
# install.packages("extrafont")
# library(extrafont)
# font_import()
# loadfonts()

#' A phd ggplot2 theme
#' 
#' @keywords plot theme
#' @import ggplot2
#' @import extrafont
#' @param base_size default 10
#' @param base_family default "Arial"
#' @export
#' @examples
#' theme_set(themePhd())
themePhd <- function(base_size=10, base_family="Arial") {
  if(!("package:extrafont") %in% search()) library(extrafont)
  return(
  theme_bw(base_size, base_family)+
    theme(
      plot.title=element_text(size=base_size,hjust=0.5),
      axis.title=element_text(size=base_size),
      axis.text=element_text(size=base_size),
      axis.text.x=element_text(angle=30, hjust=1)
    )
  )}

#' reorganise plot to minimise width (or maximise plotting width)
#' 
#' @keywords plot theme
#' @import ggplot2
#' @export
#' @examples
#' theme_set(themePhd())
narrowAndTall <- function() {
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  );
}
