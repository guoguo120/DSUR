
#Index

#################################################
#Vis

# - rMaps
# - rCharts
# - ggvis
# - googlevis
# - Slidify
# - Sparkline
# - Sparklable
# -
# -
# -
# -


#http://www.r-bloggers.com/interactive-visualizations-with-r-a-minireview/
#http://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html
#http://ggvis.rstudio.com/interactivity.html
#http://cran.r-project.org/web/packages/sparkTable/index.html
#http://rstudio.github.io/leaflet/



#################################################

setwd('\\Users\\vanvlietben\\Dropbox\\20140101_FDMG\\R\\') #macbook
setwd('C:\\Users\\bvliet\\Desktop\\BU\\R\\') #fdmg desktop
setwd('G:\\Users\\BEN\\R\\') #fdmg fileserver
getwd()



#################################################
#








#ggvis

#http://www.r-bloggers.com/learn-ggvis-with-rstudio-and-datacamp/






qplot(data = churn_csv[GD_Leeftijd < 100], Mobiel_pageviews_Q1bin, GD_Leeftijd, facets = Churn ~ .) + geom_jitter()









#################################################
#Sparkline

install.packages("devtools")
devtools::install_github('ramnathv/htmlwidgets') #https://github.com/ramnathv/htmlwidgets
devtools::install_github('htmlwidgets/sparkline') #https://github.com/htmlwidgets/sparkline

library("htmlwidgets")
library("sparkline")

data <- mtcars
sparkline(data$mpg, type = 'bar', height = 400, width = 300)
sparkline(data$disp, type = 'box', height = 400, width = 400)
sparkline(data$qsec, type = 'bar')
sparkline(x, type = 'box')

#sparkline(var, type = 'box')
#sparkline(var, type = 'line')
#sparkline(var, type = 'bar')


