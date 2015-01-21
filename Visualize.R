
#Index

#################################################
#Vis

# - rMaps
# - rCharts
# - ggvis
# - googlevis
# - Slidify
# -
# -
# -
# -
# -


#http://www.r-bloggers.com/interactive-visualizations-with-r-a-minireview/
#http://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html
#http://ggvis.rstudio.com/interactivity.html


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
