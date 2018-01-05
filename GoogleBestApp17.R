library(rvest)
library(plyr)
library(alluvial)
library(ggplot2)
library(plotrix)
library(treemap)
library(plotly)
library(xml2)
library(qdap)

###########  CREATE Function for Google Screen Scrape ################
scrapeGoogleReviews <- function(url, categoryName ){
  
  #Specifying the url for desired website to be scrapped
  
  webpage <- read_html(url)
  
  df <-data.frame(
    app_title = html_text(html_nodes(webpage,'.id-app-title')),
    rating_count = html_text(html_nodes(webpage,'.rating-count')),
    download_count = html_text(html_nodes(webpage,'.download-count')),
    content_rating = html_text(html_nodes(webpage,'.content-rating-title')),
    write_up = html_text(html_nodes(webpage,'.editorial-snippet')),
    category = categoryName)
  df
  return(df)
  
}

###########  CALL Function for Screen Scrape ################

df1 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_bestof2017&hl=en','Winner')
df2 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_entertaining_bestof2017&hl=en','Most Entertaining')
df3 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_social_bestof2017&hl=en','Best Social')
df4 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_productivity_bestof2017&hl=en','Daily Helper')
df5 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_innovative_bestof2017&hl=en','Most Innovative')
df6 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_Hiddengem_bestof2017&hl=en','Hidden Gem')
df7 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_apps_kids_bestof2017&hl=en','Best for Kids')
df8 <-scrapeGoogleReviews('https://play.google.com/store/apps/topic?id=campaign_editorial_3002de9_apps_bestof_mostpopular2017&hl=en','Most Popular')

#Combine all of the data frames
fulldf <- rbind(df1,df2, df3, df4, df5, df6, df7, df8)

#Peek at the data frame

head(fulldf)


###########  Extra formatting ################

# Remove commas and convert to numeric
fulldf$rating_count_numeric <- as.numeric(gsub("[^0-9]", "", fulldf$rating_count))

fulldf$download_count_numeric <- as.numeric(gsub("[^0-9]", "", fulldf$download_count))
attach(fulldf)

# Add percent downloads

totalDownload <- sum(download_count_numeric)

fulldf$percentDownloadApp <- round(download_count_numeric/totalDownload *100, 2)

attach(fulldf)

#Binning by downloads

breaks <- c(0,10000,1000000,10000000, 100000000)

fulldf$download_total_ranking = findInterval(download_count_numeric,breaks)

#Binning by rating totals

breaks2 <- c(10,100,1000,100000, 10000000)

fulldf$rating_total_ranking = findInterval(rating_count_numeric,breaks2)

attach(fulldf)

#peek at the data

head(fulldf)

###########  Visualize the Data ################

plot_ly(fulldf, labels =fulldf$app_title, values =ifelse(fulldf$percentDownloadApp>1,fulldf$percentDownloadApp,''), type = 'pie') %>%
  
  layout(title = 'Percentage of App Downloads by App',
         
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Treemap without category

treemap(fulldf, #Your data frame object
        
        index=c("app_title" ), 
        
        vSize = "download_count_numeric",
        
        type="index",
        
        palette = "Blues",
        
        title="Treemap of App Download Volumes",
        
        fontsize.title = 14
        
)

# Bar Plot of downloads by app

g <- ggplot(fulldf, aes(app_title, download_count_numeric))

g + geom_bar(stat="identity", width = 0.5, fill="tomato2") +
  
  labs(title="Bar Chart",
       
       subtitle="Applications",
       
       caption="Downloads of apps") +
  
  theme(axis.text.x = element_text(angle=65, vjust=0.6))