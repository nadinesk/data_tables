library(httr)
library(dplyr)
library(reshape2)
library(jsonlite)
library(tidyverse)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(tidytext)

api_key <- Sys.getenv('api_key')

nyt_df <- function(term,begin_date,end_date, apiKey) {

  
  baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                    "&begin_date=",begin_date,"&end_date=",end_date,
                    "&facet_filter=true&api-key=",api_key, sep="")
  print(baseurl)
  initialQuery <- fromJSON(baseurl)
  
  maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 
  pages <- list()
  for(i in 0:maxPages){
    nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
    message("Retrieving page ", i)
    pages[[i+1]] <- nytSearch 
    Sys.sleep(1) 
  }
  
  allNYTSearch <- rbind_pages(pages)

  allNYTSearch$pubd <- as.Date(allNYTSearch$response.docs.pub_date)
  
  allNYTSearch$pubmo <- format(as.Date(allNYTSearch$response.docs.pub_date), "%m")

  nyt_snip_nd_mo <- allNYTSearch %>%
    select('response.docs._id', 'response.docs.type_of_material', 'response.docs.word_count', 'response.docs.headline.print_headline', 'response.docs.headline.main', 'response.docs.web_url', 'response.docs.news_desk', 'response.docs.snippet', 'pubmo')
   
  nyt_snip_nd_mo
}


artWC <- function(df, colName) {
  headlineText <- paste(df[colName], collapse =" ")
  
  docs <- Corpus(VectorSource(headlineText))

  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <- tm_map(docs, toSpace, "\\‘")
  docs <- tm_map(docs, toSpace, "\\’s")
  docs <- tm_map(docs, toSpace, "\\”")
  docs <- tm_map(docs, toSpace, "\\“")

  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeWords, c("‘", "briefing", "dealbook")) 
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}


artSA <- function(df, colName, term) {
  
  headlineText <- paste(df[colName], collapse =" ")
  
  docs <- Corpus(VectorSource(headlineText))
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <- tm_map(docs, toSpace, "\\‘")
  docs <- tm_map(docs, toSpace, "\\’s")
  docs <- tm_map(docs, toSpace, "\\”")
  docs <- tm_map(docs, toSpace, "\\“")
  
 
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeWords, c("‘", "briefing", "dealbook")) 
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  snip_nrc <- d %>%
    filter(word != 'trump') %>%
    filter(word != 'stormy') %>%
    filter(word != 'john') %>%
    filter(word != 'donald') %>%
    inner_join(get_sentiments("nrc")) %>%
    group_by(sentiment) %>% 
    summarise(Frequency = sum(freq))
  
  percColName <- paste('perc', term, sep='_')
  snip_nrc[percColName] <- snip_nrc$Frequency/sum(snip_nrc$Frequency)
  
  snip_nrc
  
}

coffee <- nyt_df('coffee', "20180801", '20180831', api_key)
coffeeWC <- artWC(coffee,'response.docs.headline.main' )
coffeeSA <- artSA(coffee, 'response.docs.headline.main', 'coffee')
coffeeSA_snip <- artSA(coffee, 'response.docs.snippet', 'coffee')

democrats <- nyt_df('democrats', "20180801", '20180831', api_key)
demsWC <- artWC(democrats,'response.docs.headline.main' )
demsSA <- artSA(democrats, 'response.docs.headline.main', 'dems')
demsSA_snippet <- artSA(democrats, 'response.docs.snippet', 'dems')

demsHL <- democrats %>%
  filter(grepl("Democrat",response.docs.headline.main))

demsSnip <- democrats %>%
  filter(grepl("Democrat",response.docs.snippet))

demsHL_SA <- artSA(demsHL, 'response.docs.headline.main', 'dems')

demsSNIP_SA <- artSA(demsSnip, 'response.docs.snippet', 'dems')


republicans <- nyt_df('republicans', "20180801", '20180831', api_key)
repsWC <- artWC(republicans,'response.docs.headline.main' )
repsSA <- artSA(republicans, 'response.docs.headline.main', 'reps')
repsSA_snippet <- artSA(republicans, 'response.docs.snippet', 'reps')

repsHL <- republicans %>%
  filter(grepl("Republican",response.docs.headline.main))

repsSnip <- republicans %>%
  filter(grepl("Republican",response.docs.snippet))

repsHL_SA <- artSA(repsHL, 'response.docs.headline.main', 'reps')

repsSNIP_SA <- artSA(repsSnip, 'response.docs.snippet', 'reps')


love <- nyt_df('love', "20180801", '20180831', api_key)
loveWC <- artWC(love,'response.docs.headline.main' )
loveSA <- artSA(love, 'response.docs.headline.main', 'love')
loveSA_snippet <- artSA(love, 'response.docs.snippet', 'love')

hate <- nyt_df('hate', "20180801", '20180831', api_key)
hateWC <- artWC(hate,'response.docs.headline.main' )
hateSA <- artSA(hate, 'response.docs.headline.main', 'hate')
hateSA_snippet <- artSA(hate, 'response.docs.snippet', 'hate')

combineSA_snip <- coffeeSA_snip %>%
                inner_join(demsSA_snippet, by="sentiment") %>%
                inner_join(repsSA_snippet, by="sentiment") %>%
                inner_join(loveSA_snippet, by="sentiment")  %>%
                inner_join(hateSA_snippet, by="sentiment") 

names(combineSA_snip)[2] <- 'Freq_coffee_18'
names(combineSA_snip)[3] <- 'perc_coffee_18'
names(combineSA_snip)[4] <- 'Freq_dems_18'
names(combineSA_snip)[5] <- 'perc_dems_18'
names(combineSA_snip)[6] <- 'Freq_reps_18'
names(combineSA_snip)[7] <- 'perc_reps_18'
names(combineSA_snip)[8] <- 'Freq_love_18'
names(combineSA_snip)[9] <- 'perc_love_18'
names(combineSA_snip)[10] <- 'Freq_hate_18'
names(combineSA_snip)[11] <- 'perc_hate_18'

combineSA <- coffeeSA %>%
  inner_join(demsSA, by="sentiment") %>%
  inner_join(repsSA, by="sentiment") %>%
  inner_join(loveSA, by="sentiment")  %>%
  inner_join(hateSA, by="sentiment") 

combineDRHL <- demsHL_SA %>%
                  inner_join(repsHL_SA, by="sentiment")  %>%
                  rename(FrequencyDems18=Frequency.x) %>%
                  rename(perc_dems18=perc_dems) %>%
                  rename(FrequencyReps18=Frequency.y) %>%
                  rename(perc_reps18=perc_reps) 

write.csv(combineSA_snip, 'nyt_api/aug2018snip.csv' )
write.csv(combineSA, 'nyt_api/aug2018HLSA.csv' )
  
##################August 2017#################################

aug17_coffee <- nyt_df('coffee', "20170801", '20170831', api_key)
aug17_coffeeWC <- artWC(aug17_coffee,'response.docs.headline.main' )
aug17_coffeeSA <- artSA(aug17_coffee, 'response.docs.headline.main', 'coffee')
aug17_coffeeSA_snip <- artSA(aug17_coffee, 'response.docs.snippet', 'coffee')

aug17_democrats <- nyt_df('democrats', "20170801", '20170831', api_key)
aug17_demsWC <- artWC(aug17_democrats,'response.docs.headline.main' )
aug17_demsSA <- artSA(aug17_democrats, 'response.docs.headline.main', 'dems')
aug17_demsSA_snippet <- artSA(aug17_democrats, 'response.docs.snippet', 'dems')


aug17_demsHL <- aug17_democrats %>%
  filter(grepl("Democrat",response.docs.headline.main))

aug17_demsHL_SA <- artSA(aug17_demsHL, 'response.docs.headline.main', 'dems')

aug17_demsSnip <- aug17_democrats %>%
  filter(grepl("Democrat",response.docs.snippet))

aug17_demsSNIP_SA <- artSA(aug17_demsSnip, 'response.docs.snippet', 'dems')


aug17_republicans <- nyt_df('republicans', "20170801", '20170831', api_key)
aug17_repsWC <- artWC(aug17_republicans,'response.docs.headline.main' )
aug17_repsSA <- artSA(aug17_republicans, 'response.docs.headline.main', 'reps')
aug17_repsSA_snippet <- artSA(aug17_republicans, 'response.docs.snippet', 'reps')


aug17_repsHL <- aug17_republicans %>%
  filter(grepl("Republican",response.docs.headline.main))

aug17_repsSnip <- aug17_republicans %>%
  filter(grepl("Republican",response.docs.snippet))

aug17_repsHL_SA <- artSA(aug17_repsHL, 'response.docs.headline.main', 'reps')

aug17_repsSNIP_SA <- artSA(aug17_repsSnip, 'response.docs.snippet', 'reps')

aug17_love <- nyt_df('love', "20170801", '20170831', api_key)
aug17_loveWC <- artWC(aug17_love,'response.docs.headline.main' )
aug17_loveSA <- artSA(aug17_love, 'response.docs.headline.main', 'love')
aug17_loveSA_snippet <- artSA(aug17_love, 'response.docs.snippet', 'love')

aug17_hate <- nyt_df('hate', "20170801", '20170831', api_key)
aug17_hateWC <- artWC(aug17_hate,'response.docs.headline.main' )
aug17_hateSA <- artSA(aug17_hate, 'response.docs.headline.main', 'hate')
aug17_hateSA_snippet <- artSA(aug17_hate, 'response.docs.snippet', 'hate')


aug17_combineSA_snip <- aug17_coffeeSA_snip %>%
  inner_join(aug17_demsSA_snippet, by="sentiment") %>%
  inner_join(aug17_repsSA_snippet, by="sentiment") %>%
  inner_join(aug17_loveSA_snippet, by="sentiment")  %>%
  inner_join(aug17_hateSA_snippet, by="sentiment") 

aug17_combineSA_snip <- aug17_coffeeSA_snip %>%
  inner_join(aug17_demsSA_snippet, by="sentiment") %>%
  inner_join(aug17_repsSA_snippet, by="sentiment") %>%
  inner_join(aug17_loveSA_snippet, by="sentiment")  %>%
  inner_join(aug17_hateSA_snippet, by="sentiment") 

names(aug17_combineSA_snip)[2] <- 'Freq_coffee_17'
names(aug17_combineSA_snip)[3] <- 'perc_coffee_17'
names(aug17_combineSA_snip)[4] <- 'Freq_dems_17'
names(aug17_combineSA_snip)[5] <- 'perc_dems_17'
names(aug17_combineSA_snip)[6] <- 'Freq_reps_17'
names(aug17_combineSA_snip)[7] <- 'perc_reps_17'
names(aug17_combineSA_snip)[8] <- 'Freq_love_17'
names(aug17_combineSA_snip)[9] <- 'perc_love_17'
names(aug17_combineSA_snip)[10] <- 'Freq_hate_17'
names(aug17_combineSA_snip)[11] <- 'perc_hate_17'

aug17_combineDRHL <- aug17_demsHL_SA %>%
                        inner_join(aug17_repsHL_SA, by="sentiment") %>%
                        rename(FrequencyDems17=Frequency.x) %>%
                        rename(perc_dems17=perc_dems) %>%
                        rename(FrequencyReps17=Frequency.y) %>%
                        rename(perc_reps17=perc_reps) 

str(aug17_combineDRHL)

write.csv(aug17_combineSA_snip, 'nyt_api/aug2017snip.csv' )
write.csv(aug17_combineSA, 'nyt_api/aug2017HLSA.csv' )


##################August 2016#################################

aug16_coffee <- nyt_df('coffee', "20160801", '20160831', api_key)
aug16_coffeeWC <- artWC(aug16_coffee,'response.docs.headline.main' )
aug16_coffeeSA <- artSA(aug16_coffee, 'response.docs.headline.main', 'coffee')
aug16_coffeeSA_snip <- artSA(aug16_coffee, 'response.docs.snippet', 'coffee')

aug16_democrats <- nyt_df('democrats', "20160801", '20160831', api_key)
aug16_demsWC <- artWC(aug16_democrats,'response.docs.headline.main' )
aug16_demsSA <- artSA(aug16_democrats, 'response.docs.headline.main', 'dems')
aug16_demsSA_snippet <- artSA(aug16_democrats, 'response.docs.snippet', 'dems')

aug16_demsHL <- aug16_democrats %>%
  filter(grepl("Democrat",response.docs.headline.main))

aug16_demsHL_SA <- artSA(aug16_demsHL, 'response.docs.headline.main', 'dems')

aug16_demsSnip <- aug16_democrats %>%
  filter(grepl("Democrat",response.docs.snippet))

aug16_demsSNIP_SA <- artSA(aug16_demsSnip, 'response.docs.snippet', 'dems')

aug16_republicans <- nyt_df('republicans', "20160801", '20160831', api_key)
aug16_repsWC <- artWC(aug16_republicans,'response.docs.headline.main' )
aug16_repsSA <- artSA(aug16_republicans, 'response.docs.headline.main', 'reps')
aug16_repsSA_snippet <- artSA(aug16_republicans, 'response.docs.snippet', 'reps')


aug16_repsHL <- aug16_republicans %>%
  filter(grepl("Republican",response.docs.headline.main))

aug16_repsSnip <- aug16_republicans %>%
  filter(grepl("Republican",response.docs.snippet))

aug16_repsHL_SA <- artSA(aug16_repsHL, 'response.docs.headline.main', 'reps')

aug16_repsSNIP_SA <- artSA(aug16_repsSnip, 'response.docs.snippet', 'reps')

aug16_love <- nyt_df('love', "20160801", '20160831', api_key)
aug16_loveWC <- artWC(aug16_love,'response.docs.headline.main' )
aug16_loveSA <- artSA(aug16_love, 'response.docs.headline.main', 'love')
aug16_loveSA_snippet <- artSA(aug16_love, 'response.docs.snippet', 'love')

aug16_hate <- nyt_df('hate', "20160801", '20160831', api_key)
aug16_hateWC <- artWC(aug16_hate,'response.docs.headline.main' )
aug16_hateSA <- artSA(aug16_hate, 'response.docs.headline.main', 'hate')
aug16_hateSA_snippet <- artSA(aug16_hate, 'response.docs.snippet', 'hate')

aug16_combineSA_snip <- aug16_coffeeSA_snip %>%
  inner_join(aug16_demsSA_snippet, by="sentiment") %>%
  inner_join(aug16_repsSA_snippet, by="sentiment") %>%
  inner_join(aug16_loveSA_snippet, by="sentiment")  %>%
  inner_join(aug16_hateSA_snippet, by="sentiment") 

aug16_combineSA <- aug16_coffeeSA %>%
  inner_join(aug16_demsSA, by="sentiment") %>%
  inner_join(aug16_repsSA, by="sentiment") %>%
  inner_join(aug16_loveSA, by="sentiment")  %>%
  inner_join(aug16_hateSA, by="sentiment") 

aug16_combineDRHL <- aug16_demsHL_SA %>%
  inner_join(aug16_repsHL_SA, by="sentiment") %>%
  rename(FrequencyDems16=Frequency.x) %>%
  rename(perc_dems16=perc_dems) %>%
  rename(FrequencyReps16=Frequency.y) %>%
  rename(perc_reps16=perc_reps) 

names(aug16_combineSA_snip)[2] <- 'Freq_coffee_16'
names(aug16_combineSA_snip)[3] <- 'perc_coffee_16'
names(aug16_combineSA_snip)[4] <- 'Freq_dems_16'
names(aug16_combineSA_snip)[5] <- 'perc_dems_16'
names(aug16_combineSA_snip)[6] <- 'Freq_reps_16'
names(aug16_combineSA_snip)[7] <- 'perc_reps_16'
names(aug16_combineSA_snip)[8] <- 'Freq_love_16'
names(aug16_combineSA_snip)[9] <- 'perc_love_16'
names(aug16_combineSA_snip)[10] <- 'Freq_hate_16'
names(aug16_combineSA_snip)[11] <- 'perc_hate_16'

combineyrs_DRHL <- combineDRHL %>%
                    left_join(aug17_combineDRHL, by="sentiment") %>%
                    left_join(aug16_combineDRHL, by="sentiment")

write.csv(aug16_combineSA_snip, 'nyt_api/aug2016snip.csv' )
write.csv(aug16_combineSA, 'nyt_api/aug2016HLSA.csv' )

write.csv(combineyrs_DRHL, 'nyt_api/combineyrs_DRHL.csv' )

pos_neg_snip <- combineSA_snip %>%
                    filter(sentiment=='positive' | sentiment == 'negative') %>%
                    inner_join(aug17_combineSA_snip, by="sentiment") %>%
                    inner_join(aug16_combineSA_snip, by="sentiment") %>%
                    select("sentiment", "Freq_dems_18", "perc_dems_18", "Freq_reps_18", "perc_reps_18", 
                           "Freq_dems_17", "perc_dems_17", "Freq_reps_17", "perc_reps_17", 
                           "Freq_dems_16", "perc_dems_16", "Freq_reps_16", "perc_reps_16") 

pos_neg_snip_perc <- pos_neg_snip %>%
                        select("sentiment", "perc_dems_18", "perc_reps_18", 
                                "perc_dems_17", "perc_reps_17", 
                               "perc_dems_16", "perc_reps_16") 
pns_long <- pos_neg_snip_perc %>%
                melt(id="sentiment")

pns_long1 <- within(pns_long, variable<-data.frame(do.call('rbind', strsplit(as.character(variable), '_', fixed=TRUE))))
pns_long2 <-  do.call(data.frame, pns_long1) %>%
                  rename(party=variable.2) %>%
                  rename(year=variable.3) %>%
                  select(-variable.1)

str(pns_long2)
names(pns_long1)[2] <- 'Party'
names(pns_long1)[4] <- 'Year'

pns_long$year <- split(pns_long, pns_long$year)

pns_neg <- pns_long2 %>%
            filter(sentiment=='negative')

pns_pos <- pns_long2 %>%
  filter(sentiment=='positive')


ggplot(data=pns_neg, aes(x=year, y=value, fill=party)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

dev.copy(png, 'bar_neg.png')
dev.off()

ggplot(data=pns_pos, aes(x=year, y=value, fill=party)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

dev.copy(png, 'bar_pos.png')
dev.off()


article_counts <- data.frame("Year" = c('2018', '2018', '2017', '2017', '2016','2016'), "Party" = c('Dems','Reps','Dems','Reps','Dems','Reps'), "Count" = c(718,617,539,500,1930,1677))

ggplot(data=article_counts, aes(x=Year, y=Count, fill=Party)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

dev.copy(png, 'artCt.png')
dev.off()

