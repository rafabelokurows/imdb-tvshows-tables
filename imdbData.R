library(tidyverse)
library(scales)
library(gt)
library(data.table)
library(tictoc)
library(webshot2)


setwd("C:\\Users\\belokurowsr\\OneDrive - Kantar\\Desktop\\IMDBRatingsTable")

tic()
df_ratings <- fread('title.ratings.tsv.gz', na = "\\N", quote = '')
toc()

tic()
titles <- fread('title.basics.tsv.gz', na = "\\N", quote = '')
toc()

tic()
episodes <- read_tsv('title.episode.tsv.gz', na = "\\N", quote = '')
toc()

bcs = episodes %>% filter(parentTconst == "tt3032476" ) %>%
  left_join(titles,by=c("tconst")) %>%
  left_join(df_ratings,by=c("tconst"))
got = episodes %>% filter(parentTconst == "tt0944947" ) %>%
  left_join(titles,by=c("tconst")) %>%
  left_join(df_ratings,by=c("tconst"))
bb = episodes %>% filter(parentTconst == "tt0903747" ) %>%
  left_join(titles,by=c("tconst")) %>%
  left_join(df_ratings,by=c("tconst"))
#Sucession = tt7660850
#sopranos = tt0141842
#madmen = tt0804503

 bb = bb %>% mutate(seasonNumber =as.character(seasonNumber)) %>%
   arrange(seasonNumber,episodeNumber)
 got = got %>% mutate(seasonNumber =paste0("S",as.character(seasonNumber))) %>%
   arrange(seasonNumber,episodeNumber)
 library(plotly)


#Plot Game of Thrones seasons
m <- got[which.min(got$averageRating), ]
a <- list(
   y = m$averageRating,
   x = m$episodeNumber,
   text = paste0("Series finale \n Rating ",m$averageRating," out of 10"),
   xref = "x",
   yref = "y",
   showarrow = TRUE,
   arrowhead = 1,
   ax = 80,
   ay = -25

 )
plot = plot_ly(got,x = ~episodeNumber, y = ~averageRating,
        split = ~seasonNumber,
        color= ~seasonNumber,
        text = ~paste('</br> Season: ',seasonNumber,'</br> Episode: ', episodeNumber,
                      '</br> Rating: ', averageRating),
        hoverinfo = 'text',
        type = "scatter",
        mode = "lines+markers",
        colors = c("#092215","#092215","#092215","#092215","#092215","#092215","#092215","red")
) %>% layout(title = list(text= "Average rating of Game of Thrones episodes on IMDB", y = 1.05, yanchor =  'top'),
             xaxis = list(title = 'Episode No.'),
             yaxis = list(title = 'Avg. Rating'),
             legend = list(title=list(text='<b>Season</b>')),
             annotations=a)
plot
export(plot, "seasons-got.png")
htmlwidgets::saveWidget(plot, file = "plot.html")

#Plot Breaking Bad
tableBB  = bb %>%
  select(seasonNumber,episodeNumber,averageRating) %>%
  arrange(seasonNumber,episodeNumber) %>%
  mutate(episodeNumber = paste0("Ep. ",episodeNumber)) %>%
  pivot_wider(names_from=seasonNumber,values_from=averageRating,names_prefix = "S") %>%
  ungroup %>%
  add_row(episodeNumber = 'Mean', !!! colMeans(.[-1],na.rm=T)) %>%
  mutate_if(is.numeric,round,1) %>%
gt() %>%
  data_color(columns = starts_with("S"),colors=scales::col_bin(
    bins = c(0, 5,7,8,9,10,11),na.color = "white",right = F,
      palette = c("#B8293D","#E56717","#53BFBF","#007463","#26580F","#D4AF37"),
  ),apply_to = "fill",autocolor_text = T) %>%
  cols_label(episodeNumber="") %>%
  sub_missing(columns=starts_with("S"),missing_text = "") %>%
  tab_options(data_row.padding = px(3),table.align="center") %>%
  cols_width(1~60,everything() ~ px(45)) %>%
  cols_align(c(2:6),align="center")%>%
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "black",
      weight = px(2.3),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),rows = 17
    )
  ) %>% tab_header(title = "Breaking Bad reviews on IMDB") %>%
  tab_footnote(
    footnote = paste0("The Fly episode ",emo::ji("sleepy")),
    locations = cells_body(
      columns = S3,rows=10
    )
  )%>%
  tab_footnote(
    footnote = paste0("Ozymandias, only TV episode on IMDB with 10 avg. rating (1000+ ratings)",emo::ji("star")),
    locations = cells_body(
      columns = S5,rows=14
    )
  )
tableBB
gtsave(tableBB,"BB.png")

#Plot Game of Thrones
tableGot = got %>%
  select(seasonNumber,episodeNumber,averageRating) %>%
  arrange(seasonNumber,episodeNumber) %>%
  mutate(episodeNumber = paste0("Ep. ",episodeNumber)) %>%
  pivot_wider(names_from=seasonNumber,values_from=averageRating,names_prefix = "S") %>%
  ungroup %>%
  add_row(episodeNumber = 'Mean', !!! colMeans(.[-1],na.rm=T)) %>%
  mutate_if(is.numeric,round,1) %>%
  gt() %>%
  data_color(columns = starts_with("S"),colors=scales::col_bin(
    bins = c(0, 5,7,8,9,10,11),na.color = "white",right = F,
    palette = c("#B8293D","#E56717","#53BFBF","#007463","#26580F","#D4AF37"),
  ),apply_to = "fill",autocolor_text = T) %>%
  cols_label(episodeNumber="") %>%
  sub_missing(columns=starts_with("S"),missing_text = "") %>%
  tab_options(data_row.padding = px(3),table.align="center") %>%
  cols_width(1~60,everything() ~ px(45)) %>%
  cols_align(c(2:6),align="center")%>%
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "black",
      weight = px(2.3),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),rows = 11
    )
  ) %>% tab_header(title = "Game of Thrones reviews on IMDB") %>%
  tab_footnote(
    footnote = paste0("The abominable last season that we all love to hate ",emo::ji("poop")),
    locations = cells_column_labels(
      columns = S8
    )
  )
tableGot
gtsave(tableGot,"got.png")

#Plot Better Call Saul
tableBcs = bcs %>%
  select(seasonNumber,episodeNumber,averageRating) %>%
  arrange(seasonNumber,episodeNumber) %>%
  mutate(episodeNumber = paste0("Ep. ",episodeNumber)) %>%
  pivot_wider(names_from=seasonNumber,values_from=averageRating,names_prefix = "S") %>%
  ungroup %>%
  add_row(episodeNumber = 'Mean', !!! colMeans(.[-1],na.rm=T)) %>%
  mutate_if(is.numeric,round,1) %>%
  gt() %>%
  data_color(columns = starts_with("S"),colors=scales::col_bin(
    bins = c(0, 5,7,8,9,10,11),na.color = "white",right = F,
    palette = c("#B8293D","#E56717","#53BFBF","#007463","#26580F","#D4AF37"),
  ),apply_to = "fill",autocolor_text = T) %>%
  cols_label(episodeNumber="") %>%
  sub_missing(columns=starts_with("S"),missing_text = "") %>%
  tab_options(data_row.padding = px(3),table.align="center") %>%
  cols_width(1~60,everything() ~ px(45)) %>%
  cols_align(c(2:6),align="center")%>%
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "black",
      weight = px(2.3),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),rows = 14
    )
  ) %>% tab_header(title = "Better Call Saul reviews on IMDB")  %>%
  tab_source_note(source_note = paste0("Not a single episode below 8 avg. rating ",emo::ji("fire"),emo::ji("fire"),emo::ji("fire")))
tableBcs

gtsave(tableBcs,"bcs.png")


# "28AFB0" - teal
# "FF9F1C" - gold
# "00917c"
# "#26580F"
# "18731E"
# "#8EB69B"
# #00917C
# #20525C
# "#235347" - verde
# show_col(scales::col_bin(
#   bins = c(0, 5,6,7,9,10,11),na.color = "white",right = T,
#   palette = c("#B8293D", "orange","#54C2CC","#26580F","#FF9F1C"),
# ))


#https://www.imdb.com/interfaces/
#https://datasets.imdbws.com/
#https://minimaxir.com/2018/07/imdb-data-analysis/
#
# epsRatings = episodes %>%
#   inner_join(df_ratings,by=c("tconst"))
#
# #epsRatings %>% arrange(desc(averageRating),desc(numVotes))
# epsRatings %>% group_by(parentTconst) %>% summarize(max(seasonNumber),sum(numVotes)) %>% View()

# teste = titles %>% filter(titleType == "tvEpisode" & isAdult==0 & between(runtimeMinutes,30,70) &
#                     between(startYear,1995,2022) & !str_detect(genres,"Game-Show|Reality|Reality-TV|Documentary")) %>%
#   inner_join(episodes)
#
# teste %>% head()
