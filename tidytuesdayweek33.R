library(tidytext)
library(tidyverse)
library(tidytuesdayR)
library(ggplot2)
library(ggthemes)
library(stopwords)
library(wordcloud)

tuesdaydata <- tidytuesdayR::tt_load('2020-08-11')

avatardata<-as.data.frame(tuesdaydata$avatar)

ggplot(avatardata,aes(x=chapter_num,y=book_num, fill = imdb_rating))+
  geom_tile(color="blue")+
  geom_text(aes(label=imdb_rating))+
  scale_x_continuous("Chapter Number", labels = as.character(avatardata$chapter_num), breaks = avatardata$chapter_num) +
  scale_y_continuous("Book Number", labels = as.character(avatardata$book_num), breaks = avatardata$book_num) +
  scale_fill_gradient(
    low = "orange",
    high = "green",
    space = "Lab",
    na.value = "gray",
    guide = "colourbar",
    aesthetics = "fill")+labs(
       title = "IMBD Ratings of Book Chapters", 
       subtitle= "#tidytuesday Week 33 ", 
       fill="IMDB Rating",
       caption = "Twitter: @tolgaakurtuluss") + theme_fivethirtyeight()

tidy_avatar_text <- avatardata%>%
  select(book_num,full_text)%>%
  unnest_tokens(word, full_text)%>%
  anti_join(stop_words)


tidy_avatar_text %>%
  count(word,book_num, sort = T) %>%
  group_by(book_num) %>%
  top_n(n = 10, n) %>%
  ggplot(aes(x = reorder_within(word, n, book_num), y = n, fill = book_num)) +
  geom_col(position="dodge",show.legend = FALSE) +
  geom_label(aes(label =n),fill="yellow",colour = "red",show.legend = FALSE)+
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~book_num, ncol = 3, scales = "free_y") +
  scale_x_reordered() +
  labs(
    title = "Most Used Words in Books", 
    subtitle= "#tidytuesday Week 33 ", 
    caption = "Twitter: @tolgaakurtuluss")


tidy_avatar_text2 <- avatardata%>%
  select(chapter_num,full_text)%>%
  unnest_tokens(word, full_text)%>%
  anti_join(stop_words) 

tidy_avatar_text2 %>%
  count(word,chapter_num, sort = T) %>%
  group_by(chapter_num) %>%
  top_n(n = 3, n) %>%
  ggplot(aes(x = reorder_within(word, n, chapter_num), y = n, fill = chapter_num)) +
  geom_col(position="dodge",show.legend = FALSE) +
  geom_label(aes(label =n),fill="yellow",colour = "red",show.legend = FALSE)+
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~chapter_num, ncol = 3, scales = "free_y") +
  scale_x_reordered() +
  labs(
    title = "Most Used 3 Words in Chapters", 
    subtitle= "#tidytuesday Week 33 ", 
    caption = "Twitter: @tolgaakurtuluss")
