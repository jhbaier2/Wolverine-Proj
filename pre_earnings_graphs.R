library(tidyverse)

df_pre_earnings <- read_csv("pre_earnings_realized_volatility.csv")

df_pre_earnings <-
  df_pre_earnings %>% 
  filter(expected_move > 0.01) %>% 
  filter(move_ratio > 0.01) %>% 
  mutate(gain_loss = map_chr(actual_move, 
                             function(move) if(move < 0) {"loss"} else {"gain"})) %>% 
  mutate(move_ratio_bucket = cut(log(move_ratio), 100, labels = FALSE))


# volatility graphs -------------------------------------------------------

df_pre_earnings %>% 
  ggplot() %>% geom_point(mapping = aes(x= log(move_ratio), y = log(sd_03)))
