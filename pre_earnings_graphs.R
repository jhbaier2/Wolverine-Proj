library(tidyverse)

df_pre_earnings <- read_csv("pre_earnings_realized_volatility.csv")

df_pre_earnings <-
  df_pre_earnings %>% 
  filter(expected_move > 0.01) %>% 
  filter(move_ratio > 0.01) %>% 
  mutate(gain_loss = map_chr(actual_move, 
                             function(move) if(move < 0) {"loss"} else {"gain"})) %>% 
  mutate(move_ratio_bucket = cut(log(move_ratio), 100, labels = FALSE))

# throughout there will be 1 row with missing data

# volatility graphs -------------------------------------------------------

df_pre_earnings %>% 
  ggplot() + geom_point(mapping = aes(
    x = log(expected_move), y = log(sd_03)
  ))
# looks to be less correlation than post
  # so volatility clustering has a stronger effect after earnings
  # assuming this graph is correct

df_pre_earnings %>% 
  ggplot() + geom_point(mapping = aes(
    x = log(move_ratio), y = log(sd_03/expected_move)
  ))

df_pre_earnings %>% 
  group_by(move_ratio_bucket) %>% 
  summarize(avg_sd3_ratio = log(mean(sd_03/expected_move))) %>% 
  ggplot()+geom_point(mapping = aes(
    x = move_ratio_bucket, y = avg_sd3_ratio
  ))

# returns graphs ----------------------------------------------------------



