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
  ))+
  geom_smooth(mapping = aes(
    x = move_ratio_bucket, y = avg_sd3_ratio
  ), method = "lm", se = F)#+
  # geom_smooth(mapping= aes(
  #   x = move_ratio_bucket, y = avg_sd3_ratio, color = "red"
  # ), method = "auto", se = F)

df_pre_earnings %>% 
  group_by(move_ratio_bucket) %>% 
  summarize(avg_sd5_ratio = log(mean(sd_05/expected_move))) %>% 
  ggplot()+geom_point(mapping = aes(
    x = move_ratio_bucket, y = avg_sd5_ratio
  ))+
  geom_smooth(mapping = aes(
    x = move_ratio_bucket, y = avg_sd5_ratio
  ), method = "lm", se = F)


df_pre_earnings %>% 
  group_by(move_ratio_bucket) %>% 
  summarize(avg_sd7_ratio = log(mean(sd_07/expected_move))) %>% 
  ggplot()+geom_point(mapping = aes(
    x = move_ratio_bucket, y = avg_sd7_ratio
  ))+
  geom_smooth(mapping = aes(
    x = move_ratio_bucket, y = avg_sd7_ratio
  ), method = "lm", se = F)

df_pre_earnings %>% 
  group_by(move_ratio_bucket) %>% 
  summarize(avg_sd10_ratio = log(mean(sd_10/expected_move))) %>% 
  ggplot()+geom_point(mapping = aes(
    x = move_ratio_bucket, y = avg_sd10_ratio
  ))+
  geom_smooth(mapping = aes(
    x = move_ratio_bucket, y = avg_sd10_ratio
  ), method = "lm", se = F)

# values deviate from the mean and time goes away from earnings
# volatility is less correlated with earnings day moves as  you go back further from 
# earnings, only the -3 had any real correlation

df_pre_earnings %>% 
  group_by(move_ratio_bucket, gain_loss) %>% 
  summarize(avg_sd5_ratio = log(mean(sd_05/expected_move))) %>% 
  ggplot()+geom_point(mapping = aes(
    x = move_ratio_bucket, y = avg_sd5_ratio
  ))+
  geom_smooth(mapping = aes(
    x = move_ratio_bucket, y = avg_sd5_ratio
  ), method = "lm", se = F)+
  facet_wrap(~gain_loss, nrow = 2)

# returns data ----------------------------------------------------------

df_pre_returns <- read_csv("pre_earings_returns.csv")

df_pre_returns <-
  df_pre_returns %>% 
  filter(expected_move > 0.01) %>% 
  filter(move_ratio > 0.01) %>% 
  mutate(gain_loss = map_chr(actual_move,
                             function(move) if(move < 0) {"loss"} else {"gain"})) %>% 
  mutate(move_ratio_bucket = cut(log(move_ratio), 100, labels = FALSE))

df_post_earnings <- read_csv("post_earnings_realized_volatility.csv")

# returns graphs ----------------------------------------------------------

df_pre_returns %>% 
  filter(ret_2 < 1) %>% 
  ggplot() + geom_point(
    mapping = aes(x = log(move_ratio), y = ret_2)
  )

df_pre_returns %>% 
  filter(ret_2 < 1) %>% 
  group_by(move_ratio_bucket) %>% 
  ggplot()+ geom_point(mapping = aes(
    x = move_ratio_bucket, y = ret_2
  ))

#2 day returns vs move ratio bucket
df_pre_returns %>% 
  filter(ret_2 < 1) %>% 
  group_by(move_ratio_bucket) %>% 
  summarise(avg_ret_2 = mean(ret_2/expected_move)) %>% 
  ggplot()+ geom_point(mapping = aes(
    x = move_ratio_bucket, y = avg_ret_2
  ))+
  geom_smooth(mapping = aes(
    x = move_ratio_bucket, y = avg_ret_2
  ), se = F)

#split by gain loss on earnings day
df_pre_returns %>% 
  filter(ret_2 < 1) %>% 
  group_by(move_ratio_bucket, gain_loss) %>% 
  summarise(avg_ret_2 = mean(ret_2/expected_move)) %>% 
  ggplot()+ geom_point(mapping = aes(
    x = move_ratio_bucket, y = avg_ret_2
  ))+
  geom_smooth(mapping = aes(
    x = move_ratio_bucket, y = avg_ret_2
  ), se = F)+
  facet_wrap(~gain_loss, nrow = 2)

# pre returns vs post vol -------------------------------------------------

#join pre returns vs post earnings sd so they can be graphed together
df_comparison <- df_post_earnings %>% 
  select(symbol ,earnings_date, move_ratio, expected_move,  sd_03, sd_05, sd_07, sd_10) %>% 
  inner_join(df_pre_returns %>% 
               select(symbol ,earnings_date, ret_2, ret_3, ret_5, ret_7, ret_10)
            , c("earnings_date" = "earnings_date", "symbol" = "symbol"))

#graphs of post earnings vol vs pre earnings rewturns
df_comparison %>% 
  ggplot()+geom_point(mapping = aes(
    x= log(sd_03), y = ret_3
  ))+ xlim(-8,0)+ ylim(-.4,.4)

df_comparison %>% 
  ggplot()+ geom_point(mapping = aes(
    x = log(sd_03/expected_move), y = ret_3/expected_move
  ))


# post earnings split by pre move -----------------------------------------

#add column telling if stock gained or lossed in the three days
#leading up to earnings
df_pre_returns<-
  df_pre_returns %>% 
  mutate(leadup_move = map_chr(ret_3, 
                               function(move) if(move<0){"loss"}else{"gain"}))

#add this column to post earnings
  #we want to compare it to post earnings vol and MR
df_post_earnings<-
  df_post_earnings %>% 
  inner_join(df_pre_returns %>% select(
                                symbol, earnings_date, leadup_move
                                )
             , c("earnings_date"= "earnings_date", "symbol" = "symbol"))

df_post_earnings <-
  df_post_earnings %>% 
  mutate(move_ratio_bucket = cut(log(move_ratio), 100, labels = FALSE))

# post earnings vol split by lead up gain loss
df_post_earnings %>% 
  group_by(move_ratio_bucket, leadup_move) %>% 
  summarize(avg_sd_05_ratio = log(mean(sd_05/expected_move))
            ,num_obs = n()) %>% 
  filter(num_obs > 50) %>% 
  ggplot() + geom_point(mapping = aes(x = move_ratio_bucket, 
                                      y = avg_sd_05_ratio, color = leadup_move),
                        show.legend = F) +
  geom_smooth(mapping = aes(x = move_ratio_bucket, y = avg_sd_05_ratio), method = "lm",
              se = F) +
  facet_wrap(~ leadup_move, ncol = 1)+
  labs(x = "Move Ratio Rank", y = "Average S.D. 5")
