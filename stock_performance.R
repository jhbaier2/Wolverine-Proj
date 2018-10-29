library(tidyverse)

df_stock <- read_csv("stock_performance.csv")

df_stock <-
  df_stock %>% 
  mutate(real_move = close - prev_close) %>% 
  mutate(absmove = abs(real_move))

library(zoo)
# tic()
# df_stock <- #add returns to frame
#   # change to p_0/p_-3 - 1
#   df_stock %>% mutate(pmove = real_move / prev_close) %>% 
#   group_by(Symbol) %>% 
#   mutate(rt_m3 = rollapply(pmove, 
#                            width=list(-1:-3), sum, fill = NA, align = "left")) %>%  #rt_m3: stock return on minus 3 days before earnings
#   mutate(rt_m5 = rollapply(pmove, 
#                            width=list(-1:-5), sum, fill = NA, align = "left")) %>% 
#   mutate(rt_m7 = rollapply(pmove, 
#                            width=list(-1:-7), sum, fill = NA, align = "left")) %>% 
#   mutate(rt_m10 = rollapply(pmove, 
#                             width=list(-1:-10), sum, fill = NA, align = "left"))
# # toc() 98.69 seconds

# ^^ sum is incorrect, going to redo returns below

df_pre_earnings <- read_csv("post_earnings_realized_volatility.csv")
library(bizdays)
bizdays::load_rmetrics_calendars(2008:2018)

df_pre_earnings <-
  df_pre_earnings %>% 
  mutate(next_biz_day = bizdays::add.bizdays(earnings_date, 1 , 'Rmetrics/NYSE'))

df_pre_analysis <- df_pre_earnings %>% 
  inner_join(df_stock %>% 
               select(Symbol, TradeDate, rt_m3, rt_m5, rt_m7, rt_m10) 
             , c("symbol" = "Symbol", "next_biz_day" = "TradeDate"))

df_pre_analysis <-
  df_pre_analysis %>%
  select(symbol
         , earnings_date
         , expected_move
         , actual_move
         , abs_actual_move
         , move_ratio
         , rt_m3
         , rt_m5
         , rt_m7
         , rt_m10)

df_pre_analysis<-
  df_pre_analysis %>% 
  filter(move_ratio > 0.01) %>% 
  mutate(move_ratio_bucket = cut(log(move_ratio), 100, labels = FALSE))

gl <- function(move){
  if (move >0)
  {"gain"}
  else
  {"loss"}
}

df_pre_analysis <-
  df_pre_analysis %>% 
  mutate(gainloss = map_chr(actual_move, function(move){
    if (move >0)
    {"gain"}
    else
    {"loss"}
  }))

df_pre_analysis %>% 
  group_by(move_ratio_bucket) %>% 
  ggplot() + 
  geom_point(mapping= aes(x = move_ratio_bucket, y = mean(rt_m3)))+
  facet_wrap(~gainloss, ncol = 1)

# tic()
df_stock <- #add sds to frame
  df_stock %>% mutate(pmove = real_move / prev_close) %>% 
  group_by(Symbol) %>% 
  mutate(sd_m3 = rollapply(pmove,  
                           width=list(-1:-3), sd, fill = NA, align = "left")) %>%  #rt_m3: stock return on minus 3 days before earnings
  mutate(sd_m5 = rollapply(pmove, 
                           width=list(-1:-5), sd, fill = NA, align = "left")) %>% 
  mutate(sd_m7 = rollapply(pmove, 
                           width=list(-1:-7), sd, fill = NA, align = "left")) %>% 
  mutate(sd_m10 = rollapply(pmove, 
                            width=list(-1:-10), sd, fill = NA, align = "left"))
# toc()#640s
# function to calculate p_0/p_(-n) -1
Returns <- function(df, move){ # whats wrong with this for loop
  output <- vector("double", nrow(df))
  for (i in 4:nrow(df)){
    # if (is.na(output[i-3]))
    # {output[i] <- 0}
    # else
    output[i] <- move[i]/move[i-3] - 1
  }
}


df_stock <- #add for real returns to frame
  df_stock %>%  
  group_by(Symbol) %>% 
  mutate(rtm3 = Returns(df_stock, real_move))

# df_stock %>% for (i in 1:length(df_stock$real_move)) {
#  real_move[i]/real_move[i-3] - 1 
# }

test <- vector("double", nrow(df_stock))#make empty vector to fill with returns
test <- t(test)#want column not row vector

#loop through calculating p_0/p_-3 - 1
for (i in 4:nrow(df_stock)){ 
  test[i] <-
    df_stock$real_move[i]/df_stock$real_move[i-3] -1
}
# how to deal with inevitable divide by 0s?

df_stock %>% 
  group_by(Symbol) %>% 
  for(i in 4:nrow(df_stock)){
    df_stock$rtm3[i] <-
      df_stock$real_move[i] / df_stock$real_move[i-3] -1
  }
