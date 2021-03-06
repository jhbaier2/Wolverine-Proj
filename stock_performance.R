library(tidyverse)

df_stock <- read_csv("stock_performance.csv")

# df_stock <-
#   df_stock %>% 
#   mutate(real_move = close - prev_close) %>% 
#   mutate(absmove = abs(real_move))

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

# depricated analysis -----------------------------------------------------



# df_pre_analysis <-
#   df_pre_analysis %>%
#   select(symbol
#          , earnings_date
#          , expected_move
#          , actual_move
#          , abs_actual_move
#          , move_ratio
#          , rt_m3
#          , rt_m5
#          , rt_m7
#          , rt_m10)

# df_pre_analysis<-
#   df_pre_analysis %>% 
#   filter(move_ratio > 0.01) %>% 
#   mutate(move_ratio_bucket = cut(log(move_ratio), 100, labels = FALSE))

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
# width (-1,-n) give window for -n lines relative to calculation
# sd_mN is reference to -N days lead up



# standard deviations -----------------------------------------------------


#should be able to use old sd calcs but use -3,-5,-7,-10 in the
# bizdays offset, in order to select the sd calcualtion from the 
# correct number of days before

# todo: write stocks performance to new csv that includes sd
  # read that in to this file and do the inner join shiz

df_stock <-
  df_stock %>% #same SDs weve calculated in the past
  mutate(act_move = (close - prev_close)/prev_close) %>% 
  group_by(Symbol) %>%
  mutate(sd_03 = rollapply(act_move, 3, sd, fill = NA, align = "left")) %>% 
  mutate(sd_05 = rollapply(act_move, 5, sd, fill = NA, align = "left")) %>%
  mutate(sd_07 = rollapply(act_move, 7, sd, fill = NA, align = "left")) %>%
  mutate(sd_10 = rollapply(act_move, 10, sd, fill = NA, align = "left"))

# df_stock %>% #write the sds to a csv so we can stop running this script
#   write_csv("stock_performance_sd.csv")

#^^I uploaded this to the drive folder

df_pre_earnings <- # m#_biz means -n business days before earnings
  df_pre_earnings %>% # m for minus
  mutate(m3_biz = bizdays::add.bizdays(earnings_date, -3 , 'Rmetrics/NYSE')) %>% 
  mutate(m5_biz = add.bizdays(earnings_date, -5, 'Rmetrics/NYSE')) %>% 
  mutate(m7_biz = add.bizdays(earnings_date, -7, 'Rmetrics/NYSE')) %>% 
  mutate(m10_biz = add.bizdays(earnings_date, -10, 'Rmetrics/NYSE'))
# want to initiate the business day before earnings equal to our window size
# We can use these for our lead up realized vol

# want to remove old SDs to avoid overlap
# next step is to append new SDs that are associated with the negative windows
df_pre_earnings <-
  df_pre_earnings %>% 
  select(symbol, 
         earnings_date,
         expected_move,
         actual_move,
         abs_actual_move,
         move_ratio,
         m3_biz,
         m5_biz,
         m7_biz,
         m10_biz)

# inner join to pick out associated SDs
# probably can be done in one line to pick out all 4 but seems easier to write 
# 4 innerjoins
df_m3 <-
  df_pre_earnings %>% 
  left_join(df_stock %>% 
               select(Symbol, TradeDate, sd_03),
             c("symbol" = "Symbol", "m3_biz" = "TradeDate"))

df_m5 <-
  df_pre_earnings %>% 
  inner_join(df_stock %>% 
               select(Symbol, TradeDate, sd_05),
             c("symbol" = "Symbol", "m5_biz" = "TradeDate"))

df_m7 <-
  df_pre_earnings %>% 
  inner_join(df_stock %>% 
               select(Symbol, TradeDate, sd_07),
             c("symbol" = "Symbol", "m7_biz" = "TradeDate"))

df_m10 <-
  df_pre_earnings %>% 
  inner_join(df_stock %>% 
               select(Symbol, TradeDate, sd_10),
             c("symbol" = "Symbol", "m10_biz" = "TradeDate"))

df_pre_analysis <-
  cbind(df_m3, df_m5$sd_05, df_m7$sd_07, df_m10$sd_10)

names(df_pre_analysis)[12] <- "sd_05"
names(df_pre_analysis)[13] <- "sd_07"
names(df_pre_analysis)[14] <- "sd_10"  

df_pre_analysis <-
df_pre_analysis %>%
  select(symbol
         , earnings_date
         , expected_move
         , actual_move
         , abs_actual_move
         , move_ratio
         , sd_03
         , sd_05
         , sd_07
         , sd_10)

# df_pre_analysis %>% write_csv("pre_earnings_realized_volatility.csv")
# uploaded to drive

# need to double check this process

# deprecated returns -----------------------------------------------------------------

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




# make the data framed grouped
# add in an empty column for each returns 
# then run the for loop separately

df_stock <-
  df_stock %>% group_by(Symbol) %>% 
  mutate(rtm3 = 0) %>% 
  mutate(rtm5 = 0) %>% 
  mutate(rtm7 = 0) %>% 
  mutate(rtm10 = 0)

# now fill those columns

# vector <-
#   df_stock %>% 
#   group_by(Symbol) %>% 
#   seq_along()
# 
# for (i in vector) {
#   df_stock$rtm3[i] <-
#     df_stock$act_move[i]/df_stock$act_move[i-3] -1
# }

Returns <- function(df, move){
  output <- data.frame(nrow(df))
  for (i in 1:nrow(.)){
    output[i] <- 
      move[i]/move[i-3]-1
  }
}

# df_stock <-
  df_stock %>% 
  group_by(Symbol) %>% 
  do(rtm3 =
       # insert custom function here
       # function(.$close, .$prev_close)
       Returns(., .$close)
       )
  
  

# Returns -----------------------------------------------------------------

df_stock_returns <-  
    df_stock %>% 
    select(TradeDate,
           Symbol,
           close) %>% 
    arrange(Symbol, TradeDate)

# df_stock_returns$diff_close <- 
#   df_stock_returns %>% 
#   group_by(Symbol) %>% 
#   c(0, diff(.$close, lag = 1))
# Were trying to compare row n with n-1 to get a difference in close, we run into an error already with column length
# the first row will be 0 to becasue there is no difference for the first close of each symbol
# we will then append a new row "simple_returns" which is the entire column diff_Close/c(1,Close[1:nrows-1]) using vector division, faster than a loop


  # per kaisa:
  # refactor data to long instead of tall,
  # use spread and gather to do so and return to tall
  # then can use diff to calculate returns
  # spread() then gather() to return

# df_stock_wide <-
#   df_stock_returns %>% 
#   group_by_at(vars(-close)) %>% 
#   mutate(row_id = 1:n()) %>% ungroup() %>% 
#   spread(key = Symbol, value = close) %>%
#   select(-row_id)

# df_stock_wide <-
#   df_stock_returns %>% 
#   group_by(Symbol) %>% 
#   mutate(row_id = 1:n()) %>% 
#   ungroup() %>% 
#   spread(key = Symbol, value = close) %>% 
#   select(-row_id) %>% 
#   filter(!is.na(.$A))

  
#brute force returns
dbl_ret <- rep(NA, nrow(df_stock_returns))
dbl_ret3 <- rep(NA, nrow(df_stock_returns))
dbl_ret5 <- rep(NA, nrow(df_stock_returns))
dbl_ret7 <- rep(NA, nrow(df_stock_returns))
dbl_ret10 <- rep(NA, nrow(df_stock_returns))

for (ix in 3:nrow(df_stock_returns)){
  if (ix %% 50000 == 0) {
    print(ix)
    toc()
    tic()
  }
  if(df_stock_returns$Symbol[ix] == df_stock_returns$Symbol[ix-2]){
    dbl_ret[ix] <-
      df_stock_returns$close[ix] / df_stock_returns$close[ix-2]
  }
}
for (ix in 4:nrow(df_stock_returns)){
  if (ix %% 50000 == 0) {
    print(ix)
    toc()
    tic()
  }
  if(df_stock_returns$Symbol[ix] == df_stock_returns$Symbol[ix-3]){
    dbl_ret3[ix] <-
      df_stock_returns$close[ix] / df_stock_returns$close[ix-3]
  }
}
for (ix in 6:nrow(df_stock_returns)){
  if (ix %% 50000 == 0) {
    print(ix)
    toc()
    tic()
  }
  if(df_stock_returns$Symbol[ix] == df_stock_returns$Symbol[ix-5]){
    dbl_ret5[ix] <-
      df_stock_returns$close[ix] / df_stock_returns$close[ix-5]
  }
}
for (ix in 8:nrow(df_stock_returns)){
  if (ix %% 50000 == 0) {
    print(ix)
    toc()
    tic()
  }
  if(df_stock_returns$Symbol[ix] == df_stock_returns$Symbol[ix-7]){
    dbl_ret7[ix] <-
      df_stock_returns$close[ix] / df_stock_returns$close[ix-7]
  }
}
for (ix in 11:nrow(df_stock_returns)){
  if (ix %% 50000 == 0) {
    print(ix)
    toc()
    tic()
  }
  if(df_stock_returns$Symbol[ix] == df_stock_returns$Symbol[ix-10]){
    dbl_ret10[ix] <-
      df_stock_returns$close[ix] / df_stock_returns$close[ix-10]
  }
}

df_stock_returns$ret_2 <- dbl_ret - 1
df_stock_returns$ret_3 <- dbl_ret3 - 1
df_stock_returns$ret_5 <- dbl_ret5 - 1
df_stock_returns$ret_7 <- dbl_ret7 - 1
df_stock_returns$ret_10 <- dbl_ret10 - 1

#match dates up to earnings dates and write to a csv

df_pre_earnings <- # m#_biz means -n business days before earnings
  df_pre_earnings %>% # m for minus
  mutate(m3_biz = bizdays::add.bizdays(earnings_date, -3 , 'Rmetrics/NYSE')) %>% 
  mutate(m5_biz = add.bizdays(earnings_date, -5, 'Rmetrics/NYSE')) %>% 
  mutate(m7_biz = add.bizdays(earnings_date, -7, 'Rmetrics/NYSE')) %>% 
  mutate(m10_biz = add.bizdays(earnings_date, -10, 'Rmetrics/NYSE')) %>% 
  mutate(m2_biz = add.bizdays(earnings_date, -2, 'Rmetrics/NYSE'))

df_pre_earnings <-
  df_pre_earnings %>% 
  select(symbol, 
         earnings_date,
         expected_move,
         actual_move,
         abs_actual_move,
         move_ratio,
         m3_biz,
         m5_biz,
         m7_biz,
         m10_biz,
         m2_biz)

df_m3 <-
  df_pre_earnings %>% 
  inner_join(df_stock_returns %>% 
              select(Symbol, TradeDate, ret_3),
            c("symbol" = "Symbol", "m3_biz" = "TradeDate"))

df_m5 <-
  df_pre_earnings %>% 
  inner_join(df_stock_returns %>% 
              select(Symbol, TradeDate, ret_5),
            c("symbol" = "Symbol", "m3_biz" = "TradeDate"))

df_m7 <-
  df_pre_earnings %>% 
  inner_join(df_stock_returns %>% 
              select(Symbol, TradeDate, ret_7),
            c("symbol" = "Symbol", "m3_biz" = "TradeDate"))

df_m10 <-
  df_pre_earnings %>% 
  inner_join(df_stock_returns %>% 
               select(Symbol, TradeDate, ret_10),
             c("symbol" = "Symbol", "m3_biz" = "TradeDate"))

df_m2 <-
  df_pre_earnings %>% 
  inner_join(df_stock_returns %>% 
               select(Symbol, TradeDate, ret_2),
             c("symbol" = "Symbol", "m3_biz" = "TradeDate"))

df_pre_analysis <-
  cbind(df_m2,df_m3$ret_3, df_m5$ret_5, df_m7$ret_7, df_m10$ret_10)

names(df_pre_analysis)[13] <- "ret_3"
names(df_pre_analysis)[14] <- "ret_5"
names(df_pre_analysis)[15] <- "ret_7"
names(df_pre_analysis)[16] <- "ret_10"

df_pre_analysis <-
  df_pre_analysis %>%
  select(symbol
         , earnings_date
         , expected_move
         , actual_move
         , abs_actual_move
         , move_ratio
         , ret_2
         , ret_3
         , ret_5
         , ret_7
         , ret_10)

# df_pre_analysis %>% write_csv("pre_earings_returns.csv")
