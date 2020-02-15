# Install package nCov2019 by Guangchuang Yu, a professor of bioinformatics at Southern Medical University
remotes::install_github("GuangchuangYu/nCov2019")


library(tidyverse)
library(lubridate)
library(writexl)

# Update data from corona virus
library(nCov2019)


# Getting the latest dataset
china_cases <- get_nCov2019(lang = "en")
warnings()

china_cases

# View provincial cases in China 

china_cases_tbl <- china_cases[]
china_cases_tbl
    
    # City cases in Macau
    china_cases_tbl[33,]
    
    # City cases in Hubei
    hubei_cases_tbl <- china_cases[1, ] %>% 
        glimpse()
        mutate(confirm_num = as.numeric(confirm),
               total = (confirm_num + dead + heal)) %>%
        select(name, starts_with("confirm"), suspect, dead, heal, total, everything()) %>% 
        select(-confirm) %>%
        mutate(name = fct_reorder(name, confirm_num))
       
    hubei_cases_tbl
       
        # Visualize
        hubei_cases_tbl %>% 
            filter(total != 0) %>% 
            select(name, confirm_num) %>% 
            ggplot(aes(name, confirm_num)) +
            geom_col(alpha = 0.7, 
                     fill = "#E4572E") + coord_flip() +
            geom_text(aes(label = confirm_num), hjust = -0.2) +
            
        # Formatting
            theme_minimal(base_size = 14) +
            scale_y_continuous(expand = expand_scale(mult = c(0, 0.2)), 
                               labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
            labs(title = "Total confirmed cases in Hubei cities",
                 subtitle = "Last update: 2020-02-14 02:37:43",
                 x = "",
                 y = "",
                 caption = "Data: https://news.qq.com/zt2020/page/feiyan.htm")
            
# Looking at the global data


global_cases <- china_cases["global", ] 
global_cases

        
# Global cases
global_cases_tbl <- global_cases %>% 
    mutate(confirm_num = as.numeric(confirm),
    total = (confirm_num + dead + heal)) %>%
    select(name, starts_with("confirm"), suspect, dead, heal, total, everything()) %>% 
    select(-confirm) %>%
    mutate(name = fct_reorder(name, confirm_num))
        
global_cases_tbl %>% 
    summarise(sum(confirm_num))
        
    # Visualize
    global_cases_tbl %>% 
        filter(total != 0) %>% 
        select(name, confirm_num) %>% 
        ggplot(aes(name, confirm_num)) +
        geom_col(alpha = 0.7,fill = "#76B041") + coord_flip() +
        geom_text(aes(label = confirm_num), hjust = -0.2) +
            
        # Formatting
        theme_minimal(base_size = 14) +
        scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)), 
                           labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
        labs(title = "Total confirmed cases worldwide",
             subtitle = "Last update: 2020-02-14 02:37:43",
             x = "",
             y = "",
             caption = "Data: https://news.qq.com/zt2020/page/feiyan.htm")   
    

# Newly added data in all provinces of China
china_cases[, by = "today"]

china_cases[, by = "today"] %>% 
    summarise(confirmed_cases = sum(confirm))

# Newly added data in Hubei
china_cases["Hubei", by = "today"]

china_cases["Hubei", by = "today"] %>% 
    summarise(confirmed_cases = sum(confirm))

# Summary cases of the daily data in China

    # Cumulative daily summary: data since 13th january
    daily_cumulative_cases_china_tbl <- summary(china_cases) %>% 
        rename(cum_confirm = confirm, cum_suspect = suspect, cum_dead = dead, cum_heal = heal)
    
    daily_cumulative_cases_china_tbl %>% 
        count(date, n())
    
    # New daily summary cases: data since 20th january
    daily_new_cases_china_tbl <- summary(china_cases, by = "today") %>% 
        select(-deadRate, -healRate)
    
    daily_new_cases_china_tbl %>% 
        count(date, n())
    
    # There's about 150 confirmed cases in cumulative not considered in new cases.
    daily_new_cases_china_tbl %>% 
        summarise(sum(confirm)) 
   
    # Joining cumalitive and new cases in one dataframe
    daily_cases_china_joined_tbl <- daily_cumulative_cases_china_tbl %>% 
        left_join(daily_new_cases_china_tbl, by = c("date" = "date")) %>% 
        mutate(date = as.Date(date, "%m.%d")) %>% 
        select(date, everything()) 

    # Visualize data day by day
    daily_cases_china_joined_tbl %>% 
        ggplot(aes(date, confirm)) +
        geom_point(color = "red", na.rm = TRUE) +
        geom_line(na.rm = TRUE, aes(color = "red"), show.legend = FALSE) +
        scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
        theme_minimal(base_size = 14) +
        labs(title = "Daily confirmed cases in China",
             subtitle = "Last update: 2020-02-13 08:32:50",
             x = "",
             y = "",
             caption = "Data: https://news.qq.com/zt2020/page/feiyan.htm")   
    
    # Visualize cumulative data day by day
    daily_cases_china_joined_tbl %>% 
        ggplot(aes(date, cum_confirm, size = cum_confirm)) +
        geom_point(color = "red") +
        scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
        theme_minimal(base_size = 14) +
        labs(title = "Coronavirus confirmed cases increase in China",
             subtitle = "Last update: 2020-02-13 08:32:50",
             x = "",
             y = "",
             size = "cases",
             caption = "Data: https://news.qq.com/zt2020/page/feiyan.htm")

 

'''''''
# Historical cumulative confirmed cases in China until 2020-02-10
historical_china_cases_tbl %>% 
    select(time, confirm) %>% 
    group_by(time) %>% 
    summarize(confirmed_cases = sum(confirm, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(desc(time)) %>% 
    summarise(sum(confirmed_cases))

# Historical cumulative confirmed cases in Hubei until 2020-02-10
historical_china_cases_tbl %>% 
    filter(province == "Hubei") %>% 
    select(time, confirm) %>% 
    group_by(time) %>% 
    summarize(confirmed_cases = sum(confirm, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(desc(time)) %>% 
    summarise(sum(confirmed_cases))


historical_china_cases_tbl %>% 
    select(time, cum_confirm) %>% 
    group_by(time) %>% 
    summarize(cum_confirmed_cases = sum(cum_confirm, na.rm = TRUE)) %>% 
    ungroup() %>% 
    summarise(sum(cum_confirmed_cases))

   
    