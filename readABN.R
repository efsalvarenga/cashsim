# title: "Reading ABN statements"
# author: "EFS Alvarenga"
# date: "03/08/2020"

library(dplyr)
library(ggplot2)

# tools
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getCustomSum <- function(data, summaryGroups, generateDate = FALSE) {
    result <- data %>%
      group_by_at(summaryGroups) %>%
      summarise(cashIn = sum(cashIn),
                cashOut = sum(cashOut)) %>%
      ungroup() %>%
      mutate(delta = cashIn + cashOut)
    
    if (generateDate) {
      result$date <- as.Date(paste(result$year, result$month, '01', sep = '-'))
    }
    
    return(result)
  }

# read abn tabular data
readRaw <- read.delim2('~/Google Drive/Ninho/Financeiro/abnExports/TXT200802124803.TAB',
                      header = FALSE,
                      stringsAsFactors = FALSE)
colnames(readRaw) <- c('id', 'currency', 'date', 'before', 'after',
                       'date2', 'value', 'description')

# munging data into proper analytical format
readMunged <- readRaw %>% 
  select(-date2) %>%
  mutate(year = as.numeric(substr(date, 1, 4)),
         month = as.numeric(substr(date, 5, 6)),
         date = as.Date(as.character(date), format = '%Y%m%d'),
         label = case_when(date < as.Date('2019-12-01') ~ 'transit',
                           TRUE ~ 'WBS13'),
         cashIn = ifelse(value > 0, value, 0),
         cashOut = ifelse(cashIn == 0, value, 0))

# visuals
ggplot(readMunged, aes(x = date, y = after)) +
  geom_line()

readMunged %>%
  filter(label != 'transit') %>%
  ggplot(aes(x = date, y = after)) +
  geom_line()

# some summarised analysis
sumMonth <- getCustomSum(data = readMunged,
                         summaryGroups = c('year', 'month', 'label'),
                         generateDate = TRUE)
sumMonth %>%
  # filter(label != 'transit') %>%
  ggplot(aes(x = date, y = delta)) +
  geom_point()

sumYear <- getCustomSum(data = readMunged,
                        summaryGroups = c('year'),
                        generateDate = FALSE)

sumLabel <- getCustomSum(data = readMunged,
                         summaryGroups = c('label'),
                         generateDate = FALSE)
