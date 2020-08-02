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

getCustomSum <- function(data, summaryGroups, generateDate = FALSE){
  browser()
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

# some monthly analysis
monthly <- readMunged %>%
  group_by(year, month, label) %>%
  summarise(cashIn = sum(cashIn),
            cashOut = sum(cashOut)) %>%
  ungroup() %>%
  mutate(date = paste(year, month, '01', sep = '-'),
         delta = cashIn + cashOut)

yearlabel <- monthly %>%
  group_by(year, label) %>%
  summarise(cashIn = sum(cashIn),
            cashOut = sum(cashOut)) %>%
  ungroup() %>%
  mutate(delta = cashIn + cashOut)

yearly <- yearlabel %>%
  group_by(year) %>%
  summarise(cashIn = sum(cashIn),
            cashOut = sum(cashOut)) %>%
  ungroup() %>%
  mutate(delta = cashIn + cashOut)

labelly <- yearlabel %>%
  group_by(label) %>%
  summarise(cashIn = sum(cashIn),
            cashOut = sum(cashOut)) %>%
  ungroup() %>%
  mutate(delta = cashIn + cashOut)


monthly %>%
  filter(label != 'transit') %>%
  ggplot(aes(x = date, y = delta)) +
  geom_point()
