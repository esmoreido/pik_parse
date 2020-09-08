Sys.setlocale("LC_ALL","Russian")
# зависимости
library(readxl)     
library(writexl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(rvest)
library(stringi)
library(dplyr)
setwd("d:/EcoMeteo/sochi/pik")

parse_pik <- function(url, ind, yr){
  tables <- html_nodes(read_html(url), "table")  # парсим по тэгу
  tbl1 <- html_table(tables[[1]], header = F) # список из двух таблиц, в одной - сроки и дата, во второй - данные ??\_(???)_/??
  tbl2 <- html_table(tables[[2]], header = F) 
  
  tbl1 <- tbl1[-1,] # избавляемся от ненужных хэдеров
  tbl2 <- tbl2[-1,]
  
  # записываем в фрейм
  df <- data.frame(index = ind, 
                   time = as.numeric(tbl1$X1), 
                   daymon = as.character(tbl1$X2), 
                   temp = as.numeric(tbl2$X6), 
                   prec = as.numeric(tbl2$X16), 
                   td = as.numeric(tbl2$X7))
  
  df$month <- as.numeric(substr(df$daymon, start = stri_locate_first(df$daymon, fixed = ".") + 1, 
                                stop = stri_locate_first(df$daymon, fixed = ".") + 3)) # делаем месяц
  df$day <- as.numeric(substr(df$daymon, start = 1, stop = stri_locate_first(df$daymon, fixed = ".") - 1)) # делаем день
  
  df$date <- make_datetime(year = yr, month = df$month, day = df$day, hour = df$time) # делаем дату
  df <- df[,c(1,9,4,5,6)] # оставляем только нужные столбцы
  # print(head(df))
  return(df)
}

# stations_pik <- c(37001, 37018, 37099, 37107)
stations_pik <- read_xls('d:/EcoMeteo/sochi/List_meteo_Kub2020.xls')

dates <- seq.Date(from = as.Date('2020-03-01'), to = as.Date('2020-08-01'), by = 'month')
all_df <- data.frame()

for(i in stations_pik$SynopN){
  for(d in dates){
    d <- as.Date(d, origin = '1970-01-01')
    u <- paste0('http://www.pogodaiklimat.ru/weather.php?id=', i, '&bday=',day(d),'&fday=',days_in_month(d),'&amonth=',month(d),'&ayear=',year(d),'&bot=2')
    # print(u)
    pik_one <- tryCatch({
        print(paste(u, " parsed."))
        parse_pik(u, i, year(d))
        },
      error = function(e) {
        print(e)
        print(paste(u, " is empty."))
        return(NULL)
        }, 
      warning=function(w) {
        print(w)
        return(NULL)
      })
    # тормозим выполнение цикла на случайное число для имитации человека
    all_df <- rbind(all_df, pik_one)
    timeout <- sample(1:6, 1)
    print(paste("Timeout: ", timeout, " s", sep = " "))
    Sys.sleep(timeout) 
  }
  fn <- paste('arch', i, 'all_pik.xlsx', sep = '_')
  write_xlsx(all_df, path = fn, col_names = T)
  save(all_df, file = 'pik_sochi_large.RData')
}
summary(all_df)
# all_df <- subset(all_df, subset = index != 30317)
# df <- read_xlsx(path = 'pik_bratskoe_archive.xlsx')

all_df$index <- as.factor(all_df$index)

# температура
p1 <- all_df %>%
  dplyr::select(index, date, temp) %>%
  dplyr::group_by(index, mn = floor_date(date, 'month')) %>%
  dplyr::summarise(num = n()) %>%
  ggplot(., aes(x=mn, y=index, fill=num)) + geom_tile() + 
  geom_text(aes(label=num), col='White', size=1.5) +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%m.%y') + 
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size = 12)) +
  scale_y_discrete(labels = stations_pik$Name) + 
  labs(title='Температура', x='Дата', y='Станция', fill='Кол-во данных') +
  scale_fill_gradient(low = "white", high = "red")
p1
ggsave(plot = p1, filename = "pik_temp_numdays_2020.png", width = 16, height = 12, dpi = 300, device = 'png', limitsize = F)

# осадки
p2 <- all_df %>%
  dplyr::select(index, date, prec) %>%
  dplyr::group_by(index, mn = floor_date(date, 'month')) %>%
  dplyr::summarise(num = n()) %>%
  ggplot(., aes(x=mn, y=index, fill=num)) + geom_tile() + 
  geom_text(aes(label=num), col='White', size=1.5) +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%m.%y') + 
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size = 12)) +
  scale_y_discrete(labels = stations_pik$Name) + 
  labs(title='Осадки', x='Дата', y='Станция', fill='Кол-во дней') +
  scale_fill_gradient(low = "white", high = "Blue")
p2
ggsave(plot = p2, filename = "pik_prec_numdays_2020.png", width = 16, height = 12, dpi = 300, device = 'png', limitsize = F)

# точка росы
p3 <- all_df %>%
  dplyr::select(index, date, td) %>%
  dplyr::group_by(index, mn = floor_date(date, 'month')) %>%
  dplyr::summarise(num = n()) %>%
  ggplot(., aes(x=mn, y=index, fill=num)) + geom_tile() + 
  geom_text(aes(label=num), col='White', size=1.5) +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%m.%y') + 
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size = 12)) +
  scale_y_discrete(labels = stations_pik$Name) + 
  labs(title='Точка росы', x='Дата', y='Станция', fill='Кол-во дней') +
  scale_fill_gradient(low = "white", high = "Darkgreen")
p3
ggsave(plot = p3, filename = "pik_td_numdays_2020.png", width = 16, height = 12, dpi = 300, device = 'png', limitsize = F)


p4 <- ggplot(melt(all_df, id.vars = c('index', 'date')), aes(x=index, y=value, col=index)) +
  geom_boxplot() + scale_x_discrete(labels = stations_pik$Name) + 
  facet_grid(.~variable, scales = 'free_x', 
             labeller = as_labeller(c('temp'=' Температура', 'prec'='Осадки', 'td'='Точка росы'))) + 
  coord_flip() +
  labs(title='Распределение', x='Станция', y='Значение', col='Индекс')
p4
ggsave(plot = p4, filename = "pik_boxplot_2020.png", width = 16, height = 12, dpi = 300, device = 'png', limitsize = F)

#all_df$day <- as.Date(all_df$date)


# дефицит
all_df$es <- 6.1078 * exp((17.269388 * all_df$temp)/(all_df$temp + 237.3))
all_df$ea <- 6.1078 * exp((17.269388 * all_df$td)/(all_df$td + 237.3))
all_df$def <- all_df$es - all_df$ea
all_df$def[all_df$def < 0] <- 0

pik_daily <- all_df %>%
  dplyr::group_by(date, index) %>%
  dplyr::summarise(av_temp = mean(temp, na.rm = T),
                   sum_prec = sum(prec, na.rm = T),
                   av_def = mean(def, na.rm = T))
summary(pik_daily)

# исправление ошибок в новый датафрейм
# осадки >100, t > 37, t < -55, d > 50
pik_clear <- pik_daily
pik_clear$av_temp[pik_clear$av_temp <= -55] <- NA
pik_clear$av_temp[pik_clear$av_temp >= 37] <- NA
pik_clear$sum_prec[pik_clear$sum_prec >= 120] <- NA
pik_clear$av_def[pik_clear$av_def >= 55] <- NA
summary(pik_clear)

write_xlsx(x = pik_clear, col_names = T, path = 'st20_2020.xlsx')

t1 <- ggplot(pik_clear[,1:3], aes(x=date, y=av_temp, col=index)) + geom_line() + geom_point() + 
  scale_x_datetime(date_breaks = '10 days',date_minor_breaks = "1 day", date_labels = "%m.%d") +
  labs(x='Дата', y='Температура воздуха, °С', col='Станция') + scale_y_continuous(breaks = seq(-20, 40, 2), limits = c(-20, 40))
t1
ggsave(plot = t1, filename = "pik_temp_aug2020.png", width = 16, height = 8, dpi = 150, device = 'png', limitsize = F)

pr1 <- ggplot(pik_clear[,c(1,2,4)], aes(x=date, y=sum_prec, fill=index)) + geom_bar(stat='identity', position = 'dodge') + 
  scale_x_datetime(date_breaks = '10 days',date_minor_breaks = "1 day", date_labels = "%m.%d") +
  labs(x='Дата', y='Сумма осадков, мм', fill='Станция') + scale_y_continuous(breaks = seq(0, 40, 2), limits = c(0, 40))
pr1
ggsave(plot = pr1, filename = "pik_prec_aug2020.png", width = 16, height = 8, dpi = 150, device = 'png', limitsize = F) 

d1 <- ggplot(pik_clear[,c(1,2,5)], aes(x=date, y=av_def, col=index)) + geom_line() + geom_point() +
  scale_x_datetime(date_breaks = '10 days',date_minor_breaks = "1 day", date_labels = "%m.%d") +
  labs(x='Дата', y='Дефицит влажности воздуха, мБар', col='Станция') 
d1
ggsave(plot = d1, filename = "pik_def_aug2020.png", width = 16, height = 8, dpi = 150, device = 'png', limitsize = F) 
