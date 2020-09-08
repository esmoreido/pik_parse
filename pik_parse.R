Sys.setlocale("LC_ALL","Russian")
# зависимости
requiredPackages = c('readxl','writexl','reshape2', 'lubridate', 'ggplot2', 'rvest', 'stringi', 'dplyr')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

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

stations_pik <- c(37001, 37018, 37099, 37107)

dates <- seq.Date(from = as.Date('2020-03-01'), to = as.Date('2020-08-01'), by = 'month')
all_df <- data.frame()

for(i in stations_pik){
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
save(all_df, file = 'all_df.RData')
write_xlsx(x = all_df, col_names = T, path = 'pik_out_3h.xlsx')

all_df$index <- as.factor(all_df$index)

p4 <- ggplot(melt(all_df, id.vars = c('index', 'date')), aes(x=index, y=value, col=index)) +
  stat_boxplot() +
  facet_grid(.~variable, scales = 'free_x', 
             labeller = as_labeller(c('temp'=' Температура', 'prec'='Осадки', 'td'='Точка росы'))) + 
  coord_flip() + theme_light(base_size = 20) +
  labs(title='Распределение', x='Станция', y='Значение', col='Индекс')
p4
ggsave(plot = p4, filename = "pik_boxplot_2020.png", width = 12, height = 8, dpi = 300, device = 'png', limitsize = F)

pik_daily <- all_df %>%
  dplyr::group_by(day = as.Date(date), index) %>%
  dplyr::summarise(av_temp = mean(temp, na.rm = T),
                   sum_prec = sum(prec, na.rm = T),
                   av_td = mean(td, na.rm = T))
summary(pik_daily)

write_xlsx(x = pik_daily, col_names = T, path = 'pik_out_daily.xlsx')

pik_daily %>%
  reshape2::melt(id.vars=c('day', 'index')) %>%
  dplyr::group_by(variable) %>%
  dplyr::do(plots = ggplot(., aes(x=day, y=value, col=variable)) + labs(title = .$variable, x='', y='') +
              geom_line() + facet_grid(index~.) + scale_x_date(date_breaks = '1 month') + theme_light(base_size = 14) +
                theme(legend.position = 'NA') + ggsave(paste0(.$variable, '.png'), device = 'png'))

