# --- 1. Подключение библиотек ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(quantmod, tidyverse, plotly, PerformanceAnalytics, scales)

# --- 2. Настройки симуляции ---
tickers <- c("MSFT", "GOOG", "NKE", "VZ") 
start_date <- "2019-01-01" 
end_date <- Sys.Date()
total_capital <- 12000     
risk_free_rate <- 0

# --- 3. Функция расчета стратегий и метрик ---
run_backtest_advanced <- function(ticker, start, end, capital) {
  # Загрузка данных
  tryCatch({
    data <- getSymbols(ticker, src = "yahoo", from = start, to = end, auto.assign = FALSE)
  }, error = function(e) return(NULL))
  
  # Месячные данные
  monthly_data <- to.monthly(data, indexAt = "lastof", OHLC = FALSE)
  df <- data.frame(Date = index(monthly_data), Price = as.numeric(Ad(monthly_data)))
  
  n_months <- nrow(df)
  dca_per_month <- capital / n_months 
  
  # --- Расчет Buy & Hold ---
  bnh_shares <- capital / df$Price[1]
  df$BnH_Value <- bnh_shares * df$Price
  
  # --- Расчет DCA ---
  # Мы моделируем, что кэш лежит "под подушкой" (0 доходность) и постепенно вливается
  df$Cash_Balance <- capital - (seq_len(n_months) * dca_per_month)
  # Корректировка для последнего месяца, чтобы не уйти в минус из-за округления
  df$Cash_Balance[df$Cash_Balance < 0] <- 0 
  
  df$Shares_Bought_Month <- dca_per_month / df$Price
  df$Total_Shares_DCA <- cumsum(df$Shares_Bought_Month)
  
  # Стоимость активов + Остаток кэша
  df$DCA_Equity_Only <- df$Total_Shares_DCA * df$Price
  df$DCA_Total_Value <- df$DCA_Equity_Only + df$Cash_Balance # Полная стоимость портфеля (Assets + Cash)
  
  df$Ticker <- ticker
  
  return(df)
}

# --- 4. Запуск и агрегация ---
results_list <- lapply(tickers, run_backtest_advanced, start = start_date, end = end_date, capital = total_capital)
full_data <- bind_rows(results_list)

# --- 5. Расчет метрик риска ---
# Пишем вспомогательную функцию для расчета метрик по вектору цен
calc_metrics <- function(prices) {
  # Доходности
  returns <- ROC(prices, type = "discrete", na.pad = FALSE)
  
  # Sharpe (Annualized)
  # mean(ret) / sd(ret) * sqrt(12)
  sharpe <- round(mean(returns, na.rm=TRUE) / sd(returns, na.rm=TRUE) * sqrt(12), 2)
  
  # Max Drawdown
  # 1 - (Price / RollingMax)
  cummax_val <- cummax(prices)
  drawdowns <- (prices - cummax_val) / cummax_val
  max_dd <- round(min(drawdowns, na.rm=TRUE) * 100, 2)
  
  return(list(Sharpe = sharpe, MaxDD = max_dd))
}

# Группируем и считаем метрики для каждого тикера
metrics_table <- full_data %>%
  group_by(Ticker) %>%
  summarise(
    # Финальные значения
    BnH_End_Value = round(last(BnH_Value), 0),
    DCA_End_Value = round(last(DCA_Total_Value), 0), # Используем Total Value (с кэшем) или Equity Only
    
    # Метрики BnH
    BnH_Sharpe = calc_metrics(BnH_Value)$Sharpe,
    BnH_MaxDD_Pct = calc_metrics(BnH_Value)$MaxDD,
    
    # Метрики DCA (Считаем по Equity Curve, включая кэш, так как это снижает волатильность)
    DCA_Sharpe = calc_metrics(DCA_Total_Value)$Sharpe,
    DCA_MaxDD_Pct = calc_metrics(DCA_Total_Value)$MaxDD
  ) %>%
  mutate(
    Strategy_Verdict = ifelse(BnH_End_Value > DCA_End_Value, "BnH Wins Profit", "DCA Wins Profit"),
    Risk_Verdict = ifelse(abs(DCA_MaxDD_Pct) < abs(BnH_MaxDD_Pct), "DCA Safer", "BnH Safer")
  )

print("--- Аналитическая таблица (Risk vs Reward) ---")
print(metrics_table)

# --- 6. Визуализация ---
# Подготовка данных для ggplot
plot_data <- full_data %>%
  select(Date, Ticker, BnH_Value, DCA_Total_Value) %>%
  pivot_longer(cols = c("BnH_Value", "DCA_Total_Value"), 
               names_to = "Strategy", values_to = "Value") %>%
  mutate(Strategy = recode(Strategy, 
                           "BnH_Value" = "Buy & Hold (Lump Sum)", 
                           "DCA_Total_Value" = "DCA (Equity + Cash)"))

# Создаем статический график ggplot
p <- ggplot(plot_data, aes(x = Date, y = Value, color = Strategy)) +
  geom_line(size = 1) +
  facet_wrap(~Ticker, scales = "free_y") + # Отдельный график для каждого тикера
  scale_color_manual(values = c("#FF5733", "#33C1FF")) +
  theme_minimal() +
  labs(title = "Сравнение стратегий: Динамика портфеля",
       y = "Стоимость портфеля ($)", x = "") +
  theme(legend.position = "bottom")

# Конвертируем в интерактивный Plotly
fig <- ggplotly(p) %>%
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2))

fig

# --- 1. Подготовка данных для красивой таблицы ---
pacman::p_load(reactable, htmltools)

final_table <- metrics_table %>%
  # Добавляем расчет доходности в % (уже рассчитано в предыдущем шаге, но для чистоты пересчитаем)
  mutate(
    BnH_Return_Pct = round((BnH_End_Value - total_capital) / total_capital * 100, 1),
    DCA_Return_Pct = round((DCA_End_Value - total_capital) / total_capital * 100, 1)
  ) %>%
  select(
    Ticker, 
    BnH_Return_Pct, BnH_Sharpe, BnH_MaxDD_Pct,
    DCA_Return_Pct, DCA_Sharpe, DCA_MaxDD_Pct,
    Strategy_Verdict
  )

# --- 2. Рендеринг красивой таблицы (Reactable) ---
# Эта таблица будет интерактивной, со встроенными цветовыми акцентами
render_table <- reactable(
  final_table,
  compact = TRUE,
  bordered = TRUE,
  striped = TRUE,
  highlight = TRUE,
  columns = list(
    Ticker = colDef(name = "Тикер", style = list(fontWeight = "bold")),
    BnH_Return_Pct = colDef(name = "B&H Доход, %", 
                            style = function(value) {
                              color <- if (value > 0) "#008000" else "#e00000"
                              list(color = color, fontWeight = "bold")
                            }),
    BnH_Sharpe = colDef(name = "B&H Sharpe"),
    BnH_MaxDD_Pct = colDef(name = "B&H MaxDD, %"),
    DCA_Return_Pct = colDef(name = "DCA Доход, %", 
                            style = function(value) {
                              color <- if (value > 0) "#008000" else "#e00000"
                              list(color = color, fontWeight = "bold")
                            }),
    DCA_Sharpe = colDef(name = "DCA Sharpe"),
    DCA_MaxDD_Pct = colDef(name = "DCA MaxDD, %"),
    Strategy_Verdict = colDef(name = "Результат", 
                              cell = function(value) {
                                class <- if (grepl("BnH", value)) "tag-bnh" else "tag-dca"
                                div(class = paste("tag", class), value)
                              })
  ),
  columnGroups = list(
    colGroup(name = "Buy & Hold (Lump Sum)", columns = c("BnH_Return_Pct", "BnH_Sharpe", "BnH_MaxDD_Pct")),
    colGroup(name = "Dollar Cost Averaging", columns = c("DCA_Return_Pct", "DCA_Sharpe", "DCA_MaxDD_Pct"))
  ),
  language = reactableLang(noData = "Данные не найдены"),
  theme = reactableTheme(
    headerStyle = list(backgroundColor = "#f7f7f8")
  )
)

# Вывод таблицы
render_table
