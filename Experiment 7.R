# apply, lapply and sapply

my_list <- list(a = 1:5, b = 6:10)

lapply(my_list, mean)

df <- data.frame(x = 1:5, y = 6:10)

lapply(df, mean)

sapply(my_list, mean)
sapply(df, mean)

apply(df, 2, mean)

#function to summarize each column in a df 
column_summary <- function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
}
summary_results <-sapply(df, column_summary)
summary_results

# rbind and cbind
df <- data.frame(
  x = 1:3,
  y = c("a", "b", "c")
)

new_row <- data.frame(
  x = 4,
  y = "d"
)

df_expanded <- rbind(df, new_row)
df_expanded

new_col <- c("a", "b", "c")

df_expanded <- cbind(df, z = new_col)
df_expanded
