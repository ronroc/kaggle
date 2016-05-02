
require(readr)

# find a better way to unzip files over here

input <- list.files(path = "D:\\kaggle\\HOMESITE\\ensemble\\csv")

for(i in 1:length(input))
{
  
  assign(paste0("df_", i),
         read_csv(paste0("D:\\kaggle\\HOMESITE\\ensemble\\csv\\", input[i])))
}


# run a simple row average 

tmp <- cbind.data.frame(df_1$QuoteConversion_Flag, 
      df_2$QuoteConversion_Flag,
      df_3$QuoteConversion_Flag,
      df_4$QuoteConversion_Flag,
      df_5$QuoteConversion_Flag,
      df_6$QuoteConversion_Flag,
      df_7$QuoteConversion_Flag,
      df_8$QuoteConversion_Flag,
      df_9$QuoteConversion_Flag,
      df_10$QuoteConversion_Flag,
      df_11$QuoteConversion_Flag,
      df_12$QuoteConversion_Flag,
      df_13$QuoteConversion_Flag,
      df_14$QuoteConversion_Flag,
      df_15$QuoteConversion_Flag,
      df_16$QuoteConversion_Flag,
      df_17$QuoteConversion_Flag,
      df_18$QuoteConversion_Flag
)



tmp <- cbind.data.frame(df_1$QuoteConversion_Flag, 
                        df_2$QuoteConversion_Flag,
                        df_3$QuoteConversion_Flag
                        )

 QuoteConversion_Flag <- (rowMeans(tmp))

df <- data.frame(QuoteNumber = df_1$QuoteNumber, QuoteConversion_Flag = QuoteConversion_Flag)

names(df)

write_csv(df, "df_average_all_02072016_2.csv")

# now for the stacking part