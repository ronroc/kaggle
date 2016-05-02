
require(readr)

# find a better way to unzip files over here

path = "D:\\kaggle\\PRUDENTIAL\\blend\\bag\\xgb\\test\\xgb\\"

input <- list.files(path)

for(i in 1:length(input)) {

assign(paste0("df_", i), 

read_csv(paste0(path,  input[i])))

}


# run a simple row average 

tmp <- cbind.data.frame(df_1[1], 
      df_2[1],
      df_3[1],
      df_4[1],
      df_5[1],
      df_6[1],
      df_7[1],
      df_8[1],
      df_9[1]
)

 QuoteConversion_Flag <- (rowMeans(tmp))

df <- data.frame(QuoteNumber = df_1$QuoteNumber, QuoteConversion_Flag = QuoteConversion_Flag)

names(df)

write_csv(tmp, "D:\\kaggle\\PRUDENTIAL\\blend\\bag\\xgb\\xgb_all_test_02142016.csv")

# now for the stacking part