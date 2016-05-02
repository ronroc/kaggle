#USE  STANDARD TIME FORMAT DEFINED AT THE BEGINING

time <- Sys.time()

format(time, "%d-%m-%Y--%H:%M:%S") 

##CALCULATING DIMENSIONS OF DATA WITHOUT READING IN

##READ IN DATA
require(readr)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

dim(train); dim(test)

##REMOVE RESPONSWE AND ID VARIABLES

response <- train$target

train_ID <- train$ID

test_ID <- test$ID

##MODIFY train and test set

training <- subset(train, select = -c(ID, target))

testing <- subset(test, select = -c(ID))

dim(training); dim(testing)

##U CAN CHOOSE TO COMBINE THEM FOR ANY MODS OR ANALYSE THEM SEPERATELY

##THIS CURRENT DOCUMENT CONTAINS SEPERATE ANALYSIS FOR BOTH TEST AND TRAIN

##CHECK FOR DUPLICATE ROWS AND FIND THE NUMBER OF UNIQUE VALUES IN COLUMNS 

##CHECKING FOR DUPLICATE ROWS

nrow(training) - nrow(unique(training)) #DONT RUN TAKES RIDICULOUSLY LONG TIME

##CHECKING FOR UNIQUE ELEMENTS IN A COLUMN

col_unique <- sapply(training, function(x) unique(x))

##DONT RUN MIGHT BE TO USEFUL AFTER SUBSETTING

##CHECKING FOR NUMBER OF UNIQUE VALUES IN A COLUMN

col_ele <- as.data.frame(lapply(training, function(x) length(unique(x))))

##GET THE COLUMN NAMES

col_eleNAMES <- names(col_ele)

##CHECK FOR COLUMNS WITH 1,2,3 UNIQUE ELEMENTS

length(col_ele[col_ele == seq(1,3,1)])

length(col_ele[col_ele == 1])

length(col_ele[col_ele == 2]) #MOSTLY COLUMNS WITH DUMMY VARS

length(col_ele[col_ele == 3])

##CHECK COLUMNS WITH ONLY 1 UNIQUE VALUE

unique_one <- subset(col_ele , select = c(col_ele == 1))
 
unique_oneDF <- training[, c(names(unique_one))]

head(unique_oneDF)

##IDENTIFY AND SEPERATE NUMERIC AND NON NUMERIC COLUMNS

training_num <- training[, sapply(training, is.numeric)] #CHECK WHETHER LAPPLY WORKS

training_char <- training[,sapply(training, is.character)]

cat("Number of Numerical columns :" , dim(training_num)[2], "|Number of character columns :",
    
    dim(training_char)[2])

##LOOK INTO NUMERIC COLUMNS DF

##CHECK FOR UNIQUE AND LENGTH OF UNIQUE COLUMNS

str(lapply(training_num, unique)) #TRY RUNNING WITHOUT STR

num_uniqueLEN <- lapply(training_num, function(x) length(unique(x))) #CHECK WITH LAPPLY

##ALWAYS USE LAPPLY WHEN SUBSETTING AS IT HELPS IN BETTER O/P VIEW
##################################################################################################

numeric_ele <- (lapply(training_num, function(x) length(unique(x))))

##CHECK FOR COLUMNS WITH 1,2,3 UNIQUE ELEMENTS

length(numeric_ele[numeric_ele == 1])

length(numeric_ele[numeric_ele == 2]) #MOSTLY COLUMNS WITH DUMMY VARS

length(numeric_ele[numeric_ele == 3])

##CHECK COLUMNS WITH ONLY 1 UNIQUE VALUE

numeric_one <- subset(numeric_ele , subset  = c(numeric_ele == 1))

numeric_oneDF <- training_num[, c(names(numeric_one))]

##CHECK ELEMENTS

lapply(numeric_oneDF, table)

##CHECK COLUMNS WITH 2 UNIQUE VALUES

numeric_two <- subset(numeric_ele , subset = c(numeric_ele == 2))

numeric_twoDF <- training_num[, c(names(numeric_two))]

##CHECK ELEMENTS

lapply(numeric_twoDF, table)

##DIG DEEPER INTO THE FIELDS

##DRAW A HISTOGRAM OF LENGTHS TO BETTER UNDERSTAND THE DATA

##LOOK INTO CHARACTER COLUMNS DF

str(lapply(training_char, unique), vec.len =4 )

char_ele <- (lapply(training_char, function(x) length(unique(x))))

##################################################################################
##CHECK FOR COLUMNS WITH 1,2,3 UNIQUE ELEMENTS

range(char_ele)

length(char_ele[char_ele == 1])

length(char_ele[char_ele == 2]) #MOSTLY COLUMNS WITH DUMMY VARS

length(char_ele[char_ele == 3])

##SOME COLUMNS HAVE NAMES: FIRST SEPERATE DATES THEN LOOK INTO THEM 

##SEPERATE OUT DATES AND TIMES INTO  DIFFERENT DFS

training_date <- training_char[, grep("JAN1|FEB1|MAR1", training_char)]
##TAKES A LOT OF TIME TO RUN

##REMOVE DATES FROM char DF

training_charD <- training_char[, !(names(training_char) %in% names(training_date))] 

##training_charD has only CHAR variables checking further

str(lapply(training_charD, unique), vec.len =4 )

##SEPERATE FIELDS WITH BINARY VALUES

charD_ele <- (lapply(training_charD, function(x) length(unique(x))))

charD_two <- subset(charD_ele, subset = c(charD_ele == 2))
names(charD_two)

charD_twoDF <- training_char[, c(names(charD_two))]

##CHECK ELEMENTS OF DF

lapply(charD_twoDF, table)

##DRILL DOWN FURTHER BY REMOVING THESE COLS

training_charD_edit <- training_charD[, !(names(training_charD) %in% names(charD_two))] 

##training_charD_edit has only CHAR variables checking further

str(lapply(training_charD_edit, unique), vec.len = 4)

##FURTHER DRILL DOWN BY SEPERATING NAMES

charD_many <- subset(charD_ele, subset = c(charD_ele == 1824 | 
                                             charD_ele == 609 | charD_ele == 12387))
names(charD_many)

##PLACE EACH OF THE NAMES IN A INDI.. DF AND CHECK IT OUT

charDF <- data.frame(cities = training_charD_edit$VAR_0200)
View(charDF)

nrow(na.omit(charDF))

charDF1 <- data.frame(small_work = training_charD_edit$VAR_0404)

length(charDF1[charDF1 != -1])

##REMOVING NA`S FROM CHARDF1

no_NA1 <- charDF1[charDF1 != -1]

no_NA1 <- no_NA1[no_NA1 != ""]

no_NA1 <- no_NA1[no_NA1 != "CONTACT"]

no_NA1 <- no_NA1[no_NA1 != "CONTA"]

NA1 <- (as.data.frame(table(no_NA1)))

charDF2 <- data.frame(big_work = training_charD_edit$VAR_0493)

length(charDF2[charDF2 != -1])

nrow(charDF2) - length(charDF2[charDF2 != -1])

no_NA2 <- charDF2[charDF2 != -1]

no_NA2 <- no_NA2[no_NA2 != ""]

NA2 <- (as.data.frame(table(no_NA2)))

##################################################################################

#INTERSTING TWO DATASETS

write_csv(NA2, "BIG_WORK.csv")

write_csv(NA1, "SMALL_WORK.csv")

##FEATURE - GROUPING DATA INTO -- STILL WORKING ON IT###############################

##DRILL DOWN INTO DATES VALUES

names(training_date) 

training_date <- sapply(training_date, function(x) strptime(x, "%d%B%y :%H:%M:%S"))

training_date = do.call(cbind.data.frame, training_date)

##DRILL DOWN TIME VALUES

training_time <- training_date[, names(training_date) %in% c("VAR_0204","VAR_0217")]

training_time <- data.frame(sapply(training_time, function(x) strftime(x, "%H:%M:%S")))

training_hour <- as.data.frame(sapply(training_time, function(x) as.numeric(as.character(substr(x, 1,2)))))

training_minute <- as.data.frame(sapply(training_time, function(x) as.numeric(as.character(substr(x, 4,5)))))







#REPLACING (-1, " ", []) WITH NA

training_char[training_char == -1] = NA

training_char[training_char == ""] = NA

training_char[training_char == "[]"] = NA

