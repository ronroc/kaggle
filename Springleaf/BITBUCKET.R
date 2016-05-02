##IDENTIFY AND SEPERATE NUMERIC AND NON NUMERIC COLUMNS

training_num <- training[, sapply(training, is.numeric)] #CHECK WHETHER LAPPLY WORKS

training_char <- training[,sapply(training, is.character)]

cat("Number of Numerical columns :" , dim(training_num[2]), "Number of character columns :",
                                          
                                          dim(training_char[2])))

##LOOK INTO NUMERIC COLUMNS DF

##CHECK FOR UNIQUE AND LENGTH OF UNIQUE COLUMNS

num_unique <- str(lapply(training_num, unique), vec.len = 4) #TRY RUNNING WITHOUT STR

num_uniqueLEN <- sapply(training_num, length(unique)) #CHECK WITH LAPPLY

##DIG DEEPER INTO THE FIELDS

##DRAW A HISTOGRAM OF LENGTHS TO BETTER UNDERSTAND THE DATA

#######LOTS OF TO BE DONE WRK WITH DATA##################################


##LOOK INTO CHARACTER COLUMNS DF

char_unique <- str(lapply(training_char, unique), vec.len =4 )

char_uniqueLEN <- sapply(training_char, length(unique))

##DIG DEEPER INTO THE FIELDS

#REPLACING (-1, " ", []) WITH NA

training_char[training_char == -1] = NA

training_char[training_char == ""] = NA

training_char[training_char == "[]"] = NA

##SEPERATE OUT DATES AND TIMES INTO  DIFFERENT DFS

training_date <- training_char[, grep("JAN1|FEB1|MAR1", training_char),]

##REMOVE DATES FROM char DF

training_charD <- training_char[, !(names(training_char) %in% names(training_date))] 
#CHECK THE DISSERENCE BETWEEN colnames and names

training_date