setwd("C:/Users/hcraig/Documents/swc-R/r-novice-inflammation")

# read in file
dat<- read.csv(file="data/inflammation-01.csv", header=FALSE)

#first row, all columns
patient_1<- dat[1,]

# max inflammation for patient 1
max(patient_1)

# max inflammation for patient 2
max(dat[2,])

# minimum inflammation for var 7
min(dat[,7])

#mean
average_day_inflammation<- apply(dat, 2, mean)
plot(average_day_inflammation)

#min
min_day_inflammation<- apply(dat, 2, min)
plot(min_day_inflammation)

#max
max_day_inflammation<- apply(dat, 2, max)
plot(max_day_inflammation)

#sd
sd_day_inflammation<- apply(dat, 2, sd)
plot(sd_day_inflammation)

#create a function

fahr_to_kelvin<- function(temp) {
  kelvin<- ((temp-32)* (5/9)) + 273.15
  return(kelvin)
}

#freezing point of water
fahr_to_kelvin(32)

#boiling point of water
fahr_to_kelvin(212)

#function Mik

mik<- function(x){
  y=x+2
  return(y)
}

# another function
kelvin_to_celcius<- function(temp){
  celcius<- temp - 273.15
  return(celcius)
}
kelvin_to_celcius(0)


best_practice <- c("Write", "programs", "for", "people", "not", "computers")
asterisk <- "***"  # R interprets a variable with a single value as a vector
# with one element.

fence<- function(a,b){
  y <- c(b,a,b)
  return(y)
}
fence(best_practice, asterisk)


##


analyze <- function(filename) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}


pdf(file="inflammation-01.pdf")
analyze("data//inflammation-01.csv")
dev.off()


best_practice
print_words<- function(sentence){
  print(sentence[1])
  print(sentence[2])
  print(sentence[3])
  print(sentence[4])
  print(sentence[5])
  print(sentence[6])
}

print_words(best_practice)

print_words<- function(sentence){
  for(i in 1:length(sentence)){
    print(sentence[i])
    print(i)
  }
}
print_words(best_practice)


##for the analyse function for multiple files

filenames<- list.files(path="data", pattern="inflammation")
setwd("data/")
for (file in filenames){
  pdf(file=paste0(file,".pdf"))
  analyze(file)
  dev.off()
  print(paste0(file,".pdf"))
}



print_n<- function(N){
  for(i in 1:N){
        print(i)
  }
}


print_n(20)
print_n(4)

vec<- c(4,8,15,16,23,42)

num<- 0
num_vec <- c(1,1)
 for (i in 1:10) {
   num<- num_vec [i]+num_vec[i+1]
   num_vec[i+2]<-num
   print(i)
   }

analyze_all <- function(pattern) {
  # Runs the function analyze for each file in the current working directory
  # that contains the given pattern.
  filenames <- list.files(path = "data", pattern = pattern, full.names = TRUE)
  for (f in filenames) {
    analyze(f)
  }
}

#if

num<- 37
if(num > 100){
  print("Greater") #if true runs this code
  } else{
    print("Not greater") #if false then runs this
  }
print("done") #runs for all conditions

sign<- function(num){
  if(num > 0){
    return(1)
  } else if (num == 0){
    return(0)
  } else {
    return(-1)
  }
}
sign(50)

# & and
# | or

(1 > 0 && -1 > 0)
(1 > 0 || -1 > 0)
(2 > 0 && -1 < 2)








