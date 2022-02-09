## Author : Jyotishka Ray Choudhury
## Roll no : BS 1903

random.numbers <- function(){
  
  random.digits <- function(n){
    number <- sample(0:9 , size = n , replace = FALSE)
    if (number[1] == 0) {number[1] <- number[2]; number[2] <- 0}
    number <- as.numeric(paste(number, collapse = ""))
    return(number)
  }
  
  test.data <- c()
  for (i in 3:9) {test.data <- c(test.data , random.digits(i))}
  return(test.data)
}

cat("Starting the experiment...\n\n")
Sys.sleep(2)
cat("The digits of each number will appear one by one after an interval
    of 1 second. In order to proceed, you'll have to make the correct 
    guess. All the best !\n\n")

name <- readline(prompt = "Enter your name : ")
age <- readline(prompt = "Enter your age : ")
gender <- readline(prompt = "Enter your gender [M / F] : ")

if (gender != "M" && gender !="m" && gender != "F" && gender != "f"){
  print("Invalid gender input !")
}

trial <- function(){
  begin <- readline(prompt = "Ready to start the trial ? [ Y / N ] : ")
  cat("\f")
  
  if (begin == "Y" || begin == "y") {
    trial.data <- random.numbers()
    print("The trial will start in 2 seconds...")
    Sys.sleep(2)
    cat("\f")
    
    count <- 0
    
    for (num in trial.data) {
      z <- unlist(strsplit(as.character(num), ""))
      
      for (k in 1:length(z)) {
        print(z[k])
        Sys.sleep(1)
        cat("\f")
      }
      
      guess <- readline(prompt = "Guess the number : ")
      
      if (guess == num){ 
        if (num != trial.data[length(trial.data)]){
          print("Your guess is correct !")
          print("The next number will appear in 2 seconds...")
          Sys.sleep(2)
        }
        
        if (num == trial.data[length(trial.data)]) {
          cat("Your digit span is ",length(z),". Great work !\n\n")
          break
        }
      }
      
      cat("\f")
      
      if (guess != num){
        if (num != trial.data[length(trial.data)]) {
          cat("Sorry. Wrong guess !\n\n")
          cat("The acutal number was",num,". \n\n")
          Sys.sleep(3)
          cat("\f")
          count <- count + 1
          
          if (count == 2){
            cat("Your trial is over. Your number of right guesses is ",
                floor(log10(num)) - 3,".\n\n")
            cat("All the best !\n\n")
            return(floor(log10(num)) - 3)
          }
        }
        
        if (num == trial.data[length(trial.data)]){
          cat("Your trial is over. You guessed ",
              floor(log10(x)) - 1 - count," numbers correctly.\n\n")
          cat("All the best !\n\n")
          return(floor(log10(num)) - 1 - count)
        }
      }
    }
  }
}

final.exp <- function(d){
  begin <- readline(prompt = "Ready to start the test ? [Y / N] : ")
  cat("\f")
  
  if (begin == "Y" || begin == "y") {
    exp.data <- random.numbers()
    cat("Experiment number ",d," will start in 3 seconds...\n\n")
    Sys.sleep(3)
    cat("\f")
    
    count <- 0
    #    response.time <- c()
    for (num in exp.data) {
      z <- unlist(strsplit(as.character(num), ""))
      
      for (k in 1:length(z)) {
        print(z[k])
        Sys.sleep(1)
        cat("\f")
      }
      
      #      start.time <- Sys.time()
      guess <- readline(prompt = "Guess the number : ")
      #      end.time <- Sys.time()
      
      
      if (guess == num){ 
        if (num != exp.data[length(exp.data)]){
          print("Your guess is correct !")
          print("The next number will appear in 2 seconds...")
          Sys.sleep(2)
        }
        
        if (num == exp.data[length(exp.data)]) {
          cat("Your test #",d,"is over. You guessed ",
              floor(log10(num))-1-count,
              " numbers correctly.\n\n")
          return(floor(log10(num))+1)
        }
      }
      
      cat("\f")
      
      if (guess != num){
        if (num != exp.data[length(exp.data)]) {
          cat("Sorry. Wrong guess !\n\n")
          cat("The acutal number was",num,". \n\n")
          Sys.sleep(3)
          cat("\f")
          count <- count + 1
          
          if (count == 1){ first <- floor(log10(num)) + 1 }
          if (count == 2){
            second <- floor(log10(num)) + 1
            cat("Your test #",d,"is over. You guessed ",
                floor(log10(num))-3,
                " numbers correctly.\n\n")
            if (first != second - 1) {return(second - 1)}
            if (first == second - 1) {return(first - 1)}
          }
        }
        
        if (num == exp.data[length(exp.data)]){
          cat("Your test is over. You guessed ",
              floor(log10(num)) - 1 - count," numbers correctly.\n\n")
          if (first != second - 1) {return(second - 1)}
          if (first == second - 1) {return(first - 1)}
        }
      }
    }
  }
}

#trial()
trial <- final.exp(1)
F2 <- final.exp(2)
F3 <- final.exp(3)
dspan <- mean(c(F1,F2))

cat(name,"'s digit span is ",dspan,".\n\n")

result <- data.frame(NAME = name , AGE = age , GENDER = gender ,
                     TRIAL = trial , TEST_1 = F1 , TEST_2 = F2 , 
                     DIGIT_SPAN = dspan)
print(result)
View(result)
