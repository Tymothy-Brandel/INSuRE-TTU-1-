#Install necessary packages
install.packages("stringr")
library("stringr")

dirty_data = read.table("TymsOutput.csv",header=TRUE, sep=",")

#create a list that will store a vector for each traceroute call
#the vectors will contain the source ip, dest ip, and each hop ip
table = list()
num_rows = 1

#loops through and adds each hop ip to the vector for that traceroute call
for (i in 1:nrow(dirty_data)) {
  
  row_i = str_split(dirty_data[i,1], ";")
  
  #re-initialize the new row for the first traceroute call
  if (i == 1) {
    new_row = c(row_i[[1]][4], row_i[[1]][5])
  }
  
  #check to see if it is the beginning of a new traceroute call
  if (row_i[[1]][1] == "version") {
    #once all hops are added, add that traceroute call to the list
    table[[num_rows]] = new_row
    num_rows = num_rows + 1

    #store the source and dest ips for the next traceroute call in the new row vector
    next_row = str_split(dirty_data[i+1,1], ";")
    new_row = c(next_row[[1]][4], next_row[[1]][5])
    next #skips to the next traceroute call
  }

  new_row = c(new_row,row_i[[1]][9])
}
#add the last traceroute call into the table since the loop exits before it is added
table[[num_rows]] = new_row
num_rows = num_rows + 1