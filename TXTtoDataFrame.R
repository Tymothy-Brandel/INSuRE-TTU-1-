#Install necessary packages
#C:/Users/djnin/OneDrive/Desktop/School/TTU Spring 2021/CSC 4575/INSURE Project/Traceroute Program
install.packages("stringr")
library("stringr")

#establish connection to data file
con = file("analysis_dump.txt", "r")
trace_table = list()

#skips filler at the beginning of the file
for (i in 1:96) {
  line = readLines(con, n = 1)
}

#loop through entire txt file and read in each traceroute call
num_rows = 1;
while ( TRUE) {
  line = readLines(con, n = 1)
  
  if ( length(line) == 0 ) {
    break
  }
  
  break_line = str_split(line, "\t")
  test_row = c(break_line[[1]][2], break_line[[1]][3],break_line[[1]][7],break_line[[1]][8],
               break_line[[1]][9],break_line[[1]][10],break_line[[1]][11],break_line[[1]][12],
               break_line[[1]][13])
  
  #this loop stores each individual hop for a particular traceroute call
  for (i in 14:length(break_line[[1]])) {
    test_hop = str_split(break_line[[1]][i],",")
    test_row = c(test_row,test_hop[[1]][1])
  }
  
  trace_table[[num_rows]] = test_row
  num_rows = num_rows + 1
}
close(con)