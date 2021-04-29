#Install necessary packages
#C:/Users/djnin/OneDrive/Desktop/School/TTU Spring 2021/CSC 4575/INSURE Project/Traceroute Program
install.packages("stringr")
install.packages("cluster")
install.packages("factoextra")
install.packages("NbClust")
install.packages("dplyr")
library("stringr")
library(cluster)
library(factoextra)
library(NbClust)
library(dplyr)

data = list()
num_rows = 1;
num_files = 3

#loop can read from multiple files and all data is placed in the data list
for (i in 1:num_files) {

  file_name = paste("path",toString(i), ".txt", sep = "")
  
  #establish connection to data file
  con = file(file_name, "r")
  
  #skips filler at the beginning of the file
  for (i in 1:96) {
    line = readLines(con, n = 1)
  }
  
  #loop through entire txt file and read in each traceroute call
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
    
    data[[num_rows]] = test_row
    num_rows = num_rows + 1
  }
  close(con)

}

#create the table to store the "M1,M2...M5" values based on the data
#M1 = common IP subnet
#M2 = Out-Degree Match (hopcount <= 3)
#M3 = Out-Degree Match (hopcount <= 4)
#M4 = Hop Match (hopcount <= 3)
#M5 = Hop Match (hopcount <= 4)
Mtable = data.frame(ip1=character(),ip2=character(),M1=numeric(),
                    M2=numeric(),M3=numeric(),M4=numeric(),M5=numeric())


#loops through each set of trace route calls and compares the hops 
#between 2 calls to check for M1, M2, M3, M4, and M5
#for (i in 1:(nrow(data) - 1)) {
for (row1 in 1:length(data)) {
  
  for (row2 in 1:length(data)) {
    
    #don't compare a row to itself
    if (row1 == row2) {
      next
    }
  
    size_1 = length(data[[row1]])
    size_2 = length(data[[row2]])
    
    #don't process the traceroute call unless it has a minimum of 5 hops
    if (size_1 < 14 | size_2 < 14) {
      next
    }
    
    
    #compare every hop of the first row to every hop of the second
    #Can't compare the last 4 hops of any row because they won't have enough
    #remaining hops to calculate the M values
    for (x in 10: (size_1-4)) {
      
      for (y in 10:(size_2-4)) {
        
        #don't compare if the IPs are the same
        if (data[[row1]][x] == data[[row2]][y]) {
          next
        }
        
        temprow = c(data[[row1]][x], data[[row2]][y])
        
        #split the ips into pieces to compare subnets
        ip1 = str_split(data[[row1]][x], "\\.")
        ip2 = str_split(data[[row2]][y], "\\.")
        
        m1 = 0.0
        #this condition compares the IP subnet to see if the first three octets match
        #for example, 1.1.1.x matches 1.1.1.y
        if (ip1[[1]][1] == ip2[[1]][1] && ip1[[1]][2] == ip2[[1]][2] 
            && ip1[[1]][3] == ip2[[1]][3]) {
           m1 = 1.0# sets M1 value in temprow to 1.0
        }
        
        #compare the 4 hops after the current pair to check
        #for matches even if the hop count doesn't match
        m2 = 0.0
        m3 = 0.0
        
        for (a in 1:4) {
          for (b in 1:4) {
            
            if ((data[[row1]][x+a] == data[[row2]][y+b]) && a < 4 && b < 4) {
              m2 = m2 + (1/3)
              m3 = m3 + 0.25
            }
            
            else if (data[[row1]][x+a] == data[[row2]][y+b]) {
              m3 = m3 + 0.25
            }
            
          }
          
        }
        
        
        #compare the 4 sets of ips after the current pair to check for
        #matches but only if the hop count also matches
        m4 = 0.0
        m5 = 0.0
        for (j in 1:4) {
          if ((data[[row1]][x+j] == data[[row2]][y+j]) && j < 4) { #m4 only involves 3 pairs
            m4 = m4 + (1/3)
            m5 = m5 + 0.25
          }
          else if (data[[row1]][x+j] == data[[row2]][y+j]) { #includes the 4th pair for m5
            m5 = m5 + 0.25
          }
        }
        
        #dummy values for the other 4 M values
        temprow = c(temprow, m1, m2, m3, m4, m5)
        
        #add the temprow into the table
        Mtable[nrow(Mtable)+1,] = temprow
        
        
      }
      
    }

  }
}

#combine first two columns of the Mtable to get all IPs
ipList1 = Mtable[,1]
ipList2 = Mtable[,2]
all_ips = c(ipList1,ipList2)

#matrix will contain a row and column for each unique IP
rownames = unique(all_ips)
m_size = length(rownames)
m = matrix ( nrow = m_size, ncol = m_size, byrow = FALSE, dimnames = list(rownames, rownames))

#initialize matrix with zeros
for (i in 1:m_size) {
  for (j in 1:m_size) {
    m[i,j] = 0
  }
}


#initialize 5 graphs for kernel density distributions
#one graph for each of the features (M-values)

m1_kernel = density(as.numeric(Mtable[,3]))
m2_kernel = density(as.numeric(Mtable[,4]))
m3_kernel = density(as.numeric(Mtable[,5]))
m4_kernel = density(as.numeric(Mtable[,6]))
m5_kernel = density(as.numeric(Mtable[,7]))



#compute the alias likelihoods and store them in the matrix
for (i in 1:nrow(Mtable)) {
  likelihood = 0
  row = 0
  col = 0
  
  #get position in the matrix for the 2 IPs in a row of the Mtable
  for (j in 1:m_size) {
    if (rownames[j] == Mtable[i,1]) {
      row = j
    }
    if (rownames[j] == Mtable[i,2]) {
      col = j
    }
  }
  
  #for each M value, search the kernels x-values to find a match
  #and then store the corresponding y-value which is the probability
  #for that m value
  m1_value = Mtable[i,3]
  m1_prob = 0
  
  for (q in 1:length(m1_kernel$x)) {
    if (m1_kernel$x[q] > m1_value) {
      m1_prob = m1_kernel$y[q]
      break
    }
  }
  
  m2_value = Mtable[i,4]
  m2_prob = 0
  
  for (q in 1:length(m2_kernel$x)) {
    if (m2_kernel$x[q] > m2_value) {
      m2_prob = m2_kernel$y[q]
      break
    }
  }
  
  m3_value = Mtable[i,5]
  m3_prob = 0
  
  for (q in 1:length(m3_kernel$x)) {
    if (m3_kernel$x[q] > m3_value) {
      m3_prob = m3_kernel$y[q]
      break
    }
  }
  
  m4_value = Mtable[i,6]
  m4_prob = 0
  
  for (q in 1:length(m4_kernel$x)) {
    if (m4_kernel$x[q] > m4_value) {
      m4_prob = m4_kernel$y[q]
      break
    }
  }
  
  m5_value = Mtable[i,7]
  m5_prob = 0
  
  for (q in 1:length(m5_kernel$x)) {
    if (m5_kernel$x[q] > m5_value) {
      m5_prob = m5_kernel$y[q]
      break
    }
  }
  
  
  likelihood = m1_prob * m2_prob * m3_prob * m4_prob * m5_prob
  
  m[row,col] = likelihood
  
}


#run clustering algorithm on matrix
test = agnes(m, diss = inherits(x, "dist"), metric = "euclidian",
             stand = FALSE, method = "average", par.method,
             keep.diss = FALSE, keep.data = TRUE, trace.lev = 0)

m2 = as.data.frame(m)

# Silhouette method
sil = fviz_nbclust(m2, hcut, method = "silhouette", k.max=5)+
  labs(subtitle = "Silhouette method")

opt_clusters = 0

for (i in 1:nrow(sil$data)) {
  
  if (sil$data[i,2] == max(sil$data[,2])) {
    opt_clusters = sil$data[i,1]
  }
  
}

#extract optimal number of clusters
#opt_clusters = pull(elbow$layers[[3]]$data,xintercept)

#plot tree and draw rectangles
pltree(test)
rect.hclust(test, k = opt_clusters, border = 2:5)

#cut tree into clusters
clusters = cutree(as.hclust(test), k = opt_clusters)


cluster_list = list()
cluster_table = cbind(rownames, clusters)

#break the clusters up and store them in vectors
#each vector in the cluster_list is an individual cluster
for (i in 1:opt_clusters) {
  temp_list = vector()
  for (j in 1:m_size) {
    if (cluster_table[j,2] == i) {
      temp_list[length(temp_list)+1] = cluster_table[j,1]
    }
  }
  cluster_list[[length(cluster_list)+1]] = temp_list
}

