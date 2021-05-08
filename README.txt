This Repo is to help document and store any code written during the INSuRE TTU Group 1 Project for INSuRE.
Written documentation is available on google drive: https://drive.google.com/drive/folders/107ORdnOIBxonueNnCU3dQeLspRyyI7Ib?usp=sharing
Original paper we looked at: https://www.cs.bu.edu/faculty/crovella/paper-archive/gi-interface-disambig.pdf
CAIDA Scamper: https://www.caida.org/catalog/software/scamper/
CAIDA Datasets: https://www.caida.org/catalog/datasets/overview/
Documentation on Clusting package used in R: https://cran.r-project.org/web/packages/cluster/cluster.pdf


Current Programs stored:

  C Tracert Program
  CSVtoDataFrame.R
  ProgramTest.R
  ClusteringTest.R
  TXTtoDataFrame.R
  CSVtoDataFrame.R
  
  Suggestions:
  
    Spend time finding a large data set with a ground truth avaiable, this was one of our major difficulties within the project. CAIDA is a great place to start looking for a large data set, however a ground truth is not available. If you can find a ground truth for this set however, it would be perfect for further research.
    
    Research industry tools already being used in this field. This will save time during the project.
    
    We tested our R program alot checking for any errors in how it runs, so we are confident it works. All it should need is a larger dataset as a final test.
    
    Once a Larger dataset is accquired look into testing it with the Scamper dealias tool. This will allow you to compare two or more methods easily to your results.
    
    Once this is done improvements can be made to the current project such as a new feature (probability measurement), or focus on adding an attribute from another method compared that should mesh well with the already incorperated features. This new attribute should bring something from another method that one of the current features does not already provide.
    
    Then use the previous dataset(s) to test your new method and measure its possible improvement over the previous.

    Within our R program we used the agnes method of clustering like in the paper. Within the Cluster package used is another similiar method that takes the same input called
    diana. This method is suppose to be better for larger datasets and should be used to see if it has an improvement over agnes.
