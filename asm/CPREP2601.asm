*          DATA SET CPREP2601  AT LEVEL 007 AS OF 09/01/00                      
*PHASE CP2601A                                                                  
         TITLE 'CPREP2601-CPP MARKET FOCUS REPORT'                              
         PRINT NOGEN                                                            
CP2601   CSECT                                                                  
         SPROG 0,THRU,8                                                         
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,77,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,77,AGYADD                                                     
         PSPEC H4,41,PERIOD                                                     
         PSPEC H4,77,REPORT                                                     
         PSPEC H4,103,PAGE                                                      
         PSPEC H4,41,PERIOD                                                     
         PSPEC H1,41,C'CPP MARKET FOCUS REPORT'                                 
         PSPEC H2,41,C'-----------------------'                                 
         PSPEC H9,1,C'TARGET'                                                   
         PSPEC H10,1,C'------'                                                  
         PSPEC H9,9,C'CLIENT'                                                   
         PSPEC H10,9,C'------'                                                  
         PSPEC H10,33,C'GRP PPS  CPP  CPM'                                      
         PSPEC H09,33,C'-----------------'                                      
         PSPEC H10,53,C'GRP PPS  CPP  CPM'                                      
         PSPEC H09,53,C'-----------------'                                      
         PSPEC H10,73,C'GRP PPS  CPP  CPM'                                      
         PSPEC H09,73,C'-----------------'                                      
         PSPEC H10,93,C'GRP PPS  CPP  CPM'                                      
         PSPEC H09,93,C'-----------------'                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CPREP2601 09/01/00'                                      
         END                                                                    
