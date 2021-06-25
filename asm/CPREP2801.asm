*          DATA SET CPREP2801  AT LEVEL 003 AS OF 09/01/00                      
*PHASE CP2801A                                                                  
         TITLE 'CPREP2801-CPP MARKET SUMMARY'                                   
         PRINT NOGEN                                                            
CP2801   CSECT                                                                  
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,77,REPORT                                                     
         SSPEC H4,103,PAGE                                                      
         SSPEC H1,41,C'CPP MARKET SUMMARY'                                      
         SSPEC H2,41,C'------------------'                                      
         SSPEC H09,1,C'MARKET RANK AND NAME'                                    
         SSPEC H10,1,C'--------------------'                                    
         SSPEC H10,33,C'GRP PPS  CPP  CPM'                                      
         SSPEC H09,33,C'-----------------'                                      
         SSPEC H10,53,C'GRP PPS  CPP  CPM'                                      
         SSPEC H09,53,C'-----------------'                                      
         SSPEC H10,73,C'GRP PPS  CPP  CPM'                                      
         SSPEC H09,33,C'-----------------'                                      
         SSPEC H10,93,C'GRP PPS  CPP  CPM'                                      
         SSPEC H09,93,C'-----------------'                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CPREP2801 09/01/00'                                      
         END                                                                    
