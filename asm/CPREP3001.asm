*          DATA SET CPREP3001  AT LEVEL 007 AS OF 09/01/00                      
*PHASE CP3001A                                                                  
         TITLE 'CPREP3001-ONE SPOT PER MARKET SPECS'                            
         PRINT NOGEN                                                            
CP3001   CSECT                                                                  
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,77,REPORT                                                     
         SSPEC H4,103,PAGE                                                      
         SPROG 2                                                                
         SSPEC H1,37,C'CPP ONE SPOT PER MARKET SUMMARY'                         
         SSPEC H2,37,C'-------------------------------'                         
         SSPEC H8,1,C'MARKET'                                                   
         SSPEC H9,1,C'GROUP'                                                    
         SSPEC H9,9,C'MKT COST RTG   CPP  CPM'                                  
         SSPEC H8,9,C'-----------------------'                                  
         SSPEC H9,35,C'MKT COST RTG   CPP  CPM'                                 
         SSPEC H8,35,C'-----------------------'                                 
         SSPEC H9,61,C'MKT COST RTG   CPP  CPM'                                 
         SSPEC H8,61,C'-----------------------'                                 
         SSPEC H9,87,C'MKT COST RTG   CPP  CPM'                                 
         SSPEC H8,87,C'-----------------------'                                 
         SPROG 1                                                                
         SSPEC H1,37,C'CPP ONE SPOT PER MARKET DETAIL'                          
         SSPEC H2,37,C'------------------------------'                          
         SSPEC H9,1,C'MARKET RANK/NAME'                                         
         SSPEC H9,25,C'COST  RTG   CPP  CPM'                                    
         SSPEC H8,25,C'--------------------'                                    
         SSPEC H9,46,C'COST  RTG   CPP  CPM'                                    
         SSPEC H8,46,C'--------------------'                                    
         SSPEC H9,67,C'COST  RTG   CPP  CPM'                                    
         SSPEC H8,67,C'--------------------'                                    
         SSPEC H9,88,C'COST  RTG   CPP  CPM'                                    
         SSPEC H8,88,C'--------------------'                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CPREP3001 09/01/00'                                      
         END                                                                    
