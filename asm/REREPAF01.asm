*          DATA SET REREPAF01  AT LEVEL 041 AS OF 09/12/89                      
*PHASE RE1501A,+0                                                               
         TITLE 'SPECS FOR KING REPORT'                                          
RE1501   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 1,2,3,4,5,6,7,8                                                  
         ASPEC H1,2,REP                                                         
         ASPEC H2,2,RUN                                                         
         ASPEC H1,79,RENUM                                                      
         ASPEC H1,92,PAGE                                                       
         ASPEC H2,79,REQUESTOR                                                  
         ASPEC H3,2,C'STATION'                                                  
         ASPEC H1,37,C'ADVERTISER FORECAST REPORT'                              
         ASPEC H2,37,C'--------------------------'                              
         ASPEC H3,39,C'PERIOD'                                                  
*                                                                               
         SPROG 1,2,3,4                                                          
         ASPEC H5,2,C'RANK'                                                     
         ASPEC H6,2,C'THIS'                                                     
         ASPEC H7,2,C'YEAR'                                                     
         ASPEC H8,2,C'----'                                                     
         ASPEC H5,7,C'RANK'                                                     
         ASPEC H6,7,C'LAST'                                                     
         ASPEC H7,7,C'YEAR'                                                     
         ASPEC H8,7,C'----'                                                     
         ASPEC H7,12,C'ADVERTISER'                                              
         ASPEC H8,12,C'--------------------'                                    
         ASPEC H7,33,C'OFFICE'                                                  
         ASPEC H8,33,C'--------------------'                                    
         ASPEC H6,65,C'PRIOR'                                                   
         ASPEC H7,65,C'FINAL'                                                   
         ASPEC H8,65,C'----------'                                              
         ASPEC H6,76,C'CURRENT'                                                 
         ASPEC H7,76,C'PACING'                                                  
         ASPEC H8,76,C'----------'                                              
         ASPEC H6,87,C'STATION'                                                 
         ASPEC H7,87,C'FORECAST'                                                
         ASPEC H8,87,C'----------'                                              
         SPROG 2                                                                
         ASPEC H7,33,C'ADVERTISER CODE'                                         
         ASPEC H8,33,C'--------------------'                                    
         SPROG 3                                                                
         ASPEC H7,98,C'INDEX'                                                   
         ASPEC H8,98,C'-----'                                                   
         SPROG 4                                                                
         ASPEC H7,33,C'ADVERTISER CODE'                                         
         ASPEC H8,33,C'--------------------'                                    
         ASPEC H7,98,C'INDEX'                                                   
         ASPEC H8,98,C'-----'                                                   
*** HEADINGS FROM HERE ON --FOR SHARE REPORT OPTION                             
         SPROG 5,6,7,8                                                          
         ASPEC H5,2,C'RANK'                                                     
         ASPEC H6,2,C'THIS'                                                     
         ASPEC H7,2,C'YEAR'                                                     
         ASPEC H8,2,C'----'                                                     
         ASPEC H5,7,C'RANK'                                                     
         ASPEC H6,7,C'LAST'                                                     
         ASPEC H7,7,C'YEAR'                                                     
         ASPEC H8,7,C'----'                                                     
         ASPEC H7,12,C'ADVERTISER'                                              
         ASPEC H8,12,C'--------------------'                                    
         ASPEC H7,33,C'OFFICE'                                                  
         ASPEC H8,33,C'--------------------'                                    
         ASPEC H6,65,C'PRIOR'                                                   
         ASPEC H7,65,C'FINAL'                                                   
         ASPEC H8,65,C'----------'                                              
         ASPEC H7,77,C'SHARE'                                                   
         ASPEC H8,77,C'-----'                                                   
         ASPEC H6,83,C'CURRENT'                                                 
         ASPEC H7,83,C'PACING'                                                  
         ASPEC H8,83,C'----------'                                              
         ASPEC H6,94,C'    STATION    MARKET    '                               
         ASPEC H7,94,C'SHR FORECAST   FORECAST  '                               
         ASPEC H8,94,C'--- ---------- ----------'                               
         SPROG 6                                                                
         ASPEC H7,33,C'ADVERTISER CODE'                                         
         ASPEC H8,33,C'--------------------'                                    
         SPROG 7                                                                
         ASPEC H7,120,C'INDEX'                                                  
         ASPEC H8,120,C'-----'                                                  
         SPROG 8                                                                
         ASPEC H7,33,C'ADVERTISER CODE'                                         
         ASPEC H8,33,C'--------------------'                                    
         ASPEC H7,120,C'INDEX'                                                  
         ASPEC H8,120,C'-----'                                                  
         DC    X'00'               END MARKER FOR ASPECS                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041REREPAF01 09/12/89'                                      
         END                                                                    
