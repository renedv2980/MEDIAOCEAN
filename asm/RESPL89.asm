*          DATA SET RESPL89    AT LEVEL 002 AS OF 11/08/90                      
*PHASE T80889A,*                                                                
         TITLE 'T80889 - RESPL89 - SPECS FOR O/N DELETE'                        
T80889   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2                                                            
         PSPEC H01,001,AGYNAME                                                  
         PSPEC H01,045,C'INVENTORY DELETION'                                    
         PSPEC H01,085,REPORT                                                   
         PSPEC H01,101,PAGE                                                     
         PSPEC H02,001,REQUESTOR                                                
         PSPEC H02,045,18C'-'                                                   
         PSPEC H02,085,RUN                                                      
         PSPEC H04,001,C'STATION -'                                             
         SPROG 1                                                                
         PSPEC H04,075,C'MARKET/STATION TEXT DELETE'                            
         PSPEC H07,001,C'RECORD TYPE   DELETION DETAILS'                        
         PSPEC H08,001,C'-----------   ----------------'                        
         SPROG 2                                                                
         PSPEC H04,075,C'DAYPART -'                                             
         PSPEC H07,001,C'DAY      TIME'                                         
         PSPEC H08,001,C'---      ----'                                         
         PSPEC H07,023,C'PROGRAMMING'                                           
         PSPEC H08,023,11C'-'                                                   
         PSPEC H07,051,C'INV.   START     END    DELETION DETAILS'              
         PSPEC H08,051,C'NUM.    DATE     DATE   ----------------'              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002RESPL89   11/08/90'                                      
         END                                                                    
