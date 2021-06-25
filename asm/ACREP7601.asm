*          DATA SET ACREP7601  AT LEVEL 004 AS OF 08/16/00                      
*PHASE AC7601A                                                                  
         TITLE 'SPECS FOR FILTER VALUES LISTING'                                
AC7601   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ASPEC H1,2,RUN                                                         
         ASPEC H1,47,C'FILTER VALUES LISTING'                                   
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H2,47,21C'-'                                                     
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC H6,2,C'UNIT/     FILTER    FILTER MEANING     FILTER'            
         ASPEC H6,51,C'MEANING OF FILTER VALUE'                                 
         ASPEC H7,2,C'LEDGER    NUMBER    --------------     VALUE '            
         ASPEC H7,51,23C'-'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREP7601 08/16/00'                                      
         END                                                                    
