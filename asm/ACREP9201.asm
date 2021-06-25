*          DATA SET ACREP9201  AT LEVEL 004 AS OF 08/16/00                      
*PHASE AC9201A                                                                  
         TITLE 'SPECS FOR DIRECT TIME ANALYSIS'                                 
AC9201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H1,48,C'DIRECT TIME ANALYSIS'                                    
         ASPEC H2,48,C'--------------------'                                    
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC F1,2,REQDETS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREP9201 08/16/00'                                      
         END                                                                    
