*          DATA SET ACREP7001  AT LEVEL 037 AS OF 08/16/00                      
*PHASE AC7001A                                                                  
         PRINT NOGEN                                                            
         TITLE 'SPECS FOR AUTO-JOURNAL'                                         
AC7001   CSECT                                                                  
*                                                                               
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'USE AND SALES TAX RATE LISTING'                          
         ASPEC H1,81,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H2,40,30C'-'                                                     
         ASPEC H3,2,COMPANY                                                     
         ASPEC H6,5,C'LOCALITY'                                                 
         ASPEC H7,5,C'--------'                                                 
         ASPEC H6,16,C'NAME'                                                    
         ASPEC H7,16,C'--------------------'                                    
         ASPEC H6,41,C'EFFECTIVE'                                               
         ASPEC H7,41,C'DATE'                                                    
         ASPEC H6,55,C'RATE'                                                    
         ASPEC H7,55,C'------'                                                  
         ASPEC H6,67,C'CREDIT'                                                  
         ASPEC H7,67,C'ACCOUNT'                                                 
         ASPEC H6,83,C'ACCOUNT NAME'                                            
         ASPEC H7,83,C'--------------------'                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037ACREP7001 08/16/00'                                      
         END                                                                    
