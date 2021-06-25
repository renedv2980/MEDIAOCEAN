*          DATA SET ACREP4101  AT LEVEL 002 AS OF 08/16/00                      
*PHASE AC4101A                                                                  
         TITLE 'SPECS FOR PROFILE EXCEPTION REPORT'                             
AC4101   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MODE,PROCLEV                                                     
         ACDEF READ,ACCOUNTS                                                    
         ACDEF H1,2,RUN                                                         
         ACDEF H1,37,C'PROFILE EXCEPTION REPORT'                                
         ACDEF H1,75,REPORT                                                     
         ACDEF H1,89,PAGE                                                       
         ACDEF H2,37,24C'-'                                                     
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,75,REQUESTOR                                                  
         ACDEF H6,2,C'CODE'                                                     
         ACDEF H7,2,C'----'                                                     
         ACDEF H6,17,C'NAME'                                                    
         ACDEF H7,17,C'----'                                                    
         ACDEF H6,38,C'RECEIVABLE'                                              
         ACDEF H7,41,C'CODE'                                                    
         ACDEF H6,51,C'COSTING'                                                 
         ACDEF H7,52,C'CODE'                                                    
         ACDEF H6,64,C'BILLING     ANALYSIS UNBILLABLE'                         
         ACDEF H7,64,C'  TYPE        UNIT   WORK-CODES'                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREP4101 08/16/00'                                      
         END                                                                    
