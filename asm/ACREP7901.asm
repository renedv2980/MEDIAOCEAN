*          DATA SET ACREP7901  AT LEVEL 005 AS OF 08/16/00                      
*PHASE AC7901A                                                                  
         TITLE 'SPECS FOR ANALYSIS CODE LISTING'                                
AC7901   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ANALYSIS                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,48,C'ANALYSIS CODE LISTING'                                   
         ASPEC H2,48,21C'-'                                                     
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H3,86,REQUESTOR                                                  
         ASPEC H3,2,COMPANY                                                     
         ASPEC H4,2,C'UNIT'                                                     
         ASPEC H5,2,C'LEDGER'                                                   
         ASPEC H7,2,C'CODE       DESCRIPTION'                                   
         ASPEC H8,2,C'----       -----------'                                   
         ASPEC H7,42,C'CODE       DESCRIPTION'                                  
         ASPEC H8,42,C'----       -----------'                                  
         ASPEC H7,82,C'CODE       DESCRIPTION'                                  
         ASPEC H8,82,C'----       -----------'                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREP7901 08/16/00'                                      
         END                                                                    
