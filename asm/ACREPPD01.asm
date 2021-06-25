*          DATA SET ACREPPD01  AT LEVEL 005 AS OF 07/05/07                      
*PHASE ACPD01A                                                                  
         TITLE 'SPECS FOR ORDER RECORD LIST/DELETE/RESTORE'                     
ACPD01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ORDERS                                                      
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF H1,2,RUN                                                         
         ACDEF H1,109,REPORT                                                    
         ACDEF H1,123,PAGE                                                      
         ACDEF H2,109,REQUESTOR                                                 
         ACDEF H3,2,COMPANY                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPPD01 07/05/07'                                      
         END                                                                    
