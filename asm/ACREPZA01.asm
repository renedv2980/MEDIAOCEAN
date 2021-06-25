*          DATA SET ACREPZA01  AT LEVEL 045 AS OF 12/09/99                      
*PHASE ACZA01A,*                                                                
         TITLE 'ACZA01 - 48 TYPE TRN FIX'                                       
ACZA01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         FSPEC READ,TRANS                                                       
         ACDEF WIDTH,198                                                        
*                                                                               
         SPROG 0,1                                                              
         ACDEF H1,2,RUN                                                         
         ACDEF H1,88,REPORT                                                     
         ACDEF H1,102,PAGE                                                      
         ACDEF H3,88,REQUESTOR                                                  
         ACDEF H3,2,ORIGIN                                                      
*                                                                               
         SPROG 1                                                                
         ACDEF H1,45,C'FIX TYPE 48 TRANSACTIONS'                                
         ACDEF H2,45,C'------------------------'                                
         ACDEF H8,02,C'CMP C/U    ACCOUNT               NAME     '              
         ACDEF H9,02,C'--- ---    -------               ----     '              
         ACDEF H8,53,C'WC TYP  CONTRA A/C       DATE     ACT/DATE  OLD'         
         ACDEF H9,53,C'-- ---  ----------       ----     --------  ---'         
         ACDEF H8,103,C'REF'                                                    
         ACDEF H9,103,C'---'                                                    
         ACDEF H8,115,C'DESCRIPTION         DEX              AMOUNT'            
         ACDEF H9,115,C'-----------         ---              ------'            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045ACREPZA01 12/09/99'                                      
         END                                                                    
