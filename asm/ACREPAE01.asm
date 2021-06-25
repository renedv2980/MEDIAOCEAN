*          DATA SET ACREPAE01  AT LEVEL 001 AS OF 10/12/01                      
*PHASE ACAE01A,+0                                                               
         TITLE 'ACAE - ACCOUNT EQUIVALENT POINTER FINDER'                       
ACAE01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
*                                                                               
         SPROG 1,2                                                              
         ACDEF H1,2,RUN                                                         
         ACDEF H1,88,REPORT                                                     
         ACDEF H1,102,PAGE                                                      
         ACDEF H3,2,ORIGIN                                                      
         ACDEF H3,88,REQUESTOR                                                  
         ACDEF H1,45,C'ACCOUNT EQUIVALENT POINTER FINDER'                       
         ACDEF H2,45,C'---------------------------------'                       
         ACDEF H8,2,C'CMP'                                                      
         ACDEF H10,2,C'---'                                                     
         ACDEF H8,7,C'U/L'                                                      
         ACDEF H10,7,C'---'                                                     
*                                                                               
         SPROG 1                                                                
         ACDEF H5,45,C'RULES AT LEDGER RECORD'                                  
         ACDEF H6,45,C'----------------------'                                  
         ACDEF H8,11,C'TOTAL '                                                  
         ACDEF H9,11,C'LENGTH'                                                  
         ACDEF H10,11,C'-----'                                                  
         ACDEF H8,19,C'NO. OF'                                                  
         ACDEF H9,19,C'LEVELS'                                                  
         ACDEF H10,19,C'-----'                                                  
         ACDEF H8,32,C'EQU ACCNT NAME'                                          
         ACDEF H10,32,C'--------------'                                         
         ACDEF H8,48,C'RULE  '                                                  
         ACDEF H10,48,C'-----'                                                  
*                                                                               
         SPROG 2                                                                
*        ACDEF H5,45,C'EQUIVALENT ACCOUNTS AT ACCOUNT SIDE   '                  
*        ACDEF H6,45,C'--------------------------------------'                  
         ACDEF H8,11,C'ACCOUNT CODE'                                            
         ACDEF H10,11,C'------------'                                           
         ACDEF H8,33,C'ACCOUNT NAME'                                            
         ACDEF H10,33,C'------------'                                           
         ACDEF H8,71,C'TOTAL '                                                  
         ACDEF H9,71,C'LENGTH'                                                  
         ACDEF H10,71,C'-----'                                                  
         ACDEF H8,78,C'EQUIVALENT ACCOUNT'                                      
         ACDEF H10,78,C'------------------'                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPAE01 10/12/01'                                      
         END                                                                    
