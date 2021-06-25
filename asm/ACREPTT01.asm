*          DATA SET ACREPTT01  AT LEVEL 017 AS OF 10/29/20                      
*PHASE ACTT01A,*                                                                
         TITLE 'ACTT01 - ACCPAK 1099 FORM SPECS'                                
ACTT01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         FSPEC READ,TRANS                                                       
*                                                                               
         SPROG 0,1,2,4,5,6                                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H1,88,REPORT                                                     
         ACDEF H1,102,PAGE                                                      
         ACDEF H3,88,REQUESTOR                                                  
         ACDEF H3,2,C'COMPANY'                                                  
*                                                                               
         SPROG 1,4                                                              
         ACDEF H1,45,C'1099 MISC REGISTER'                                      
         ACDEF H2,45,C'------------------'                                      
         SPROG 1,6                                                              
         ACDEF H8,02,C'TAX ID NUMBER'                                           
         ACDEF H8,18,C'NAME/ADDRESS'                                            
         ACDEF H8,64,C'AMOUNT'                                                  
         ACDEF H7,85,C'------ BREAKDOWN BY ------'                              
         ACDEF H8,85,C'ACCOUNT             AMOUNT'                              
         ACDEF H9,02,109C'-'                                                    
*                                                                               
         SPROG 2                                                                
         ACDEF H1,40,C'1099 LIST OF MISSING TIN NUMBERS'                        
         ACDEF H2,40,C'--------------------------------'                        
         ACDEF H8,02,C'TAX ID NUMBER'                                           
         ACDEF H8,18,C'NAME/ADDRESS'                                            
         ACDEF H8,64,C'AMOUNT'                                                  
         ACDEF H8,85,C'ACCOUNT'                                                 
         ACDEF H9,02,109C'-'                                                    
*                                                                               
         SPROG 3                                                                
         ACDEF H1,01,C'FORM 4804/4802'                                          
         ACDEF H1,51,C'TRANSMITTER CONTROL CODE='                               
         ACDEF H2,17,C'TRANSMITTAL OF INFORMATION RETURNS'                      
         ACDEF H3,17,C'REPORTED ON MAGNETIC MEDIA'                              
*                                                                               
         SPROG 5                                                                
         ACDEF H1,43,C'1099 INVALID ADDRESS LIST'                               
         ACDEF H2,43,C'-------------------------'                               
         ACDEF H8,02,C'ACCOUNT'                                                 
         ACDEF H9,02,C'-------'                                                 
         ACDEF H8,20,C'DESCRIPTION'                                             
         ACDEF H9,20,C'-----------'                                             
*                                                                               
         SPROG 6                                                                
         ACDEF H1,45,C'1099 NEC  REGISTER'                                      
         ACDEF H2,45,C'------------------'                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACREPTT01 10/29/20'                                      
         END                                                                    
