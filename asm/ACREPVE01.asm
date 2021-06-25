*          DATA SET ACREPVE01  AT LEVEL 001 AS OF 11/13/19                      
*PHASE ACVE01A,+0                                                               
         TITLE 'SPECS FOR ACCOUNT LISTING'                                      
ACVE01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF MODE,PROCLEV                                                     
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,49,C'VENDOR ENROLLMENT REPORT'                                
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H2,49,24C'-'                                                     
         ACDEF H4,86,REQUESTOR                                                  
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
*                                                                               
         ACDEF H8,2,C'VENDOR NAME'                                              
         ACDEF H8,39,C'VENDOR ID'                                               
         ACDEF H8,52,C'ME'                                                      
         ACDEF H8,55,C'ADDRESS LINE 1'                                          
         ACDEF H8,96,C'ADDRESS LINE 2'                                          
*                                                                               
         ACDEF H9,7,C'CITY'                                                     
         ACDEF H9,37,C'ST'                                                      
         ACDEF H9,40,C'ZIP'                                                     
         ACDEF H9,50,C'PHONE'                                                   
         ACDEF H9,62,C'MEMO'                                                    
*                                                                               
         ACDEF H10,12,C'CONTACT'                                                
         ACDEF H10,42,C'EMAIL'                                                  
         ACDEF H10,110,C'CC'                                                    
         ACDEF H10,113,C'CRE DT'                                                
         ACDEF H10,122,C'LST ACT'                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPVE01 11/13/19'                                      
         END                                                                    
