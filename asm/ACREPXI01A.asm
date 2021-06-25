*          DATA SET ACREPXI01A AT LEVEL 009 AS OF 08/16/00                      
*PHASE ACXI01A,+0                                                               
         TITLE 'FIX TRANSACTION OFFICE'                                         
ACXI01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'FIX INPUT TYPE ONES '                                    
         ACDEF H2,38,C'--------------------'                                    
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREPXI01A08/16/00'                                      
         END                                                                    
