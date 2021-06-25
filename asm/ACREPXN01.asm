*          DATA SET ACREPXN01  AT LEVEL 011 AS OF 02/16/00                      
*PHASE ACXN01A,+0                                                               
         TITLE 'ACXN - FIX ACTIVITY DATE'                                       
ACXN01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF GETOPT,NO                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0,1                                                        
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         FSPEC UPDATE,ACCFILE                                                   
         ACDEF H1,2,RUN                                                         
         ACDEF H1,045,C'ACXN - ACTIVITY DATE FIX'                               
         ACDEF H1,090,C'FORMAT'                                                 
         ACDEF H2,090,PAGE                                                      
*                                                                               
         ACDEF SPROG,1                                                          
*        ACDEF H7,006,C'SZ'                                                     
         ACDEF H8,003,C'ACCOUNT'                                                
         ACDEF H7,019,C'CONTRA'                                                 
         ACDEF H8,018,C'ACCOUNT'                                                
         ACDEF H8,033,C'TYPE'                                                   
         ACDEF H8,044,C'AMOUNT'                                                 
         ACDEF H7,057,C'ACTIVITY'                                               
         ACDEF H8,059,C'DATE'                                                   
         ACDEF H8,068,C'MOA'                                                    
         ACDEF H7,077,C'NEW ACT'                                                
         ACDEF H8,079,C'DATE'                                                   
         ACDEF H8,092,C'TRN DATE'                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREPXN01 02/16/00'                                      
         END                                                                    
