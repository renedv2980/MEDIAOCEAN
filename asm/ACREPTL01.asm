*          DATA SET ACREPTL01  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACTL01A,+0                                                               
         TITLE 'ACTL01 - SPECS FOR TRANSACTION UPLOAD PROGRAM'                  
ACTL01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF GETOPT,N                                                         
         ACDEF RESET                                                            
         ACDEF WIDTH,198                                                        
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,143,C'TRANS UPLOAD'                                           
         ACDEF H2,143,PAGE                                                      
*                                                                               
         ACDEF H6,002,C'WC'                                                     
         ACDEF H6,005,C'CONTRA ACCOUNT'                                         
         ACDEF H6,020,C'CONTRA NAME'                                            
         ACDEF H6,057,C'DATE'                                                   
         ACDEF H6,064,C'REFER '                                                 
         ACDEF H6,071,C'SQ'                                                     
         ACDEF H6,074,C'BATREF'                                                 
         ACDEF H6,081,C'TP'                                                     
         ACDEF H6,084,C'TT'                                                     
         ACDEF H6,092,C'RATE'                                                   
         ACDEF H6,102,C'HOURS'                                                  
         ACDEF H6,114,C'DEBITS'                                                 
         ACDEF H6,126,C'CREDITS'                                                
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H2,76,C'**ERROR REPORT**'                                        
         ACDEF H6,005,C'ACCOUNT        '                                        
         ACDEF H6,020,C'CONTRA ACCOUNT '                                        
         ACDEF H6,114,C'AMOUNT '                                                
         ACDEF H6,126,C'ERRORS '                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPTL01 08/16/00'                                      
         END                                                                    
