*          DATA SET ACREPZH01  AT LEVEL 017 AS OF 08/16/00                      
*PHASE ACZH01A                                                                  
         TITLE '$BILL FIX, REMOVE WC FROM KEY'                                  
ACZH01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,COMPANY                                                     
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,035,C'REMOVE WC FROM KEY'                                     
         ACDEF H1,70,PAGE                                                       
*                                                                               
         ACDEF H6,003,C'ACCOUNT'                                                
         ACDEF H7,003,C'--------------'                                         
         ACDEF H6,019,C'CONTRA'                                                 
         ACDEF H7,019,C'--------------'                                         
         ACDEF H6,035,C'WC'                                                     
         ACDEF H7,035,C'--'                                                     
         ACDEF H6,039,C'DATE'                                                   
         ACDEF H7,039,C'--------'                                               
         ACDEF H6,049,C'REFER.'                                                 
         ACDEF H7,049,C'------'                                                 
         ACDEF H6,057,C'TYPE'                                                   
         ACDEF H7,057,C'----'                                                   
         ACDEF H5,063,C'DR'                                                     
         ACDEF H6,063,C'CR'                                                     
         ACDEF H7,063,C'--'                                                     
         ACDEF H6,067,C'AMOUNT'                                                 
         ACDEF H7,067,C'---------------'                                        
         ACDEF H5,084,C' NEW  '                                                 
         ACDEF H6,084,C'SUBREF'                                                 
         ACDEF H7,084,C'------'                                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACREPZH01 08/16/00'                                      
         END                                                                    
