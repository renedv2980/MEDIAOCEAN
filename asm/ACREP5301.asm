*          DATA SET ACREP5301  AT LEVEL 013 AS OF 08/16/00                      
*PHASE AC5301A                                                                  
         TITLE 'SPECS FOR SPOT CLEARANCE PROGRAM'                               
*                                                                               
AC5301   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF GETOPT,N                                                         
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,108,C'REPORT AC54'                                            
         ACDEF H1,125,PAGE                                                      
*                                                                               
         ACDEF SPROG,1,2,3,4,5                                                  
         ACDEF H4,2,C'AGENCY'                                                   
         ACDEF H6,2,C'REP  STATION CLIENT INVOICE     PERIOD        '           
         ACDEF H7,2,C'---  ------- ------ NUMBER      ------        '           
         ACDEF H6,48,C'PRODUCT'                                                 
         ACDEF H7,48,C'-------'                                                 
         ACDEF H6,109,C' CASH      AMOUNT LESS '                                
         ACDEF H7,109,C'DISCOUNT  CASH DISCOUNT'                                
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H6,73,C' TYPE'                                                   
         ACDEF H7,73,C' ----'                                                   
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H6,73,C' TYPE'                                                   
         ACDEF H7,73,C' ----'                                                   
         ACDEF H6,84,C'   GST        PST  '                                     
         ACDEF H7,84,C'--------   --------'                                     
*                                                                               
         ACDEF SPROG,4                                                          
         ACDEF H6,73,C' CASH'                                                   
         ACDEF H7,73,C'RECEIPT'                                                 
*                                                                               
         ACDEF SPROG,5                                                          
         ACDEF H6,73,C' CASH'                                                   
         ACDEF H7,73,C'RECEIPT'                                                 
         ACDEF H6,84,C'   GST        PST  '                                     
         ACDEF H7,84,C'--------   --------'                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREP5301 08/16/00'                                      
         END                                                                    
