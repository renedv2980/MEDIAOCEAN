*          DATA SET ACREP5401  AT LEVEL 021 AS OF 05/28/99                      
*PHASE AC5401A,+0                                                               
         TITLE 'SPECS FOR PRINT CLEARANCE PROGRAM'                              
*                                                                               
AC5401   CSECT                                                                  
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
         ACDEF H6,2,C'REP  PUBL     CLIENT INVOICE    PERIOD        '           
         ACDEF H7,2,C'---  ----     -----  NUMBER     ------        '           
         ACDEF H6,48,C'PRODUCT'                                                 
         ACDEF H7,48,C'-------'                                                 
         ACDEF H6,109,C' CASH      AMOUNT LESS '                                
         ACDEF H7,109,C'DISCOUNT  CASH DISCOUNT'                                
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H6,76,C'TYPE'                                                    
         ACDEF H7,76,C'----'                                                    
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H6,76,C'TYPE'                                                    
         ACDEF H7,76,C'----'                                                    
         ACDEF H6,86,C'   GST         PST  '                                    
         ACDEF H7,86,C'--------    --------'                                    
*                                                                               
         ACDEF SPROG,4                                                          
         ACDEF H6,74,C' CASH  '                                                 
         ACDEF H7,74,C'RECEIPT'                                                 
*                                                                               
         ACDEF SPROG,5                                                          
         ACDEF H6,74,C' CASH  '                                                 
         ACDEF H7,74,C'RECEIPT'                                                 
         ACDEF H6,86,C'   GST         PST  '                                    
         ACDEF H7,86,C'--------    --------'                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACREP5401 05/28/99'                                      
         END                                                                    
