*          DATA SET ACREPOC01  AT LEVEL 001 AS OF 09/30/02                      
*PHASE ACOC01A                                                                  
         TITLE 'CONTRA-OFFICE BUCKET REPORT'                                    
ACOC01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ACDEF READ,ACCOUNTS                                                    
         ACDEF WIDTH,198                                                        
         ACDEF UPDATE,ACCFILE                                                   
*                                                                               
         ACDEF SPROG,0,1,2,3                                                    
         ACDEF H1,60,C'CONTRA-OFFICE BALANCES'                                  
         ACDEF H2,60,C'----------------------'                                  
         ACDEF H1,2,RUN                                                         
         ACDEF H1,125,REPORT                                                    
         ACDEF H1,145,PAGE                                                      
         ACDEF H2,125,REQUESTOR                                                 
         ACDEF H3,2,COMPANY                                                     
         ACDEF H4,2,UNIT                                                        
         ACDEF H5,2,LEDGER                                                      
*                                                                               
         SPROG 1,2                                                              
         ACDEF H6,2,C'ACCOUNT'                                                  
         ACDEF H09,85,C'                    HISTORY BUCKETS'                    
         ACDEF H11,85,C'        DEBITS           CREDITS   '                    
         ACDEF H11,121,C'       BALANCE   '                                     
         ACDEF H11,139,C'  OUT OF BALANCE '                                     
*                                                                               
         SPROG 1                                                                
         ACDEF H10,2,C'OFFICE'                                                  
         ACDEF H09,31,C'          ACCOUNT TOTALS          '                     
         ACDEF H11,13,C'   BALANCE FRWD  '                                      
         ACDEF H11,31,C'        DEBITS           CREDITS   '                    
         ACDEF H11,66,C'       BALANCE   '                                      
*                                                                               
         SPROG 2                                                                
         ACDEF H10,3,C'CONTRA'                                                  
         ACDEF H10,17,C'OFFICE'                                                 
*                                                                               
         SPROG 3                                                                
         ACDEF H6,2,C'ACCOUNT'                                                  
         ACDEF H09,31,C'          NON-OFFICE TOTALS       '                     
         ACDEF H11,31,C'        DEBITS           CREDITS   '                    
         ACDEF H11,66,C'       BALANCE   '                                      
         ACDEF H09,85,C'                    OFFICE  BUCKETS'                    
         ACDEF H11,85,C'        DEBITS           CREDITS   '                    
         ACDEF H11,121,C'       BALANCE   '                                     
         ACDEF H11,139,C'  OUT OF BALANCE '                                     
         ACDEF H10,3,C'CONTRA'                                                  
         ACDEF H10,23,C'MONTH'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPOC01 09/30/02'                                      
         END                                                                    
