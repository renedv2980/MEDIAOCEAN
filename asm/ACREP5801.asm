*          DATA SET ACREP5801  AT LEVEL 008 AS OF 12/14/90                      
*PHASE AC5801A,+0                                                               
         TITLE 'SPECS FOR DISBURSEMENTS'                                        
AC5801   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         RSPEC MAXLINES,54                                                      
         SPACE 1                                                                
         SPROG 0,1,2,3,4                                                        
         ASPEC F1,2,REQDETS                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H3,2,COMPANY                                                     
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H4,2,LEDGER                                                      
         SPACE 1                                                                
         SPROG 0,3,4                                                            
         ASPEC H1,42,C'CLEARED BUT UNDISBURSED ITEMS'                           
         ASPEC H2,42,29C'-'                                                     
         ASPEC H8,93,C'TOTAL'                                                   
         ASPEC H9,43,C'AND PRIOR'                                               
*&&US*&& ASPEC H9,93,C'-----'                                                   
         ASPEC H9,79,C'AND AFTER'                                               
         SPACE 1                                                                
         SPROG 3                                                                
         ASPEC H8,102,C'  CASH'                                                 
         ASPEC H9,102,C'DISCOUNT'                                               
         SPACE 1                                                                
         SPROG 4                                                                
         ASPEC H8,104,C' URGENT'                                                
         ASPEC H9,104,C'INVOICES'                                               
         SPACE 1                                                                
         SPROG 1,2                                                              
         ASPEC H1,43,C'CASH DISBURSEMENT REPORT'                                
         ASPEC H2,43,24C'-'                                                     
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H8,23,C'DESCRIPTION OF DISBURSEMENT'                             
*&&US*&& ASPEC H8,59,C'CASH DISC    NET LESS   CHECK   CHECK     BANK'          
*&&UK*&& ASPEC H8,59,C'CASH DISC    NET LESS  CHEQUE  CHEQUE     BANK'          
*&&US*&& ASPEC H9,23,27C'-'                                                     
         ASPEC H9,59,C'  TAKEN     CASH DISC  NUMBER    DATE   ACCOUNT'         
         SPACE 1                                                                
         SPROG 2                                                                
*&&US*&& ASPEC H9,60,C'----TOTAL CHECKS-----'                                   
*&&UK*&& ASPEC H9,60,C'----TOTAL CHEQUES----'                                   
         ASPEC H9,86,C'----TOTAL VOIDS-----'                                    
         ASPEC H10,60,C'CASH DISC   NET LESS'                                   
         ASPEC H10,86,C'CASH DISC   NET LESS'                                   
         ASPEC H11,62,C'TAKEN    CASH DISC'                                     
         ASPEC H11,88,C'TAKEN    CASH DISC'                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREP5801 12/14/90'                                      
         END                                                                    
