*          DATA SET ACREPP601  AT LEVEL 005 AS OF 08/17/00                      
*PHASE ACP601A                                                                  
         TITLE 'SPECS FOR JOB POSTING DETAIL'                                   
ACP601   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
*                                                                               
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,45,C'JOB POSTING DETAIL'                                      
         ASPEC H2,45,C'------------------'                                      
         ASPEC H1,80,REPORT                                                     
         ASPEC H1,98,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H5,2,C'CLIENT'                                                   
         ASPEC H6,2,C'PRODUCT'                                                  
         ASPEC H7,2,C'JOB'                                                      
         ASPEC H4,80,REQUESTOR                                                  
         ASPEC H5,80,C'OPTIONS'                                                 
*                                                                               
         SPROG 0                                                                
         ASPEC H10,2,C'WORK-CODE AND SUPPLIER'                                  
         ASPEC H11,2,C'----------------------'                                  
         ASPEC H10,25,C'---INVOICE---  ORDER'                                   
         ASPEC H11,25,C'NUMBER   DATE  NUMBER'                                  
         ASPEC H10,47,C'AUTHORISED BY'                                          
         ASPEC H11,47,C'-------------'                                          
         ASPEC H10,63,C'A/H'                                                    
         ASPEC H11,63,C'---'                                                    
         ASPEC H10,67,C'-----BILL----'                                          
         ASPEC H11,67,C'NUMBER   DATE'                                          
         ASPEC H10,86,C'NET'                                                    
         ASPEC H11,84,C'AMOUNT'                                                 
         ASPEC H10,95,C'ALLOC'                                                  
         ASPEC H11,95,C'AMOUNT'                                                 
         ASPEC H10,104,C'BALANCE'                                               
         ASPEC H11,104,C'-------'                                               
*                                                                               
         SPROG 1                                                                
         ASPEC M1,21,C'ORIGINAL    PRESENT      ORDER'                          
         ASPEC M2,21,C'ESTIMATE    ESTIMATE     AMOUNT'                         
         ASPEC M1,57,C'BILLED'                                                  
         ASPEC M2,57,C'AMOUNT'                                                  
         ASPEC M1,71,C'NET       COMM      GROSS   UNBILLED'                    
         ASPEC M2,71,C'---       ----      -----    CHARGES'                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPP601 08/17/00'                                      
         END                                                                    
