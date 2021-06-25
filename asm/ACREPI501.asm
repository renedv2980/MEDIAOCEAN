*          DATA SET ACREPI501  AT LEVEL 014 AS OF 08/17/00                      
*PHASE ACI501A                                                                  
         TITLE 'ACI501 OGILVY MATHER PRODUCTION BILLING, MEDIA RECAP'           
ACI501   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0                                                                
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,39,C'   INVOICE REGISTER WITH FEES    '                       
         ASPEC H2,2,COMPANY                                                     
         ASPEC H1,87,REPORT                                                     
         ASPEC H1,102,PAGE                                                      
         ASPEC H3,87,PERIOD                                                     
         ASPEC H7,43,C'RECEIVABLE'                                              
         ASPEC H8,43,10C'-'                                                     
         ASPEC H7,60,C'INVENTORY'                                               
         ASPEC H8,60,9C'-'                                                      
         ASPEC H7,75,C'COMMISSION'                                              
         ASPEC H8,75,10C'-'                                                     
         ASPEC H7,88,C'CASH DISCOUNT'                                           
         ASPEC H8,88,13C'-'                                                     
         ASPEC H7,107,C'FEE AMOUNT'                                             
         ASPEC H8,107,10C'-'                                                    
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H3,42,C'PRODUCTION BILLING SUMMARY'                              
         ASPEC H4,42,26C'-'                                                     
         ASPEC H7,2,C'CLI/PROD/JOB'                                             
         ASPEC H8,2,12C'-'                                                      
         ASPEC H7,20,C'DATE'                                                    
         ASPEC H8,20,4C'-'                                                      
         ASPEC H7,31,C'INV. NO.'                                                
         ASPEC H8,31,8C'-'                                                      
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H3,46,C'MEDIA BILLING RECAP'                                     
         ASPEC H4,46,19C'-'                                                     
         ASPEC H7,2,C'MEDIA/CLIENT'                                             
         ASPEC H8,2,12C'-'                                                      
         SPACE 1                                                                
         SPROG 2                                                                
*        ASPEC H3,2,20C' '                                                      
         ASPEC H3,46,C'GROUP TOTAL SUMMARY'                                     
         ASPEC H4,46,19C'-'                                                     
         ASPEC H7,7,C'GROUP CATEGORY'                                           
         ASPEC H8,7,14C'-'                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACREPI501 08/17/00'                                      
         END                                                                    
