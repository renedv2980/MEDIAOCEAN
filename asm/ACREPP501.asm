*          DATA SET ACREPP501  AT LEVEL 002 AS OF 08/17/00                      
*PHASE ACP501A                                                                  
         TITLE 'SPECS FOR UNAPPROVED/AUTHORISER REPORT'                         
ACP501   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,98,PAGE                                                       
         ASPEC H1,40,C'PRODUCTION INVOICES BY AUTHORISER'                       
         ASPEC H2,40,C'---------------------------------'                       
         ASPEC H2,85,REQUESTOR                                                  
         ASPEC H3,2,COMPANY                                                     
         ASPEC H5,2,C'AUTHORISED BY'                                            
         ASPEC H7,2,C'JOB CODE        SUPPLIER'                                 
         ASPEC H8,2,C'--------        --------'                                 
         ASPEC H7,49,C'INVOICE'                                                 
         ASPEC H8,49,C'NUMBER'                                                  
         ASPEC H7,60,C'INVOICE'                                                 
         ASPEC H8,60,C' DATE'                                                   
         ASPEC H7,70,C'ORDER'                                                   
         ASPEC H8,70,C'NUMBER'                                                  
         ASPEC H7,80,C'DATE'                                                    
         ASPEC H8,79,C'ADDED'                                                   
         ASPEC H7,88,C'BATCH'                                                   
         ASPEC H8,89,C'REF.'                                                    
         ASPEC H7,99,C'NET AMOUNT'                                              
         ASPEC H8,99,C'----------'                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPP501 08/17/00'                                      
         END                                                                    
