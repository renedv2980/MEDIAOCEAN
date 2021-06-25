*          DATA SET ACREP3601  AT LEVEL 002 AS OF 08/16/00                      
*PHASE AC3601A                                                                  
         TITLE 'SPECS FOR INTERAGENCY BRAND SELLOFF REPORT'                     
AC3601   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H2,45,C'INTERAGENCY SELLOFF REPORT'                              
         ASPEC H3,45,C'--------------------------'                              
         ASPEC H2,86,REPORT                                                     
         ASPEC H2,99,PAGE                                                       
         ASPEC H8,2,C'INVOICE'                                                  
         ASPEC H9,2,C'-------'                                                  
         ASPEC H8,10,C'DESCRIPTION'                                             
         ASPEC H9,10,C'-----------'                                             
         ASPEC H8,44,C'MEDIA'                                                   
         ASPEC H9,44,C'-----'                                                   
         ASPEC H8,50,C'MONTH OF'                                                
         ASPEC H9,50,C'SERVICE'                                                 
         ASPEC H8,60,C'ESTIMATE'                                                
         ASPEC H9,60,C' NUMBER '                                                
         ASPEC H8,70,C'INVOICE'                                                 
         ASPEC H9,70,C'  DATE '                                                 
         ASPEC H8,83,C'ESTIMATE'                                                
         ASPEC H9,83,C' AMOUNT '                                                
         ASPEC H8,96,C'PAYABLE'                                                 
         ASPEC H9,96,C'AMOUNT'                                                  
         ASPEC H8,105,C'PERCENT'                                                
         ASPEC H9,105,C'-------'                                                
         ASPEC F1,2,REQDETS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREP3601 08/16/00'                                      
         END                                                                    
