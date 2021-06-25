*          DATA SET ACREP6001  AT LEVEL 010 AS OF 08/16/00                      
*PHASE AC6001A                                                                  
         TITLE 'SPECS FOR CASH CONTROL REPORT'                                  
AC6001   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         RSPEC REQUEST,NOREP                                                    
         SPROG 0                                                                
         ASPEC H1,40,C'CASH CONTROL REPORT'                                     
         SPROG 1                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'CASH CONTROL TRACE'                                      
         ASPEC H2,40,18C'-'                                                     
         ASPEC H6,2,LEDGER                                                      
         ASPEC H8,2,C'ACCOUNT CODE'                                             
         ASPEC H9,2,C'------------'                                             
         ASPEC H8,19,C'REFERENCE'                                               
         ASPEC H9,19,C'---------'                                               
         ASPEC H8,33,C'DATE'                                                    
         ASPEC H9,33,C'----'                                                    
         ASPEC H8,46,C'AMOUNT'                                                  
         ASPEC H9,46,C'------'                                                  
         ASPEC H8,57,C'DR/CR'                                                   
         ASPEC H9,57,C'-----'                                                   
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,80,REPORT                                                     
         ASPEC H1,98,PAGE                                                       
         ASPEC H2,40,19C'-'                                                     
         ASPEC H4,2,COMPANY                                                     
         ASPEC H5,2,C'CODE'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREP6001 08/16/00'                                      
         END                                                                    
