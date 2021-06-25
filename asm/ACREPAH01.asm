*          DATA SET ACREPAH01  AT LEVEL 019 AS OF 08/08/97                      
*PHASE ACAH01A,+0                                                               
         TITLE 'SPECS FOR AUTO HOLD REPORT'                                     
ACAH01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         FSPEC UPDATE,ACCFILE                                                   
         ACDEF PRORATA                                                          
         SPACE 1                                                                
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,98,REPORT                                                     
         ASPEC H1,115,PAGE                                                      
         ASPEC H3,2,COMPANY                                                     
         ASPEC H2,98,REQUESTOR                                                  
         ASPEC H4,2,C'CLIENT'                                                   
         ASPEC H5,2,C'PRODUCT'                                                  
         ASPEC H6,2,C'JOB'                                                      
*                                                                               
         ASPEC H3,98,C'JOB OPENED'                                              
         ASPEC H4,98,C'JOB CLOSED'                                              
         ASPEC H5,98,C'LAST BILLED'                                             
         ASPEC H6,98,C'BILL TYPE'                                               
         ASPEC F1,2,REQDETS                                                     
*                                                                               
         SPROG 0                                                                
         ASPEC H9,3,C'ADDITIONAL '                                              
         ASPEC H10,3,C'REVISION:'                                               
*                                                                               
         ASPEC H9,17,C'CURRENT'                                                 
         ASPEC H10,17,C'ESTIMATE:'                                              
*                                                                               
         ASPEC H9,31,C' EXCESS OVER '                                           
         ASPEC H10,31,C' EST ALLOWED '                                          
*                                                                               
         ASPEC H9,45,C'MAX ESTIMATE '                                           
         ASPEC H10,45,C'    AMOUNT   '                                          
*                                                                               
         ASPEC H9,59,C'    TOTAL    '                                           
         ASPEC H10,59,C'   ACTUALS   '                                          
*                                                                               
         ASPEC H9,73,C'   BILLED   '                                            
         ASPEC H10,73,C'OPEN ACTUALS '                                          
*                                                                               
         ASPEC H9,87,C'   HOLD TO   '                                           
         ASPEC H10,87,C'    BILL     '                                          
*                                                                               
         ASPEC H9,101,C'CURRENTLY ON '                                          
         ASPEC H10,101,C'    HOLD     '                                         
*                                                                               
         ASPEC H9,115,C'    HOLD     '                                          
         ASPEC H10,115,C'   TARGET    '                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACREPAH01 08/08/97'                                      
         END                                                                    
