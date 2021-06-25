*          DATA SET ACREPAA01  AT LEVEL 008 AS OF 03/24/20                      
*PHASE ACAA01C,+0                                                               
         TITLE 'ACAA - AUTO APPROVE PAYABLES'                                   
ACAA01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANS                                                       
*                                                                               
         DC    AL1(10)                                                          
         DC    AL1(03)                                                          
         DC    AL1(22)             READ TRANSACTION DIRECTORY                   
*                                                                               
         SPROG 0,1,2,3,4,5                                                      
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,C'AUTO APPROVE PAYABLES'                                   
         ACDEF H2,86,C'---------------------'                                   
         ACDEF H4,2,COMPANY                                                     
         ACDEF H1,160,REPORT                                                    
         ACDEF H1,180,PAGE                                                      
         ACDEF H2,160,REQUESTOR                                                 
*                                                                               
         SPROG 0,1,2,3                                                          
         ACDEF H6,3,C'S'                                                        
         ACDEF H7,3,C'Y'                                                        
         ACDEF H8,3,C'S'                                                        
         ACDEF H6,5,C'M'                                                        
         ACDEF H7,5,C'E'                                                        
         ACDEF H8,5,C'D'                                                        
         ACDEF H6,7,C'CLI'                                                      
         ACDEF H6,11,C'PRO'                                                     
         ACDEF H6,16,C'EST'                                                     
         ACDEF H6,23,C'MOS'                                                     
         ACDEF H6,30,C'VENDOR'                                                  
         ACDEF H6,44,C'VENDOR'                                                  
         ACDEF H7,44,C'INVOICE # '                                              
         ACDEF H6,59,C' AMOUNT '                                                
         ACDEF H7,59,C' BILLED '                                                
         ACDEF H8,59,C' NET    '                                                
         ACDEF H6,77,C' CASH    '                                               
         ACDEF H7,77,C'RECEIVED '                                               
         ACDEF H8,77,C'NET      '                                               
         ACDEF H6,94,C' CHECKS '                                                
         ACDEF H7,94,C'DISBURSED'                                               
         ACDEF H6,111,C'CASH   '                                                
         ACDEF H7,111,C'POSITION'                                               
         ACDEF H8,107,C'(RECEIVED-DISB)'                                        
         ACDEF H6,128,C'TOTAL   '                                               
         ACDEF H7,128,C'CLEARED '                                               
         ACDEF H6,145,C'CLEARED BUT'                                            
         ACDEF H7,145,C'UNDISBURSED'                                            
         ACDEF H6,163,C'CASH'                                                   
         ACDEF H7,163,C'AVAILABLE'                                              
         ACDEF H8,162,C'(CASH-APPROVED)'                                        
*                                                                               
         SPROG 2                                                                
         ACDEF H3,155,C'**ERROR** NO ESTIMATES'                                 
*                                                                               
         SPROG 3                                                                
         ACDEF H3,155,C'**ERROR** BAD ESTIMATES'                                
*                                                                               
         SPROG 4                                                                
         ACDEF H3,160,C'SUMMARY OF TOTALS'                                      
*                                                                               
         SPROG 5                                                                
         ACDEF H8,2,C'**ERROR** INCONSISTENT WITH PROFILE'                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPAA01 03/24/20'                                      
         END                                                                    
