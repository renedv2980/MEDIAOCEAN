*          DATA SET ACREPHR01  AT LEVEL 031 AS OF 12/22/08                      
*PHASE ACHR01A                                                                  
         TITLE 'RECOVERY REPORT'                                                
ACHR01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0,1                                                        
         ASPEC H1,2,RUN                                                         
         ASPEC H1,100,PAGE                                                      
         ASPEC H2,2,C'COMPANY'                                                  
         ASPEC H3,2,C'UNIT'                                                     
         ASPEC H3,100,REPORT                                                    
         ASPEC H4,2,C'LEDGER'                                                   
         ASPEC H4,100,REQUESTOR                                                 
         ASPEC H5,2,C'PEEL DATE'                                                
*                                                                               
         ASPEC H1,62,C'RECOVERY HISTORY REPORT'                                 
         ASPEC H2,62,C'-----------------------'                                 
         ASPEC H8,03,C'ACCOUNT NUMBER'                                          
         ASPEC H9,03,C'--------------'                                          
         ASPEC H8,21,C'ACCOUNT NAME'                                            
         ASPEC H9,21,C'------------'                                            
*                                                                               
         ACDEF SPROG,0,1                                                        
         ASPEC H7,70,C'RESTORED'                                                
         ASPEC H8,70,C' DEBIT  '                                                
         ASPEC H9,70,C'--------'                                                
         ASPEC H7,90,C'RESTORED'                                                
         ASPEC H8,90,C' CREDIT '                                                
         ASPEC H9,90,C'--------'                                                
*                                                                               
         ACDEF SPROG,1                                                          
         ASPEC H8,37,C'OF'                                                      
         ASPEC H9,37,C'--'                                                      
         ASPEC H8,47,C'PREVIOUS   '                                             
         ASPEC H8,47,C'BALANCE B/F'                                             
         ASPEC H9,47,C'-----------'                                             
         ASPEC H7,107,C'NEW'                                                    
         ASPEC H8,107,C'BALANCE B/F'                                            
         ASPEC H9,107,C'-----------'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACREPHR01 12/22/08'                                      
         END                                                                    
