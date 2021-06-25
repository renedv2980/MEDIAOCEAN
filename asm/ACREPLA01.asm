*          DATA SET ACREPLA01  AT LEVEL 006 AS OF 08/17/00                      
*PHASE ACLA01A                                                                  
         TITLE 'SPECS FOR LEDGER ANALYSIS'                                      
ACLA01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
*                                                                               
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,45,C'LEDGER ANALYSIS'                                         
         ASPEC H2,45,C'---------------'                                         
         ASPEC H1,83,REPORT                                                     
         ASPEC H2,83,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,83,REQUESTOR                                                  
*                                                                               
         SPROG 0,1                                                              
         ASPEC H8,26,C'       DEBITS  '                                         
         ASPEC H9,26,C'  -------------'                                         
         ASPEC H8,44,C'      CREDITS  '                                         
         ASPEC H9,44,C'  -------------'                                         
         ASPEC H8,62,C'    DIFFERENCE '                                         
         ASPEC H9,62,C'  -------------'                                         
*                                                                               
         SPROG 0                                                                
         ASPEC H7,3,C' UNIT '                                                   
         ASPEC H8,3,C'LEDGER'                                                   
         ASPEC H9,3,C'------'                                                   
         ASPEC H8,13,C'TYPE'                                                    
         ASPEC H9,13,C'----'                                                    
*                                                                               
         SPROG 1                                                                
         ASPEC H8,4,C'TYPE'                                                     
         ASPEC H9,4,C'----'                                                     
         ASPEC H7,11,C' UNIT '                                                  
         ASPEC H8,11,C'LEDGER'                                                  
         ASPEC H9,11,C'------'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPLA01 08/17/00'                                      
         END                                                                    
