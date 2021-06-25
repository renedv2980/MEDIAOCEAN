*          DATA SET ACREPWD01  AT LEVEL 006 AS OF 08/16/00                      
*PHASE ACWD01A,*+0                                                              
         TITLE 'ACWD01 - SPECS WORKER FILE DUMP'                                
ACWD01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0                                                                
         RSPEC REQUEST,NOSUM                                                    
         RSPEC REQUEST,NOREP                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,99,PAGE                                                       
         ASPEC H1,45,C'WORKER LISTING'                                          
         ASPEC H2,45,C'--------------'                                          
         ASPEC H3,12,C'ACCOUNT          CONTRA-ACCOUNT  CODES     DATE'         
         ASPEC H4,12,C'-------          --------------  -----     ----'         
         ASPEC H3,64,C'MOS'                                                     
         ASPEC H4,64,C'---'                                                     
         ASPEC H3,68,C'REF.    NARRATIVE'                                       
         ASPEC H4,68,C'----    ---------'                                       
         ASPEC H3,108,C'DEBITS      CREDITS'                                    
         ASPEC H4,108,C'------      -------'                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPWD01 08/16/00'                                      
         END                                                                    
