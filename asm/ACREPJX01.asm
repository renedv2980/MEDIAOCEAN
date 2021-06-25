*          DATA SET ACREPJX01  AT LEVEL 006 AS OF 07/02/01                      
*PHASE ACJX01A                                                                  
         TITLE 'JOB EXTRACT REPORT'                                             
ACJX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
*        FSPEC READ,TRANSACTIONS                                                
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,070,C'JOB EXTRACT REPORT'                                     
         ACDEF H1,105,REPORT                                                    
         ACDEF H1,120,PAGE                                                      
         ACDEF H2,070,C'------------------'                                     
         ACDEF H2,105,REQUESTOR                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPJX01 07/02/01'                                      
         END                                                                    
