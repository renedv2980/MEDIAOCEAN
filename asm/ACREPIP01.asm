*          DATA SET ACREPIP01  AT LEVEL 002 AS OF 08/17/00                      
*PHASE ACIP01A                                                                  
         TITLE 'ACIP01 - WRITES MAD WORKER FILE FOR PROD / JOB'                 
ACIP01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ASPEC H1,1,MEDIA                                                       
         ASPEC H2,1,REQUESTOR                                                   
         ASPEC H1,38,C'SCRIPT WORKER FILE'                                      
         ASPEC H2,38,C'------------------'                                      
*                                                                               
         SSPEC H1,100,COMPANY                                                   
         SSPEC H2,100,PAGE                                                      
         SSPEC H2,111,REPORT                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPIP01 08/17/00'                                      
         END                                                                    
