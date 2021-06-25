*          DATA SET ACREPBK01  AT LEVEL 001 AS OF 11/12/17                      
*PHASE ACBK01A                                                                  
         TITLE 'BANK RECORD DETAIL FOR COMPAMY CODE,ALPHA ID'                   
ACBK01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPBK01 11/12/17'                                      
         END                                                                    
