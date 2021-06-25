*          DATA SET SPREPMQ01  AT LEVEL 005 AS OF 08/29/00                      
*PHASE SPMQ01A                                                                  
         TITLE 'SPMM01 - MMP CONVERSION TO EASI INPUT FILE'                     
SPMM01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,REQUESTOR                                                   
*                                                                               
         SSPEC H1,54,C'MMPLUS CONVERSION ERRORS'                                
         SSPEC H2,54,C'------------------------'                                
*                                                                               
         SSPEC H1,100,REPORT                                                    
         SSPEC H2,100,RUN                                                       
         SSPEC H2,120,PAGE                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
 END                                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPREPMQ01 08/29/00'                                      
         END                                                                    
