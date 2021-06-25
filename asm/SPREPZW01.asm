*          DATA SET SPREPZW01  AT LEVEL 001 AS OF 02/07/05                      
*PHASE SPZW01A                                                                  
         TITLE 'SPZW01 - SCAN WORKER FILES FOR INVOICES'                        
SPZW01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,38,C'WORKER FILE INVOICES'                                    
         SSPEC H2,38,C'--------------------'                                    
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
 END                                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPZW01 02/07/05'                                      
         END                                                                    
