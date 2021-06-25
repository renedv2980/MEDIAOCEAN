*          DATA SET SPREPMM01  AT LEVEL 003 AS OF 08/29/00                      
*PHASE SPMM01A                                                                  
         TITLE 'SPMM01 - MMP CONVERSION TO WORKER FILE'                         
SPMM01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
*                                                                               
         SSPEC H1,38,C'MM+ UPLOAD'                                              
         SSPEC H2,38,C'----------'                                              
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
**PAN#1  DC    CL21'003SPREPMM01 08/29/00'                                      
         END                                                                    
