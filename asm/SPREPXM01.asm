*          DATA SET SPREPXM01  AT LEVEL 003 AS OF 08/29/00                      
*PHASE SPXM01A                                                                  
         TITLE 'SPXM01 - ARMED FORCES SPOT TAPE INTERFACE'                      
SPXM01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
*                                                                               
         PSPEC H1,37,C'ARMED FORCES SPOT INTERFACE TAPE'                        
         PSPEC H2,37,C'---------------------------------'                       
*                                                                               
         PSPEC H1,80,AGYNAME                                                    
         PSPEC H2,80,AGYADD                                                     
         PSPEC H4,80,REPORT                                                     
         PSPEC H5,80,PAGE                                                       
*                                                                               
         PSPEC H2,1,MEDIA                                                       
         PSPEC H4,1,CLIENT                                                      
*                                                                               
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPXM01 08/29/00'                                      
         END                                                                    
