*          DATA SET PPREPXM01  AT LEVEL 008 AS OF 08/09/00                      
*PHASE PPXM01A                                                                  
         TITLE 'PPXM01 - ARMED FORCES PRINT TAPE INTERFACE'                     
PPXM01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
*                                                                               
         PSPEC H1,37,C'ARMED FORCES PRINT INTERFACE TAPE'                       
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
**PAN#1  DC    CL21'008PPREPXM01 08/09/00'                                      
         END                                                                    
