*          DATA SET PPREPXJ01  AT LEVEL 002 AS OF 08/09/00                      
*PHASE PPXJ01A                                                                  
         TITLE 'PPXJ01 - PRINTPAK TAPE INTERFACE'                               
PPXJ01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
*                                                                               
         SPROG 0,10                                                             
         PSPEC H1,36,C'PRINTPAK BUCKET INTERFACE TAPE'                          
         PSPEC H2,36,C'------------------------------'                          
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
**PAN#1  DC    CL21'002PPREPXJ01 08/09/00'                                      
         END                                                                    
