*          DATA SET PPREPXB01  AT LEVEL 009 AS OF 08/09/00                      
*PHASE PPXB01A                                                                  
         TITLE 'PPXB01 - PRINTPAK BRAND INTERFACE'                              
PPXB01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,CLIENTS                                                     
*                                                                               
         PSPEC H1,46,C'PRINTPAK BRAND INTERFACE'                                
         PSPEC H2,46,C'------------------------'                                
*                                                                               
         PSPEC H1,1,MEDIA                                                       
         PSPEC H2,1,CLIENT                                                      
*                                                                               
         PSPEC H1,90,AGYNAME                                                    
         PSPEC H2,90,AGYADD                                                     
         PSPEC H4,90,REPORT                                                     
         PSPEC H5,90,PAGE                                                       
*                                                                               
         PSPEC H4,1,C' SYS  MED  CLT  PRD  EST  AGY CODE'                       
         PSPEC H5,1,C' ---  ---  ---  ---  ---  --------'                       
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009PPREPXB01 08/09/00'                                      
         END                                                                    
