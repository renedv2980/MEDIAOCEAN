*          DATA SET PPREPSB01  AT LEVEL 002 AS OF 08/04/14                      
*          DATA SET PPREPSA01  AT LEVEL 005 AS OF 08/04/14                      
*          DATA SET PPREPA901  AT LEVEL 004 AS OF 11/14/08                      
*PHASE PPSB01A                                                                  
         TITLE 'PPBA01 - SAP INTERFACE - SPECS'                                 
PPSB01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         PSPEC H1,46,C'PRINTPAK SAP CODES INTERFACE'                            
         PSPEC H2,46,C'-------------------------'                               
*                                                                               
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H4,98,PAGE                                                       
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREPSB01 08/04/14'                                      
         END                                                                    
