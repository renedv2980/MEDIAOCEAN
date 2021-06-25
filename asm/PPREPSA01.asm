*          DATA SET PPREPSA01  AT LEVEL 007 AS OF 08/07/17                      
*          DATA SET PPREPA901  AT LEVEL 004 AS OF 11/14/08                      
*PHASE PPSA01B                                                                  
         TITLE 'PPSA01 - SAP INTERFACE - SPECS'                                 
PPSA01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,CLIENTS                                                     
*                                                                               
         PSPEC H1,46,C'PRINTPAK SAP INTERFACE'                                  
         PSPEC H2,46,C'-------------------------'                               
*                                                                               
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H4,98,PAGE                                                       
         PSPEC M1,01,C'AG MD OF CLT PRD ----PUB----    EST YS MS'               
         PSPEC M1,43,C'TYP  INVOICE   --GROSS---  ---NET----'                   
         PSPEC M2,01,C'-- -- -- --- --- -----------    --- -- --'               
         PSPEC M2,43,C'---  -------   ----------  ----------'                   
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PPREPSA01 08/07/17'                                      
         END                                                                    
