*          DATA SET REREP2501  AT LEVEL 005 AS OF 03/10/94                      
*PHASE RE2501A,*                                                                
         TITLE 'REREP2501 (RE2501) --- SPECS FOR STRATEGY KEY CHG'              
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP2501 - RE2501 - STRATEGY KEY CHANGE                        *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 10MAR94 (SKU) DATE OF CONCEPTION                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE2501   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREP2501 03/10/94'                                      
         END                                                                    
