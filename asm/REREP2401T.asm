*          DATA SET REREP2401T AT LEVEL 005 AS OF 06/04/98                      
*PHASE RE2401T,*                                                                
         TITLE 'REREP2401 (RE2401) --- SPECS FOR COMBO BUY FLAGGING'            
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP2401 - RE2401 - MARKS COMBO BUY                            *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 31MAR93 (SKU) DATE OF CONCEPTION                                  *           
* 04JUN98 (SCH) DON'T NEED TO READ CONTRACTS FOR INV PURGE          *           
*               (REREP2402T)                                        *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE2401   CSECT                                                                  
*        FSPEC READ,CONTRACTS      <======= NO NEED TO READ CONTRACTS           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREP2401T06/04/98'                                      
         END                                                                    
