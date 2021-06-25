*          DATA SET REREP2401  AT LEVEL 004 AS OF 03/31/93                      
*PHASE RE2401A,*                                                                
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
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE2401   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004REREP2401 03/31/93'                                      
         END                                                                    
