*          DATA SET REREP2401H AT LEVEL 006 AS OF 10/29/98                      
*PHASE RE2401H,*                                                                
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
***>>    FSPEC READ,CONTRACTS                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006REREP2401H10/29/98'                                      
         END                                                                    
