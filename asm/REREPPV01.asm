*          DATA SET REREPPV01  AT LEVEL 005 AS OF 09/18/96                      
*PHASE REPV01A,*                                                                
         TITLE 'REREPPV01 (REPV01) --- SPECS FOR PETRY WKYC BUG FIX'            
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREPPV01 - REPV01 - FIX PETRY WKYC RECORDS                     *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 18SEP96 (SKU) DATE OF CONCEPTION                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
REPV01   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREPPV01 09/18/96'                                      
         END                                                                    
