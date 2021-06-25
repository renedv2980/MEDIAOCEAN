*          DATA SET REREP2301  AT LEVEL 005 AS OF 08/31/00                      
*          DATA SET REREP2301  AT LEVEL 004 AS OF 11/16/99                      
*PHASE RE2301A                                                                  
         TITLE 'REREP2301 (RE2301) --- SPECS FOR RATE CARD MAINT.'              
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP2301 - RE2301 - RATE CARD MAINTENANCE                      *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 11NOV99 (SCH) DATE OF CONCEPTION                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE2301   CSECT                                                                  
*        FSPEC READ,CONTRACTS                                                   
*        RSPEC REQUEST,NOREP                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREP2301 08/31/00'                                      
         END                                                                    
