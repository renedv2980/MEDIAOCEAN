*          DATA SET SPREPR101  AT LEVEL 001 AS OF 10/22/91                      
*PHASE SPR101A,*                                                                
         TITLE 'SPR101 - SPECS FOR REP TO SPOT BUY X-FER EXTRACT'               
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR101 (SPR101) --- REPPAK TO SPOTPAK OVERNIGHT BUY TRNASFER   *          
*                          RECOVERY FILE EXTRACT PROGRAM             *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  JUL11/91 (MRR) --- >INITIAL DEVELOPMENT                           *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
SPR101   CSECT                                                                  
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H01,050,C'REPPAK TO SPOTPAK TRANSFER REPORT'                     
         SSPEC H02,050,C'---------------------------------'                     
         SSPEC H03,050,C'  (RECOVERY FILE EXTRACT PHASE)'                       
*                                                                               
         SSPEC H04,095,REPORT                                                   
         SSPEC H04,118,PAGE                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPR101 10/22/91'                                      
         END                                                                    
