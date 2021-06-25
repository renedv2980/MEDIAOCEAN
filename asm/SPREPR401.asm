*          DATA SET SPREPR401  AT LEVEL 001 AS OF 03/12/92                      
*PHASE SPR401A,*                                                                
         TITLE 'SPR401 - SPECS FOR REP TO SPOT FILE FIXER'                      
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR401 (SPR401) --- REPPAK TO SPOTPAK OVERNIGHT BUY TRNASFER   *          
*                          SPOT FILE FIXER                           *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  FEB19/92 (MRR) --- >INITIAL DEVELOPMENT                           *          
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
SPR401   CSECT                                                                  
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H01,050,C'REPPAK TO SPOTPAK TRANSFER REPORT'                     
         SSPEC H02,050,C'---------------------------------'                     
         SSPEC H03,050,C'        (SPOT FILE FIXER)        '                     
*                                                                               
         SSPEC H04,095,REPORT                                                   
         SSPEC H04,118,PAGE                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPR401 03/12/92'                                      
         END                                                                    
