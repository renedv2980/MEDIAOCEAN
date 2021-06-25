*          DATA SET SPREPR301  AT LEVEL 001 AS OF 10/22/91                      
*PHASE SPR301A,*                                                                
         TITLE 'SPR301 - SPECS FOR REP TO SPOT REP FILE UPDATE'                 
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR301 (SPR301) --- REPPAK TO SPOTPAK OVERNIGHT BUY TRNASFER   *          
*                          REP FILE UPDATE FROM SPOT FILE ADDS       *          
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
SPR301   CSECT                                                                  
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H01,050,C'REPPAK TO SPOTPAK TRANSFER REPORT'                     
         SSPEC H02,050,C'---------------------------------'                     
         SSPEC H03,050,C'        (REP FILE UPDATE)        '                     
*                                                                               
         SSPEC H04,095,REPORT                                                   
         SSPEC H04,118,PAGE                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPR301 10/22/91'                                      
         END                                                                    
