*          DATA SET SPREPR501  AT LEVEL 003 AS OF 08/05/99                      
*PHASE SPR501A,*                                                                
         TITLE 'SPR501 - SPECS FOR REP TO SPOT BUY XTRACT AUDIT '               
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR501 (SPR501) --- REPPAK TO SPOTPAK OVERNIGHT BUY TRNASFER   *          
*                          SPOT FILE UPDATING AND REPORT GENERATOR   *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  AUG28/91 (MRR) --- >INITIAL DEVELOPMENT                           *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
SPR501   CSECT                                                                  
         FSPEC USE,SP0003                                                       
*        FSPEC UPDATE,SPTDIR                                                    
*        FSPEC UPDATE,SPTFILE                                                   
*        FSPEC OPEN,DEMFILES                                                    
*                                                                               
*        ALL REPORT SPECS                                                       
*                                                                               
         SPROG 0                                                                
*                                                                               
         SSPEC H01,050,C'REPPAK TO SPOTPAK EXTRACT REPORT'                      
         SSPEC H02,050,C'--------------------------------'                      
*                                                                               
         SSPEC H04,095,REPORT                                                   
         SSPEC H04,118,PAGE                                                     
*                                                                               
*        DETAIL REPORTS SPEC                                                    
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPR501 08/05/99'                                      
         END                                                                    
