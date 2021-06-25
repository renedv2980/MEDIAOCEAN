*          DATA SET REREPTA01  AT LEVEL 002 AS OF 01/15/97                      
*PHASE RETA01A,*                                                                
         TITLE 'RETA01 - SPECS FOR MASTER TURNAROUND REQUEST'                   
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                    *          
*  REREPTA01 (RETA01) --- READ REP RECOVERY FILE AND GENERATE MASTER *          
*                          TURNAROUND WHENEVER DEMO TRACK CHANGED    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  JAN15/97 (BOB) --- >INITIAL DEVELOPMENT                           *          
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
RETA01   CSECT                                                                  
****     FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H01,055,C'REP MASTER TURNAROUNDS'                                
         SSPEC H02,055,C'----------------------'                                
         SSPEC H03,055,C'  (RECOVERY FILE EXTRACT PHASE)'                       
*                                                                               
         SSPEC H04,095,REPORT                                                   
         SSPEC H04,118,PAGE                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REREPTA01 01/15/97'                                      
         END                                                                    
