*          DATA SET SPREPR601  AT LEVEL 015 AS OF 05/18/00                      
*PHASE SPR601A,*                                                                
         TITLE 'SPR601 - SPECS FOR REP TO SPOT BUY XTRACT AUDIT '               
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR601 (SPR601) --- RTS SPOT EXTRACT TO CREATE FILE FOR        *          
*                         AUDIT PURPOSES                             *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  MAR17/00 (BU ) --- >INITIAL DEVELOPMENT                           *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
SPR601   CSECT                                                                  
         FSPEC USE,SP0003                                                       
*        FSPEC UPDATE,SPTDIR                                                    
*        FSPEC UPDATE,SPTFILE                                                   
*        FSPEC OPEN,DEMFILES                                                    
*                                                                               
*        ALL REPORT SPECS                                                       
*                                                                               
         SPROG 0                                                                
*                                                                               
         SSPEC H01,002,AGYNAME                                                  
         SSPEC H01,050,C'REPPAK TO SPOTPAK  AUDIT  REPORT'                      
         SSPEC H01,095,REPORT                                                   
         SSPEC H01,118,PAGE                                                     
*                                                                               
         SSPEC H02,050,C'--------------------------------'                      
         SSPEC H02,002,REQUESTOR                                                
*                                                                               
*                                                                               
         SPROG 0                                                                
*                                                                               
*                        0       1         2         3         4                
*                        2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.           
         SSPEC H04,002,C'  SPOT  SPOT   SPOT  STATION   REP      REP '          
         SSPEC H05,002,C'CLIENT PRODUCT EST           CONTRACT   BUY '          
         SSPEC H06,002,C' CODE   CODE   NUM            NUMBER    NUM '          
*                        4   5         6         7         8       9            
*                        6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.8.0.           
         SSPEC H04,046,C'  SPOT  REP  TRANSFER CANCEL? SPOTS  '                 
         SSPEC H05,046,C'  BUY   CODE   DATE            PER   '                 
         SSPEC H06,046,C'  NUM  *FLIGHT START - END*   WEEK   '                 
*                                                                               
*                        8      9         0         1       2                   
*                        .4.6.8.0.2.4.6.8.0.2.4.6.8.0.2.4.8.0.                  
         SSPEC H04,089,C'PROBLEM    '                                           
         SSPEC H05,089,C'DESCRIPTION'                                           
*                                                                               
         SPROG 1                                                                
*                                                                               
         SSPEC H01,002,AGYNAME                                                  
         SSPEC H01,050,C'REPPAK TO SPOTPAK  AUDIT  REPORT'                      
         SSPEC H01,095,REPORT                                                   
         SSPEC H01,118,PAGE                                                     
*                                                                               
         SSPEC H02,050,C'--------------------------------'                      
         SSPEC H02,002,REQUESTOR                                                
*                                                                               
         SSPEC H03,050,C'---AUDIT REPORT RECORD COUNTS---'                      
*                                                                               
*                                                                               
*        DETAIL REPORTS SPEC                                                    
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPR601 05/18/00'                                      
         END                                                                    
