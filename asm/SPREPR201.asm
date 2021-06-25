*          DATA SET SPREPR201  AT LEVEL 003 AS OF 05/07/03                      
*PHASE SPR201A,*                                                                
         TITLE 'SPR201 - SPECS FOR REP TO SPOT BUY X-FER UPDATER'               
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR201 (SPR201) --- REPPAK TO SPOTPAK OVERNIGHT BUY TRNASFER   *          
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
SPR201   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC OPEN,DEMFILES                                                    
*                                                                               
*        ALL REPORT SPECS                                                       
*                                                                               
         SPROG 0,1,2,3                                                          
*                                                                               
         SSPEC H01,050,C'REPPAK TO SPOTPAK TRANSFER REPORT'                     
         SSPEC H02,050,C'---------------------------------'                     
*                                                                               
         SSPEC H03,001,REQUESTOR                                                
*                                                                               
         SSPEC H04,095,REPORT                                                   
         SSPEC H04,118,PAGE                                                     
*                                                                               
*        DETAIL REPORTS SPEC                                                    
*                                                                               
         SPROG 0,1                                                              
*                                                                               
         SSPEC H04,001,C'CLIENT'                                                
         SSPEC H04,042,C'REPPAK ADVERTISER'                                     
         SSPEC H05,001,C'PRODUCT'                                               
         SSPEC H05,049,C'PRODUCT'                                               
         SSPEC H06,001,C'ESTIMATE'                                              
         SSPEC H06,049,C'NETWORK CON#'                                          
*                                                                               
         SSPEC H09,001,18C'-'                                                   
         SSPEC H09,019,C'SPOTPAK'                                               
         SSPEC H09,026,17C'-'                                                   
         SSPEC H09,048,23C'-'                                                   
         SSPEC H09,070,C'REPPAK'                                                
         SSPEC H09,076,23C'-'                                                   
*                                                                               
         SSPEC H10,001,C'STATION LINE DATES'                                    
         SSPEC H10,026,C'SPOTS'                                                 
         SSPEC H10,036,C'DOLLARS'                                               
         SSPEC H10,048,C'REP AGENCY  STATION CONTRACT LINE SPOTS'               
         SSPEC H10,092,C'DOLLARS'                                               
         SSPEC H10,105,C'TRANSFER ERRORS'                                       
*                                                                               
         SSPEC H11,001,132C'-'                                                  
*                                                                               
*        ERROR RE-CAP REPORTS SPEC                                              
*                                                                               
         SPROG 2                                                                
*                                                                               
         SSPEC H03,054,C'* TRANSFER ERROR RECAP *'                              
*                                                                               
         SSPEC H06,001,18C'-'                                                   
         SSPEC H06,019,C'SPOTPAK'                                               
         SSPEC H06,026,17C'-'                                                   
         SSPEC H06,048,23C'-'                                                   
         SSPEC H06,070,C'REPPAK'                                                
         SSPEC H06,076,23C'-'                                                   
*                                                                               
         SSPEC H07,001,C'STATION LINE DATES'                                    
         SSPEC H07,026,C'SPOTS'                                                 
         SSPEC H07,036,C'DOLLARS'                                               
         SSPEC H07,048,C'REP AGENCY  STATION CONTRACT LINE SPOTS'               
         SSPEC H07,092,C'DOLLARS'                                               
         SSPEC H07,105,C'TRANSFER ERRORS'                                       
*                                                                               
         SSPEC H08,001,132C'-'                                                  
*                                                                               
*        END RUN MESSAGES REPORTS SPEC                                          
*                                                                               
         SPROG 3                                                                
*                                                                               
         SSPEC H03,056,C'* END RUN MESSAGES *'                                  
*                                                                               
         SSPEC H06,001,132C'-'                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPR201 05/07/03'                                      
         END                                                                    
