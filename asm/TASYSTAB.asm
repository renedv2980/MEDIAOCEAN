*          DATA SET TASYSTAB   AT LEVEL 234 AS OF 03/07/17                      
*PHASE T00A88B,*                                                                
         TITLE 'T00A88 - TASYSTAB - TALENT SYSTEM TABLES'                       
TASYSTAB CSECT                                                                  
         SPACE 1                                                                
         DC    AL4(TAUNITS-TASYSTAB)  TALENT FEDERAL/STATE UNIT CODES           
         DC    AL4(TACTYPS-TASYSTAB)  CANADIAN COMMERCIAL TYPES                 
         DC    AL4(TAMEDS-TASYSTAB)   MEDIA CODES                               
         DC    AL4(TAUNIS-TASYSTAB)   UNION CODES                               
         DC    AL4(TAYEARS-TASYSTAB)  CONTRACT YEARS                            
         DC    AL4(TACATS-TASYSTAB)   CATEGORY CODES                            
         DC    AL4(TAUPGRS-TASYSTAB)  UPGRADES                                  
         DC    AL4(TAMAJS-TASYSTAB)   MAJORS                                    
         DC    AL4(TAUSES-TASYSTAB)   USE TYPES                                 
         DC    AL4(TACOMS-TASYSTAB)   COMMERCIAL TYPES                          
         DC    AL4(TABTYPS-TASYSTAB)  BILLING TYPES                             
         DC    AL4(TAAPPLS-TASYSTAB)  APPLIED CODES                             
         DC    AL4(TAIERRS-TASYSTAB)  INVOICE ERROR MESSAGES                    
         DC    AL4(TASTAFS-TASYSTAB)  STAFF CODES                               
         DC    AL4(TACKLOCK-TASYSTAB) URGENT CHECK RUN LOCKOUT STATUS           
         DC    AL4(TALICS-TASYSTAB)   MUSIC LICENSER CODES                      
         DC    AL4(TAADJ-TASYSTAB)    ADJUSTMENT CODES                          
         DC    AL4(TACERRS-TASYSTAB)  CAST ERROR CODES                          
         DC    AL4(TAGERACT-TASYSTAB) $GEN RECACT TABLE                         
         DC    AL4(TARERACT-TASYSTAB) $REP RECACT TABLE                         
         DC    AL4(TATHRES-TASYSTAB)  THRESHOLDS BY AIRDATE                     
         DC    AL4(TALOCS-TASYSTAB)   FGR LOCATIONS                             
         DC    AL4(TAEDTYP-TASYSTAB)  EDIT TYPES                                
         DC    AL4(TACTRYS-TASYSTAB)  COUNTRIES AND PROVINCES                   
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(TACONTS-TASYSTAB)  CONTRACT TYPES                            
         SPACE 3                                                                
         DC    C'*CK*'             URGENT CHECK RUN LOCKOUT STATUS              
TACKLOCK DC    X'00'               SEE TASYSEQUS FOR BIT SETTINGS               
         EJECT                                                                  
       ++INCLUDE TASYSUNITS                                                     
         EJECT                                                                  
       ++INCLUDE TASYSCCTYP                                                     
         EJECT                                                                  
       ++INCLUDE TASYSMEDS                                                      
         EJECT                                                                  
       ++INCLUDE TASYSUNIS                                                      
         EJECT                                                                  
       ++INCLUDE TASYSYEARS                                                     
         EJECT                                                                  
       ++INCLUDE TASYSCATS                                                      
         EJECT                                                                  
       ++INCLUDE TASYSUPGRS                                                     
         EJECT                                                                  
       ++INCLUDE TASYSMAJS                                                      
         EJECT                                                                  
       ++INCLUDE TASYSUSES                                                      
         EJECT                                                                  
       ++INCLUDE TASYSLICS                                                      
         EJECT                                                                  
       ++INCLUDE TASYSCTYP                                                      
         EJECT                                                                  
       ++INCLUDE TASYSBTYPS                                                     
         EJECT                                                                  
       ++INCLUDE TASYSAPPLS                                                     
         EJECT                                                                  
       ++INCLUDE TASYSIERR                                                      
         EJECT                                                                  
       ++INCLUDE TASYSSTAFS                                                     
         EJECT                                                                  
       ++INCLUDE TASYSADJS                                                      
         EJECT                                                                  
       ++INCLUDE TASYSCERR                                                      
         EJECT                                                                  
       ++INCLUDE TAGENRACT                                                      
         EJECT                                                                  
       ++INCLUDE TAREPRACT                                                      
         EJECT                                                                  
       ++INCLUDE TASYSTHRES                                                     
         EJECT                                                                  
       ++INCLUDE TASYSLOCS                                                      
         EJECT                                                                  
       ++INCLUDE TASYSEDT                                                       
         EJECT                                                                  
       ++INCLUDE TASYSCTRY                                                      
         EJECT                                                                  
       ++INCLUDE TASYSCONT                                                      
         EJECT                                                                  
       ++INCLUDE TASYSDSECT                                                     
         EJECT                                                                  
       ++INCLUDE TASYSEQUS                                                      
         EJECT                                                                  
       ++INCLUDE TAGENEQUS                                                      
         EJECT                                                                  
* TAGENFILE                                                                     
* DDSPLWORKD                                                                    
* DDGENTWA                                                                      
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPLWORKD                                                     
TWAD     DSECT                                                                  
         DS    CL64                                                             
CONHEADH EQU   *                                                                
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'234TASYSTAB  03/07/17'                                      
         END                                                                    
