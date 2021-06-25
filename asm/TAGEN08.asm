*          DATA SET TAGEN08    AT LEVEL 035 AS OF 01/16/14                      
*PHASE T70208C,*                                                                
*                                                                               
         TITLE 'TAGEN08 - TABLES FOR ONLINE ESTIMATING'                         
T70208   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         DC    AL4(GUARTAB-T70208)  GUARANTEE TABLE                             
         DC    AL4(COMLTAB-T70208)  ACTUAL COMMERCIAL TABLE                     
         DC    AL4(HCOMTAB-T70208)  HYPOTHETICAL COMMERCIAL TABLE               
         DC    AL4(CASTTAB-T70208)  CAST TABLE                                  
         DC    AL4(HOLDTAB-T70208)  HOLD TABLE                                  
         DC    AL4(HISTTAB-T70208)  USAGE HISTORY TABLE                         
         DC    AL4(ASOFCB-T70208)   AS/OF DATE CONTROL BLOCK                    
         DC    AL4(0)               WORK CODE TABLE - NOT USED                  
         DC    AL4(0)               CLASS A TABLE   - NOT USED                  
         DC    AL4(0)               PAX TABLE       - NOT USED                  
         DC    AL4(0)               WILDSPOT TABLE  - NOT USED                  
         DC    AL4(RBLK-T70208)     REPORT CONTROL BLOCK (FAREPORT)             
         DC    AL4(GLOBAL-T70208)   GLOBAL AREA FOR DROOL                       
*                                                                               
         DS    0D                   GUARANTEE TABLE                             
         DC    C'**GUAR**'                                                      
GUARTAB  DC    AL4(NGUARS)          MAX. NUMBER OF RECORDS                      
         DC    (NGUARS*GUARLNQ)X'00'                                            
NGUARS   EQU   50                                                               
*                                                                               
         DS    0D                   ACTUAL COMMERCIAL TABLE                     
         DC    C'**COML**'                                                      
COMLTAB  DC    AL4(NCOMLS)          MAX. NUMBER OF RECORDS                      
         DC    (NCOMLS*4)X'00'                                                  
NCOMLS   EQU   75                                                               
*                                                                               
         DS    0D                   HYPOTHETICAL COMMERCIAL TABLE               
         DC    C'**HCOM**'                                                      
HCOMTAB  DC    AL4(NHCOMS)          MAX. NUMBER OF RECORDS                      
         DC    (NHCOMS*HCOMLNQ)X'00'                                            
NHCOMS   EQU   50                                                               
*                                                                               
         DS    0D                   CAST TABLE                                  
         DC    C'**CAST**'                                                      
CASTTAB  DC    AL4(NCAST)           MAX. NUMBER OF RECORDS                      
         DC    (NCAST*2)X'00'                                                   
NCAST    EQU   250                                                              
*                                                                               
         DS    0D                   HOLD TABLE                                  
         DC    C'**HOLD**'                                                      
HOLDTAB  DC    AL4(NHOLD)           MAX. NUMBER OF RECORDS                      
         DC    (NHOLD*HOLDLNQ)X'00'                                             
NHOLD    EQU   50                                                               
*                                                                               
         DS    0D                   USAGE HISTORY TABLE                         
         DC    C'**HIST**'                                                      
HISTTAB  DC    AL4(NHISTS)          MAX. NUMBER OF RECORDS                      
         DC    (NHISTS*HISTLNQ)X'00'                                            
NHISTS   EQU   22                                                               
         EJECT                                                                  
         DS    0D                   AS/OF DATE CONTROL BLOCK                    
         DC    C'**ASOF**'                                                      
ASOFCB   DC    AL4(NASOFS)          MAX. NUMBER OF RECORDS                      
         DC    (NASOFS*ASOFLNQ)X'00'                                            
NASOFS   EQU   4                                                                
*                                                                               
         DS    0D                   REPORT CONTROL BLOCK FOR FAREPORT           
         DC    C'*REPBLK*'                                                      
RBLK     DC    (REPBLKL)X'00'                                                   
*                                                                               
         DS    0D                   GLOBAL STORAGE FOR DROOL                    
         DC    C'*GLOBAL*'                                                      
GLOBAL   DC    (LOCALEND-GLOBALD)X'00'                                          
         SPACE 2                                                                
*                                                                               
*TAESTDSECT                                                                     
*DRGLOBAL                                                                       
*DROOLLOCAL                                                                     
*FAREPBLK                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAESTDSECT                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DROOLLOCAL                                                     
         DSECT                                                                  
       ++INCLUDE FAREPBLK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035TAGEN08   01/16/14'                                      
         END                                                                    
