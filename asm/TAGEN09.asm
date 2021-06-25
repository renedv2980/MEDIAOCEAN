*          DATA SET TAGEN09    AT LEVEL 061 AS OF 07/13/17                      
*PHASE T70209B,*                                                                
*INCLUDE TAINTER                                                                
*INCLUDE TALIM                                                                  
*                                                                               
         TITLE 'TAGEN09 - TABLES FOR OFFLINE ESTIMATING'                        
T70209   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         DC    AL4(GUARTAB-T70209)  GUARANTEE TABLE                             
         DC    AL4(COMLTAB-T70209)  ACTUAL COMMERCIAL TABLE                     
         DC    AL4(HCOMTAB-T70209)  HYPOTHETICAL COMMERCIAL TABLE               
         DC    AL4(CASTTAB-T70209)  CAST TABLE                                  
         DC    AL4(HOLDTAB-T70209)  HOLD TABLE                                  
         DC    AL4(HISTTAB-T70209)  USAGE HISTORY TABLE                         
         DC    AL4(ASOFCB-T70209)   AS/OF DATE CONTROL BLOCK                    
         DC    AL4(WCTAB-T70209)    WORK-CODE TABLE                             
         DC    AL4(CLATAB-T70209)   CLASS A TABLE                               
         DC    AL4(PAXTAB-T70209)   PAX TABLE                                   
         DC    AL4(WSPTAB-T70209)   WILDSPOT TABLE                              
         DC    V(TAINTER)           ADDRESS OF TAINTER ROUTINE                  
         DC    V(TALIM)             ADDRESS OF TALIM ROUTINE                    
*                                                                               
         DS    0D                   GUARANTEE TABLE                             
         DC    C'**GUAR**'                                                      
GUARTAB  DC    AL4(NGUARS)          MAX. NUMBER OF RECORDS                      
         DC    (NGUARS*GUARLNQ)X'00'                                            
NGUARS   EQU   300                                                              
*                                                                               
         DS    0D                   ACTUAL COMMERCIAL TABLE                     
         DC    C'**COML**'                                                      
COMLTAB  DC    AL4(NCOMLS)          MAX. NUMBER OF RECORDS                      
         DC    (NCOMLS*4)X'00'                                                  
NCOMLS   EQU   400                                                              
*                                                                               
         DS    0D                   HYPOTHETICAL COMMERCIAL TABLE               
         DC    C'**HCOM**'                                                      
HCOMTAB  DC    AL4(NHCOMS)          MAX. NUMBER OF RECORDS                      
         DC    (NHCOMS*HCOMLNQ)X'00'                                            
NHCOMS   EQU   200                                                              
*                                                                               
         DS    0D                   CAST TABLE                                  
         DC    C'**CAST**'                                                      
CASTTAB  DC    AL4(NCAST)           MAX. NUMBER OF RECORDS                      
         DC    (NCAST*2)X'00'                                                   
NCAST    EQU   300                                                              
*                                                                               
         DS    0D                   HOLD TABLE                                  
         DC    C'**HOLD**'                                                      
HOLDTAB  DC    AL4(NHOLD)           MAX. NUMBER OF RECORDS                      
         DC    (NHOLD*HOLDLNQ)X'00'                                             
NHOLD    EQU   100                                                              
*                                                                               
         DS    0D                   USAGE HISTORY TABLE                         
         DC    C'**HIST**'                                                      
HISTTAB  DC    AL4(NHISTS)          MAX. NUMBER OF RECORDS                      
         DC    (NHISTS*HISTLNQ)X'00'                                            
NHISTS   EQU   50                                                               
*                                                                               
         DS    0D                   AS/OF DATE CONTROL BLOCK                    
         DC    C'**ASOF**'                                                      
ASOFCB   DC    AL4(NASOFS)          MAX. NUMBER OF RECORDS                      
         DC    (NASOFS*ASOFLNQ)X'00'                                            
NASOFS   EQU   4                                                                
*                                                                               
         DS    0D                   WORK-CODE TABLE                             
         DC    C'**WKCD**'                                                      
WCTAB    DC    AL4(NWKCD)           MAX. NUMBER OF RECORDS                      
         DC    (NWKCD*WCLNQ)X'00'                                               
NWKCD    EQU   (L'TNWCS/2)                                                      
*                                                                               
         DS    0D                   CLASS A TABLE                               
         DC    C'*CLATAB*'                                                      
CLATAB   DC    AL4(NCLA)            MAX. NUMBER OF RECORDS                      
         DC    (NCLA*CLALNQ)X'00'                                               
NCLA     EQU   100                                                              
*                                                                               
         DS    0D                   PAX TABLE                                   
         DC    C'*PAXTAB*'                                                      
PAXTAB   DC    AL4(NPAX)            MAX. NUMBER OF RECORDS                      
         DC    (NPAX*CLALNQ)X'00'                                               
NPAX     EQU   100                                                              
*                                                                               
         DS    0D                   WILDSPOT TABLE                              
         DC    C'*WSPTAB*'                                                      
WSPTAB   DC    AL4(NWSP)            MAX. NUMBER OF RECORDS                      
         DC    (NWSP*3)X'00'                                                    
NWSP     EQU   35                                                               
*                                                                               
*TAESTDSECT                                                                     
*TAINTERD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAESTDSECT                                                     
       ++INCLUDE TAINTERD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061TAGEN09   07/13/17'                                      
         END                                                                    
