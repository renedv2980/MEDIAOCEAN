*          DATA SET TAREP1B    AT LEVEL 054 AS OF 03/12/15                      
*PHASE T7031BD,*                                                                
         TITLE 'T7031B - UNEMPLOYMENT EDIT'                                     
T7031B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T7031B,R7,R8                                       
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         CLI   RECNUM,UCDISK       FOR UC DISK                                  
         BE    *+12                                                             
         CLI   RECNUM,UCTAPES      AND FOR UC TAPES                             
         BNE   *+8                                                              
         MVI   PQSW,2              SET TO NOT OPEN PQ                           
         B     XIT                                                              
         SPACE 1                                                                
*                                  PROGRAM IS HANDLING THE FOLLOWING            
UREPORT  EQU   37                  UNEMPLOYMENT REPORT                          
UCTAPES  EQU   46                  UC TAPES                                     
UC941    EQU   47                  941 FORMS                                    
AGYPROF  EQU   48                  AGENCY PROFITABILITY                         
NYDISK   EQU   65                  NY DISK                                      
UCDISK   EQU   72                  UC DISK                                      
         EJECT                                                                  
       ++INCLUDE TAREP1BE                                                       
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MINVALUE DS    PL8                                                              
TRALIMIT DS    PL6                                                              
RECLIMIT DS    PL6                                                              
REPLIMIT DS    PL6                                                              
APERH    DS    A                                                                
*                                  OPTIONS                                      
WRITAPE  DS    CL1                 N = DON'T WRITE TAPE                         
TRACOPT  DS    CL1                 Y=TRACE                                      
UTRACE   DS    CL1                                                              
SSSUI    DS    CL1                 PRINT PERFORMERS SUI WAGES                   
GREYOPT  DS    CL1                                                              
READOPT  DS    CL1                 D OR T                                       
DOLLOPT  DS    CL1                 D OR T                                       
AMENDTAP DS    CL1                 Y = AMENDED DATA NY TAPE                     
EXCESS   DS    CL1                 PRINT PERFORMERS EXCESS WAGES                
OTHBRK   DS    CL1                                                              
OMNTHS   DS    CL1                 SHOW MONTHS IN DETAIL                        
EXEMP    DS    CL1                 SHOW EXEMPTIONS                              
*PROCOPT DS    CL1                 PROCESS TYPE - TEST OR PRODUCTION            
STSTAT   DS    XL1                 STATE STATUS - CAN ONLY BE ONE               
STSTATFL EQU   X'80'               TIFUNIT=FL                                   
STSTATTN EQU   X'40'               TIFUNIT=TN AND USING FL FORMAT               
STSTATIL EQU   X'20'               TIFUNIT=IL MONTHLY FORMAT                    
STSTATM1 EQU   X'10'               MAINE WAGE                                   
STSTATM2 EQU   X'08'               MAINE WITH                                   
MYEND    DS    0D                                                               
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*DDTWADCONS                                                                     
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
         DSECT                                                                  
       ++INCLUDE TALIMD                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPEBD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPCBD                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054TAREP1B   03/12/15'                                      
         END                                                                    
