*          DATA SET TAREP39    AT LEVEL 006 AS OF 04/26/13                      
*PHASE T70339A                                                                  
         TITLE 'T70339 - DUE DATE REPORT'                                       
T70339   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70339                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING LOCALD,R7                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    *+12                                                             
         CLI   MODE,DISPREC        OR DISPLAY RECORD                            
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
*                                                                               
         TM    WHEN,X'40'          IF REPORTING 'NOW'                           
         BZ    XIT                                                              
         XC    CONSERV,CONSERV     AUTO CALL $DQU                               
         MVC   CONSERV(4),=C'$DQU'                                              
         OI    CONSERVH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,SDDPERH          VALIDATE PERIOD                              
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)                                                  
         USING PERVALD,R3                                                       
         MVC   BSTART,PVALBSTA     SAVE BINARY START DATE                       
         MVC   ESTART,PVALESTA     AND EBCDIC START DATE                        
         MVC   EEND,PVALEEND       AND EBCDIC END DATE                          
         MVC   DPERIOD,PVALCPER    AND DISPLAYABLE PERIOD                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         LA    R1,HOOK             INITIALIZE PRINTING FIELDS                   
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
*                                                                               
         L     R1,SYSPARMS         GET A(GETRET)                                
         L     R1,16(R1)                                                        
         USING COMFACSD,R1                                                      
         MVC   MYGETRET,CGETRET                                                 
*                                                                               
         LA    R3,WORK             R3=A(GETRET BLOCK)                           
         USING GETRETD,R3                                                       
         MVC   EDATE,ESTART        SET CURRENT EBCDIC DATE                      
*                                                                               
PR30     GOTO1 DATCON,DMCB,(0,EDATE),(3,BDATE)  SET CURRENT BINARY DATE         
         GOTO1 (RF),(R1),,(8,P+1)               DISPLAY ON PRINT LINE           
*                                                                               
         XC    WORK,WORK           INITIALIZE GETRET BLOCK                      
         MVC   GRDIDY(3),BDATE     SET START DATE                               
         MVC   GRDHRS,=Y(12*24)    SET +12 DAYS                                 
         OI    GRDFLAG,GRDFHLOK    SET START ON WEEKEND/HOLIDAY OK              
***      OI    GRDFLAG,GRDFTAL                                                  
         OI    GRDFLAG,GRDFTPU     USE UNION CALENDAR                           
         GOTO1 MYGETRET,(R3)       DETERMINE DUE DATE                           
                                                                                
         GOTOR UNIOND,DMCB,MYGETRET,(R3),PARAS                                  
         BE    *+6                 TP HOLIDAY                                   
         DC    H'0'                                                             
                                                                                
         GOTO1 DATCON,DMCB,(3,GRDODY),(8,P+12)  DISPLAY DUE DATE                
*                                                                               
         XC    WORK,WORK           INITIALIZE GETRET BLOCK                      
         MVC   GRDIDY(3),BDATE     SET START DATE                               
         MVC   GRDHRS,=Y(15*24)    SET +15 DAYS                                 
         OI    GRDFLAG,GRDFHLOK    SET START ON WEEKEND/HOLIDAY OK              
*****    OI    GRDFLAG,GRDFTAL                                                  
         OI    GRDFLAG,GRDFTPU     USE UNION CALENDAR                           
         GOTO1 MYGETRET,(R3)       DETERMINE DUE DATE                           
         GOTOR UNIOND,DMCB,MYGETRET,(R3),PARAS                                  
         BE    *+6                 TP HOLIDAY                                   
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,GRDODY),(8,P+23)  DISPLAY DUE DATE                
*                                                                               
         BAS   RE,PRNTIT           PRINT THE LINE                               
*                                                                               
         CLC   EDATE,EEND          IF HAVEN'T REACHED END YET                   
         BE    PRX                                                              
         GOTO1 ADDAY,DMCB,EDATE,EDATE,1  ADD 1 TO CURRENT DATE                  
         B     PR30                      AND LOOP BACK                          
*                                                                               
PRX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 2                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   HEAD4+9(L'DPERIOD),DPERIOD  DISPLAY REQUEST PERIOD               
         B     XIT                                                              
         EJECT                                                                  
*              EXITS                                                            
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,33,C'DUE DATE REPORT'                                         
         SSPEC H2,33,C'---------------'                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SSPEC H4,2,C'PERIOD'                                                   
         SPACE 1                                                                
         SSPEC H6,2,C'DATE       +12 DAYS   +15 DAYS'                           
         SSPEC H7,2,C'----       --------   --------'                           
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
       ++INCLUDE TAUNIOND                                                       
         EJECT                                                                  
LOCALD   DSECT                                                                  
MYGETRET DS    A                   A(GETRET)                                    
BSTART   DS    XL3                 BINARY START DATE                            
ESTART   DS    CL6                 EBCDIC START DATE                            
EEND     DS    CL6                 EBCDIC END DATE                              
DPERIOD  DS    CL17                DISPLAYABLE REQUEST PERIOD                   
BDATE    DS    XL3                 CURRENT BINARY DATE                          
EDATE    DS    CL6                 CURRENT EBCDIC DATE                          
LOCALX   EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPD9D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
* DDPERVALD                                                                     
* DDCOMFACS                                                                     
* DDGETRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGETRETD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006TAREP39   04/26/13'                                      
         END                                                                    
