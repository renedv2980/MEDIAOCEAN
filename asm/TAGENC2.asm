*          DATA SET TAGENC2    AT LEVEL 004 AS OF 07/20/12                      
*PHASE T702C2C                                                                  
         TITLE 'T702C2 - SESSION ESTIMATE RECORD LIST'                          
T702C2   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702C2                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         SPACE 3                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   *+16                                                             
         LA    R2,LISTAR                                                        
         BAS   RE,LREC                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         XC    KEY,KEY             INSURE START REPORT FROM BEG OF LIST         
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P+1                                                           
         BAS   RE,LREC                                                          
         B     XIT                                                              
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY                                          
         SPACE 1                                                                
VKEY     NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'40',SSSAGYH),(X'80',SSSSTRH)                      
         BE    VKX                                                              
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',SSSAGYH)  VALIDATE AGENCY             
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         SPACE 1                                                                
         XC    TGCLI,TGCLI                                                      
         CLI   SSSCLIH+5,0                  IF PRESENT                          
         BE    VK10                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,SSSCLIH  VALIDATE CLIENT                     
         SPACE 1                                                                
VK10     XC    TGPRD,TGPRD                                                      
         CLI   SSSPRDH+5,0                  IF PRESENT                          
         BE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,SSSPRDH  VALIDATE PRODUCT                    
         SPACE 1                                                                
VK20     GOTO1 FLDVAL,DMCB,(X'20',SSSAGYH),(X'80',SSSSTRH)                      
         BAS   RE,INIT             AND INITIALIZE FOR SYSIO                     
VKX      B     XIT                                                              
         EJECT                                                                  
*              INITIALIZE FOR SYSIO                                             
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    KEY,KEY             INITIALIZE KEY                               
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM SCREEN           
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIFTYPE,TLSSTYPT    SET TO FILTER ON TV                          
         CLI   RECNUM,ET                                                        
         BE    *+8                                                              
         MVI   TIFTYPE,TLSSTYPR    ELSE FILTER ON RADIO                         
         MVC   TIFAGY,TGAGY        SET AGENCY                                   
         MVC   TIFCLI,TGCLI        SET CLIENT                                   
         MVC   TIFPRD,TGPRD        SET PRODUCT                                  
         MVC   TIQSTART,SSSSTR     AND START ESTIMATE                           
         MVI   TIREAD,TLSSCDQ      SET FOR READS                                
INITX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL LISTING RECORDS                               
         SPACE 1                                                                
*                                  R2=A(SCREEN LINE OR P)                       
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   PASS SOME MORE THINGS                        
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,15           N'LIST LINES                                 
         MVI   GLSTSTAT,RETEXTRA   OK TO RETURN EXTRA                           
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  PASS CONTROL TO SYSIO FOR LIST            
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF WE'RE SPOOLING                            
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       AND WE REPORTED SOMETHING                    
         BE    LRX                                                              
         BAS   RE,PRNTIT           SKIP A LINE ...                              
         SPACE 1                                                                
         EDIT  COUNTER,(8,P+1),COMMAS=YES,ALIGN=LEFT  AND PRINT TOTALS          
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(24,R1),=C'SESSION ESTIMATE RECORDS'                            
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING NOW                              
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS SYSIO HOOKS                                   
         SPACE 1                                                                
         USING LINED,R2            R2=A(DISPLAY LINE)                           
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         SPACE 1                                                                
         USING TLSSD,R3                                                         
         L     R3,TIAREC           R3=A(RECORD)                                 
         CLI   TLSSSEQ,0           REJECT IF NOT PRIMARY RECORD                 
         BNE   XIT                                                              
         SPACE 1                                                                
         USING TACTD,R4                                                         
         OC    TIFCLI,TIFCLI       IF FILTERING ON CLIENT                       
         BZ    LRH10                                                            
         LR    R4,R3                                                            
         MVI   ELCODE,TACTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         CLC   TACTCLI,TIFCLI      CLIENT ON RECORD MUST MATCH                  
         BNE   XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAPRD,R4                                                         
LRH10    OC    TIFPRD,TIFPRD       IF FILTERING ON PRODUCT                      
         BZ    LRH20                                                            
         LR    R4,R3                                                            
         MVI   ELCODE,TAPRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         CLC   TAPRPRD,TIFPRD      PRODUCT ON RECORD MUST MATCH                 
         BNE   XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
LRH20    MVC   LINEST,TLSSEST      DISPLAY ESTIMATE                             
         MVC   LINNAME,TINAME      AND NAME                                     
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRH30                                                            
         GOTO1 CATCHIOS            INSURE DON'T DO TOO MANY I/O'S               
         BAS   RE,PRNTIT           PRINT REPORT                                 
         AP    COUNTER,=P'1'                                                    
         B     LRHX                                                             
         SPACE 1                                                                
LRH30    MVC   DMDSKADD,TIDSKADD   SET DISK ADDRESS                             
         GOTO1 LISTMON             AND MOVE DISPLAY LINE TO SCREEN              
         SPACE 1                                                                
LRHX     B     XIT                 RETURN TO SYSIO                              
         EJECT                                                                  
*              ROUTINE TO SEND A LINE TO QUEUE                                  
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         MVC   HEAD4+9(6),SSSAGY                                                
         MVC   HEAD4+64(2),=C'TV'                                               
         CLI   RECNUM,ET                                                        
         BE    *+10                                                             
         MVC   HEAD4+64(5),=C'RADIO'                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS/EXITS                                                     
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 3                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SPROG 0,1                                                              
         SSPEC H1,2,RUN                                                         
         SSPEC H1,28,C'SESSION ESTIMATE RECORD LIST'                            
         SSPEC H2,28,C'----------------------------'                            
         SSPEC H1,58,REPORT                                                     
         SSPEC H1,71,PAGE                                                       
         SSPEC H2,58,REQUESTOR                                                  
         SSPEC H4,2,C'AGENCY'                                                   
         SSPEC H4,58,C'MEDIA'                                                   
         SPACE 1                                                                
         SSPEC H6,2,C'ESTIMATE             NAME'                                
         SSPEC H7,2,C'--------             ----'                                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER DISPLAY LINE                                      
         SPACE 1                                                                
LINED    DSECT                                                                  
LINDATA  DS    0CL(L'SSSDATA)                                                   
LINEST   DS    CL(L'TLSSEST)                                                    
         DS    CL1                                                              
LINNAME  DS    CL36                                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRC2D                                                       
         EJECT                                                                  
*              SAVED STORAGE AT END OF SCREEN                                   
         SPACE 1                                                                
COUNTER  DS    PL4                 LINE COUNTER                                 
         SPACE 3                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA                         MUST FOLLOW LAST SCREEN                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004TAGENC2   07/20/12'                                      
         END                                                                    
