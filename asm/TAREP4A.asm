*          DATA SET TAREP4A    AT LEVEL 046 AS OF 05/01/02                      
*PHASE T7034AA                                                                  
         TITLE 'T7034A - ESTIMATE PURGE REPORT'                                 
T7034A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 ESTRECL,T7034A,CLEAR=YES                                         
         LR    R3,RC                                                            
         SPACE 1                                                                
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
         USING WORKD,R7            R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         ST    R3,AESTREC          A(FULL ESTIMATE RECORD)                      
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE SCREEN                              
         BNE   *+12                                                             
         BAS   RE,VKEY             VALIDATE SCREEN FIELDS                       
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP             PRINT REPORT                                 
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
VKEY     NTR1                                                                   
         LR    RE,R7               CLEAR LOCAL W/S                              
         LA    RF,WORKLNQ                                                       
         XCEFL ,                                                                
*                                                                               
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
         XC    SEPCLIN,SEPCLIN     CLEAR OPTIONAL CLIENT/PRD NAMES              
         OI    SEPCLINH+6,X'80'                                                 
         XC    SEPPRDN,SEPPRDN                                                  
         OI    SEPPRDNH+6,X'80'                                                 
*                                                                               
         LA    R2,SEPAGYH          R2=A(AGENCY FIELD)                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SEPAGYNH                        
         MVC   TIFAGY,TGAGY                                                     
*                                                                               
         LA    R2,SEPDTEH          R2=A(LAST CHANGED DATE FIELD)                
         GOTO1 DTVAL,DMCB,LCHGDATE                                              
         BNE   FLDINV                                                           
         SPACE 1                                                                
         XC    BLOCK,BLOCK                                                      
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         MVC   WORK,SPACES                                                      
         MVC   WORK(8),=C'(-18M)-T'                                             
         GOTO1 PERVAL,DMCB,(8,WORK),('PVINSGLS',(R3))                           
         CLI   4(R1),PVRCOK                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   LCHGDATE,PVALPSTA   MUST BE MORE THAN 18 MTHS BACK               
         BNL   FLDINV                                                           
*                                                                               
         LA    R2,SEPCLIH          R2=A(CLIENT FIELD)                           
         XC    TGCLI,TGCLI                                                      
         CLI   5(R2),0                                                          
         BE    VKEY10                                                           
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SEPCLINH                        
*                                                                               
VKEY10   LA    R2,SEPPRDH          R2=A(PRODUCT FIELD)                          
         XC    TGPRD,TGPRD                                                      
         CLI   5(R2),0                                                          
         BE    VKEY20                                                           
         OC    TGCLI,TGCLI         IF NO CLIENT INPUT                           
         BNZ   VKEY15                                                           
         MVC   TGPRD,8(R2)         TAKE UNVALIDATED INPUT                       
         OC    TGPRD,SPACES                                                     
         B     VKEY20                                                           
VKEY15   GOTO1 RECVAL,DMCB,TLPRCDQ,(X'08',(R2)),SEPPRDNH                        
*                                                                               
VKEY20   LA    R2,SEPOPTH          R2=A(OPTIONS FIELD)                          
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
*                                  NTRY (R2) = A(OPTIONS FIELD)                 
         SPACE 1                                                                
VALOPT   NTR1                                                                   
         CLI   5(R2),0             IF OPTIONS FIELD INPUT                       
         BE    VOPTX                                                            
*                                                                               
         XC    BLOCK,BLOCK         CLEAR SCAN BLOCK                             
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
*                                                                               
VOPT2    CLC   =C'TRACE',SCDATA1   TRACE                                        
         BNE   VOPT4                                                            
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    EPOPTS,EPTRACE      SET TRACE ON                                 
         B     VOPT8                                                            
*                                                                               
VOPT4    CLC   =C'WRITE',SCDATA1   WRITE                                        
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         OI    EPOPTS,EPWRITEN     SET SOFT WRITE                               
         B     VOPT8                                                            
*                                                                               
VOPT8    LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT2            AND CONTINUE                                 
*                                                                               
VOPTX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
         ZAP   EPCNT,=P'0'         CLEAR DELETED COUNTER                        
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIHOOK,=A(IOHOOK)   A(I/O HOOK)                                  
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIREAD,TLESCDQ      READ ESTIMATE RECORDS                        
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
         SPACE 1                                                                
         OC    EPCNT,EPCNT         IF ANY ESTIMATE RECORDS DELETED              
         BZ    XIT                                                              
         BAS   RE,PRNTIT                                                        
         MVC   P+1(27),=CL27'ESTIMATE RECORDS DELETED ='                        
         EDIT  EPCNT,(12,P+30),ALIGN=LEFT,ZERO=NOBLANK                          
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO, DELETES ESTIMATE RECORDS             
*              FOR AGENCY/DATE SPECIFIED AND PRINTS REPORT                      
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF PROCESSING ESTIMATE RECORD                
         BNE   XIT                                                              
*                                                                               
         L     R4,TIAREC           R4=A(FILE RECORD)                            
         USING TLESD,R4                                                         
         CLI   TLESSEQ,0           IF BASE RECORD                               
         BNE   XIT                                                              
*                                                                               
         TM    EPOPTS,EPTRACE                                                   
         BZ    IOHOOK5                                                          
         GOTO1 HEXOUT,DMCB,TLESKEY,P,L'TLESKEY,=C'TOG'                          
         BAS   RE,PRNTIT                                                        
*                                                                               
IOHOOK5  BAS   RE,GETEREC          GET FULL ESTIMATE RECORD                     
         MVC   AIO,AESTREC         INTO AESTREC                                 
*                                                                               
         L     R4,AIO              R4=A(FULL ESTIMATE RECORD)                   
         BAS   RE,TSTDEL           TEST ELIGIBLE FOR DELETION                   
         BNE   IOHKX                                                            
*                                                                               
         BAS   RE,PRTDETS          PRINT ESTIMATE DETAILS                       
         GOTO1 MYTRACE,DMCB,=C'ESTREC',AIO,0                                    
*                                                                               
         AP    EPCNT,=P'1'         ADD TO DELETED COUNT                         
         TM    EPOPTS,EPWRITEN     IF WRITING TO FILE                           
         BO    IOHKX                                                            
         BAS   RE,DELETE           DELETE ESTIMATE RECORD                       
*                                                                               
IOHKX    OI    DMINBTS,X'08'       RE-READ SYSIO'S KEY                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TIKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    DMINBTS,X'F7'                                                    
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO GET AN ESTIMATE RECORD                                
*                                  RETURNS WITH RECORD IN AESTREC               
         SPACE 1                                                                
GETEREC  NTR1                                                                   
         MVC   KEY,TIKEY           GET ESTIMATE RECORD INTO AESTREC             
         GOTO1 HIGH                                                             
         SPACE 1                                                                
         LA    R4,KEY                                                           
         USING TLESD,R4                                                         
         CLC   TLESKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                DIE IF WE DIDN'T GET RECORD                  
         SPACE 1                                                                
         MVC   KEYSAVE,TLESKEY     SAVE ENTIRE DIR. REC. IN KEYSAVE             
         MVC   AIO,AESTREC         GET RECORD INTO AESTREC                      
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
GREC4    GOTO1 SEQ                             LOOK FOR ANOTHER RECORD          
         LA    R4,KEY                                                           
         CLC   TLESKEY(TLESSEQ-TLESD),KEYSAVE  WITH ALL SAME UP TO SEQ.         
         BNE   GRECX                                                            
         GOTO1 GETREC              GET NEW RECORD INTO IO1                      
         SPACE 1                                                                
         L     RE,AIO              NOW COMBINE THEM - RE=A(NEW RECORD)          
         L     R4,AESTREC                             R3=A(MAIN RECORD)         
         LH    RF,TLESLEN-TLESD(RE)  L'NEW RECORD                               
         SH    RF,DATADISP           LESS DATADISP                              
         SPACE 1                                                                
         LH    R1,TLESLEN                                                       
         BCTR  R1,0                                                             
         LR    R0,R1               R0=R1=L'MAIN RECORD (-1 FOR EOR)             
         SPACE 1                                                                
         AR    R1,RF               PLUS L'NEW RECORD                            
         STH   R1,TLESLEN          IS L'COMBINED RECORD                         
         SPACE 1                                                                
         AR    R0,R4               R0=A(END OF MAIN RECORD)                     
         AH    RE,DATADISP         RE=A(1ST EL. IN NEW RECORD)                  
         LR    R1,RF               R1=RF=L'NEW RECORD ELEMENTS                  
         SPACE 1                                                                
         MVCL  R0,RE               MOVE NEW RECORD AFTER MAIN                   
         B     GREC4               LOOK FOR ANOTHER                             
         SPACE 1                                                                
GRECX    MVC   KEY,KEYSAVE         RESTORE ORIG. DIR. REC. TO KEY               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE CHECKS ESTIMATE RECORD ELIGIBLE FOR DELETION             
*                                  XIT - CC EQ IS OKAY TO DELETE                
*                                        RECDATE=LCHG DATE ON RECORD            
         SPACE 1                                                                
TSTDEL   NTR1                                                                   
         OC    TGCLI,TGCLI         IF CLIENT SPECIFIED                          
         BZ    *+12                                                             
         BAS   RE,TSTCLI           CHECK CLIENT MATCH                           
         BNE   NO                                                               
*                                                                               
         OC    TGPRD,TGPRD         IF PRODUCT SPECIFIED                         
         BZ    *+12                                                             
         BAS   RE,TSTPRD           CHECK PRODUCT MATCH                          
         BNE   NO                                                               
*                                                                               
         XC    RECDATE,RECDATE     PRECLEAR DATE ON RECORD                      
*                                                                               
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
TSTDEL5  BAS   RE,NEXTEL                                                        
         BNE   TSTDEL9                                                          
         SPACE 1                                                                
         USING TAACD,R4                                                         
         CLI   TAACSCR,0           MATCH ON MAINT. ELEMENT                      
         BNE   TSTDEL5                                                          
         CLC   TAACCDTE,RECDATE                                                 
         BL    TSTDEL5                                                          
         MVC   RECDATE,TAACCDTE    FIND LATEST ACTIVITY DATE                    
         B     TSTDEL5                                                          
*                                                                               
TSTDEL9  CLC   RECDATE,LCHGDATE    REJECT RECORDS W/ HIGHER ACTIVITY            
         BH    NO                  DON'T DELETE                                 
         B     YES                 DELETE                                       
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINES CHECK FOR CLIENT AND/OR PRODUCT MATCH                   
         SPACE 1                                                                
TSTCLI   NTR1                                                                   
         MVI   WORK,TAESTCLI       FILTER ON CLIENT                             
         MVC   WORK+1(L'TGCLI),TGCLI                                            
         B     TSTEL                                                            
         SPACE 1                                                                
TSTPRD   NTR1                                                                   
         MVI   WORK,TAESTPRD       FILTER ON PRODUCT                            
         MVC   WORK+1(L'TGPRD),TGPRD                                            
         SPACE 1                                                                
TSTEL    MVI   ELCODE,0            LOOP THROUGH ELEMENTS                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
TSTEL5   BAS   RE,NEXTEL                                                        
         BNE   NO                                                               
         USING TAESD,R4                                                         
         CLC   WORK(1),TAESTYPE    IF CLIENT OR PRODUCT ELEMENT                 
         BNE   TSTEL5                                                           
         CLC   WORK+1(6),TAESDATA                                               
         BNE   TSTEL5                                                           
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE DELETES AN ESTIMATE RECORD                               
         SPACE 1                                                                
         USING TLESD,R4                                                         
DELETE   NTR1                                                                   
         MVC   KEY,TLESKEY         SET KEY                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLESKEY),KEYSAVE                                           
         BE    DEL4                                                             
         DC    H'0'                DIE IF WE DIDN'T GET RECORD                  
         SPACE 1                                                                
DEL2     GOTO1 HIGH                RE-READ RECORD WE JUST WROTE                 
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 SEQ                 GET NEXT                                     
         SPACE 1                                                                
         CLC   KEY(TLESSEQ-TLESD),KEYSAVE  TEST STILL SAME ESTIMATE             
         BNE   XIT                                                              
         SPACE 1                                                                
DEL4     MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD                               
         OI    TLESSTAT,X'80'      MARK IT DELETED                              
         GOTO1 PUTREC              AND WRITE IT BACK                            
         SPACE 1                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  MARK DIRECTORY DELETED                 
         GOTO1 WRITE                     AND WRITE IT BACK                      
         OI    DMINBTS,X'08'       SET TO READ DELETED                          
         B     DEL2                LOOK FOR MORE                                
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT ESTIMATE DETAILS                                
         SPACE 1                                                                
         USING TLESD,R4                                                         
PRTDETS  NTR1                                                                   
         LA    R2,P+1              PRINT ESTIMATE DETAILS                       
         USING PRNTD,R2                                                         
         MVC   PEST,TLESEST                                                     
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   PESTNAME,TGNAME                                                  
         GOTO1 DATCON,DMCB,(1,RECDATE),(8,PLCHGDT)                              
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         DROP  R2,R4                                                            
         SPACE 2                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO TRACE RECORDS                                         
         SPACE 1                                                                
MYTRACE  NTR1                                                                   
         TM    EPOPTS,EPTRACE      TEST TRACE ENABLED                           
         BZ    MYTRACEX                                                         
         LM    R2,R3,0(R1)         R2=A(LITERAL),R3=A(I/O AREA)                 
         ZIC   R4,0(R1)            R4=L'LITERAL                                 
         GOTO1 TRACE,DMCB,(R3),0,(R2),(R4)                                      
MYTRACEX B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DISPLAY HEADLINE DETAILS                              
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   H4+9(L'TIFAGY),TIFAGY                                            
         MVC   H4+16(L'SEPAGYN),SEPAGYN                                         
         OC    TGCLI,TGCLI                                                      
         BZ    HOOK10                                                           
         MVC   H5+1(6),=C'CLIENT'                                               
         MVC   H5+9(L'TGCLI),TGCLI                                              
         MVC   H5+16(L'SEPCLIN),SEPCLIN                                         
HOOK10   OC    TGPRD,TGPRD                                                      
         BZ    HOOK20                                                           
         MVC   H6+1(7),=C'PRODUCT'                                              
         MVC   H6+9(L'TGPRD),TGPRD                                              
         MVC   H6+16(L'SEPPRDN),SEPPRDN                                         
*                                                                               
HOOK20   GOTO1 DATCON,DMCB,(1,LCHGDATE),(8,H4+113)                              
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
INVSOON  MVI   ERROR,ERSOONRQ      INVALID SOON REQUEST                         
         LA    R2,SEPAGYH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 1                                                                
YES      SR    RC,RC               SET CONDITION CODES                          
NO       LTR   RC,RC                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,50,C'PURGED ESTIMATES'                                        
         SSPEC H2,50,C'----------------'                                        
         SSPEC H1,100,REQUESTOR                                                 
         SSPEC H2,100,REPORT                                                    
         SSPEC H2,120,PAGE                                                      
         SSPEC H4,2,C'AGENCY'                                                   
         SSPEC H4,100,C'ACTIVITY DATE'                                          
         SSPEC H8,2,C'ESTIMATE             NAME'                                
         SSPEC H9,2,C'--------             ----'                                
         SSPEC H8,60,C'LAST CHANGED'                                            
         SSPEC H9,60,C'------------'                                            
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRNTD    DSECT                                                                  
PEST     DS    CL(L'TLESEST)                                                    
         DS    CL1                                                              
PESTNAME DS    CL36                                                             
         DS    CL1                                                              
PLCHGDT  DS    CL8                                                              
PRNTLNQ  EQU   *-PRNTD                                                          
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
WORKD    DSECT                                                                  
         DS    0A                                                               
AESTREC  DS    A                   A(ESTIMATE RECORD)                           
*                                                                               
EPCNT    DS    PL8                 ESTIMATE DELETE COUNT                        
*                                                                               
EPOPTS   DS    XL1                 OPTIONS                                      
EPTRACE  EQU   X'80'               TRACE ACTIVE                                 
EPWRITEN EQU   X'40'               WRITE=N (DON'T MARK THE FILE)                
*                                                                               
LCHGDATE DS    XL3                 LAST CHANGED DATE REQUESTED                  
RECDATE  DS    XL3                 LAST CHANGED DATE ON EST RECORD              
*                                                                               
ESTRECL  EQU   6000                L'FULL ESTIMATE RECORD                       
WORKLNQ  EQU   *-WORKD                                                          
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPCAD                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDSPOOLD                                                                      
* DDWIDED                                                                       
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046TAREP4A   05/01/02'                                      
         END                                                                    
