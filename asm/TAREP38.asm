*          DATA SET TAREP38    AT LEVEL 014 AS OF 04/30/14                      
*PHASE T70338A                                                                  
         TITLE 'T70338 - INVOICE MAINT REPORT'                                  
T70338   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70338                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)            R7=A(LOCAL W/S)                              
         USING TMD,R7                                                           
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE SCREEN                              
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LH    RF,=AL2(TMLNQ)           INITIALIZE LOCAL STORAGE                
         XCEFL TMD                                                              
         ZAP   INVCOUNT,=P'0'                                                   
         ZAP   CHKCOUNT,=P'0'                                                   
         SPACE 1                                                                
         LH    RF,=AL2(TIEND-TASYSIOD)  INITIALIZE SYSIO STORAGE                
         XCEFL TASYSIOD                                                         
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SIMAGYH),SIMAGYNH   AGENCY            
         MVC   TIFAGY,TGAGY                                                     
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',SIMFCLIH),SIMFCLNH  FROM CLI          
         MVC   TIFCLI,TGCLI                                                     
         MVC   FROMCLI,TGCLI                                                    
         SPACE 1                                                                
         LA    R2,SIMFPRDH         TEST FOR PRODUCT                             
         XC    SIMFPRN,SIMFPRN                                                  
         OI    SIMFPRNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'08',(R2)),SIMFPRNH  FROM PRD              
         MVC   TIFPRD,TGPRD                                                     
         MVC   FROMPRD,TGPRD                                                    
         SPACE 1                                                                
VK10     LA    R2,SIMFESTH         FROM ESTIMATE                                
         CLI   5(R2),0             OPTIONAL                                     
         BE    VK20                                                             
         GOTO1 ANY                                                              
         CLI   5(R2),3                                                          
         BNE   *+14                                                             
         CLC   =C'ANY',WORK        IF INPUT C'ANY' THEN LEAVE                   
         BE    VK20                FROMEST CLEAR                                
         MVC   FROMEST,WORK                                                     
         SPACE 1                                                                
VK20     GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',SIMTCLIH),SIMTCLNH  TO CLI            
         MVC   TOCLI,TGCLI         NEW CLIENT                                   
         SPACE 1                                                                
         LA    R2,SIMTPRDH         TO PRODUCT                                   
         XC    SIMTPRN,SIMTPRN                                                  
         OI    SIMTPRNH+6,X'80'                                                 
         CLI   SIMFPRDH+5,0        IF THERE IS NO OLD PRODUCT                   
         BNE   *+12                                                             
         CLI   5(R2),0             THEN INPUT IS OPTIONAL                       
         BE    VK30                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'08',(R2)),SIMTPRNH  TO PRD                
         MVC   TOPRD,TGPRD                                                      
         SPACE 1                                                                
VK30     LA    R2,SIMTESTH         TO ESTIMATE                                  
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   SIMFESTH+5,0                                                     
         BNE   FLDMISS             IF INPUT FROM - 'TO' IS REQUIRED             
         B     VK40                                                             
         SPACE 1                                                                
         CLI   SIMFESTH+5,0        INPUT ALLOWED ONLY IF 'FROM' INPUT           
         BE    FLDINV                                                           
         GOTO1 ANY                                                              
         MVC   TOEST,WORK          SAVE SPACE-PADDED ESTIMATE NUMBER            
         SPACE 1                                                                
VK40     CLC   FROM,TO             DO NOT ALLOW USER TO CHANGE                  
         BNE   *+12                TO SAME CLI/PRD/EST                          
         LA    R2,SIMTCLIH                                                      
         B     FLDINV                                                           
         SPACE 1                                                                
         LA    R2,SIMINVSH         START INVOICE NUMBER                         
         CLI   SIMINVEH+5,0        (REQUIRED IF END INVOICE INPUT)              
         BE    VK50                                                             
         GOTO1 ANY                                                              
VK50     LA    R3,STRTINV                                                       
         BAS   RE,VALINV                                                        
         SPACE 1                                                                
         LA    R2,SIMINVEH         END INVOICE NUMBER                           
         LA    R3,ENDINV                                                        
         BAS   RE,VALINV                                                        
         SPACE 1                                                                
         OC    ENDINV,ENDINV       IF NO END INVOICE NUMBER INPUT               
         BNZ   *+10                                                             
         MVC   ENDINV,STRTINV      THEN SET END=START                           
         SPACE 1                                                                
         LA    R2,SIMASOFH         AS OF DATE                                   
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         GOTO1 DATVAL,DMCB,8(R2),WORK                                           
         OC    0(4,R1),0(R1)                                                    
         BZ    FLDINV                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,TIQPSTR)  SET START FOR SYSIO            
         SPACE 1                                                                
VK60     BAS   RE,VALOPT           OPTIONS                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE INVOICE NUMBER                               
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
*                                  R3=A(W/S FOR COMP. INVOICE NUMBER)           
VALINV   NTR1                                                                   
         XC    0(6,R3),0(R3)       PRE-CLEAR OUTPUT FIELD                       
         CLI   5(R2),0             GET OUT IF NO INPUT                          
         BE    VIX                                                              
         GOTO1 TINVCON,DMCB,8(R2),(R3),DATCON  TEST IF VALID INV NUMBER         
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         XC    0(6,R3),=6X'FF'                   COMPLEMENT IT                  
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'84',(R3))  INSURE RECORD ON FILE          
         BNE   THEEND                                                           
VIX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 1                                                                
VALOPT   NTR1                                                                   
         LA    R2,SIMOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         SPACE 1                                                                
VOPT10   DS    0H                                                               
         CLC   =C'TRACE',SCDATA1   TRACE                                        
         BNE   VOPT20                                                           
         OI    TMOPTS,TMTRACE      SET TRACE ON                                 
         B     VOPT100                                                          
         SPACE 1                                                                
VOPT20   DS    0H                                                               
         B     FLDINV                                                           
         SPACE 1                                                                
VOPT100  LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10           AND CONTINUE                                 
         SPACE 1                                                                
VOPTX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT                                          
         SPACE 1                                                                
PREP     NTR1                                                                   
         MVC   SPECS,=A(MYSPECS)   SET A(SPECS) FOR PRINTING                    
         MVC   HEADHOOK,=A(HOOK)   SET A(HEADLINE HOOK) FOR PRINTING            
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVC   TIHOOK,=A(IOHOOK)   A(I/O HOOK)                                  
         MVI   TIREAD,TLINDCDQ     READ INVOICE BILL DATE POINTERS              
         MVI   TISUBRD,TLCKCDQ     SUB-READ CHECK RECORDS                       
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
         SPACE 1                                                                
         MVC   P+01(18),=C'Invoices Changed ='                                  
         EDIT  INVCOUNT,(10,P+20),ALIGN=LEFT,ZERO=NOBLANK                       
         MVC   P2+1(18),=C'Checks Changed   ='                                  
         EDIT  CHKCOUNT,(10,P2+20),ALIGN=LEFT,ZERO=NOBLANK                      
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO                                       
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         MVC   AIO,TIAREC          SET A(RECORD) IN AIO                         
         MVC   DMDSKADD,TIDSKADD                                                
         GOTO1 ASAVPTRS,DMCB,OLDPTRS  SAVE CURRENT PASSIVE POINTERS             
         SPACE 1                                                                
         CLI   TIMODE,PROCINV      TEST THIS IS INVOICE RECORD HOOK             
         BNE   IOH50                                                            
         L     R4,AIO              R4=A(INVOICE RECORD)                         
         USING TLIND,R4                                                         
         OC    STRTINV,STRTINV     IF START INVOICE INPUT                       
         BZ    *+14                                                             
         CLC   TLININV,STRTINV     MUST BE LE (BECAUSE IT'S COMP.)              
         BH    IOHNO                                                            
         SPACE 1                                                                
         OC    ENDINV,ENDINV       IF END INVOICE INPUT                         
         BZ    *+14                                                             
         CLC   TLININV,ENDINV      MUST BE GE (BECAUSE IT'S COMP.)              
         BL    IOHNO                                                            
         SPACE 1                                                                
         CLI   SIMFESTH+5,0        SKIP IF THERE'S NO ESTIMATE CHANGE           
         BE    IOH60                                                            
         OC    FROMEST,FROMEST     IF FROM 'ANY' THEN OK TO CHANGE              
         BZ    IOH20                                                            
         MVI   ELCODE,TANUELQ      GET ESTIMATE NUMBER                          
         MVI   WORK,TANUTEST                                                    
         GOTO1 GETL,DMCB,(1,WORK)                                               
         BNE   IOHNO               NOT FOUND - SET TO NOT PASS CHECKS           
         USING TANUD,R4                                                         
         L     R4,TGELEM                                                        
         ZIC   R1,SIMFESTH+5       ELSE SET FOR COMPARE FOR L'INPUT             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FROMEST(0),TANUMBER                                              
         BNE   IOHNO               NOT EQUAL - SET TO NOT PASS CHECKS           
         SPACE 1                                                                
IOH20    GOTO1 DELL,DMCB,(1,WORK)  REMOVE EXISTING ELEMENT                      
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     NOW BUILD NEW ELEMENT                        
         LA    R4,ELEMENT                                                       
         MVI   TANUEL,TANUELQ                                                   
         ZIC   R1,SIMTESTH+5       L'NEW ESTIMATE NUMBER                        
         LA    R1,TANULNQ(R1)      +L'BEG. OF ELEMENT                           
         STC   R1,TANULEN          =L'ELEMENT                                   
         MVI   TANUTYPE,TANUTEST   TYPE=ESTIMATE                                
         MVC   TANUMBER(L'TOEST),TOEST  NEW ESTIMATE NUMBER                     
         GOTO1 ADDELEM             ADD IT TO RECORD                             
         MVI   TIMODE,PROCPTRC     ASK SYSIO TO WRITE BACK RECORD               
         B     IOH60                                                            
         SPACE 1                                                                
IOH50    CLI   TIMODE,PROCREC      TEST THIS IS CHECK RECORD HOOK               
         BNE   IOHX                                                             
         SPACE 1                                                                
IOH60    CLC   FROMCLI,TOCLI       IF NOT CHANGING CLIENT                       
         BNE   IOH70                                                            
         CLC   FROMPRD,TOPRD       OR PRODUCT                                   
         BNE   IOH70                                                            
         CLI   TIMODE,PROCPTRC     AND IF DIDN'T CHANGE ESTIMATE                
         BNE   IOHNO               THEN GET OUT                                 
         B     IOH80                                                            
         SPACE 1                                                                
IOH70    L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         CLC   FROMCLI,TOCLI       IF CHANGING CLIENT CODE                      
         BE    *+10                                                             
         MVC   TAPDCLI,TOCLI       UPDATE CLIENT CODE                           
         SPACE 1                                                                
         CLC   FROMPRD,TOPRD       IF CHANGING PRODUCT CODE                     
         BE    *+10                                                             
         MVC   TAPDPRD,TOPRD       UPDATE PRODUCT CODE                          
         SPACE 1                                                                
         MVI   TIMODE,PROCPTRC     ASK SYSIO TO WRITE BACK RECORD               
         SPACE 1                                                                
IOH80    L     R1,AIO                                                           
         CLI   0(R1),TLINCDQ       IF THIS IS INVOICE RECORD                    
         BNE   IOH90                                                            
         AP    INVCOUNT,=P'1'      UPDATE INVOICE COUNT                         
         GOTO1 MYTRACE,DMCB,=C'UPDATED INVOICE'                                 
         B     IOH100                                                           
         SPACE 1                                                                
IOH90    AP    CHKCOUNT,=P'1'      ELSE UPDATE CHECK COUNT                      
         GOTO1 MYTRACE,DMCB,=C'UPDATED CHECK'                                   
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         SPACE 1                                                                
*OH100   MVC   DMDSKADD,TIDSKADD   SET D/A OF RECORD                            
IOH100   LA    RF,X'08'                                                         
         TM    TMOPTS,TMTRACE                                                   
         BZ    *+8                                                              
         LA    RF,X'18'                                                         
         GOTO1 AADDPTRS,DMCB,((RF),OLDPTRS),NEWPTRS UPDATE PASSIVE PTRS         
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         SPACE 1                                                                
         CLI   TWAWRITE,C'N'       IF WRITE=NO SPECIFIED                        
         BNE   *+8                                                              
         MVI   TIMODE,0            CLEAR UPDATE SWITCH                          
         B     IOHX                                                             
         SPACE 1                                                                
IOHNO    MVI   TIMODE,PROCNOCK     SET DON'T PASS CHECK RECORDS                 
         SPACE 1                                                                
IOHX     MVC   AIO,AIO1            RESTORE I/O AREA                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE INTERFACES WITH GLOBAL TRACE ROUTINE                     
         SPACE 1                                                                
MYTRACE  NTR1                                                                   
         TM    TMOPTS,TMTRACE      TEST TRACE ENABLED                           
         BZ    XIT                                                              
         L     R2,0(R1)            R2=A(LITERAL)                                
         ZIC   R3,0(R1)            R3=L'LITERAL                                 
         GOTO1 TRACE,DMCB,AIO,0,(R2),(R3)                                       
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   HEAD4+16(6),TGAGY    AGENCY                                      
         MVC   HEAD4+24(36),SIMAGYN                                             
         SPACE 1                                                                
         MVC   HEAD6+16(6),FROMCLI  FROM CLIENT                                 
         MVC   HEAD6+24(36),SIMFCLN                                             
         MVC   HEAD7+16(6),FROMPRD       PRODUCT                                
         MVC   HEAD7+24(36),SIMFPRN                                             
         MVC   HEAD8+16(16),SIMFEST      ESTIMATE                               
         SPACE 1                                                                
         MVC   HEAD10+16(6),TOCLI      TO CLIENT                                
         MVC   HEAD10+24(36),SIMTCLN                                            
         MVC   HEAD11+16(6),TOPRD         PRODUCT                               
         MVC   HEAD11+24(36),SIMTPRN                                            
         MVC   HEAD12+16(16),SIMTEST      ESTIMATE                              
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SPACE 1                                                                
         SSPEC H1,43,C'Invoice Maintenance Report'                              
         SSPEC H2,43,26X'BF'                                                    
         SPACE 1                                                                
         SSPEC H4,2,C'Agency'                                                   
         SPACE 1                                                                
         SSPEC H6,02,C'From Client'                                             
         SSPEC H7,02,C'     Product'                                            
         SSPEC H8,02,C'     Estimate'                                           
         SPACE 1                                                                
         SSPEC H10,2,C'To   Client'                                             
         SSPEC H11,2,C'     Product'                                            
         SSPEC H12,2,C'     Estimate'                                           
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
TMD      DSECT                                                                  
TMOPTS   DS    XL1                 OPTIONS                                      
TMTRACE  EQU   X'80'               TRACE ACTIVE                                 
*                                                                               
TMSTAT   DS    XL1                 STATUS                                       
*                                                                               
STRTINV  DS    XL6                 STARTING INVOICE NUMBER FILTER               
ENDINV   DS    XL6                 ENDING INVOICE NUMBER FILTER                 
*                                                                               
FROM     DS    0CL28               FROM FIELDS                                  
FROMCLI  DS    CL6                 CLIENT                                       
FROMPRD  DS    CL6                 PRODUCT                                      
FROMEST  DS    CL16                ESTIMATE                                     
*                                                                               
TO       DS    0CL28               TO FIELDS                                    
TOCLI    DS    CL6                 CLIENT                                       
TOPRD    DS    CL6                 PRODUCT                                      
TOEST    DS    CL16                ESTIMATE                                     
*                                                                               
INVCOUNT DS    PL6                 INVOICE CHANGE COUNT                         
CHKCOUNT DS    PL6                 CHECK CHANGE COUNT                           
*                                                                               
TMLNQ    EQU   *-TMD                                                            
*                                                                               
OLDPTRS  DS    CL(30*L'TLDRREC+1)  SAVED CURRENT POINTERS                       
NEWPTRS  DS    CL(30*L'TLDRREC+1)  USED BY ADDPTRS TO UPDATE POINTERS           
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPD8D                                                       
         SPACE 3                                                                
         PRINT OFF                                                              
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014TAREP38   04/30/14'                                      
         END                                                                    
