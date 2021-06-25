*          DATA SET TAREP04    AT LEVEL 039 AS OF 07/27/15                      
*PHASE T70304E,*                                                                
         TITLE 'T70304 - TALENT CHECK WRITING - VALIDATE SCREEN'                
T70304   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70304                                                         
         L     RC,0(R1)            RC = A(GENCON WORKING STORAGE)               
         USING GEND,RC                                                          
         L     R8,ASPOOLD          R8 = A(SPOOL DSECT)                          
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD         R9 = A(CONTROLLER WORKING STORAGE)           
         USING SUBSYSD,R9                                                       
         LA    R7,BUFF             R7 = A(CHECK'S WORKING STORAGE)              
         LA    R7,8(R7)                                                         
         USING CHECKD,R7                                                        
         L     RA,ATWA             RA = A(TWA)                                  
         USING T703FFD,RA                                                       
         EJECT                                                                  
* MAIN ROUTINE                                                                  
*                                                                               
MAIN     DS    0H                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE SYSTEM WORKING STORAGE            
*                                                                               
         CLI   MODE,VALKEY         DO ALL THROUGH MODE VALKEY                   
         BNE   MX                                                               
*                                                                               
         CLC   =C'REP',CONACT      REPORT ONLY                                  
         BNE   M07                                                              
         TM    WHEN,X'08'          DDS, OK                                      
         BO    M07                                                              
         CLC   REMUSER,=C'TCK'                                                  
         BE    M07                                                              
         CLC   REMUSER,=C'TPC'                                                  
         BE    M07                                                              
         CLC   REMUSER,=C'TPK'                                                  
         BE    M07                                                              
         L     R2,EFHWHEN                                                       
*                                                                               
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,=Y(ERRQITCK)   REQUEST INITIALS MUST BE TCK              
         MVC   GTMSYS,GETMSYS         USE CURRENT SYSTEM                        
         MVI   GTMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 ERREXIT                ERROR ROUTINE                             
         DROP  RF                                                               
*                                                                               
M07      LR    RE,R7               PRE-CLEAR CHECK PROGRAM WORKING STR          
         L     RF,=A(CHECKL)                                                    
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,SCKURGH          VALIDATE URGENT FIELD                        
*                                                                               
         CLI   5(R2),0             IF NOTHING ENTERED                           
         BNE   M10                                                              
         MVI   REQURG,C'O'         THEN SET TO 'O'                              
         B     M20                                                              
*                                                                               
M10      CLI   8(R2),C'U'          ELSE MUST BE 'U'                             
         BNE   INVERR                                                           
*                                                                               
         LA    R2,SCKPERH          NO INPUT TO THROUGH DATE ALLOWED             
         CLI   5(R2),0                                                          
         BNE   INVERR                                                           
         MVC   REQURG,SCKURG       SAVE URGENT FLAG                             
         MVC   REQEND1,TGTODAY1    SET THROUGH DATE OF TODAY                    
         OI    TGGNSTAT,TGURGCHK   SAVE GLOBAL URGENT FLAG                      
         B     M50                                                              
*                                                                               
M20      LA    R2,SCKPERH          VALIDATE THROUGH DATE FIELD                  
*                                                                               
         CLC   =C'ALL',8(R2)       IF FIELD CONTAINS 'ALL'                      
         BNE   M30                                                              
         MVC   REQEND1,=X'FFFFFF'  SET TO INFINITY                              
         B     M40                                                              
*                                                                               
M30      LA    R3,BLOCK            ELSE VALIDATE THROUGH DATE                   
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   REQEND1,PVALPEND                                                 
         DROP  R3                                                               
*                                                                               
M40      LA    R2,SCKASOFH         VALIDATE AS/OF DATE                          
         CLI   5(R2),0                                                          
         BE    M50                                                              
         ZIC   R3,5(R2)                                                         
         GOTO1 DATVAL,DMCB,8(R2),WORK                                           
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,REQASOF)                                 
*                                                                               
M50      BAS   RE,VALIOPT          VALIDATE OPTIONS FIELD                       
*                                                                               
         CLI   OFFLINE,C'Y'        IF NOT OFFLINE                               
         BE    MX                                                               
         TM    WHEN,X'20'          AND THIS IS SOON REQUEST                     
         BZ    MX                                                               
*                                                                               
         BAS   RE,OKSOON           SEE IF OK FOR SOON                           
*                                                                               
         CLI   REQURG,C'U'         AND THIS IS URGENT CHECK REQUEST             
         BNE   SOONERR                                                          
*                                                                               
         BAS   RE,SETLOCK          SET URGENT CHECK RUN LOCKOUT STATUS          
         MVI   REQPRI2,C'9'        SET HIGHEST PRIORITY                         
         MVI   TWAWHEN,5           SET UPDATIVE SOON (FOR MONSOON)              
                                                                                
MX       B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE OPTIONS FIELD                                      
*                                                                               
VALIOPT  NTR1                                                                   
         MVI   REQTAPP,C'N'        PRE-SET SUPPRESS TAPPGUAR OPT TO NO          
         MVI   REQOPTS,0                                                        
*                                                                               
         LA    R2,SCKOPTH          IF NO OPTIONS THEN DONE                      
         CLI   5(R2),0                                                          
         BE    VO110                                                            
*                                                                               
         LA    R5,BLOCK            R5 = A(SCANNER BLOCK)                        
         USING SCAND,R5                                                         
*                                  SCAN FIELD INTO SCANNER BLOCK                
         GOTO1 SCANNER,DMCB,(R2),(R5),0                                         
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
*                                                                               
         ZIC   R6,4(R1)            R6 = # OF SCANNER BLOCK ENTRIES              
         SR    R3,R3               R3 = SCR FLD DISP TO CURRENT ENTRY           
*                                                                               
*                                  IF AGENCY FILTER OPTION                      
VO10     CLC   SCDATA1(7),=C'AGENCY '                                           
         BNE   VO20                                                             
*                                  THEN VAL AND SAVE AGENCY IN REQAGY           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',SCDATA2)                              
         BNE   FLDERR                                                           
         MVC   REQAGY,TGAGY                                                     
         B     VO100                                                            
*                                  ELSE IF INVOICE FILTER OPTION                
VO20     CLC   SCDATA1(8),=C'INVOICE '                                          
         BNE   VO30                                                             
*                                                                               
         OC    REQAGY,REQAGY       THEN AGENCY MUST BE SPECIFIED                
         BZ    FLDERR                                                           
*                                  VALIDATE AND SAVE INVOICE IN REQINV          
         GOTO1 TINVCON,DMCB,SCDATA2,REQINV,DATCON                               
         CLI   0(R1),X'FF'                                                      
         BZ    FLDERR                                                           
         MVC   DUB(6),REQINV       COMPLEMENT FOR RECVAL CALL                   
         XC    DUB(6),=X'FFFFFFFFFFFF'                                          
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'80',DUB)                                  
         BNE   FLDERR                                                           
         B     VO100                                                            
*                                  ELSE IF CHKNUM FILTER OPTION                 
VO30     CLC   SCDATA1(7),=C'CHKNUM '                                           
         BNE   VO40                                                             
         CLI   TWAWRITE,C'N'       THEN TWAWRITE MUST = 'N'                     
         BNE   FLDERR                                                           
         MVC   REQCHKN,SCDATA2     SAVE CHECK NUMBER FILTER                     
         B     VO100                                                            
*                                  ELSE IF SUPPRESS TAPPGUAR OPTION             
VO40     CLC   SCDATA1(7),=C'NOTAPP '                                           
         BNE   VO50                                                             
         MVI   REQTAPP,C'Y'        SET SUPPRESS TAPPGUAR INDICATOR              
         B     VO100                                                            
*                                  ELSE IF STARTING CHECK NUMBER OPTION         
VO50     CLC   SCDATA1(9),=C'STARTCHK '                                         
         BNE   VO60                                                             
         OC    SCBIN2,SCBIN2       THEN MUST BE NON-ZERO                        
         BZ    FLDERR                                                           
         MVC   REQSCHK,SCBIN2      SAVE STARTING CHECK NUMBER                   
         B     VO100                                                            
*                                  ELSE IF PERFORMER FILTER                     
VO60     CLC   SCDATA1(5),=C'PERF '                                             
         BNE   VO65                                                             
         MVC   REQSSN,SCDATA2      SAVE SOCIAL SECURITY NUMBER                  
         B     VO100                                                            
*                                  ELSE IF FORCE CHK W4TYPE FROM W4 REC         
VO65     CLC   SCDATA1(3),=C'OFF'                                               
         BNE   VO70                                                             
         MVC   REQOFFC,SCDATA2     SAVE OFFICE FILTER                           
         B     VO100                                                            
*                                  ELSE IF FORCE CHK W4TYPE FROM W4 REC         
VO70     CLC   SCDATA1(10),=C'FORCEW4TY '                                       
         BNE   VO72                                                             
         MVI   REQFRCW4,C'Y'       SET CORRESPONDING PROGRAM SWITCH             
         B     VO100                                                            
*                                                                               
VO72     CLC   SCDATA1(10),=C'FORCECDTE '                                       
         BNE   VO74                                                             
         OI    REQOPTS,REQOCDTE    SET CHECK DATE = PERIOD END DATE             
         CLI   SCLEN2,0            LOOK FOR FORCED DATE IN 2ND HALF             
         BE    VO100                                                            
         GOTO1 DATVAL,DMCB,SCDATA2,WORK                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,REQFORC)                                 
         B     VO100                                                            
*                                                                               
VO74     CLC   SCDATA1(10),=CL10'TASEGUEA'                                      
         BNE   VO75                                                             
         OI    REQOPTS,REQALTXA    LOAD TASEQUEA                                
         B     VO100                                                            
*                                                                               
VO75     CLC   SCDATA1(10),=CL10'REG2'                                          
         BNE   VO76                                                             
         TM    PRGSTAT,CSCSYS+FQASYS                                            
         BZ    VO90                                                             
         OI    REQOPTS,REQREG2     REGRESSION RUN 2                             
         B     VO100                                                            
*                                                                               
VO76     CLC   SCDATA1(10),=CL10'REG'                                           
         BNE   VO78                                                             
         TM    PRGSTAT,CSCSYS+FQASYS                                            
         BZ    VO90                                                             
         OI    REQOPTS,REQREG      REGRESSION RUN                               
         B     VO100                                                            
*                                                                               
VO78     CLC   SCDATA1(5),=CL10'PRTLA'                                          
         BNE   VO80                                                             
         OI    REQOPTS,REQOCAL     CALIFORNIA CHECK RUN                         
         B     VO100                                                            
*                                                                               
VO80     CLC   SCDATA1(6),=CL10'TACKESUA'                                       
         BNE   VO82                                                             
         OI    REQOPTS,REQESUA     LOAD TACKESUA                                
         B     VO100                                                            
*                                                                               
VO82     CLC   SCDATA1(6),=CL10'TACKPYTA'                                       
         BNE   VO90                                                             
         OI    REQOPTS,REQPYTA     LOAD TACKPYTA                                
         B     VO100                                                            
*                                                                               
VO90     B     FLDERR              ELSE INVALID OPTION                          
*                                                                               
VO100    ZIC   RF,SCLEN1           BUMP R3 TO NEXT ENTRY                        
         LA    R3,1(R3,RF)                                                      
         ZIC   RF,SCLEN2                                                        
         LA    R3,1(R3,RF)                                                      
*                                                                               
         LA    R5,SCANNEXT         BUMP R5 TO NEXT ENTRY                        
*                                                                               
         BCT   R6,VO10             REPEAT UNTIL NO MORE ENTRIES                 
*                                                                               
*                                  IF TRACE FIELD STARTS WITH 'TRACE='          
VO110    CLC   SCKTRAC(6),=C'ALANE='                                            
         BNE   VO120                                                            
         MVC   REQTRACE,SCKTRAC+6  THEN SAVE TRACE BYTES                        
         B     VOX                                                              
*                                                                               
VO120    XC    SCKTRAC,SCKTRAC     ELSE CLEAR OUT FIELD                         
         OI    SCKTRACH+6,X'80'                                                 
*                                                                               
VOX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*=====================================================================          
* THIS ROUTINE SEES IF CHECK LOCKOUT DAY WAS TODAY.                             
* IF NOT, URGENT RUN WITH AGENCY & INVOICE FILTER NOT ALLOWED                   
* IF IT IS, AGENCY & INVOICE FILTER MUST BE REQUESTED                           
*=====================================================================          
OKSOON   NTR1                                                                   
         CLI   RECNUM,PC           PRINT CHECKS?                                
         BE    *+8                                                              
         CLI   RECNUM,PK           P+ CHECKS?                                   
         BNE   OKSOON10                                                         
         OI    REQOPTS,REQOCAL     CALIFORNIA CHECK RUN                         
*                                                                               
OKSOON10 GOTO1 RECVAL,DMCB,TLSYCDQ,(X'30',0) READ SYSTEM REC FOR UPDATE         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,TASYELQ      GET SYSTEM DETAILS EL.                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASYD,R3                                                         
*                                                                               
         CLC   TASYIDCD,=C'DPS2  ' ALLOW DPS2 TO RUN MULTIPLE TIMES             
         BE    OKSOON30                                                         
         CLC   TASYIDCD,=C'TALFQA' ALLOW TALFQA TO RUN MULTIPLE TIMES           
         BE    OKSOON30                                                         
                                                                                
         CLI   RECNUM,PC           PRINT CHECKS?                                
         BE    OKSOON20                                                         
         CLI   RECNUM,PK           P+ CHECKS?                                   
         BE    OKSOON25                                                         
         CLC   TASYLKDT,TGTODAY1   WAS SOON CHECKS REQUESTED TODAY?             
         BE    OKSOON50                                                         
         B     OKSOON30                                                         
                                                                                
OKSOON20 CLC   TASYPLDT,TGTODAY1   WAS PRINT CHECKS REQUESTED TODAY?            
         BE    OKSOON50                                                         
         B     OKSOON30                                                         
*                                                                               
OKSOON25 CLC   TASYLLDT,TGTODAY1   WAS P+ CHECKS REQUESTED TODAY?               
         BE    OKSOON50                                                         
*                                                                               
OKSOON30 TM    PRGSTAT,TESTSYS+CSCSYS+FQASYS                                    
         BNZ   XIT                                                              
         OC    REQAGY,REQAGY       NO, AGY AND INV NOT ALLOWED                  
         BNZ   SOONERR                                                          
         OC    REQINV,REQINV                                                    
         BNZ   SOONERR                                                          
         B     XIT                                                              
*                                                                               
OKSOON50 CLI   RECNUM,PC           PRINT CHECKS?                                
         BE    OKSOON70                                                         
         CLI   RECNUM,PK           P+ CHECKS?                                   
         BE    OKSOON75                                                         
         OC    TASYLKEN,TASYLKEN   IS IT RUNNING?                               
         BZ    RNGERR              CAN'T REQUEST AGAIN                          
         B     OKSOON80                                                         
OKSOON70 OC    TASYPLEN,TASYPLEN   IS IT RUNNING FOR PRINT?                     
         BZ    RNGERR              CAN'T REQUEST AGAIN                          
         B     OKSOON80                                                         
OKSOON75 OC    TASYLLEN,TASYLLEN   IS IT RUNNING FOR P+                         
         BZ    RNGERR              CAN'T REQUEST AGAIN                          
                                                                                
OKSOON80 OC    REQAGY,REQAGY       AGY AND INV MUST BE REQUESTED                
         BZ    RANERR                                                           
*        BZ    SOONERR                                                          
         OC    REQINV,REQINV                                                    
         BZ    RANERR                                                           
*        BZ    SOONERR                                                          
         B     XIT                                                              
         EJECT                                                                  
*=====================================================================          
* THIS ROUTINE SETS THE CURRENT DATE AND TIME IN THE SYSTEM RECORD              
* CORRESPONDING TO WHEN THE URGENT CHECK RUN WAS REQUESTED.  IT ALSO            
* SETS THE LOCKOUT STATUS IN THE CORE-RES LOCKOUT BYTE IN TASYSTAB.             
*=====================================================================          
SETLOCK  NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'30',0) READ SYSTEM REC FOR UPDATE         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         L     R3,AIO                                                           
         MVI   ELCODE,TASYELQ      GET SYSTEM DETAILS EL.                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASYD,R3                                                         
         SPACE 1                                                                
         CLI   RECNUM,PC           IF PRINT CHECKS THEN SKIP                    
         BE    SL10                                                             
         CLI   RECNUM,PK           IF P+ CHECKS THEN SKIP                       
         BE    SL20                                                             
         CLC   TASYLKDT,TGTODAY1   ELSE IF ALREADY REQUESTED                    
         BNE   SL5                                                              
*        CLC   TWAAGY,=C'D2'       AND NOT TEST ID THEN DON'T ALLOW             
*        BNE   SOONERR                                                          
         OC    TASYLKEN,TASYLKEN   ELSE OK IF IT HAS ENDED ALREADY              
         BZ    RNGERR                                                           
SL5      MVC   TASYLKDT,TGTODAY1   SET TODAY'S DATE                             
         SPACE 1                                                                
         TIME  DEC                                                              
         STCM  R0,14,TASYLKST      AND LOCKOUT START TIME                       
         SPACE 1                                                                
         XC    TASYLKEN,TASYLKEN   CLEAR END TIME                               
         B     SL60                                                             
         SPACE 1                                                                
SL10     CLC   TASYPLDT,TGTODAY1   PRINT - IF ALREADY REQUESTED                 
         BNE   SL15                                                             
         CLC   TWAAGY,=C'D2'       AND NOT TEST ID THEN DON'T ALLOW             
         BNE   SOONERR                                                          
         OC    TASYPLEN,TASYPLEN   ELSE OK IF IT HAS ENDED ALREADY              
         BZ    SOONERR                                                          
SL15     MVC   TASYPLDT,TGTODAY1   SET TODAY'S DATE                             
         SPACE 1                                                                
         TIME  DEC                                                              
         STCM  R0,14,TASYPLST      AND LOCKOUT START TIME                       
         SPACE 1                                                                
         XC    TASYPLEN,TASYPLEN   CLEAR END TIME                               
         B     SL70                                                             
*                                                REGULAR CHECKS                 
SL20     CLC   TASYLLDT,TGTODAY1   P+ - IF ALREADY REQUESTED                    
         BNE   SL25                                                             
         CLC   TWAAGY,=C'D2'       AND NOT TEST ID THEN DON'T ALLOW             
         BNE   SOONERR                                                          
         OC    TASYLLEN,TASYLLEN   ELSE OK IF IT HAS ENDED ALREADY              
         BZ    SOONERR                                                          
SL25     MVC   TASYLLDT,TGTODAY1   SET TODAY'S DATE                             
         SPACE 1                                                                
         TIME  DEC                                                              
         STCM  R0,14,TASYLLST      AND LOCKOUT START TIME                       
         SPACE 1                                                                
         XC    TASYLLEN,TASYLLEN   CLEAR END TIME                               
         B     SL80                                                             
*                                                REGULAR CHECKS                 
SL60     BAS   RE,SLTSTURG         TEST IF ANY LOCKOUT ACTIVE                   
*                                                                               
         GOTO1 SETLCKT,DMCB,=CL10'TAL_CHECKS',1         LOCK IT                 
         BNE   LOCKERR             YES -- ONLY ONE URGENT RUN AT A TIME         
         B     SL90                                                             
*                                                PRINT CHECKS                   
SL70     BAS   RE,SLTSTURG         TEST IF ANY LOCKOUT ACTIVE                   
*                                                                               
         GOTO1 SETLCKT,DMCB,=CL10'TAL_PRCHKS',1         LOCK IT                 
         BNE   LOCKERR             YES -- ONLY ONE URGENT RUN AT A TIME         
         B     SL90                                                             
*                                                                               
SL80     BAS   RE,SLTSTURG         TEST IF ANY LOCKOUT ACTIVE                   
*                                                                               
         GOTO1 SETLCKT,DMCB,=CL10'TAL_P+CHKS',1         LOCK IT                 
         BNE   LOCKERR             YES -- ONLY ONE URGENT RUN AT A TIME         
         B     SL90                                                             
*                                                                               
SL90     GOTO1 PUTREC              WRITE BACK THE SYSTEM RECORD                 
         B     XIT                                                              
*                                                                               
SLTSTURG NTR1                                                                   
         GOTO1 TSTLCKT,DMCB,=CL10'TAL_CHECKS'    TEST LOCKOUT ACTIVE            
         BE    LOCKERR             ONLY ONE URGENT RUN AT A TIME                
         GOTO1 TSTLCKT,DMCB,=CL10'TAL_PRCHKS'    TEST LOCKOUT ACTIVE            
         BE    LOCKERR             ONLY ONE URGENT RUN AT A TIME                
         GOTO1 TSTLCKT,DMCB,=CL10'TAL_P+CHKS'    TEST LOCKOUT ACTIVE            
         BE    LOCKERR             ONLY ONE URGENT RUN AT A TIME                
         B     XIT                                                              
         EJECT                                                                  
FLDERR   L     RE,SYSPARMS         INVALID INPUT WITHIN FIELD                   
         ICM   RE,15,0(RE)                                                      
         BZ    INVERR                                                           
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,15,TIOBCURD                                                   
         STC   R3,TIOBCURI                                                      
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERREXIT                                                          
*                                                                               
SOONERR  MVI   ERROR,ERSOONRQ      INVALID SOON REQUEST                         
         L     R2,EFHWHEN                                                       
         B     ERREXIT                                                          
*                                                                               
LOCKERR  MVI   ERROR,ERCKLOCK      ACTION RESTRICTED DURING URGENT RUN          
         L     R2,EFHWHEN                                                       
         B     ERREXIT                                                          
*                                12345678901234567890123456789012345            
RNGERR   MVC   CONHEAD(L'RNGMSG),RNGMSG                                         
         B     USERERR                                                          
                                                                                
RANERR   MVC   CONHEAD(L'RANMSG),RANMSG                                         
         B     USERERR                                                          
                                                                                
USERERR  MVI   ERROR,X'FE'                                                      
         L     R2,EFHWHEN                                                       
         GOTO1 ERREX2              MY OWN ERROR MESSAGE                         
         B     XIT                                                              
*                                                                               
ERREXIT  GOTO1 ERREX                                                            
         B     XIT                                                              
*                                                                               
CCYES    CR    RB,RB                                                            
         B     XIT                                                              
CCNO     LTR   RB,RB                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         SPACE 3                                                                
RNGMSG   DC    C'** ERROR ** URGENT CHECKS STILL RUNNING'                       
RANMSG   DC    C'** ERROR ** URGENT CHECKS RAN TODAY ALREADY'                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF4D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE TACKREPD                                                       
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*TAREPWORKD                                                                     
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*FATIOB                                                                         
*DDPERVALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039TAREP04   07/27/15'                                      
         END                                                                    
