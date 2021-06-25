*          DATA SET TAREP4D    AT LEVEL 033 AS OF 02/06/08                      
*PHASE T7034DA,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T7034D - CEINTER REPORT/DOWN (DISK/DOWNLOAD)'                   
T7034D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7034D,R6                                                      
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
         USING TUD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALREC         VALIDATE SCREEN                              
         BE    *+12                                                             
         CLI   MODE,DISPREC                                                     
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
*                                                                               
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
         L     R2,TWADCONS                                                      
         USING TWADCOND,R2                                                      
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
         L     R2,AMASTD                                                        
         USING MASTD,R2                                                         
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
*                                                                               
         BAS   RE,PREP                                                          
         DROP  R2                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE KEY ROUTINE                                             
*                                                                               
VKEY     NTR1                                                                   
         LR    RE,R7               A(LOCAL WORKING STORAGE)                     
         LA    RF,TULNQ                                                         
         XCEFL ,                                                                
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
         XC    SITCLIN,SITCLIN     CLEAR CLIENT NAME                            
         OI    SITCLINH+6,X'80'                                                 
*                                                                               
         GOTO1 RECVAL,DMCB,(X'40',TLAYCDQ),(X'08',SITAGYH),SITAGYNH             
         CLI   SITAGY,C'@'                                                      
         BE    *+14                                                             
         MVC   TIFAGY,TGAGY        SET SYSIO AGENCY FILTER                      
         B     VK20                                                             
         MVC   TIFAGY,TGLST                                                     
         NI    TIFAGY,X'7F'        TURN OFF X'80' FOR SYSIO=FLIST               
*                                                                               
VK20     LA    R2,SITPERH          VALIDATE PERIOD                              
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   TUPER,PVALCPER      SAVE PRINTABLE PERIOD                        
         MVC   TIQPSTR,PVALPSTA                                                 
         MVC   TIQPEND,PVALPEND                                                 
         MVI   TIQDTYPE,TIQDBILL  SET FILTERING ON BILL DATE                    
         DROP  R3                                                               
*                                                                               
         LA    R2,SITCLIH          VALIDATE CLIENT FIELD                        
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SITCLINH                        
         MVC   TIFCLI,TGCLI        SET SYSIO FILTER                             
*                                                                               
VK40     BAS   RE,VALOPT           VALIDATE OPTIONS                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
*                                                                               
VALOPT   NTR1                                                                   
         LA    R2,SITOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            NUMBER OF SCANNER ENTRIES                    
*                                                                               
VOPT10   CLC   =C'DISK',SCDATA1                                                 
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         CLI   ACTEQU,ACTDOWN      DISK NOT VALID OPTION FOR THIS               
         BE    FLDINV                                                           
         OI    TUOPTS,TUNODISK     DON'T GENERATE DISK                          
         B     VOPTNEXT                                                         
*                                                                               
VOPTNEXT LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10                                                        
*                                                                               
VOPTX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO GENERATE REPORTS - CALLS SYSIO                        
*                                                                               
PREP     NTR1                                                                   
         ZAP   COUNT,=P'0'                                                      
         XC    TOTINV,TOTINV                                                    
         MVI   ACTVSW,C'N'                                                      
*                                                                               
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    *+20                                                             
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
*                                                                               
         OI    TIQFLAG2,TIQFSUB    PASS SUBSIDIARY INVOICE RECORDS              
         MVI   TIREAD,TLINDCDQ     READ BILL RECORDS                            
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
*                                                                               
         CLI   ACTVSW,C'N'         IF PROCESSED AN INVOICE                      
         BE    PREPX                                                            
         BAS   RE,CLOSDISK         CLOSE THE DATASET                            
         BAS   RE,PRNTOT           PRINT TOTALS                                 
         CLI   ACTEQU,ACTDOWN      DOWNLOADING TO PRINT?                        
         BNE   PREPX               NO                                           
         BAS   RE,PREPD            YES, PRINT DOWNLOADABLE REPORT               
*                                                                               
PREPX    B     XIT                                                              
         EJECT                                                                  
*              HOOK FROM SYSIO                                                  
*                                                                               
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC                                                   
         BNE   XIT                                                              
*                                                                               
         CP    COUNT,=P'0'         IF DISK NOT OPENED                           
         BH    *+8                                                              
         BAS   RE,OPENDISK         OPEN IT                                      
*                                                                               
         MVC   AIO,TIAREC          SET IOAREA                                   
         L     R6,TIAREC           R6=A(INVOICE RECORD)                         
         USING TLIND,R6                                                         
*                                                                               
         LA    R5,DISKREC          R5=A(DISK RECORD)                            
         USING RECD,R5                                                          
         MVI   0(R5),C' '          MOVE SPACES TO DISK RECORD                   
         MVC   1(RECLNQ-1,R5),0(R5)                                             
*                                                                               
         MVC   RECID,=CL11'TALENT PART'              COMPANY NAME               
         GOTO1 TINVCON,DMCB,TIINV,RECINVN,DATCON     INVOICE NUMBER             
         GOTO1 DATCON,DMCB,(1,TIBIDATE),(20,RECDATE) INVOICE DATE               
         BAS   RE,SETJOB                             ESTIMATE NUMBER            
         BAS   RE,SETCOST                            SET COST COMPONENT         
         BNE   XIT                                                              
         BAS   RE,SETAMT                             INVOICE AMOUNT             
         MVC   RECVEND,=C'000010578'                 VENDOR NUMBER              
         BAS   RE,SETCOML                            SET COMMERICAL             
*                                                                               
         BAS   RE,PUTIT            PUT ONE REC PER INV TO DATASET               
         AP    COUNT,=P'1'         ADD TO COUNT ON DISK                         
         MVI   ACTVSW,C'Y'         INVOICE PROCESSED                            
         BAS   RE,PRNTIT           PRINT INVOICE INFO                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS INVOICE AMOUNT IN DISK                              
*                                                                               
SETAMT   NTR1                                                                   
         LR    R4,R6                                                            
         MVI   ELCODE,TABDELQ      GET BILLING DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TABDD,R4                                                         
         L     R1,TABDTOT          INVOICE AMOUNT                               
         CVD   R1,DUB                                                           
         UNPK  RECINV,DUB                                                       
         MVC   INVAMT,TABDTOT      SAVE FOR PRINTING PURPOSES                   
         L     R1,TOTINV                                                        
         A     R1,INVAMT           ADD TO TOTAL                                 
         ST    R1,TOTINV                                                        
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
*                                                                               
*              THIS ROUTINE SETS ESTIMATE JOB NUMBER                            
*                                                                               
SETJOB   NTR1                                                                   
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTEST))                                     
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         USING TANUD,R4                                                         
         ZIC   RE,TANULEN                                                       
         SH    RE,=Y(TANULNQ+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RECJOB(0),TANUMBER  MOVE EST NUMBER AS IS                        
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*              THIS ROUTINE SETS HARD CODED COST COMPONENT NUMBER               
*                                                                               
SETCOST  NTR1                                                                   
         LR    R4,R6                                                            
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TAPDD,R4                                                         
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
* AS PER FRAN, 12/19/07, COMMENT IT OUT                                         
*        CLI   TGUSEQU,UGRT        SKIP INVOICE IF GUARANTEE PAYMENT            
*        BNE   SCOST05             IS >= $50,000.00                             
*        CLC   TAPDPAYI,=F'5000000'  INDIVIDUAL PAYMENT AMOUNT                  
*        BNL   NO                                                               
*        CLC   TAPDPAYC,=F'5000000'  CORPORATION PAYMENT AMOUNT                 
*        BNL   NO                                                               
*                                                                               
SCOST05  CLI   TGUSEQU,ULFT        IF LFT PAYMENT,                              
         BE    *+8                                                              
         CLI   TGUSEQU,UALF        ADDENDUM LIFT                                
         BE    *+8                                                              
         CLI   TGUSEQU,USLF        SPANISH LIFT                                 
         BNE   SCOST08                                                          
         BAS   RE,CHKSING          CHECK FOR SINGERS                            
         B     SCOST90                                                          
*                                                                               
SCOST08  BAS   RE,CHKMUSIC         PER FRAN - ALL MUS PAY TYPES -> A490         
         BE    SCOST90             DEC07 2006 EMAIL                             
*                                                                               
SCOST10  MVC   RECCOST,=C'A480'                                                 
         TM    TGUSSTAT,SESSION                                                 
         BO    *+10                                                             
         MVC   RECCOST,=C'A490'                                                 
*                                                                               
SCOST20  CLC   TICLI,=CL6'04532'   FOR CLIENT 04532 ONLY                        
         BNE   SCOST90                                                          
         MVC   RECCOST,=C'7203'    PUT '7203' FOR SESSIONS                      
         TM    TGUSSTAT,SESSION                                                 
         BO    *+10                                                             
         MVC   RECCOST,=C'7204'    AND '7204' FOR REUSE                         
         MVC   REC04532,=C'72'     AND '72'                                     
*                                                                               
SCOST90  CLI   TGUSEQU,ULFT        LFT                                          
         BE    SCOST95                                                          
         CLI   TGUSEQU,UALF                                                     
         BE    SCOST95                                                          
         CLI   TGUSEQU,USLF                                                     
         BE    SCOST95                                                          
         CLI   TGUSEQU,UFMU        FMU                                          
         BE    SCOST95                                                          
         CLI   TGUSEQU,UMUS        MUSIC                                        
         BNE   YES                                                              
         CLI   TGUSTYP,UMUSDUB     DUB                                          
         BE    SCOST95                                                          
         CLI   TGUSTYP,UMUSDUB8                                                 
         BE    SCOST95                                                          
         CLI   TGUSTYP,UMUSDSH8                                                 
         BNE   YES                                                              
*                                                                               
SCOST95  CLC   RECJOB(2),=C'08'    JOB STARTING WITH "08"                       
         BNE   YES                                                              
*                                                                               
SCOST100 CLI   RECJOB+6,C'B'       B / H -> A490                                
         BE    SCOST110                                                         
         CLI   RECJOB+6,C'H'                                                    
         BNE   SCOST120                                                         
SCOST110 MVC   RECCOST,=C'A490'                                                 
         B     YES                                                              
*                                                                               
SCOST120 CLI   RECJOB+6,C'T'       T / R -> (SINGER=A340) (ELSE A480)           
         BE    SCOST130                                                         
         CLI   RECJOB+6,C'R'                                                    
         BNE   YES                                                              
SCOST130 MVC   RECCOST,=C'A480'                                                 
         TM    TGCASTAT,SINGER     SINGER?                                      
         BZ    *+10                                                             
         MVC   RECCOST,=C'A340'                                                 
         B     YES                                                              
*------------------------------------------------                               
*&&DO                                                                           
SCOST150 CLC   RECJOB+2(2),=C'YL'  YL -> A490                                   
         BNE   SCOST160                                                         
         MVC   RECCOST,=C'A490'                                                 
         B     YES                                                              
*                                                                               
SCOST160 CLC   RECJOB+2(2),=C'GR'  GR / CH -> (SINGER=A340) (ELSE A480)         
         BE    SCOST170                                                         
         CLC   RECJOB+2(2),=C'CH'  GR / CH -> (SINGER=A340) (ELSE A480)         
         BNE   YES                                                              
SCOST170 MVC   RECCOST,=C'A480'                                                 
         TM    TGCASTAT,SINGER     SINGER?                                      
         BZ    *+10                                                             
         MVC   RECCOST,=C'A340'                                                 
         B     YES                                                              
*&&                                                                             
         DROP  R4                                                               
         EJECT                                                                  
SETCOML  NTR1                                                                   
         LR    R4,R6                                                            
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
*                                                                               
         USING TACOD,R4                                                         
         MVC   RECCOML,TACOCID                                                  
*                                                                               
         LR    R4,R6                                                            
         MVI   ELCODE,TAVRELQ      USE VERSION CID IF THERE IS ONE              
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAVRD,R4                                                         
         MVC   RECCOML,TAVRCID                                                  
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*======================================================================         
*              THIS ROUTINE CHECKS IF USE WAS A MUSIC USE                       
*======================================================================         
         USING TAPDD,R4                                                         
CHKMUSIC NTR1                                                                   
         LA    R1,MUSUTAB          MUSIC USE TABLE                              
CMUS10   CLI   0(R1),X'FF'         EOT                                          
         BE    NO                                                               
         CLC   TAPDUSE,0(R1)                                                    
         BE    CMUS20                                                           
         LA    R1,3(R1)                                                         
         B     CMUS10                                                           
*                                                                               
CMUS20   MVC   RECCOST,=C'A490'                                                 
**NO-OP  MVC   RECCOST,=C'A340'                                                 
**NO-OP  MVC   RECCOST,=C'1120'                                                 
         B     YES                                                              
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*=====================================================================          
*              THIS ROUTINE CHECKS FOR SINGERS ON THE INVOICE                   
*              IF LFT PAYMENT                                                   
*=====================================================================          
CHKSING  NTR1                                                                   
         BAS   RE,SETCHK           SET TO READ CHECK FILE                       
         MVC   AIO,AIO1                                                         
*                                                                               
         USING TLCKD,R2                                                         
         LA    R2,KEY              FIND FIRST CHECK FOR THIS INVOICE            
         XC    KEY,KEY                                                          
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,TIAGY                                                    
         MVC   TLCKINV,TIINV                                                    
         GOTO1 HIGH                                                             
         CLC   0(TLCKSORT-TLCKD,R2),KEYSAVE                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         MVC   RECCOST,=C'A480'    ALL NON-SINGER CATS -> A480                  
         GOTO1 CATVAL,DMCB,TLCKCAT                                              
         TM    TGCASTAT,SINGER     SINGER?                                      
         BZ    *+10                                                             
         MVC   RECCOST,=C'A340'    ALL SINGERS (S/D/GROUPS) -> A340             
         DROP  R2                                                               
*                                                                               
         BAS   RE,SETTAL           RESTORE TO READ TALENT FILE                  
         MVC   KEY,TIKEY           RESTORE READ SEQUENCE                        
         MVC   AIO,TIAREC                                                       
         GOTO1 HIGH                                                             
         GOTO1 GETREC              AND RESTORE AIO                              
         B     XIT                                                              
*                                                                               
SETCHK   DS    0H                  SET TO USE CHECK FILES                       
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         BR    RE                                                               
*                                                                               
SETTAL   DS    0H                  SET TO USE TALENT FILES                      
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*              THIS ROUTINE PRINTS OUT A LINE OF INFO                           
*                                                                               
         USING RECD,R5             R5=A(DISK RECORD)                            
PRNTIT   NTR1                                                                   
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    XIT                                                              
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
         MVC   PRTID,RECID         COMPANY NAME                                 
         MVC   PRTINVN,RECINVN     INVOICE NUMBER                               
         MVC   PRTDATE,RECDATE     INVOICE DATE                                 
         MVC   PRTJOB,RECJOB       ESTIMATE JOB NUMBER                          
         EDIT  INVAMT,(11,PRTINV),2,MINUS=YES INVOICE AMT                       
         MVC   PRTVEND,RECVEND     VENDOR NUMBER                                
         MVC   PRTCOST,RECCOST     COST COMPONENT NUMBER                        
         MVC   PRTCOML,RECCOML     COMMERCIAL                                   
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT                                     
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
*              ROUTINE TO PRINT TOTAL LINE                                      
*                                                                               
PRNTOT   NTR1                                                                   
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)     PRINT A BLANK LINE                           
         EDIT  COUNT,(8,P+1),ALIGN=LEFT                                         
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(13,R1),=CL13'TOTAL RECORDS'                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)     PRINT A BLANK LINE                           
         MVC   P+1(23),=CL23'TOTAL INVOICE AMOUNT = '                           
         EDIT  TOTINV,(11,P+24),2,MINUS=YES,ALIGN=LEFT  TOTAL INV AMT           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OPEN DISK                                             
*                                                                               
OPENDISK NTR1                                                                   
         TM    TUOPTS,TUNODISK     IF NO DISK REQUESTED                         
         BO    XIT                 EXIT                                         
         L     R2,=A(CEDISK)                                                    
         CLI   ACTEQU,ACTDOWN      DOWNLOADING TO PRINT?                        
         BNE   *+8                 NO                                           
         L     R2,=A(TADOWN)       YES, USE TEMPORARY DATASET                   
         OPEN  ((2),OUTPUT)        OPEN THE DISK                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
*                                                                               
*              ROUTINE TO PUT A RECORD TO DISK                                  
*                                  NTRY - (R5) = RECORD                         
PUTIT    NTR1                                                                   
         TM    TUOPTS,TUNODISK     IF NO DISK REQUESTED                         
         BO    XIT                 EXIT                                         
         L     R2,=A(CEDISK)                                                    
         CLI   ACTEQU,ACTDOWN      DOWNLOADING TO PRINT?                        
         BNE   *+8                 NO                                           
         L     R2,=A(TADOWN)       YES, USE TEMPORARY DATASET                   
         PUT   (R2),(R5)           PUT IT TO DISK                               
         B     XIT                                                              
*                                                                               
*                                                                               
*              ROUTINE TO CLOSE THE DISK                                        
*                                                                               
CLOSDISK NTR1                                                                   
         TM    TUOPTS,TUNODISK     IF NO DISK REQUESTED                         
         BO    XIT                 EXIT                                         
         L     R2,=A(CEDISK)                                                    
         CLI   ACTEQU,ACTDOWN      DOWNLOADING TO PRINT?                        
         BNE   *+8                 NO                                           
         L     R2,=A(TADOWN)       YES, USE TEMPORARY DATASET                   
         CLOSE ((2))               CLOSE THE DISK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET FROM DATASET AND PRINT SECOND REPORT              
*                                                                               
PREPD    NTR1                                                                   
         BAS   RE,NEWPRTQ           SET NEW PRINT QUEUE REPORT                  
         GOTO1 REQTWA,DMCB,(3,ATWA),ACOMFACS,VPRINT,(C'B',ABOX)                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R2,AMASTD            DO NOT PRINT LOGOS                          
         USING MASTD,R2                                                         
         NI    MCPRTIND,X'FF'-MCPRTINL                                          
         DROP  R2                                                               
*                                                                               
         L     R2,=A(TADOWN)                                                    
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         BAS   RE,INITDWN                                                       
*                                                                               
PREPD2   GET   (R2),DISKREC        GET REC FROM TEMP DATASET                    
         LA    R5,DISKREC                                                       
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBLEN,40                                                       
         MVC   DLCBFLD(40),0(R5)    PASS 1ST 40 BYTES                           
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         LA    R5,40(R5)           BUMP TO NEXT TAPE FIELD                      
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBLEN,32                                                       
         MVC   DLCBFLD(32),0(R5)   PASS LAST 32 BYTES                           
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         LA    R5,32(R5)           BUMP TO NEXT TAPE FIELD                      
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBLEN,12                                                       
         MVC   DLCBFLD(12),0(R5)   PASS LAST 12 BYTES                           
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPD2                                                           
*                                                                               
*                                                                               
*                                                                               
NOMORE   CLOSE ((2))               CLOSE THE DATASET                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   ACTEQU,ACTDOWN                                                   
         BE    NOMORE2                                                          
         L     R1,ALOGO            NO-OP SO DON'T END AGAIN                     
         MVC   0(2,R1),=X'07FE'                                                 
         B     PREPDX                                                           
NOMORE2  MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
PREPDX   B     XIT                                                              
         EJECT                                                                  
INITDWN  NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
*                                                                               
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,SPLATDWN         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPDX                                                           
         EJECT                                                                  
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
         SPACE 1                                                                
SPLATDWN NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              PREVENT PAGE BREAK                           
         B     PREPDX                                                           
         SPACE 2                                                                
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*                                                                               
NEWPRTQ  NTR1                                                                   
         L     R2,ALOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 ALOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BO    NPRT10                                                           
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
*                                                                               
NPRT10   XC    SPECS,SPECS         CLEAR SPECS                                  
         XC    HEADHOOK,HEADHOOK   AND HEADLINE HOOK                            
*                                                                               
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         LA    RE,132                                                           
         ST    RE,BOXWIDTH         SET LENGTH OF PRINT LINE                     
*                                                                               
         L     RF,AMASTD                                                        
         USING MASTD,RF                                                         
         L     R2,AREMOT                                                        
         USING REMOTED,R2                                                       
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BZ    NPRT20                                                           
         XC    MCREMPQK,MCREMPQK                                                
         B     NPRT30                                                           
*                                                                               
NPRT20   MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'TCD'                                                 
NPRT30   MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(6),=C'CEDATA'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*------------------------------------*                                          
* HDHOOK -  HEADLINE HOOK (HEADHOOK) *                                          
*------------------------------------*                                          
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H4+108(L'TUPER),TUPER                                            
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS, CONSTANTS, ETC.                                   
*                                                                               
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 ERREX                                                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
COMPLM   DC    6X'FF'                                                           
*                                                                               
CEDISK   DCB   DDNAME=CEDISK,DSORG=PS,RECFM=FB,LRECL=84,               X        
               BLKSIZE=8400,MACRF=PM                                            
*                                                                               
TADOWN   DCB   DDNAME=TADOWN,DSORG=PS,RECFM=FB,LRECL=84,               X        
               BLKSIZE=8400,MACRF=(GM,PM),EODAD=NOMORE                          
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MUSUTAB  DS    0H                                                               
         DC    C'BSM'                                                           
         DC    C'IMS'                                                           
         DC    C'CMS'                                                           
         DC    C'MUS'                                                           
         DC    C'FMU'                                                           
         DC    C'SMU'                                                           
         DC    C'OTM'                                                           
         DC    C'PEM'                                                           
         DC    X'FF'                                                            
         EJECT                                                                  
*              REPORT SPECS                                                     
*                                                                               
MYSPECS  DS    0H                                                               
         SPROG 0,1,10                                                           
         SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H4,100,C'PERIOD'                                                 
*                                                                               
         SSPEC H7,2,C'COMPANY     INV #  INV DATE ESTIMATE #'                   
         SSPEC H8,2,C'-------     -----  -------- ----------'                   
*                                                                               
         SSPEC H7,46,C'INVOICE AMT VEND #    COST COMMERCIAL'                   
         SSPEC H8,46,C'----------- ------    ---- ----------'                   
*                                                                               
         SSPEC H1,56,C'INTERFACE REPORT'                                        
         SSPEC H2,56,16X'BF'                                                    
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*                                                                               
TUD      DSECT                                                                  
*                                                                               
INVAMT   DS    F                   INVOICE AMOUNT                               
TOTINV   DS    F                   TOTAL INVOICE AMOUNT                         
TUOPTS   DS    XL1                 OPTIONS                                      
TUNODISK EQU   X'80'               DON'T CREATE DISK DATASET                    
*                                                                               
AMASTD   DS    A                   A(MASTER)                                    
ALOGOC   DS    A                   A(LOGOC)                                     
ALOGO    DS    A                   A(LOGO)                                      
AREMOT   DS    A                   A(REMOTE)                                    
*                                                                               
ACTVSW   DS    CL1                 ACTIVITY INDICATOR                           
TUPER    DS    CL17                PRINTABLE PERIOD                             
*                                                                               
DLBLOCK  DS    CL(DLCBXLX)                                                      
COUNT    DS    PL4                 INVOICE COUNTER                              
*                                                                               
DISKREC  DS    CL(RECLNQ)          DISK RECORD AREA                             
*                                                                               
TULNQ    EQU   *-TUD                                                            
         EJECT                                                                  
*              DSECT TO COVER DISK RECORD                                       
*                                                                               
RECD     DSECT                                                                  
RECID    DS    CL11                'TALENT PART'                                
RECINVN  DS    CL12                INVOICE NUMBER (LEFT JUSTIFIED)              
RECDATE  DS    CL8                 INVOICE DATE (YYYYMMDD)                      
RECJOB   DS    CL15                ESTIMATE NUMBER                              
RECINV   DS    CL11                INVOICE AMOUNT                               
RECVEND  DS    CL9                 VENDOR NUMBER                                
REC04532 DS    CL2                 ONLY USED BY CLIENT 04532                    
RECCOST  DS    CL4                 REUSE='A490', SESSION= 'A480'                
*                                  FOR CLIENT 04532, USE 7204 AND 7203          
RECCOML  DS    CL12                COMMERCIAL CODE                              
RECLNQ   EQU   *-RECD                                                           
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
PRNTD    DSECT                                                                  
         DS    CL1                 SPACE                                        
PRTID    DS    CL11                TALENT PART                                  
         DS    CL1                 SPACE                                        
PRTINVN  DS    CL6                 INVOICE NUMBER                               
         DS    CL1                 SPARE                                        
PRTDATE  DS    CL8                 INVOICE DATE                                 
         DS    CL1                 SPARE                                        
PRTJOB   DS    CL15                ESTIMATE JOB NUMBER                          
         DS    CL1                 SPARE                                        
PRTINV   DS    CL11                INVOICE AMOUNT                               
         DS    CL1                 SPARE                                        
PRTVEND  DS    CL9                 VENDOR NUMBER                                
         DS    CL1                 SPARE                                        
PRTCOST  DS    CL4                 COST COMPONENT NUMBER                        
         DS    CL1                 SPARE                                        
PRTCOML  DS    CL12                COMMERCIAL                                   
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPC0D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDSPOOLD                                                                      
* DDLOGOD                                                                       
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* DDMASTD                                                                       
* DDREMOTED                                                                     
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDPERVALD                                                                     
* DDTWADCONS                                                                    
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDPERVALD                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033TAREP4D   02/06/08'                                      
         END                                                                    
