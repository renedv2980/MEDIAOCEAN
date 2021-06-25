*          DATA SET TAREP46    AT LEVEL 043 AS OF 10/04/16                      
*PHASE T70346C,*                                                                
*INCLUDE DLFLD                                                                  
*INCLUDE POWWOW                                                                 
         TITLE 'T70346 - OM INTERFACE REPORT (DISK/DOWNLOAD)'                   
T70346   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70346,R6                                                      
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
         MVC   MYSORTER,SORTER                                                  
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
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
         L     R2,TWADCONS                                                      
         USING TWADCOND,R2                                                      
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
         MVC   APRNTBL,TPRNTBL     SAVE A(PRNTBL)                               
         MVC   APRNT,TWAVPRNT      SAVE A(PRINT)                                
         L     R2,AMASTD                                                        
         USING MASTD,R2                                                         
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
         DROP  R2                                                               
*                                                                               
         BAS   RE,PREP                                                          
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
AYLINT   EQU   83                  SPECIAL FOR BURNETT -REPORT IS SAME          
         EJECT                                                                  
*              VALIDATE KEY ROUTINE                                             
         SPACE                                                                  
VKEY     NTR1                                                                   
         LR    RE,R7               A(LOCAL WORKING STORAGE)                     
         LA    RF,TULNQ                                                         
         XCEFL ,                                                                
         XC    AGYFLST,AGYFLST                                                  
         MVI   AGYFLST,X'FF'                                                    
         SPACE 1                                                                
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
         XC    SITCLIN,SITCLIN     CLEAR CLIENT NAME                            
         OI    SITCLINH+6,X'80'                                                 
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,(X'40',TLAYCDQ),(X'08',SITAGYH),SITAGYNH             
         CLI   SITAGY,C'@'                                                      
         BE    VK10                                                             
         BAS   RE,SETAGY           SET SVAGYCO AND LENGTHS                      
         MVC   TIFAGY,TGAGY        SET SYSIO AGENCY FILTER                      
         B     VK20                                                             
         SPACE 1                                                                
VK10     MVC   TIFAGY,TGLST                                                     
         NI    TIFAGY,X'7F'        TURN OFF X'80' FOR SYSIO=FLIST               
         SPACE 1                                                                
         USING TAGLD,R4                                                         
         LA    R2,AGYFLST                                                       
         L     R4,AIO              BUILD FLIST LIST                             
         MVI   ELCODE,TAGLELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VK15     BAS   RE,NEXTEL                                                        
         BNE   VK20                                                             
         ZIC   RE,TAGLLEN                                                       
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TAGLDATA                                                 
         OC    0(L'TGAGY,R2),SPACES                                             
         LA    R2,L'TGAGY(R2)                                                   
         MVI   0(R2),X'FF'                                                      
         B     VK15                                                             
         DROP  R4                                                               
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
         SPACE                                                                  
*              THIS ROUTINE SETS SVAGYCO AND LENGHTS                            
         SPACE                                                                  
SETAGY   NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTCO                                   
         MVC   SVAGYCO,TGNAME                                                   
*                                                                               
         XC    LENGTHS,LENGTHS                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTJOB))                                     
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         USING TANUD,R4                                                         
         MVC   LENGTHS,TANUMBER                                                 
         NI    LENGTHS,X'0F'       TURN OFF CHARACTERS                          
         NI    LENGTHS+1,X'0F'                                                  
         NI    LENGTHS+2,X'0F'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE                                                                  
VALOPT   NTR1                                                                   
         MVC   OVRVEND,SPACES      PRESET TO SPACES                             
         MVI   FILEOPT,C'1'                                                     
*                                                                               
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
VOPT10   CLC   =C'TAPE',SCDATA1                                                 
         BNE   VOPT20                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         OI    TUOPTS,TUNOTAPE     DON'T GENERATE DISK OR                       
         B     VOPTNEXT            DOWNLOADABLE REPORT                          
*                                                                               
VOPT20   CLC   =C'ERROR',SCDATA1                                                
         BNE   VOPT30                                                           
         OI    TUOPTS,TUERROR      COPY ESTIMATE # EXACTLY                      
         B     VOPTNEXT                                                         
*                                                                               
VOPT30   CLC   =C'NOPOW',SCDATA1   DON'T CALL POWWOW                            
         BNE   VOPT40                                                           
         OI    TUOPTS,TUNOPOW                                                   
         B     VOPTNEXT                                                         
*                                                                               
VOPT40   CLC   =C'TRACE',SCDATA1   TRACE POWWOW JCL                             
         BNE   VOPT50                                                           
         OI    TUOPTS,TUTRACE                                                   
         B     VOPTNEXT                                                         
*                                                                               
VOPT50   CLC   =C'VEND',SCDATA1    IF VENDOR OVERRIDE                           
         BNE   VOPT70                                                           
         TM    TUOPTS,TUGREY       CAN'T USE WITH GREY OPTION                   
         BO    FLDINV                                                           
         ZIC   R1,SCLEN2                                                        
         LTR   R1,R1                                                            
         BZ    FLDINV                                                           
         CH    R1,=H'11'                                                        
         BH    FLDINV                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OVRVEND(0),SCDATA2                                               
         B     VOPTNEXT                                                         
*&&DO                                                                           
VOPT60   CLC   =C'RESEND',SCDATA1  RESEND INVOICES                              
         BNE   VOPT70                                                           
         OI    TUOPTS,TURESND                                                   
         B     VOPTNEXT                                                         
*&&                                                                             
VOPT70   CLC   =C'FILE',SCDATA1    IF VENDOR OVERRIDE                           
         BNE   VOPT80                                                           
         CLI   SCDATA1+4,C'1'                                                   
         BL    FLDINV                                                           
         CLI   SCDATA1+4,C'3'                                                   
         BH    FLDINV                                                           
         MVC   FILEOPT(1),SCDATA1+4                                             
         B     VOPTNEXT                                                         
*                                                                               
VOPT80   B     FLDINV              INVALID OPTION                               
*                                                                               
VOPTNEXT LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10                                                        
*                                                                               
VOPTX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO GENERATE REPORTS - CALLS SYSIO                        
         SPACE                                                                  
PREP     NTR1                                                                   
         MVI   ACTVSW,C'N'         NOTHING ACTIVE                               
         ZAP   COUNT,=P'0'                                                      
         ZAP   ERRCNT,=P'0'                                                     
         ZAP   HDRCOUNT,=P'0'                                                   
         ZAP   DTLCOUNT,=P'0'                                                   
         NI    TUOPTS,X'FF'-TURESND                                             
*                                                                               
         L     R2,=A(OIDISK1)      OIDISK = REPORT                              
         CLI   FILEOPT,C'1'                                                     
         BE    PREP03                                                           
         L     R2,=A(OIDISK2)                                                   
         CLI   FILEOPT,C'2'                                                     
         BE    PREP03                                                           
         L     R2,=A(OIDISK3)                                                   
PREP03   ST    R2,OIDSKDCB                                                      
         MVC   DSNME2(1),FILEOPT                                                
*                                                                               
         BRAS  RE,SETXML                                                        
*                                                                               
         MVI   RCSUBPRG,1                                                       
*                                                                               
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
*                                                                               
         OI    TIQFLAG2,TIQFSUB                                                 
         MVI   TIREAD,TLINDCDQ     READ INVOICE RECORDS                         
         XC    SVAGY,SVAGY         CLEAR SAVED AGENCY                           
         XC    SVCLI,SVCLI         CLEAR SAVED CLIENT                           
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
*============                                                                   
         BAS   RE,RESEND           SEND INVOICES MARKED FOR RESEND              
*============                                                                   
PREP05   LA    R2,P                                                             
         GOTO1 =A(TAGS),DMCB,XGDOCEN                                            
         BAS   RE,SPLATDWN                                                      
*                                                                               
         BAS   RE,CLOSTAPE         CLOSE THE TAPE/DATASET                       
*                                                                               
         CP    COUNT,=P'0'                                                      
         BE    PREPX                                                            
         BAS   RE,CALLPOW          CALL TO POWWOW TO FTP                        
*                                                                               
PREPX    B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
*======================================================================         
SPLATDWN NTR1                                                                   
         LA    R5,P                                                             
         BAS   RE,PUTIT                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              PREVENT PAGE BREAK                           
         B     XIT                                                              
         SPACE 2                                                                
*======================================================================         
*              HOOK FROM SYSIO                                                  
*======================================================================         
IOHOOK   NTR1                                                                   
         MVC   AIO,TIAREC          SET IOAREA                                   
         TM    TUOPTS,TURESND                                                   
         BZ    *+10                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R5,TAPEREC          R5=A(TAPE RECORD)                            
         USING RECD,R5                                                          
*                                                                               
         CLI   TIMODE,PROCREC      INVOICE RECORD HOOK                          
         BNE   IOHOOKX                                                          
*                                                                               
         MVC   SVAGY,TIAGY         SAVE AGENCY CODE                             
         MVC   SVCLI,TICLI         SAVE CLIENT CODE                             
         MVC   SVPRD,TIPRD         SAVE PRODUCT CODE                            
         MVC   SVOFF,TIOFF         SAVE OFFICE CODE                             
         MVI   ACTVSW,C'Y'         INVOICE PROCESSED                            
         XC    CNTRS(CNTRSX),CNTRS CLEAR COUNTERS                               
         XC    FEESTOT,FEESTOT     AND TOTALS / INVOICE                         
         MVI   0(R5),C' '          MOVE SPACES TO TAPE                          
         MVC   1(RECLNQ-1,R5),0(R5)                                             
         BAS   RE,SAVETIA                                                       
*                                                                               
         BAS   RE,SETCAN           SET CANADIAN CONVERSION RATE                 
*                                                                               
         BAS   RE,SETINV           SET INVOICE INFORMATION                      
IOHOOKX  B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              SET CANADIAN CONVERSION RATE FOR CAN$ INVOICES                   
*======================================================================         
SETCAN   NTR1                                                                   
         XC    CANRATE,CANRATE                                                  
         MVI   SUBSID,C'N'                                                      
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         TM    TUOPTS,TURESND                                                   
         BZ    *+8                                                              
         L     R4,AIO1                                                          
*                                                                               
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R4                                                         
         TM    TAPDSTA2,TAPDSSUB   SUBSIDIARY INVOICE                           
         BZ    *+8                                                              
         MVI   SUBSID,C'Y'                                                      
         TM    TAPDSTAT,TAPDSCAN   CHECK THIS IS CANADIAN                       
         BNO   XIT                                                              
         L     R4,TIAREC                                                        
         TM    TUOPTS,TURESND                                                   
         BZ    *+8                                                              
         L     R4,AIO1                                                          
*                                                                               
         MVI   ELCODE,TABDELQ      FIND RATE FROM INVOICE                       
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TABDD,R4                                                         
         OC    TABDCCVT,TABDCCVT                                                
         BZ    XIT                                                              
         ZAP   DUB,TABDCCVT                                                     
         CVB   R1,DUB                                                           
         ST    R1,CANRATE                                                       
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              FIX CANADIAN DOLLARS                                             
*======================================================================         
         USING TAPDD,R4            R4=A(TAPD EL)                                
FIXCAN   NTR1                                                                   
         OC    CANRATE,CANRATE     DON'T BOTHER IF RATE NOT DEFINED             
         BZ    XIT                                                              
         LA    R1,TAPDAMTS                                                      
         LA    R0,TAPDAMTL/L'TAPDAMTS                                           
FIXCAN4  BAS   RE,FIXCAN6          CONVERT THE TAPDS                            
         LA    R1,4(R1)                                                         
         BCT   R0,FIXCAN4                                                       
         B     XIT                                                              
         SPACE 3                                                                
FIXCAN6  NTR1                      ADJUST R1=A(FULLWORD) BY CANRATE             
         L     RF,0(R1)                                                         
         M     RE,CANRATE                                                       
         D     RE,=F'5000'                                                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R1)                                                         
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE SETS TAPEREC INFO AT INVOICE LEVEL                  
*======================================================================         
SETINV   NTR1                                                                   
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         TM    TUOPTS,TURESND                                                   
         BZ    *+8                                                              
         L     R4,AIO1                                                          
*                                                                               
         BAS   RE,VALPO            VALIDATE PO                                  
*        BE    SINV5                                                            
*        TM    PGMSTAT,PRGSPOER                                                 
         BNE   XIT                                                              
SINV5    BAS   RE,SETAPD           SET TAPD INFO (AMOUNT/SOR)                   
         BAS   RE,SETABD           SET TABD AMOUNTS                             
         OC    CNTINV,CNTINV       DON'T SEND IF INVOICE TOTAL = 0              
         BZ    XIT                                                              
         AP    COUNT,=P'1'                                                      
*                                                                               
         LA    R2,P                        <Invoice>                            
         GOTO1 =A(TAGS),DMCB,XGINVST                                            
         BAS   RE,SPLATDWN                                                      
*                                                                               
         BAS   RE,HEADER                                                        
         BAS   RE,ITEMS                                                         
         BRAS  RE,SUMMARY                                                       
*                                                                               
         LA    R2,P                        </Invoice>                           
         GOTO1 =A(TAGS),DMCB,XGINVEN                                            
         BAS   RE,SPLATDWN                                                      
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE SETS HEADER PORTION OF INVOICES                     
*======================================================================         
HEADER   NTR1                                                                   
         LA    R2,P                                                             
         GOTO1 =A(TAGS),DMCB,XGHDRST       <Header>                             
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P+17                     <CompanyCode>                        
         GOTO1 =A(TAGS),DMCB,XGCMPCDS                                           
         BAS   RE,SETCCODE                                                      
         GOTO1 =A(TAGS),DMCB,XGCMPCDE                                           
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P+17                     <Currency>                           
         GOTO1 =A(TAGS),DMCB,XGCURR                                             
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P+17                     <DocDate>                            
         GOTO1 =A(TAGS),DMCB,XGDOCDT                                            
         GOTO1 DATCON,DMCB,(5,0),(23,P+26)                                      
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P+17                     <BLineDate>                          
         GOTO1 =A(TAGS),DMCB,XGBLINE                                            
         GOTO1 DATCON,DMCB,(1,TIBIDATE),(23,P+28)                               
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P+17                     <RefDocNum>                          
         GOTO1 =A(TAGS),DMCB,XGREFDCN                                           
         GOTO1 TINVCON,DMCB,TIINV,P+28,DATCON                                   
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P                        </Header>                            
         GOTO1 =A(TAGS),DMCB,XGHDREN                                            
         BAS   RE,SPLATDWN                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE SETS ITEM PORTION OF INVOICES                       
*======================================================================         
ITEMS    NTR1                                                                   
         LA    R1,10                                                            
         STH   R1,INVITMCT                                                      
*        LA    R1,NUMSINV                                                       
*                                                                               
         L     RF,CNTHAND          COMBINE TAX AND HANDLING                     
         A     RF,CNTTAX                                                        
         A     RF,CNTGST                                                        
         ST    RF,CNTHAND          AND SAVE IT                                  
*                                                                               
         A     RF,CNTTUSGE                                                      
         A     RF,CNTMUS                                                        
         A     RF,CNTPNH           SUM MUST EQUAL INVOICE TOTAL                 
         C     RF,CNTINV                                                        
         BE    ITEMS0                                                           
         L     R1,CNTINV           IF NOT, PUT DIFFERENCE IN HANDLING           
         SR    R1,RF                                                            
         A     R1,CNTHAND                                                       
         ST    R1,CNTHAND                                                       
*                                                                               
ITEMS0   L     RF,CNTHAND          COMBINE TAX/HANDLING,                        
         A     RF,CNTPNH           PNH,                                         
         A     RF,CNTTUSGE         AND TALENT USAGE                             
         ST    RF,CNTTOT           AND SAVE IT                                  
*                                                                               
*TEMS0   LA    R1,3                                                             
         LA    R1,1                                                             
**NO-OP  LA    R2,CNTSINV          INVOICE AMOUNTS                              
         LA    R2,CNTTOT           INVOICE AMOUNTS                              
         LA    R3,WKCDTB1          WORKCODE TABLES                              
         LA    R5,WKCDSCR          WORKCODE DESCRIPTIONS                        
*                                                                               
         TM    TGUSSTAT,SESSION    SESSION                                      
         BZ    ITEMS2              RESIDUAL, NORMAL POSTINGS                    
*        OC    CNTMUS,CNTMUS       MUSIC?, IF SO THIS WILL BE NON-ZERO          
*        BNZ   ITEMS1                                                           
         BAS   RE,MUSNGCHK         SEE IF THERE WERE MUSICIANS OR               
         BNE   ITEMS2              SINGERS ON THIS INVOICE                      
*                                                                               
ITEMS1   LA    R1,1                ONLY ONE WORKCODE                            
         LA    R3,WKCDTB1A         CE00                                         
         LA    R5,WKCDSCRA                                                      
         LA    R2,CNTMUS                                                        
         L     RF,CNTMUS           ADD ALL COSTS TO CNTMUS                      
         A     RF,CNTTUSGE                                                      
         A     RF,CNTPNH                                                        
         A     RF,CNTHAND                                                       
         ST    RF,CNTMUS           MUSIC POSTS ALL AMOUNTS TO CE00              
         B     ITEMS3                                                           
*                                                                               
ITEMS2   OC    CNTMUS,CNTMUS                                                    
         BZ    ITEMS3                                                           
         L     RF,CNTTUSGE                                                      
         A     RF,CNTMUS                                                        
         ST    RF,CNTTUSGE                                                      
*                                                                               
ITEMS3   OC    0(4,R2),0(R2)       ONLY SHOW NON-ZERO AMOUNTS                   
         BZ    ITEMS5                                                           
         BAS   RE,SUBITM           TAG THEM                                     
         LH    RF,INVITMCT                                                      
         LA    RF,10(RF)                                                        
         STH   RF,INVITMCT                                                      
*                                                                               
ITEMS5   LA    R2,L'CNTRS(R2)      BUMP IT UP                                   
         LA    R3,4(R3)                                                         
         LA    R5,4(R5)                                                         
         BCT   R1,ITEMS3                                                        
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE EACH ITEM PORTION OF INVOICES                       
*======================================================================         
SUBITM   NTR1                                                                   
*                                                                               
         MVC   TMPAMT,0(R2)        SAVE IT                                      
         LA    R2,P                                                             
         GOTO1 =A(TAGS),DMCB,XGITMST       <Item>                               
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P+17                                                          
         GOTO1 =A(TAGS),DMCB,XGPORDS       <PO_Num>                             
         BAS   RE,SETPO                                                         
         GOTO1 =A(TAGS),DMCB,XGPORDE       </PO_Num>                            
         BAS   RE,SPLATDWN                                                      
*                                                                               
         BAS   RE,POINFO                   GET INFO FROM PURCHASE ORDER         
*                                                                               
         LA    R2,P+17                                                          
         GOTO1 =A(TAGS),DMCB,XGAMNTS       <Amount>                             
         EDIT  TMPAMT,(15,0(R2)),2,ALIGN=LEFT,FLOAT=-                           
         AR    R2,R0                                                            
         GOTO1 =A(TAGS),DMCB,XGAMNTE       </Amount>                            
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P+17                                                          
         GOTO1 =A(TAGS),DMCB,XGQNTYS       <Quantity>                           
         MVI   0(R2),C'1'                                                       
         LA    R2,1(R2)                                                         
         GOTO1 =A(TAGS),DMCB,XGQNTYE       </Quantity>                          
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P+17                                                          
         GOTO1 =A(TAGS),DMCB,XGUNIT        <Unit>                               
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P                                                             
         GOTO1 =A(TAGS),DMCB,XGITMEN       </Item>                              
         BAS   RE,SPLATDWN                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE GETS INFO FROM PURCHASE ORDER                       
*======================================================================         
POINFO   NTR1                                                                   
         USING TAPUD,R4                                                         
         SR    R1,R1                                                            
         TM    PGMSTAT,PRGSPOER                                                 
         BO    XIT                                                              
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,TAPUELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
POINFO10 BAS   RE,NEXTEL                                                        
         BNE   POINFO30                                                         
*                                                                               
         CLI   TAPUTYPE,TAPUTITM                                                
         BNE   POINFO10                                                         
         CLC   TAPUICOD,=CL10'6SZ0'                                             
         BNE   *+10                                                             
         MVC   TAPUICOD,=CL10'HG03'                                             
         CLC   0(4,R3),TAPUICOD     MATCH ON WORKCODE                           
         BNE   POINFO10                                                         
*                                                                               
         LA    R2,P+17                                                          
         GOTO1 =A(TAGS),DMCB,XGPITMS       <PO_Item>                            
         MVC   0(5,R2),TAPUINUM                                                 
         LA    R2,5(R2)                                                         
         GOTO1 =A(TAGS),DMCB,XGPITME       </PO_Item>                           
         BAS   RE,SPLATDWN                                                      
*                                                                               
POINFO30 LA    R2,P+17                                                          
         GOTO1 =A(TAGS),DMCB,XGINVIS       <InvoiceItem>                        
         EDIT  INVITMCT,(4,0(R2)),ZERO=NOBLANK,FILL=0                           
         LA    R2,4(R2)                                                         
         GOTO1 =A(TAGS),DMCB,XGINVIE       </InvoiceItem>                       
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P+17                                                          
         GOTO1 =A(TAGS),DMCB,XGITMCS       <ItemCode>                           
         MVC   0(4,R2),0(R3)                                                    
         LA    R2,4(R2)                                                         
         GOTO1 =A(TAGS),DMCB,XGITMCE       </ItemCode>                          
         BAS   RE,SPLATDWN                                                      
*                                                                               
         LA    R2,P+17                                                          
         GOTO1 =A(TAGS),DMCB,XGITMDS       <ItemDesc>                           
         L     RF,0(R5)                                                         
         GOTO1 =A(TAGS),DMCB,(RF)                                               
         GOTO1 =A(TAGS),DMCB,XGITMDE       </ItemDesc>                          
         BAS   RE,SPLATDWN                                                      
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE PUTS THE XML TAGS WHERE R2 IS                       
*                   R2 IS ALSO UPDATED                                          
*                                                                               
*======================================================================         
TAGS     DS    0H                                                               
         L     RF,DMCB             A(XML TAG)                                   
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),1(RF)                                                    
         LA    R2,1(R1,R2)                                                      
         BR    RE                                                               
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE SETS AGENCY VALUES IF AGY FLIST REQUESTED           
*======================================================================         
*                                                                               
SETTAGY  NTR1                                                                   
         MVI   REREADSW,C'N'       NO NEED TO READ SYSIO'S KEY                  
         CLI   SITAGY,C'@'         IF FLIST REQUESTED                           
         BNE   XIT                                                              
*                                                                               
         LA    RE,MAXTBLS          RE=MAXIMUM TABLE ENTRIES                     
         LA    R2,AGYTAB           R2=A(AGENCY TABLE)                           
         USING AGYTABD,R2                                                       
SETTAGY2 OC    0(AGYTABL,R2),0(R2) TEST END OF TABLE                            
         BZ    SETTAGY4                                                         
         CLC   TIAGY,AGYTAGY       MATCH ON AGENCY                              
         BNE   SETTAGY3                                                         
         MVC   SVAGYCO,AGYTCO                                                   
         MVC   LENGTHS,AGYTLEN                                                  
         B     XIT                                                              
*                                                                               
SETTAGY3 LA    R2,AGYTABL(R2)       TRY NEXT TABLE ENTRY                        
         BCT   RE,SETTAGY2                                                      
*                                                                               
         LA    R2,AGYTAB           END OF TABLE - SET TO REUSE BEG              
         XC    0(10*AGYTABL,R2),0(R2)                                           
*                                                                               
SETTAGY4 MVC   AIO,AIO2            NOT IN TABLE- ADD NEW ENTRY                  
         MVI   REREADSW,C'Y'       REREAD SYSIO'S KEY                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',TIAGY)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,SETAGY           SET AGENCY INFO                              
         MVC   AIO,TIAREC                                                       
         TM    TUOPTS,TURESND                                                   
         BZ    *+10                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   AGYTCO,SVAGYCO                                                   
         MVC   AGYTLEN,LENGTHS                                                  
         MVC   AGYTAGY,TIAGY                                                    
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE PRINTS PO NUMBER IN R2                              
*======================================================================         
VALPO    NTR1                                                                   
*                                                                               
         NI    PGMSTAT,X'FF'-PRGSPOER                                           
         MVC   SAVPO,=CL10'0000000000'                                          
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTAUT))                                     
*        GOTO1 GETL,DMCB,(1,=AL1(TANUTEST))                                     
         BE    VALPO05                                                          
         LA    R2,1                                                             
         B     VALPONO                                                          
*                                                                               
VALPO05  L     R1,TGELEM                                                        
         USING TANUD,R1                                                         
         CLI   TANUMBER,C'0'                                                    
         BL    VALPONO                                                          
*                                                                               
         ZIC   RE,TANULEN                                                       
         SH    RE,=Y(TANULNQ+1)     RE=LENGTH OF TANUMBER+1                     
         CHI   RE,L'SAVPO                                                       
         BNH   *+8                                                              
         LHI   RE,L'SAVPO                                                       
         LA    R2,SAVPO+L'SAVPO-1                                               
         SR    R2,RE                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TANUMBER                                                 
         OC    SAVPO,=CL10'0000000000'                                          
*                                                                               
         XR    R2,R2                                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLPUD,R4                                                         
         MVI   TLPUCD,TLPUCDQ                                                   
         MVI   TLPUSCD,TLPUSCDQ                                                 
         MVC   TLPUAGY,=CL6'OMNY'                                               
*        MVC   TLPUPO,=CL10'0000050774'      *** Test ***                       
         MVC   TLPUPO,SAVPO                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLPUKEY),KEYSAVE                                           
         BE    VALPO10                                                          
*        LA    R2,1                                                             
         OI    PGMSTAT,PRGSPOER                                                 
         B     VALPD90                                                          
*                                                                               
VALPO10  MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
VALPD90  MVC   AIO,TIAREC                                                       
         TM    TUOPTS,TURESND                                                   
         BZ    *+10                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         CLC   TIKEY,KEY                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         BAS   RE,RESTTIA                                                       
VALPONO  LTR   R2,R2                                                            
         B     *+6                                                              
VALPOYES CR    RE,RE                                                            
VALPOZ   XIT1                                                                   
         DROP  R1                                                               
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE PRINTS COMPANY CODE IN R2                           
*======================================================================         
SETCCODE NTR1                                                                   
*                                                                               
         L     R5,AIO              SAVE AIO                                     
         MVC   AIO,AIO2                                                         
         MVI   ELCODE,TAPUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPUTSUM))                                     
         BNE   SETCCX                                                           
         L     R1,TGELEM                                                        
         USING TAPUD,R1                                                         
         MVC   0(4,R2),TAPUSCPY                                                 
         LA    R2,4(R2)                                                         
*                                                                               
SETCCX   ST    R5,AIO              RESTORE AIO                                  
         XIT1  REGS=(R2)                                                        
         DROP  R1                                                               
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE PRINTS PO NUMBER IN R2                              
*======================================================================         
SETPO    NTR1                                                                   
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTAUT))                                     
         BE    SETPO10                                                          
*                                                                               
         BAS   RE,RESTTIA                                                       
         GOTO1 GETL,DMCB,(1,=AL1(TANUTAUT))                                     
         BNE   SETPOX                                                           
*                                                                               
SETPO10  L     R1,TGELEM                                                        
         USING TANUD,R1                                                         
         ZIC   RE,TANULEN                                                       
         SH    RE,=Y(TANULNQ+1)     RE=LENGTH OF TANUMBER+1                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TANUMBER                                                 
         LA    R2,1(RE,R2)                                                      
*                                                                               
SETPOX   XIT1  REGS=(R2)                                                        
         DROP  R1                                                               
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE SETS INFO FROM TAPDD                                
*======================================================================         
SETAPD   NTR1                                                                   
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
*                                                                               
         GOTO1 USEVAL,DMCB,(X'20',TAPDUSE),TAPDTYPE                             
*                                                                               
         BAS   RE,FIXCAN           ADJUST TO US$ IF NECESSARY                   
         L     R1,TAPDGRS                                                       
         A     R1,TAPDREXP                                                      
         A     R1,TAPDAPPL                                                      
         A     R1,TAPDGUAR                                                      
*                                                                               
         LA    R2,CNTTUSGE         TALENT USAGE (DEFAULT)                       
         TM    TGUSXUNS,AFM                                                     
         BO    SETTAPD5                                                         
         TM    TGUSSTAT,SESSION                                                 
         BZ    SETTAPD5                                                         
         LA    R2,CNTMUS                                                        
*                                                                               
SETTAPD5 ST    R1,0(R2)                                                         
*                                                                               
         L     R1,TAPDPNH          P&H                                          
         ST    R1,CNTPNH                                                        
*                                                                               
         B     XIT                                                              
*                                                                               
*======================================================================         
*              THIS ROUTINE SETS AMOUNTS FROM TABDD                             
*======================================================================         
SETABD   NTR1                                                                   
         MVI   ELCODE,TABDELQ      GET BILLING DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TABDD,R4                                                         
         MVC   CNTINV,TABDTOT      INVOICE AMOUNT                               
*                                                                               
         L     R2,TABDTAX                                                       
         A     R2,TABDFICR                                                      
         ST    R2,CNTTAX           PAYROLL TAXES                                
*                                                                               
         L     R2,TABDHND                                                       
         A     R2,TABDHNDC                                                      
         ST    R2,CNTHAND          HANDLING                                     
*                                                                               
         L     RF,TABDGST          GST AND PST                                  
         A     RF,TABDPST                                                       
         ST    RF,CNTGST                                                        
*                                                                               
         TM    TABDSTAT,TABDSCNW   TEST T&H IN CAN$                             
         BZ    SETABD7                                                          
         LA    R1,CNTTAX                                                        
         BAS   RE,FIXCAN6                                                       
         LA    R1,CNTHAND                                                       
         BAS   RE,FIXCAN6                                                       
         LA    R1,CNTGST                                                        
         BAS   RE,FIXCAN6                                                       
*                                                                               
SETABD7  B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              ROUTINE TO PUT A RECORD TO SORTER                                
*======================================================================         
PUTSORT  NTR1                                                                   
         CLI   SORTINIT,C'Y'                                                    
         BE    PUTSORTX                                                         
         MVI   SORTINIT,C'Y'                                                    
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD                                   
*                                                                               
PUTSORTX DS    0H                                                               
         GOTO1 MYSORTER,DMCB,=C'PUT',SORTIO                                     
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              ROUTINE TO PUT A RECORD TO TAPE OR DATASET                       
*              IF WCODE OPTION - R3 = A(HEADER)                                 
*                                R3 = A(DETAIL)                                 
*              ELSE              R5 = A(TAPEREC)                                
*======================================================================         
PUTIT    NTR1                                                                   
         TM    PGMSTAT,PRGSTOPN                                                 
         BO    *+8                                                              
         BAS   RE,OPENTAPE                                                      
*                                                                               
         L     R2,OIDSKDCB         OIDISK = REPORT                              
         PUT   (R2),(R5)           PUT IT TO TAPE/DATASET                       
*                                                                               
PUTITX   B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              ROUTINE TO OPEN TAPE/DATASET                                     
*======================================================================         
OPENTAPE NTR1                                                                   
         L     R2,OIDSKDCB                                                      
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LINKX MF=(E,PARMLST),SF=(E,LINKLST)  GET GENERATION NUMBER             
*                                                                               
OPENTX   OI    PGMSTAT,PRGSTOPN    TAPE ALREADY OPENED                          
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              ROUTINE TO CLOSE THE TAPE/DATASET                                
*======================================================================         
CLOSTAPE NTR1                                                                   
         TM    PGMSTAT,PRGSTOPN                                                 
         BZ    CLOSTX                                                           
*                                                                               
         L     R2,OIDSKDCB         REPORT                                       
         CLOSE ((2))                                                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CLOSTX   B     XIT                                                              
         EJECT                                                                  
*=====================================================================          
*              ROUTINE TO SEND INVOICES MARKED FOR RESEND                       
*=====================================================================          
RESEND   NTR1                                                                   
         OI    TUOPTS,TURESND                                                   
         CLI   AGYFLST,X'FF'     IF RUNNING FOR SPECIFIC AGENCY                 
         BNE   RSND100           PUT JUST THAT AGENCY INTO FLIST                
         MVC   AGYFLST(L'TGAGY),TGAGY                                           
         MVI   AGYFLST+L'TGAGY,X'FF'                                            
*                                                                               
RSND100  MVC   AIO,AIO1                                                         
*                                                                               
         LA    R5,AGYFLST        IF NO MORE AGENCIES IN FLIST                   
RSND200  CLI   0(R5),X'FF'       THEN DONE                                      
         BE    RESENDX                                                          
*                                                                               
         USING TLRID,R4                                                         
         LA    R4,KEY            SET TO READ SPECIAL RESEND INVOICE             
         XC    KEY,KEY           PASSIVE KEYS FOR THIS AGENCY                   
         MVI   TLRICD,TLRICDQ                                                   
         MVI   TLRISCD,TLRISCDQ                                                 
         MVC   TLRIAGY,0(R5)                                                    
         GOTO1 HIGH                                                             
         B     RSND400                                                          
RSND300  GOTO1 SEQ                                                              
RSND400  CLC   KEY(TLRIINV-TLRIKEY),KEYSAVE                                     
         BNE   RSND700                                                          
*                                                                               
         MVC   TIKEY,KEY         SET SYSIO KEY AND DISK ADDRESS                 
         MVC   TIDSKADD,KEY+TLDRDA-TLDRD                                        
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING TLIND,R4                                                         
         L     R4,AIO           SET SYSIO VARIABLES FROM RECORD KEY             
         XC    TICODES,TICODES                                                  
         MVC   TIAGY,TLINAGY                                                    
         MVC   TIINV,TLININV                                                    
         XC    TIINV,=6X'FF'                                                    
         DROP  R4                                                               
*                                                                               
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ   SET SYSIO VARIABLES FROM PAYMENT                
         BAS   RE,GETEL         DETAILS ELEMENT                                 
         BNE   RSND500                                                          
         LR    R1,R4                                                            
         MVC   TIEMP,TAPDEMP                                                    
         MVC   TIOFF,TAPDOFF                                                    
         MVC   TICLI,TAPDCLI                                                    
         MVI   TICUR,C'U'                                                       
         TM    TAPDSTAT,TAPDSCAN                                                
         BZ    RSND500                                                          
         MVI   TICUR,C'C'                                                       
         DROP  R4                                                               
*                                                                               
         USING TAIND,R4                                                         
RSND500  L     R4,AIO            SET SYSIO VARABLES FROM INVOICE                
         MVI   ELCODE,TAINELQ    DETAILS ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   RSND600                                                          
         TM    TAINSTA2,TAINSPRM                                                
         BO    RSND300                                                          
         MVC   TIBIDATE,TAINBDTE                                                
         TM    TAPDOPT3-TAPDD(R1),TAPDODUM                                      
         BNO   RSND600                                                          
         MVC   TIBIDATE,TAINPDTE                                                
         DROP  R4                                                               
*                                                                               
RSND600  MVI   TIMODE,PROCREC      EXECUTE RECORD HOOK                          
         BAS   RE,IOHOOK                                                        
*                                                                               
         MVC   KEY,TIKEY           REREAD THE KEY                               
         GOTO1 HIGH                AND DELETE IT                                
         CLC   KEY(L'TIKEY),TIKEY                                               
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    KEY+TLRISTAT-TLRID,X'80'                                         
         GOTO1 WRITE                                                            
*                                                                               
         B     RSND300             GO READ NEXT KEY                             
*                                                                               
RSND700  LA    R5,L'TIAGY(R5)                                                   
         B     RSND200             GET ANOTHER AGENCY                           
RESENDX  B     XIT                                                              
*                                                                               
         EJECT                                                                  
*=====================================================================          
*              THIS ROUTINE CHECKS IF THERE WERE OFF-CAMERA                     
*              MUSICIANS OR OFF-CAMERA SINGERS ON INVOICE                       
*=====================================================================          
MUSNGCHK NTR1                                                                   
         BAS   RE,SETCHK           SET TO READ CHECK FILE                       
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
         USING TACAD,R4                                                         
         MVI   TGBYTE,C'N'         SAVE WHETHER PERFORMER IS ON                 
         L     R4,AIO              OR OFF CAMERA IN TGBYTE                      
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   MSCHK20                                                          
         CLC   TACAONOF,=C'OFF'                                                 
         BNE   MSCHK20                                                          
         MVI   TGBYTE,C'F'                                                      
         DROP  R4                                                               
*                                                                               
         USING CATTABD,R3                                                       
         L     R3,TGACATS          R3=A(CATEGORY TABLE)                         
MSCHK10  CLI   0(R3),X'FF'         EOT?                                         
         BNE   *+6                                                              
         DC    H'0'                CATEGORY MUST BE THERE                       
         CLC   TLCKCAT,CATCDE      MATCH CATEGORY                               
         BE    MSCHK20                                                          
         ZIC   RE,CATLEN                                                        
         AR    R3,RE                                                            
         B     MSCHK10                                                          
*                                                                               
MSCHK20  BAS   RE,SETTAL           RESTORE TO READ TALENT FILE                  
         MVC   KEY,TIKEY           RESTORE READ SEQUENCE                        
         GOTO1 HIGH                                                             
         GOTO1 GETREC              AND RESTORE AIO                              
*                                                                               
         CLI   TGBYTE,C'F'         WAS PERFORMER OFF CAMERA?                    
         BNE   MSCHKNO                                                          
         TM    CATUNI,AFM          AND WAS HE/SHE A MUSICIAN ...                
         BO    MSCHKYES                                                         
         TM    CATSTAT,SINGER      OR A SINGER?                                 
         BZ    MSCHKNO                                                          
         DROP  R3                                                               
*                                                                               
MSCHKYES SR    RC,RC                                                            
MSCHKNO  LTR   RC,RC                                                            
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
*=====================================================================          
*              ROUTINE TO SUBMIT JCL TO POWWOW                                  
*              FOR ADVANTIS TRANSMISSION                                        
*=====================================================================          
CALLPOW  NTR1                                                                   
         MVC   TAPEC4,RTGEN        UPDATE GENERATION NUMBER                     
         MVC   FILE3(5),RTGEN      "                                            
         MVC   SUBJ2(1),FILEOPT    MOVE IN CORRECT DCB FROM FILEOPT             
         MVC   FILE2(1),FILEOPT                                                 
         MVC   TAPEC2(1),FILEOPT                                                
         MVC   DSNME2(1),FILEOPT                                                
         LA    R0,NUMCRD           R0=N'OF JCL CARDS                            
         LA    R3,JCL              R3=A(JCL FOR POWWOW)                         
*                                                                               
CALLP5   TM    TUOPTS,TUTRACE      IF TRACING REQUESTED                         
         BZ    CALLP8                                                           
         MVC   P,0(R3)             PRINT JCL                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CALLP8   MVC   POWJCL,0(R3)                                                     
         TM    TUOPTS,TUNOPOW      UNLESS OTHERWISE REQUESTED                   
         BO    CALLP10                                                          
         GOTO1 =V(POWWOW),DMCB,=C'PUT',=C'POWER',POWKEY,POWHEAD                 
*                                                                               
CALLP10  LA    R3,L'JCL(R3)                                                     
         BCT   R0,CALLP5                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SAVE TIAREC                                           
         SPACE 1                                                                
SAVETIA  NTR1                                                                   
         L     RE,TIAREC                                                        
         TM    TUOPTS,TURESND                                                   
         BZ    *+8                                                              
         L     RE,AIO1                                                          
*                                                                               
         LA    RF,SVTIAREC                                                      
         SPACE 1                                                                
         LHI   R1,8                                                             
         B     SRTIA10                                                          
         SPACE 2                                                                
*              ROUTINE TO RESTORE SAVED TIAREC                                  
         SPACE 1                                                                
RESTTIA  NTR1                                                                   
         LA    RE,SVTIAREC                                                      
         L     RF,TIAREC                                                        
         TM    TUOPTS,TURESND                                                   
         BZ    *+8                                                              
         L     RF,AIO1                                                          
*                                                                               
         LHI   R1,8                                                             
SRTIA10  MVC   0(250,RF),0(RE)                                                  
         LA    RE,250(RE)                                                       
         LA    RF,250(RF)                                                       
         BCT   R1,SRTIA10                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS, CONSTANTS, ETC.                                   
*                                                                               
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
COMPLM   DC    6X'FF'                                                           
         SPACE 2                                                                
*              WORK CODE TABLE 1 TO CORRESPOND TO INVOICE AMOUNTS               
WKCDTB1  DS    0CL4                                                             
         DC    CL4'TU00'           TALENT USAGE + PNH + T&H                     
**NO-OP  DC    CL4'TU00'           TALENT USAGE                                 
**NO-OP  DC    CL4'YJ00'           PENSION AND HEALTH                           
**NO-OP  DC    CL4'HG03'           PAYROLL TAX AND HANDLING                     
*KCDTB1A DC    CL4'CE00'           MUSICIANS PAYMENTS                           
WKCDTB1A DC    CL4'CE01'           MUSICIANS PAYMENTS                           
         DC    CL4'HG03'           HANDLING                                     
         DC    CL4'HG03'           GOODS AND SERVICE TAX                        
         SPACE 2                                                                
*              WORK CODE TABLE 2 TO CORRESPOND TO CHECK AMOUNTS                 
WKCDTB2  DS    0CL10                                                            
         DC    CL10'ZC'            SIGNERS PAYMENTS                             
         DC    CL10'ZD'            ON CAMERA TALENT                             
         DC    CL10'ZE'            EXTRAS PAYMENTS                              
         DC    CL10'ZF'            VOICE OVER PAYMENTS                          
         DC    CL10'ZH'            HAND MODELS PAYMENTS                         
         DC    CL10'ZZ'            CARTAGE AMOUNTS WAS ZX 4/10/96               
         DC    CL10'ZY'            WARDROBE AMOUNTS                             
         DC    CL10'ZZ'            MISCELLANEOUS AMOUNTS                        
         SPACE 2                                                                
*              WORK CODE TABLE 3 TO CORRESPOND TO CHECK AMOUNTS                 
*              SPECIFICALLY FOR CLIENT ATT                                      
WKCDTB3  DS    0CL10                                                            
         DC    CL10'ZA'            MUSICIANS PAYMENTS                           
         DC    CL10'ZA'            SIGNERS PAYMENTS                             
         DC    CL10'ZA'            ON CAMERA TALENT                             
         DC    CL10'ZA'            EXTRAS PAYMENTS                              
         DC    CL10'ZA'            VOICE OVER PAYMENTS                          
         DC    CL10'ZA'            HAND MODELS PAYMENTS                         
         DC    CL10'ZA'            CARTAGE AMOUNTS                              
         DC    CL10'ZA'            WARDROBE AMOUNTS                             
         DC    CL10'ZA'            MISCELLANEOUS AMOUNTS                        
         SPACE 2                                                                
*              WORK CODE TABLE 1 TO CORRESPOND TO INVOICE AMOUNTS               
WKCDSCR  DS    0F                                                               
         DC    A(XGTOT)            TOTAL - TALENT + PNH + T&H                   
**NO-OP  DC    A(XGTSESS)          TALENT USAGE                                 
**NO-OP  DC    A(XGPNH)            PENSION AND HEALTH                           
**NO-OP  DC    A(XGHAND)           TAX AND HANDLING                             
WKCDSCRA DC    A(XGMUS)            MUSIC FEE                                    
         DC    A(XGPYTX)           PAYROLL TAX                                  
         DC    A(XGGST)            GOODS AND SERVICE TAX                        
         SPACE 2                                                                
*----------------------------------------------------------------------         
*                               ** REPLACED WITH SETXML **                      
*&&DO                                                                           
AGYACCTB DC    0CL18                                                            
         DC    CL6'CONQ  ',CL12'104883'                                         
         DC    CL6'GRDR  ',CL12'133388'                                         
         DC    CL6'OAOCH ',CL12'104885'                                         
         DC    CL6'ODCH  ',CL12'106290'                                         
         DC    CL6'ODHO  ',CL12'104883'                                         
         DC    CL6'ODLA  ',CL12'104883'                                         
         DC    CL6'ODNY  ',CL12'104883'                                         
         DC    CL6'OMAT  ',CL12'104885'                                         
         DC    CL6'OMCH  ',CL12'106290'                                         
         DC    CL6'OMDA  ',CL12'104883'                                         
         DC    CL6'OMDE  ',CL12'104885'                                         
         DC    CL6'OMHO  ',CL12'104885'                                         
         DC    CL6'OMLA  ',CL12'104884'                                         
         DC    CL6'OMNY  ',CL12'104883'                                         
         DC    CL6'OMPC  ',CL12'104885'                                         
         DC    CL6'OPCH  ',CL12'106290'                                         
         DC    CL6'OPNY  ',CL12'104883'                                         
         DC    CL6'SQNY  ',CL12'104883'                                         
         DC    CL6'1578  ',CL12'104884'                                         
         DC    CL6'2437  ',CL12'119925'                                         
         DC    CL6'3899  ',CL12'133388'                                         
         DC    CL6'4361  ',CL12'122327'                                         
         DC    CL6'9036  ',CL12'104125'                                         
         DC    CL6'9059  ',CL12'104884'                                         
         DC    CL6'9078  ',CL12'104885'                                         
         DC    CL6'9085  ',CL12'104125'                                         
         DC    CL6'9095  ',CL12'104125'                                         
         DC    CL6'9338  ',CL12'104125'                                         
         DC    X'FF'                                                            
*&&                                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(240)'                                 
*                                                                               
OIDISK1  DCB   DDNAME=OIDISK1,DSORG=PS,RECFM=FB,LRECL=132,             X        
               BLKSIZE=13200,MACRF=PM                                           
OIDISK2  DCB   DDNAME=OIDISK2,DSORG=PS,RECFM=FB,LRECL=132,             X        
               BLKSIZE=13200,MACRF=PM                                           
OIDISK3  DCB   DDNAME=OIDISK3,DSORG=PS,RECFM=FB,LRECL=132,             X        
               BLKSIZE=13200,MACRF=PM                                           
*                                                                               
JCL      DS    0CL80                                                            
JOBC     DC    CL80'//TPCHTOMP  JOB   ''CNTL'',MSGLEVEL=(1,1),COND=((0,X        
               NE)),'                                                           
         DC    CL80'//     MSGCLASS=X,CLASS=A,NOTIFY=EVIR'                      
         DC    CL80'//*MAIN CLASS=ADVANTIS,SYSTEM=SY1'                          
         DC    CL80'//SS  EXEC  BDEDICT'                                        
         DC    CL80'//EXTSYSIN DD *'                                            
*                                                                               
EDICT1   DC    C'EDICTKEY='                                                     
EDICT2   DC    C'FTPINTE'                                                       
EDICT3   DC    CL(80-(*-EDICT1))' '                                             
*                                                                               
SUBJ1    DC    C'SUBJECT=CLA(TA0OMDS'                                           
SUBJ2    DC    C'1'               CHANGE NUMBER TO MATCH FILE NUMBER            
         DC    C'),CHA(3),ACC('                                                 
SUBJ3    DC    C'LNSC'                                                          
         DC    C'),USE('                                                        
SUBJ4    DC    C'AASCMRP'                                                       
         DC    C'),MOD(0),'                                                     
         DC    CL(80-(*-SUBJ1))' '                                              
*                                                                               
FILE1    DC    C'FILE=TA0OMDS'                                                  
FILE2    DC    C'1'               CHANGE NUMBER TO MATCH FILE NUMBER            
         DC    C'.'                                                             
FILE3    DC    C'G0000'                                                         
FILE4    DC    CL(80-(*-FILE1))' '                                              
*                                                                               
         DC    CL80'EXT=XML'                                                    
*                                                                               
TAPEC1   DC    C'DSN=TALDISK.TA0OMDS'                                           
TAPEC2   DC    C'1'               CHANGE NUMBER TO MATCH FILE NUMBER            
TAPEC3   DC    CL1'.'                                                           
TAPEC4   DC    CL8'G0000000'                                                    
TAPEC5   DC    CL(80-(*-TAPEC1))' '                                             
*                                                                               
NUMCRD   EQU   (*-JCL)/80                                                       
*                                                                               
POWKEY   DC    CL10' '                                                          
POWHEAD  DC    XL8'00'                                                          
POWJCL   DS    CL80                                                             
PARMLST  CALL  ,(DSNME,RETAREA),MF=L                                            
LINKLST  LINKX EP=FRGETDSN,SF=L                                                 
*                                                                               
DSNME    DC    0CL8                                                             
         DC    C'OIDISK'                                                        
DSNME2   DC    C'1'               CHANGE NUMBER TO MATCH FILE NUMBER            
         DC    C' '                                                             
RETAREA  DS    0CL44                                                            
RTDSN    DC    CL17' '                                                          
RTGEN    DC    CL8' '        G0000000                                           
RTND     DC    CL19' '       SPARE                                              
ERRCHK   DS    CL1                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
XGDOCEN  DC    AL1(10),C'</Document>'                                           
XGINVST  DC    AL1(10),C'  <Invoice>'                                           
XGINVEN  DC    AL1(11),C'  </Invoice>'                                          
XGHDRST  DC    AL1(16),C'         <Header>'                                     
XGHDREN  DC    AL1(17),C'         </Header>'                                    
XGITMST  DC    AL1(14),C'         <Item>'                                       
XGITMEN  DC    AL1(15),C'         </Item>'                                      
*                                                                               
* These are indented 17 spaces                                                  
*                                                                               
XGCMPCDS DC    AL1(12),C'<CompanyCode>'                                         
XGCMPCDE DC    AL1(13),C'</CompanyCode>'                                        
XGCURR   DC    AL1(23),C'<Currency>USD</Currency>'                              
XGDOCDT  DC    AL1(28),C'<DocDate>YYYY-MM-DD</DocDate>'                         
XGBLINE  DC    AL1(32),C'<BLineDate>YYYY-MM-DD</BLineDate>'                     
XGREFDCN DC    AL1(28),C'<RefDocNum>MY9999</RefDocNum>'                         
*                                                                               
XGPORDS  DC    AL1(07),C'<PO_Num>'                                              
XGPORDE  DC    AL1(08),C'</PO_Num>'                                             
XGPITMS  DC    AL1(08),C'<PO_Item>'                                             
XGPITME  DC    AL1(09),C'</PO_Item>'                                            
XGINVIS  DC    AL1(12),C'<InvoiceItem>'                                         
XGINVIE  DC    AL1(13),C'</InvoiceItem>'                                        
XGITMCS  DC    AL1(09),C'<ItemCode>'                                            
XGITMCE  DC    AL1(10),C'</ItemCode>'                                           
XGITMDS  DC    AL1(09),C'<ItemDesc>'                                            
XGITMDE  DC    AL1(10),C'</ItemDesc>'                                           
XGAMNTS  DC    AL1(07),C'<Amount>'                                              
XGAMNTE  DC    AL1(08),C'</Amount>'                                             
XGQNTYS  DC    AL1(09),C'<Quantity>'                                            
XGQNTYE  DC    AL1(10),C'</Quantity>'                                           
XGUNIT   DC    AL1(14),C'<Unit>EA</Unit>'                                       
XGTSESS  DC    AL1(11),C'TALENT USAGE'                                          
XGPNH    DC    AL1(13),C'PENSION/HEALTH'                                        
XGMUS    DC    AL1(15),C'MUSIC PRODUCTION'                                      
XGHAND   DC    AL1(21),C'HANDLING FEE AND TAXES'                                
XGPYTX   DC    AL1(04),C'TAXES'                                                 
XGGST    DC    AL1(21),C'GOODS AND SERVICES TAX'                                
XGTOT    DC    AL1(36),C'TALENT USAGE + PNH + TAX AND HANDLING'                 
*                                                                               
         EJECT                                                                  
*              REPORT SPECS                                                     
*                                                                               
MYSPECS  DS    0H                                                               
         SPROG 1,2,10                                                           
         SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
*                                                                               
         SPROG 1,2                                                              
         SSPEC H7,2,C'AGENCY INV DT INV NO PO NUM    CLI PRD JOB'               
         SSPEC H8,2,C'------ ------ ------ ------    --- --- ---'               
*                                                                               
         SPROG 10                                                               
         SSPEC H7,2,C'INV DT INV #  PO NUM    ESTIMATE #'                       
         SSPEC H8,2,C'------ -----  ------    ----------'                       
*                                                                               
         SPROG 1,2,10                                                           
         SSPEC H4,100,C'PERIOD'                                                 
         SSPEC H7,50,C'TY        FEES      P AND H  PAYROLL TAX'                
         SSPEC H8,50,C'--        ----      -------  -----------'                
         SSPEC H7,92,C'   HANDLING          GST  INVOICE TOT'                   
         SSPEC H8,92,C'   --------          ---  -----------'                   
*                                                                               
         SPROG 1,10                                                             
         SSPEC H1,53,C'OGILVY INTERFACE REPORT'                                 
         SSPEC H2,53,23X'BF'                                                    
*                                                                               
         SPROG 2                                                                
         SSPEC H1,46,C'OGILVY INTERFACE REPORT (INCLUDES CID)'                  
         SSPEC H2,46,38X'BF'                                                    
         DC    X'00'                                                            
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE SETS SUMMARY PORTION OF INVOICES                    
*======================================================================         
SUMMARY  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,P                                                             
         GOTO1 =A(TAG3),DMCB,XGSUMST       <Summary>                            
         BRAS  RE,SPLAT2                                                        
*                                                                               
         LA    R2,P+17                                                          
         GOTO1 =A(TAG3),DMCB,XGNAMTS       <NetAmount>                          
         EDIT  CNTINV,(15,0(R2)),2,ALIGN=LEFT,FLOAT=-                           
         AR    R2,R0                                                            
         GOTO1 =A(TAG3),DMCB,XGNAMTE       </NetAmount>                         
         BRAS  RE,SPLAT2                                                        
*                                                                               
         L     RE,CNTTAX           SKIP IF NO TAX                               
         A     RE,CNTGST                                                        
         LTR   RE,RE                                                            
         BZ    SUM300                                                           
*                                                                               
         LA    R2,P+17                                                          
         GOTO1 =A(TAG3),DMCB,XGTTAXS       <TotalTax>                           
         L     RE,CNTTAX           SKIP IF NO TAX                               
         A     RE,CNTGST                                                        
         EDIT  (RE),(15,0(R2)),2,ALIGN=LEFT,FLOAT=-                             
         AR    R2,R0                                                            
         GOTO1 =A(TAG3),DMCB,XGTTAXE       </TotalTax>                          
         BRAS  RE,SPLAT2                                                        
*                                                                               
SUM300   LA    R2,P                                                             
         GOTO1 =A(TAG3),DMCB,XGSUMEN       </Summary>                           
         BRAS  RE,SPLAT2                                                        
         XIT1                                                                   
*                                                                               
XGNAMTS  DC    AL1(14),C'<NetInvoiceAmt>'                                       
XGNAMTE  DC    AL1(15),C'</NetInvoiceAmt>'                                      
XGTTAXS  DC    AL1(09),C'<TotalTax>'                                            
XGTTAXE  DC    AL1(10),C'</TotalTax>'                                           
XGSUMST  DC    AL1(17),C'         <Summary>'                                    
XGSUMEN  DC    AL1(18),C'         </Summary>'                                   
         LTORG                                                                  
*======================================================================         
*              THIS ROUTINE PUTS THE XML TAGS WHERE R2 IS                       
*                   R2 IS ALSO UPDATED                                          
*                                                                               
*======================================================================         
TAG3     DS    0H                                                               
         L     RF,DMCB             A(XML TAG)                                   
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),1(RF)                                                    
         LA    R2,1(R1,R2)                                                      
         BR    RE                                                               
         EJECT                                                                  
*=====================================================================          
*              THIS ROUTINE SETS XML HEADER INFO                                
*=====================================================================          
SETXML   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XGXMLHD       <?xml>                               
         BRAS  RE,SPLAT2                                                        
         MVI   LINE,1              PREVENT PAGE BREAK                           
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XGDOCST       <Document>                           
         BRAS  RE,SPLAT2                                                        
*        MVI   LINE,1              PREVENT PAGE BREAK                           
*        LA    R2,P                                                             
*        GOTO1 =A(TAG2),DMCB,XGDOCS2       <Document>                           
*        BRAS  RE,SPLAT2                                                        
         MVI   LINE,1              PREVENT PAGE BREAK                           
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XGVENIDS      <VendorID>                           
         MVC   0(6,R2),=C'104883'  (DEFAULT=NEW YORK)                           
*                                                                               
         CLC   TGLST,SPACES        FLIST USED?                                  
         BNH   SETXML5                                                          
         CLC   TGLST,=CL8'OMINT'                                                
         BNE   *+14                                                             
         MVC   0(6,R2),=C'104883'  OMINT (NEW YORK)                             
         B     SETXML7                                                          
*                                                                               
         CLC   TGLST,=CL8'OMPRT'                                                
         BNE   *+14                                                             
         MVC   0(6,R2),=C'104125'  OMPRT (PRINT 9085/9095)                      
         B     SETXML7                                                          
*                                                                               
         CLC   TGLST,=CL8'GEOM'                                                 
         BNE   SETXML8                                                          
         MVC   0(6,R2),=C'133388'  GEOMETRY (GRDR,3899)                         
         B     SETXML7                                                          
*---------------------------------------------------------                      
SETXML5  CLC   TGAGY,=CL8'OMCH'                                                 
         BNE   *+14                                                             
         MVC   0(6,R2),=C'106290'  OMCH                                         
         B     SETXML7                                                          
*                                                                               
         CLC   TGAGY,=CL8'OMDE'                                                 
         BNE   *+14                                                             
         MVC   0(6,R2),=C'104885'  OMDE                                         
         B     SETXML7                                                          
*                                                                               
         CLC   TGAGY,=CL8'2437'                                                 
         BNE   *+14                                                             
         MVC   0(6,R2),=C'119925'  2437                                         
         B     SETXML7                                                          
*                                                                               
         CLC   TGAGY,=CL8'4361'                                                 
         BNE   *+14                                                             
         MVC   0(6,R2),=C'122327'  4361                                         
         B     SETXML7                                                          
*                                                                               
         CLC   TGAGY,=CL8'GRDR'                                                 
         BNE   *+14                                                             
         MVC   0(6,R2),=C'133388'  GRDR                                         
         B     SETXML7                                                          
*                                                                               
         CLC   TGAGY,=CL8'3899'                                                 
         BNE   *+14                                                             
         MVC   0(6,R2),=C'133388'  3899                                         
         B     SETXML7                                                          
*                                                                               
         CLC   TGAGY,=CL8'OMLA'                                                 
         BNE   SETXML8                                                          
         MVC   0(6,R2),=C'104884'  OMLA                                         
         B     SETXML7                                                          
*                                                                               
SETXML7  MVC   SVVENDR,0(R2)                                                    
         LA    R2,6(R2)                                                         
*                                                                               
SETXML8  GOTO1 =A(TAG2),DMCB,XGVENIDE      </VENDORID>                          
         BRAS  RE,SPLAT2                                                        
         MVI   LINE,1              PREVENT PAGE BREAK                           
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XGVENNM       <VendorName>                         
         BRAS  RE,SPLAT2                                                        
         MVI   LINE,1              PREVENT PAGE BREAK                           
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XGRCVID       <ReceiverID>                         
         BRAS  RE,SPLAT2                                                        
         MVI   LINE,1              PREVENT PAGE BREAK                           
         LA    R2,P                                                             
         GOTO1 =A(TAG2),DMCB,XGDTTTS       <DateTime>                           
         GOTO1 DATCON,DMCB,(5,0),(23,0(R2))                                     
         LA    R2,10(R2)                                                        
         MVC   0(9,R2),=C'THH:MM:00'                                            
         LA    R2,1(R2)                                                         
*        TIME  DEC                                                              
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,TMPTIME                                                    
         ST    R1,FULL                                                          
         AP    TMPTIME(4),FULL                                                  
         CP    TMPTIME(4),=P'240000'                                            
         BL    *+10                                                             
         SP    TMPTIME(4),=P'240000'                                            
         ICM   R1,15,TMPTIME      ALIGN IT                                      
         SLL   R1,4                                                             
         STCM  R1,15,TMPTIME                                                    
*                                                                               
         GOTO1 HEXOUT,DMCB,TMPTHRS,0(R2),1                                      
         LA    R2,3(R2)                                                         
         GOTO1 HEXOUT,DMCB,TMPTMIN,0(R2),1                                      
         LA    R2,3(R2)                                                         
         GOTO1 HEXOUT,DMCB,TMPTSEC,0(R2),1                                      
         LA    R2,2(R2)                                                         
*                                                                               
         GOTO1 =A(TAG2),DMCB,XGDTTTE                                            
         BRAS  RE,SPLAT2                                                        
         MVI   LINE,1              PREVENT PAGE BREAK                           
SETXMLX  XIT1                                                                   
*                                                                               
XGXMLHD  DC    AL1(20),C'<?xml version="1.0"?>'                                 
XGDOCST  DC    AL1(63),C'<Document xmlns:xsi="http://www.w3.org/2001/XMX        
               LSchema-instance">'                                              
XGDOCS2  DC    AL1(54),C'  xsi:noNamespaceSchemaLocation=''Inbound InvoX        
               ice.xsd''>     '                                                 
XGVENID  DC    AL1(28),C'  <VendorID>104883</VendorID>'                         
XGVENIDS DC    AL1(11),C'  <VendorID>'                                          
XGVENIDE DC    AL1(10),C'</VendorID>'                                           
XGVENNM  DC    AL1(41),C'  <VendorName>Talent Partners</VendorName>'            
XGRCVID  DC    AL1(36),C'  <ReceiverID>PHN910FICO</ReceiverID>'                 
XGDTTTS  DC    AL1(11),C'  <DateTime>'                                          
XGDTTTE  DC    AL1(10),C'</DateTime>'                                           
         LTORG                                                                  
*======================================================================         
*              THIS ROUTINE PUTS THE XML TAGS WHERE R2 IS                       
*                   R2 IS ALSO UPDATED                                          
*                                                                               
*======================================================================         
TAG2     DS    0H                                                               
         L     RF,DMCB             A(XML TAG)                                   
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),1(RF)                                                    
         LA    R2,1(R1,R2)                                                      
         BR    RE                                                               
         EJECT                                                                  
*======================================================================         
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
*======================================================================         
SPLAT2   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,P                                                             
         BAS   RE,PUTIT2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              PREVENT PAGE BREAK                           
SPLAT2X  XIT1                                                                   
         EJECT                                                                  
*======================================================================         
*              ROUTINE TO PUT A RECORD TO TAPE OR DATASET                       
*              IF WCODE OPTION - R3 = A(HEADER)                                 
*                                R3 = A(DETAIL)                                 
*              ELSE              R5 = A(TAPEREC)                                
*======================================================================         
PUTIT2   NTR1                                                                   
         TM    PGMSTAT,PRGSTOPN                                                 
         BO    PUTIT210                                                         
         L     R2,OIDSKDCB         OIDISK = REPORT                              
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LINKX MF=(E,PARMLST),SF=(E,LINKLST)  GET GENERATION NUMBER             
*                                                                               
         OI    PGMSTAT,PRGSTOPN    TAPE ALREADY OPENED                          
*                                                                               
PUTIT210 L     R2,OIDSKDCB         OIDISK = REPORT                              
         PUT   (R2),(R5)           PUT IT TO TAPE/DATASET                       
*                                                                               
PUTIT2X  B     SPLAT2X                                                          
         EJECT                                                                  
*=====================================================================          
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*=====================================================================          
TUD      DSECT                                                                  
         DS    0A                                                               
AMASTD   DS    A                   A(MASTER)                                    
ALOGOC   DS    A                   A(LOGOC)                                     
ALOGO    DS    A                   A(LOGO)                                      
AREMOT   DS    A                   A(REMOTE)                                    
APRNTBL  DS    A                   A(PRNTBL)                                    
APRNT    DS    A                   A(PRNT)                                      
MYSORTER DS    A                                                                
AJVTAB   DS    A                   A(JOB VALIDATION TABLE)                      
OIDSKDCB DS    A                   OIDISK DCB ADDRESS                           
*                                                                               
TUOPTS   DS    XL1                 OPTIONS                                      
TUNOTAPE EQU   X'80'               DON'T GENERATE TAPE                          
TUWCODE  EQU   X'40'               TAPE BY WORK CODE                            
TUERROR  EQU   X'20'               ERRORS ONLY                                  
TUGREY   EQU   X'10'               DO SPECIAL STUFF FOR GREY                    
TUNOPOW  EQU   X'08'               NO POWWOW CALL FOR FTP'ING                   
TUTRACE  EQU   X'04'               TRACE JCL                                    
TURESND  EQU   X'02'               RESEND                                       
*                                                                               
FILEOPT  DS    CL1                 FILE OPTION (NEW)                            
*                                                                               
INVITMCT DS    H                   INVOICE ITEM COUNTER                         
SAVPO    DS    CL10                SAVE PURCHASE ORDER                          
*                                                                               
ACTVSW   DS    CL1                 ACTIVITY INDICATOR                           
REREADSW DS    CL1                 REREAD SYSIOS KEY                            
TUPER    DS    CL17                PRINTABLE PERIOD                             
PGMSTAT  DS    CL1                 JOB TO SAP JOB STATUS                        
PRGSSAPE EQU   X'80'               ERROR ON JOB TO SAP CONVERSION               
PRGSTOPN EQU   X'40'               TAPE OPENED                                  
PRGSPOER EQU   X'20'               PURCHASE ORDER ERROR                         
*                                                                               
OVRVEND  DS    CL11                VENDOR OVERRIDE                              
SVAGYCO  DS    CL4                 SAVED AGENCY CO=                             
LENGTHS  DS    CL3                 AGENCY ESTIMATE SETUP LENGTHS                
SVAGY    DS    CL6                 SAVED AGENCY                                 
SVCPJ    DS    0CL12                                                            
SVCLI    DS    CL3                 SAVED CLIENT                                 
SVPRD    DS    CL3                 SAVED PRODUCT                                
SVJOB    DS    CL6                 SAVED PRODUCT                                
SVOFF    DS    CL1                 SAVED OFFICE                                 
AMOUNT   DS    PL4                                                              
COUNT    DS    PL4                 INVOICE COUNTER                              
ERRCNT   DS    PL4                 ERROR COUNTER                                
HDRCOUNT DS    PL4                 HEADER COUNT FOR WCODEOPTION                 
DTLCOUNT DS    PL4                 DETAIL COUNT FOR WCODE OPTION                
*                                  COUNTER ORDER SAME AS TAPE                   
NUMCNTRS EQU   15                  TOTAL NUMBER OF COUNTERS                     
NUMINV   EQU   6                   NUMBER OF INVOICE AMOUNTS                    
CNTRS    DS    0F                                                               
CNTINV   DS    F                   INVOICE TOTAL                                
CNTSINV  DS    0F                  SUB-INVOICE TOTAL                            
NUMSINV  EQU   6                   NUMBER OF SUB-INVOICE AMOUNTS                
CNTTUSGE DS    F                   TALENT USAGE                                 
CNTPNH   DS    F                   P AND H                                      
CNTHAND  DS    F                   HANDLING                                     
CNTTAX   DS    F                   PAYROLL TAX                                  
CNTMUS   DS    F                   MUSICIANS AMOUNT                             
CNTGST   DS    F                   GST                                          
*                                                                               
CNTTOT   DS    F                   TALENT + TAXES + HANDLING + P&H              
*                                                                               
NUMFEES  EQU   8                   NUMBER OF CHECK AMOUNTS                      
CNTRFEES DS    0F                  AMOUNT TOTALS                                
CNTSING  DS    F                   SINGERS AMOUNT                               
CNTON    DS    F                   ONCAMERA AMOUNT                              
CNTXTRA  DS    F                   EXTRAS AMOUNT                                
CNTOFF   DS    F                   OFF CAMERA (VOICE OVER) AMOUNT               
CNTMODEL DS    F                   HAND MODEL AMOUNT                            
CNTCART  DS    F                   CARTAGE AMOUNT                               
CNTWRD   DS    F                   WARDROBE AMOUNT                              
CNTMISC  DS    F                   MISCELLANEOUS AMOUNT                         
CNTRSX   EQU   *-CNTRS                                                          
*                                                                               
FEESTOT  DS    F                   CHECK AMOUNTS TOTAL / INVOICE                
TMPAMT   DS    F                                                                
TMPTIME  DS    0F                                                               
TMPTHRS  DS    X                                                                
TMPTMIN  DS    X                                                                
TMPTSEC  DS    X                                                                
         DS    X                                                                
*                                                                               
TOTCNTRS DS    0F                  TOTAL COUNTERS                               
TOTINV   DS    F                   INVOICE TOTAL                                
TOTTUSGE DS    F                   TALENT USAGE TOTAL                           
TOTTAX   DS    F                   PAYROLL TAX TOTAL                            
TOTPNH   DS    F                   P AND H TOTAL                                
TOTHAND  DS    F                   HANDLING TOTAL                               
TOTGST   DS    F                   GST TOTAL                                    
*                                                                               
TOTFEES  DS    F                   FEES TOTAL                                   
*                                                                               
CANRATE  DS    F                   CANADIAN CONV RATE                           
LASTAGY  DS    CL4                 LAST AGENCY                                  
AGYCNT   DS    PL4                 AGENCY INVOICE COUNTER                       
AGYDEBT  DS    F                   AGENCY DEBIT                                 
AGYCRDT  DS    F                   AGENCY CREDIT                                
AGYINV   DS    F                   AGENCY INVOICE TOTAL                         
*                                                                               
DLBLOCK  DS    CL(DLCBXLX)                                                      
MAXTBLS  EQU   40                                                               
AGYTAB   DS    (MAXTBLS)CL(AGYTABL) AGENCY TABLE                                
*                                                                               
SORTINIT DS    C                                                                
SORTIO   DS    0CL(RECLNQ+L'SORTKEY)                                            
SORTKEY  DS    CL44                SORT SPACE FOR AYLINTER                      
TAPEREC  DS    CL(RECLNQ)          TAPE RECORD AREA                             
*                                                                               
SUBSID   DS    CL1                 SUBSIDIARY INVOICE                           
JBVLCNT  DS    F                                                                
AGYFLST  DS    CL241               AGENCY FLIST                                 
SVKEY    DS    CL(L'KEY)                                                        
SVVENDR  DS    CL6                 SAVE VENDOR                                  
*                                                                               
SVTIAREC DS    XL(L'TIAREC)        SAVED TIAREC                                 
TULNQ    EQU   *-TUD                                                            
         EJECT                                                                  
*              DSECT TO SORT KEY                                                
*                                                                               
SRTKD    DSECT                                                                  
SRTKAGY  DS    CL4                 AGENCY                                       
SRTKCLI  DS    CL4                 CLIENT                                       
SRTKPO   DS    CL9                 PURCHASE ORDER                               
         DS    CL3                                                              
*                                                                               
SRTKINV  DS    F                                                                
SRTKTUSG DS    F                                                                
SRTKTAX  DS    F                                                                
SRTKPNH  DS    F                                                                
SRTKHAND DS    F                                                                
SRTKGST  DS    F                                                                
SRTKFEES DS    F                                                                
SRTKLNQ  EQU   *-SRTKD                                                          
         EJECT                                                                  
*              DSECT TO COVER AGENCY TABLE                                      
*                                                                               
AGYTABD  DSECT                                                                  
AGYTAGY  DS    CL(L'TIAGY)         AGENCY                                       
AGYTCO   DS    CL4                 COMPANY & OFFICE CODE                        
AGYTLEN  DS    CL3                 ESTIMATE SETUP LENGTHS                       
AGYTABL  EQU   *-AGYTABD                                                        
         EJECT                                                                  
*              DSECT TO COVER AGENCY/ACCOUNT TABLE                              
*                                                                               
AACTABD  DSECT                                                                  
AACTAGY  DS    CL(L'TIAGY)         AGENCY                                       
AACTACC  DS    CL12                ACCOUNT                                      
AACTABL  EQU   *-AACTABD                                                        
         EJECT                                                                  
*              DSECT TO COVER FILE RECORD                                       
*                                                                               
RECD     DSECT                                                                  
REC      DS    0CL239                                                           
RECCMPY  DS    CL4                 COMPANY CODE                                 
RECDOCT  DS    CL2                 'SA'                                         
RECDOCD  DS    CL8                 DOCUMENT DATE                                
RECREFN  DS    CL16                REFERENCE NUMBER                             
RECHDR   DS    CL25                'TALENT PARTNERS'                            
RECGLAC  DS    CL10                G/L ACCOUNT                                  
RECPSTK  DS    CL2                 POSTING KEY                                  
RECAMNT  DS    CL16                AMOUNT                                       
RECPURD  DS    CL10                PURCHASE DOCUMENT #                          
RECASSN  DS    0CL18               ASSIGNMENT                                   
RECASCLI DS    CL3                    CLIENT                                    
RECASPRD DS    CL3                    PRODUCT                                   
         DS    CL12                                                             
RECITEM  DS    CL50                   N/A, SPACES                               
RECCSTC  DS    CL10                   N/A, SPACES                               
RECPRFT  DS    CL10                PROFIT CENTER                                
RECMTRL  DS    CL18                MATERIAL                                     
RECCNTR  DS    CL10                CONTRACT NUMBER                              
RECINTO  DS    CL12                   N/A, SPACES                               
RECSTAX  DS    CL16                SALES TAX AMOUNT                             
RECSDIV  DS    CL2                    N/A, SPACES                               
*                                                                               
         ORG   REC                                                              
*                   TRAILER RECORD                                              
RECRCNT  DS    CL8                 RECORD COUNT, EXCLUDING THIS ONE             
RECDEBT  DS    CL16                DEBIT TOTAL                                  
RECCRDT  DS    CL16                CREDIT TOTAL                                 
         DS    CL199                                                            
RECLNQ   EQU   *-RECD                                                           
         EJECT                                                                  
*                   JOB VALIDATION RECORD                                       
JBVLD    DSECT                                                                  
JBVLCNTR DS    CL10                CONTRACT NUMBER                              
JBVLCPYC DS    CL4                 COMPANY CODE                                 
JBVLPRFT DS    CL4                 PROFIT CENTER                                
JBVLSLSD DS    CL2                 SALES DIVISION                               
*BVLSHIP DS    CL10                SHIP TO                                      
JBVLLNQ  EQU   *-JBVLD                                                          
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
PRNTD    DSECT                                                                  
         DS    CL1                                                              
PRTAGY   DS    CL6                 AGENCY                                       
         DS    CL1                                                              
PRTDATE  DS    CL6                 INVOICE DATE                                 
         DS    CL1                                                              
PRTINVN  DS    CL6                 INVOICE NUMBER                               
         DS    CL1                                                              
PRTPO    DS    CL9                 PURCHASE ORDER NUMBER (AUTH/PO)              
         DS    CL1                                                              
PRTCLI   DS    CL3                 CLIENT                                       
         DS    CL1                                                              
PRTPRD   DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
PRTJOB   DS    CL10                JOB CODE                                     
         DS    CL1                                                              
PRTTUSGE DS    CL12                TALENT USAGE AMOUNT                          
         DS    CL1                                                              
PRTPNH   DS    CL12                P & H CONTRIBUTIONS AMOUNT                   
         DS    CL1                                                              
PRTTAX   DS    CL12                PAYROLL TAXES AMOUNT                         
         DS    CL1                                                              
PRTHND   DS    CL12                HANDLING AMOUNT                              
         DS    CL1                                                              
PRTGST   DS    CL12                GOODS/SERVICES TAX AMOUNT                    
         DS    CL1                                                              
PRTINV   DS    CL12                INVOICE AMOUNT                               
         DS    CL1                                                              
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
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043TAREP46   10/04/16'                                      
         END                                                                    
