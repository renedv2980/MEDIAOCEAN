*          DATA SET TAREP30    AT LEVEL 077 AS OF 10/04/16                      
*PHASE T70330B,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70330 - PMUSIC REPORT'                                         
T70330   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70330,R6,RR=R2                                                
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
         USING TPMD,R7             R7=A(LOCAL W/S)                              
         ST    R2,RELO                                                          
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
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
PMDSK    EQU   80                  PMDISK/REPORT                                
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
*                                                                               
VKEY     NTR1                                                                   
*                                                                               
         LR    RE,R7               A(LOCAL W/S)                                 
         LA    RF,TPMLNQ                                                        
         XCEFL ,                   CLEAR LOCAL W/S                              
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
         ZAP   CNTER,=P'0'                                                      
*                                                                               
* VALIDATE AGENCY                                                               
*                                                                               
         GOTO1 RECVAL,DMCB,(X'40',TLAYCDQ),(X'08',SMUAGYH),SMUAGYNH             
         MVC   AGYNAM,TGNAME                                                    
         CLI   SMUAGY,C'@'                                                      
         BE    *+14                                                             
         MVC   TIFAGY,TGAGY        AGENCY CODE                                  
         B     VK5                                                              
*                                                                               
         MVC   TIFAGY,TGLST        SET SYSIO FOR FLIST                          
         NI    TIFAGY,X'7F'        TURN OFF X'80'                               
*                                                                               
* VALIDATE CLIENT                                                               
*                                                                               
VK5      XC    TGCLI,TGCLI                                                      
         XC    SMUCLIN,SMUCLIN     CLEAR CLIENT NAME                            
         OI    SMUCLINH+6,X'80'                                                 
         LA    R2,SMUCLIH          R2=A(CLIENT FIELD)                           
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    VK10                                                             
         GOTO1 ANY                 GET INPUT IN WORK PADDED WITH SPACES         
         CLI   SMUCLI,C'-'         TEST NEGATIVE FILTER                         
         BNE   VK9                                                              
         CLI   SMUCLI+1,C'@'       TEST FLIST                                   
         BNE   VK7                                                              
         MVI   TGLTYP,TLGLTYPF     SET FILTER FOR READ                          
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'88',WORK+2),SMUCLINH  GET NAME            
         BNE   THEEND                                                           
         MVC   TIFCLI,TGLST        SET FLIST FOR CLIENT FILTER                  
         NI    TIFCLI,X'FF'-X'C0'  TELL SYSIO IT'S NEGATIVE FLIST               
         B     VK10                                                             
         SPACE                                                                  
VK7      CLI   5(R2),7             MAX INPUT LENGTH IS 7                        
         BH    FLDINV                                                           
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'88',WORK+1),SMUCLINH  GET NAME            
         BNE   THEEND                                                           
         MVC   TIFCLI,TGCLI        SET CLIENT FILTER                            
         NI    TIFCLI,X'FF'-X'40'  LET SYSIO KNOW IT'S NEGATIVE                 
         B     VK10                                                             
         SPACE                                                                  
VK9      MVI   BYTE,6              MAX INPUT LENGTH IS 6                        
         CLI   SMUCLI,C'@'         IF FLIST                                     
         BNE   *+8                                                              
         MVI   BYTE,7              MAX INPUT LENGTH IS 7                        
         CLC   5(1,R2),BYTE                                                     
         BH    FLDINV                                                           
         GOTO1 RECVAL,DMCB,(X'40',TLCLCDQ),(X'08',SMUCLIH),SMUCLINH             
         MVC   TIFCLI,TGCLI                                                     
         CLI   SMUCLI,C'@'         IF FLIST                                     
         BNE   VK10                                                             
         MVC   TIFCLI,TGLST        SET FLIST FOR CLIENT FILTER                  
         NI    TIFCLI,X'FF'-X'80'  LET SYSIO KNOW IT'S FLIST                    
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
VK10     LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         LA    R2,SMUPDH           R2 = A(PERIOD FIELD)                         
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
         MVC   TPERIOD,PVALCPER    SAVE DISPLAYABLE PERIOD IN W/S               
         GOTO1 ADDAY,DMCB,PVALESTA,WORK,F'-60' READ INVOICES STARTING 2         
         GOTO1 DATCON,DMCB,WORK,(1,TIQPSTR)     MTHS PRIOR TO RQST STRT         
         MVC   TSTRT,PVALPSTA      SAVE REQUEST DATES FOR FILTERING             
         MVC   TEND,PVALPEND                                                    
         DROP  R3                                                               
*                                                                               
* VALIDATE NETWORK                                                              
*                                                                               
         XC    SMUNETN,SMUNETN     CLEAR NETWORK NAME                           
         OI    SMUNETNH+6,X'80'                                                 
         CLI   SMUNETH+5,0         ANY NETWORK FILTER?                          
         BE    VK30                NO                                           
         L     RF,=A(NETWORKS)     TABLE OF NETWORKS                            
         A     RF,RELO                                                          
VK20     CLI   0(RF),X'FF'                                                      
         BE    VK30                END OF LIST                                  
         CLC   SMUNET,0(RF)        MATCH?                                       
         BE    *+12                                                             
         LA    RF,L'NETWORKS(RF)                                                
         B     VK20                                                             
         MVC   SMUNETN,0(RF)       DISPLAY NETWORK NAME ON SCREEN               
*                                                                               
* VALIDATE MUSIC                                                                
*                                                                               
VK30     XC    TGMUS,TGMUS                                                      
         XC    SMUMUSN,SMUMUSN     CLEAR MUSIC COMPOSITION NAME                 
         OI    SMUMUSNH+6,X'80'                                                 
         LA    R2,SMUMUSH                                                       
         CLI   5(R2),0             ANY MUSIC FILTER?                            
         BNE   VK40                                                             
         CLI   ACTEQU,ACTDOWN                                                   
         BE    VK50                                                             
         CLI   WHEN,X'20'          IF REQUESTING SOON                           
         BE    MISSERR             MUST HAVE MUSIC INPUT                        
         B     VK50                                                             
*                                                                               
VK40     GOTO1 RECVAL,DMCB,(X'40',TLMUCDQ),(X'08',SMUMUSH),SMUMUSNH             
         CLI   SMUMUS,C'@'         IF NOT FLIST INPUT                           
         BE    *+14                                                             
         MVC   TIFMUSIC,TGMUS      SET MUSIC FILTER FOR SYSIO                   
         B     VK50                                                             
         MVC   TIFMUSIC,TGLST      ELSE SET FLIST NAME                          
         NI    TIFMUSIC,X'7F'      AND TURN OFF X'80' FOR SYSIO=FLIST           
*                                                                               
* VALIDATE COMM ID                                                              
*                                                                               
VK50     XC    TGCID,TGCID                                                      
         XC    TIQSTART,TIQSTART                                                
         XC    SMUCIDN,SMUCIDN     CLEAR COMM ID NAME                           
         OI    SMUCIDNH+6,X'80'                                                 
         CLI   SMUCIDH+5,0         ANY COMM ID FILTER?                          
         BE    VK60                                                             
*                                                                               
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',SMUCIDH),SMUCIDNH                    
         MVC   TIFCID,TGCID                                                     
         OC    TIFMUSIC,TIFMUSIC   IF HAVE MUSIC FILTER                         
         BZ    *+10                                                             
         MVC   TIQSTART,TGCID      SET CID AS START                             
*                                                                               
VK60     LA    R2,SMUOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         BAS   RE,VALOPT                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
*                                                                               
VALOPT   NTR1                                                                   
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
*                                                                               
VOPT10   CLC   =C'TRACE',SCDATA1   TRACE                                        
         BNE   VOPT20                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    TPMOPTS,TTRACE      SET TRACE ON                                 
         B     VOPTNEXT                                                         
*                                                                               
VOPT20   CLC   =C'BOX',SCDATA1     BOX                                          
         BNE   VOPT30                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         OI    TPMOPTS,TNOBOX      TURN BOXES OFF                               
         B     VOPTNEXT                                                         
*                                                                               
VOPT30   CLC   =C'NONWK',SCDATA1   NO NETWORK OPTION                            
         BNE   VOPT40                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   *+12                                                             
         OI    TPMOPTS,TOKNNWK     NO NETWORK IS OK                             
         B     VOPTNEXT                                                         
         CLI   SCDATA2,C'O'                                                     
         BNE   FLDINV                                                           
         OI    TPMOPTS,TOKNNWK+TNONWK  NO NETWORK ONLY                          
         B     VOPTNEXT                                                         
*                                                                               
VOPT40   CLC   =C'COUNT',SCDATA1   REQUESTING A COUNT OF USES                   
         BNE   VOPT50                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    TPMOPTS,TCOUNT      SET BIT                                      
         B     VOPTNEXT                                                         
*                                                                               
VOPT50   CLC   =C'ASCAP',SCDATA1   REQUESTING FILTER BY ASCAP                   
         BNE   VOPT60                                                           
         OI    TPMOPTS,TASCAP      SET BIT                                      
         B     VOPTNEXT                                                         
*                                                                               
VOPT60   CLC   =C'BMI',SCDATA1     REQUESTING FILTER BY BMI                     
         BNE   VOPT70                                                           
         OI    TPMOPTS,TBMI        SET BIT                                      
         B     VOPTNEXT                                                         
*                                                                               
VOPT70   CLC   =C'SESAC',SCDATA1   REQUESTING FILTER BY SESAC                   
         BNE   FLDINV                                                           
         OI    TPMOPTS,TSESAC      SET BIT                                      
*                                                                               
VOPTNEXT LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10           AND CONTINUE                                 
VOPTX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
*                                                                               
PREP     NTR1                                                                   
         L     R1,=A(SAVERC)                                                    
         ST    RC,0(R1)            SAVE RC FOR HEADHOOK                         
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    *+16                                                             
         MVC   SPECS,=A(MYSPECS)   SET A(SPECS) FOR PRINTING                    
         MVC   HEADHOOK,=A(HDHOOK) SET A(HEADLINE HOOK)                         
         MVC   MYTRACE,=A(MYTRC)   SET A(TRACE ROUTINE)                         
*                                                                               
         CLI   RECNUM,PMDSK        IF PMDISK/REPORT                             
         BNE   *+8                                                              
         OI    TPMOPTS2,TTAPE      GENERATE A TAPE                              
*                                                                               
         LA    R1,SORTKYLQ         SET L'SORT KEY IN SORTCARD                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SORTLNQ          SET L'SORT RECORD IN RECCARD                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB+6(2)                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD  INITIALIZE                         
*                                                                               
         LA    R2,USETAB                                                        
PREP10   CLI   0(R2),X'FF'                                                      
         BE    PREP20                                                           
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIREAD,TLCOMCDQ     READ COMMERCIAL RECORDS                      
         MVI   TISUBRD,TLINHCDQ    INVOICE PAYMENT HISTORY                      
         MVI   TIQDTYPE,TIQDBILL   SET FILTERING ON BILL DATE                   
         MVC   TIFUSE,0(R2)        USE                                          
         MVI   TIFINSTN,TAINSCIN+TAINSCAN  IGNORE CANCELLED INVOICES            
         MVI   TIFPDPN,TAPDPCRD    IGNORE CREDIT PAYMENTS                       
         OI    TIQFLAGS,TIQFPDUM   SET OK TO PASS DUMMY PAYMENTS                
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTXFF  WRITE FINAL RECORD TO SORT          
*                                                                               
         LA    R2,L'USETAB(R2)                                                  
         B     PREP10                                                           
*                                                                               
PREP20   ZAP   USECNT,=P'0'        CLEAR NETWORK USE COUNTER                    
         BAS   RE,OPENTAPE         OPEN TAPE                                    
         BAS   RE,GETSORT          GET SORT RECORDS AND PRINT REPORT            
         BAS   RE,CLOSTAPE         CLOSE TAPE                                   
*                                                                               
         CLI   ACTEQU,ACTDOWN      IF DOWNLOADING, DO SECOND REPORT             
         BNE   *+8                                                              
         BAS   RE,PREPD                                                         
*                                                                               
         GOTO1 SORTER,DMCB,=C'END' CLOSE SORT                                   
         BAS   RE,COUNT                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET FROM DATASET AND PRINT SECOND REPORT              
*                                                                               
PREPD    NTR1                                                                   
         BAS   RE,NEWPRTQ           SET NEW PRINT QUEUE REPORT                  
         GOTO1 REQTWA,DMCB,(3,ATWA),,VPRINT,(C'B',ABOX)                         
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
         MVI   DMCB,0                                                           
         BAS   RE,PRTHEAD          PRINT HEADLINES FOR SPREADSHEET              
*                                                                               
PREPD2   GET   (R2),TAPEREC        GET REC FROM TEMP DATASET                    
         BAS   RE,MOVEDWN                                                       
         B     PREPD2                                                           
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
*                                                                               
USETAB   DS    0CL3                                                             
         DC    C'CLA'                                                           
         DC    C'LNA'                                                           
         DC    C'LNC'                                                           
         DC    C'LNF'                                                           
         DC    C'LNN'                                                           
         DC    C'PAX'                                                           
         DC    X'FF'                                                            
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*                                                                               
NEWPRTQ  NTR1                                                                   
         XC    SPECS,SPECS         CLEAR SPECS                                  
         XC    HEADHOOK,HEADHOOK   AND HEADLINE HOOK                            
         TM    TPMOPTS,TNOBOX                                                   
         BO    NEWP10                                                           
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
NEWP10   L     R2,ALOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 ALOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BO    NEWP20                                                           
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
*                                                                               
NEWP20   L     R2,ABOX                                                          
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
         BZ    NEWP30                                                           
         XC    MCREMPQK,MCREMPQK                                                
         B     NEWP40                                                           
*                                                                               
NEWP30   MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'TID'                                                 
NEWP40   MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(6),=C'PMDATA'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
         B     XIT                                                              
         DROP  R2                                                               
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
         B     XIT                                                              
         EJECT                                                                  
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
         SPACE 1                                                                
SPLATDWN NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              PREVENT PAGE BREAK                           
         B     XIT                                                              
         EJECT                                                                  
*              MOVE FIELDS FOR DOWNLOAD                                         
*                                                                               
MOVEDWN  NTR1                                                                   
         LA    R5,TAPEREC                                                       
         LA    R2,DOWNTAB          FIELD LENGTHS                                
MOVED10  CLI   0(R2),X'FF'                                                      
         BE    MOVEDWNX                                                         
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVC   DLCBTYP,0(R2)       DATA TYPE IS TEXT                            
         MVC   DLCBLEN,1(R2)                                                    
         SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         LR    R4,RF                                                            
         CLI   0(R2),C'D'          DATE TYPE                                    
         BE    MOVED20                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R5)                                                 
         B     MOVED30                                                          
*                                                                               
MOVED20  MVI   DLCBTYP,C'T'                                                     
         MVC   DLCBFLD(2),0(R5)                                                 
         MVI   DLCBFLD+2,C'/'                                                   
         MVC   DLCBFLD+3(2),2(R5)                                               
         MVI   DLCBFLD+5,C'/'                                                   
         MVC   DLCBFLD+6(2),4(R5)                                               
*                                                                               
MOVED30  GOTO1 =V(DLFLD),DLCBD,RR=RELO     SEND TO DLFLD                        
         LA    R2,2(R2)                                                         
         AR    R5,R4                                                            
         B     MOVED10                                                          
*                                                                               
MOVEDWNX MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD,RR=RELO                                          
         B     XIT                                                              
         EJECT                                                                  
*              PRINT HEADER LINE FOR SPREADSHEET                                
         SPACE 1                                                                
PRTHEAD  NTR1                                                                   
*                                                                               
         LA    R2,HDLINE1          ESTIMATE INFO                                
PHEAD1   CLI   0(R2),X'FF'                                                      
         BE    PHEADX                                                           
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVC   DLCBLEN,0(R2)                                                    
         SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),1(R2)                                                 
         GOTO1 =V(DLFLD),DLCBD,RR=RELO     SEND TO DLFLD                        
         SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         LA    R2,1(RF,R2)                                                      
         B     PHEAD1                                                           
*                                                                               
PHEADX   MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD,RR=RELO                                          
PHEADZ   B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*------------------------------------*                                          
* HDHOOK -  HEADLINE HOOK (HEADHOOK) *                                          
*------------------------------------*                                          
*              PROCESS RECORDS FROM SYSIO AND WRITE TO SORT                     
*                                                                               
IOHOOK   NTR1                                                                   
*                                                                               
         MVC   AIO,TIAREC                                                       
         CLI   TIMODE,PROCCOMM     COMMERCIAL?                                  
         BNE   IOHOOK10                                                         
*                                                                               
         GOTO1 MYTRACE,DMCB,=C'PROCCOMM',AIO,0                                  
         MVC   AGYCD,TIAGY         SAVE AGENCY CODE                             
         MVC   MUSIC,TIMUSIC       SAVE MUSIC CODE                              
         MVC   CNAM,TINAME         SAVE COMMERCIAL TITLE                        
         B     IOHOOKX                                                          
*                                                                               
IOHOOK10 CLI   TIMODE,PROCREC                                                   
         BNE   IOHOOKX                                                          
         GOTO1 MYTRACE,DMCB,=C'PROCREC',AIO,0                                   
         L     RF,TIAREC                                                        
         CLI   0(RF),TLINCDQ       INVOICE RECORD?                              
         BNE   IOHOOKX                                                          
         BAS   RE,XTRACT           EXTRACT INVOICE DATA AND PUT TO SORT         
*                                                                               
IOHOOKX  MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              EXTRACT DATA FROM CLASS A ELEMENTS AND BUILD SORT RECS           
*                                                                               
XTRACT   NTR1                                                                   
         LA    R2,SRTREC           R2=A(SORT RECORD)                            
         USING SORTD,R2                                                         
*                                                                               
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ      PAYMENT DETAILS ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R3,TAPDSTUS         PICK UP STARTING USE NUMBER                  
         MVC   SORTUSE,TAPDUSE                                                  
         DROP  R4                                                               
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TANPELQ      GET NETWORK/CLASS A ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   EXTX                NO SORT RECORDS FROM HERE                    
*                                                                               
         USING TANPD,R4            R4=A(ELEMENT)                                
EXT10    TM    TPMOPTS,TOKNNWK     NO NETWORK IS OK                             
         BO    *+12                                                             
         CLI   TANPNWK,0           ANY NETWORK?                                 
         BE    EXT20                                                            
         TM    TPMOPTS,TNONWK      NO NETWORK ONLY - FILTER S/B EMPTY           
         BO    *+12                                                             
         CLI   SMUNETH+5,0         YES -- ANY NETWORK FILTER?                   
         BE    *+14                                                             
         CLC   SMUNET,TANPNWK      YES -- MATCH ON NETWORK FILTER?              
         BNE   EXT20               NO                                           
*                                                                               
         CLC   TANPDATE,TSTRT      DATE MUST BE WITHIN REQUESTED RANGE          
         BL    EXT20                                                            
         CLC   TANPDATE,TEND                                                    
         BH    EXT20                                                            
*                                                                               
         MVC   SORTAGY,AGYCD       AGENCY CODE                                  
         MVC   SORTMUS,MUSIC       MUSIC CODE                                   
         MVC   SORTNET,TANPNWK     NETWORK CODE                                 
*                                                                               
         CLI   SORTNET,C'X'        IF NETWORK CODE IS PAX                       
         BNE   *+8                                                              
         MVI   SORTNET,C'P'        CHANGE TO P                                  
*                                                                               
         MVC   SORTCID,TICID       COMMERICAL ID                                
         MVC   SORTDATE,TANPDATE   USE DATE                                     
         STCM  R3,3,SORTUSEN       USE NUMBER                                   
         MVC   SORTPNME,TANPPNME   PROGRAM NAME                                 
         MVC   SORTCNAM,CNAM       COMMERCIAL TITLE                             
         AP    CNTER,=P'1'                                                      
         DROP  R4                                                               
*                                                                               
EXT15    GOTO1 SORTER,DMCB,=C'PUT',(R2)     WRITE OUT SORT RECORD               
*                                                                               
EXT20    LA    R3,1(R3)            INCREMENT USE NUMBER                         
         BAS   RE,NEXTEL                                                        
         BE    EXT10               BUILD NEXT SORT RECORD                       
*                                                                               
EXTX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              GET RECORDS FROM SORT                                            
*                                                                               
GETSORT  NTR1                                                                   
         LA    R3,P                R3=A(PRINT LINE)                             
         USING PRNTD,R3                                                         
         LA    R4,BC1              R4 = A(FIRST DATE COLUMN IN P)               
         USING BC1,R4                                                           
         ST    R4,FULL                                                          
*                                                                               
         LA    R2,SRTREC           R2=A(SORT RECORD)                            
         USING SORTD,R2                                                         
         LA    R5,OLDREC           PREVIOUS SORT RECORD                         
*                                                                               
         GOTO1 SORTER,DMCB,=C'GET' GET A SORT RECORD                            
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                 MUST BE AT LEAST ONE SORT RECORD             
         DC    H'0'                                                             
         MVC   OLDREC,0(RF)        MOVE SORT RECORD TO LOCAL AREA               
         CLC   OLDREC,SORTXFF      ONLY RECORD IS E-O-F MARKER?                 
         BE    XIT                 YES -- NOTHING TO DO                         
         GOTO1 MYTRACE,DMCB,=C'SORTOUT',OLDREC,L'OLDREC                         
         MVC   TGAGY,SORTAGY-SORTD(R5)                                          
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A8',0),0                                  
         GOTO1 MYTRACE,DMCB,=C'AGY',AIO,0                                       
         MVC   SAGYNAM,TGNAME                                                   
         MVC   TGMUS,SORTMUS-SORTD(R5)                                          
         BAS   RE,RDTLMU           READ MUSIC RECORD INTO AIO2                  
*                                                                               
GETS10   GOTO1 SORTER,DMCB,=C'GET' GET A SORT RECORD                            
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   SRTREC,0(RF)        MOVE SORT RECORD TO LOCAL AREA               
         CLC   OLDREC(SORTKYLQ),SRTREC                                          
         BE    GETS10              IGNORE DUPLICATE SORT KEYS                   
*                                                                               
         NI    TPMSTAT,X'FF'-TNOPRNT                                            
         TM    TPMOPTS,TASCAP+TBMI+TSESAC                                       
         BZ    GETS15                                                           
         BAS   RE,FILTLIC          FILTER ON LICENSER                           
         BE    GETS15                                                           
         XC    OLDREC,OLDREC                                                    
         OI    TPMSTAT,TNOPRNT                                                  
         B     GETS35                                                           
*                                                                               
GETS15   GOTO1 MYTRACE,DMCB,=C'SORTOUT',SRTREC,L'SRTREC                         
*                                                                               
         TM    TPMSTAT,TNOT1ST     HAVE WE ALREADY PRINTED THIS CMRCL?          
         BO    GETS20              YES                                          
         BAS   RE,HORIZ            DRAW HORIZONTAL LINE HERE                    
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SORTCID),SORTCID-SORTD(R5)                                
         MVC   WORK+L'SORTCID+2(L'SORTCNAM),SORTCNAM-SORTD(R5)                  
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   PRNTCOMM,WORK                                                    
*                                                                               
GETS20   MVC   DUB(L'SORTDATE),SORTDATE-SORTD(R5)  USE DATE                     
         CLC   DUB(L'SORTDATE),=C'TBA'                                          
         BNE   *+14                                                             
         MVC   PRNTDATE,=C'TBA  '                                               
         B     GETS30                                                           
         GOTO1 DATCON,DMCB,(1,DUB),(7,PRNTDATE)                                 
*                                                                               
GETS30   MVC   PRNTPNME,SORTPNME-SORTD(R5)         PROGRAM NAME                 
         SR    R0,R0                                                            
         ICM   R0,3,SORTUSEN-SORTD(R5)                                          
         EDIT  (R0),(4,PRNTUSEN)                   USE NUMBER                   
         AP    USECNT,=P'1'        ADD TO NETWORK USE COUNTER                   
         LA    R4,PRNTDLQ(R4)      BUMP TO NEXT COLUMN IN PRINT LINE            
         BAS   RE,SETPREC          SET TAPE RECORD/PUT IT                       
*                                                                               
         CLC   SORTD(SORTLNQ),SORTXFF IF LAST RECORD                            
         BNE   GETS35                                                           
         BAS   RE,PRNTIT           PRINT THE REMAINING DATES                    
         BAS   RE,PRTNUSE          PRINT USE COUNT BY NETWORK                   
         B     XIT                                                              
*                                                                               
GETS35   CLC   SORTAGY,SORTAGY-SORTD(R5)                                        
         BE    GETS38                                                           
         BAS   RE,PRNTIT           PRINT THE REMAINING DATES                    
         BAS   RE,PRTNUSE          PRINT USE COUNT BY NETWORK                   
         MVC   TGAGY,SORTAGY       READ NEW MUSIC RECORD                        
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A8',0),0                                  
         GOTO1 MYTRACE,DMCB,=C'AGY',AIO,0                                       
         MVC   SAGYNAM,TGNAME                                                   
         MVC   TGMUS,SORTMUS                                                    
         BAS   RE,RDTLMU                                                        
         MVI   FORCEHED,C'Y'       EJECT PAGE ON AGENCY CHANGE                  
         B     GETS55                                                           
*                                                                               
GETS38   CLC   SORTMUS,SORTMUS-SORTD(R5)                                        
         BE    GETS40              NO CHANGE OF MUSIC                           
         BAS   RE,PRNTIT           PRINT THE REMAINING DATES                    
         CLC   SORTNET,SORTNET-SORTD(R5) AND IF CHANGE IN NETWORK               
         BE    *+8                                                              
         BAS   RE,PRTNUSE          PRINT USE COUNT AS WELL                      
         MVI   FORCEHED,C'Y'       EJECT PAGE ON MUSIC CHANGE                   
         MVC   TGAGY,SORTAGY       READ NEW MUSIC RECORD                        
         MVC   TGMUS,SORTMUS                                                    
         BAS   RE,RDTLMU                                                        
         B     GETS55                                                           
*                                                                               
GETS40   CLC   SORTNET,SORTNET-SORTD(R5)                                        
         BE    GETS50              NO CHANGE OF NETWORK                         
         BAS   RE,PRNTIT           PRINT THE REMAINING DATES                    
         BAS   RE,PRTNUSE          PRINT USE COUNT BY NETWORK                   
         MVI   FORCEHED,C'Y'       EJECT PAGE ON NETWORK CHANGE                 
         B     GETS55                                                           
*                                                                               
GETS50   CLC   SORTCID,SORTCID-SORTD(R5)                                        
         BE    GETS60              NO CHANGE OF COMMERCIAL ID                   
         BAS   RE,PRNTIT           PRINT THE REMAINING DATES                    
*                                                                               
GETS55   L     R4,FULL             R4 = A(FIRST DATE COLUMN IN P)               
         NI    TPMSTAT,X'FF'-TNOT1ST  PRINT NEW COMMERCIAL DETAILS              
*                                                                               
GETS60   MVC   OLDREC,SRTREC       THIS IS THE NEW SORT RECORD                  
         CLC   OLDREC,SORTXFF      LAST RECORD WAS E-O-F MARKER?                
         BE    XIT                 THEN EXIT                                    
*                                                                               
         LA    RF,P2               A(NEXT PRINT LINE)                           
         CR    R4,RF               HAVE WE HIT THE END OF THE LINE?             
         BL    GETS10                                                           
*                                                                               
         BAS   RE,PRNTIT           YES -- PRINT WHAT WE HAVE SO FAR             
         L     R4,FULL             R4 = A(FIRST DATE COLUMN IN P)               
         B     GETS10                                                           
         EJECT                                                                  
*              READ MUSIC RECORD INTO AIO2                                      
*                                                                               
RDTLMU   NTR1                                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLMUCDQ,(X'20',0)                                    
         GOTO1 MYTRACE,DMCB,=C'MUSIC',AIO,0                                     
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE FILTERS ASCAP/BMI ON PUBLISHER 1                         
*                                                                               
FILTLIC  NTR1                                                                   
         MVC   AIO,AIO2            SET IOAREA TO MUSIC RECORD                   
         MVI   ELCODE,TAMUELQ      LOOK FOR PUBLISHER 1 ELEMENT                 
         GOTO1 GETL,DMCB,(2,=X'D701')                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            RESET IOAREA                                 
         L     R4,TGELEM                                                        
         USING TAMUD,R4                                                         
         TM    TPMOPTS,TASCAP      IF REQUESTING ASCAP ONLY                     
         BZ    FILTLIC5                                                         
         CLI   TAMULIC,ASCAP       JUST REPORT ON ASCAP                         
         BE    YES                                                              
         B     NO                                                               
*                                                                               
FILTLIC5 TM    TPMOPTS,TBMI        IF REQUESTING BMI ONLY                       
         BZ    FILTLIC7                                                         
         CLI   TAMULIC,BMI         ELSE, JUST REPORT ON BMI                     
         BE    YES                                                              
         B     NO                                                               
*                                                                               
FILTLIC7 CLI   TAMULIC,SESAC       ELSE, JUST REPORT ON SESAC                   
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO DRAW A HORIZONTAL LINE                                
*                                                                               
HORIZ    NTR1                                                                   
         L     R1,ABOX             INSERT A HORIZONTAL LINE HERE                
         USING BOXD,R1                                                          
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         BAS   RE,PRNTIT                                                        
         OI    TPMSTAT,TNOT1ST                                                  
         B     XIT                                                              
         DROP  R1                                                               
         SPACE 2                                                                
*              ROUTINE TO PRINT USE COUNT BY NETWORK                            
*                                                                               
PRTNUSE  NTR1                                                                   
         TM    TPMOPTS,TCOUNT      DID WE WANT A COUNT                          
         BNO   PRTNUSEX                                                         
         CP    USECNT,=P'1'        IF USE COUNT BY NETWORK                      
         BNH   PRTNUSEX                                                         
         BAS   RE,PRNTIT           SKIP A LINE                                  
         LA    RE,PRNTCOMM                                                      
         EDIT  USECNT,(9,0(RE)),COMMAS=YES,ALIGN=LEFT                           
         AR    RE,R0                                                            
         MVC   1(16,RE),=CL16'Uses for Network'                                 
         MVC   18(L'SVPNET,RE),SVPNET                                           
         BAS   RE,PRNTIT           PRINT THE REMAINING DATES                    
*                                                                               
PRTNUSEX ZAP   USECNT,=P'0'        CLEAR FOR NEXT NETWORK                       
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*              ROUTINE TO SET TAPE RECORD                                       
*                                                                               
SETPREC  NTR1                                                                   
         TM    TPMOPTS2,TTAPE      IF GENERATING A TAPE                         
         BZ    XIT                                                              
         LA    R3,TAPEREC          R3=A(TAPE RECORD)                            
         USING TRECD,R3                                                         
*                                                                               
         MVI   TAPEREC,C' '                                                     
         LA    R0,TAPEREC          PRE-CLEAR TAPE RECORD                        
         LA    R1,TRECLNQ                                                       
         BCTR  R1,0                                                             
         LA    RE,TAPEREC+1                                                     
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
**NOP    MVC   TAPEREC(1),SPACES   PRE-CLEAR TAPE RECORD                        
**NOP    MVC   TAPEREC+1(TRECLNQ-1),TAPEREC                                     
*                                                                               
         MVC   TRECAGY,TGAGY       AGENCY CODE                                  
         MVC   TRECAGYN,SAGYNAM    AGENCY NAME                                  
*                                                                               
         MVC   AIO,AIO2            SET IO TO MUSIC RECORD                       
         MVI   ELCODE,TAMUELQ                                                   
         GOTO1 GETL,DMCB,(2,=X'D701')      P1                                   
         BNE   *+12                                                             
         LA    R1,TRECPUB1                 PUBLISHER 1 NAME                     
         BAS   RE,SETNAME                                                       
*                                                                               
         GOTO1 GETL,DMCB,(2,=X'D702')                                           
         BNE   *+12                                                             
         LA    R1,TRECPUB2                 PUBLISHER 2 NAME                     
         BAS   RE,SETNAME                                                       
*                                                                               
         GOTO1 GETL,DMCB,(2,=X'D703')                                           
         BNE   *+12                                                             
         LA    R1,TRECPUB3                 PUBLISHER 3 NAME                     
         BAS   RE,SETNAME                                                       
*                                                                               
         GOTO1 GETL,DMCB,(2,=X'C301')                                           
         BNE   *+12                                                             
         LA    R1,TRECOMP1                 COMPOSER 1 NAME                      
         BAS   RE,SETNAME                                                       
*                                                                               
         GOTO1 GETL,DMCB,(2,=X'C302')                                           
         BNE   *+12                                                             
         LA    R1,TRECOMP2                 COMPOSER 2 NAME                      
         BAS   RE,SETNAME                                                       
*                                                                               
         GOTO1 GETL,DMCB,(2,=X'C303')                                           
         BNE   *+12                                                             
         LA    R1,TRECOMP3                 COMPOSER 3 NAME                      
         BAS   RE,SETNAME                                                       
*                                                                               
         GOTO1 GETL,DMCB,(2,=X'C304')                                           
         BNE   *+12                                                             
         LA    R1,TRECOMP4                 COMPOSER 4 NAME                      
         BAS   RE,SETNAME                                                       
*                                                                               
         GOTO1 GETL,DMCB,(2,=X'C101')                                           
         BNE   *+12                                                             
         LA    R1,TRECAUT1                 AUTHOR 1 NAME                        
         BAS   RE,SETNAME                                                       
*                                                                               
         GOTO1 GETL,DMCB,(2,=X'C102')                                           
         BNE   *+12                                                             
         LA    R1,TRECAUT2                 AUTHOR 2 NAME                        
         BAS   RE,SETNAME                                                       
*                                                                               
         GOTO1 CHAROUT,DMCB,TANAELQ,0,0                                         
         MVC   TRECSONG,TGNAME             COMPOSITION NAME (SONG)              
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTCLI                                  
         MVC   TRECCLIN,TGNAME             CLIENT NAME                          
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTPRD                                  
         MVC   TRECPRDN,TGNAME             PRODUCT NAME                         
*                                                                               
         MVC   TRECCID,SORTCID-SORTD(R5)   COMMERCIAL ID                        
         MVC   TRECPROG(L'SORTPNME),SORTPNME-SORTD(R5)                          
         OC    TRECPROG,SPACES             SHOW NAME                            
*                                                                               
         MVC   TRECNWK(1),SORTNET-SORTD(R5)                                     
         L     RF,=A(NETWORKS)     IF IN NETWORK TABLE                          
SETPREC5 CLI   0(RF),X'FF'                                                      
         BE    SETPREC9                                                         
         CLC   TRECNWK(1),0(RF)                                                 
         BE    *+12                                                             
         LA    RF,L'NETWORKS(RF)                                                
         B     SETPREC5                                                         
         MVC   TRECNWK,0(RF)       SHOW EXPANDED NETWORK CALL LETTERS           
*                                                                               
SETPREC9 MVC   DUB(L'SORTDATE),SORTDATE-SORTD(R5)  USE DATE                     
         CLC   =C'TBA',DUB                                                      
         BNE   *+14                                                             
         MVC   TRECDATE,=6C'0'                                                  
         B     SETPRECX                                                         
         GOTO1 DATCON,DMCB,(1,DUB),(X'20',WORK)                                 
         MVC   TRECDATE(4),WORK+2                  SET MMDDYY                   
         MVC   TRECDATE+4(2),WORK                                               
*                                                                               
SETPRECX BAS   RE,PUTTAPE                                                       
         MVC   AIO,AIO1            RESET IOAREA                                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET NAME                                              
*                                  NTRY R1=TAPE FIELD,TGELEM=A(ELEMENT)         
SETNAME  NTR1                                                                   
         L     R4,TGELEM                                                        
         USING TAMUD,R4                                                         
         ZIC   RE,TAMULEN                                                       
         SH    RE,=Y(TAMULNQ)                                                   
         LA    RF,L'TRECAGYN                                                    
         CR    RE,RF               IF NAME LONGER THEN TAPE NAME LENGTH         
         BNH   *+6                                                              
         LR    RE,RF               USE LENGTH OF TAPE NAME                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),TAMUNAME                                                 
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              PRINT A LINE                                                     
*                                                                               
PRNTIT   NTR1                                                                   
         TM    TPMSTAT,TNOPRNT     IF PRINTING                                  
         BO    PRNTX                                                            
*                                                                               
*RNT2    CLI   ACTEQU,ACTDOWN                                                   
*        BE    XIT                                                              
PRNT2    GOTO1 SPOOL,DMCB,(R8)     PRINT IT                                     
*                                                                               
         TM    TPMSTAT,THOOKED     DID WE JUST DO HEADLINES?                    
         BZ    PRNTX                                                            
         NI    TPMSTAT,X'FF'-THOOKED  YES -- TURN OFF BIT                       
*                                                                               
         L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES      SET COLUMNS                                  
         LA    RF,BOXCOLS                                                       
         USING PRNTD,RF            USE PRINT LINE DSECT                         
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC7,C'C'                                                         
         MVI   BC8,C'C'                                                         
         MVI   BC9,C'C'                                                         
         MVI   BC10,C'C'                                                        
         MVI   BC11,C'C'                                                        
         MVI   BR,C'R'                                                          
         DROP  RF                                                               
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         MVI   2(RF),C'M'                                                       
         MVI   BOXROWS+60,C'B'                                                  
         DROP  R4                                                               
*                                                                               
         MVC   P2(123),=C' Commercial ID and Name                      +        
                   Date   Use Program          Date   Use Program      +        
                   Date   Use Program'                                          
*                                                                               
         TM    TPMSTAT,TNOT1ST     IF WE ALREADY PRINTED THIS CMRCL             
         BZ    *+8                                                              
         MVI   SPACING,2           ENSURE WE PRINT MIDLINE BEFORE DTL           
*                                                                               
         BAS   RE,PRNTIT           PRINT COLUMN HEADINGS                        
*                                                                               
         MVC   P,SAVEP             RESTORE DETAIL LINE                          
         NI    SPOOLIND,X'FF'-SPNSPACE  TURN OFF LINE SKIP SUPPRESS             
         B     PRNT2               PRINT IT                                     
*                                                                               
PRNTX    B     XIT                                                              
         EJECT                                                                  
*        PRINT OUT PAGE WITH COUNTER                                            
*                                                                               
COUNT    NTR1                                                                   
         TM    TPMOPTS,TCOUNT      DID WE WANT A COUNT                          
         BNO   COUNTX                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   BYTE,C'E'           MARK END PAGE FOR HEADHOOK                   
         BAS   RE,CPRNT                                                         
         BAS   RE,CPRNT                                                         
         MVC   P,SPACES                                                         
         MVC   P+1(33),LTTOTAL                                                  
         EDIT  CNTER,(9,P+34),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK                
         BAS   RE,CPRNT                                                         
COUNTX   B     XIT                                                              
         SPACE 2                                                                
*        PRINT THE LINE                                                         
*                                                                               
CPRNT    NTR1                                                                   
         TM    TPMSTAT,TNOPRNT     IF PRINTING                                  
         BO    XIT                                                              
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)     PRINT IT                                     
         B     XIT                                                              
         EJECT                                                                  
*        OPEN THE TAPE                                                          
*                                                                               
OPENTAPE NTR1                                                                   
         TM    TPMOPTS2,TTAPE      IF GENERATING A TAPE                         
         BZ    XIT                                                              
         LA    R2,PMDISK           OPEN IT                                      
         CLI   ACTEQU,ACTDOWN      IF DOWNLOAD, USE TADOWN                      
         BNE   *+8                                                              
         LA    R2,TADOWN                                                        
         OPEN  ((2),OUTPUT)                                                     
         B     XIT                                                              
         SPACE 2                                                                
*        PUT TO THE TAPE                                                        
*                                                                               
PUTTAPE  NTR1                                                                   
         TM    TPMOPTS2,TTAPE      IF GENERATING A TAPE                         
         BZ    XIT                                                              
         LA    R2,PMDISK           PUT TO IT                                    
         CLI   ACTEQU,ACTDOWN      IF DOWNLOAD, USE TADOWN                      
         BNE   *+8                                                              
         LA    R2,TADOWN                                                        
         PUT   (R2),TAPEREC                                                     
         GOTO1 MYTRACE,DMCB,=C'PUTTAPE',TAPEREC,L'TAPEREC                       
         B     XIT                                                              
         SPACE 2                                                                
*        CLOSE THE TAPE                                                         
*                                                                               
CLOSTAPE NTR1                                                                   
         TM    TPMOPTS2,TTAPE      IF GENERATING A TAPE                         
         BZ    XIT                                                              
         LA    R2,PMDISK           CLOSE IT                                     
         CLI   ACTEQU,ACTDOWN      IF DOWNLOAD, USE TADOWN                      
         BNE   *+8                                                              
         LA    R2,TADOWN                                                        
         CLOSE ((2))                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS, CONSTANTS, ETC.                                   
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
THEEND   GOTO1 ERREX                                                            
         SPACE 2                                                                
MISSERR  MVI   ERROR,MISSING       MISSING                                      
         GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
RELO     DS    F                                                                
LTTOTAL  DC    C'Total number of uses in report = '                             
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,XXX,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=XXX'                                   
         SPACE 2                                                                
PMDISK   DCB   DDNAME=PMDISK,DSORG=PS,RECFM=FB,LRECL=222,              X        
               BLKSIZE=2220,MACRF=PM                                            
*                                                                               
TADOWN   DCB   DDNAME=TADOWN,DSORG=PS,RECFM=FB,LRECL=372,              X        
               BLKSIZE=3720,MACRF=(GM,PM),EODAD=NOMORE                          
*                                                                               
SORTXFF  DC    (SORTLNQ)X'FF'      SORT RECORD OF ALL X'FF'                     
         EJECT           12345678901                                            
HDLINE1  DC    AL1(06),C'Agency'                                                
         DC    AL1(11),C'Agency Name'                                           
         DC    AL1(11),C'Publisher 1'                                           
         DC    AL1(11),C'Publisher 2'                                           
         DC    AL1(11),C'Publisher 3'                                           
         DC    AL1(06),C'Client'                                                
         DC    AL1(07),C'Product'                                               
         DC    AL1(10),C'Composer 1'                                            
         DC    AL1(10),C'Composer 2'                                            
         DC    AL1(10),C'Composer 3'                                            
         DC    AL1(10),C'Composer 4'                                            
         DC    AL1(08),C'Author 1'                                              
         DC    AL1(08),C'Author 2'                                              
         DC    AL1(11),C'Composition'                                           
         DC    AL1(10),C'Commercial'                                            
         DC    AL1(07),C'Program'                                               
         DC    AL1(07),C'Network'                                               
         DC    AL1(04),C'Date'                                                  
         DC    X'FFFF'                                                          
*                                                                               
DOWNTAB  DC    C'T',AL1(6)                                                      
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(25)                                                     
         DC    C'T',AL1(12)                                                     
         DC    C'T',AL1(20)                                                     
         DC    C'T',AL1(3)                                                      
         DC    C'D',AL1(8)         TEXT TYPE, BUT CONVERT DATE                  
         DC    X'FFFF'                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
*                                                                               
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,89,REPORT                                                     
         SSPEC H1,106,PAGE                                                      
         SSPEC H2,89,REQUESTOR                                                  
         SSPEC H4,89,C'Period'                                                  
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              ROUTINE HANDLES RECORD TRACES                                    
*                                                                               
MYTRC    NMOD1 0,*MYTRC*                                                        
         L     RC,SAVERC           RC=A(WORKING STORAGE)                        
*                                                                               
         TM    TPMOPTS,TTRACE      TRACE ENABLED?                               
         BZ    MYTRCX                                                           
         LM    R2,R4,0(R1)         R2=A(LIT),R3=A(IOAREA),R4=A(LEN)             
         ZIC   RF,0(R1)            RF=L'LITERAL                                 
         GOTO1 TRACE,DMCB,(R3),(R4),(R2),(RF)                                   
*                                                                               
MYTRCX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
*                                                                               
         DS    0D                                                               
HDHOOK   NMOD1 0,**HOOK*                                                        
         L     RC,SAVERC           RC=A(WORKING STORAGE)                        
*                                                                               
         LA    R2,OLDREC           R2=A(PREVIOUS SORT RECORD)                   
         USING SORTD,R2                                                         
*                                                                               
         MVC   H4+97(17),TPERIOD   REQUESTED PERIOD                             
         CLI   BYTE,C'E'           MARK END PAGE                                
         BNE   HDHK2                                                            
         MVC   H1+55(7),=CL7'Summary'                                           
         MVC   H2+55(7),=7X'BF'                                                 
         B     HDHK150                                                          
*                                                                               
HDHK2    MVC   H4+1(6),=C'Agency'                                               
         MVC   H4+13(L'TGAGY),TGAGY                                             
         MVC   H4+20(L'AGYNAM),AGYNAM                                           
*                                                                               
         MVC   H5+1(5),=C'Music'                                                
         MVC   H5+13(L'SORTMUS),SORTMUS                                         
         BAS   RE,HDHKTTL          SET REPORT TITLE (W/NWK NAME)                
*                                                                               
         MVC   AIO,AIO2            SET IOAREA TO MUSIC RECORD                   
         L     R4,AIO              R4=A(MUSIC RECORD)                           
         LA    R3,H6               START FLOATING LEFT HEADLINES AT H6          
*                                                                               
         BAS   RE,HDHKCOM          SET COMPOSITION                              
*                                                                               
         GOTO1 HDHKPUB,DMCB,(1,0)  SET PUBLISHER 1                              
         GOTO1 HDHKPUB,DMCB,(2,0)  SET PUBLISHER 2                              
         GOTO1 HDHKPUB,DMCB,(3,0)  SET PUBLISHER 3                              
*                                                                               
         GOTO1 HDHKCMP,DMCB,(1,0)  SET COMPOSER 1                               
         GOTO1 HDHKCMP,DMCB,(2,0)  SET COMPOSER 2                               
         GOTO1 HDHKCMP,DMCB,(3,0)  SET COMPOSER 3                               
         GOTO1 HDHKCMP,DMCB,(4,0)  SET COMPOSER 4                               
*                                                                               
         LA    R3,H6               START FLOATING RIGHT HEADLINES AT H6         
         GOTO1 HDHKAUT,DMCB,(1,0)  SET AUTHOR 1                                 
         GOTO1 HDHKAUT,DMCB,(2,0)  SET AUTHOR 2                                 
         BAS   RE,HDHKCLPR         SET CLIENT AND PRODUCT NAME                  
*                                                                               
HDHK150  MVC   AIO,AIO1            RESTORE AIO                                  
         BAS   RE,INTBOX           INITIALIZE BOX FIELDS                        
         MVC   SAVEP,P             SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         OI    TPMSTAT,THOOKED     WE JUST HAD HEADHOOK                         
         OI    SPOOLIND,SPNSPACE   SUPPRESS LINE SKIP AFTER HEADS               
         B     HDHKX                                                            
         SPACE 2                                                                
HDHKX    XIT1                                                                   
HDHKXR3  XIT1 REGS=(R3)                                                         
         EJECT                                                                  
*              REPORT TITLE (W/NWK NAME)                                        
*                                                                               
HDHKTTL  NTR1                                                                   
         MVC   H1+43(2),SPACES     CLEAR NETWORK NAME                           
         MVC   H1+45(1),SORTNET    DEFAULT IS FIRST CHARACTER ONLY              
         MVC   SVPNET,SPACES      SAVE DEFAULT NWK NAME                         
         MVC   SVPNET(1),SORTNET                                                
         L     RF,=A(NETWORKS)     TABLE OF NETWORKS                            
HDHKTTL5 CLI   0(RF),X'FF'                                                      
         BE    HDHKTTL8            END OF LIST                                  
         CLC   SORTNET,0(RF)       MATCH?                                       
         BE    *+12                                                             
         LA    RF,L'NETWORKS(RF)   NO                                           
         B     HDHKTTL5                                                         
         MVC   H1+43(3),0(RF)      NETWORK NAME                                 
         MVC   SVPNET,0(RF)        SAVE NETWORK NAME FOR LATER USE              
*                                                                               
HDHKTTL8 MVC   H1+46(24),=C' NETWORK BROADCAST DATES'                           
         CLI   H1+43,C' '          SINGLE CHARACTER NETWORK?                    
         BE    *+14                                                             
         MVC   H2+43(27),=27X'BF'                                               
         B     *+10                                                             
         MVC   H2+45(25),=27X'BF'  YES -- USE SHORTENED UNDERLINE               
*                                                                               
*        MVI   H2+52,C'('                                                       
*        MVC   H2+53(3),SORTUSE    USE TYPE                                     
*        MVC   H2+57(4),=C'USE)'                                                
         B     HDHKX                                                            
         EJECT                                                                  
*              ROUTINE TO SET COMPOSITION                                       
*                                                                               
HDHKCOM  NTR1                                                                   
         MVC   H6+1(11),=C'Composition'                                         
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL2           LOCATE COMPOSITION ELEMENT                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TANAD,R4            R4=A(COMPOSITION NAME ELEMENT)               
         ZIC   R1,TANALEN          ELEMENT LENGTH                               
         SH    R1,=Y(TANALNQ)      R1 = LENGTH OF ALL NAMES                     
         SR    R0,R0                                                            
         D     R0,=A(L'TANANAME)                                                
         LA    R4,TANANAME                                                      
         LTR   R1,R1               R1 = NUMBER OF FULL LINES                    
         BZ    HDHKCOM5                                                         
*                                                                               
HDHKCOM3 MVC   13(L'TANANAME,R3),0(R4)  COMPOSITION NAME                        
         LA    R3,L'H1(R3)         BUMP TO NEXT HEADLINE                        
         LA    R4,L'TANANAME(R4)                                                
         BCT   R1,HDHKCOM3                                                      
*                                                                               
HDHKCOM5 LTR   R1,R0               R1 = NUMBER OF CHARACTERS REMAINING          
         BZ    HDHKCOMX            NOTHING LEFT                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   13(0,R3),0(R4)                                                   
         LA    R3,L'H1(R3)         BUMP TO NEXT HEADLINE                        
HDHKCOMX B     HDHKXR3             XIT WITH R3                                  
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET AUTHOR INFO                                       
*                                                                               
HDHKAUT  NTR1                                                                   
         MVI   ELCODE,TAMUELQ      LOOK FOR AUTHOR ELEMENT                      
*                                                                               
         MVC   88(11,R3),=C'Author 1   '                                        
         MVI   HALF,C'A'                                                        
         MVI   HALF+1,1                                                         
         CLI   0(R1),1                                                          
         BNE   HKAUT10                                                          
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BE    HKAUT20                                                          
         B     HDHKXR3                                                          
*                                                                               
HKAUT10  CLI   0(R1),2                                                          
         BNE   HDHKXR3                                                          
         MVI   HALF+1,2                                                         
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   HDHKXR3                                                          
         MVC   88(11,R3),=C'Author 2   '                                        
*                                                                               
         USING TAMUD,R4                                                         
HKAUT20  L     R4,TGELEM                                                        
         ZIC   R5,TAMULEN          ELEMENT LENGTH                               
         SH    R5,=Y(TAMULNQ)      R1 = L'NAME                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   97(0,R3),TAMUNAME   AUTHOR NAME                                  
*                                                                               
         GOTO1 LICVAL,DMCB,(X'80',TAMULIC)                                      
         BNE   HDHKXR3                                                          
*                                                                               
         LA    R1,97(R3)                                                        
         LA    RF,36                                                            
HKAUT30  CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         CLI   1(R1),C' '                                                       
         BE    HKAUT40                                                          
         AHI   R1,1                                                             
         BCT   RF,HKAUT30                                                       
         B     HKAUT50                                                          
*                                                                               
HKAUT40  MVI   1(R1),C'-'                                                       
         MVC   3(L'TGLCNAME,R1),TGLCNAME   LICENSER NAME                        
HKAUT50  LA    R3,L'H1(R3)         BUMP TO NEXT HEADLINE                        
         B     HDHKXR3             XIT WITH R3                                  
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO SET PUBLISHER INFO                                    
*                                                                               
HDHKPUB  NTR1                                                                   
         MVI   ELCODE,TAMUELQ      LOOK FOR PUBLISHER ELEMENT                   
*                                                                               
         MVC   1(11,R3),=C'Publisher 1'                                         
         MVI   HALF,C'P'                                                        
         MVI   HALF+1,1                                                         
         CLI   0(R1),1                                                          
         BNE   HKPUB10                                                          
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BE    HKOUTPUT                                                         
         B     HDHKXR3                                                          
*                                                                               
HKPUB10  CLI   0(R1),2                                                          
         BNE   HKPUB20                                                          
         MVI   HALF+1,2                                                         
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   HDHKXR3                                                          
         MVC   1(11,R3),=C'Publisher 2'                                         
         B     HKOUTPUT                                                         
*                                                                               
HKPUB20  CLI   0(R1),3                                                          
         BNE   HDHKXR3                                                          
         MVI   HALF+1,3                                                         
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   HDHKXR3                                                          
         MVC   1(11,R3),=C'Publisher 3'                                         
         B     HKOUTPUT                                                         
*                                                                               
*              ROUTINE TO SET COMPOSER INFO                                     
*                                                                               
HDHKCMP  NTR1                                                                   
         MVI   ELCODE,TAMUELQ      LOOK FOR COMPOSER ELEMENT                    
*                                                                               
         MVI   HALF,C'C'                                                        
         MVI   HALF+1,1                                                         
         CLI   0(R1),1                                                          
         BNE   HKCMP10                                                          
         MVC   1(11,R3),=C'Composer 1 '                                         
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BE    HKOUTPUT                                                         
         B     HDHKXR3                                                          
*                                                                               
HKCMP10  CLI   0(R1),2                                                          
         BNE   HKCMP20                                                          
         MVI   HALF+1,2                                                         
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   HDHKXR3                                                          
         MVC   1(11,R3),=C'Composer 2 '                                         
         B     HKOUTPUT                                                         
*                                                                               
HKCMP20  CLI   0(R1),3                                                          
         BNE   HKCMP30                                                          
         MVI   HALF+1,3                                                         
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   HDHKXR3                                                          
         MVC   1(11,R3),=C'Composer 3 '                                         
         B     HKOUTPUT                                                         
*                                                                               
HKCMP30  CLI   0(R1),4                                                          
         BNE   HDHKXR3                                                          
         MVI   HALF+1,4                                                         
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   HDHKXR3                                                          
         MVC   1(11,R3),=C'Composer 4 '                                         
         B     HKOUTPUT                                                         
*                                                                               
         USING TAMUD,R4                                                         
HKOUTPUT L     R4,TGELEM                                                        
         ZIC   R5,TAMULEN          ELEMENT LENGTH                               
         SH    R5,=Y(TAMULNQ)      R1 = L'NAME                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   13(0,R3),TAMUNAME   AUTHOR NAME                                  
*                                                                               
         CLI   TAMULIC,0                                                        
         JE    HDHKXR3                                                          
*                                                                               
         GOTO1 LICVAL,DMCB,(X'80',TAMULIC)                                      
         BNE   HDHKXR3                                                          
         MVC   P1LIC,TGLCCDE       SAVE THIS LICENSER CODE                      
*                                                                               
         LA    R1,13(R3)                                                        
         LA    RF,36                                                            
HKOUTP10 CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         CLI   1(R1),C' '                                                       
         BE    HKOUTP20                                                         
         AHI   R1,1                                                             
         BCT   RF,HKOUTP10                                                      
         B     HKOUTP30                                                         
*                                                                               
HKOUTP20 MVI   1(R1),C'-'                                                       
         MVC   3(L'TGLCNAME,R1),TGLCNAME   LICENSER NAME                        
HKOUTP30 LA    R3,L'H1(R3)         BUMP TO NEXT HEADLINE                        
         B     HDHKXR3             XIT WITH R3                                  
         DROP  R4                                                               
         EJECT                                                                  
*               ROUTINE TO SET CLIENT AND PRODUCT NAME                          
*                                                                               
HDHKCLPR NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTCLI  CLIENT NAME                     
         MVC   88(6,R3),=C'Client'                                              
         MVC   97(36,R3),TGNAME                                                 
         LA    R3,L'H1(R3)         BUMP TO NEXT HEADLINE                        
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTPRD  PRODUCT NAME (OPT)              
         CLC   TGNAME,SPACES                                                    
         BE    *+16                                                             
         MVC   88(7,R3),=C'Product'                                             
         MVC   97(36,R3),TGNAME                                                 
         B     HDHKXR3                                                          
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO INITIALIZE BOX FIELDS                                 
*                                                                               
INTBOX   NTR1                                                                   
         L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES      INITIALIZE FIELDS                            
         MVC   BOXCOLS,SPACES                                                   
         TM    TPMOPTS,TNOBOX                                                   
         BO    *+8                                                              
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         B     HDHKX                                                            
         DROP  R4                                                               
         EJECT                                                                  
         GETEL2 (R4),DATADISP,ELCODE                                            
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
*              NETWORK TABLE                                                    
*                                                                               
NETWORKS DS    0CL3                FIRST CHARACTER IS THE NETWORK CODE          
         DC    C'ABC'                                                           
         DC    C'CBS'                                                           
         DC    C'FOX'                                                           
         DC    C'NBC'                                                           
         DC    C'PAX'                                                           
         DC    C'UPN'                                                           
         DC    C'WB '                                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
*              USE TABLE                                                        
         SPACE 2                                                                
SAVERC   DC    A(0)                                                             
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*                                                                               
TPMD     DSECT                                                                  
*                                                                               
MYTRACE  DS    A                   A(MYTRACE ROUTINE)                           
*                                                                               
AMASTD   DS    A                   A(MASTER)                                    
ALOGOC   DS    A                   A(LOGOC)                                     
ALOGO    DS    A                   A(LOGO)                                      
AREMOT   DS    A                   A(REMOTE)                                    
*                                                                               
AGYNAM   DS    CL36                AGENCY NAME                                  
SAGYNAM  DS    CL36                AGENCY NAME FROM SORT AGY                    
*                                                                               
TPERIOD  DS    CL17                DISPLAYABLE REQUEST PERIOD                   
TSTRT    DS    PL3                 START REQUEST DATE                           
TEND     DS    PL3                 END REQUEST DATE                             
*                                                                               
TPMOPTS  DS    XL1                 OPTIONS                                      
TTRACE   EQU   X'80'               TRACE ACTIVE                                 
TNOBOX   EQU   X'40'               BOXES OFF                                    
TOKNNWK  EQU   X'20'               NO NETWORK IS OK                             
TNONWK   EQU   X'10'               NO NETWORK ONLY                              
TCOUNT   EQU   X'08'               COUNT USES                                   
TASCAP   EQU   X'04'               FILTER BY ASCAP (PUB1)                       
TBMI     EQU   X'02'               FILTER BY BMI   (PUB1)                       
TSESAC   EQU   X'01'               FILTER BY SESAC (PUB1)                       
*                                                                               
TPMOPTS2 DS    XL1                 OPTIONS 2                                    
TTAPE    EQU   X'80'               GENERATE TAPE                                
*                                                                               
TPMSTAT  DS    XL1                 STATUS                                       
THOOKED  EQU   X'80'               WE HAD HEADHOOK                              
TNOT1ST  EQU   X'40'               NOT 1ST LINE OF COMMERCIAL ANY MORE          
TNOPRNT  EQU   X'20'               DON'T PRINT A LINE                           
*                                                                               
SVPNET   DS    CL3                 PRINTED NETWORK                              
AGYCD    DS    CL6                 AGENCY CODE                                  
MUSIC    DS    CL8                 MUSIC CODE                                   
CNAM     DS    CL36                COMMERCIAL TITLE                             
P1LIC    DS    C                   PUBLISHER 1 LICENSER                         
*                                                                               
CNTER    DS    PL6                 COUNTER                                      
USECNT   DS    PL6                 USE COUNTER BY NETWORK                       
*                                                                               
SRTREC   DS    CL(SORTLNQ)         SORT RECORD AREA                             
OLDREC   DS    CL(SORTLNQ)         PREVIOUS SORT RECORD                         
*                                                                               
SAVEP    DS    CL(L'P)             SAVE AREA FOR PRINT LINE                     
*                                                                               
DLBLOCK  DS    CL(DLCBXLX)                                                      
TAPEREC  DS    CL(TRECLNQ)          TAPE RECORD AREA                            
TPMLNQ   EQU   *-TPMD                                                           
         EJECT                                                                  
*              DSECT TO COVER SORT RECORD                                       
*                                                                               
SORTD    DSECT                                                                  
SORTAGY  DS    CL6                 AGENCY CODE                                  
SORTUSE  DS    CL3                 USE                                          
SORTMUS  DS    CL8                 MUSIC CODE                                   
SORTNET  DS    C                   NETWORK CODE                                 
SORTCID  DS    CL12                COMMERCIAL ID                                
SORTDATE DS    PL3                 USE DATE                                     
SORTUSEN DS    XL2                 USE NUMBER                                   
SORTKYLQ EQU   *-SORTD                                                          
*                                                                               
SORTPNME DS    CL15                PROGRAM NAME                                 
SORTCNAM DS    CL36                COMMERICAL TITLE                             
SORTLNQ  EQU   *-SORTD                                                          
         SPACE 2                                                                
*              DSECT TO COVER TAPE RECORD                                       
*                                                                               
TRECD    DSECT                                                                  
TRECAGY  DS    CL6                 AGENCY CODE                                  
TRECAGYN DS    CL25                AGENCY NAME                                  
TRECPUB1 DS    CL25                PUBLISHER 1 NAME                             
TRECPUB2 DS    CL25                PUBLISHER 2 NAME                             
TRECPUB3 DS    CL25                PUBLISHER 3 NAME                             
TRECCLIN DS    CL25                CLIENT NAME                                  
TRECPRDN DS    CL25                PRODUCT NAME                                 
TRECOMP1 DS    CL25                COMPOSER 1 NAME                              
TRECOMP2 DS    CL25                COMPOSER 2 NAME                              
TRECOMP3 DS    CL25                COMPOSER 3 NAME                              
TRECOMP4 DS    CL25                COMPOSER 4 NAME                              
TRECAUT1 DS    CL25                AUTHER 1 NAME                                
TRECAUT2 DS    CL25                AUTHER 2 NAME                                
TRECSONG DS    CL25                COMPOSITION NAME                             
TRECCID  DS    CL12                COMMERICAL ID                                
TRECPROG DS    CL20                SHOW(PROGRAM) NAME                           
TRECNWK  DS    CL3                 NETWORK CODE                                 
TRECDATE DS    CL6                 DATE                                         
TRECLNQ  EQU   *-TRECD                                                          
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
PRNTD    DSECT                                                                  
BL       DS    C                                                                
PRNTCOMM DS    CL47                COMMERCIAL ID                                
BC1      DS    C                                                                
PRNTDATE DS    CL5                 COLUMN 1:  MMMDD                             
BC2      DS    C                                                                
PRNTUSEN DS    CL4                 COLUMN 1:  USE NUMBER                        
BC3      DS    C                                                                
PRNTPNME DS    CL15                COLUMN 1:  PROGRAM NAME                      
BC4      DS    C                                                                
PRNTDLQ  EQU   *-BC1               LENGTH OF ONE COLUMN                         
BC5      DS    C                                                                
         DS    CL5                 COLUMN 2:  MMMDD                             
BC6      DS    C                                                                
         DS    CL4                 COLUMN 2:  USE NUMBER                        
BC7      DS    C                                                                
         DS    CL15                COLUMN 2:  PROGRAM NAME                      
BC8      DS    C                                                                
BC9      DS    C                                                                
         DS    CL5                 COLUMN 3:  MMMDD                             
BC10     DS    C                                                                
         DS    CL4                 COLUMN 3:  USE NUMBER                        
BC11     DS    C                                                                
         DS    CL15                COLUMN 3:  PROGRAM NAME                      
BR       DS    C                                                                
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         SPACE                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPD0D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDLOGOD                                                                       
* DDMASTD                                                                       
* DDREMOTED                                                                     
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
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'077TAREP30   10/04/16'                                      
         END                                                                    
