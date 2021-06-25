*          DATA SET TAREP40    AT LEVEL 083 AS OF 02/22/10                      
*PHASE T70340C,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70340 - AGENCY INTERFACE REPORT (DISK/DOWNLOAD)'               
T70340   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70340,R6                                                      
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
         SPACE 1                                                                
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
*                                                                               
VK10     MVC   TIFAGY,TGLST                                                     
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
VK40     BRAS  RE,VALOPT           VALIDATE OPTIONS                             
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
*              ROUTINE TO GENERATE REPORTS - CALLS SYSIO                        
         SPACE                                                                  
PREP     NTR1                                                                   
         MVI   ACTVSW,C'N'         NOTHING ACTIVE                               
         ZAP   COUNT,=P'0'                                                      
         ZAP   HDRCOUNT,=P'0'                                                   
         ZAP   DTLCOUNT,=P'0'                                                   
*                                                                               
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    *+20                                                             
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVI   RCSUBPRG,1                                                       
         TM    TUOPTS,TUEXACT                                                   
         BZ    *+8                                                              
         MVI   RCSUBPRG,10                                                      
         CLI   RECNUM,AYLINT                                                    
         BNE   *+8                                                              
         MVI   RCSUBPRG,2                                                       
*                                                                               
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
*                                                                               
         OI    TIQFLAG2,TIQFSUB    PASS SUBSIDIARY INVOICE RECORDS              
* NO-OP  MVI   TIFCUR,C'U'         READ US DOLLARS ONLY                         
         MVI   TIREAD,TLINDCDQ     READ BILL RECORDS                            
         MVI   TISUBRD,TLCKCDQ     SUBREAD CHECK RECORDS                        
         XC    SVAGY,SVAGY         CLEAR SAVED AGENCY                           
         XC    SVCLI,SVCLI         CLEAR SAVED CLIENT                           
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
*                                                                               
         CLI   ACTVSW,C'N'         IF PROCESSED AN INVOICE                      
         BE    PREPX                                                            
         BAS   RE,PUTTAPE          PUT LAST INV INFO TO TAPE/DATASET            
*                                                                               
         CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BNE   PREPW                                                            
         BAS   RE,SRT2TAPE         PUT SORTED RECORDS TO TAPE                   
         GOTO1 MYSORTER,DMCB,=C'END'                                            
*                                                                               
PREPW    BAS   RE,CLOSTAPE         CLOSE THE TAPE/DATASET                       
         BAS   RE,PRNTOT           PRINT TOTALS                                 
         CLI   ACTEQU,ACTDOWN      IF DOWNLOADING                               
         BNE   *+8                                                              
         BAS   RE,PREPD            PRINT SECOND REPORT FROM DATASET             
*                                                                               
PREPX    B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              PUT SORT RECORDS TO TAPE                                         
*======================================================================         
         USING SRTKD,R2                                                         
         USING RECBD,R5                                                         
SRT2TAPE NTR1                                                                   
         ZAP   AGYCNT,=P'0'                                                     
         XC    AGYCOM(12),AGYCOM                                                
SRT2TA   GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2               IF EOF?                                      
         BZ    SRT2TX                                                           
         LA    R5,L'SORTKEY(R2)                                                 
         OC    LASTAGY,LASTAGY     FIRST RECORD?                                
         BZ    SRT2TC                                                           
         CLC   LASTAGY,RECBAGY     AGENCY CHANGED?                              
         BE    SRT2TC                                                           
         LR    R4,R5               YES, SAVE ADDRESS OF CURRENT                 
         LA    R5,TOTLREC                                                       
         BAS   RE,BLDTOTAL         BUILD TOTAL RECORD                           
         BAS   RE,PUTIT            WRITE TOTAL RECORD                           
         ZAP   AGYCNT,=P'0'        RESET AGENCY INVOICE COUNTER                 
         XC    AGYCOM(12),AGYCOM                                                
         LR    R5,R4                                                            
*                                                                               
SRT2TC   MVC   LASTAGY,RECBAGY     SAVE CURRENT AGENCY                          
         AP    AGYCNT,=P'1'                                                     
         BAS   RE,PUTIT            WRITE TO TAPE                                
         MVC   CNTINV(20),SRTKINV                                               
         MVC   FEESTOT,SRTKFEES                                                 
         L     R1,AGYCOM                                                        
         A     R1,FEESTOT                                                       
         A     R1,CNTPNH                                                        
         ST    R1,AGYCOM                                                        
         L     R1,AGYNCOM                                                       
         A     R1,CNTTAX                                                        
         A     R1,CNTHAND                                                       
         A     R1,CNTGST                                                        
         ST    R1,AGYNCOM                                                       
         L     R1,AGYINV                                                        
         A     R1,CNTINV                                                        
         ST    R1,AGYINV                                                        
         BAS   RE,PRNTIT                                                        
         B     SRT2TA                                                           
*                                                                               
SRT2TX   LA    R5,TOTLREC                                                       
         BAS   RE,BLDTOTAL                                                      
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              BUILD TOTAL RECORD                                               
*======================================================================         
BLDTOTAL NTR1                                                                   
         MVI   RECB,C' '           CLEAR RECORD                                 
         MVC   RECB+1(L'RECB-1),RECB                                            
         MVC   RECBVND,=CL11'TALENTPART'                                        
         MVI   RECBTYP,C'T'        TOTAL RECORD                                 
         MVC   RECBAGY,LASTAGY                                                  
         GOTO1 DATCON,DMCB,(0,TGTODAY0),(X'20',RECBTDTE) SET TODAYS DT          
*                                                                               
         EDIT  AGYCNT,(6,RECBICNT),FILL=0                                       
         L     R1,AGYCOM                                                        
         CVD   R1,DUB                                                           
         UNPK  RECBCTOT,DUB                                                     
         L     R1,AGYNCOM                                                       
         CVD   R1,DUB                                                           
         UNPK  RECBNTOT,DUB                                                     
         L     R1,AGYINV                                                        
         CVD   R1,DUB                                                           
         UNPK  RECBTOTL,DUB                                                     
*        EDIT  AGYCOM,(11,RECBCTOT),FILL=0                                      
*        EDIT  AGYNCOM,(11,RECBNTOT),FILL=0                                     
*        EDIT  AGYINV,(11,RECBTOTL),FILL=0                                      
         B     XIT                                                              
*                                                                               
         DROP  R2,R5                                                            
         EJECT                                                                  
*              ROUTINE TO PRINT TOTAL LINE                                      
         SPACE                                                                  
PRNTOT   NTR1                                                                   
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)     PRINT A BLANK LINE                           
*                                                                               
         TM    TUOPTS,TUWCODE      IF WCODE OPTION                              
         BZ    PRNTOT10                                                         
         EDIT  HDRCOUNT,(8,P+1),ALIGN=LEFT                                      
         LR    R2,R0                                                            
         LA    R2,P+2(R2)                                                       
         MVC   0(14,R2),=C'HEADER RECORDS'                                      
         LA    R2,14(R2)                                                        
         MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
         EDIT  DTLCOUNT,(8,0(R2)),ALIGN=LEFT                                    
         LR    R1,R0                                                            
         AR    R2,R1                                                            
         MVC   1(14,R2),=C'DETAIL RECORDS'                                      
         B     PRNTOT20                                                         
*                                                                               
PRNTOT10 EDIT  COUNT,(8,P+1),ALIGN=LEFT                                         
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(13,R1),=CL13'TOTAL RECORDS'                                    
*                                                                               
PRNTOT20 LA    R2,P                                                             
         USING PRNTD,R2                                                         
         EDIT  TOTFEES,(12,PRTFEES),2,MINUS=YES                                 
         EDIT  TOTPNH,(12,PRTPNH),2,MINUS=YES                                   
         EDIT  TOTTAX,(12,PRTTAX),2,MINUS=YES                                   
         EDIT  TOTHAND,(12,PRTHND),2,MINUS=YES                                  
         EDIT  TOTGST,(12,PRTGST),2,MINUS=YES   GST/CSF/PST                     
         EDIT  TOTINV,(12,PRTINV),2,MINUS=YES                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO GET FROM DATASET AND PRINT SECOND REPORT              
         SPACE                                                                  
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
         L     R2,=A(AIDOWN)       AYINTER DOWN READS FROM AIDOWN               
         TM    TUOPTS,TUAGY                                                     
         BZ    *+8                                                              
         L     R2,=A(AIDOWNA)      AYINTER DOWN FOR AGY                         
         TM    TUOPTS,TUWCODE                                                   
         BZ    *+8                                                              
         L     R2,=A(AIDOWNW)      AYINTER DOWN W/WCODE READS FROM              
         CLI   RECNUM,AYLINT       AIDOWNW                                      
         BNE   *+8                                                              
         L     R2,=A(ALDOWN)       AYLINTER DOWN READS FROM ALDOWN              
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,DLBLOCK          INITIALIZE DLBLOCK FOR DOWNLOAD              
         USING DLCBD,R3                                                         
         BAS   RE,INITDWN                                                       
*                                                                               
PREPD2   GET   (R2),TAPEREC        GET REC FROM TEMP DATASET                    
         LA    R5,TAPEREC          FOR AYINTER DOWN ...                         
         LA    R0,5                RECORD IS 221 BYTES LONG (40X5+21)           
         TM    TUOPTS,TUAGY                                                     
         BZ    *+8                                                              
         LA    R0,8                RECORD IS 325 BYTES LONG (40X8+5)            
         TM    TUOPTS,TUWCODE                                                   
         BZ    *+8                 FOR AYINTER DOWN W/WCODE ...                 
         LA    R0,2                RECORD IS 90 BYTES LONG (40X2+10)            
         CLI   RECNUM,AYLINT                                                    
         BNE   *+8                 FOR AYLINTER DOWN ...                        
         LA    R0,3                RECORD IS 140 BYTES LONG (40X3+20)           
*                                                                               
PREPD3   MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBLEN,40                                                       
         MVC   DLCBFLD(40),0(R5)    PASS DATA                                   
         GOTO1 =V(DLFLD),DLCBD                                                  
         LA    R5,40(R5)           BUMP TO NEXT TAPE FIELD                      
         BCT   R0,PREPD3                                                        
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBLEN,21                                                       
         TM    TUOPTS,TUAGY        FOR AGY OPT DO THE LAST 5 BYTES              
         BZ    *+8                                                              
         MVI   DLCBLEN,5           FOR AYINTER DOWN W/WCODE DO THE              
         TM    TUOPTS,TUWCODE      FOR AYINTER DO THE LAST 21 BYTES             
         BZ    *+8                                                              
         MVI   DLCBLEN,10          FOR AYINTER DOWN W/WCODE DO THE              
         CLI   RECNUM,AYLINT       10 BYTES                                     
         BNE   *+8                                                              
         MVI   DLCBLEN,20          FOR AYLINTER DO THE LAST 20 BYTES            
         ZIC   RE,DLCBLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R5)    PASS DATA                                    
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPD2                                                           
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
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
         SPACE                                                                  
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
         ST    RE,BOXWIDTH                                                      
*                                                                               
         L     RF,AMASTD                                                        
         USING MASTD,RF                                                         
         L     R2,AREMOT                                                        
         USING REMOTED,R2                                                       
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BZ    NPRT30                                                           
         XC    MCREMPQK,MCREMPQK                                                
         B     NPRT40                                                           
*                                                                               
NPRT30   MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'TAD'                                                 
NPRT40   MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(6),=C'AYDATA'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
         B     PREPDX                                                           
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
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
*              HOOK FROM SYSIO                                                  
         SPACE                                                                  
IOHOOK   NTR1                                                                   
         MVC   AIO,TIAREC          SET IOAREA                                   
         LA    R5,TAPEREC          R5=A(TAPE RECORD)                            
         USING RECD,R5                                                          
*                                                                               
         CLI   TIMODE,PROCINV      INVOICE RECORD HOOK                          
         BNE   IOHOOK10                                                         
         CLI   ACTVSW,C'N'         IF PREVIOUS ACTIVITY                         
         BE    *+8                                                              
         BAS   RE,PUTTAPE          PUT PREV INV INFO TO TAPE/DATASET            
*                                                                               
         MVC   SVAGY,TIAGY         SAVE AGENCY CODE                             
         MVC   SVCLI,TICLI         SAVE CLIENT CODE                             
         MVI   ACTVSW,C'Y'         INVOICE PROCESSED                            
         XC    CNTRS(CNTRSX),CNTRS CLEAR COUNTERS                               
         XC    FEESTOT,FEESTOT     AND TOTALS / INVOICE                         
         MVI   0(R5),C' '          MOVE SPACES TO TAPE                          
         MVC   1(RECLNQ-1,R5),0(R5)                                             
         LA    RE,RECAYL                                                        
         MVI   0(RE),C' '          MOVE SPACES TO AYLINTER INFO ALSO            
         MVC   1(RECAYLLN-1,RE),0(RE)                                           
*                                                                               
         BAS   RE,SETCAN           SET CANADIAN CONVERSION RATE                 
*                                                                               
         BAS   RE,SETINV           SET INVOICE INFORMATION                      
*                                                                               
         CLI   REREADSW,C'N'       IF NEED TO READ SYSIO'S KEY                  
         BE    IOHOOKX                                                          
         MVC   KEY,TIKEY           RESTORE READ SEQUENCE                        
         GOTO1 HIGH                                                             
         B     IOHOOKX                                                          
*                                                                               
IOHOOK10 CLI   TIMODE,PROCREC      PROCESS RECORD MODE?                         
         BNE   IOHOOKX                                                          
         BAS   RE,SETCHK           SET CHECK INFORMATION                        
*                                                                               
IOHOOKX  B     XIT                                                              
         EJECT                                                                  
*              SET CANADIAN CONVERSION RATE FOR CAN$ INVOICES                   
         SPACE 3                                                                
SETCAN   NTR1                                                                   
         XC    CANRATE,CANRATE                                                  
         MVI   SUBSID,C'N'                                                      
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
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
*              FIX CANADIAN DOLLARS                                             
         SPACE 3                                                                
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
*              THIS ROUTINE SETS TAPEREC INFO AT INVOICE LEVEL                  
         SPACE                                                                  
SETINV   NTR1                                                                   
         MVC   RECID,=CL11'TALENTPART' TAPE ID                                  
*                                                                               
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         USING TLIND,R4                                                         
         CLC   OVRVEND,SPACES      IF VENDOR OVERRIDE                           
         BE    *+10                                                             
         MVC   RECID,OVRVEND       USE IT                                       
         TM    TUOPTS,TUGREY       IF GREY FEATURES REQUESTED                   
         BZ    SETINV5                                                          
         MVC   RECID,SPACES                                                     
         MVC   RECID(L'TIAGY),TIAGY SET AGENCY CODE                             
*                                                                               
SETINV5  BAS   RE,SETTAGY          SET INFO FROM AGENCY IF AGY FLIST            
         CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BE    SETINV7                                                          
*                                                                               
         MVC   RECCNUM(L'SVAGYCO),SVAGYCO                                       
         GOTO1 DATCON,DMCB,(0,TGTODAY0),(X'20',RECTODAY) SET TODAYS DT          
         GOTO1 TINVCON,DMCB,TIINV,RECINVN,DATCON  INVOICE NUMBER                
         GOTO1 DATCON,DMCB,(1,TIBIDATE),(X'20',RECDATE) INVOICE DATE            
         B     SETINV9                                                          
*                                                                               
         USING RECBD,R5                                                         
SETINV7  DS    0H                                                               
         MVC   RECBAGY(L'SVAGYCO),TLINAGY                                       
         GOTO1 TINVCON,DMCB,TIINV,RECBINV,DATCON  INVOICE NUMBER                
         GOTO1 DATCON,DMCB,(1,TIBIDATE),(X'20',RECBIDTE) INVOICE DATE           
*                                                                               
         USING RECD,R5                                                          
SETINV9  BAS   RE,SETPO            SET PURCHASE ORDER NUMBER                    
         BRAS  RE,SETJOB           SET CLI, PRD, JOB CODE OR EST #              
         BAS   RE,SETAPD           SET TAPD INFO (AMOUNT/SOR)                   
         BAS   RE,SETABD           SET TABD AMOUNTS                             
*        BAS   RE,SETCID           SET CID AND COMMENT FOR AYLINTER             
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE SETS INFO FOR AYLINTER                                   
*              R4=A(INVOICE RECORD) IN AIO, R5=A(TAPE RECORD)                   
         SPACE                                                                  
SETCID   NTR1                                                                   
         CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BNE   XIT                                                              
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   RECCID,TACOCID      SAVE CID                                     
         SPACE                                                                  
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPG))  GET GENERAL COMMENT                
         BNE   SETCX                                                            
         L     R4,TGELEM                                                        
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'            R1=L'DATA-1                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RECCMNT(0),3(R4)    SAVE ON TAPEREC                              
SETCX    B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE SETS AGENCY VALUES IF AGY FLIST REQUESTED           
         SPACE                                                                  
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
SETTAGY4 MVC   AIO,AIO1            NOT IN TABLE- ADD NEW ENTRY                  
         MVI   REREADSW,C'Y'       REREAD SYSIO'S KEY                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',TIAGY)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,SETAGY           SET AGENCY INFO                              
         MVC   AIO,TIAREC                                                       
         MVC   AGYTCO,SVAGYCO                                                   
         MVC   AGYTLEN,LENGTHS                                                  
         MVC   AGYTAGY,TIAGY                                                    
         B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE SETS RECPO - (ALL NUMERIC/RIGHT JUST)               
         SPACE                                                                  
SETPO    NTR1                                                                   
         CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BNE   *+14                                                             
*        MVC   RECBPO-RECBD(L'RECBPO,R5),=9C'0'                                 
         MVC   RECBPO-RECBD(L'RECBPO,R5),SPACES                                 
         B     *+10                                                             
         MVC   RECPO,=9C'0'        ZERO THE FIELD                               
*                                                                               
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTAUT))                                     
         BNE   SETPOX                                                           
         L     R1,TGELEM                                                        
         USING TANUD,R1                                                         
         ZIC   RE,TANULEN                                                       
         SH    RE,=Y(TANULNQ)      RE=LENGTH OF TANUMBER                        
*                                                                               
         LA    R3,TANUMBER-1(RE)   R3=A(LAST CHARACTER OF TANUMBER)             
         LA    R2,RECPO+L'RECPO-1  R2=(LAST CHAR OF RECORD FIELD)               
         CHI   RE,9                                                             
         BL    *+8                                                              
         LA    RE,9                                                             
         CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BNE   SETPO10                                                          
         LA    R2,RECBPO+L'RECBPO-1-RECBD                                       
         AR    R2,R5                                                            
         LR    RF,R2                                                            
*                                                                               
SETPO10  CLI   0(R3),C'A'          IF NON CHARACTER DATA                        
         BL    SETPO20             IGNORE                                       
         CLI   RECNUM,AYLINT       BURNETT                                      
         BNE   SETPO15                                                          
         CLI   0(R3),C'0'          IF NON NUMERIC DATA                          
         BL    SETPO20             MAKE ALL ZEROS                               
         B     SETPO19                                                          
*                                                                               
SETPO15  CLI   0(R3),C'0'          NOT BURNETT, ALPHA, CLEAR ALL ZEROS          
         BL    SETPO30                                                          
SETPO19  MVC   0(1,R2),0(R3)       ELSE, MOVE IT                                
         BCTR  R2,0                TRAVERSE THE NUMBER BACKWARDS                
*                                                                               
SETPO20  CLI   0(R3),C'/'          STOP                                         
         BE    SETPO30                                                          
         BCTR  R3,0                                                             
         BCT   RE,SETPO10                                                       
*                                                                               
SETPO30  CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BNE   SETPO40                                                          
         CLI   0(RF),X'F0'                                                      
         BL    SETPOX                                                           
         OC    RECBPO-RECBD(L'RECBPO,R5),=9C'0'                                 
         B     SETPOX                                                           
*                                                                               
SETPO40  MVC   RECPO,=9C'0'        ZERO THE FIELD                               
*                                                                               
SETPOX   B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*              THIS ROUTINE SETS INFO FROM TAPDD                                
         SPACE                                                                  
SETAPD   NTR1                                                                   
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         BAS   RE,FIXCAN           ADJUST TO US$ IF NECESSARY                   
         L     R1,TAPDPNH          P&H OR I&R                                   
         A     R1,TAPDINR                                                       
         ST    R1,CNTPNH                                                        
*                                                                               
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         CLI   RECNUM,AYLINT       IF AYLINTER                                  
         BE    SETAPD5                                                          
         MVI   RECSOR,C'S'                                                      
         TM    TGUSSTAT,SESSION                                                 
         BO    *+8                                                              
         MVI   RECSOR,C'R'                                                      
         TM    TUOPTS,TUGREY       IF GREY FEATURES REQUESTED                   
         BZ    SETAPD5                                                          
         CLI   TGUSEQU,UGRT        AND PAYMENT IS A GUARANTEE                   
         BNE   SETAPD5                                                          
         MVI   RECSOR,C'G'         INDICATE WITH G                              
*                                                                               
SETAPD5  CLI   RECNUM,AYLINT       ONLY IF AYLINTER                             
         BNE   XIT                                                              
         BRAS  RE,USEDET           SET USE NAME AND DETAILS                     
         B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE SETS AMOUNTS FROM TABDD                             
         SPACE                                                                  
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
**NO-OP  MVC   CNTACOM,TABDACOM    AGENCY COMMISSION                            
**11/09                                                                         
*                                                                               
         L     R2,TABDGST                                                       
         A     R2,TABDCSF                                                       
         A     R2,TABDPST                                                       
         ST    R2,CNTGST           GST/CSF/PST                                  
*                                                                               
         TM    TABDSTAT,TABDSCNW   TEST T&H IN CAN$                             
         BZ    XIT                                                              
         LA    R1,CNTTAX                                                        
         BAS   RE,FIXCAN6                                                       
         LA    R1,CNTHAND                                                       
         BAS   RE,FIXCAN6                                                       
         LA    R1,CNTGST                                                        
         BAS   RE,FIXCAN6                                                       
         B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE ACCUMULATES AMOUNTS AT THE CHECK LEVEL              
         SPACE                                                                  
SETCHK   NTR1                                                                   
         L     R3,TIAREC           R3=A(CHECK RECORD)                           
         USING TLCKD,R3                                                         
         CLC   TLCKSSN,=C'000000055'                                            
         BE    SETC5               THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000000066'                                            
         BE    SETC5               THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000003755'                                            
         BE    SETC5               THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000003876'                                            
         BE    SETC5               THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000007106'                                            
         BE    SETC5               THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000008213'                                            
         BE    SETC5               THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000003855'                                            
         BNE   SETC10              THIS IS ACTRA CHECK - NOT A PERSON!          
SETC5    CLC   TLCKSORT,=6X'FF'                                                 
         BE    XIT                 IGNORE IF GENERATED BY PAY                   
         SPACE                                                                  
SETC10   LR    R4,R3                                                            
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         SPACE 1                                                                
         BAS   RE,FIXCAN           ADJUST TO US$ IF NECESSARY                   
         CLC   TLCKSSN,=C'000000055'                                            
         BE    SETC12              IF ACTRA CHECK                               
         CLC   TLCKSSN,=C'000000066'                                            
         BE    SETC12              IF ACTRA CHECK                               
         CLC   TLCKSSN,=C'000003755'                                            
         BE    SETC12              IF ACTRA CHECK                               
         CLC   TLCKSSN,=C'000003876'                                            
         BE    SETC12              IF ACTRA CHECK                               
         CLC   TLCKSSN,=C'000007106'                                            
         BE    SETC12              IF ACTRA CHECK                               
         CLC   TLCKSSN,=C'000008213'                                            
         BE    SETC12              IF ACTRA CHECK                               
         CLC   TLCKSSN,=C'000003855'                                            
         BNE   SETC15              IF ACTRA CHECK                               
SETC12   L     R1,CNTPNH                                                        
         A     R1,TAPDPAYC         ADD ADD PAYMENT TO P&H TOTAL                 
         ST    R1,CNTPNH           (TO KEEP IN BALANCE)                         
         B     XIT                                                              
*                                                                               
SETC15   L     R2,TAPDPAYI                                                      
         A     R2,TAPDPAYC                                                      
         BAS   RE,SETPAY           SET PAYMENT AMOUNT                           
*                                                                               
         L     R2,TAPDREXP                                                      
         BAS   RE,SETREXP          SET REIM EXPENSES AMOUNT                     
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE ADDS TO APPROPRIATE CNTRS                           
*                                  R2=AMOUNT TO ADD                             
         SPACE                                                                  
SETPAY   NTR1                                                                   
         GOTO1 CATVAL,DMCB,TICAT                                                
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
*                                                                               
         LA    R3,CNTMODEL         R3=A(MODEL COUNTER)                          
         BAS   RE,TSTHAND          TEST HAND MODEL                              
         BE    SETPAY10                                                         
*                                                                               
         LA    R3,CNTXTRA          R3=A(EXTRA COUNTER)                          
         BAS   RE,TSTXTRA          TEST EXTRA                                   
         BE    SETPAY10                                                         
*                                                                               
         LA    R3,CNTMUS           R3=A(MUSICIAN COUNTER)                       
         BAS   RE,TSTMUS           TEST MUSICIAN                                
         BE    SETPAY10                                                         
*                                                                               
         LA    R3,CNTSING          R3=A(SINGER COUNTER)                         
         BAS   RE,TSTSING          TEST SINGER                                  
         BE    SETPAY10                                                         
*                                                                               
         LA    R3,CNTON            R3=A(ON CAMERA COUNTER)                      
         BAS   RE,TSTON            TEST ON CAMERA                               
         BE    SETPAY10                                                         
*                                                                               
         LA    R3,CNTOFF           R3=A(OFF CAMERA COUNTER)                     
         BAS   RE,TSTOFF           TEST OFF CAMERA                              
         BE    SETPAY10                                                         
*                                                                               
         LA    R3,CNTMISC          ADD ALL OTHER CATS TO MISC COUNTER           
*                                                                               
SETPAY10 L     R1,0(R3)                                                         
         AR    R1,R2               ADD TO TOTAL IN COUNTERS                     
         ST    R1,0(R3)                                                         
         A     R2,FEESTOT          ADD TO TOTAL FOR CHECK AMOUNTS               
         ST    R2,FEESTOT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS CC EQ IF HAND MODEL                                 
TSTHAND  NTR1                                                                   
         CLI   TGCAEQU,CTHM                                                     
         BE    YES                                                              
         CLI   TGCAEQU,CTHMB                                                    
         BE    YES                                                              
         B     NO                                                               
         SPACE                                                                  
*              ROUTINE SETS CC EQ IF EXTRA                                      
TSTXTRA  NTR1                                                                   
         TM    TGCATYPE,EXTRA                                                   
         BZ    NO                                                               
         B     YES                                                              
         SPACE                                                                  
*              ROUTINE SETS CC EQ IF MUSICIAN                                   
TSTMUS   NTR1                                                                   
         GOTO1 UNIVAL,DMCB,TACAUN                                               
         BNE   NO                                                               
*        TM    TGUNEQU,AFM                                                      
         GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    NO                                                               
         B     YES                                                              
         SPACE                                                                  
*              ROUTINE SETS CC EQ IF SINGER                                     
TSTSING  NTR1                                                                   
         TM    TGCASTAT,SINGER                                                  
         BZ    NO                                                               
         B     YES                                                              
         SPACE                                                                  
*              ROUTINE SETS CC EQ IF ON CAMERA                                  
TSTON    NTR1                                                                   
         CLC   =C'ON ',TACAONOF                                                 
         B     XIT                                                              
         SPACE                                                                  
*              ROUTINE SETS CC EQ IF OFF CAMERA                                 
TSTOFF   NTR1                                                                   
         CLC   =C'OFF',TACAONOF                                                 
         B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE ADDS TO CARCNT,WARDCNT,OR MISCCNT                   
*                                  R2=AMOUNT TO ADD                             
         SPACE                                                                  
         USING TAPDD,R4                                                         
SETREXP  NTR1                                                                   
         LA    R3,CNTCART          R3=A(CARTAGE AMOUNT)                         
         CLI   TAPDICDE,C'3'                                                    
         BE    SETREXP5                                                         
*                                                                               
         LA    R3,CNTWRD           R3=A(WARDROBE AMOUNT)                        
         CLI   TAPDICDE,C'1'                                                    
         BE    SETREXP5                                                         
*                                                                               
         LA    R3,CNTMISC          ADD TO MISCELLANEOUS AMOUNTS                 
*                                                                               
SETREXP5 L     R1,0(R3)                                                         
         AR    R1,R2               ADD TO TOTALS IN COUNTER                     
         ST    R1,0(R3)                                                         
         A     R2,FEESTOT          ADD TO TOTAL FOR CHECK AMOUNTS               
         ST    R2,FEESTOT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT TO TAPE OR DATASET                                
         SPACE                                                                  
PUTTAPE  NTR1                                                                   
         LA    R5,TAPEREC                                                       
         USING RECD,R5                                                          
*                                                                               
         CLC   RECINVN(6),=C'G10093'                                            
         BE    XIT                                                              
         CLC   RECINVN(6),=C'G10094'                                            
         BE    XIT                                                              
*                                                                               
         ZAP   AMOUNT,COUNT                                                     
         TM    TUOPTS,TUWCODE      IF WCODE OPTION                              
         BZ    *+10                                                             
         ZAP   AMOUNT,HDRCOUNT     USE HEADER COUNT                             
         CP    AMOUNT,=P'0'        IF TAPE NOT OPENED                           
         BH    *+8                                                              
         BAS   RE,OPENTAPE         OPEN IT                                      
*                                                                               
         SR    R4,R4               ACCUMULATE SUB-INV AMOUNTS                   
         LA    R3,CNTSINV          SUB-INVOICE COUNTERS                         
         LA    R1,NUMSINV          NUMBER OF COUNTERS                           
PUTT5    A     R4,0(R3)                                                         
         LA    R3,L'CNTRS(R3)                                                   
         BCT   R1,PUTT5                                                         
*                                                                               
         A     R4,FEESTOT          ADD CHECK FEES TO SUB-INV AMOUNT             
         L     R1,CNTINV                                                        
         SR    R1,R4               SHOULD EQUAL INVOICE TOTAL                   
         BZ    PUTT8                                                            
         OC    CANRATE,CANRATE     IF THIS ISN'T CAN$ INVOICE                   
         BNZ   *+6                                                              
         DC    H'0'                DIE IF SUM OF PARTS NEQ WHOLE                
*                                                                               
         A     R1,CNTHAND          ELSE ADD DIFF. BACK TO HANDLING AMT          
         ST    R1,CNTHAND                                                       
*                                                                               
PUTT8    CLI   RECNUM,AYLINT       IF AYLINTER                                  
         BE    PUTT13                                                           
*                                                                               
         LA    RE,NUMCNTRS                                                      
         LA    R3,CNTRS            R3=A(COUNTERS)                               
         LA    R2,RECINV           R2=A(TAPE AMOUNTS)                           
PUTT10   TM    TUOPTS,TUREGA                                                    
         BO    PUTT11A                                                          
         L     R1,0(R3)            ZONE 9 DECIMAL FORMAT                        
         CVD   R1,DUB                                                           
         UNPK  0(11,R2),DUB                                                     
         B     PUTT11X                                                          
*                                                                               
PUTT11A  EDIT  (B4,0(R3)),(11,(R2)),0,FILL=0                                    
PUTT11X  LA    R2,11(R2)                                                        
         LA    R3,L'CNTRS(R3)                                                   
         BCT   RE,PUTT10                                                        
*                                                                               
         TM    TUOPTS,TUAGY                                                     
         BZ    *+16                                                             
         MVC   RECAGY,SVAGY                                                     
         MVC   RECTPCLI,SVCLI                                                   
*                                                                               
         TM    TUOPTS,TUWCODE      IF WORK CODE OPTION                          
         BZ    PUTT15                                                           
         BAS   RE,PUTHDR           PUT HEADER RECORD TO TAPE                    
         BAS   RE,PUTDTL           PUT DETAIL RECORDS TO TAPE                   
         B     PUTT20                                                           
*                                                                               
         USING RECBD,R5                                                         
PUTT13   OC    CNTINV,CNTINV       IGNORE ZERO INVOICES                         
         BZ    XIT                                                              
         L     R1,FEESTOT          WAGES + P&H                                  
         A     R1,CNTPNH                                                        
         LR    R2,R1               SAVE IT                                      
         CVD   R1,DUB                                                           
         UNPK  RECBCOM,DUB                                                      
*        EDIT  (R1),(11,RECBCOM),FILL=0                                         
*                                                                               
*        EDIT  CNTINV,(11,RECBITOT),FILL=0                                      
         L     R1,CNTINV           TAX + HAND = ( TOTAL - WAGES+P&H )           
         CVD   R1,DUB                                                           
         UNPK  RECBITOT,DUB                                                     
         L     R1,CNTINV           TAX + HAND = ( TOTAL - WAGES+P&H )           
         SR    R1,R2                                                            
         LR    R2,R1                                                            
         CVD   R1,DUB                                                           
         UNPK  RECBNCOM,DUB                                                     
*        EDIT  (R1),(11,RECBNCOM),FILL=0                                        
         S     R2,CNTTAX           RECALCULATE HANDLING FOR ROUNDING            
         S     R2,CNTGST           DIFFERENCE                                   
         ST    R2,CNTHAND                                                       
*                                                                               
         CLC   RECBUSE,=C'GRT'     GUARANTEE?                                   
         BNE   PUTT13X                                                          
         TM    FEESTOT,X'80'       NEGATIVE AMOUNT?                             
         BO    PUTT13B                                                          
         CLC   FEESTOT,=F'5000000' AND WAGE >= $50,000.00                       
         BNL   PUTT13P                                                          
         B     PUTT13X                                                          
PUTT13B  CLC   FEESTOT,=F'-5000000' AND WAGE <= -$50,000.00                     
         BL    PUTT13X                                                          
PUTT13P  MVI   RECBPFLG,C'Y'       PREVIOUSLY BILLED                            
*                                                                               
PUTT13X  MVI   RECBTYP,C'D'        DETAILS                                      
         LA    R2,SORTKEY                                                       
         USING SRTKD,R2                                                         
         MVC   SRTKAGY,RECBAGY                                                  
         MVC   SRTKCLI,RECBCLI                                                  
         MVC   SRTKPO,RECBPO                                                    
         MVC   SRTKINV(20),CNTINV                                               
         MVC   SRTKFEES,FEESTOT                                                 
         DROP  R2                                                               
*                                                                               
         BAS   RE,PUTSORT                                                       
         AP    COUNT,=P'1'         ADD TO COUNT ON TAPE                         
*        B     PUTT20                                                           
         B     XIT                                                              
         USING RECD,R5                                                          
*                                                                               
PUTT15   BAS   RE,PUTIT            PUT ONE REC PER INV TO TAPE/DATASET          
         AP    COUNT,=P'1'         ADD TO COUNT ON TAPE                         
*                                                                               
PUTT20   BAS   RE,PRNTIT           PRINT INVOICE INFO                           
         B     XIT                                                              
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
*              ROUTINE TO PUT A HEADER RECORD TO TAPE OR DATASET                
         SPACE                                                                  
PUTHDR   NTR1                                                                   
         LA    R5,TAPEREC          R5=A(TAPE RECORD)                            
         USING RECD,R5                                                          
*                                                                               
         LA    R3,HDRREC           R3,=A(HEADER RECORD)                         
         USING RECHD,R3                                                         
*                                                                               
         MVI   0(R3),C' '          MOVE SPACES TO TAPE                          
         MVC   1(RECHLNQ-1,R3),0(R3)                                            
*                                                                               
         MVC   RECHCNUM,RECCNUM    COMPANY NUMBER                               
         MVC   RECHOFCD,RECOFCD    OFFICE CODE                                  
         MVC   RECHID,RECID        MOVE 'TALENTPART '                           
         MVI   RECH1,C'1'          C'1'                                         
         MVC   RECHINVN,RECINVN    INVOICE NUMBER                               
         MVC   RECHDATE,RECDATE    INVOICE DATE                                 
**NO-OP**MVC   RECHPO,RECPO        P.O NUMBER AS OF 4/10/96                     
         MVC   RECHINV,RECINV      INVOICE TOTAL                                
         MVC   RECHSOR,RECSOR      SESSION OR REUSE                             
         MVC   RECHCLI(L'RECCLI),RECCLI  CLIENT                                 
         MVC   RECHPRD(L'RECPRD),RECPRD  PRODUCT                                
         MVC   RECHJOB(L'RECJOB),RECJOB  JOB CODE                               
         MVC   RECHTYP,=C'10'            RECORD TYPE                            
*                                                                               
         BAS   RE,PUTIT            PUT HEADER REC TO TAPE/DATASET               
         AP    HDRCOUNT,=P'1'      ADD TO HEADER COUNTER                        
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
*              ROUTINE TO PUT DETAIL RECORDS TO TAPE OR DATASET                 
         SPACE                                                                  
PUTDTL   NTR1                                                                   
         LA    R5,TAPEREC          R5=A(TAPE RECORD)                            
         USING RECD,R5                                                          
*                                                                               
         LA    R3,DTLREC           R3=A(DETAIL RECORD)                          
         USING RECDD,R3                                                         
*                                                                               
         MVI   0(R3),C' '          MOVE SPACES TO TAPE                          
         MVC   1(RECDLNQ-1,R3),0(R3)                                            
*                                                                               
         MVC   RECDCNUM,RECCNUM    COMPANY NUMBER                               
         MVC   RECDOFCD,RECOFCD    OFFICE CODE                                  
         MVC   RECDID,RECID        MOVE 'TALENTPART '                           
         MVI   RECD1,C'1'          C'1'                                         
         MVC   RECDINVN,RECINVN    INVOICE NUMBER                               
         MVC   RECDTYP,=C'20'      DETAIL RECORD TYPE                           
*                                                                               
         GOTO1 PUTDAMTS,DMCB,NUMSINV,CNTSINV,WKCDTB1                            
*                                                                               
         CLI   RECSOR,C'R'         FOR REUSE PAYMENTS ON 0700/ATT               
         BNE   PUTDTL5                                                          
         CLC   =C'0700',SVAGY      USE DIFFERENT WKCODES                        
         BNE   PUTDTL5                                                          
         CLC   =C'ATT',SVCLI                                                    
         BE    PUTDTL10                                                         
PUTDTL5  GOTO1 PUTDAMTS,DMCB,NUMFEES,CNTRFEES,WKCDTB2                           
         B     XIT                                                              
*                                                                               
PUTDTL10 GOTO1 PUTDAMTS,DMCB,NUMFEES,CNTRFEES,WKCDTB3                           
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
*              ROUTINE TO PUT TO TAPE FOR EACH WORK CODE                        
         SPACE                                                                  
         USING RECDD,R3            R3=A(TAPE RECORD)                            
PUTDAMTS NTR1                                                                   
         L     R4,0(R1)            R4=(NUMBER OF AMOUNTS)                       
         L     R2,4(R1)            R2=A(WORK CODE AMOUNTS)                      
         L     R5,8(R1)            R5=A(WORK CODE TABLE)                        
*                                                                               
PUTDAMT5 L     R1,0(R2)            IF WORK CODE AMOUNT                          
         LTR   R1,R1                                                            
         BZ    PUTDAMT8                                                         
         CVD   R1,DUB                                                           
         UNPK  RECDWKAM(11),DUB    WORK CODE AMOUNT                             
         MVC   RECDWKCD,0(R5)      WORK CODE                                    
         BAS   RE,PUTIT            PUT REC TO TAPE/DATASET                      
         AP    DTLCOUNT,=P'1'      ADD TO DETAIL COUNTER                        
*                                                                               
PUTDAMT8 LA    R5,L'WKCDTB1(R5)    BUMP TO NEXT WORK CODE                       
         LA    R2,L'CNTSINV(R2)    BUMP TO NEXT WORK CODE AMOUNT                
         BCT   R4,PUTDAMT5         LOOP                                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO PUT A RECORD TO TAPE OR DATASET                       
*              IF WCODE OPTION - R3 = A(HEADER)                                 
*                                R3 = A(DETAIL)                                 
*              ELSE              R5 = A(TAPEREC)                                
*                                                                               
PUTIT    NTR1                                                                   
         TM    TUOPTS,TUNOTAPE     NO TAPE REQUESTED?                           
         BO    PUTITX                                                           
*                                                                               
         TM    TUOPTS,TUWCODE      TEST WCODE OPTION                            
         BZ    PUTIT10                                                          
*                                                                               
*                                  IF WCODE OPTION                              
         L     R2,=A(AIDOWNW)      AIDOWNW = AYINTER DOWN                       
         CLI   ACTEQU,ACTDOWN                                                   
         BE    *+8                                                              
         L     R2,=A(AIDISKW)      AIDISKW = AYINTER REPORT                     
         PUT   (R2),(R3)                                                        
         B     PUTITX                                                           
*                                                                               
*                                  IF NO WCODE OPTION                           
PUTIT10  CLI   ACTEQU,ACTDOWN      TEST DOWNLOAD                                
         BE    PUTIT20                                                          
*                                                                               
*                                  IF NOT DOWNLOADING                           
         L     R2,=A(ALDISK)       ALDISK = AYLINTER REPORT                     
         CLI   RECNUM,AYLINT                                                    
         BE    PUTIT30                                                          
         L     R2,=A(AIDISK)       AIDISK = AYINTER REPORT                      
         TM    TUOPTS,TUAGY                                                     
         BZ    PUTIT30                                                          
         L     R2,=A(AIDISKA)      AIDISK = AYINTER REPORT                      
         B     PUTIT30                                                          
*                                                                               
*                                  IF DOWNLOADING                               
PUTIT20  L     R2,=A(ALDOWN)       ALDOWN = AYLINTER DOWN                       
         CLI   RECNUM,AYLINT                                                    
         BE    PUTIT30                                                          
         L     R2,=A(AIDOWN)       AIDOWN = AYINTER DOWN                        
         TM    TUOPTS,TUAGY                                                     
         BZ    PUTIT30                                                          
         L     R2,=A(AIDOWNA)      AIDOWN = AYINTER DOWN                        
                                                                                
PUTIT30  PUT   (R2),(R5)           PUT IT TO TAPE/DATASET                       
*                                                                               
PUTITX   B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE PRINTS OUT A LINE OF INFO                           
         SPACE                                                                  
         USING RECD,R5             R5=A(TAPE RECORD)                            
PRNTIT   NTR1                                                                   
*        CLI   ACTEQU,ACTDOWN                                                   
*        BE    XIT                                                              
*                                                                               
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
         CLI   RECNUM,AYLINT                                                    
         BE    PRNTIT30                                                         
*                                                                               
         MVC   PRTCNUM,RECCNUM     COMPANY NUMBER                               
         MVC   PRTOFCD,RECOFCD     OFFICE CODE                                  
         MVC   PRTINVN,RECINVN     INVOICE NUMBER                               
         MVC   PRTDATE,RECDATE     INVOICE DATE                                 
         MVC   PRTPO,RECPO         PURCHASE ORDER NUMBER                        
*                                                                               
         TM    TUOPTS,TUEXACT      IF EXACT OPTION                              
         BZ    *+14                                                             
         MVC   PRTEST,RECEST       MOVE OUT WHOLE ESTMATE NUMBER                
         B     PRNTIT10                                                         
         MVC   PRTCLI,RECCLI       ELSE, CLIENT                                 
         MVC   PRTPRD,RECPRD             PRODUCT                                
         MVC   PRTJOB,RECJOB             JOB CODE                               
*                                                                               
PRNTIT10 MVC   PRTTYPE(L'RECSOR),RECSOR     SESSION OR REUSE                    
         B     PRNTIT50                                                         
*                                                                               
         USING RECBD,R5                                                         
PRNTIT30 MVC   PRTAGY,RECBAGY                                                   
         MVC   PRTINVN,RECBINV     INVOICE NUMBER                               
         MVC   PRTDATE,RECBIDTE    INVOICE DATE                                 
         MVC   PRTPO,RECBPO        PURCHASE ORDER NUMBER                        
         MVC   PRTCLI,RECBCLI      CLIENT                                       
         MVC   PRTJOB,RECBJOB      JOB CODE                                     
         USING RECD,R5                                                          
*                                                                               
PRNTIT50 EDIT  CNTINV,(12,PRTINV),2,MINUS=YES   INVOICE AMOUNT                  
         EDIT  CNTTAX,(12,PRTTAX),2,MINUS=YES   PAYROLL TAXES                   
         EDIT  CNTPNH,(12,PRTPNH),2,MINUS=YES   P & H CONTRIBUTIONS             
         EDIT  CNTHAND,(12,PRTHND),2,MINUS=YES  HANDLING                        
         EDIT  CNTGST,(12,PRTGST),2,MINUS=YES   GST/CSF/PST                     
         BAS   RE,ADDTOT           ADD TO TOTALS                                
*                                                                               
         EDIT  FEESTOT,(12,PRTFEES),2,MINUS=YES                                 
         L     R1,TOTFEES                                                       
         A     R1,FEESTOT          ADD TO TOTAL FEES                            
         ST    R1,TOTFEES                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    PRINT IT                                      
         B     XIT                                                              
         SPACE 2                                                                
*              ADD CURRENT AMOUNTS TOT TOTAL                                    
         SPACE                                                                  
ADDTOT   NTR1                                                                   
         LA    RE,NUMINV           NUMBER OF INVOICE COUNTERS                   
         LA    R3,TOTCNTRS         R3=A(TOTAL COUNTERS)                         
         LA    R2,CNTRS            R2=A(CURRENT COUNTERS)                       
*                                                                               
ADDTOT5  L     R1,0(R3)                                                         
         A     R1,0(R2)            ADD TO TOTAL                                 
         ST    R1,0(R3)                                                         
*                                                                               
         LA    R2,4(R2)            BUMP CURRENT COUNTERS                        
         LA    R3,4(R3)            BUMP TOTAL COUNTERS                          
         BCT   RE,ADDTOT5          LOOP                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OPEN TAPE/DATASET                                     
         SPACE                                                                  
OPENTAPE NTR1                                                                   
         TM    TUOPTS,TUNOTAPE     IF NO TAPE REQUESTED                         
         BO    OPENTX              EXIT                                         
         TM    FILESTAT,FILEOPEN   FILE OPENED ALREADY?                         
         BO    OPENTX              DON'T OPEN AGAIN                             
*                                                                               
         TM    TUOPTS,TUWCODE      TEST WCODE OPTION                            
         BZ    OPENT05                                                          
*                                                                               
*                                  IF WCODE OPTION                              
         L     R2,=A(AIDOWNW)      AIDOWNW = AYINTER DOWN                       
         CLI   ACTEQU,ACTDOWN                                                   
         BE    *+8                                                              
         L     R2,=A(AIDISKW)      AIDISKW = AYINTER REPORT                     
         B     OPENT20                                                          
*                                                                               
*                                  NO WCODE OPTION                              
OPENT05  CLI   ACTEQU,ACTDOWN                                                   
         BE    OPENT10                                                          
*                                  IF NOT DOWNLOADING                           
         L     R2,=A(ALDISK)       ALDISK = AYLINTER REPORT                     
         CLI   RECNUM,AYLINT                                                    
         BE    OPENT20                                                          
         L     R2,=A(AIDISK)       AIDISK = AYINTER REPORT                      
         TM    TUOPTS,TUAGY                                                     
         BZ    OPENT20                                                          
         L     R2,=A(AIDISKA)      AIDISK = AYINTER REPORT                      
         B     OPENT20                                                          
*                                                                               
*                                  IF DOWNLOADING                               
OPENT10  L     R2,=A(ALDOWN)       ALDOWN = AYLINTER DOWN                       
         CLI   RECNUM,AYLINT                                                    
         BE    OPENT20                                                          
         L     R2,=A(AIDOWN)       AIDOWN = AYINTER DOWN                        
         TM    TUOPTS,TUAGY                                                     
         BZ    OPENT20                                                          
         L     R2,=A(AIDOWNA)      AIDISK = AYINTER REPORT                      
*                                                                               
OPENT20  OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    FILESTAT,FILEOPEN                                                
*                                                                               
OPENTX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CLOSE THE TAPE/DATASET                                
         SPACE                                                                  
CLOSTAPE NTR1                                                                   
         TM    TUOPTS,TUNOTAPE     ONLY IF TAPE REQUESTED                       
         BO    CLOSTX                                                           
*                                                                               
         TM    TUOPTS,TUWCODE      TEST WCODE OPTION                            
         BZ    CLOST05                                                          
*                                                                               
*                                  IF WCODE OPTION                              
         L     R2,=A(AIDOWNW)      AIDOWNW = AYINTER DOWN                       
         CLI   ACTEQU,ACTDOWN                                                   
         BE    *+8                                                              
         L     R2,=A(AIDISKW)      AIDISKW = AYINTER REPORT                     
         B     CLOST20                                                          
*                                                                               
*                                  NO WCODE OPTION                              
CLOST05  CLI   ACTEQU,ACTDOWN      TEST DOWNLOAD                                
         BE    CLOST10                                                          
*                                  IF NOT DOWNLOADING                           
         L     R2,=A(ALDISK)       ALDISK = AYLINTER REPORT                     
         CLI   RECNUM,AYLINT                                                    
         BE    CLOST20                                                          
         L     R2,=A(AIDISK)       AIDISK = AYINTER REPORT                      
         TM    TUOPTS,TUAGY                                                     
         BZ    CLOST20                                                          
         L     R2,=A(AIDISKA)      AIDISK = AYINTER REPORT                      
         B     CLOST20                                                          
*                                                                               
*                                  IF DOWNLOADING                               
CLOST10  L     R2,=A(ALDOWN)       ALDOWN = AYLINTER DOWN                       
         CLI   RECNUM,AYLINT                                                    
         BE    CLOST20                                                          
         L     R2,=A(AIDOWN)       AIDOWN = AYINTER DOWN                        
         TM    TUOPTS,TUAGY                                                     
         BZ    CLOST20                                                          
         L     R2,=A(AIDOWNA)      AIDISK = AYINTER REPORT                      
*                                                                               
CLOST20  CLOSE ((2))                                                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         NI    FILESTAT,X'FF'-FILEOPEN                                          
*                                                                               
CLOSTX   B     XIT                                                              
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
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
COMPLM   DC    6X'FF'                                                           
         SPACE 2                                                                
*              WORK CODE TABLE 1 TO CORRESPOND TO INVOICE AMOUNTS               
WKCDTB1  DS    0CL10                                                            
         DC    CL10'ZM'            PAYROLL TAXES                                
         DC    CL10'ZW'            PENSION                                      
         DC    CL10'ZM'            HANDLING                                     
         DC    CL10'GST'           GOODS AND SERVICE TAX                        
         SPACE 2                                                                
*              WORK CODE TABLE 2 TO CORRESPOND TO CHECK AMOUNTS                 
WKCDTB2  DS    0CL10                                                            
         DC    CL10'ZB'            MUSICIANS PAYMENTS                           
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
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(184)'                                 
*                                                                               
AIDISK   DCB   DDNAME=AIDISK,DSORG=PS,RECFM=FB,LRECL=221,              X        
               BLKSIZE=2210,MACRF=PM                                            
         SPACE 2                                                                
AIDISKW  DCB   DDNAME=AIDISK,DSORG=PS,RECFM=FB,LRECL=90,               X        
               BLKSIZE=900,MACRF=PM                                             
         SPACE 2                                                                
ALDISK   DCB   DDNAME=ALDISK,DSORG=PS,RECFM=FB,LRECL=140,              X        
               BLKSIZE=1400,MACRF=PM                                            
         SPACE 2                                                                
AIDOWN   DCB   DDNAME=TADOWN,DSORG=PS,RECFM=FB,LRECL=221,              X        
               BLKSIZE=2210,MACRF=(GM,PM),EODAD=NOMORE                          
         SPACE 2                                                                
AIDOWNW  DCB   DDNAME=TADOWN,DSORG=PS,RECFM=FB,LRECL=90,               X        
               BLKSIZE=900,MACRF=(GM,PM),EODAD=NOMORE                           
         SPACE 2                                                                
ALDOWN   DCB   DDNAME=TADOWN,DSORG=PS,RECFM=FB,LRECL=140,              X        
               BLKSIZE=1400,MACRF=(GM,PM),EODAD=NOMORE                          
*                                                                               
AIDISKA  DCB   DDNAME=AIDISK,DSORG=PS,RECFM=FB,LRECL=325,              X        
               BLKSIZE=3250,MACRF=PM                                            
*                                                                               
AIDOWNA  DCB   DDNAME=TADOWN,DSORG=PS,RECFM=FB,LRECL=325,              X        
               BLKSIZE=3250,MACRF=(GM,PM),EODAD=NOMORE                          
         EJECT                                                                  
         LTORG                                                                  
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
         SSPEC H7,2,C'CO OF INV DT INV #  PO NUM    CLI  PRD  JOB'              
         SSPEC H8,2,C'-- -- ------ -----  ------    ---  ---  ---'              
*                                                                               
         SPROG 10                                                               
         SSPEC H7,2,C'CO OF INV DT INV #  PO NUM    ESTIMATE #'                 
         SSPEC H8,2,C'-- -- ------ -----  ------    ----------'                 
*                                                                               
         SPROG 1,2,10                                                           
         SSPEC H4,100,C'PERIOD'                                                 
         SSPEC H7,50,C'TY        FEES      P AND H  PAYROLL TAX'                
         SSPEC H8,50,C'--        ----      -------  -----------'                
         SSPEC H7,92,C'   HANDLING      GST/CSF  INVOICE TOT'                   
         SSPEC H8,92,C'   --------      -------  -----------'                   
*                                                                               
         SPROG 1,10                                                             
         SSPEC H1,53,C'AGENCY INTERFACE REPORT'                                 
         SSPEC H2,53,23X'BF'                                                    
*                                                                               
         SPROG 2                                                                
         SSPEC H1,46,C'AGENCY INTERFACE REPORT (INCLUDES CID)'                  
         SSPEC H2,46,38X'BF'                                                    
         DC    X'00'                                                            
         EJECT                                                                  
*=====================================================================          
*              ROUTINE TO VALIDATE OPTIONS                                      
*=====================================================================          
VALOPT   NTR1  BASE=*,LABEL=*                                                   
         MVC   OVRVEND,SPACES      PRESET TO SPACES                             
*                                                                               
         LA    R2,SITOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0                                                          
         JE    FLDINV                                                           
         ZIC   R0,4(R1)            NUMBER OF SCANNER ENTRIES                    
*                                                                               
VOPT10   CLC   =C'TAPE',SCDATA1                                                 
         BNE   VOPT15                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'N'                                                     
         JNE   FLDINV                                                           
         OI    TUOPTS,TUNOTAPE     DON'T GENERATE DISK OR                       
         B     VOPTNEXT            DOWNLOADABLE REPORT                          
*                                                                               
VOPT15   CLC   =C'WCODE',SCDATA1                                                
         BNE   VOPT20                                                           
         CLI   RECNUM,AYLINT       NOT VALID FOR AYLINTER                       
         JE    FLDINV                                                           
         TM    TUOPTS,TUEXACT      AND NOT VALID WITH EXACT OPTION              
         JO    FLDINV                                                           
         OI    TUOPTS,TUWCODE      TAPE BY WORK CODE                            
         B     VOPTNEXT                                                         
*                                                                               
VOPT20   CLC   =C'EXACT',SCDATA1                                                
         BNE   VOPT30                                                           
         CLI   RECNUM,AYLINT       NOT VALID FOR AYLINTER                       
         JE    FLDINV                                                           
         TM    TUOPTS,TUWCODE      AND NOT VALID WITH WORK CODE OPTION          
         JO    FLDINV                                                           
         OI    TUOPTS,TUEXACT      COPY ESTIMATE # EXACTLY                      
         B     VOPTNEXT                                                         
*                                                                               
VOPT30   CLC   =C'GREY',SCDATA1    IF GREY OVERRIDE                             
         BNE   VOPT40                                                           
         CLC   OVRVEND,SPACES      CAN'T USE WITH VEND=                         
         JNE   FLDINV                                                           
         OI    TUOPTS,TUGREY       DO GREY FEATURES                             
         B     VOPTNEXT                                                         
*                                                                               
VOPT40   CLC   =C'VEND',SCDATA1    IF VENDOR OVERRIDE                           
         BNE   VOPT50                                                           
         TM    TUOPTS,TUGREY       CAN'T USE WITH GREY OPTION                   
         JO    FLDINV                                                           
         ZIC   R1,SCLEN2                                                        
         LTR   R1,R1                                                            
         JZ    FLDINV                                                           
         CH    R1,=H'11'                                                        
         JH    FLDINV                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OVRVEND(0),SCDATA2                                               
         B     VOPTNEXT                                                         
*                                                                               
VOPT50   CLC   =C'AGY',SCDATA1     INCLUDE AGENCY                               
         BNE   VOPT60                                                           
         OI    TUOPTS,TUAGY        DO AGY FEATURES                              
         B     VOPTNEXT                                                         
*                                                                               
VOPT60   CLC   =C'REGA',SCDATA1    REGULAR AMOUNTS                              
         JNE   FLDINV                                                           
         OI    TUOPTS,TUREGA       DO REGULAR AMOUNTS                           
         B     VOPTNEXT                                                         
*                                                                               
VOPTNEXT LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10                                                        
*                                                                               
VOPTX    XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              THIS ROUTINE SETS RECJOB                                         
*=====================================================================          
SETJOB   NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTEST))                                     
         BNE   SETJOBX                                                          
         L     R1,TGELEM           R1=A(ELEMENT)                                
         USING TANUD,R1                                                         
*                                                                               
         TM    TUOPTS,TUEXACT      IF WANT EST NUMBER EXACTLY                   
         BZ    SETJOB1                                                          
         CLI   RECNUM,AYLINT       NOT AYLINT (DON'T NEED ESTIMATE)             
         BE    SETJOB1                                                          
         ZIC   RE,TANULEN                                                       
         SH    RE,=Y(TANULNQ+1)                                                 
         EX    RE,*+8                                                           
         B     SETJOBX                                                          
         MVC   RECEST(0),TANUMBER  MOVE EST NUMBER AS IS                        
*                                                                               
SETJOB1  OC    LENGTHS,LENGTHS     IF ESTIMATE SETUP LENGTHS                    
         BZ    SETJOB2                                                          
         LA    R3,RECCLI                                                        
         LA    R4,RECJOB                                                        
         CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BNE   *+12                                                             
         LA    R3,RECBCLI-RECBD(R5)                                             
         LA    R4,RECBJOB-RECBD(R5)                                             
*                                                                               
         LA    R2,TANUMBER                                                      
         ZIC   R1,LENGTHS          R1=LENGTH OF CLIENT                          
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,SETJOB1A                                                      
         LA    R2,1(R1,R2)                                                      
         ZIC   R1,LENGTHS+1        R1=LENGTH OF PRODUCT                         
         BCTR  R1,0                                                             
         CLI   RECNUM,AYLINT       SKIP IF AYLINT                               
         BE    *+8                                                              
         EX    R1,SETJOB1B                                                      
         LA    R2,1(R1,R2)                                                      
         ZIC   R1,LENGTHS+2        R1=LENGTH OF JOB                             
         BCTR  R1,0                                                             
         EX    R1,SETJOB1C                                                      
         B     SETJOBX                                                          
*                                                                               
SETJOB1A MVC   0(0,R3),0(R2)     MOVE CLIENT CODE                               
SETJOB1B MVC   RECPRD(0),0(R2)                                                  
SETJOB1C MVC   0(0,R4),0(R2)                                                    
*                                                                               
SETJOB2  OC    TICLI,TICLI         IF CLIENT CODE                               
         BZ    SETJOB2K                                                         
         CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BNE   *+14                                                             
         MVC   RECBCLI-RECBD(L'RECBCLI,R5),TICLI                                
         B     *+10                                                             
         MVC   RECCLI,TICLI        SET CLIENT CODE                              
*                                                                               
SETJOB2K OC    TIPRD,TIPRD         IF PRODUCT CODE                              
         BZ    SETJOB2M                                                         
         CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BE    *+10                                                             
         MVC   RECPRD,TIPRD        SET PRODUCT CODE                             
*                                                                               
SETJOB2M LA    R2,TANUMBER         R2=A(START OF JOB NUMBER)                    
         ZIC   RE,TANULEN                                                       
         LA    R4,RECCLI                                                        
         CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BNE   *+8                                                              
         LA    R4,RECBCLI-RECBD(R5)                                             
*                                                                               
         SH    RE,=Y(TANULNQ)      RE=LENGTH OF TANUMBER                        
         CH    RE,=H'8'            IF LENGTH > 8                                
         BNH   SETJOB3                                                          
         CLI   SUBSID,C'Y'         IS IT A SUBSIDIARY?                          
         BE    SETJOB3             CLIENT ALREADY SET, GO TO JOB                
         MVC   0(L'RECCLI,R4),0(R2)  ASSUME CLIENT 1ST 4 CHAR                   
         OC    0(L'RECCLI,R4),SPACES                                            
*                                                                               
         CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BE    *+16                                                             
         MVC   RECPRD,4(R2)        AND PRODUCT IN NEXT 4 CHAR                   
         OC    RECPRD,SPACES                                                    
         SH    RE,=H'8'            IGNORE FIRST 8 CHARS (CLI & PRD)             
         AH    R2,=H'8'            FOR JOB                                      
SETJOB3  MVC   WORK,SPACES                                                      
         LA    R3,WORK                                                          
SETJOB5  CLI   0(R2),C'A'          IF ALPHA NUMERIC                             
         BL    *+14                                                             
         MVC   0(1,R3),0(R2)       MOVE CHARACTER/NUMBER                        
         LA    R3,1(R3)                                                         
         LA    R2,1(R2)            BUMP TO NEXT POSITION IN TANUMBER            
         BCT   RE,SETJOB5          LOOP                                         
         LA    R4,RECJOB                                                        
         CLI   RECNUM,AYLINT       ONLY IF AYLINT                               
         BNE   *+8                                                              
         LA    R4,RECBJOB-RECBD(R5)                                             
         MVC   0(L'RECJOB,R4),WORK         SET JOB CODE                         
*                                                                               
SETJOBX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R1                                                               
         EJECT                                                                  
*=====================================================================          
*              ROUTINE SETS USE NAME AND DETAILS                                
*              R4=A(TAPD EL)                                                    
*=====================================================================          
USEDET   NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,AYLINT       IF AYLINTER                                  
         BNE   *+14                                                             
         MVC   RECBUSE-RECBD(L'RECBUSE,R5),TAPDUSE                              
         B     USEDX                                                            
*                                                                               
         MVC   RECUSE,TGUSNAME                                                  
         OC    TAPDAREA(6),TAPDAREA   IF THERE'S PRINT AREA AND USE             
         BZ    USED2                                                            
         MVC   RECUSDET(3),TAPDAREA   DISPLAY IT                                
         MVC   RECUSDET+4(3),TAPDPUSE                                           
         B     USEDX                                                            
USED2    TM    TGUSTYST,USES                                                    
         BO    USED10                       BRANCH IF USES REQUIRED             
         TM    TGUSTYST,MAJORS                                                  
         BO    USED3                        BRANCH IF MAJORS REQUIRED           
         TM    TGUSTYST,UNITS                                                   
         BO    USED5                        BRANCH IF UNITS REQUIRED            
         TM    TGUSTYST,INSERTS                                                 
         BO    USED7                        BRANCH IF INSERTS REQUIRED          
         CLI   TGUSEQU,UTAG                                                     
         BE    USED8                        BRANCH IF TAG PAYMENT               
         CLI   TGUSEQU,UDEM                                                     
         BE    USED9                        BRANCH IF DEMO PAYMENT              
         CLI   TGUSEQU,USNA                                                     
         BE    USED9                        BRANCH IF SPANISH DEMO              
         CLI   TGUSEQU,UCDM                                                     
         BE    USED9                        BRANCH IF CANADIAN DEMO             
         B     USEDX                       ELSE DONE                            
*                                                                               
USED3    GOTO1 MAJVAL,DMCB,(X'80',TAPDMAJ) VALIDATE MAJORS                      
         BNE   *+10                                                             
         MVC   RECUSDET(L'TGMACHAR),TGMACHAR                                    
USED5    OC    TAPDUNIT,TAPDUNIT         IF THERE ARE UNITS                     
         BZ    USEDX                                                            
         LA    RF,RECUSDET+L'TGMACHAR-1  FIND END OF MAJORS                     
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         EDIT  TAPDUNIT,(5,2(RF)),ALIGN=LEFT NUMBER OF UNITS                    
         B     USEDX                                                            
*                                                                               
USED7    OC    TAPDINS,TAPDINS     IF THERE ARE INSERTS                         
         BZ    USEDX                                                            
         EDIT  TAPDINS,(5,RECUSDET),ALIGN=LEFT NUMBER OF INSERTS                
         B     USEDX                                                            
*                                                                               
USED8    OC    TAPDTAGS,TAPDTAGS   IF THERE ARE TAGS                            
         BZ    USEDX                                                            
         EDIT  TAPDTAGS,(3,RECUSDET),ALIGN=LEFT NUMBER OF TAGS                  
         B     USEDX                                                            
*                                                                               
USED9    OC    TAPDDEMS,TAPDDEMS   IF THERE ARE DEMOS                           
         BZ    USEDX                                                            
         EDIT  TAPDDEMS,(3,RECUSDET),ALIGN=LEFT NUMBER OF DEMOS                 
         B     USEDX                                                            
*                                                                               
         USING TANDD,R4                                                         
USED10   MVI   ELCODE,TANDELQ      GET NETWORK/CLASS A DETAILS ELEMENT          
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   USEDX                                                            
         SR    R2,R2                                                            
         ICM   R2,3,TANDSTUS       FIND TOTAL START USE NUMBER                  
         BNZ   *+8                                                              
         AH    R2,=H'1'            + 1 IF 0 FROM CONVERSION                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,TANDSTUL                                                    
         BNZ   *+8                                                              
         AH    R1,=H'1'                                                         
         AR    R2,R1                                                            
         BCTR  R2,0                R2=TOTAL START USE NUMBER                    
         EDIT  (R2),(3,RECUSDET),ALIGN=LEFT                                     
         LA    RF,RECUSDET+2       FIND END OF START USE NUM                    
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         AH    R2,TANDUSES         FIND TOTAL END USE NUMBER                    
         AH    R2,TANDUSEL                                                      
         BCTR  R2,0                                                             
         EDIT  (R2),(3,2(RF)),ALIGN=LEFT                                        
*                                                                               
         OC    TANDUSEL,TANDUSEL   IF THERE ARE LIFT USES                       
         BZ    USEDX                                                            
         LA    RF,RECUSDET+6       FIND END OF END USE NUM                      
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   2(RF),C'L'          SHOW LIFT USES                               
         EDIT  (2,TANDSTUL),(3,3(RF)),ALIGN=LEFT LIFT START USENUM              
         LA    RF,RECUSDET+11      FIND END OF LIFT START USE NUMBER            
         CLI   0(RF),X'40'                                                      
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         LH    R2,TANDSTUL         FIND LIFT END USE NUMBER                     
         AH    R2,TANDUSEL                                                      
         BCTR  R2,0                                                             
         EDIT  (R2),(3,2(RF)),ALIGN=LEFT                                        
*                                                                               
USEDX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*                                                                               
TUD      DSECT                                                                  
         DS    0A                                                               
AMASTD   DS    A                   A(MASTER)                                    
ALOGOC   DS    A                   A(LOGOC)                                     
ALOGO    DS    A                   A(LOGO)                                      
AREMOT   DS    A                   A(REMOTE)                                    
APRNTBL  DS    A                   A(PRNTBL)                                    
APRNT    DS    A                   A(PRNT)                                      
MYSORTER DS    A                                                                
*                                                                               
TUOPTS   DS    XL1                 OPTIONS                                      
TUNOTAPE EQU   X'80'               DON'T GENERATE TAPE                          
TUWCODE  EQU   X'40'               TAPE BY WORK CODE                            
TUEXACT  EQU   X'20'               EXACT ESTIMATE NUMBER                        
TUGREY   EQU   X'10'               DO SPECIAL STUFF FOR GREY                    
TUAGY    EQU   X'08'               INCLUDE AGENCY                               
TUREGA   EQU   X'04'               REGULAR NUMBERS                              
*                                                                               
ACTVSW   DS    CL1                 ACTIVITY INDICATOR                           
REREADSW DS    CL1                 REREAD SYSIOS KEY                            
TUPER    DS    CL17                PRINTABLE PERIOD                             
*                                                                               
OVRVEND  DS    CL11                VENDOR OVERRIDE                              
SVAGYCO  DS    CL4                 SAVED AGENCY CO=                             
LENGTHS  DS    CL3                 AGENCY ESTIMATE SETUP LENGTHS                
SVAGY    DS    CL6                 SAVED AGENCY                                 
SVCLI    DS    CL6                 SAVED CLIENT                                 
AMOUNT   DS    PL4                                                              
COUNT    DS    PL4                 INVOICE COUNTER                              
HDRCOUNT DS    PL4                 HEADER COUNT FOR WCODEOPTION                 
DTLCOUNT DS    PL4                 DETAIL COUNT FOR WCODE OPTION                
*                                  COUNTER ORDER SAME AS TAPE                   
FILESTAT DS    XL1                 FILE STATUS                                  
FILEOPEN EQU   X'80'               FILE IS OPEN                                 
*                                                                               
NUMCNTRS EQU   14                  TOTAL NUMBER OF COUNTERS                     
*NUMCNTRS EQU   15                  TOTAL NUMBER OF COUNTERS                    
NUMINV   EQU   5                   NUMBER OF INVOICE AMOUNTS                    
*NUMINV  EQU   6                   NUMBER OF INVOICE AMOUNTS                    
CNTRS    DS    0F                                                               
CNTINV   DS    F                   INVOICE TOTAL                                
CNTSINV  DS    0F                  SUB-INVOICE TOTAL                            
NUMSINV  EQU   4                   NUMBER OF SUB-INVOICE AMOUNTS                
*NUMSINV EQU   5                   NUMBER OF SUB-INVOICE AMOUNTS                
CNTTAX   DS    F                   PAYROLL TAX                                  
CNTPNH   DS    F                   P AND H                                      
CNTHAND  DS    F                   HANDLING                                     
CNTGST   DS    F                   GST/COMMERCIAL SERVICE FEE                   
**CNTACOM DS    F                   AGENCY COMMISSION                           
*                                                                               
NUMFEES  EQU   9                   NUMBER OF CHECK AMOUNTS                      
CNTRFEES DS    0F                  AMOUNT TOTALS                                
CNTMUS   DS    F                   MUSICIANS AMOUNT                             
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
*                                                                               
TOTCNTRS DS    0F                  TOTAL COUNTERS                               
TOTINV   DS    F                   INVOICE TOTAL                                
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
AGYCOM   DS    F                   AGENCY COMMISSIONABLE AMOUNT                 
AGYNCOM  DS    F                   AGENCY NON-COMMISSIONABLE AMOUNT             
AGYINV   DS    F                   AGENCY INVOICE TOTAL                         
*                                                                               
DLBLOCK  DS    CL(DLCBXLX)                                                      
MAXTBLS  EQU   40                                                               
AGYTAB   DS    (MAXTBLS)CL(AGYTABL) AGENCY TABLE                                
*                                                                               
SORTINIT DS    C                                                                
SORTIO   DS    0CL(RECLNQ2+L'SORTKEY)                                           
SORTKEY  DS    CL44                SORT SPACE FOR AYLINTER                      
TAPEREC  DS    CL(RECLNQ2)         TAPE RECORD AREA                             
TOTLREC  DS    CL140               TOTAL RECORD FOR AYLINT                      
HDRREC   DS    CL(RECHLNQ)         TAPE HEADER RECORD AREA (WCODE)              
DTLREC   DS    CL(RECDLNQ)         TAPE DETAIL RECORD AREA (WCODE)              
*                                                                               
SUBSID   DS    CL1                 SUBSIDIARY INVOICE                           
*                                                                               
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
*              DSECT TO COVER FILE RECORD                                       
*                                                                               
RECD     DSECT                                                                  
RECID    DS    CL11                TAPE ID                                      
RECCNUM  DS    CL2                 COMPANY NUMBER                               
RECOFCD  DS    CL2                 OFFICE CODE                                  
RECINVN  DS    CL15                INVOICE NUMBER (LEFT JUSTIFIED)              
RECDATE  DS    CL6                 INVOICE DATE                                 
RECPO    DS    CL9                 PURCHASE ORDER NUM(AUTH/PO) RIGHT            
RECCLI   DS    CL4                 CLIENT                                       
RECPRD   DS    CL4                 PRODUCT                                      
RECJOB   DS    CL7                 JOB CODE                                     
         ORG   RECCLI                                                           
RECEST   DS    CL15                ESTIMATE NUMBER                              
*                                                                               
RECSOR   DS    CL1                 SESSION OR REUSE                             
RECTODAY DS    CL6                 TODAYS DATE                                  
RECINV   DS    CL11                INVOICE AMOUNT                               
RECTAX   DS    CL11                PAYROLL TAXES AMOUNT                         
RECPNH   DS    CL11                P & H CONTRIBUTIONS AMOUNT                   
RECHND   DS    CL11                HANDLING AMOUNT                              
RECGST   DS    CL11                GOODS/SERVICES TAX AMOUNT                    
RECMUS   DS    CL11                MUSICIANS AMOUNT                             
RECSING  DS    CL11                SINGERS AMOUNT                               
RECON    DS    CL11                ONCAMERA AMOUNT                              
RECXTRA  DS    CL11                EXTRAS AMOUNT                                
RECOFF   DS    CL11                OFF CAMERA (VOICE OVER) AMOUNT               
RECMODEL DS    CL11                HAND MODEL AMOUNT                            
RECCART  DS    CL11                CARTAGE AMOUNT                               
RECWRD   DS    CL11                WARDROBE AMOUNT                              
RECMISC  DS    CL11                MISCELLANEOUS AMOUNT                         
RECLNQ   EQU   *-RECD                                                           
RECAYL   DS    0C                  INFO FOR AYLINTER REPORT (ONLY)              
*                                                                               
RECAGY   DS    CL6                 AGENCY                                       
RECTPCLI DS    CL6                 TP CLIENT CODE                               
         DS    CL92                SPARE                                        
*                                                                               
         ORG   RECAYL              INFO FOR AYLINTER REPORT (LEO BRNTT)         
RECCID   DS    CL12                COMMERCIAL ID                                
RECUSE   DS    CL16                USE NAME AND TYPE                            
RECUSDET DS    CL16                USE DETAILS                                  
RECCMNT  DS    CL60                INVOICE COMMENT (GENERAL)                    
RECAYLLN EQU   *-RECAYL                                                         
RECLNQ2  EQU   *-RECD                                                           
         EJECT                                                                  
*              DSECT TO COVER HEADER RECORD (WCODE OPTION)                      
         SPACE                                                                  
RECHD    DSECT                                                                  
RECHCNUM DS    CL2                 COMPANY NUMBER                               
RECHOFCD DS    CL2                 OFFICE NUMBER                                
RECHID   DS    CL11                'TALENTPART '                                
RECH1    DS    CL1                 '1'                                          
RECHINVN DS    CL15                INVOICE NUMBER                               
RECHDATE DS    CL6                 INVOICE DATE                                 
RECHPO   DS    CL9                 P.O NUMBER                                   
RECHINV  DS    CL11                INVOICE AMOUNT                               
RECHSOR  DS    CL1                 SESSION OR RESUSE                            
RECHCLI  DS    CL10                CLIENT                                       
RECHPRD  DS    CL10                PRODUCT                                      
RECHJOB  DS    CL10                JOB CODE                                     
RECHTYP  DS    CL2                 '10'                                         
RECHLNQ  EQU   *-RECHD                                                          
         SPACE 2                                                                
*              DSECT TO COVER DETAIL RECORD (WCODE OPTION)                      
         SPACE                                                                  
RECDD    DSECT                                                                  
RECDCNUM DS    CL2                 COMPANY NUMBER                               
RECDOFCD DS    CL2                 OFFICE NUMBER                                
RECDID   DS    CL11                'TALENTPART '                                
RECD1    DS    CL1                 '1'                                          
RECDINVN DS    CL15                INVOICE NUMBER                               
RECDWKCD DS    CL10                WORK CODE                                    
RECDWKAM DS    CL11                WORK CODE AMOUNT                             
         DS    CL36                BLANK                                        
RECDTYP  DS    CL2                 '20'                                         
RECDLNQ  EQU   *-RECDD                                                          
         EJECT                                                                  
*              DSECT TO COVER BURNETT LAYOUT                                    
*                                                                               
RECBD    DSECT                                                                  
RECB     DS    0CL140                                                           
RECBVND  DS    CL11                'TALENTPART'                                 
RECBTYP  DS    CL1                 'D' / 'T'                                    
RECBAGY  DS    CL4                 AGENCY CODE                                  
*                   DETAIL RECORD                                               
RECBIDTE DS    CL6                 INVOICE DATE (YYMMDD)                        
RECBINV  DS    CL6                 INVOICE NUMBER                               
RECBPO   DS    CL9                 PURCHASE ORDER NUM(AUTH/PO) RIGHT            
RECBCLI  DS    CL4                 CLIENT                                       
RECBJOB  DS    CL7                 JOB CODE                                     
RECBCOM  DS    CL11                COMMISSIONABLE (WAGES+P&H)                   
RECBNCOM DS    CL11                NON-COMMISSIONABLE (TAX+HANDLING)            
RECBITOT DS    CL11                INVOICE TOTAL                                
RECBUSE  DS    CL3                 USE CODE                                     
RECBPFLG DS    CL1                 PREVIOUS BILLED FLAG                         
         DS    CL55                                                             
*                                                                               
         ORG   RECBIDTE                                                         
*                   TRAILER RECORD                                              
RECBTDTE DS    CL6                 TRANSMISSION DATE                            
RECBICNT DS    CL6                 AGENCY INVOICE COUNT                         
RECBCTOT DS    CL11                AGENCY COMM. TOTAL                           
RECBNTOT DS    CL11                AGENCY NON-COMM. TOTAL                       
RECBTOTL DS    CL11                AGENCY INVOICE TOTAL                         
         DS    CL79                                                             
RECBLNQ  EQU   *-RECBD                                                          
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
PRNTD    DSECT                                                                  
         DS    CL1                 SPACE                                        
PRTCNUM  DS    CL2                 COMPANY NUMBER                               
         DS    CL1                 SPACE                                        
PRTOFCD  DS    CL2                 OFFICE CODE                                  
         ORG   PRTCNUM                                                          
         DS    CL1                                                              
PRTAGY   DS    CL4                 AGENCY                                       
*                                                                               
         DS    CL1                 SPARE                                        
PRTDATE  DS    CL6                 INVOICE DATE                                 
         DS    CL1                 SPARE                                        
PRTINVN  DS    CL6                 INVOICE NUMBER                               
         DS    CL1                 SPARE                                        
PRTPO    DS    CL9                 PURCHASE ORDER NUMBER (AUTH/PO)              
         DS    CL1                 SPARE                                        
PRTCLI   DS    CL4                 CLIENT                                       
         DS    CL1                 SPARE                                        
PRTPRD   DS    CL4                 PRODUCT                                      
         DS    CL1                 SPARE                                        
PRTJOB   DS    CL7                 JOB CODE                                     
         ORG   PRTCLI                                                           
PRTEST   DS    CL15                OR ESTIMATE NUMBER                           
         DS    CL2                                                              
         ORG                                                                    
         DS    CL1                 SPARE                                        
PRTTYPE  DS    CL2                 SESSION OR REUSE                             
         DS    CL1                 SPARE                                        
PRTFEES  DS    CL12                FEES (CHECK TOTALS)                          
         DS    CL1                 SPARE                                        
PRTPNH   DS    CL12                P & H CONTRIBUTIONS AMOUNT                   
         DS    CL1                 SPARE                                        
PRTTAX   DS    CL12                PAYROLL TAXES AMOUNT                         
         DS    CL1                 SPARE                                        
PRTHND   DS    CL12                HANDLING AMOUNT                              
         DS    CL1                 SPARE                                        
PRTGST   DS    CL12                GOODS/SERVICES TAX AMOUNT                    
         DS    CL1                 SPARE                                        
PRTINV   DS    CL12                INVOICE AMOUNT                               
         DS    CL1                 SPARE                                        
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
**PAN#1  DC    CL21'083TAREP40   02/22/10'                                      
         END                                                                    
