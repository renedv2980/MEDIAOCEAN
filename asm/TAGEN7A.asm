*          DATA SET TAGEN7A    AT LEVEL 007 AS OF 07/20/12                      
*PHASE T7027AC                                                                  
         TITLE 'T7027A - ADVICE/COMPLETE'                                       
T7027A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7027A                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         BAS   RE,PFHANDLE                                                      
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   XIT                                                              
         BAS   RE,VK                                                            
         BAS   RE,DR                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              INITIALIZE AND HANDLE PF KEYS                                    
*                                                                               
PFHANDLE NTR1                                                                   
         USING TWAHOLED,RE                                                      
         LA    RE,TWAHOLE                                                       
         MVI   SVRECUSE,C'O'                                                    
         MVI   PFCT,C'I'                                                        
         MVC   PFDR,DELREAS                                                     
         GOTO1 INITIAL,DMCB,PFTABLE          INITIALIZE                         
         DROP  RE                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              VALIDATE KEY                                                     
*                                                                               
VK       NTR1                                                                   
         CLI   ACMAGYH+5,0                                                      
         BNE   VK10                                                             
         LA    R2,ACMAGYH                                                       
         B     ERRMIS                                                           
         SPACE 1                                                                
VK10     MVC   TGAGY,ACMAGY                                                     
         OC    TGAGY,SPACES                                                     
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',ACMAGYH)                              
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
         CLI   ACMCIDH+5,0                                                      
         BNE   VK20                                                             
         LA    R2,ACMCIDH                                                       
         B     ERRMIS                                                           
         SPACE 1                                                                
VK20     MVC   TGCID,ACMCID                                                     
         OC    TGCID,SPACES                                                     
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLDVCDQ,(X'20',ACMADVH)                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              DISPLAY ADVICE                                                   
*                                                                               
DR       NTR1                                                                   
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         GOTO1 FLDVAL,DMCB,(X'03',ACMVERH),ACMVEDH                              
         SPACE 1                                                                
         MVI   TGVER,0                                                          
         SPACE 1                                                                
         USING TAFND,R4                                                         
         L     R4,AIO1             R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BNE   DR30                                                             
         L     R4,TGELEM                                                        
         MVC   TGVER,TAFNNAME                                                   
         MVC   ACMVER(1),TAFNNAME  MOVE VERSION LETTER TO SCREEN                
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    DR05                                                             
         EDIT  (1,TAFNNAME),ACMVER,ALIGN=LEFT                                   
         DROP  R4                                                               
         SPACE 1                                                                
DR05     L     R4,AIO2             R4=A(COMMERCIAL RECORD)                      
         SPACE 1                                                                
         TM    TGSYSTAT,TASYS3VR   IF SYSTEM SET TO HANDLE 3-CHARACTER          
         BZ    DR05C               VERSION CODES                                
         CLI   TGVER,26            AND VERSION CODE IS GREATER THAN             
         BNH   DR05C               26                                           
         SPACE                                                                  
         USING VINDEXD,RE                                                       
         LA    RE,VERINDEX         FIND RECORD EQUATE FOR THIS                  
DR05A    CLI   0(RE),X'FF'         VERSION NUMBER                               
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TGVER,VINDUPLM                                                   
         BNH   DR05B                                                            
         LA    RE,VINDLNQ(RE)                                                   
         B     DR05A                                                            
         SPACE 1                                                                
DR05B    XC    KEY,KEY              GET COMMERCIAL RECORD FOR                   
         MVC   KEY(L'TLCOKEY),0(R4) THAT VERSION                                
         MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),VINDEQUT                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   DR30                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         DROP  RE                                                               
         SPACE                                                                  
         USING TAVRD,R4                                                         
DR05C    MVI   ELCODE,TAVRELQ                                                   
         BAS   RE,GETEL                                                         
         B     DR20                                                             
DR10     BAS   RE,NEXTEL                                                        
DR20     BNE   DR30                                                             
         CLC   TAVRVERS,TGVER                                                   
         BNE   DR10                                                             
         MVC   ACMVED,TAVRCID      MOVE VERSION CID TO SCREEN                   
         DROP  R4                                                               
         SPACE 1                                                                
DR30     CLC   AIO,AIO2            IF READ SEQUENCED COMMERCIAL                 
         BNE   DR30A               FOR VERSION INFO, RESET IO AREAS             
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'24',ACMCIDH)                             
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
DR30A    GOTO1 FLDVAL,DMCB,(X'03',ACMCLINH),1                                   
         GOTO1 (RF),(R1),(X'03',ACMPRDNH),1                                     
         SPACE 1                                                                
         GOTO1 CHAROUT,DMCB,TAFNELQ,ACMCLINH,TAFNTCLI                           
         GOTO1 (RF),(R1),TAFNELQ,ACMPRDNH,TAFNTPRD                              
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'03',ACMFRSTH),(X'40',ACMLSTH)                     
         SPACE 1                                                                
         MVI   COMPSTAT,1          INITIALIZE THE COMPLETED STATUS              
         SPACE 1                                                                
         USING LINED,R2                                                         
         LA    R2,ACMFRSTH                                                      
         MVC   LINEDET(L'ASESSION),ASESSION                                     
         SPACE 1                                                                
         USING TADVD,R4                                                         
         L     R4,AIO1             IF ADVICE IS A SESSION, CAN'T                
         MVI   ELCODE,TADVELQ      COMPLETE IT                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   TADVTYPE,C'S'                                                    
         BE    DR160                                                            
         SPACE 1                                                                
         MVC   LINEDET(L'NOTRECVD),NOTRECVD                                     
         TM    TADVSTAT,TADVSRCV                                                
         BZ    DR35                                                             
         MVC   LINEDET(L'PREDATE),PREDATE                                       
         MVI   COMPSTAT,0                                                       
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAAID,R4                                                         
DR35     L     R4,AIO1             R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TAAIELQ                                                   
         BAS   RE,GETEL            GET INVOICE/ADVICE ASSIGN ELEMENT            
         BNE   DR160                                                            
         SPACE 1                                                                
         MVC   LINEDET(L'PREDATE),SPACES                                        
         DROP  R2                                                               
         SPACE 1                                                                
         LA    R3,TAAIINUM         IF REUSE INVOICE EXISTS                      
         OC    TAAIINUM,TAAIINUM   R3=(FIRST REUSE INVOICE)                     
         BNZ   DR40                ELSE, R3=A(MUSIC INVOICE)                    
         LA    R3,TAAIINU2                                                      
         SPACE 1                                                                
DR40     PACK  DUB,2(4,R3)                                                      
         CVB   R1,DUB                                                           
         ST    R1,INV1             INV1=FIRST INVOICE ON ADVICE                 
         MVC   INVHEAD,0(R3)                                                    
         SPACE 1                                                                
         LA    R3,TAAIINU2         IF MUSIC INVOICE                             
         OC    TAAIINU2,TAAIINU2   R3=A(MUSIC INVOICE)                          
         BNZ   DR60                                                             
         SPACE 1                                                                
         CLI   TAAILEN,TAAIILNQ    IF NOT A MUSIC INVOICE                       
         BE    DR50                BUT MULTIPLE REUSE INVOICES                  
         LA    R3,TAAIINUX         R3=A(LAST REUSE INVOICE)                     
         OC    TAAIINUX,TAAIINUX                                                
         BNZ   DR60                                                             
         SPACE 1                                                                
DR50     LA    R3,TAAIINUM         ELSE, R3=A(SOLE REUSE INVOICE)               
         SPACE 1                                                                
DR60     PACK  DUB,2(4,R3)                                                      
         CVB   R1,DUB                                                           
         ST    R1,INV2             INV2=LAST INVOICE ON ADVICE                  
         SPACE 1                                                                
         USING LINED,R2                                                         
         LA    R2,ACMFRSTH                                                      
         MVC   INV,INV1                                                         
         MVC   AIO,AIO3                                                         
         SPACE 1                                                                
DR70     CLC   INV,INV2            PRINT OUT EACH INVOICE                       
         BH    DR160                                                            
         MVC   LINEINV(L'INVHEAD),INVHEAD                                       
         EDIT  INV,(4,LINEINV+2),ALIGN=RIGHT                                    
         OC    LINEINV+2(4),=4X'F0'                                             
         MVI   LINEINVH+5,6                                                     
         SPACE 1                                                                
         ZIC   RE,COMPSTAT                                                      
         AHI   RE,1                                                             
         STC   RE,COMPSTAT                                                      
         SPACE 1                                                                
         MVC   LDDELETD,DELETED        TRY TO GET INVOICE RECORD                
         GOTO1 TINVCON,DMCB,LINEINV,CNVINV,DATCON                               
         CLI   0(R1),X'FF'                                                      
         BE    DR130                                                            
         XC    CNVINV,=6X'FF'                                                   
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',CNVINV)                               
         BE    DR100                                                            
         SPACE 1                                                                
         CLC   KEY(L'TLINKEY),KEYSAVE  IF NOT FOUND                             
         BNE   DR130                                                            
         TM    KEY+L'TLINKEY,TLINSDEL  AND IN PSEUDO-DELETE STATUS              
         BNO   DR130                                                            
         GOTO1 GETREC                  WE CAN GET DELETED INFO                  
         SPACE 1                                                                
         USING TAACD,R4                                                         
         L     R4,AIO3                 GET ACTIVITY ELEMENT                     
         MVI   ELCODE,TAACELQ          FROM DELETE SCREEN                       
         BAS   RE,GETEL                                                         
         B     DR90                                                             
DR80     BAS   RE,NEXTEL                                                        
DR90     BNE   DR130                                                            
         CLI   TAACSCR,X'1F'           AND COPY INFORMATION                     
         BNE   DR80                    TO SCREEN                                
         MVC   LDDELDON,DELETEON                                                
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,LDDELDDT)                            
         MVC   LDDELDBY,PAIDBY                                                  
         MVC   LDDELDST,TAACSTAF                                                
         B     DR130                                                            
         SPACE 1                                                                
         USING TAIND,R4                                                         
DR100    L     R4,AIO3                                                          
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR130                                                            
         MVC   LDNTPDYT,NTPAIDYT                                                
         OC    TAINPINF,TAINPINF                                                
         BZ    DR130                                                            
         MVC   LDPAIDON,PAIDON                                                  
         GOTO1 DATCON,DMCB,(1,TAINPDTE),(8,LDPAIDDT)                            
         MVC   LDPAIDBY,PAIDBY                                                  
         MVC   LDPAIDST,TAINPST                                                 
         MVC   LDNTBLLD,NTBILLYT                                                
         OC    TAINBDTE,TAINBDTE                                                
         BZ    DR110                                                            
         MVC   LDBILDON,BILLEDON                                                
         GOTO1 DATCON,DMCB,(1,TAINBDTE),(8,LDBILDDT)                            
         DROP  R4                                                               
         SPACE 1                                                                
DR110    MVC   LINEUSE,SPACES                                                   
         SPACE 1                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO3             DISPLAY USE CODE                             
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR130                                                            
         MVC   LINEUSE(3),TAPDUSE                                               
         SPACE 1                                                                
         MVC   TGMACHAR,SPACES     DISPLAY MAJORS                               
         GOTO1 MAJVAL,DMCB,(X'80',TAPDMAJ)                                      
         MVC   LINEUSE+4(L'TGMACHAR),TGMACHAR                                   
         SPACE 1                                                                
         OC    TAPDUNIT,TAPDUNIT   IF UNITS PRESENT                             
         BZ    DR120                                                            
         LA    R3,LINEUSE+4                                                     
DR115    CLI   0(R3),C' '          FIND SPOT TO DISPLAY UNITS                   
         BE    DR116                                                            
         LA    R3,1(R3)                                                         
         B     DR115               AND DISPLAY THEM                             
         DC    H'00'                                                            
DR116    EDIT  TAPDUNIT,(4,1(R3)),ALIGN=LEFT                                    
         SPACE 1                                                                
DR120    OC    TAPDSTUS,TAPDSTUS                                                
         BZ    DR130                                                            
         LA    R3,LINEUSE+4                                                     
         EDIT  TAPDSTUS,(3,0(R3)),ALIGN=LEFT                                    
         AR    R3,R0                                                            
         MVI   0(R3),C'-'                                                       
         LH    R1,TAPDSTUS                                                      
         SHI   R1,1                                                             
         AH    R1,TAPDUSES                                                      
         EDIT  (R1),(3,1(R3)),ALIGN=LEFT                                        
         DROP  R4                                                               
         SPACE 1                                                                
DR130    CLC   LDDELETD,DELETED    IF INVOICE IS DELETED                        
         BE    DR140                                                            
         CLC   LDPAIDON,PAIDON     OR PAID                                      
         BNE   DR150                                                            
DR140    ZIC   RE,COMPSTAT         THEN IT WON'T HOLD ADVICE                    
         AHI   RE,-1               UP FROM BEING COMPLETED                      
         STC   RE,COMPSTAT                                                      
         SPACE 1                                                                
DR150    L     RE,INV                                                           
         AHI   RE,1                                                             
         ST    RE,INV                                                           
         SPACE 1                                                                
         LA    R2,LINELNQ(R2)                                                   
         B     DR70                                                             
         SPACE 1                                                                
         SPACE 1                                                                
DR160    MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         CLI   PFAID,14            IF TRYING TO COMPLETE ADVICE                 
         BNE   DR180                                                            
         LA    R2,ACMADVH          ALL INVOICES MUST BE DELETED                 
         CLI   COMPSTAT,0          OR BILLED OR PREDATE THIS PROCESS            
         BNE   ERRNCOMP                                                         
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLDVCDQ,(X'30',ACMADVH)                              
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         SPACE 1                                                                
         USING TADVD,R4                                                         
         L     R4,AIO1             GET ADVICE DETAILS ELEMENT                   
         MVI   ELCODE,TADVELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         TM    TADVSTAT,TADVSCMP   CANNOT COMPLETE IF ALREADY                   
         BO    ERRACOMP            COMPLETED                                    
         OI    TADVSTAT,TADVSCMP                                                
         DROP  R4                                                               
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,(X'80',0) SAVE STAFF MEMBER                          
         SPACE 1                                                                
         GOTO1 PUTREC              AND PUT THE RECORD                           
         GOTO1 ADDPTRS,DMCB,PTRBLK                                              
         SPACE 1                                                                
         USING TAACD,R4                                                         
DR180    MVC   ACMCOMP,NOTCOMPL    INDICATE WHETHER ADVICE HAS                  
         OI    ACMCOMPH+6,X'80'    ALREADY BEEN COMPLETED OR NOT                
         L     R4,AIO                                                           
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR190    BAS   RE,NEXTEL                                                        
         BNE   DR200                                                            
         CLI   TAACSCR,X'7A'                                                    
         BNE   DR190                                                            
         MVC   ACMCOMP(L'COMPLETE),COMPLETE                                     
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,ACMCOMP+L'COMPLETE)                  
         MVC   ACMCOMP+L'COMPLETE+8(L'PAIDBY),PAIDBY                            
         MVC   ACMCOMP+L'COMPLETE+L'PAIDBY+8(8),TAACSTAF                        
         SPACE 1                                                                
DR200    CLI   PFAID,14            IF ADVICE JUST COMPLETED                     
         BE    DRX                 WE'RE DONE                                   
         SPACE 1                                                                
         USING TADVD,R4                                                         
         L     R4,AIO              IF ADVICE NOT YET COMPLETED,                 
         MVI   ELCODE,TADVELQ      GIVE PROMPT TO HIT PF14                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         TM    TADVSTAT,TADVSCMP                                                
         BZ    HITPF14                                                          
DRX      B     XIT                                                              
         EJECT                                                                  
         SPACE 2                                                                
ERRINV   MVI   ERROR,INVALID                                                    
         B     MESSEND                                                          
         SPACE 2                                                                
ERRMIS   MVI   ERROR,MISSING                                                    
         B     MESSEND                                                          
         SPACE 2                                                                
ERRNCOMP MVC   MYMSGNO,=Y(ERADVCMN)                                             
         B     EMESSEND                                                         
         SPACE 2                                                                
ERRACOMP MVC   MYMSGNO,=Y(ERADVCMA)                                             
         B     EMESSEND                                                         
         SPACE 2                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)                                             
         B     EMESSEND                                                         
         SPACE 2                                                                
EMESSEND OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     MESSEND                                                          
         SPACE 2                                                                
HITPF14  MVI   MYMSGNO1,249                                                     
         B     SMESSEND                                                         
         SPACE 2                                                                
SMESSEND L     R2,EFHREC                                                        
         OI    GENSTAT2,USGETTXT                                                
         B     MESSEND                                                          
         SPACE 2                                                                
MESSEND  XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              LITERALS                                                         
         SPACE 2                                                                
ASESSION DC    C'ADVICE IS A SESSION'                                           
NOTRECVD DC    C'ADVICE NOT YET RECEIVED'                                       
PREDATE  DC    C'ADVICE PREDATES COMPLETION PROCESS'                            
DELETED  DC    C'DELETED'                                                       
DELETEON DC    C'DELETED ON '                                                   
NTPAIDYT DC    C'NOT YET PAID'                                                  
PAIDON   DC    C'PAID ON '                                                      
PAIDBY   DC    C' BY '                                                          
NTBILLYT DC    C', NOT YET BILLED'                                              
BILLEDON DC    C', BILLED ON '                                                  
         SPACE 2                                                                
NOTCOMPL DC    CL(L'ACMCOMP)'ADVICE NOT YET COMPLETE'                           
COMPLETE DC    C'ADVICE COMPLETED ON '                                          
         SPACE 2                                                                
DELREAS  DC    C'DELETE REASON:'                                                
         EJECT                                                                  
*              TABLES                                                           
         SPACE 2                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'ADVICE',CL8'DISPLAY'                                  
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF14X    EQU   *                                                                
*                                                                               
         DC    AL1(PF15X-*,15,PFTINT+PFTCPROG,(PF15X-PF15)/KEYLNQ,0)            
         DC    CL3'S',CL8'HISTORY ',CL8'DISPLAY'                                
PF15     DC    AL1(KEYTYTWA,L'ACMAGY-1),AL2(ACMAGY-T702FFD)                     
         DC    AL1(KEYTYCUR,L'LINEINV-1),AL2(0)                                 
PF15X    EQU   *                                                                
*                                                                               
         DC    AL1(PF16X-*,16,PFTINT+PFTCPROG,(PF16X-PF16)/KEYLNQ,0)            
         DC    CL3'DE',CL8'COMMENT ',CL8'ADD    '                               
PF16     DC    AL1(KEYTYTWA,L'ACMAGY-1),AL2(ACMAGY-T702FFD)                     
         DC    AL1(KEYTYTWA,L'PFCT-1),AL2(PFCT-T702FFD)                         
         DC    AL1(KEYTYCUR,L'LINEINV-1),AL2(0)                                 
         DC    AL1(0,0),AL2(0)                                                  
         DC    AL1(0,0),AL2(0)                                                  
         DC    AL1(0,0),AL2(0)                                                  
         DC    AL1(KEYTYTWA,L'PFDR-1),AL2(PFDR-T702FFD)                         
PF16X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
*              TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE                   
*              VERSION CODE IS ON                                               
         SPACE 1                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER TABLE THAT DETERMINES WHICH COMMERCIAL            
*              RECORD THE VERSION CODE IS ON                                    
         SPACE 1                                                                
VINDEXD  DSECT                                                                  
VINDUPLM DS    X                 RECORD'S UPPER LIMIT                           
VINDEQUT DS    X                 RECORD'S EQUATE                                
VINDLNQ  EQU   *-VINDEXD                                                        
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR7AD                                                       
         EJECT                                                                  
         ORG   ACMWORK           BEGINNING OF WORKING STORAGE                   
INVHEAD  DS    CL2                                                              
INV1     DS    F                                                                
INV2     DS    F                                                                
INV      DS    F                                                                
CNVINV   DS    XL6                                                              
COMPSTAT DS    X                                                                
         SPACE 2                                                                
PFCT     DS    C                                                                
PFDR     DS    CL(L'DELREAS)                                                    
         SPACE 2                                                                
PTRBLK   DS    CL((10*L'TLDRREC)+1)                                             
         SPACE 1                                                                
         DS    0D                                                               
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
TWAHOLED DSECT                                                                  
SVRECUSE DS    CL1                                                              
         EJECT                                                                  
LINED    DSECT                                                                  
LINESELH DS    XL8                                                              
LINESEL  DS    CL2                                                              
LINEINVH DS    XL8                                                              
LINEINV  DS    CL6                                                              
LINEUSEH DS    XL8                                                              
LINEUSE  DS    CL18                                                             
LINEDETH DS    XL8                                                              
LINEDET  DS    CL48                                                             
LINELNQ  EQU   *-LINED                                                          
         SPACE 2                                                                
         ORG   LINEDET                                                          
LDDELETD DS    CL7               DELETED                                        
         ORG   LINEDET                                                          
LDDELDON DS    CL11              DELETED ON                                     
LDDELDDT DS    CL8               DELETED DATE                                   
LDDELDBY DS    CL4               BY                                             
LDDELDST DS    CL8               STAFF MEMBER                                   
         ORG   LINEDET                                                          
LDNTPDYT DS    CL12              NOT PAID YET                                   
         ORG   LINEDET                                                          
LDPAIDON DS    CL8               PAID ON                                        
LDPAIDDT DS    CL8               PAID DATE                                      
LDPAIDBY DS    CL4               BY                                             
LDPAIDST DS    CL8               STAFF MEMBER                                   
LDBILDON DS    CL12              BILLED ON                                      
LDBILDDT DS    CL8               BILLED DATE                                    
         ORG   LDBILDON                                                         
LDNTBLLD DS    CL16              NOT BILLED YET                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007TAGEN7A   07/20/12'                                      
         END                                                                    
