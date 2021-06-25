*          DATA SET TAREP41    AT LEVEL 042 AS OF 10/04/16                      
*PHASE T70341B,*                                                                
*                                                                               
         TITLE 'T70341 - NETWORK/TALENT INTERFACE REPORT'                       
T70341   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70341,R7,R5                                                   
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         LA    RA,BUFF             RA=A(LOCAL W/S)                              
         LA    RA,8(RA)                                                         
         USING TNID,RA                                                          
         EJECT                                                                  
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'  GET ADDRESS OF TRPACK                    
         GOTO1 CALLOV,DMCB                                                      
         MVC   ATRPACK,0(R1)                                                    
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALKEY         IF MODE TO VALIDATE                          
         BNE   *+12                                                             
         BAS   RE,VKEY             VALIDATE IT                                  
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       IF MODE TO PRINT REPORT                      
         BNE   XIT                                                              
         BAS   RE,INIT             INITIALIZE                                   
         BAS   RE,PREP             PROCESS THE REPORT                           
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE THE KEY                                                 
VKEY     NTR1                                                                   
         L     R6,ATWA             R6=A(SCREEN)                                 
         USING T703FFD,R6                                                       
*                                                                               
         LA    R2,SNXAGYH          R2=A(AGENCY FIELD)                           
         CLI   5(R2),0             IF INPUT AND                                 
         BE    ERRMISS                                                          
         XC    PROCAGY,PROCAGY                                                  
         CLC   =C'ALL',8(R2)       IF SPECIFIC AGENCY                           
         BE    VK20                                                             
         GOTO1 USERVAL,DMCB,(X'40',SNXAGYH),SNXAGYNH                            
         MVC   PROCAGY,TGUSER      SET AGENCY ID NUMBER                         
*                                                                               
         LA    R2,SNXSEQH          R2=A(SEQUENCE NUMBER)                        
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   WORK(4),=4X'F0'     INSURE VALID NUMERIC                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=4X'F0'                                                  
         BNE   INVERR                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         RETURN AMOUNT IN SEQNO                       
         CVB   R1,DUB                                                           
         STH   R1,SEQNO                                                         
*                                                                               
VK20     BAS   RE,VALOPT           VALIDATE OPTIONS                             
*                                                                               
         MVI   MAINRUN,C'N'                                                     
         OC    PROCAGY,PROCAGY     IF NO WORKER FILE ID SET                     
         BNZ   VKX                                                              
         OC    TALAGY,TALAGY       AND NO TALENT AGENCY SET                     
         BNZ   VKX                                                              
         MVI   MAINRUN,C'Y'        THEN MAIN RUN                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE                                                                  
VALOPT   NTR1                                                                   
         LA    R2,SNXOPTH          R2=A(OPTIONS)                                
         CLI   5(R2),0                                                          
         BE    VALOPTX                                                          
*                                                                               
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
*                                                                               
VALOPT10 CLC   =C'TALAGY',SCDATA1  TALENT AGENCY                                
         BNE   VALOPT25                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A8',SCDATA2),0                            
         BNE   INVERR                                                           
         MVC   TALAGY,TGAGY                                                     
         B     VALOPT50                                                         
*                                                                               
VALOPT25 CLC   =C'BOX',SCDATA1     BOX                                          
         BNE   VALOPT30                                                         
         CLI   SCDATA2,C'Y'                                                     
         BE    VALOPT40                                                         
         CLI   SCDATA2,C'N'                                                     
         BNE   INVERR                                                           
         OI    NOPTION,NOBOX       NO BOXES                                     
         B     VALOPT50                                                         
*                                                                               
VALOPT30 CLC   =C'TRACE',SCDATA1   TRACE                                        
         BNE   INVERR                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VALOPT40                                                         
         CLI   SCDATA2,C'N'                                                     
         BE    VALOPT50                                                         
         B     INVERR                                                           
VALOPT40 OI    NOPTION,NTRACE      SET TRACE ON                                 
*                                                                               
VALOPT50 LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VALOPT10         AND CONTINUE                                 
*                                                                               
VALOPTX  B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
*              ROUTINE TO INITIALIZE                                            
         SPACE                                                                  
INIT     NTR1                                                                   
         L     R6,ATWA             R6=A(SCREEN)                                 
         USING T703FFD,R6                                                       
         SPACE 1                                                                
         LR    RE,R6               SAVE ADDRESS OF BLOCK                        
         A     RE,=A(6144)         AT 6144 PAST BEGINNING OF TWA                
         ST    RE,ASPBLK                                                        
         SPACE 1                                                                
         L     R0,=A(NHOLDLNQ)                                                  
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
         ST    R1,ANEWHOLD         SET A(NETWORK XFER RECORD IOAREA)            
         SPACE 1                                                                
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK         SET A(HEADHOOK)                              
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS            SET A(SPECS)                                 
         SPACE 1                                                                
         L     R2,TWADCONS         GET ADDRESS OF PRNTBL                        
         USING TWADCOND,R2                                                      
         MVC   PRNTBL,TPRNTBL                                                   
         MVC   NLOGOC,TLOGOC       GET A(LOGOC)                                 
         SPACE 1                                                                
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         CLI   MCPOSTNG,C'N'       IF POSTING = N IN MASTER                     
         BNE   *+8                                                              
         OI    NOPTION,NOPOST      THEN SET POST = N                            
         CLI   MCRERUN,C'Y'        IF RERUN=YES IN MASTER                       
         BNE   INIT20                                                           
         OI    NOPTION,NRERUN      SET RERUN INDICATOR                          
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   RERUNDY,DUB         SET RERUN DAY                                
         SPACE 1                                                                
INIT20   MVC   NLOGO,MCVLOGO       GET A(LOGO)                                  
         SPACE 1                                                                
         L     R4,=A(WRKBUFF)      SET A(WORKER FILE BUFFER)                    
         ST    R4,AWRKBUFF                                                      
         SPACE 1                                                                
         XC    INDEX,INDEX                                                      
         XC    WRKNM,WRKNM                                                      
         SPACE 1                                                                
         MVC   WRKFILEN,=CL8'WKFILE' SET OLD WORKER FILE FILE                   
         MVC   WRKNM(3),=C'NE1'      IF NET TO TALENT TRANSFER                  
         CLI   RECNUM,SX             SET TO READ NET WORKER FILES               
         BNE   INITX                 IF SPOT TO TALENT TRANSFER                 
         MVC   WRKNM(3),=C'STA'      SET TO READ SPOT WORKER FILES              
         SPACE 1                                                                
INITX    B     XIT                                                              
         DROP  R1,R2,R6                                                         
         EJECT                                                                  
*              PROCESS REPORT                                                   
         SPACE                                                                  
PREP     NTR1                                                                   
         MVI   FROMMRGE,C'N'                                                    
         MVI   FRSTWFL,C'Y'        FIRST WORKER FILE                            
*                                                                               
PREP10   BAS   RE,GTWFILE          GET FIRST/(NEXT) WORKER FILE                 
         BNE   PREPX                                                            
*                                                                               
         BAS   RE,WKREAD           READ FIRST WORKER FILE RECORD                
         BNE   PREP10                                                           
         BAS   RE,CKTALAGY         CHECK TAL AGY OKAY TO PROC. THIS RUN         
         BNE   PREP10                                                           
*                                                                               
         CLI   FRSTWFL,C'Y'        IF NOT FIRST WORKER FILE                     
         BE    *+8                                                              
         BAS   RE,PRTELOGO         PRINT END LOGO PAGES                         
*                                                                               
         ZAP   CNTMAT,=P'0'        COUNTER OF MATCHED COMML'S                   
         ZAP   CNTUMAT,=P'0'       COUNTER OF UNMATCHED COMML'S                 
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD INITIALIZE                          
         BAS   RE,PROCWF           READ WORKER FILE RECORDS/PUT SORT            
*                                                                               
         MVI   FRSTWFL,C'N'        SET NOT FIRST WORKER FILE                    
         BAS   RE,PRTSLOGO         PRINT START LOGO PAGES                       
*                                                                               
         BAS   RE,PRTREP           GET SORT/ADD XFER RECS/PRINT REPORT          
         BAS   RE,PRTSUM           PRINT SUMMARY OF MATCHED/UNMATCHED           
*                                                                               
         GOTO1 SORTER,DMCB,=C'END' END THE SORT                                 
         TM    NOPTION,NOPOST      IF POSTING = Y                               
         BO    PREP50                                                           
         BAS   RE,KEEP             PUT WORKER FILE ON KEEP                      
         BAS   RE,RETAIN           CLEAR RETENTION DAYS                         
*                                                                               
PREP50   BAS   RE,CLOSE            CLOSE WORKER FILE                            
         BAS   RE,INDRERD          RE-READ THIS INDEX RECORD                    
         B     PREP10              LOOP FOR NEXT WORKER FILE                    
*                                                                               
PREPX    L     R0,=A(NHOLDLNQ)                                                  
         L     R1,ANEWHOLD                                                      
         FREEMAIN RC,A=(1),LV=(0)                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET A WORKER FILE                                     
         SPACE 1                                                                
GTWFILE  NTR1                                                                   
         XC    RECORD,RECORD                                                    
         LA    R2,INDEX                                                         
         USING UKRECD,R2                                                        
*                                                                               
GTWFL20  BAS   RE,INDEXRD          GET INDEX OF FIRST/NEXT FILE                 
         BNE   NO                                                               
*                                                                               
         TM    UKSTAT,X'08'        IF WORKER FILE NOT ON KEEP                   
         BO    GTWFL20                                                          
         OC    PROCAGY,PROCAGY     IF PROCESSING A SINGLE AGENCY                
         BZ    *+14                                                             
         CLC   UKUSRID,PROCAGY     MATCH ON AGENCY NUMBER                       
         BNE   GTWFL20                                                          
*                                                                               
         CLC   UKSYSPRG,WRKNM      WORKER FILE NAME MUST MATCH                  
         BNE   GTWFL20                                                          
         CLI   UKCLASS,C'T'        MUST BE CLASS T                              
         BNE   GTWFL20                                                          
         OC    SEQNO,SEQNO         IF SPECIFIC SEQ. NUM REQUESTED               
         BZ    GTWFL25                                                          
         CLC   UKFILNO,SEQNO       MUST MATCH                                   
         BNE   GTWFL20                                                          
         B     GTWFL30                                                          
         CLC   NUKFILENO-NUKRECD(2,R2),SEQNO                                    
         BNE   GTWFL20                                                          
         B     GTWFL30                                                          
*                                                                               
GTWFL25  TM    NOPTION,NRERUN      IF RERUNING                                  
         BZ    GTWFL28                                                          
         CLC   UKDAY,RERUNDY       JUST GET WORKERS FOR THAT DAY                
         BNE   GTWFL20                                                          
         B     GTWFL30                                                          
*                                                                               
GTWFL28  CLC   UKDAY,TGTODAY1+2    SKIP WORKER FILES TODAY                      
         BNE   GTWFL30                                                          
         CLI   UKSUBPRG,C'N'       EXCEPT IF SUB PROGRAM IS N                   
         BNE   GTWFL20             DO NOT SKIP NON-DS CLARUS                    
*                                                                               
GTWFL30  BAS   RE,SETWKAGY         SET WORKER FILE AGENCY ADDRESS, ETC          
*                                                                               
         LA    R3,L'INDEX                                                       
         GOTO1 MYTRACE,DMCB,=C'GOOD INDEX',INDEX,(R3)                           
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO SET WORKER FILE AGENCY CODE & NAME                    
         SPACE                                                                  
SETWKAGY NTR1                                                                   
         XC    WORK,WORK           SET ORIGIN NUMBER                            
         MVC   WORK+8(L'UKUSRID),UKUSRID                                        
         GOTO1 USERVAL,DMCB,(X'B0',WORK)                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WAGYNUM,TGUSER      SAVE WORKER FILE NUMBER                      
         MVC   WAGYCD,TGUSERID     SAVE WORKER FILE AGENCY CODE                 
         MVC   WAGYCD2,TGUSERI2                                                 
         MVC   WAGYNAME,TGNAME     SAVE WORKER FILE AGENCY NAME                 
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO CHECK TALENT AGENCY OKAY TO PROCESS THIS RUN          
         SPACE 1                                                                
CKTALAGY NTR1                                                                   
         LA    R4,WREC+4           R4=A(FIRST WORKER FILE RECORD)               
         USING TANXD,R4                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A8',TANXAGY),0                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   MAINRUN,C'Y'        IF NOT MAIN RUN                              
         BE    CKTAGY10                                                         
         OC    TALAGY,TALAGY       AND IF TALENT AGENCY SPECIFIED               
         BZ    CKTAGY20                                                         
         CLC   TALAGY,TANXAGY      MUST MATCH ON IT                             
         BE    CKTAGY20                                                         
         B     NO                                                               
*                                                                               
CKTAGY10 L     R4,AIO              FOR MAIN RUN                                 
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         TM    TAAYSTA3,TAAYSPNX                                                
         BO    NO                  SKIP AGENCIES GOING DIRECT                   
*                                                                               
CKTAGY20 BAS   RE,SETALAGY         SET TALENT AGENCY NAME & ADDRESS             
         B     YES                 AND STATUS                                   
         EJECT                                                                  
*              ROUTINE TO SET NAME AND ADDRESS OF TALENT AGENCY                 
         SPACE 1                                                                
SETALAGY NTR1                                                                   
         MVC   AGYNAME,TGNAME      SET AGENCY NAME                              
         MVC   AGYADD,SPACES       PRE-CLEAR ADDRESS TO SPACES                  
         MVI   AGYSTA4,0           CLEAR AGENCY STATUS 4                        
*                                                                               
         XR    R3,R3                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   AGYSTA4,TAAYSTA4    SAVE AGENCY STATUS 4                         
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   SETTAGY8                                                         
*                                                                               
         USING TAADD,R4            R4=A(AGENCY ADDRESS ELEMENT)                 
         ZIC   RF,TAADLNES         SHOW ONLY 3 LINES                            
         CH    RF,=H'4'                                                         
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         LR    R3,RF               SAVE NUMBER OF LINES                         
         LA    R1,TAADADD                                                       
         LA    RE,AGYADD                                                        
*                                                                               
SETTAGY5 MVC   0(L'TAADADD,RE),0(R1)                                            
         LA    RE,L'AGYADD1(RE)                                                 
         LA    R1,L'TAADADD(R1)                                                 
         BCT   RF,SETTAGY5                                                      
*                                                                               
SETTAGY8 CH    R3,=H'3'            IF ADDRESS LESS THAN THREE LINES             
         BNL   SETTAGYX                                                         
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTATT                                  
         BNE   SETTAGYX                                                         
         MVC   AGYADD3,TGNAME      USE THIRD LINE FOR ATTENTION NAME            
SETTAGYX B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PROCESS CURRENT WORKER FILE                           
*              NE1 LAYOUT  - ONE REC WITH HDR INFO AND ONE CLA ELEMENT          
*              STAL LAYOUT - ONE HDR REC FOLW'D BY ONE OR MORE DTL RECS         
*                            DTL REC CONTAINS ALL WSP ELEMENTS                  
*              EACH SORTREC  CONTAINS HEADER INFO WITH ONE CLA/MKT ELE          
         SPACE 1                                                                
PROCWF   NTR1                                                                   
         B     *+12                                                             
*                                                                               
PROCWF5  BAS   RE,WKREAD           READ NEXT WORKER FILE RECORD                 
         BNE   XIT                                                              
*                                                                               
         LA    R4,WREC+4           R4=A(WORKER FILE RECORD)                     
         LA    R6,SRTREC           R6=A(SORT RECORD)                            
         USING SRTRECD,R6                                                       
         BAS   RE,PROCNREC         PROCESS A NETWORK WORKER FILE RECORD         
         B     PROCWF5                                                          
         EJECT                                                                  
*        ROUTINE TO PROCESS A RECORD FROM THE NETWORK WORKER FILE               
         SPACE 1                                                                
PROCNREC NTR1                                                                   
         BAS   RE,CLRSREC          CLEAR SORT RECORD AREA                       
*                                                                               
         BAS   RE,SETSLEN          SET SORT RECORD LENGTH                       
         USING TANXD,R4                                                         
         BAS   RE,SETSKEY          SET SORT KEY AND SOME GLOBASS                
         CLI   SKIP,C'Y'                                                        
         BE    XIT                                                              
         BAS   RE,SETUMAT          DEFAULT SORT REC TO UNMATCHED                
         BRAS  RE,SETTMKT          TMKTS HAVE TO BE CNVRTED TO ALPHA            
         BRAS  RE,CLRPRG           CLEAR PROGRAM NAMES                          
         CLI   SRTCCDE,0           IF NO CHANGE CODE                            
         BNE   PROCNR30                                                         
         BAS   RE,CHKNID           CHECK ID IS A MATCH                          
         BNE   PROCNR30                                                         
         BRAS  RE,SETCYC           SOME USES REQUIRE CYCLES TO BE               
         BNE   PROCNR30            SET BEFORE REC CAN BE MATCHED                
         BAS   RE,SETMAT           SET SORT DETAILS AS MATCHED                  
*                                                                               
PROCNR30 BAS   RE,CPYWREC          COPY WREC TO SORT RECORD                     
         BAS   RE,PUTSORT          PUT RECORD TO SORT                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET KEY OF SORT RECORD AND SOME GLOBASS               
         SPACE                                                                  
         USING TANXD,R4            R4=A(WORKER FILE RECORD)                     
SETSKEY  NTR1                                                                   
         MVC   TGAGY,TANXAGY       SET GLOBAS AGENCY                            
         MVC   TGNID,TANXNCID      SET NETWORK/SPOT COMML ID                    
         MVC   TGADID,TANXADID     SET NETWORK/SPOT AD ID                       
         MVC   SRTNID,TANXNCID     SET COMML ID IN SORT KEY                     
         TM    TANXSTAT,TANXPACK   PACKED?                                      
         BZ    *+8                                                              
         OI    SRTSTAT,SRTSPACK    SET STATUS IN SORT KEY                       
         SPACE 1                                                                
         MVC   SRTCCDE,TANXCCDE    SET CHANGE CODE IN SORTKEY TOO               
         SPACE 1                                                                
         XC    TGADID,TGADID                                                    
         CLI   TANXLEN,TANXLN2Q                                                 
         BL    *+10                                                             
         MVC   TGADID,TANXADID     SAVE AD-ID IF PRESENT                        
         SPACE 1                                                                
         MVI   SRTMED,C'T'         HARD CODE MEDIA TO TV                        
         CLI   TANXMED,0                                                        
         BE    *+10                                                             
         MVC   SRTMED,TANXMED                                                   
         SPACE 1                                                                
         XC    SRTNCLI,SRTNCLI     SET NET/SPOT CLIENT IN SORT                  
         USING TACTD,R4                                                         
         LA    R4,WREC+4                                                        
         MVI   ELCODE,TACTELQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   *+10                                                             
         MVC   SRTNCLI,TACTCLI                                                  
         SPACE 1                                                                
         XC    SRTNPRD,SRTNPRD     SET NET/SPOT PRODUCT IN SORT                 
         USING TAPRD,R4                                                         
         LA    R4,WREC+4                                                        
         MVI   ELCODE,TAPRELQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   *+10                                                             
         MVC   SRTNPRD,TAPRPRD                                                  
         SPACE 1                                                                
         MVC   TGNCLI,SRTNCLI      SET GLOBAL CLIENT CODE                       
         MVC   TGNPRD,SRTNPRD             AND PRODUCT CODE                      
         SPACE 1                                                                
         LA    R4,WREC+4           SET USE DATE IN SORT KEY                     
         USING TANPD,R4                                                         
         MVI   ELCODE,TANPELQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   *+10                                                             
         MVC   SRTUSEDT,TANPDATE                                                
         SPACE 1                                                                
         MVC   SRTUSE,=C'CLA'      AND USE TO NETWORK CLASS A                   
         SPACE 1                                                                
         TM    TANPSTAT,TANPPAX    IF NETWORK IS PAX                            
         BZ    SSKEY05                                                          
         MVC   SRTUSE,=C'PAX'      SET USE TO NETWORK PAX                       
         MVI   TANPLEN,TANPLNQ3                                                 
         B     SSKEY20             ***TEMPORARY CODE***                         
         SPACE 1                                                                
*SKEY05  TM    TANPSTAT,TANPITN    IS NETWORK IS ITN                            
*        BZ    *+10                                                             
*        MVC   SRTUSE,=C'ITN'      SET USE TO NETWORK ITN                       
         SPACE 1                                                                
SSKEY05  TM    TANPSTAT,TANPFLAT   IF FLAT RATE PROGRAM                         
         BZ    SSKEY10                                                          
         MVC   SRTUSE,=C'LNA'      SET USE BY NETWORK                           
         CLI   TANPNWK,C'A'        LNA FOR ABC                                  
         BE    SSKEY20                                                          
         MVC   SRTUSE,=C'LNN'      LNN FOR NBC                                  
         CLI   TANPNWK,C'N'                                                     
         BE    SSKEY20                                                          
         MVC   SRTUSE,=C'LNF'      LNF FOR FOX                                  
         CLI   TANPNWK,C'F'                                                     
         BE    SSKEY20                                                          
         MVC   SRTUSE,=C'LNC'      LNC FOR CBS                                  
         CLI   TANPNWK,C'C'                                                     
         B     SSKEY20                                                          
         SPACE 1                                                                
SSKEY10  CLI   TANPTYP,TANPNET     IF CABLE NETWORK PROGRAM                     
         BNE   *+10                                                             
         MVC   SRTUSE,=C'CBL'      SET USE TO CABLE                             
         SPACE 1                                                                
         CLI   TANPTYP,TANPSPT     IF SPOT PROGRAM                              
         BNE   *+10                                                             
         MVC   SRTUSE,=C'WSP'      SET USE TO WILDSPOT                          
         SPACE 1                                                                
         CLI   TANPTYP,TANPCSYS    IF LOCAL CABLE PRGRAM                        
         BNE   *+10                                                             
         MVC   SRTUSE,=C'LCB'      SET USE TO LCB                               
         SPACE 1                                                                
SSKEY20  GOTO1 MEDVAL,DMCB,SRTMED                                               
         GOTO1 USEVAL,DMCB,(X'40',SRTUSE)                                       
                                                                                
         MVI   SKIP,C'N'                                                        
         TM    TANPSTAT,X'08'                                                   
         BZ    XIT                                                              
         CLC   SRTUSE,=C'CLA'                                                   
         BE    SETSKIP                                                          
         CLC   SRTUSE,=C'PAX'                                                   
         BE    SETSKIP                                                          
         CLC   SRTUSE,=C'ITN'                                                   
         BE    SETSKIP                                                          
         CLC   SRTUSE,=C'LNA'                                                   
         BE    SETSKIP                                                          
         CLC   SRTUSE,=C'LNC'                                                   
         BE    SETSKIP                                                          
         CLC   SRTUSE,=C'LNF'                                                   
         BE    SETSKIP                                                          
         CLC   SRTUSE,=C'LNN'                                                   
         BNE   XIT                                                              
SETSKIP  MVI   SKIP,C'Y'                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET SORT RECORD UNMATCHED                             
         SPACE                                                                  
SETUMAT  NTR1                                                                   
         MVI   SRTTYPE,SRTTYPU     SET UNMATCHED                                
         MVC   SRTCLI,SPACES       SET NETWORK CLIENT CODE                      
         MVC   SRTCLI(L'SRTNCLI),SRTNCLI                                        
         MVC   SRTPRD,SPACES       SET NETWORK PRODUCT CODE                     
         MVC   SRTPRD(L'SRTNPRD),SRTNPRD                                        
         MVC   SRTID,SPACES        SET NETWORK COMMERCIAL ID                    
         MVC   SRTID(L'SRTNID),SRTNID                                           
*                                                                               
         MVC   SRTTTL,SPACES       PRE-CLEAR NETWORK TITLE                      
         MVC   SRTCLIN,SPACES      PRE-CLEAR CLIENT NAME                        
         MVC   SRTPRDN,SPACES      PRE-CLEAR PRODUCT NAME                       
*                                                                               
         LA    R4,WREC+4                                                        
         MVI   ELCODE,TAFNELQ                                                   
*                                                                               
         BAS   RE,FIRSTEL                                                       
         B     *+8                                                              
SETUMAT5 BAS   RE,NEXTEL                                                        
         BNE   SETUMATX                                                         
         USING TAFND,R4                                                         
*                                                                               
         CLI   TAFNTYPE,TAFNTTTL                                                
         BNE   SETUMAT6                                                         
         ZIC   R1,TAFNLEN                                                       
         SH    R1,=Y(TAFNLNQ+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTTTL(0),TAFNNAME  SET NETWORK ID TITLE NAME                    
         B     SETUMAT5                                                         
*                                                                               
SETUMAT6 CLI   TAFNTYPE,TAFNTCLI                                                
         BNE   SETUMAT8                                                         
         ZIC   R1,TAFNLEN                                                       
         SH    R1,=Y(TAFNLNQ+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTCLIN(0),TAFNNAME SET NETWORK CLIENT NAME                      
         B     SETUMAT5                                                         
*                                                                               
SETUMAT8 CLI   TAFNTYPE,TAFNTPRD                                                
         BNE   SETUMAT5                                                         
         ZIC   R1,TAFNLEN                                                       
         SH    R1,=Y(TAFNLNQ+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTPRDN(0),TAFNNAME SET NETWORK PRODUCT NAME                     
         B     SETUMAT5                                                         
*                                                                               
SETUMATX B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK WHETHER NET/SPOT COMML ID IS ITSELF             
*              A TALENT COMML, LIFT ID, OR ALIAS                                
         SPACE 1                                                                
       ++INCLUDE TACHKNID                                                       
         EJECT                                                                  
*              ROUTINE TO SET SORT REC MATCHED AND TAL CID & METHOD             
*              IN WORKER FILE RECORD                                            
         SPACE                                                                  
SETMAT   NTR1                                                                   
         MVI   SRTTYPE,SRTTYPM     SET MATCHED IN SORT RECORD                   
*                                                                               
         L     R4,AIO              R4=A(TALENT COMMERCIAL)                      
         USING TLCOD,R4                                                         
         MVC   TGCLI,TLCOCLI       SET GLOBAS CLIENT                            
         MVC   TGPRD,TLCOPRD       SET GLOBAS PRODUCT                           
*                                                                               
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   SRTID,TACOCID       SET TALENT COMML IN SORT                     
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   SRTTTL,SPACES       AND TITLE NAME                               
         MVC   SRTTTL(L'TGNAME),TGNAME                                          
*                                                                               
         BAS   RE,SETTCLI              AND CLIENT CODE AND NAME                 
         BAS   RE,SETTPRD              AND PRODUCT CODE AND NAME                
*                                                                               
         MVC   SRTVER,NEWVER       SET VERSION LETTER                           
*                                                                               
         LA    R4,WREC+4           AND SET MATCHED INFO IN WRKR REC             
         USING TANXD,R4                                                         
         MVC   TANXCOM,NEWCOM      TALENT INTERNAL COMML NUMBER                 
         MVC   TANXTYPE,NEWCTYPE   AND METHOD                                   
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET TALENT CLIENT CODE AND NAME                       
         SPACE                                                                  
SETTCLI  NTR1                                                                   
         MVC   SRTCLI,TGCLI        SET TALENT CLIENT IN SORT                    
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   SRTCLIN,TGNAME      SET CLIENT NAME                              
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET TALENT PRODUCT CODE AND NAME                      
         SPACE                                                                  
SETTPRD  NTR1                                                                   
         MVC   SRTPRD,TGPRD        SET TALENT PRODUCT IN SORT                   
         MVC   SRTPRDN,SPACES      SET PRODUCT NAME                             
         OC    SRTPRD,SRTPRD                                                    
         BZ    SETTPRDX                                                         
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A0',0)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   SRTPRDN,TGNAME      SET PRODUCT NAME                             
SETTPRDX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO COPY WORKER FILE RECORD TO SORT RECORD                
         SPACE 1                                                                
CPYWREC  NTR1                                                                   
         LA    R2,SRTKLNQ(R6)      BUMP TO DATA                                 
         LH    RE,WREC                                                          
         SH    RE,=H'5'            4 FOR HEADER + 1 FOR EX                      
         CH    RE,=H'254'                                                       
         BNH   *+6                                                              
         DC    H'0'                BAD WORKER FILE RECORD                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WREC+4                                                   
         LA    RE,1(RE)            RESTORE LENGTH                               
         AR    R2,RE                                                            
         MVI   0(R2),0             END OF RECORD                                
         ST    R2,ASRTEND                                                       
         B     XIT                                                              
         EJECT                                                                  
*            ROUTINE TO ADD NET/SPOT TRANSFER RECS AND PRINT REPORT             
         SPACE                                                                  
PRTREP   NTR1                                                                   
         MVC   AIO,ANEWHOLD        SET AIO FOR TRANSFER RECORD                  
         XC    SVNID,SVNID         CLEAR LAST SORT COMMERCIAL ID                
         XC    SEQCNT,SEQCNT       CLEAR TANPD SEQ NUMBER COUNTER               
         MVI   FRST,C'Y'                                                        
*                                                                               
         USING SRTRECD,R6                                                       
PRT10    BAS   RE,GETSORT          RETURNS R6=A(SORT RECORD)                    
         LTR   R6,R6                                                            
         BZ    PRT60                                                            
*                                                                               
         OC    SVNID,SVNID         IF FIRST RECORD                              
         BNZ   *+16                                                             
PRT15    BAS   RE,SVVALS           SAVE CURRENT SORT RECORD VALUES              
         BAS   RE,SETNXREC         SET PRELIMINARY XFER REC INFO                
         B     PRT10               LOOP FOR NEXT RECORD                         
*                                                                               
         CLC   SVNID,SRTNID        IF SAME NETWORK ID                           
         BNE   PRT30                                                            
         CLC   SVNCLI,SRTNCLI      AND SAME CLIENT CODE                         
         BNE   PRT30                                                            
         CLC   SVNPRD,SRTNPRD      AND SAME PRODUCT CODE                        
         BNE   PRT30                                                            
         CLC   SVMED,SRTMED        AND SAME MEDIA CODE                          
         BNE   PRT30                                                            
         CLC   SVUSE,SRTUSE        AND SAME USE CODE                            
         BNE   PRT30                                                            
         CLC   SVCCDE,SRTCCDE      AND SAME CHANGE CODE (FOR USES)              
         BNE   PRT30                                                            
         BAS   RE,ADDDTLEL         ADD TANPD TO CURRENT REC                     
         B     PRT10                                                            
*                                                                               
PRT30    BAS   RE,ADDIT            ADD XFER RECS AND PRINT                      
         BAS   RE,CKBRK            IF NEED PRINTING BREAK                       
         BE    *+12                                                             
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE FOR HEADINGS                  
         B     *+8                                                              
         BAS   RE,BXBOT            ELSE, PRINT MIDDLE LINE                      
         B     PRT15                                                            
*                                                                               
PRT60    OC    SVNID,SVNID         IF SORT KEY                                  
         BZ    XIT                                                              
         BAS   RE,ADDIT            ADD XFER RECS AND PRINT                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SAVE CURRENT SORT RECORD VALUES                       
         SPACE                                                                  
SVVALS   NTR1                                                                   
         MVC   REPTYPE,SRTTYPE     SAVE REPORT TYPE                             
*                                                                               
         MVC   SVNID,SRTNID        SAVE CURRENT NET/SPOT COMML ID               
         MVC   SVNCLI,SRTNCLI      SAVE CURRENT NETWORK CLIENT CODE             
         MVC   SVNPRD,SRTNPRD      SAVE CURRENT NETWORK PRODUCT CODE            
*                                                                               
         MVC   SVCLIN,SRTCLIN      SAVE CLIENT NAME (TAL OR NWK)                
         MVC   SVPRDN,SRTPRDN      SAVE PRODUCT NAME (TAL OR NWK)               
         MVC   SVTTTL,SRTTTL       SAVE CURRENT TITLE (TAL OR NWK)              
*                                                                               
         MVC   SVCLI,SRTCLI        SAVE CURRENT CLI CODE (TAL OR NET)           
         MVC   SVPRD,SRTPRD        SAVE CURRENT PROD (TAL OR NET)               
*                                                                               
         MVC   SVMED,SRTMED        SAVE CURRENT MEDIA CODE                      
         MVC   SVUSE,SRTUSE        SAVE CURRENT USE CODE                        
         MVC   SVCCDE,SRTCCDE      SAVE CURRENT CHANGE CODE                     
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CHECK FOR NEW PAGE NEEDED                             
*                                  XIT CC EQ IF PAGE BREAK NEEDED               
         SPACE                                                                  
CKBRK    NTR1                                                                   
         CLC   SVCLI,SRTCLI        IF CLIENT CHANGED                            
         BNE   NO                  BREAK TO NEW PAGE                            
         CLC   SVPRD,SRTPRD        OR IF PRODUCT CHANGED                        
         BNE   NO                  BREAK TO NEW PAGE                            
         CLC   SVMED,SRTMED        OR IF MEDIA CHANGED                          
         BNE   NO                  BREAK TO NEW PAGE                            
         CLC   SVUSE,SRTUSE        OR IF USE CHANGED                            
         BNE   NO                  BREAK TO NEW PAGE                            
*                                                                               
         CLC   REPTYPE,SRTTYPE     IF REPORT CHANGE                             
         BE    YES                                                              
         CLI   SRTTYPE,SRTTYPM                                                  
         BNE   YES                                                              
         CLI   FRST,C'Y'           AND FIRST ONE                                
         BNE   YES                                                              
         MVI   FRST,C'N'                                                        
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO SET PRELIMINARY NETWORK TRANSFER REC INFO             
*              ON ENTRY, R6=A(SORT RECORD)                                      
         SPACE 1                                                                
SETNXREC NTR1                                                                   
         USING TLNXD,R4                                                         
         L     R4,AIO              R4=A(NEW RECORD)                             
         SPACE 1                                                                
         MVC   TLNXLEN,DATADISP      SET START LENGTH                           
         XC    TLNXSTAT(10),TLNXSTAT AND CLEAR STATUS                           
         SPACE 1                                                                
         BAS   RE,SETHKEY                     SET KEY IN KEY                    
         MVC   TLNXKEY,KEY                    SET KEY IN IOAREA                 
         MVC   TLNXSTAT(1),KEY+TLDRSTAT-TLDRD SET STATUS IN RECORD              
         SPACE 1                                                                
         USING ELETABD,R2                                                       
         LA    R2,ELETAB           R2=A(ELEMENT TABLE)                          
         SPACE 1                                                                
         LA    R3,ELEMENT          R3=A(ELEMENT TO BE ADDED)                    
         SPACE 1                                                                
SETR10   CLI   ETABCD,X'FF'        TEST END OF TABLE                            
         BE    SETR90                                                           
         MVC   ELCODE,0(R2)        ELEMENT CODE TO ADD                          
         LA    R4,SRTKLNQ(R6)      R4=A(FIRST ELEMENT IN SORT RECORD)           
         BAS   RE,FIRSTEL                                                       
         BNE   SETR80                                                           
         SPACE 1                                                                
SETR20   MVC   ELEMENT,0(R4)       SET SORT RECORD ELEMENT                      
         SPACE 1                                                                
         USING TANPD,R3                                                         
SETR30   CLI   ELEMENT,TANPELQ     IF NETWORK/CLASS A DETAILS ELEMENT           
         BNE   SETR60                                                           
         OC    TANPFEED,SPACES     PAD FEED CODE WITH SPACES                    
         MVC   TANPSEQ,SEQCNT+1    SET SEQUENCE COUNT                           
         BAS   RE,INCSEQ           AND INCREMENT ELEMENT SEQ COUNTER            
         SPACE 1                                                                
         LHI   RF,L'TANPNTI                                                     
         BAS   RE,CBLCHK           IF NETWORK CABLE PROGRAM                     
         BE    SETR40                                                           
         LHI   RF,L'TANPMKT                                                     
         BAS   RE,WSPCHK           OR WILDSPOT PROGRAM                          
         BE    SETR40                                                           
         LHI   RF,L'TANPSYS                                                     
         BAS   RE,LCBCHK           OR LOCAL CABLE PROGRAM                       
         BNE   SETR60                                                           
SETR40   LA    RE,TANPDATA                                                      
         MVI   TANPCNTR,1          SET THE COUNTER AS ONE                       
SETR50   CLI   0(RE),C' '          AND PAD THE CNET/MKT/CSYS CODE               
         BNE   *+8                 WITH ZEROES                                  
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,SETR50                                                        
         DROP  R3                                                               
         SPACE 1                                                                
         USING TANXD,R3                                                         
SETR60   CLI   ELEMENT,TANXELQ     IF NETWORK DETAILS ELEMENT                   
         BNE   SETR70                                                           
         MVC   TANXUID,SPACES      SET WORKER FILE ID                           
*        MVC   TANXUID(L'WAGYCD),WAGYCD                                         
         MVC   TANXUID,WAGYCD2                                                  
         MVC   TANXADTE,TGTODAY1   SET DATE ADDED IN ELEMENT                    
         CLI   SRTTYPE,SRTTYPU     AND RECORD IS MATCHED                        
         BE    SETR70                                                           
         MVC   TANXMDTE,TGTODAY1   SET MATCHED DATE IN ELEMENT                  
         MVC   TANXVERS,SRTVER     AND VERSION LETTER                           
         DROP  R3                                                               
         SPACE 1                                                                
SETR70   GOTO1 ADDELEM             ADD TO THE RECORD                            
         SPACE 1                                                                
         BAS   RE,NEXTEL                                                        
         BE    SETR20                                                           
         SPACE 1                                                                
SETR80   LA    R2,ETABLNQ(R2)      BUMP TO NEXT ELEMENT IN TABLE                
         B     SETR10              LOOP                                         
         SPACE 1                                                                
SETR90   GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY ELEMENT                         
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO SET KEY FOR ADD                                       
         SPACE                                                                  
SETHKEY  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLNXD,R4                                                         
         MVI   TLNXCD,TLNXCDQ      SET RECORD CODE                              
         MVC   TLNXAGY,TGAGY       SET AGENCY                                   
         MVC   TLNXNID,SRTNID      NETWORK COMMERCIAL ID                        
         TM    SRTSTAT,SRTSPACK    PACKED COMMERCIAL?                           
         BZ    *+8                                                              
         OI    TLDRSTAT-TLDRD(R4),TLNXPACK   ELSE, SET KEY PACKED               
         MVC   TLNXNCLI,SRTNCLI    NETWORK CLIENT CODE                          
         MVC   TLNXNPRD,SRTNPRD    NETWORK PRODUCT CODE                         
         MVC   TLNXMED,SRTMED      MEDIA CODE                                   
         MVC   TLNXUSE,SRTUSE      USE CODE                                     
         MVC   TLNXDATE,TGTODAY1   PWOS                                         
         XC    TLNXDATE,XFFS       COMPLEMENT DATE                              
         MVC   TLNXUID,WAGYNUM     WORKER FILE ID NUMBER                        
         MVC   TLNXCCDE,SRTCCDE    CHANGE CODE (FOR USES) IF ANY                
*                                                                               
         CLI   SRTTYPE,SRTTYPU     IF SORT RECORD NOT MATCHED                   
         BNE   *+14                                                             
         AP    CNTUMAT,=P'1'       ADD TO UNMATCHED COUNTER                     
         B     SETHKEYX                                                         
*                                                                               
         OI    TLDRSTAT-TLDRD(R4),TLNXSMAT   ELSE, SET KEY MATCHED              
         AP    CNTMAT,=P'1'        ADD TO MATCHED COUNTER                       
*                                                                               
SETHKEYX B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO ADD DETAIL ELEMENT                                    
         SPACE                                                                  
ADDDTLEL NTR1                      ROUTINE CALLED FROM PRTREP                   
         LA    R4,SRTKLNQ(R6)      PT TO FIRST ELEMENT IN SORT RECORD           
         MVI   ELCODE,TANPELQ      LOOK FOR TANPD ELEMENT                       
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ADDTANP          COPY OVER TANPD INFO                         
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO ADD TANPD ELEMENT TO HOLD RECORD                      
         SPACE 1                                                                
         USING TANPD,R4                                                         
ADDTANP  NTR1                                                                   
         ST    R4,TGFULL           TFULL=A(CURRENT TANPD ELEMENT)               
         SPACE 1                                                                
         MVC   ELEMENT,0(R4)       MOVE TANP ELEMENT TO HOLD RECORD             
*                                                                               
         LA    R4,ELEMENT                                                       
         SPACE 1                                                                
         OC    TANPFEED,SPACES     PAD FEED CODE WITH SPACES                    
         SPACE 1                                                                
         LHI   RF,L'TANPNTI                                                     
         BAS   RE,CBLCHK           IF CABLE NETWORK PROGRAM                     
         BE    ATANP10                                                          
         LHI   RF,L'TANPMKT                                                     
         BAS   RE,WSPCHK           OR WILDSPOT PROGRAM                          
         BE    ATANP10                                                          
         LHI   RF,L'TANPSYS                                                     
         BAS   RE,LCBCHK           OR LOCAL CABLE PROGRAM                       
         BNE   ATANP50                                                          
ATANP10  LA    RE,TANPDATA                                                      
ATANP20  CLI   0(RE),C' '          PAD CSYS/MKT/CNET CODE WITH ZEROES           
         BNE   *+8                                                              
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,ATANP20                                                       
         SPACE 1                                                                
         LA    R3,ELEMENT                                                       
         L     R4,AIO                                                           
         MVI   ELCODE,TANPELQ                                                   
         BAS   RE,GETEL            IF ELEMENT ALREADY EXISTS FOR                
         B     *+8                 THIS CNET/MKT/CSYS ON THIS DAY               
ATANP30  BAS   RE,NEXTEL                                                        
         BNE   ATANP40                                                          
         CLC   TANPDATE,TANPDATE-TANPD(R3)                                      
         BNE   ATANP30                                                          
         CLC   TANPLFT(3),TANPLFT-TANPD(R3)                                     
         BNE   ATANP30                                                          
         LHI   RF,L'TANPNTI                                                     
         BAS   RE,CBLCHK           IF CABLE NETWORK PROGRAM                     
         BE    ATANP35                                                          
         LHI   RF,L'TANPMKT                                                     
         BAS   RE,WSPCHK           OR WILDSPOT PROGRAM                          
         BE    ATANP35                                                          
         LHI   RF,L'TANPSYS        OR LOCAL CABLE PROGRAM                       
ATANP35  BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TANPSYS(0),TANPSYS-TANPD(R3)                                     
         BNE   ATANP30                                                          
         ZIC   RF,TANPCNTR         ADD ONE TO EXISTING ELEMENT'S                
         CHI   RF,255              COUNTER AND DON'T ADD AGAIN                  
         BNL   ATANP60                                                          
         AHI   RF,1                                                             
         STC   RF,TANPCNTR                                                      
         B     ATANP60                                                          
         SPACE 1                                                                
ATANP40  LA    R4,ELEMENT          IF OK TO ADD THIS ELEMENT                    
         MVI   TANPCNTR,1                                                       
ATANP50  MVC   TANPSEQ,SEQCNT+1    SET SEQUENCE NUMBER                          
         BAS   RE,INCSEQ                                                        
         BAS   RE,ADDNEWEL         AND ADD ELEMENT TO NEWHOLD                   
         SPACE 1                                                                
ATANP60  CLI   FROMMRGE,C'Y'                                                    
         BNE   XIT                                                              
         L     R4,AIO2                                                          
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
ATANP70  BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         C     R4,TGFULL                                                        
         BNE   ATANP70                                                          
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO ADD ELEMENT TO ANEWHOLD                               
         SPACE 1                                                                
ADDNEWEL NTR1                                                                   
         L     RE,AIO              RE=A(TRANSFER RECORD)                        
         USING TLNXD,RE                                                         
         LH    R1,TLNXLEN          IF MAXIMUM RECORD LENGTH REACHED             
         C     R1,=A(NHOLDLNQ)                                                  
         BL    *+6                                                              
         DC    H'0'                NEED TO INCREASE NEWHOLD IOAREA              
*                                                                               
         GOTO1 HELLO,DMCB,(C'p',SYSFIL),AIO,ELEMENT,=C'ADD=END',       *        
               =A(NHOLDLNQ)                                                     
         CLI   12(R1),5                                                         
         BE    XIT                                                              
         CLI   12(R1),0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
         EJECT                                                                  
*              ROUTINE TO ADD TRANSFER RECORD & PRINT INFORMATION               
*              IF RECORD TOO BIG, WILL SPLIT INTO MULTIPLE RECS                 
*              IF RECORD ALREADY ON FILE, WILL MERGE RECORDS                    
         SPACE                                                                  
ADDIT    NTR1                                                                   
         MVI   WRNING,0            CLEAR WARNING MSG                            
         MVI   PRNTED,C'N'         PRINT INDICATOR                              
         L     R4,AIO              R4=A(NETWORK TRANSFER TO ADD)                
         USING TLNXD,R4                                                         
         XC    KEY,KEY             IF KEY ALREADY EXISTS                        
         MVC   KEY(L'TLNXKEY),TLNXKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLNXKEY),KEYSAVE                                           
         BNE   ADDIT20                                                          
*                                                                               
         TM    NOPTION,NRERUN      IF RERUN MODE                                
         BZ    *+12                                                             
         MVI   WRNING,C'D'         SET INDICATOR AND DON'T ADD                  
         B     ADDIT30                                                          
         BAS   RE,PLINE            ELSE, PRINT NEW RECORD                       
         BAS   RE,MERGE            AND MERGE REC(S) ON FILE W/NEW REC           
*                                                                               
ADDIT20  BAS   RE,SPLIT            SPLIT/ADD/WRITE RECORD(S)                    
*                                                                               
ADDIT30  CLI   PRNTED,C'Y'         IF NOT YET PRINTED                           
         BE    *+8                                                              
         BAS   RE,PLINE            PRINT THE RECORD GENERATED                   
         XC    SEQCNT,SEQCNT       RESET TANPD SEQ NUMBER                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE MERGES CURRENT RECORD(S) ON FILE WITH                    
*              NEW RECORD IN ANEWHOLD                                           
         SPACE 1                                                                
MERGE    NTR1                                                                   
         GOTO1 MYTRAC2,DMCB,=C'NEWHOLD',ANEWHOLD,0                              
         BAS   RE,DELXTRA          STRIP NEW HOLD OF NAME ELEMENTS              
         GOTO1 MYTRAC2,DMCB,=C'MODNEW',ANEWHOLD,0                               
*                                                                               
         L     R3,AIO1             SET UP MERGE AREA                            
         MVI   0(R3),0                                                          
         GOTO1 MYTRAC2,DMCB,=C'MRGEAREA',AIO1,0                                 
*                                                                               
         L     R4,AIO              READ EXISTING HOLD RECORD INTO               
         MVC   KEY,0(R4)           AIO2                                         
         GOTO1 HIGH                                                             
         B     MRGE20                                                           
MRGE10   GOTO1 SEQ                                                              
         CLC   KEY(TLNXSEQ-TLNXKEY),KEYSAVE                                     
         BNE   MRGE60                                                           
MRGE20   MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         GOTO1 MYTRAC2,DMCB,=C'EXSTHOLD',AIO2,0                                 
*                                                                               
         MVC   AIO,ANEWHOLD                                                     
*                                                                               
         L     R4,AIO2                                                          
         MVI   ELCODE,0            LOOP THROUGH ALL ELEMENTS ON                 
         BAS   RE,GETEL            EXISTING HOLD RECORDS                        
         B     *+8                                                              
MRGE30   BAS   RE,NEXTEL                                                        
         BNE   MRGE10                                                           
*                                                                               
         CLI   0(R4),TANPELQ       ADD TANPS TO NEW HOLD RECORD                 
         BNE   MRGE200                                                          
         BAS   RE,CBLCHK                                                        
         BE    MRGE40                                                           
         BAS   RE,LCBCHK                                                        
         BE    MRGE40                                                           
         BAS   RE,WSPCHK                                                        
         BNE   *+8                                                              
MRGE40   MVI   FROMMRGE,C'Y'                                                    
         BAS   RE,ADDTANP                                                       
         MVI   FROMMRGE,C'N'                                                    
         GOTO1 MYTRAC2,DMCB,=C'ADDTANP',ANEWHOLD,0                              
         B     MRGE30                                                           
*                                                                               
MRGE200  BAS   RE,ADDHDREL         ELSE, ADD HDR TYPE ELEMENTS TO AIO1          
         GOTO1 MYTRAC2,DMCB,=C'ADDHEAD1',AIO1,0                                 
         B     MRGE30                                                           
*                                                                               
MRGE60   DS    0H                                                               
         GOTO1 MYTRAC2,DMCB,=C'PRESORT',ANEWHOLD,0                              
         BAS   RE,RESRTDTL         RE-SORT TANPD ELEMENTS                       
         GOTO1 MYTRAC2,DMCB,=C'POSTSORT',ANEWHOLD,0                             
*                                                                               
         L     R4,ANEWHOLD         R4=A(NETWORK TRANSFER RECORD)                
         L     R3,AIO1             R3=A(HDR TYPE ELEMENTS)                      
MRGE70   CLI   0(R3),0             IF MORE ELEMENTS                             
         BE    MRGEX                                                            
         MVC   ELEMENT,0(R3)       ADD ELEMENT BACK                             
         BAS   RE,ADDNEWEL         TO ANEWHOLD                                  
         GOTO1 MYTRAC2,DMCB,=C'ADDHEAD2',ANEWHOLD,0                             
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     MRGE70                                                           
*                                                                               
MRGEX    B     XIT                                                              
         EJECT                                                                  
*              DELETE HDR TYPE ELEMENTS (ALL EXCEPT TANPD)                      
         SPACE 1                                                                
DELXTRA  NTR1                                                                   
         L     R4,ANEWHOLD                                                      
         MVI   ELCODE,0            DELETE HDR TYPE ELEMENTS                     
         BAS   RE,GETEL                                                         
DELXTRA5 CLI   0(R4),TANPELQ       IF NOT TANPD                                 
         BE    DELXTRA8                                                         
         MVI   0(R4),X'FF'         MARK FOR DELETION                            
DELXTRA8 BAS   RE,NEXTEL                                                        
         BE    DELXTRA5                                                         
*                                                                               
         GOTO1 HELLO,DMCB,(C'd',SYSFIL),(X'FF',AIO),0,0,=A(NHOLDLNQ)            
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO ADD HDR TYPE ELEMENTS TO AIO1                         
         SPACE 1                                                                
ADDHDREL ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)                                                    
         LA    R1,1(R1)                                                         
         AR    R3,R1                                                            
         MVI   0(R3),0             RETURN R3 =A(NEXT AVAILABLE SPOT)            
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO RE-SORT DETAIL ELEMENTS                               
*              NET - TANPD ELEMENTS BY USE DATE                                 
         SPACE 1                                                                
RESRTDTL NTR1                                                                   
         L     R4,ANEWHOLD         RE-SORT ELEMENTS                             
         LA    R4,TLNXELEM-TLNXD(R4)                                            
         LH    R2,SEQCNT           R2=NUMBER OF ELEMENTS TO SORT                
*                                                                               
         LH    R3,=Y(TANPDATE-TANPD) R3=DISPLACEMENT OF KEY IN RECORD           
         BAS   RE,CBLCHK                                                        
         BE    RESRT10                                                          
         BAS   RE,WSPCHK                                                        
         BE    RESRT20                                                          
         BAS   RE,LCBCHK                                                        
         BE    RESRT30                                                          
         GOTO1 XSORT,DMCB,(R4),(R2),TANPLNQ3,L'TANPDATE,(R3)                    
         B     XIT                                                              
RESRT10  GOTO1 XSORT,DMCB,(R4),(R2),TANPLNQ4,L'TANPDATE,(R3)                    
         B     XIT                                                              
RESRT20  GOTO1 XSORT,DMCB,(R4),(R2),TANPLNQ4,L'TANPDATE,(R3)                    
         B     XIT                                                              
RESRT30  GOTO1 XSORT,DMCB,(R4),(R2),TANPLNQ4,L'TANPDATE,(R3)                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE ADDS TRANSFER RECORDS TO FILE                            
*              IF NECESSARY, SPLITS 'BIG' RECORDS INTO SMALLER ONES             
*              HDR TYPE ELEMENTS MUST BE ON FIRST PHYSICAL RECORD               
         SPACE 1                                                                
SPLIT    NTR1                                                                   
         MVI   PROCEL,C'H'         PROCESS HDR TYPE ELEMENTS FIRST              
         XC    SEQCNT,SEQCNT       SEQUENCE NUMBER FOR DTL ELS (NP/MT)          
         L     R4,ANEWHOLD                                                      
         L     R3,AIO2                                                          
         ST    R3,AIO                                                           
         USING TLNXD,R3                                                         
         MVC   TLNXKEY,0(R4)       SET KEY AND STATUS IN RECORD                 
         MVC   TLNXSTAT,TLNXSTAT-TLNXKEY(R4)                                    
*                                                                               
SPL1     LA    R4,TLNXELEM-TLNXD(R4) R4=A(FIRST ELEMENT)                        
         MVI   ELCODE,0            SET TO LOOP THROUGH ALL ELEMS                
*                                                                               
SPL2     MVC   TLNXLEN,DATADISP    INIT RECORD LENGTH                           
         XC    TLNXELEM(2),TLNXELEM                                             
*                                                                               
SPL4     DS    0H                                                               
         CLI   0(R4),TANXELQ       SEE IF AD-ID PRESENT                         
         BNE   SPL5                                                             
         CLI   1(R4),TANXLN2Q                                                   
         BNE   SPL5                                                             
         OI    TLNXSTAT,TLNXADID                                                
*                                                                               
SPL5     CLI   PROCEL,C'D'         IF PROCESSING DTL TYPE ELEMENTS              
         BNE   SPL6                                                             
         USING TANPD,R4                                                         
         CLI   0(R4),TANPELQ       ONLY ADD TANPD'S TO RECORD                   
         BNE   SPL8                                                             
         MVC   TANPSEQ,SEQCNT+1                                                 
         BAS   RE,INCSEQ                                                        
         B     SPL7                                                             
*                                                                               
SPL6     CLI   0(R4),TANPELQ       ELSE, ADD HDR TYPE ELES ONLY                 
         BE    SPL8                                                             
*                                                                               
SPL7     ZIC   R1,1(R4)            R1=L'ELEMENT                                 
         AH    R1,TLNXLEN             +L'RECORD                                 
         CHI   R1,1923             WILL RECORD BECOME TOO LONG                  
         BH    SPL10               YES - GO ADD IT                              
         MVC   ELEMENT,0(R4)       NO - MOVE ELEMENT TO W/S                     
         GOTO1 ADDELEM,DMCB,,,,0   AND ADD TO NEW REC                           
*                                                                               
SPL8     BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    SPL4                                                             
*                                                                               
         CLI   PROCEL,C'D'         IF NOT PROCESSING DTL ELEMENTS               
         BE    SPL10                                                            
         MVI   PROCEL,C'D'         GO DO THEM                                   
         L     R4,ANEWHOLD         AND LOOP THROUGH ALL ELS AGAIN               
         LA    R4,TLNXELEM-TLNXD(R4)                                            
         MVI   ELCODE,0                                                         
         B     SPL4                                                             
*                                                                               
SPL10    BAS   RE,DOADD            ADD/WRITE THE RECORD                         
         ZIC   R1,TLNXSEQ          BUMP SEQUENCE NUMBER IN KEY OF REC           
         LA    R1,1(R1)                                                         
         STC   R1,TLNXSEQ                                                       
         CLI   0(R4),0             IF THERE ARE MORE ELS. TO PROCESS            
         BNE   SPL2                GO BACK                                      
*                                                                               
SPLITX   MVC   AIO,ANEWHOLD                                                     
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO ADD/WRITE BACK NETWORK TRANSFER RECORD                
         SPACE 1                                                                
DOADD    NTR1                                                                   
         GOTO1 MYTRAC2,DMCB,=C'NETWORK XFER REC',AIO,0                          
*                                                                               
         L     R3,AIO              MOVE KEY FROM RECORD TO KEY                  
         USING TLNXD,R3            R3=A(A RECORD TO BE WRITTEN)                 
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLNXKEY),TLNXKEY                                           
         GOTO1 HIGH                SEE IF RECORD ALREADY ON FILE                
         CLC   KEY(L'TLNXKEY),KEYSAVE IS RECORD ALREADY ON FILE                 
         BE    DOADD30                                                          
*                                                                               
         MVC   KEY,KEYSAVE         NO, SO RESTORE SAVED KEY                     
         GOTO1 ADDREC              AND ADD NETWORK TRANSFER RECORD              
         MVI   BYTE,X'01'          FORCE SEQ IN PASSIVE BASED ON TIME           
         TM    NOPTION,NTRACE      IF TRACE ON                                  
         BNO   *+8                                                              
         OI    BYTE,X'10'          SET TRACE FOR ADDPTRS                        
         L     R2,ASPBLK                                                        
         XC    0(L'TLDRREC+1,R2),0(R2)                                          
         GOTO1 AADDPTRS,DMCB,(BYTE,(R2))                                        
*                                                                               
         STIMERM SET,ID=STIMERID,BINTVL=TENTHSEC,WAIT=YES                       
         LTR   RF,RF                                                            
         BZ    *+6                 WAIT ONE SECOND                              
         DC    H'0'                                                             
*                                                                               
         B     DOADDX                                                           
*                                                                               
DOADD30  MVC   AIO,AIO1            RECORD EXISTS - READ IT INTO IO1             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2            RESET AIO TO A(NEW RECORD)                   
         GOTO1 PUTREC              WRITE BACK NEW FILE RECORD                   
*                                                                               
DOADDX   XC    SEQCNT,SEQCNT       RESET SEQUENCE ELE NUMBS FOR NEW REC         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*            ROUTINE TO PRINT NEW RECORD INFORMATION                            
         SPACE 1                                                                
PLINE    NTR1                                                                   
         BAS   RE,SETSPRG          SET RCSUBPRG                                 
*                                                                               
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
         BAS   RE,CNTR             CENTER THE REPORT R2 SET ON XIT              
         L     R4,AIO              R4=A(NETWORK TRANSFER RECORD)                
         USING TLNXD,R4                                                         
*                                                                               
         CLI   TLNXSEQ,0           IF BASE RECORD                               
         BNE   PLINE20                                                          
*                                                                               
         MVC   PRTNID,TLNXNID      NETWORK COMMERCIAL ID                        
         CLI   WRNING,C'D'         IF DUPLICATE KEY - ALREADY ADDED             
         BNE   PLINE5                                                           
         MVC   PRTWARN,=CL18'*ALREADY ON FILE*'                                 
*                                                                               
PLINE5   BAS   RE,P2NTTL           NETWORK TITLE                                
         MVI   ELCODE,TANXELQ      LENGTH IN SECONDS                            
         BAS   RE,GETEL                                                         
         BNE   PLINE10                                                          
         USING TANXD,R4                                                         
         EDIT  TANXSEC,(3,PRTNLEN)                                              
         CLI   TANXLEN,TANXLN2Q                                                 
         BL    PLINE10                                                          
         CLC   TANXADID,SPACES                                                  
         BNH   PLINE10                                                          
         MVC   PRTNID(12),TANXADID                                              
*                                                                               
PLINE10  CLI   REPTYPE,SRTTYPU     IF UNMATCHED                                 
         BNE   PLINE15                                                          
         CLI   TANXCCDE,0          AND IF CHANGE CODE                           
         BE    PLINE15                                                          
         BAS   RE,P1CHG            PRINT CHANGE DESCRIPTION                     
*                                                                               
PLINE15  OC    TANXCOM,TANXCOM     IF INTERNAL COMMERCIAL NUMBER                
         BZ    PLINE20                                                          
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TANXCOM)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,P1CID            PRINT CID                                    
         MVC   AIO,ANEWHOLD                                                     
         BAS   RE,P2TTTL           AND TALENT TITLE                             
         BAS   RE,P1METHD          PRINT METHOD MATCHED                         
*                                                                               
PLINE20  BAS   RE,PDTLINE          PRINT DETAIL LINES                           
         MVI   PRNTED,C'Y'         SET PRINTED                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT CHANGE DESCRIPTION                              
         USING TANXD,R4            R4=A(NETWORK TRANSFER DETAILS ELE)           
         SPACE                                                                  
P1CHG    NTR1                                                                   
         XR    RF,RF               FIRST TIME INDICATOR                         
         LA    R1,PRTCHG           R1=A(PRINT CHANGE COLUMN)                    
         LA    RE,CHGTAB           RE=A(CHANGE TABLE)                           
         USING CHGTABD,RE                                                       
P1CHG5   CLI   CHGSTAT,X'FF'       TEST END OF TABLE                            
         BE    P1CHG10                                                          
         MVC   BYTE,TANXCCDE                                                    
         NC    BYTE,CHGSTAT        TEST MATCH ON CHANGE                         
         BZ    P1CHG8                                                           
         LTR   RF,RF               IF NOT FIRST TIME                            
         BZ    *+12                                                             
         MVI   0(R1),C','          SEPERATE CHANGES                             
         LA    R1,1(R1)            BUMP TO NEXT PRINT POSITION                  
         LA    RF,1(RF)            SET NOT FIRST TIME                           
         MVC   0(L'CHGDESC,R1),CHGDESC PRINT CHANGE DESCRIPTION                 
         LA    R1,L'CHGDESC-1(R1)  BUMP TO NEXT PRINT POSITION                  
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
P1CHG8   LA    RE,L'CHGTAB(RE)     BUMP TO NEXT CHANGE IN TABLE                 
         B     P1CHG5              LOOP                                         
*                                                                               
P1CHG10  GOTO1 SQUASHER,DMCB,PRTCHG,L'PRTCHG                                    
         B     XIT                                                              
         DROP  R4,RE                                                            
         EJECT                                                                  
*              ROUTINE TO PRINT TALENT CID                                      
P1CID    NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   PRTCID,TACOCID                                                   
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO MATCH ON METHOD                                       
         USING TANXD,R4            R4=A(NETWORK TRANSFER DETAILS ELE)           
         SPACE                                                                  
P1METHD  NTR1                                                                   
         LA    RE,MTCHTAB          RE=A(MATCHED METHOD TABLE)                   
P1METHD5 CLI   0(RE),X'FF'         TEST END OF TABLE                            
         BE    P1METHDX                                                         
         CLC   TANXTYPE,0(RE)      TEST MATCHED METHOD                          
         BE    *+12                                                             
         LA    RE,L'MTCHTAB(RE)    BUMP TO NEXT MATCHED METHOD                  
         B     P1METHD5            LOOP                                         
*                                                                               
         MVC   PRTMETHD,1(RE)      PRINT METHOD                                 
*                                                                               
         CLI   TANXTYPE,TANXTVER   PRINT VERSION LETTER                         
         BNE   P1METHDX                                                         
         MVC   PRTMETHD+5(1),TANXVERS                                           
P1METHDX B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT NETWORK TITLE                                   
         SPACE                                                                  
P2NTTL   NTR1                                                                   
         LA    R2,P2                                                            
         BAS   RE,CNTR             CENTER THE REPORT R2 SET ON XIT              
         USING PRNT2D,R2                                                        
*                                                                               
         LA    R4,SVTTTL           NETWORK ID IN SORT KEY                       
         LA    R3,L'SVTTTL                                                      
         CLI   REPTYPE,SRTTYPU     IF REPORT IS MATCHED                         
         BE    P2NTTL10                                                         
*                                                                               
         MVI   ELCODE,TAFNELQ      NETWORK ID IN RECORD                         
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTTTL))                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,TGELEM                                                        
         USING TAFND,R1                                                         
         LA    R4,TAFNNAME                                                      
         ZIC   R3,TAFNLEN                                                       
         SH    R3,=Y(TAFNLNQ)                                                   
         DROP  R1                                                               
*                                                                               
P2NTTL10 GOTO1 CHOPPER,DMCB,((R3),(R4)),(38,BLOCK),2                            
         MVC   PRTNIDN,BLOCK                                                    
         LA    R2,L'P2(R2)                                                      
         MVC   PRTNIDN,BLOCK+38                                                 
*                                                                               
P2NTTLX  B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO PRINT TALENT TITLE                                    
         SPACE                                                                  
P2TTTL   NTR1                                                                   
         LA    R2,P2                                                            
         BAS   RE,CNTR             CENTER THE REPORT R2 SET ON XIT              
         USING PRNT2D,R2                                                        
*                                                                               
         MVC   PRTCIDN,SVTTTL      TALENT TITLE                                 
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO DETAIL LINES (NETWORK USE OR MARKET DETAILS)          
         SPACE 1                                                                
PDTLINE  NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TANPELQ                                                   
         BAS   RE,PRTFRST          PRINT THE FIRST THREE LINES                  
         BAS   RE,PRTREM           PRINT REMAINING LINES                        
         CLI   ELCODE,TANPELQ                                                   
         BNE   *+8                                                              
         BAS   RE,PRTUCNT          PRINT ITS USE COUNT                          
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT FIRST THREE DETAIL LINES                        
         SPACE                                                                  
         USING PRNTD,R2            R2=A(PRINT LINE)                             
PRTFRST  NTR1                                                                   
         LA    R0,3                                                             
         BAS   RE,GETEL                                                         
         BE    PRTFRST5                                                         
         DC    H'0'                                                             
*                                                                               
PRTFRST2 BAS   RE,NEXTEL                                                        
         BNE   PRTFRSTX                                                         
*                                                                               
PRTFRST5 CLI   0(R4),TANPELQ                                                    
         BNE   *+8                                                              
         BAS   RE,MVCUSE           PRINT USE INFO                               
         LA    R2,L'P(R2)          BUMP TO SECOND PRINT LINE                    
         BCT   R0,PRTFRST2                                                      
*                                                                               
PRTFRSTX BAS   RE,PRNTIT           PRINT THE FIRST AND SECOND LINE              
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              PRINT REMAINING DETAIL LINES                                     
*                                                                               
PRTREM   NTR1                                                                   
         LA    R2,P                                                             
         USING PRNTD,R2                                                         
         BAS   RE,CNTR             CENTER THE REPORT R2 SET ON XIT              
*                                                                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,2                                                             
PRTREM2  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         BCT   R0,PRTREM2                                                       
*                                                                               
PRTREM5  BAS   RE,NEXTEL           LOOK FOR 4TH & REMAINING ELEMENTS            
         BNE   XIT                                                              
         CLI   0(R4),TANPELQ                                                    
         BNE   *+8                                                              
         BAS   RE,MVCUSE           PRINT MARKET INFO                            
         BAS   RE,PRNTIT                                                        
         B     PRTREM5                                                          
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO MOVE USE TO PRINT LINE                                
         SPACE                                                                  
         USING TANPD,R4                                                         
         USING PRNTD,R2                                                         
MVCUSE   NTR1                                                                   
         LH    R3,USECNT           R3=(TOTAL NUMBER OF USES)                    
         LA    R3,1(R3)            INCREMENT USE COUNT                          
         STH   R3,USECNT           SAVE TEMPORARILY FOR HOOK ROUTINE            
*                                                                               
         CLI   REPTYPE,SRTTYPU     IF MATCHED                                   
         BE    MVCUSE40                                                         
         GOTO1 DATCON,DMCB,(1,TANPDATE),(5,PRTUSED)  USE DATE                   
*                                                                               
         CLI   TANPTYP,TANPNET     IF NOT CABLE                                 
         BE    MVCUSE10                                                         
         CLI   TANPTYP,TANPSPT     OR WILDSPOT                                  
         BE    MVCUSE20                                                         
         CLI   TANPTYP,TANPCSYS    OR LOCAL CABLE                               
         BE    MVCUSE30                                                         
         MVC   PRTPGM,TANPPNME     PROGRAM NAME                                 
         MVC   PRTNWK,TANPNWK      NETWORK                                      
         B     MVCUSEX                                                          
*                                                                               
MVCUSE10 MVC   PRTCNTI,TANPNTI     NTI CODE                                     
         EDIT  TANPCNTR,PRTCUSE,ALIGN=RIGHT                                     
         B     MVCUSEX                                                          
*                                                                               
MVCUSE20 MVC   PRTCMKT,TANPMKT     MARKET CODE                                  
*        EDIT  TANPCNTR,PRTCUSE,ALIGN=RIGHT                                     
         B     MVCUSEX                                                          
*                                                                               
MVCUSE30 MVC   PRTCSYS,TANPSYS     SYSTEM CODE                                  
*        EDIT  TANPCNTR,PRTCUSE,ALIGN=RIGHT                                     
         B     MVCUSEX                                                          
*                                  (UNMATCHED)                                  
MVCUSE40 GOTO1 DATCON,DMCB,(1,TANPDATE),(5,PRTUUSED)                            
*                                                                               
         CLI   TANPTYP,TANPNET     IF NOT CABLE                                 
         BE    MVCUSE50                                                         
         CLI   TANPTYP,TANPSPT     AND NOT WILDSPOT                             
         BE    MVCUSE60                                                         
         CLI   TANPTYP,TANPCSYS    AND NOT LOCAL CABLE                          
         BE    MVCUSE70                                                         
         MVC   PRTUPGM,TANPPNME    PROGRAM NAME                                 
         MVC   PRTUNWK,TANPNWK     NETWORK                                      
         B     MVCUSEX                                                          
*                                                                               
MVCUSE50 MVC   PRTUCNTI,TANPNTI    NTI CODE                                     
         EDIT  TANPCNTR,PRTUCUSE,ALIGN=RIGHT                                    
         B     MVCUSEX                                                          
*                                                                               
MVCUSE60 MVC   PRTUCMKT,TANPMKT    MARKET CODE                                  
*        EDIT  TANPCNTR,PRTUCUSE,ALIGN=RIGHT                                    
         B     MVCUSEX                                                          
*                                                                               
MVCUSE70 MVC   PRTUCSYS,TANPSYS    NTI CODE                                     
*        EDIT  TANPCNTR,PRTUCUSE,ALIGN=RIGHT                                    
         B     MVCUSEX                                                          
*                                                                               
MVCUSEX  B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO PRINT USE COUNT                                       
         SPACE                                                                  
PRTUCNT  NTR1                                                                   
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
         BAS   RE,CNTR             CENTER THE REPORT, R2 SET ON XIT             
*                                                                               
         BAS   RE,CBLCHK           EXIT IF CABLE USE                            
         BE    PRTUCNTX                                                         
         BAS   RE,WSPCHK           WSP USE                                      
         BE    PRTUCNTX                                                         
         BAS   RE,LCBCHK           OR LCB USE                                   
         BE    PRTUCNTX                                                         
*                                                                               
         LH    RE,USECNT                                                        
         CH    RE,=H'1'            IF USE COUNT                                 
         BNH   PRTUCNTX                                                         
         LA    R3,PRTPGM+6         PRINT NUMBER OF USES                         
         CLI   REPTYPE,SRTTYPU                                                  
         BNE   *+8                                                              
         LA    R3,PRTUPGM+6                                                     
         EDIT  USECNT,(4,0(R3)),COMMAS=YES                                      
         MVC   5(4,R3),=C'USES'                                                 
         BAS   RE,PRNTIT           PRINT USE COUNT                              
PRTUCNTX XC    USECNT,USECNT       CLEAR FOR NEW COMMERCIAL                     
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO CENTER REPORT                                         
*                                  XIT R2=A(PRINT LINE)                         
         SPACE                                                                  
CNTR     NTR1                                                                   
         LA    R1,DSPUNMAT         R1=DISPLACEMENT FROM START OF LINE           
         CLI   REPTYPE,SRTTYPU     IF MATCHED REPORT                            
         BE    *+8                                                              
         LA    R1,DSPMAT           R1=DISPLACEMENT FROM START OF LINE           
CNTRX    AR    R2,R1                                                            
         XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
*              ROUTINE TO SET CORRECT SPECS                                     
         SPACE 1                                                                
SETSPRG  NTR1                                                                   
         MVI   RCSUBPRG,20         UNMATCHED REPORT                             
         CLI   REPTYPE,SRTTYPU                                                  
         BE    *+8                                                              
         MVI   RCSUBPRG,10         MATCHED REPORT                               
*                                                                               
         BAS   RE,CBLCHK                                                        
         BNE   SSPRG10                                                          
         MVI   RCSUBPRG,26         UNMATCHED REPORT                             
         CLI   REPTYPE,SRTTYPU                                                  
         BE    *+8                                                              
         MVI   RCSUBPRG,25         MATCHED REPORT                               
*                                                                               
SSPRG10  BAS   RE,WSPCHK                                                        
         BNE   SSPRG20                                                          
         MVI   RCSUBPRG,28         UNMATCHED REPORT                             
         CLI   REPTYPE,SRTTYPU                                                  
         BE    *+8                                                              
         MVI   RCSUBPRG,27         MATCHED REPORT                               
*                                                                               
SSPRG20  BAS   RE,LCBCHK                                                        
         BNE   XIT                                                              
         MVI   RCSUBPRG,30         UNMATCHED REPORT                             
         CLI   REPTYPE,SRTTYPU                                                  
         BE    *+8                                                              
         MVI   RCSUBPRG,29         MATCHED REPORT                               
         B     XIT                                                              
         EJECT                                                                  
*              PRINT OUT SUMMARY PAGE                                           
         SPACE                                                                  
PRTSUM   NTR1                                                                   
         MVI   FORCEHED,C'Y'       FORCE TO NEW PAGE                            
         MVI   RCSUBPRG,60         SUMMARY REPORT SPECS                         
         CLI   RECNUM,SX                                                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,61                                                      
         BAS   RE,PRNTIT           SKIP A LINE                                  
*                                                                               
         LA    R2,P                                                             
         EDIT  CNTUMAT,(12,0(R2)),ZERO=NOBLANK,COMMAS=YES                       
         MVC   13(21,R2),=C'UNMATCHED COMMERCIALS'                              
         BAS   RE,PRNTIT                                                        
*                                                                               
         EDIT  CNTMAT,(12,0(R2)),ZERO=NOBLANK,COMMAS=YES                        
         MVC   13(19,R2),=C'MATCHED COMMERCIALS'                                
         BAS   RE,PRNTIT                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              SET INFO FOR TRACE ROUTINE                                       
         SPACE                                                                  
MYTRACE  NTR1                                                                   
         TM    NOPTION,NTRACE      IF TRACE ON                                  
         BNO   XIT                                                              
         L     R2,0(R1)            A(LITERAL)                                   
         L     RF,8(R1)            SET LENGTH OF RECORD                         
         L     R4,4(R1)            A(RECORD)                                    
         GOTO1 PRNTBL,DMCB,(R2),(R4),C'DUMP',(RF),=C'2D'                        
         B     XIT                                                              
         SPACE 1                                                                
*              SET INFO FOR TRACE ROUTINE                                       
         SPACE                                                                  
MYTRAC2  NTR1                                                                   
         TM    NOPTION,NTRACE      IF TRACE ON                                  
         BNO   XIT                                                              
         LM    R2,R3,0(R1)         R2=A(LITERAL), R3=A(I/O AREA)                
         ZIC   R4,0(R1)            R4=L'LITERAL                                 
         GOTO1 TRACE,DMCB,(R3),0,(R2),(R4)                                      
         B     XIT                                                              
         SPACE 1                                                                
*              PRINT A LINE                                                     
         SPACE                                                                  
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE                                                                  
*              INCREMENT TANPD ELEMENT SEQUENCE NUMBER                          
         SPACE                                                                  
INCSEQ   NTR1                                                                   
         LH    R1,SEQCNT           INCREMENT SEQUENCE NUMBER                    
         LA    R1,1(R1)                                                         
         STH   R1,SEQCNT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT START LOGO PAGES                                
         SPACE                                                                  
PRTSLOGO NTR1                                                                   
         MVI   FORCEHED,C'Y'       FORCE TO NEW PAGE                            
*                                                                               
         L     R2,NLOGOC           R2=A(LOGOC)                                  
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'S'       START LOGOS                                  
         MVC   LOGO2,SPACES        SET TAL AGENCY CODE, NAME & ADDRESS          
         MVC   LOGO2(L'TGAGY),TGAGY                                             
         MVC   LOGONAME,AGYNAME                                                 
         MVC   LOGOADD,AGYADD1                                                  
         MVC   LOGOADD2,AGYADD2                                                 
         MVC   LOGOADD3,AGYADD3                                                 
*                                                                               
         GOTO1 NLOGO,DMCB,(R2)                                                  
         MVC   PAGE,=H'1'          START FROM PAGE 1                            
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO PRINT END LOGO PAGES                                  
         SPACE                                                                  
PRTELOGO NTR1                                                                   
         L     R2,NLOGOC           R2=A(LOGOC)                                  
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'       END LOGOS                                    
         GOTO1 NLOGO,DMCB,(R2)                                                  
         MVI   FORCEHED,C'Y'       FORCE TO NEW PAGE                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*              VARIOUS SORT ROUTINES                                            
*                                                                               
         SPACE 2                                                                
*              ROUTINE TO PUT RECORD TO SORTER                                  
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         LH    R2,SRTLEN           LENGTH OF SORT RECORD                        
         SH    R2,=H'4'                                                         
         GOTO1 MYTRACE,DMCB,=C'PUT TO SORT',SRTREC+4,(R2)                       
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SRTREC                                       
         B     XIT                                                              
         SPACE 2                                                                
*              CLEAR SORT RECORD                                                
         SPACE 1                                                                
CLRSREC  NTR1                                                                   
         LA    RE,SRTREC           CLEAR SORT RECORD AREA                       
         LA    RF,L'SRTREC                                                      
         XCEFL                                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              SORT RECORD LENGTH IS TRUE WORKER FILE RECORD LENGTH             
*              PLUS LENGTH OF SORT KEY                                          
         SPACE 1                                                                
SETSLEN  NTR1                                                                   
         LH    RE,WREC             LEN = L'WRKR - L'HDR + L'SORT KEY            
         SH    RE,=H'4'                                                         
         LA    RE,SRTKLNQ(RE)                                                   
         LA    RE,1(RE)             + 1 FOR END OF RECORD                       
         STH   RE,SRTLEN                                                        
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO GET A RECORD FROM SORT                                
*                                  XIT - R6=A(SORT RECORD)                      
         SPACE 1                                                                
GETSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R6,15,4(R1)         R6=A(SORT RECORD)                            
         BZ    GETSORTX                                                         
         LH    R2,SRTLEN           LENGTH OF SORT RECORD                        
         SH    R2,=H'4'                                                         
         GOTO1 MYTRACE,DMCB,=C'GET FROM SORT',SRTRECD+4,(R2)                    
GETSORTX XIT1  REGS=(R6)                                                        
         EJECT                                                                  
*                                                                               
*              WORKER FILE I/O ROUTINES                                         
*                                                                               
         SPACE 3                                                                
*              ROUTINE TO READ A RECORD IN THE CURRENT WORKER FILE              
*                                  SAME ROUTINE FOR OLD & NEW STYLE             
WKREAD   NTR1                                                                   
         LA    RE,WREC             CLEAR WORKER FILE RECORD AREA                
         LH    RF,=AL2(L'WREC)                                                  
         XCEFL                                                                  
*                                                                               
         MVC   COMMAND2,=CL8'READ'                                              
         GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,WREC,AWRKBUFF               
         CLI   DMCB+8,0            IF RECORD FOUND                              
         BNE   NO                                                               
*                                                                               
         LH    R2,WREC             LENGTH OF RECORD                             
         SH    R2,=H'4'                                                         
         GOTO1 MYTRACE,DMCB,=C'READ WORKER RECORD',WREC+4,(R2)                  
         B     YES                                                              
         SPACE 2                                                                
*              ROUTINE TO TRY TO FIND A WORKER FILE                             
*                                                                               
INDEXRD  NTR1                                                                   
         MVC   COMMAND2,=CL8'INDEX'                                             
INDRD10  GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,WREC,AWRKBUFF               
         CLI   8(R1),0             IF NO ERROR CONDITION CODE                   
         BNE   NO                                                               
*                                                                               
         LA    R3,L'INDEX                                                       
         GOTO1 MYTRACE,DMCB,=C'POSSIBLE INDEX',INDEX,(R3)                       
         B     YES                                                              
*              ROUTINE REREADS INDEX OF CURRENT FILE                            
         SPACE 1                                                                
INDRERD  NTR1                                                                   
         MVC   COMMAND2,=CL8'INDEX'                                             
         GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,WREC,AWRKBUFF               
*                                                                               
         LA    R3,L'INDEX                                                       
         GOTO1 MYTRACE,DMCB,=C'REREAD OF INDEX',INDEX,(R3)                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT A WORKER FILE ON KEEP                             
         SPACE 1                                                                
KEEP     MVC   COMMAND2,=CL8'KEEP'                                              
         LA    R3,RECORD                                                        
         B     GO                                                               
         SPACE 2                                                                
*              ROUTINE TO CLEAR THE NUMBER OF RETENTION DAYS                    
         SPACE 1                                                                
RETAIN   MVC   COMMAND2,=CL8'RETAIN'                                            
         LA    R3,RECORD                                                        
         LA    R1,28(R3)                                                        
         USING WKRECD,R1                                                        
         MVC   WKRETN,=AL2(1)      RETAIN FOR ANOTHER DAY                       
         B     GO                                                               
         DROP  R1                                                               
         SPACE 2                                                                
*              ROUTINE TO CLOSE THE CURRENT WORKER FILE (NETWORK ONLY)          
         SPACE 1                                                                
CLOSE    LA    R3,RECORD                                                        
         MVC   COMMAND2,=CL8'CLOSE'                                             
         B     GO                                                               
*                                                                               
GO       NTR1                                                                   
         GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,(R3),AWRKBUFF               
         CLI   DMCB+8,0                                                         
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE/BOX ROUTINES (HEADHOOK)                                 
         SPACE                                                                  
HOOK     NTR1                                                                   
         TM    NOPTION,NOBOX       IF BOXES REQUESTED                           
         BO    *+8                                                              
         BAS   RE,INTBOX           INITIALIZE THEM                              
*                                                                               
         MVC   H3+9(L'TGAGY),TGAGY    SET AGENCY CODE AND NAME                  
         MVC   H3+17(L'AGYNAME),AGYNAME                                         
*                                                                               
*        MVC   H3+116(L'WAGYCD),WAGYCD                                          
         MVC   H3+116(L'WAGYCD2),WAGYCD2                                        
         MVC   H4+98(33),WAGYNAME                                               
*                                                                               
         MVC   H3+58(12),SPACES    SET TITLE FOR REPORT                         
         CLI   RCSUBPRG,60         IF SUMMARY REPORT                            
         BL    HOOK05                                                           
         CLI   RCSUBPRG,61                                                      
         BH    HOOK05                                                           
         MVC   H3+61(7),=C'SUMMARY'                                             
         B     HOOKX                                                            
*                                                                               
HOOK05   CLI   RCSUBPRG,70         IF SUMMARY REPORT                            
         BL    HOOK06                                                           
         CLI   RCSUBPRG,71                                                      
         BH    HOOK06                                                           
         MVC   H3+58(7),=C'SUMMARY'                                             
         B     HOOKX                                                            
*                                                                               
HOOK06   LA    R2,P                                                             
         USING PRNTD,R2                                                         
         BAS   RE,CNTR                                                          
         CLC   PRTNID,SPACES                                                    
         BNE   HOOK10                                                           
         L     R4,ANEWHOLD                                                      
         USING TLNXD,R4                                                         
         CLI   TLNXCD,TLNXCDQ      IF TRANSFER RECORD                           
         BNE   HOOK10                                                           
         CLI   RECNUM,SX                                                        
         BE    *+14                                                             
         OC    USECNT,USECNT       AND IF USES TO PRINT                         
         BZ    HOOK10                                                           
         MVC   PRTNID,TLNXNID      SET NETWORK ID (CONTINUED)                   
         TM    TLNXSTAT,TLNXPACK   IF PACKED, UNPACK                            
         BZ    HOOK08                                                           
         XC    WORK,WORK                                                        
         GOTO1 ATRPACK,DMCB,(C'U',TLNXNID),WORK                                 
         MVC   PRTNID(12),WORK                                                  
HOOK08   MVC   PRTCONT,=C'(CONTINUED)'                                          
*                                                                               
HOOK10   MVC   H4+9(L'SVCLI),SVCLI      TALENT OR NETWORK CLIENT CODE           
         MVC   H5+9(L'SVPRD),SVPRD      TALENT OR NETWORK PRODUCT CODE          
         MVC   H4+17(L'SVCLIN),SVCLIN   TALENT OR NETWORK CLIENT NAME           
         MVC   H5+17(L'SVPRDN),SVPRDN   TALENT OR NETWORK PRODUCT NAME          
         MVC   H6+9(L'SVMED),SVMED      MEDIA CODE                              
         MVC   H7+9(L'SVUSE),SVUSE      USE CODE                                
*                                                                               
         CLI   REPTYPE,SRTTYPU                                                  
         BE    HOOK20                                                           
         MVC   H3+61(7),=C'MATCHED'                                             
         B     HOOKX                                                            
*                                                                               
HOOK20   MVC   H3+60(9),=C'UNMATCHED'                                           
HOOKX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
INTBOX   NTR1                                                                   
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXYORN,C'N'                                                     
         CLI   RCSUBPRG,60         IF SUMMARY REPORT                            
         BNL   INTBOXX             DON'T WANT ANY BOXES                         
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+60,C'B'                                                  
*                                                                               
         LA    R2,BOXCOLS                                                       
         BAS   RE,CNTR             CENTER THE REPORT,R2 SET ON XIT              
         USING PRNTD,R2                                                         
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         CLI   REPTYPE,SRTTYPU                                                  
         BE    INTBOX5                                                          
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         BAS   RE,CBLCHK                                                        
         BE    INTBOX0C                                                         
         BAS   RE,WSPCHK                                                        
         BE    INTBOX0D                                                         
         BAS   RE,LCBCHK                                                        
         BE    INTBOX0D                                                         
         MVI   BC6,C'C'                                                         
         MVI   BR,C'R'                                                          
         B     INTBOXX                                                          
INTBOX0C MVI   BC6C,C'C'                                                        
         MVI   BRC,C'R'                                                         
         B     INTBOXX                                                          
INTBOX0D MVI   BC6C,C'R'                                                        
         B     INTBOXX                                                          
*                                                                               
*                                                                               
INTBOX5  MVI   UBC3,C'C'                                                        
         BAS   RE,CBLCHK                                                        
         BE    INTBOX5C                                                         
         BAS   RE,WSPCHK                                                        
         BE    INTBOX5C                                                         
         BAS   RE,LCBCHK                                                        
         BE    INTBOX5C                                                         
         MVI   UBC4,C'C'                                                        
         MVI   UBC5,C'C'                                                        
         MVI   UBR,C'R'                                                         
         B     INTBOXX                                                          
INTBOX5C MVI   UBC4C,C'C'                                                       
         MVI   UBC6C,C'C'                                                       
         MVI   UBRC,C'R'                                                        
         B     INTBOXX                                                          
*                                                                               
INTBOXX  B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO PRINT MIDDLE LINE                                     
         SPACE 1                                                                
BXBOT    NTR1                                                                   
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         MVI   BOXINIT,0                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              CHECK IF CABLE PROGRAM                                           
         SPACE 1                                                                
CBLCHK   NTR1                                                                   
         USING TLNXD,RE                                                         
         L     RE,AIO                                                           
         CLC   TLNXUSE,=C'CBL'                                                  
         BE    YES                                                              
         B     NO                                                               
         DROP  RE                                                               
*              CHECK IF WILDSPOT PROGRAM                                        
         SPACE 1                                                                
WSPCHK   NTR1                                                                   
         USING TLNXD,RE                                                         
         L     RE,AIO                                                           
         CLC   TLNXUSE,=C'WSP'                                                  
         BE    YES                                                              
         B     NO                                                               
         DROP  RE                                                               
*              CHECK IF LOCAL CABLE PROGRAM                                     
         SPACE 1                                                                
LCBCHK   NTR1                                                                   
         USING TLNXD,RE                                                         
         L     RE,AIO                                                           
         CLC   TLNXUSE,=C'LCB'                                                  
         BE    YES                                                              
         B     NO                                                               
         DROP  RE                                                               
         EJECT                                                                  
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRORXIT                                                         
*                                                                               
ERRMISS  MVI   ERROR,MISSING                                                    
         B     ERRORXIT                                                         
*                                                                               
ERRORXIT GOTO1 ERREX                                                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,124,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(1004,,,,189)'                         
         EJECT                                                                  
*                                                                               
*  CONSTANTS                                                                    
         SPACE 1                                                                
NHOLDLNQ EQU   90000                                                            
         SPACE 1                                                                
DSPUNMAT EQU   11                  DISPL FROM P TO CENTER IN UNMATCHED          
DSPMAT   EQU   4                   DISPL FROM P TO CENTER IN MATCHED            
SDSPUNM  EQU   10                  DISPL FROM P TO CENTER IN UNMATCHED          
SDSPMAT  EQU   1                   DISPL FROM P TO CENTER IN MATCHED            
MAXWRKFN EQU   C'8'                FOR NOW - NEED TO FIND REAL EQUATE           
TENTHSEC DC    F'11'               WAIT A LITTLE MORE THAN 1/10 SEC             
         SPACE 1                                                                
XFFS     DC    3X'FF'                                                           
         SPACE 2                                                                
*              ELEMENT TABLE                                                    
ELETAB   DS    0X                                                               
         DC    AL1(TANXELQ)                                                     
         DC    AL1(TAFNELQ)                                                     
         DC    AL1(TANPELQ)                                                     
         DC    AL1(TAMDELQ)                                                     
         DC    X'FF'                                                            
         SPACE                                                                  
*              TABLE OF MATCHED METHODS                                         
MTCHTAB  DS    0CL9                                                             
         DC    AL1(TANXTTAL),CL8'TAL CID'                                       
         DC    AL1(TANXTLFT),CL8'LIFT ID'                                       
         DC    AL1(TANXTAKA),CL8'ALIAS'                                         
         DC    AL1(TANXTALF),CL8'ALIAS(L)'                                      
         DC    AL1(TANXTVER),CL8'VERS'                                          
         DC    X'FF'                                                            
         SPACE                                                                  
*              TABLE OF CHANGE DESCRIPTIONS                                     
CHGTAB   DS    0CL11                                                            
         DC    AL1(TANXCCID),CL10'COMMERCIAL'                                   
         DC    AL1(TANXCDTE),CL10'DATE'                                         
         DC    AL1(TANXCPRD),CL10'PRODUCT'                                      
         DC    AL1(TANXCLEN),CL10'LENGTH'                                       
         DC    AL1(TANXCUNM),CL10'UNMARKED'                                     
**NO-OP**DC    AL1(TANXCMGD),CL10'MAKEGOOD'                                     
         DC    AL1(TANXCVFY),CL10'VERFIY'                                       
         DC    AL1(TANXCAIR),CL10'NOT AIRED'    ALWAYS KEEP THIS LAST           
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        SPECS                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SPROG 10,20,25,26,27,28,29,30,60,61,70,71                              
         SSPEC H1,2,RUN                                                         
         SSPEC H1,99,REPORT                                                     
         SSPEC H1,119,PAGE                                                      
         SSPEC H2,99,REQUESTOR                                                  
         SSPEC H3,99,C'TRANSFERRED FROM:'                                       
         SSPEC H3,2,C'AGENCY'                                                   
         SPACE 1                                                                
         SPROG 10,20,25,26,27,28,29,30                                          
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'MEDIA'                                                    
         SSPEC H7,2,C'USE'                                                      
         SPACE 1                                                                
         SPROG 10,20,25,26,60                                                   
         SSPEC H1,57,C'NETWORK INTERFACE'                                       
         SSPEC H2,57,17X'BF'                                                    
         SPACE 1                                                                
         SPROG 27,28,29,30,61,71                                                
         SSPEC H1,59,C'SPOT INTERFACE'                                          
         SSPEC H2,59,14X'BF'                                                    
         SPACE 1                                                                
         SPROG 10                                                               
         SSPEC H10,6,C'TRAFFIC ID AND TITLE'                                    
         SSPEC H10,45,C'LEN TALENT CID AND TITLE'                               
         SSPEC H10,86,C'METHOD   USE DATE PROGRAM NAME    NWK'                  
         SPACE 1                                                                
         SPROG 20                                                               
         SSPEC H10,13,C'TRAFFIC ID AND TITLE'                                   
         SSPEC H10,52,C'LEN USE DATE PROGRAM NAME    NWK'                       
         SSPEC H10,85,C'REASON FOR CHANGE'                                      
         SPACE 1                                                                
         SPROG 25                                                               
         SSPEC H10,6,C'TRAFFIC ID AND TITLE'                                    
         SSPEC H10,45,C'LEN TALENT CID AND TITLE'                               
         SSPEC H10,86,C'METHOD   USE DATE CNET   #USES'                         
         SPACE 1                                                                
         SPROG 26                                                               
         SSPEC H10,13,C'TRAFFIC ID AND TITLE'                                   
         SSPEC H10,52,C'LEN USE DATE CNET   #USES'                              
         SSPEC H10,81,C'REASON FOR CHANGE'                                      
         SPACE 1                                                                
         SPROG 27                                                               
         SSPEC H10,6,C'TRAFFIC ID AND TITLE'                                    
         SSPEC H10,45,C'LEN TALENT CID AND TITLE'                               
         SSPEC H10,86,C'METHOD   USE DATE MKT'                                  
         SPACE 1                                                                
         SPROG 28                                                               
         SSPEC H10,13,C'TRAFFIC ID AND TITLE'                                   
         SSPEC H10,52,C'LEN USE DATE MKT'                                       
         SSPEC H10,81,C'REASON FOR CHANGE'                                      
         SPACE 1                                                                
         SPROG 29                                                               
         SSPEC H10,6,C'TRAFFIC ID AND TITLE'                                    
         SSPEC H10,45,C'LEN TALENT CID AND TITLE'                               
         SSPEC H10,86,C'METHOD   USE DATE CSYS'                                 
         SPACE 1                                                                
         SPROG 30                                                               
         SSPEC H10,13,C'TRAFFIC ID AND TITLE'                                   
         SSPEC H10,52,C'LEN USE DATE CSYS'                                      
         SSPEC H10,81,C'REASON FOR CHANGE'                                      
         SPACE 1                                                                
         DC    X'00'                                                            
         SPACE 2                                                                
STIMERID DS    XL4                 FOR STIMERID ONLY                            
         SPACE 1                                                                
         DS    0D                  WORKER FILE RECORD                           
         DC    C'**WREC**'                                                      
WREC     DS    CL4096                                                           
         DS    0D                                                               
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**WRK1**'                                                      
WRKBUFF  DS    14336C                                                           
         EJECT                                                                  
*              ROUTINE ATTEMPTS TO CHANGE NSI CODES INTO ALPHA                  
*              CODES FOR TMKT RECORDS                                           
         SPACE 1                                                                
         USING TANXD,R4                                                         
SETTMKT  NTR1  BASE=*,LABEL=*                                                   
         CLI   TANXMED,C'T'                                                     
         BNE   STMKTX                                                           
         DROP  R4                                                               
         USING TANPD,R4                                                         
         LA    R4,WREC+4                                                        
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,FIRSTEL                                                       
         BNE   STMKTX                                                           
         CLI   TANPLEN,TANPLNQ3                                                 
         BNH   STMKTX                                                           
         CLI   TANPTYP,TANPSPT                                                  
         BNE   STMKTX                                                           
         SPACE 1                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(L'TANPMKT),TANPMKT                                           
         MVI   TGMTTYPE,C'T'                                                    
         GOTO1 RECVAL,DMCB,TLMTCDQ,(X'A4',DUB)                                  
         BNE   STMKTX                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         LR    R1,R4                                                            
         SPACE 1                                                                
         USING TASND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TASNELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   STMKTX                                                           
         SPACE 1                                                                
         USING TANPD,R1                                                         
         MVC   TANPMKT,TASNAME                                                  
         DROP  R4                                                               
         SPACE 1                                                                
         LHI   RE,L'TANPMKT                                                     
         LA    RF,TANPMKT                                                       
STMKT10  CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         MVI   0(RF),0                                                          
         LA    RF,1(RF)                                                         
         BCT   RE,STMKT10                                                       
STMKTX   XIT1                                                                   
*                                                                               
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CLEARS PROGRAM NAME FROM CABLE, WILDSPOT                 
*              AND LOCAL CABLE PROGRAM ELEMENTS                                 
         SPACE 1                                                                
CLRPRG   NTR1  BASE=*,LABEL=*                                                   
         USING TANPD,R4                                                         
         LA    R4,WREC+4         R4=A(WORKER FILE RECORD)                       
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,FIRSTEL        GET NETWORK PROGRAM DETAILS ELEMENT            
         BNE   CPX                                                              
         SPACE 1                                                                
         CLI   TANPLEN,TANPLNQ3  IF SPOT OR CABLE                               
         BNH   CPX                                                              
         XC    TANPPNME,TANPPNME DELETE PROGRAM NAME                            
         DROP  R4                                                               
CPX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE ATTEMPTS TO SET CYCLES IN HOLD RECORD                    
*              SOME USES REQUIRE CYCLES TO BE SET IN ORDER TO BE                
*              MATCHED                                                          
         SPACE 1                                                                
SETCYC   NTR1  BASE=*,LABEL=*                                                   
         USING TANPD,R4                                                         
         LA    R4,WREC+4         R4=A(WORKER FILE RECORD)                       
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,FIRSTEL        GET NETWORK PROGRAM DETAILS ELEMENT            
         BNE   SCYCYES                                                          
         SPACE 1                                                                
         CLI   TANPLEN,TANPLNQ3  ONLY WANT SPOT ELEMENTS                        
         BNH   SCYCYES                                                          
         CLI   TANPTYP,TANPSPT                                                  
         BNE   SCYCYES                                                          
         SPACE 1                                                                
         ST    R4,ATANPEL        SAVE ADDRESS OF THE TANPD ELEMENT              
         DROP  R4                                                               
         SPACE 1                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         MVC   TGCOM,TLCOCOM     SAVE INTERNAL COMMERCIAL NUMBER                
         DROP  R4                                                               
         SPACE 1                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ    IF COMMERCIAL BEING MATCHED                    
         BRAS  RE,GETEL                                                         
         BNE   SCYCYES                                                          
         MVI   SVCOTYPE,C'R'                                                    
         CLI   TACOMED,C'R'      IS NOT MEDIA RADIO                             
         BE    SCYC10                                                           
         MVI   SVCOTYPE,C'A'                                                    
         CLI   TACOTYPE,C'A'     OR TYPE ADDENDUM                               
         BNE   SCYCYES           EXIT                                           
         SPACE 1                                                                
SCYC10   MVC   TGCID,TACOCID     SAVE COMMERCIAL ID                             
         DROP  R4                                                               
         SPACE 1                                                                
         USING TANXD,R4                                                         
         LA    R4,WREC+4         R4=A(WORKER FILE RECORD)                       
         MVC   TGAGY,TANXAGY     SAVE AGENCY                                    
         MVI   TANXTYPE,TANXTCYC                                                
         DROP  R4                                                               
         SPACE 1                                                                
         USING TLDVD,R4                                                         
         LA    R4,KEY            READ THROUGH ALL ADVICE RECORDS                
         XC    KEY,KEY                                                          
         MVI   TLDVCD,TLDVCDQ                                                   
         MVC   TLDVAGY,TGAGY     FOR THIS AGENCY                                
         MVC   TLDVCID,TGCID     AND COMMERCIAL                                 
         GOTO1 HIGH                                                             
         B     SCYC30                                                           
SCYC20   GOTO1 SEQ                                                              
SCYC30   CLC   KEY(TLDVADV-TLDVD),KEYSAVE                                       
         BNE   SCYC60                                                           
         GOTO1 GETREC                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TADVD,R4                                                         
         L     R4,AIO            IGNORE THE ADVICE IF IT IS                     
         MVI   ELCODE,TADVELQ    VERIFIED,SENT,RECEIVED,PAID OR                 
         BRAS  RE,GETEL          COMPLETE                                       
         BNE   SCYC20                                                           
         CLI   TADVSTAT,0                                                       
         BNE   SCYC20                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,AIO            GET ADVICE USE DETAILS ELEMENT                 
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   SCYC20                                                           
         SPACE 1                                                                
         CLI   SVCOTYPE,C'R'     IF LOOKING FOR RADIO WSP ADVICES               
         BNE   SCYC40                                                           
         CLI   TAVUUSE,UWSP      ADVICE MUST BE RADIO WILDSPOT                  
         BNE   SCYC20                                                           
         SPACE 1                                                                
SCYC40   CLI   SVCOTYPE,C'A'     IF LOOKING FOR ADDENDUM WSP ADVICES            
         BNE   SCYC50                                                           
         CLI   TAVUUSE,UADW      ADVICE MUST BE ADDENDUM WILDSPOT               
         BNE   SCYC20                                                           
         SPACE 1                                                                
         USING TANPD,R2                                                         
SCYC50   L     R2,ATANPEL        IF USE DATE FITS WITHIN ADVICE'S               
         CLC   TANPDATE,TAVUCYCS CYCLE                                          
         BL    SCYC20                                                           
         CLC   TANPDATE,TAVUCYCE                                                
         BH    SCYC20                                                           
         MVC   TANPCYCS,TAVUCYCS WE HAVE FOUND OUR CYCLE                        
         MVC   TANPCYCE,TAVUCYCE                                                
         B     SCYC110                                                          
         DROP  R2,R4                                                            
         SPACE 1                                                                
         USING TLUHD,R4                                                         
SCYC60   LA    R4,KEY            PREPARE TO READ USAGE HISTORY                  
         XC    KEY,KEY           RECORDS                                        
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         SPACE 1                                                                
         CLI   SVCOTYPE,C'R'     READ RADIO WILDSPOT                            
         BNE   SCYC70                                                           
         MVC   TLUHUSE,=C'WSP'                                                  
         SPACE 1                 OR ADDENDUM WILDSPOT HISTORY                   
SCYC70   CLI   SVCOTYPE,C'A'                                                    
         BNE   SCYC80                                                           
         MVC   TLUHUSE,=C'ADW'                                                  
         SPACE 1                                                                
SCYC80   GOTO1 HIGH                                                             
         B     SCYC100                                                          
SCYC90   GOTO1 SEQ                                                              
SCYC100  CLC   KEY(TLUHINV-TLUHD),KEYSAVE                                       
         BNE   SCYCNO                                                           
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         USING TAUHD,R4                                                         
         L     R4,AIO            GET USAGE HISTORY ELEMENT                      
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   SCYC90                                                           
         SPACE 1                                                                
         USING TANPD,R2                                                         
         L     R2,ATANPEL        IF USE DATE FITS WITHIN HISTORY'S              
         CLC   TANPDATE,TAUHSTRT CYCLE                                          
         BL    SCYC90                                                           
         CLC   TANPDATE,TAUHEND                                                 
         BH    SCYC90                                                           
         MVC   TANPCYCS,TAUHSTRT WE HAVE FOUND OUR CYCLE                        
         MVC   TANPCYCE,TAUHEND                                                 
         DROP  R4                                                               
         SPACE 1                                                                
SCYC110  GOTO1 DATCON,DMCB,(1,TANPCYCS),(0,DUB)                                 
         SPACE 1                                                                
         MVC   TANPCLEN,=CL5'3DAY'        SET CYCLE LENGTH                      
         GOTO1 ADDAY,DMCB,DUB,WORK,2      AS 3 DAYS                             
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         CLC   TANPCYCE,TGDUB                                                   
         BE    SCYC120                                                          
         SPACE 1                                                                
         MVC   TANPCLEN,=CL5'1WK'         OR 1 WEEK                             
         GOTO1 ADDAY,DMCB,DUB,WORK,6                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         CLC   TANPCYCE,TGDUB                                                   
         BE    SCYC120                                                          
         SPACE 1                                                                
         MVC   TANPCLEN,=CL5'4WK'         OR 4 WEEKS                            
         GOTO1 ADDAY,DMCB,DUB,WORK,27                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         CLC   TANPCYCE,TGDUB                                                   
         BE    SCYC120                                                          
         SPACE 1                                                                
         MVC   TANPCLEN,=CL5'31DAY'       OR 31 DAYS                            
         GOTO1 ADDAY,DMCB,DUB,WORK,30                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         CLC   TANPCYCE,TGDUB                                                   
         BE    SCYC120                                                          
         SPACE 1                                                                
         MVC   TANPCLEN,=CL5'8WK'         OR 8 WEEKS                            
         GOTO1 ADDAY,DMCB,DUB,WORK,55                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGDUB)                                   
         CLC   TANPCYCE,TGDUB                                                   
         BE    SCYC120                                                          
         SPACE 1                                                                
         MVC   TANPCLEN,=CL5'13WK'  ELSE, 13 WEEKS                              
         DROP  R2                                                               
         SPACE 1                                                                
         USING TLCOPD,R4                                                        
SCYC120  LA    R4,KEY               RE-GET THE COMMERCIAL RECORD                
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,TGCOM                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOPKEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R4                                                               
         SPACE 1                                                                
SCYCYES  XR    RC,RC                                                            
SCYCNO   LTR   RC,RC                                                            
SCYCX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*                                                                               
TNID     DSECT                                                                  
*                                                                               
         DS    0A                                                               
NLOGOC   DS    A                   A(LOGOC)                                     
NLOGO    DS    A                   A(LOGO)                                      
PRNTBL   DS    A                   A(PRNTBL)                                    
ASPBLK   DS    A                   A(SAVED PASSIVE POINTER BLOCK)               
ANEWHOLD DS    A                   A(NETWORK TRANSFER/HOLD RECORD)              
AWRKBUFF DS    A                   A(WORKER FILE BUFFER)                        
ASRTEND  DS    A                   A(END OF SORT RECORD)                        
ATRPACK  DS    A                   A(TRPACK)                                    
*                                                                               
CNTMAT   DS    PL6                 COUNTER OF MATCHED COMML'S                   
CNTUMAT  DS    PL6                 COUNTER OF UNMATCHED COMML'S                 
*                                                                               
NOPTION  DS    XL1                                                              
NTRACE   EQU   X'80'               TRACE RECORDS                                
NOBOX    EQU   X'40'               NO BOXES                                     
NOPOST   EQU   X'20'               POSTINGS = NO                                
NRERUN   EQU   X'10'               RERUN=YES                                    
*                                                                               
MAINRUN  DS    XL1                 Y=MAIN RUN                                   
PROCEL   DS    XL1                 ELEMENT PROCESSING                           
PRNTED   DS    XL1                 Y=RECORD PRINTED                             
*                                                                               
SEQNO    DS    H                   REQUESTED SEQUENCE NUMBER TO PROCESS         
USECNT   DS    H                   COUNT OF USES PER COMMERCIAL                 
SEQCNT   DS    H                   TANPD SEQUENCE NUMBER                        
FRST     DS    CL1                 Y= FIRST FOR UNMATCHED REPORT                
WRNING   DS    CL1                 D=DUPLICATE - DON'T ADD                      
RERUNDY  DS    CL1                 RERUN DAY                                    
FRSTWFL  DS    CL1                 Y= FIRST WORKER FILE                         
PROCAGY  DS    XL2                 REQUESTED ONE AGY/USER ID TO PROCESS         
NEWCOM   DS    XL(L'TANXCOM)       INTERNAL COMMERCIAL NUMBER                   
NEWVER   DS    CL1                 NEW COMMERCIAL VERSION LETTER                
NEWCTYPE DS    XL(L'TANXTYPE)      COMMERCIAL TYPE                              
*                                                                               
SVCOTYPE DS    X                                                                
ATANPEL  DS    A                                                                
*                                                                               
WRKNM    DS    CL4                 WRK FILE SYS/PROGRAM/SUB-PROGRAM             
INDEX    DS    CL42                WORKER FILE INDEX                            
COMMAND2 DS    CL8                 WORKER FILE COMMAND                          
WRKFILEN DS    CL8                 WORKER FILE FILE                             
RECORD   DS    CL150                                                            
*                                                                               
REPTYPE  DS    CL(L'SRTTYPE)       SAVED CURRENT REPORT TYPE                    
SVNID    DS    CL(L'SRTNID)        SAVED SORT NETWORK ID                        
SVNCLI   DS    CL(L'SRTNCLI)       SAVED SORT NETWORK CLIENT                    
SVNPRD   DS    CL(L'SRTNPRD)       SAVED SORT NETWORK PRODUCT                   
SVCLI    DS    CL(L'SRTCLI)        SAVED CLIENT CODE (TAL OR NET)               
SVPRD    DS    CL(L'SRTPRD)        SAVED PRODUCT CODE (TAL OR NET)              
SVMED    DS    CL(L'SRTMED)        SAVED MEDIA                                  
SVUSE    DS    CL(L'SRTUSE)        SAVED USE                                    
SVCCDE   DS    CL(L'SRTCCDE)       SAVED USE STATUS                             
SVCLIN   DS    CL(L'SRTCLIN)       CLIENT NAME                                  
SVPRDN   DS    CL(L'SRTPRDN)       PRODUCT NAME                                 
SVTTTL   DS    CL(L'SRTTTL)        SAVED SORT TALENT TITLE                      
*                                                                               
TALAGY   DS    CL6                 REQUESTED TALENT AGENCY CODE                 
AGYNAME  DS    CL36                CURRENT TALENT AGENCY NAME                   
AGYSTA4  DS    XL1                 AGENCY STATUS # 4                            
AGYADD   DS    CL(3*33)            CURRENT TALENT AGENCY ADDRESS                
         ORG   AGYADD                                                           
AGYADD1  DS    CL33                                                             
AGYADD2  DS    CL33                                                             
AGYADD3  DS    CL33                                                             
*                                                                               
FROMMRGE DS    XL1                                                              
*                                                                               
WAGYNUM  DS    XL2                 CURRENT WORKER FILE NUMBER                   
WAGYCD   DS    CL6                 CURRENT WORKER FILE AGENCY CODE              
WAGYCD2  DS    CL8                                                              
WAGYNAME DS    CL36                CURRENT WORKER FILE AGENCY NAME              
*                                                                               
SKIP     DS    C                                                                
         DS    0D                  SORT RECORD                                  
SRTREC   DS    CL(1000+SRTKLNQ)                                                 
*                                                                               
         EJECT                                                                  
*              DSECT TO COVER ELETAB                                            
         SPACE                                                                  
ELETABD  DSECT                                                                  
ETABCD   DS    XL1                 ELEMENT CODE                                 
ETABLNQ  EQU   *-ELETABD                                                        
         SPACE 2                                                                
*              DSECT TO COVER CHGTAB                                            
         SPACE                                                                  
CHGTABD  DSECT                                                                  
CHGSTAT  DS    XL1                 CHANGE STATUS                                
CHGDESC  DS    CL10                CHANGE DESCRIPTION                           
CHGTLNQ  EQU   *-CHGTABD                                                        
         SPACE 2                                                                
*              DSECT TO COVER SRTREC                                            
         SPACE                                                                  
SRTRECD  DSECT                                                                  
SRTLEN   DS    XL2                 LENGTH OF RECORD                             
         DS    CL2                 SPARE                                        
*                                                                               
SRTKEY   DS    0X                  START OF SORT KEY                            
SRTTYPE  DS    XL1                 COMMERCIAL TYPE                              
SRTTYPU  EQU   X'04'               UNMATCHED                                    
SRTTYPM  EQU   X'20'               MATCHED COMMERCIAL                           
*                                                                               
SRTCLI   DS    CL6                 CLIENT CODE  (TALENT OR NETWORK)             
SRTPRD   DS    CL6                 PRODUCT CODE (TALENT OR NETWORK)             
SRTMED   DS    CL1                 MEDIA CODE                                   
SRTUSE   DS    CL3                 USE CODE                                     
SRTTTL   DS    CL76                COMML TITLE  (TALENT OR NETWORK)             
SRTID    DS    CL12                COMML ID     (TALENT OR NETWORK)             
*                                                                               
SRTNID   DS    CL8                 NETWORK COMMERCIAL ID (AKA)                  
SRTNCLI  DS    CL3                 NETWORK CLIENT                               
SRTNPRD  DS    CL3                 NETWORK PRODUCT (OR BINARY ZEROS)            
SRTCCDE  DS    XL1                 CHANGE CODE FOR USES                         
SRTUSEDT DS    XL3                 USE DATE (NET)                               
         ORG   SRTUSEDT                                                         
SRTMKT   DS    CL2                 BINARY MARKET NUMBER (SPOT)                  
         DS    XL1                                                              
*                                                                               
SRTCLIN  DS    CL36                CLIENT NAME  (TALENT OR NETWORK)             
SRTPRDN  DS    CL36                PRODUCT NAME (TALENT OR NETWORK)             
*                                                                               
SRTVER   DS    CL1                 VERSION LETTER                               
*                                                                               
SRTSTAT  DS    XL1                 SORT STATUS                                  
SRTSPACK EQU   X'80'               PACKED                                       
SRTKLNQ  EQU   *-SRTRECD                                                        
         EJECT                                                                  
PRNTD    DSECT                                                                  
BL       DS    C                                                                
PRTNID   DS    CL(L'SRTNID)        NETWORK COMMERCIAL ID                        
         DS    CL1                                                              
PRTCONT  DS    CL11                                                             
PRTWARN  DS    CL18                                                             
BC1      DS    C                                                                
PRTNLEN  DS    CL3                 NID LENGTH                                   
BC2      DS    C                                                                
*                                  (MATCHED FOR NETWORK)                        
PRTCID   DS    CL(L'TGCID)         TALENT COMMERCIAL ID                         
         DS    CL24                                                             
BC3      DS    C                                                                
PRTMETHD DS    CL8                 METHOD MATCHED                               
BC4      DS    C                                                                
PRTUSED  DS    CL8                 USE DATE                                     
BC5      DS    C                                                                
PRTPGM   DS    CL(L'TANPPNME)      PROGRAM NAME                                 
BC6      DS    C                                                                
         DS    CL1                                                              
PRTNWK   DS    CL1                 NETWORK                                      
         DS    CL1                                                              
BR       DS    C                                                                
         ORG   PRTPGM              (MATCHED FOR CABLE)                          
PRTCNTI  DS    CL4                                                              
         ORG   PRTCNTI                                                          
PRTCMKT  DS    CL4                                                              
         ORG   PRTCMKT                                                          
PRTCSYS  DS    CL6                                                              
BC6C     DS    C                                                                
         DS    C                                                                
PRTCUSE  DS    CL3                                                              
         DS    C                                                                
BRC      DS    C                                                                
         ORG   PRTCID              (UNMATCHED FOR NETWORK)                      
PRTUUSED DS    CL8                 USED DATE                                    
UBC3     DS    C                                                                
PRTUPGM  DS    CL15                PROGRAM                                      
UBC4     DS    C                                                                
         DS    CL1                                                              
PRTUNWK  DS    CL1                 NETWORK                                      
         DS    CL1                                                              
UBC5     DS    C                                                                
PRTCHG   DS    CL30                CHANGE DESCRIPTION                           
UBR      DS    C                                                                
         ORG   PRTUPGM             (UNMATCHED FOR CABLE)                        
PRTUCNTI DS    CL4                 NTI CODE                                     
         ORG   PRTUPGM                                                          
PRTUCMKT DS    CL4                 MARKET CODE                                  
         ORG   PRTUPGM                                                          
PRTUCSYS DS    CL6                 CABLE SYSTEM                                 
UBC4C    DS    C                                                                
         DS    C                                                                
PRTUCUSE DS    CL3                                                              
         DS    C                                                                
UBC6C    DS    C                                                                
PRTCCHG  DS    CL30                CHANGE DESCRIPTION                           
UBRC     DS    C                                                                
         EJECT                                                                  
*              DSECT FOR SECOND PRINT LINE                                      
         SPACE 1                                                                
PRNT2D   DSECT                                                                  
         DS    C                                                                
PRTNIDN  DS    CL38                NID TITLE                                    
         DS    CL5                                                              
PRTCIDN  DS    CL36                TALENT COMMERCIAL TITLE                      
*                                                                               
         ORG   PRTCIDN             (MATCHED REPORT FOR SPOT)                    
PRTCIDNS DS    CL25                TALENT COMMERCIAL TITLE                      
         EJECT                                                                  
*TAREPFFD                                                                       
*TAREPC1D                                                                       
*DDGENTWA                                                                       
*DDTWACOND                                                                      
*DMDTFIS                                                                        
*DDMASTD                                                                        
*DMWRKRK                                                                        
*DMWRKRD                                                                        
*DMWRKRL                                                                        
*DDSPOOLD                                                                       
*DDLOGOD                                                                        
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*TAREPWORKD                                                                     
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         ORG CONTAGH                                                            
       ++INCLUDE TAREPC1D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE DMWRKRD                                                        
*PREFIX=N =UKFILENO                                                             
       ++INCLUDE DMWRKFK                                                        
*PREFIX=                                                                        
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042TAREP41   10/04/16'                                      
         END                                                                    
