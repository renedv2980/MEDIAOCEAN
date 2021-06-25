*          DATA SET REDAR31    AT LEVEL 080 AS OF 11/24/03                      
*PHASE T80F31A                                                                  
*&&      SET   DB=N,XX=N,CR=Y,PR=Y,TK=Y,DR=N,TP=N,PN=N,ZC=Y                     
*INCLUDE REGENPBY                                                               
         TITLE 'T80F31 - REDAR31 - DARE REVISION REPORT'                        
***********************************************************************         
*                                                                     *         
*  REDAR31 (T80F31) --- DARE REVISION REPORT                          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 07NOV03 (HQ ) FIX REPORT "TOO BIG" ERROR                            *         
* 06AUG03 (HQ ) FIX REP MASTER LINE WITH NUL AGENCY LINK LINE DUMP    *         
* 22JAN03 (SKU) SUPPORT ALPHANUMERIC ESTIMATE                         *         
* 03DEC01 (SKU) ADD TRADE INDICATORS                                  *         
* 25OCT01 (SKU) MAKEGOOD PRINT BUG FIX                                *         
* 13APR01 (SKU) FIX BUG OF HANDLING CANCELLED/DELETED BUYS            *         
* 08JAN01 (SKU) CHECK FOR NON-BLANK OR NON-NULL IN BUY ROTATION FIELD *         
* 18JUL00 (SKU) SUPPOR REQUESTOR FIELD                                *         
* 17OCT97 (JRD) INITIAL RELEASE                                       *         
* 12NOV97 (JRD) TAKEOVER ORDER CHOPPING                               *         
*               MOVED AGENCY ORDER TOTALING INTO BUYDETL              *         
* 24NOV97 (JRD) FIX FOR CROSSLINKED BUYS (VIA APPROVE)                *         
* 05DEC97 (JRD) FIX FOR LOST AGENCY GRANDPARENTS                      *         
* 11FEB98 (JRD) FIX PROGRAM NAME READING ON REP RECORDS (LVL 32)      *         
*               FLAG TO DISABLE PROGRAM NAME CHECKING   (LVL 31)      *         
* 21APR98 (SKU) SUPPORT OFFLINE PRINTING                              *         
* 04JUN98 (SKU) UPDATE CALL TO REGENDAB AND REGENPBY                  *         
* 05AUG98 (JRD) MOVE UNMATCHED REP BUY LIST BUILD INTO AGY BUY LOOP   *         
* 17AUG98 (JRD) FIX SOFT MATCHING BLOCK BUG.                          *         
* 19OCT98 (JRD) IGNORE CANCELED LINE ON REP/AGY SIDES                 *         
* 25AUG99 (SKU) SKIP PRINTING IF NO TRAILER RECORD FOUND              *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
* SETTING DB = Y WILL TURN ON ADDITIONAL DEBUG PRINTING               *         
* SETTING DR = Y WILL DUMP AGENCY BUY DETAILS BEFORE & AFTER CHOPPING *         
* SETTING CR = N WILL TURN OFF RECORD COMPAREING                      *         
* SETTING PR = N WILL TURN OFF RECORD PRINTING                        *         
PRFILL   EQU   0                   SET TO 1 IF PR=N                   *         
* SETTING TK = N WILL TURN OFF TAKEOVER ORDER CHOPPING                *         
* SETTING XX = Y WILL ENABLE CODE THAT IS NOT TO BE USED RIGHT NOW    *         
* SETTING TP = Y WILL ENABLE TKO DATE PATCHING USING PATCHDT          *         
* SETTING PN = Y WILL ENABLE PROGRAM NAME CHECKING                    *         
* SETTING ZC = N WILL DISABLE ZERO SPOTS/CANCEL CHECK ON AGENCY BUYS  *         
*                                                                     *         
* SYSSPARE RESERVED BY REDAR01!!!!!!!!!!                              *         
*                                                                     *         
* USES RA FOR SECONDARY WORKING STORAGE REGISTER                      *         
*                                                                     *         
* THIS EQUATE SETS THE NUMBER OF TSAR PAGES REQUESTED OFFLINE:        *         
TSARPGS  EQU   86                                                     *         
*                                                                     *         
***********************************************************************         
T80F31   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLQ,*T80F31*,R7,RR=R3,CLEAR=YES                               
         LR    R5,RC                                                            
         USING MYAREAD,R5          R5 = A(OVERLAY STORAGE AREA)                 
         LR    RA,R5               RA = A(SECOND PORTION OF WS)                 
         AH    RA,=Y(MYAREAD2-MYAREAD)                                          
         USING MYAREAD2,RA                                                      
*                                                                               
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         ST    R3,RELO                                                          
         LR    R0,R5                                                            
         AH    R0,=Y(LISTAREA-MYAREAD)                                          
         ST    R0,ALSTAREA                                                      
         LR    R0,R5                                                            
         AH    R0,=Y(LISTARE2-MYAREAD)                                          
         ST    R0,ALSTARE2                                                      
*                                                                               
         LR    R0,R5                                                            
         AH    R0,=Y(TLBUFF-MYAREAD)                                            
         ST    R0,ATLBUFF                                                       
*                                                                               
         MVC   PRTSTAT,4(R1)       PRINT STATUS                                 
         XC    CMTFLAG,CMTFLAG                                                  
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP ONLINE STUFF                            
         BE    INIT010                                                          
*                                                                               
         ICM   R0,14,=X'D9000A'                                                 
         ICM   R0,1,=AL1(QTSAR)                                                 
         GOTOX CALLOV,(R1),0,(R0)                                               
         MVC   VTSAR,0(R1)         VTSAR                                        
         B     INIT020                                                          
*                                                                               
INIT010  DS    0H                                                               
         L     RF,ATWA                                                          
         L     RF,TWAMASTC-T80FFFD(RF)                                          
         L     RF,MCVLOADR-MASTD(RF)                                            
         GOTOX (RF),DMCB,=CL8'T00A7D'                                           
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,VTSAR            LOAD TSAROFF                                 
*                                                                               
INIT020  DS    0H                                                               
         L     RE,=V(REGENPBY)                                                  
         A     RE,RELO                                                          
         ST    RE,REGENPBY                                                      
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         CLI   MODE,PRINTREP                                                    
         BNE   EXIT                                                             
*                                                                               
         B     PR                                                               
         DC    H'0'                                                             
         SPACE 3                                                                
*-------------------------------------------                                    
AGYCOLQ  EQU   0                                                                
REPCOLQ  EQU   132/2                                                            
FF       EQU   X'FF'                                                            
*-------------------------------------------                                    
T2       USING TLSTD,TLST                                                       
TS1      USING TLSTD,TLSTSAV                                                    
TS2      USING TLSTD,TLSTSAV2                                                   
TK1      USING TLKEY,TLSVKEY                                                    
TK2      USING TLKEY,TLSVKEY2                                                   
PL1      USING PLINED,P                                                         
PL2      USING PLINED,P2                                                        
PL3      USING PLINED,P3                                                        
PL4      USING PLINED,P4                                                        
         EJECT                                                                  
***********************************************************************         
* PRINT A REPORT                                                                
* DARE HEADER RECORD WILL BE READ INTO IO1                                      
***********************************************************************         
PR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        SKIP ONLINE STUFF                            
         BE    PR00                                                             
*                                                                               
         TM    PRTSTAT,PRTCLOSE    CLOSE THE PRINTQ?                            
         BNO   *+12                NO                                           
         BAS   RE,PQCLOSE                                                       
         B     PRX                 CLEAN UP AFTER PRINTING                      
*                                                                               
         TM    PRTSTAT,PRTNEWPG    PQ OPEN?                                     
         BO    *+12                                                             
         BAS   RE,PQOPEN           NO                                           
         B     *+8                                                              
         MVI   FORCEHED,C'Y'       YES - FORCEHEAD ON NEXT REPORT               
*                                                                               
         B     PR02                                                             
*                                                                               
PR00     DS    0H                                                               
         LA    R0,TSARPGS          GET OFFLINE TSAR                             
         SLL   R0,12                                                            
*                                                                               
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                GETMAIN FAILED RC IN (RF)                    
                                                                                
         STCM  R1,15,TBUFFADR      SAVE UP A(TSAR BUFFER)                       
         STCM  R0,15,TBUFFLEN      SAVE LENGTH OF BUFFER                        
*                                                                               
PR02     DS    0H                                                               
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         OC    SELECTKY,SELECTKY   *** SHOULD NEVER HAPPEN !                    
         BZ    PRX                                                              
*                                                                               
         MVI   LSTSINDS,0                                                       
         BAS   RE,XCTLST                                                        
         GOTOX TSAR,TSAINI                                                      
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         NI    MISCFLAG,X'FF'-MSFTRADE                                          
         CLI   RDARCORT,C'T'       FLAG IF TRADE ORDER                          
         BNE   *+8                                                              
         OI    MISCFLAG,MSFTRADE                                                
                                                                                
         MVC   PRBUYKEY,RDARKEY    INIT BUY KEY                                 
         MVC   REVNUM,RDARRNUM                                                  
         MVC   REVDATE,RDARDATE                                                 
         MVC   REVTIME,RDARTIME                                                 
*                                                                               
         ZAP   TOTRSPTS,=P'0'                                                   
         ZAP   TOTRCOST,=P'0'                                                   
         ZAP   TOTASPTS,=P'0'                                                   
         ZAP   TOTACOST,=P'0'                                                   
*                                                                               
         ZAP   TOTXSPTS,=P'0'                                                   
         ZAP   TOTXCOST,=P'0'                                                   
*                                                                               
         MVI   METHOD,0                                                         
         CLI   STAMETH,0                                                        
         BE    *+14                                                             
         MVC   METHOD,STAMETH                                                   
         B     PR10                                                             
*                                                                               
         L     R4,ATWA             USE R2 TO COVER THE ENTRY                    
         AH    R4,=AL2(DARPROFS-CONHEADH)                                       
         USING SVDSECT,R4                                                       
*                                                                               
         TM    SVPGPBIT+CNTDRV1B,CNTDRV1A                                       
         BZ    *+12                                                             
         MVI   METHOD,1                                                         
         B     PR10                                                             
*                                                                               
         TM    SVPGPBIT+CNTDRV2B,CNTDRV2A                                       
         BZ    *+12                                                             
         MVI   METHOD,2                                                         
         B     PR10                                                             
*                                                                               
         TM    SVPGPBIT+CNTDRV3B,CNTDRV3A                                       
         BZ    *+12                                                             
         MVI   METHOD,3                                                         
         B     PR10                                                             
         DROP  R4                                                               
*                                                                               
PR10     DS    0H                                                               
         XC    SVCONNUM,SVCONNUM                                                
         OC    RDARREP#,RDARREP#   NOT LINKED TO A CONTRACT                     
         BNZ   *+6                                                              
         DC    H'0'                UH, HOW DID WE GET HERE GEORGE?              
*                                                                               
         MVC   SVCONNUM,CCONKNUM   SAVE OFF CON NUM, SO WE CAN RESTORE          
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),RDARREP#                                              
         XC    FAKEHDR,FAKEHDR     SETUP FAKE FIELD HEADER                      
         LA    R2,FAKEHDR           TO CALL VCON WITH                           
         EDIT  (P5,WORK+20),(8,8(R2)),ALIGN=LEFT                                
         STC   R0,5(R2)            SET LENGTH OF FIELD                          
         MVI   0(R2),16            FIELD HEADER LENGTH                          
         MVI   4(R2),X'08'         SET VALID NUMERIC                            
         GOTO1 VALICON,DMCB,(R2)                                                
*                                                                               
         LA    R6,KEY              PUT CONTRACT RECORD IN IO2                   
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,CCONNUM                                                 
         DROP  R6                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    KEY,KEY             CHECK FOR TAKEOVERS                          
         LA    RE,KEY                                                           
         USING RSTAKEY,RE                                                       
         L     RF,AIO2                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,RCONKREP-RCONREC(RF)                                    
         MVC   RSTAKSTA,RCONKSTA-RCONREC(RF)                                    
         DROP  RE                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AIO1                                                          
         MVC   TKODATE,RSTASTRT-RSTAREC(RE)                                     
*                                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                GET THE DARE RECORD                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AIO1                                                          
         LA    R0,RDARESST-RDARREC(RE)                                          
         GOTO1 DATCON,DMCB,(2,(R0)),(3,WORK)                                    
         CLC   TKODATE,WORK                                                     
         BNL   *+10                                                             
         XC    TKODATE,TKODATE     CLEAR TKODATE - ORDER AFTER JOIN             
*&&TP                                                                           
         B     *+10                                                             
PATCHDT  DC    C'980105'                                                        
         GOTO1 DATCON,DMCB,(0,PATCHDT),(3,TKODATE)                              
*&&                                HARD CODED TKO DATE - JRD                    
*                                                                               
         GOTO1 =A(READRECS),RR=Y   READ BUYS INTO TSAR & TOTAL                  
*&&CR                                                                           
         GOTO1 =A(CMPRECS),RR=Y    COMPARE BUY DETAILS                          
*&&                                                                             
         L     R6,AIO2             CHECK FOR MANUAL METHOD REQ'D                
         MVI   ELCODE,X'1D'        RETRIEVE                                     
         USING RCONDREL,R6                                                      
         BAS   RE,GETEL6                                                        
         BNE   PR20                                                             
*                                                                               
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    PR20                                                             
         TM    RCONDRF2,X'04'                                                   
         BZ    PR20                                                             
         MVI   METHOD,0                                                         
         DROP  R6                                                               
*                                                                               
PR20     DS    0H                                                               
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                RESTORE THE DARE RECORD                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         NI    PRTSTAT,X'FF'-PRTPAGE1                                           
         GOTO1 =A(PRPAGE1),RR=Y    PRINT PAGE HEADINGS                          
         OI    PRTSTAT,PRTPAGE1                                                 
*&&DB                                                                           
         GOTOX (0,=A(PRNTTSAR)),RR=Y   PRINT TSAR FOR DEBUGING                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   PL1.P1TAG,=C'--------------------'                               
         MVC   PL1.P1XTAG,=C'....................'                              
         MVC   PL1.P1BUYLN,=C'--------------------'                             
         MVC   PL1.P1DAYS,=C'--------------------'                              
         MVC   PL1.P1TIMES,=C'--------------------'                             
         MVC   PL1.P1LEN,=C'--------------------'                               
         MVC   PL1.P1DATES,=C'--------------------'                             
         MVC   PL1.P1NPW,=C'--------------------'                               
         MVC   PL1.P1SPRATE,=C'--------------------'                            
         MVC   PL1.P1TOTSPT,=C'--------------------'                            
         MVC   PL1.P1TOTRAT,=C'--------------------'                            
         MVC   PL1.P1CLASS,=C'--------------------'                             
         MVC   PL1.P1REMARK,=C'--------------------'                            
         BAS   RE,PRINT                                                         
*&&                                                                             
*---------------*                                                               
* PRINT RECORDS                                                                 
*---------------*                                                               
*&&PR                                                                           
         BAS   RE,XCTLST                                                        
         GOTO1 TSAR,TSARDH         ANY RECORDS IN TSAR?                         
         BL    PR600               NO                                           
*                                                                               
         MVC   PCOMFACS,ACOMFACS   SET UP ROUTINE ADDRESS BLOCK                 
         MVC   PUNDAY,VOUTDAY                                                   
         MVC   PUNTIME,UNTIME                                                   
         MVC   PDEMOCON,DEMOCON                                                 
*                                                                               
         MVC   P2UNDAY,VOUTDAY                                                  
         MVC   P2UNTIME,UNTIME                                                  
         MVC   P2DATCON,DATCON                                                  
         MVC   P2ADDAY,ADDAY                                                    
*                                                                               
         NI    MISCFLG1,FF-(MF1UNMA+MF1UNMR+MF1SFTM)                            
         NI    MISCFLG2,FF-(MF2CHGS)                                            
         XC    LASTMAST,LASTMAST                                                
         MVI   LASTAGY,0                                                        
PR090    DS    0H                                                               
         CLI   T2.TLKTYP,X'40'                                                  
         BNL   PR600               DONE PRINTING FROM TSAR                      
*&&DB                                                                           
         CLI   T2.TLKTYP,X'02'     SOFT MATCHED BUYS?  *DEBUG *********         
         BNE   PR092               NO                  *DEBUG *********         
         TM    MISCFLG1,MF1SFTM    MSG PRINTED?        *DEBUG *********         
         BO    PR092               YES                 *DEBUG *********         
*                                                 *DEBUG **************         
         OI    MISCFLG1,MF1SFTM                   *DEBUG **************         
         MVI   P,0                                *DEBUG **************         
         MVC   P2(L'SOFTMSG),SOFTMSG              *DEBUG **************         
         MVI   P3,0                               *DEBUG **************         
         BAS   RE,PRINT                           *DEBUG **************         
*&&                                                                             
PR092    DS    0H                                                               
         CLI   T2.TLKSORT,0        AGYENCY LINE?                                
         BNE   PR200               NO                                           
*                                                                               
*--------------*                                                                
* AGY BUY LINE                                                                  
*--------------*                                                                
         NI    MISCFLG2,FF-MF2INCOM                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),T2.TLDSKAD                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AIO              SETUP PRINT CALL                             
         MVC   PRBUYKEY,0(RE)                                                   
         XC    PRBUYKEY+27(L'PRBUYKEY-27),PRBUYKEY+27                           
         MVC   BUYNUM,T2.TLKABUY                                                
*&&TK                                                                           
         OC    TKODATE,TKODATE     NEED TO CHOP & PRINT?                        
         BNZ   PR100               YES                                          
*&&                                                                             
         GOTO1 VREGENDB,DMCB,(X'94',PRBUYKEY),(32,ALSTAREA),           X        
               (BUYNUM,AIO3),PARMS                                              
         B     PR101                                                            
*                                                                               
PR100    DS    0H                                                               
         MVC   WORK(4),VREDARTK                                                 
         MVC   WORK+4(4),PERVERT                                                
         MVC   WORK+8(3),TKODATE                                                
         GOTO1 VREGENDB,DMCB,(X'D4',PRBUYKEY),(32,ALSTAREA),           X        
               (BUYNUM,AIO3),PARMS,WORK                                         
         CLI   DMCB+8,0            DELETED VIA TKO?                             
         BNE   PR101               NO                                           
*&&DB                                                                           
         MVC   P(19),=C'XXX BUY XXX TKO DEL'      *DEBUG **************         
         MVC   P(3),=C'AGY'                       *DEBUG **************         
         EDIT  T2.TLKABUY,(3,P+8),ZERO=NOBLANK    *DEBUG **************         
         BAS   RE,PRINT                           *DEBUG **************         
*&&                                                                             
         MVI   LASTMAST,C'A'                                                    
         MVC   LASTMAST+1(1),T2.TLKACMB                                         
         B     PR500               YES - SKIP TO NEXT                           
*                                                                               
PR101    DS    0H                                                               
         NI    PRTSTAT,X'FF'-PRTDAILY                                           
         CLI   DMCB+10,0           CHECK IF DAILY                               
         BE    *+8                                                              
         OI    PRTSTAT,PRTDAILY                                                 
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,DMCB+8                                                      
         BNZ   *+6                                                              
         DC    H'0'                HOW DID THIS HAPPEN?                         
*                                                                               
         TM    T2.TLFLG1,TLF1DLQ   DELETED?                                     
         BZ    PR102               NO                                           
*&&DB                                                                           
         MVC   P(19),=C'XXX BUY XXX DELETED'      *DEBUG **************         
         MVC   P(3),=C'AGY'                       *DEBUG **************         
         CLI   T2.TLKTYP,X'11'                    *DEBUG **************         
         BNE   *+10                               *DEBUG **************         
         MVC   P+20(10),=C'UNMATCHED'             *DEBUG **************         
         EDIT  T2.TLKABUY,(3,P+8),ZERO=NOBLANK    *DEBUG **************         
         BAS   RE,PRINT                           *DEBUG **************         
*&&                                                                             
         MVI   LASTMAST,C'A'                                                    
         MVC   LASTMAST+1(1),T2.TLKACMB                                         
         B     PR500               NEXT TSAR RECORD                             
*                                                                               
PR102    DS    0H                                                               
         TM    T2.TLFLG1,TLF1MAQ   MAKEGOOD APPLIED?                            
         BNZ   PR112               YES - NOT CANCELED                           
*                                                                               
         L     R4,ALSTAREA         CHECK FOR ANY SPOTS                          
         USING LBUYD,R4                                                         
         LR    R6,R3               USE RF, NEED R3 FOR PRINTING                 
PR104    DS    0H                                                               
         CLC   =C'P=',09(R4)       PROGRAM TEXT                                 
         BE    PR108               YES                                          
         OC    0(9,R4),SPACES                                                   
         CLC   0(9,R4),SPACES      POTENTIAL COMMENT?                           
         BH    PR106               NO                                           
*                                                                               
         LA    RE,LBUYDAYS+L'LBUYDAYS                                           
         LA    R0,9(R4)                                                         
         SR    RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   9(0,R4),SPACES                                                   
         BH    PR108               COMMENT - SKIP SPOT CHECK                    
*                                                                               
PR106    DS    0H                                                               
         CLC   LBUYSPT,SPACES      ANY SPOTS?                                   
         BNH   PR108               NO SKIP                                      
*                                                                               
         GOTO1 CASHVAL,DMCB,(X'82',LBUYSPT),L'LBUYSPT                           
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CP    DMCB+4(8),=P'0'                                                  
         BH    PR112               FOUND SPOTS - NOT CANCELED                   
*                                                                               
PR108    DS    0H                  NEXT LINE                                    
         LA    R4,L'LISTAREA(R4)                                                
         BCT   R6,PR104                                                         
*&&ZC                              AGENCY LINE CANCELED(NO SPOTS)               
         CLI   T2.TLKTYP,X'11'     UNMATCHED CANCELS DON'T SHOW                 
*&&DB*&& B     *+8                 PRINT CANCEL     *DEBUG ************         
         BE    PR110                                                            
*                                                                               
         L     RE,=A(AGYCANCL)                                                  
         A     RE,RELO                                                          
         MVC   P2(L'AGYCANCL),0(RE)                                             
         EDIT  T2.TLKABUY,(3,P2+14),ZERO=NOBLANK                                
         BAS   RE,PRINT                                                         
*                                                                               
PR110    DS    0H                                                               
         MVI   LASTMAST,C'A'                                                    
         MVC   LASTMAST+1(1),T2.TLKACMB                                         
         B     PR500               NEXT TSAR RECORD                             
*&&                                                                             
PR112    DS    0H                  SPOTS OR MAKEGOOD APPLIED - PRINT            
         CLI   T2.TLKTYP,X'11'     UNMATCHED?                                   
         BNE   PR114               NO                                           
         TM    MISCFLG1,MF1UNMA    MSG PRINTED?                                 
         BO    PR114               YES                                          
*                                                                               
         OI    MISCFLG1,MF1UNMA                                                 
         MVI   P,0                                                              
         MVC   P2(UMAGYMLQ),UMAGYMSG                                            
         MVI   P3,0                                                             
         BAS   RE,PRINT                                                         
*                                                                               
PR114    DS    0H                                                               
         OI    MISCFLG2,MF2CHGS    SET CHANGES PRINTED                          
*                                                                               
         CLI   T2.TLKTYP,X'11'     UNMATCHED?                                   
         BE    PR116                                                            
*                                                                               
         CLI   LASTMAST,C'A'       CHECK ALREADY IN BLOCK                       
         BNE   *+14                                                             
         CLC   T2.TLKACMB,LASTMAST+1                                            
         BE    PR116                                                            
*                                                                               
         CLI   LASTMAST,0          FIRST TIME?                                  
         BE    PR116               YES                                          
*                                                                               
         MVI   P1,0                                                             
         MVI   P2,C'-'                                                          
         MVC   P2+1(L'P2-2),P2                                                  
         MVI   P3,0                                                             
         BAS   RE,PRINT            SKIP LINE AFTER BLOCK                        
*                                                                               
PR116    DS    0H                                                               
         CLI   T2.TLKTYP,X'11'     UNMATCHED?                                   
         BE    PR118                                                            
*                                                                               
         MVC   PL1.P1TAG(6),=C'AGENCY'                                          
         TM    T2.TLFLG1,TLF1NMQ                                                
         BZ    *+10                                                             
         MVC   PL1.P1TAG(8),=C'**AGENCY'                                        
*                                                                               
PR118    DS    0H                                                               
         MVI   LASTMAST,C'A'                                                    
         MVC   LASTMAST+1(1),T2.TLKACMB                                         
*                                                                               
         TM    PRTSTAT,PRTDAILY    SHOW IF DAILY                                
         BZ    *+10                                                             
         MVC   PL1.P1REMARK,=CL12'** DAILY **'                                  
*                                                                               
         L     R4,ALSTAREA                                                      
         USING LBUYD,R4                                                         
         ZAP   PCKOF08B,=P'0'      INITIALIZE RATE                              
PR120    DS    0H                                                               
         CLC   =C'P=',09(R4)       PROGRAM TEXT                                 
         BE    PR140               YES                                          
         OC    0(9,R4),SPACES                                                   
         CLC   0(9,R4),SPACES      POTENTIAL COMMENT?                           
         BH    PR130               NO                                           
*                                                                               
         LA    RE,LBUYDAYS+L'LBUYDAYS                                           
         LA    R0,9(R4)                                                         
         SR    RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   9(0,R4),SPACES                                                   
         BH    PR140               COMMENT - PRINT AS IS                        
*                                                                               
PR130    DS    0H                                                               
         CLC   LBUYSPT,SPACES                                                   
         BH    *+14                                                             
         ZAP   PCKOF16B,=P'0'                                                   
         B     PR134                                                            
*                                                                               
         CLC   LBUYRATE,SPACES                                                  
         BH    *+14                                                             
         ZAP   PCKOF16B,PCKOF08B                                                
         B     PR132                                                            
*                                                                               
         GOTO1 CASHVAL,DMCB,(X'82',LBUYRATE),L'LBUYRATE                         
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   PCKOF16B,DMCB+4(8)  RATE PER SPOT                                
         ZAP   PCKOF08B,DMCB+4(8)  KEEP COST FOR CARRY                          
*                                                                               
PR132    DS    0H                                                               
         GOTO1 CASHVAL,DMCB,(X'82',LBUYSPT),L'LBUYSPT                           
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MP    PCKOF16B,DMCB+4(8)  * TOTAL SPOTS                                
         SRP   PCKOF16B,64-2,5                                                  
*                                                                               
PR134    DS    0H                                                               
         MVC   PL1.P1BUYLN,LBUYLINE                                             
         MVC   PL1.P1XTAG(1),LBUYTYPE    <- NOT SURE WHAT THIS IS               
         MVC   PL1.P1DAYS,LBUYDAYS                                              
         MVC   PL1.P1TIMES,LBUYTIME                                             
         MVC   PL1.P1LEN+(L'P1LEN-L'LBUYLEN)(L'LBUYLEN),LBUYLEN                 
         MVC   PL1.P1DATES,LBUYDATE                                             
*                                                                               
         CLC   LBUYSPT,SPACES                                                   
         BNH   PR136                                                            
*                                                                               
         MVC   PL1.P1NPW,LBUYNPW                                                
         MVC   PL1.P1SPRATE,LBUYRATE                                            
         MVC   PL1.P1TOTSPT+(L'P1TOTSPT-L'LBUYSPT)(L'LBUYSPT),LBUYSPT           
         EDIT  PCKOF16B,PL1.P1TOTRAT,2,ZERO=NOBLANK,MINUS=YES                   
         MVC   PL1.P1CLASS,LBUYNW                                               
*                                                                               
PR136    DS    0H                                                               
         L     RE,ALSTAREA                                                      
         TM    PRTSTAT,PRTDAILY    SHOW IF DAILY                                
         BZ    *+8                                                              
         LA    RE,L'LISTAREA(RE)                                                
*                                                                               
         CR    R4,RE               FIRST LINE? (SECOND LINE ON DAILY)           
         BNE   PR138               NO - SKIP REMARK                             
*                                                                               
         TM    T2.TLFLG1,TLF1MGQ   MAKEGOOD?                                    
         BZ    *+14                NO                                           
         MVC   PL1.P1REMARK,=CL12'MAKEGOOD'                                     
         B     PR138                                                            
*                                                                               
         TM    T2.TLFLG1,TLF1CRQ   CREDIT?                                      
         BZ    *+14                NO                                           
         MVC   PL1.P1REMARK,=CL12'CREDIT'                                       
         B     PR138                                                            
*                                                                               
PR138    DS    0H                                                               
         BAS   RE,PRINT                                                         
         B     PR141                                                            
*                                                                               
PR140    DS    0H                  PRINT LINE AS IS                             
         MVC   P(L'LISTAREA),0(R4)                                              
*                                                                               
PR141    DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         LA    R4,L'LISTAREA(R4)                                                
         BCT   R3,PR120                                                         
*                                                                               
         B     PR500               NEXT TSAR RECORD                             
         EJECT                                                                  
*--------------*                                                                
* REP BUY LINE                                                                  
*--------------*                                                                
PR200    DS    0H                                                               
         CLI   T2.TLKSORT,X'01'    REP LINE?                                    
         BNE   PR300               NO                                           
*                                                                               
         NI    MISCFLG2,FF-MF2INCOM                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),T2.TLDSKAD                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         CLI   T2.TLKTYP,X'12'     UNMATCHED?                                   
         BNE   PR202               NO                                           
         TM    MISCFLG1,MF1UNMR    MSG PRINTED?                                 
         BO    PR202               YES                                          
*                                                                               
         OI    MISCFLG1,MF1UNMR                                                 
         MVI   P,0                                                              
         MVC   P2(UMREPMLQ),UMREPMSG                                            
         MVI   P3,0                                                             
         BAS   RE,PRINT                                                         
*                                                                               
PR202    DS    0H                                                               
         TM    T2.TLFLG1,TLF1DLQ   DELETED?                                     
         BZ    PR203               NO                                           
*&&DB                                                                           
         MVC   P(19),=C'XXX BUY XXX DELETED'      *DEBUG **************         
         MVC   P(3),=C'REP'                       *DEBUG **************         
         EDIT  T2.TLKRBUY,(3,P+8),ZERO=NOBLANK    *DEBUG **************         
         BAS   RE,PRINT                           *DEBUG **************         
*&&                                                                             
         MVI   LASTMAST,C'R'                                                    
         MVC   LASTMAST+1(1),T2.TLKRCMB                                         
         MVC   LASTAGY,T2.TLKACMB                                               
         B     PR500               NEXT TSAR RECORD                             
*                                                                               
PR203    DS    0H                                                               
         OI    MISCFLG2,MF2CHGS    SET CHANGES PRINTED                          
*                                                                               
         GOTO1 REGENPBY,DMCB,AIO,(32,ALSTARE2),PARMS2,AIO2                      
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,DMCB+4                                                      
         BZ    PR500               BUY IS CANCELLED/DELETED, SKIP               
*                                                                               
         CLI   T2.TLKTYP,X'12'     UNMATCHED?                                   
         BE    PR204                                                            
*                                                                               
         CLI   LASTMAST,C'R'       CHECK ALREADY IN BLOCK                       
         BNE   *+14                                                             
         CLC   T2.TLKACMB,LASTAGY                                               
         BE    PR204                                                            
*                                                                               
         CLC   T2.TLKACMB,LASTMAST+1    LINKING ERROR?                          
         BE    PR204                    NO                                      
*                                                                               
         L     RE,=A(AGYCANCL)                                                  
         A     RE,RELO                                                          
         MVC   P2(L'AGYCANCL),0(RE)                                             
         EDIT  T2.TLAGYB,(3,P2+14),ZERO=NOBLANK                                 
         BAS   RE,PRINT                                                         
*                                                                               
PR204    DS    0H                                                               
         CLI   T2.TLKTYP,X'12'     UNMATCHED?                                   
         BE    PR206                                                            
*                                                                               
         MVC   PL1.P1TAG(3),=C'REP'                                             
         CLI   T2.TLAGYB,0                                                      
         BNE   *+10                                                             
         MVC   PL1.P1TAG(5),=C'**REP'                                           
*                                                                               
PR206    DS    0H                                                               
         MVI   LASTMAST,C'R'                                                    
         MVC   LASTMAST+1(1),T2.TLKRCMB                                         
         MVC   LASTAGY,T2.TLKACMB                                               
*                                                                               
         L     R4,ALSTARE2                                                      
         USING LBUYD,R4                                                         
         ZAP   PCKOF08B,=P'0'      INITIALIZE RATE                              
*                                                                               
PR210    DS    0H                                                               
         CLC   =C'P=',13(R4)             PROGRAM TEXT                           
         BE    PR220P                                                           
         CLC   =C'MAKE',LBUYDAYS+6       MAKEGOOD COMMENT                       
         BE    PR220P                                                           
         CLC   =C'MKGD',LBUYDAYS+6       MAKEGOOD COMMENT                       
         BE    PR220P                                                           
         CLC   =C'CREDIT',LBUYDAYS+6     CREDIT COMMENT                         
         BE    PR220P                                                           
         CLC   =C'WEEKLY RATE',LBUYDATE+1 PLAN COMMENT                          
         BE    PR220P                                                           
         CLC   =C'SOC-',LBUYDAYS+6       STATION ORDER COMMENT                  
         BE    PR220                                                            
         CLC   =C'ROC-',LBUYDAYS+6       REP ORDER COMMENT                      
         BE    PR220                                                            
*                                                                               
RCOMPRE  EQU   (LBUYDAYS+6)-LBUYD                                               
         OC    0(RCOMPRE,R4),SPACES                                             
         CLC   0(RCOMPRE,R4),SPACES      POTENTIAL COMMENT?                     
         BH    PR211                     NO                                     
*                                                                               
         LA    RE,LBUYDAYS+L'LBUYDAYS                                           
         LA    R0,RCOMPRE(R4)                                                   
         SR    RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   RCOMPRE(0,R4),SPACES                                             
         BH    PR220P              COMMENT - PRINT AS IS                        
*                                                                               
PR211    DS    0H                                                               
         CLC   LBUYRATE,SPACES                                                  
         BH    *+14                                                             
         ZAP   PCKOF16B,PCKOF08B                                                
         B     PR212                                                            
*                                                                               
         GOTO1 CASHVAL,DMCB,(X'82',LBUYRATE),L'LBUYRATE                         
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   PCKOF08B,DMCB+4(8)  CARRY RATE FOR NEXT LINE                     
*                                                                               
         CLC   LBUYSPT,SPACES                                                   
         BH    *+14                                                             
         ZAP   PCKOF16B,=P'0'                                                   
         B     PR214                                                            
*                                                                               
         ZAP   PCKOF16B,DMCB+4(8)  RATE PER SPOT                                
*                                                                               
PR212    DS    0H                                                               
         CLC   LBUYSPT,SPACES                                                   
         BNH   PR214                                                            
*                                                                               
         GOTO1 CASHVAL,DMCB,(X'82',LBUYSPT),L'LBUYSPT                           
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MP    PCKOF16B,DMCB+4(8)  * TOTAL SPOTS                                
         SRP   PCKOF16B,64-2,5                                                  
*                                                                               
PR214    DS    0H                                                               
         MVC   PL1.P1BUYLN,LBUYLINE                                             
         MVC   PL1.P1XTAG(1),LBUYTYPE    <- NOT SURE WHAT THIS IS               
         MVC   PL1.P1DAYS,LBUYDAYS                                              
         MVC   PL1.P1TIMES,LBUYTIME                                             
         MVC   PL1.P1LEN+(L'P1LEN-L'LBUYLEN)(L'LBUYLEN),LBUYLEN                 
         MVC   PL1.P1DATES,LBUYDATE                                             
*                                                                               
         MVC   PL1.P1NPW,LBUYNPW                                                
         MVC   PL1.P1SPRATE,LBUYRATE                                            
         MVC   PL1.P1TOTSPT+(L'P1TOTSPT-L'LBUYSPT)(L'LBUYSPT),LBUYSPT           
*                                                                               
         CLC   LBUYSPT,SPACES                                                   
         BNH   PR216                                                            
*                                                                               
         EDIT  PCKOF16B,PL1.P1TOTRAT,2,ZERO=NOBLANK,MINUS=YES                   
         MVC   PL1.P1CLASS,LBUYNW                                               
*                                                                               
PR216    DS    0H                  PRINT LINE AND CONTINUE                      
         C     R4,ALSTARE2         FIRST LINE?                                  
         BNE   PR218               NO - SKIP REMARK                             
*                                                                               
         TM    T2.TLFLG1,TLF1MGQ   MAKEGOOD?                                    
         BZ    *+14                NO                                           
         MVC   PL1.P1REMARK,=CL12'MAKEGOOD'                                     
         B     PR218                                                            
*                                                                               
         TM    T2.TLFLG1,TLF1CRQ   CREDIT?                                      
         BZ    *+14                NO                                           
         MVC   PL1.P1REMARK,=CL12'CREDIT'                                       
         B     PR218                                                            
PR218    DS    0H                                                               
         B     PR220A                                                           
*                                                                               
PR220P   DS    0H                  PRINT LINE AS IS                             
         MVC   P(L'LISTARE2),0(R4)                                              
*                                                                               
PR220A   DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
PR220    DS    0H                                                               
         LA    R4,L'LISTARE2(R4)                                                
         BCT   R3,PR210                                                         
*                                                                               
         B     PR500               NEXT TSAR RECORD                             
         EJECT                                                                  
*--------------*                                                                
* COMMENT LINE                                                                  
*--------------*                                                                
PR300    DS    0H                                                               
         CLI   T2.TLKSORT,X'FF'    COMMENT?                                     
         BE    *+6                                                              
         DC    H'0'                NO, WHAT IS IT THEN?                         
*&&DB                                                                           
         B     PR310               PRINT ALL COMMENTS *DEBUG *********          
*&&                                                                             
         CLI   T2.TLKTYP,X'11'     UNMATCHED RECORD?                            
         BE    PR320               YES - DON'T PRINT COMMENTS                   
         CLI   T2.TLKTYP,X'12'                                                  
         BE    PR320               YES - DON'T PRINT COMMENTS                   
*                                                                               
         TM    T2.TLCFLG1,TLCF1DL  DELETED?                                     
         BNZ   PR320               YES                                          
*                                                                               
PR310    DS    0H                                                               
         OI    MISCFLG2,MF2CHGS    SET CHANGES PRINTED                          
*                                                                               
         TM    MISCFLG2,MF2INCOM   ALREADY DOING COMMENTS?                      
         BO    *+10                YES                                          
         MVC   P(10),=C'REVISION: '                                             
*                                                                               
         OI    MISCFLG2,MF2INCOM                                                
*                                                                               
         ZICM  RE,T2.TLLEN,2                                                    
         SH    RE,=Y(TLCOMLQ+1)                                                 
         LTR   RE,RE                                                            
         BNM   *+6                 EX CHECK                                     
         DC    H'0'                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P+12(0),T2.TLCCOM                                                
         BAS   RE,PRINT                                                         
*                                                                               
PR320    DS    0H                                                               
         B     PR500               NEXT TSAR RECORD                             
         EJECT                                                                  
*----------------*                                                              
* NEXT TSAR LINE                                                                
*----------------*                                                              
PR500    DS    0H                                                               
         GOTO1 TSAR,TSANXT                                                      
         BNL   PR090                                                            
*---------------------------*                                                   
* NO TSAR RECORDS FROM HERE                                                     
*---------------------------*                                                   
PR600    DS    0H                                                               
         TM    MISCFLG2,MF2CHGS    ANY CHANGES PRINTED?                         
         BNZ   PR602               YES                                          
*                                                                               
         MVI   P,0                                                              
         MVI   P2,C'~'                                                          
         MVC   P2+1(L'P2-2),P2                                                  
         MVC   P4,P2                                                            
*                                                                               
         L     RE,=A(ORDOK)        PRINT ORDER OK MESSAGE                       
         A     RE,RELO                                                          
         MVC   P3(ORDOKLQ),0(RE)                                                
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   P,0                                                              
         BAS   RE,PRINT                                                         
*                                                                               
PR602    DS    0H                                                               
         XC    KEY,KEY             ANY COMMENTS TO PRINT?                       
         MVC   KEY(RDARKRT-RDARKEY),PRBUYKEY                                    
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'20'       TYPE STANDARD COMMENT                        
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    PR608               YES                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),PRBUYKEY                                    
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'30'       TYPE ORDER COMMENT                           
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   PR630               NO                                           
*                                                                               
PR608    DS    0H                  PRINT COMMENTS                               
         BAS   RE,PRINT                                                         
         MVI   P,C'*'                                                           
         MVC   P+1(109),P                                                       
         MVC   P+3(19),=C' START OF COMMENTS '                                  
         BAS   RE,PRINT                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),PRBUYKEY                                    
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'20'       TYPE STANDARD COMMENT                        
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   PR610               IF STANDARD COMMENT                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,PRTSTCMT                                                      
*                                                                               
PR610    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),PRBUYKEY                                    
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'30'       TYPE ORDER COMMENT                           
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   PR620               IF ORDER COMMENT                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,PRTORCMT                                                      
*                                                                               
PR620    DS    0H                                                               
         MVI   P,C'*'                                                           
         MVC   P+1(109),P                                                       
         MVC   P+3(17),=C' END OF COMMENTS '                                    
         BAS   RE,PRINT                                                         
*                                                                               
PR630    DS    0H                                                               
*&&                                                                             
*-------------------------------------------------------------------            
* IF WE CAME FROM THE SELECT OR REJECT SCREEN, WE CAN CLOSE THE PQ              
*    BEFORE EXITING                                                             
*-------------------------------------------------------------------            
         CLI   OFFLINE,C'Y'                                                     
         BE    *+16                                                             
         TM    PRTSTAT,PRTONE                                                   
         BZ    *+8                                                              
         BAS   RE,PQCLOSE                                                       
*-------------------------*                                                     
* CLEAN UP AFTER PRINTING *                                                     
*-------------------------*                                                     
PRCLNUP  DS    0H                                                               
         OC    SVCONNUM,SVCONNUM   CONTRACT #?                                  
         BZ    PRX                 NO                                           
*                                                                               
         ZAP   WORK+20(5),=P'0'       RESTORE GLOBAL CONTRACT VALUES            
         MVO   WORK+20(5),SVCONNUM     BEFORE EXITING                           
         XC    FAKEHDR,FAKEHDR     SETUP FAKE FIELD HEADER                      
         LA    R2,FAKEHDR           TO CALL VCON WITH                           
         EDIT  (P5,WORK+20),(8,8(R2)),ALIGN=LEFT                                
         STC   R0,5(R2)            SET LENGTH OF FIELD                          
         MVI   0(R2),16            FIELD HEADER LENGTH                          
         MVI   4(R2),X'08'         SET VALID NUMERIC                            
         GOTO1 VALICON,DMCB,(R2)                                                
*                                                                               
PRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         DS    (PRFILL)XL500                                                    
***********************************************************************         
* PRINT PAGE1 "HEADINGS"                                                        
*   CONTRACT DETAILS ARE SET                                                    
*   DARE RECORD IS IN AIO                                                       
***********************************************************************         
PRPAGE1  NTR1                                                                   
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*--------------------*                                                          
* 1ST 3 REP HEADINGS                                                            
*--------------------*            12345678901234567890                          
         MVC   P1+REPCOLQ(001),=X'00'   <- BLANK LINE                           
         MVC   P2+REPCOLQ(001),=X'00'   <- BLANK LINE                           
         MVC   P3+REPCOLQ(009),=C'---REP---'                                    
*-----------------------*                                                       
* 1ST 3 AGENCY HEADINGS                                                         
*-----------------------*         12345678901234567890                          
         MVC   P1+AGYCOLQ(001),=X'00'   <- BLANK LINE                           
         MVC   P2+AGYCOLQ(001),=X'00'   <- BLANK LINE                           
         MVC   P3+AGYCOLQ(009),=C'---AGY---'                                    
*                                                                               
         BAS   RE,PRINT                                                         
*--------------------*                                                          
* 2ND 4 REP HEADINGS                                                            
*--------------------*            12345678901234567890                          
         MVC   P1+REPCOLQ(007),=C'AGENCY:'                                      
         MVC   P2+REPCOLQ(011),=C'CONTRACT #:'                                  
         MVC   P3+REPCOLQ(011),=C'ADVERTISER:'                                  
         MVC   P4+REPCOLQ(011),=C'ESTIMATE #:'                                  
*-----------------------*                                                       
* 2ND 4 AGENCY HEADINGS                                                         
*-----------------------*         12345678901234567890                          
         MVC   P1+AGYCOLQ(007),=C'AGENCY:'                                      
         MVC   P2+AGYCOLQ(008),=C'ORDER #:'                                     
         MVC   P3+AGYCOLQ(011),=C'ADVERTISER:'                                  
         MVC   P4+AGYCOLQ(011),=C'ESTIMATE #:'                                  
*------------*                                                                  
* REP AGENCY                                                                    
*------------*                                                                  
         MVC   P1+REPCOLQ+020(L'CCONKAGY),CCONKAGY                              
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    PRAGY02             NO                                           
*                                                                               
         LA    RE,P1+REPCOLQ+20                                                 
         CLI   0(RE),C' '                                                       
         BNH   *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
*                                                                               
PRAGY02  DS    0H                                                               
         MVC   P1+REPCOLQ+30(20),EAGYNAM1 AGENCY EXPANSION                      
*------------*                                                                  
* AGY AGENCY                                                                    
*------------*                                                                  
         MVC   P1+AGYCOLQ+20(L'RDARKAGY),RDARKAGY                               
         OC    RDARKAOF,RDARKAOF                                                
         BZ    PAAGY02                                                          
         LA    RE,P1+AGYCOLQ+20                                                 
         CLI   0(RE),C' '                                                       
         BNH   *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RDARKAOF     AGENCY OFFICE                               
                                                                                
PAAGY02  DS    0H                                                               
         MVC   P1+AGYCOLQ+30(34),RDARAGNM    AGENCY NAME                        
*---------------------*                                                         
* REP CONTRACT NUMBER                                                           
*---------------------*                                                         
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),CCONKNUM                                              
         EDIT  (P5,WORK+20),(8,P2+REPCOLQ+20),ALIGN=LEFT                        
*---------------------*                                                         
* AGY CONTRACT NUMBER                                                           
*---------------------*                                                         
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),RDARKORD                                              
         EDIT  (P5,WORK+20),(8,P2+AGYCOLQ+20),ALIGN=LEFT                        
*----------------*                                                              
* REP ADVERTISER                                                                
*----------------*                                                              
         MVC   P3+REPCOLQ+20(4),CCONKADV                                        
         MVC   P3+REPCOLQ+30(20),EADVNAME                                       
*----------------*                                                              
* AGY ADVERTISER                                                                
*----------------*                                                              
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL6                                                        
         USING RDARCLEM,R6                                                      
         MVC   P3+AGYCOLQ+20(6),RDARCLI                                         
         MVC   P3+AGYCOLQ+30(34),RDARCLNM                                       
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*--------------*                                                                
* REP ESTIMATE                                                                  
*--------------*                                                                
         MVC   P4+REPCOLQ+20(10),CCONXEST                                       
*--------------*                                                                
* AGY ESTIMATE                                                                  
*--------------*                                                                
         EDIT  RDAREST#,(8,P4+AGYCOLQ+20),ALIGN=LEFT                            
         DROP  R6                                                               
*                                                                               
* USE ALPHANUMERIC ESTIMATE IF PRESENT                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL6                                                        
         BNE   PAEST10                                                          
         USING RDAREL2M,R6                                                      
         OC    RDAR2EST,RDAR2EST                                                
         BZ    PAEST10                                                          
         MVC   P4+AGYCOLQ+20(8),SPACES                                          
         MVC   P4+AGYCOLQ+20(6),RDAR2EST                                        
         DROP  R6                                                               
*                                                                               
PAEST10  DS    0H                                                               
         BAS   RE,PRINT                                                         
*--------------------*                                                          
* 2ND 4 REP HEADINGS                                                            
*--------------------*            12345678901234567890                          
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   P1+REPCOLQ(013),=C'FLIGHT DATES:'                                
         MVC   P2+REPCOLQ(008),=C'STATION:'                                     
         MVC   P3+REPCOLQ(008),=C'PRODUCT:'                                     
         MVC   P4+REPCOLQ(011),=C'REP OFFICE:'                                  
*-----------------------*                                                       
* 2ND 4 AGENCY HEADINGS                                                         
*-----------------------*         12345678901234567890                          
         MVC   P1+AGYCOLQ(013),=C'FLIGHT DATES:'                                
         MVC   P2+AGYCOLQ(008),=C'STATION:'                                     
         MVC   P3+AGYCOLQ(008),=C'PRODUCT:'                                     
         MVC   P4+AGYCOLQ(012),=C'2ND PRODUCT:'                                 
*------------------*                                                            
* REP FLIGHT DATES                                                              
*------------------*                                                            
         MVC   P1+REPCOLQ+20(17),ECONDATE                                       
*------------------*                                                            
* AGY FLIGHT DATES                                                              
*------------------*                                                            
         OC    TKODATE,TKODATE     TAKEOVER CHOPPING?                           
         BZ    PRAFL10             NO                                           
*&&TK                                                                           
         GOTO1 DATCON,DMCB,(2,RDARESEN),(3,WORK)                                
         CLC   TKODATE,WORK                                                     
         BNH   PRAFL08                                                          
*                                                                               
         MVI   P1+AGYCOLQ+20,C'-'  ENTIRE FLIGHT IS BOGUS                       
         MVC   P1+AGYCOLQ+21(15),P1+AGYCOLQ+20                                  
         B     PRAFL14                                                          
*                                                                               
PRAFL08  DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,TKODATE),(5,P1+AGYCOLQ+20)                        
         B     PRAFL12                                                          
*&&                                                                             
PRAFL10  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RDARESST),(5,P1+AGYCOLQ+20)                       
*                                                                               
PRAFL12  DS    0H                                                               
         MVI   P1+AGYCOLQ+28,C'-'                                               
         GOTO1 DATCON,DMCB,(2,RDARESEN),(5,P1+AGYCOLQ+29)                       
*                                                                               
PRAFL14  DS    0H                                                               
*----------------------*                                                        
* REP STATION + MARKET                                                          
*----------------------*                                                        
         XC    WORK,WORK                                                        
         MVC   WORK(4),CCONKSTA                                                 
         MVC   WORK+4(3),=C'- M'                                                
         CLI   CCONKSTA+4,C' '                                                  
         BNE   *+14                                                             
         MVC   WORK+5(2),=C'TV'                                                 
         B     PRSTA02                                                          
*                                                                               
         MVC   WORK+5(1),CCONKSTA+4                                             
         CLI   CCONKSTA+4,C'L'                                                  
         BNE   *+8                                                              
         MVI   WORK+6,C' '                                                      
*                                                                               
PRSTA02  DS    0H                                                               
         MVC   P2+REPCOLQ+20(7),WORK                                            
         CLI   P2+REPCOLQ+24,C' '                                               
         BNE   *+10                                                             
         MVC   P2+REPCOLQ+24(3),WORK+5                                          
*                                                                               
         MVC   P2+REPCOLQ+30(20),EMKTNAME                                       
*----------------------*                                                        
* AGY STATION + MARKET                                                          
*----------------------*                                                        
         MVC   P2+AGYCOLQ+20(4),RDARKSTA                                        
         MVC   P2+AGYCOLQ+24(3),=C'- M'                                         
         MVC   P2+AGYCOLQ+25(1),RDARKSTA+4                                      
         CLI   RDARKSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   P2+AGYCOLQ+26,C'V'                                               
*                                                                               
         MVC   P2+AGYCOLQ+30(20),EMKTNAME                                       
*-------------*                                                                 
* REP PRODUCT                                                                   
*-------------*                                                                 
         MVC   P3+REPCOLQ+20(3),CCONPRD                                         
         OC    CCONPRD,CCONPRD                                                  
         BZ    *+14                                                             
         MVC   P3+REPCOLQ+30(20),EPRDNAME                                       
         B     *+10                                                             
         MVC   P3+REPCOLQ+20(20),EPRDNAME                                       
*--------------*                                                                
* AGY PRODUCT 1                                                                 
*--------------*                                                                
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL6                                                        
         USING RDARCLEM,R6                                                      
         MVC   P3+AGYCOLQ+20(4),RDARPRD1                                        
         MVC   P3+AGYCOLQ+30(34),RDARPRN1                                       
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*------------*                                                                  
* REP OFFICE                                                                    
*------------*                                                                  
         MVC   P4+REPCOLQ+20(2),CCONKOFF                                        
*--------------*                                                                
* AGY PRODUCT 2                                                                 
*--------------*                                                                
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL6                                                        
         USING RDARCLEM,R6                                                      
         MVC   P4+AGYCOLQ+20(4),RDARPRD2                                        
         MVC   P4+AGYCOLQ+30(34),RDARPRN2                                       
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
*                                                                               
         BAS   RE,PRINT                                                         
*--------------------*                                                          
* 3RD 4 REP HEADINGS                                                            
*--------------------*            12345678901234567890                          
         MVC   P1+REPCOLQ(012),=C'SALESPERSON:'                                 
*        MVC   P2+REPCOLQ(000),                                                 
*        MVC   P3+REPCOLQ(000),                                                 
*        MVC   P4+REPCOLQ(000),                                                 
*-----------------------*                                                       
* 3RD 4 AGENCY HEADINGS                                                         
*-----------------------*         12345678901234567890                          
         MVC   P1+AGYCOLQ(006),=C'BUYER:'                                       
*        MVC   P2+AGYCOLQ(000),                                                 
*        MVC   P3+AGYCOLQ(000),                                                 
*        MVC   P4+AGYCOLQ(000),                                                 
*-----------------*                                                             
* REP SALESPERSON                                                               
*-----------------*                                                             
         MVC   P1+REPCOLQ+20(3),CCONSAL                                         
         MVC   P1+REPCOLQ+30(20),ESALNAME                                       
*-----------*                                                                   
* AGY BUYER                                                                     
*-----------*                                                                   
         MVC   P1+AGYCOLQ+20(20),RDARBUYR                                       
*                                                                               
         BAS   RE,PRINT                                                         
*--------------------*                                                          
* 4TH 4 REP HEADINGS                                                            
*--------------------*            12345678901234567890                          
         MVC   P1+REPCOLQ(001),=X'00'           <- BLANK LINE                   
         MVC   P2+REPCOLQ(001),=X'00'           <- BLANK LINE                   
         MVC   P3+REPCOLQ(014),=C'TOTAL DOLLARS:'                               
         MVC   P4+REPCOLQ(012),=C'TOTAL SPOTS:'                                 
*                                                                               
         TM    MISCFLAG,MSFTRADE                                                
         BZ    *+16                                                             
         MVC   P3+REPCOLQ(020),=C'TRADE TOTAL DOLLARS:'                         
         MVC   P4+REPCOLQ(018),=C'TRADE TOTAL SPOTS:'                           
*                                                                               
*-----------------------*                                                       
* 4TH 4 AGENCY HEADINGS                                                         
*-----------------------*         12345678901234567890                          
         MVC   P1+AGYCOLQ(001),=X'00'           <- BLANK LINE                   
         MVC   P2+AGYCOLQ(001),=X'00'           <- BLANK LINE                   
         MVC   P3+AGYCOLQ(014),=C'TOTAL DOLLARS:'                               
         MVC   P4+AGYCOLQ(012),=C'TOTAL SPOTS:'                                 
*                                                                               
         TM    MISCFLAG,MSFTRADE                                                
         BZ    *+16                                                             
         MVC   P3+AGYCOLQ(020),=C'TRADE TOTAL DOLLARS:'                         
         MVC   P4+AGYCOLQ(018),=C'TRADE TOTAL SPOTS:'                           
*                                                                               
         EDIT  TOTRCOST,(17,P3+REPCOLQ+20),2,ZERO=NOBLANK,MINUS=YES             
         EDIT  TOTRSPTS,(17,P4+REPCOLQ+20),ZERO=NOBLANK,MINUS=YES               
*                                                                               
         EDIT  TOTACOST,(17,P3+AGYCOLQ+20),2,ZERO=NOBLANK,MINUS=YES             
         EDIT  TOTASPTS,(17,P4+AGYCOLQ+20),ZERO=NOBLANK,MINUS=YES               
*                                                                               
*&&DB                                                                           
         EDIT  TOTXCOST,(17,P3+AGYCOLQ+40),2,ZERO=NOBLANK,MINUS=YES             
         EDIT  TOTXSPTS,(17,P4+AGYCOLQ+40),ZERO=NOBLANK,MINUS=YES               
*&&                                                                             
PRTTR20  DS    0H                                                               
         BAS   RE,PRINT                                                         
*----------*                                                                    
* HEADINGS                                                                      
*----------*                                                                    
         MVI   P1,C'*'                                                          
         MVC   P1+1(L'P1-2),P1                                                  
         MVI   P2,C'*'                                                          
         MVC   P2+1(L'P2-2),P2                                                  
*                                                                               
         MVC   P3(132),BUYTITL1                                                 
         MVC   P4(132),BUYTITL2                                                 
*                                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   P1,C'-'                                                          
         MVC   P1+1(L'P1-2),P1                                                  
*                                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT STANDARD COMMENTS                                                       
***********************************************************************         
PRTSTCMT NTR1                                                                   
         BAS   RE,PRINT                                                         
         MVC   P+4(24),=C'AGENCY STANDARD COMMENT:'                             
         BAS   RE,PRINT                                                         
                                                                                
PRTST10  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDARSCEL,R6                                                      
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL6                                                        
         BNE   PRTSTX                                                           
                                                                                
PRTST20  DS    0H                                                               
         ZIC   R1,RDARSCLN                                                      
         CLI   RDARSCLN,3                                                       
         BL    *+8                                                              
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),RDARSCCM                                                  
         BAS   RE,PRINT                                                         
         BAS   RE,NEXTEL6                                                       
         BE    PRTST20                                                          
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
                                                                                
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   PRTSTX                                                           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     PRTST10                                                          
                                                                                
PRTSTX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT ORDER COMMENTS                                                          
***********************************************************************         
PRTORCMT NTR1                                                                   
         BAS   RE,PRINT                                                         
         MVC   P+4(21),=C'AGENCY ORDER COMMENT:'                                
         BAS   RE,PRINT                                                         
                                                                                
PRTOR10  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDAROREL,R6                                                      
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL6                                                        
         BNE   PRTORX                                                           
                                                                                
PRTOR20  DS    0H                                                               
         ZIC   R1,RDARORLN                                                      
         CLI   RDARORLN,3                                                       
         BL    *+8                                                              
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),RDARORCM                                                  
         BAS   RE,PRINT                                                         
         BAS   RE,NEXTEL6                                                       
         BE    PRTOR20                                                          
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
                                                                                
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   PRTORX                                                           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     PRTOR10                                                          
                                                                                
PRTORX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CLOSE PRINTQ                                                                  
***********************************************************************         
PQCLOSE  NTR1                                                                   
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
***********************************************************************         
* OPEN PRINTQ AND SET HEADSPECS/HOOK                                            
***********************************************************************         
PQOPEN   NTR1                                                                   
         OI    GENSTAT3,NOCLRSPK                                                
         MVC   REMUSER,=C'DAO'     DARE AGENCY ORDER                            
*                                                                               
         L     R4,ATWA                                                          
         USING CONHEADH-64,R4      BASE SCREEN + OUR SCREEN                     
         OC    REQINIT,REQINIT     CHECK IF OVERRIDE REPORT NAME                
         BZ    *+10                                                             
         MVC   REMUSER,REQINIT                                                  
         DROP  R4                                                               
*&&DB                                                                           
         MVC   REMUSER,=C'DBR'     DEBUG REPORT                                 
*&&                                                                             
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLDESC,=C'WORKSHEET  '                                           
         MVI   PLCLASS,C' '                                                     
         OI    SPOOLIND,SPUINIT    PERMITS SETTING OF CLASS                     
         MVC   PLSUBID,=C'DAO'     DARE AGENCY ORDER                            
*                                                                               
         L     R4,ATWA                                                          
         USING CONHEADH-64,R4      BASE SCREEN + OUR SCREEN                     
         OC    REQINIT,REQINIT     CHECK IF OVERRIDE REPORT NAME                
         BZ    *+10                                                             
         MVC   REMUSER,REQINIT                                                  
         DROP  R4                                                               
*&&DB                                                                           
         MVC   PLSUBID,=C'DBR'     DEBUG REPORT                                 
*&&                                                                             
         DROP  RF                                                               
*                                                                               
         LA    RE,SPLKEYAD                                                      
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'36'                                                   
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'CO'                                                     
         DROP  RE                                                               
*                                                                               
         GOTO1 OPENPQ                                                           
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
EXITL    CLI   *,FF                                                             
         B     EXIT                                                             
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
EXIT     DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
***********************************************************************         
* CLEAR THE TSAR LINE                                                           
***********************************************************************         
XCTLST   NTR1                                                                   
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         LA    RE,TLST                                                          
         LA    RF,TLSTLQ                                                        
         MVCL  RE,R0                                                            
         B     EXIT                                                             
***********************************************************************         
* MOVE THE TSAR LINE TO A SAVED BUFFER                                          
*  NUMBER OF BUFFER IN HIGH ODER BYTE OF RF                                     
***********************************************************************         
MVCTLST  NTR1                                                                   
         CLM   RF,8,=AL1(1)                                                     
         BNE   *+12                                                             
         LA    R0,TS1.TLNUM                                                     
         B     MVCTLST2                                                         
*                                                                               
         CLM   RF,8,=AL1(2)                                                     
         BNE   *+12                                                             
         LA    R0,TS2.TLNUM                                                     
         B     MVCTLST2                                                         
*                                                                               
         CLM   RF,8,=AL1(11)       REVERSED MOVE                                
         BNE   *+12                                                             
         LA    R0,TS1.TLNUM                                                     
         B     MVCTLST4                                                         
*                                                                               
         CLM   RF,8,=AL1(12)       REVERSED MOVE                                
         BNE   *+12                                                             
         LA    R0,TS2.TLNUM                                                     
         B     MVCTLST4                                                         
*                                                                               
         DC    H'0'                                                             
*                                                                               
MVCTLST2 DS    0H                                                               
         LA    R1,TLSTLQ                                                        
         LA    RE,TLST                                                          
         LA    RF,TLSTLQ                                                        
         MVCL  R0,RE                                                            
         B     EXIT                                                             
*                                                                               
MVCTLST4 DS    0H                                                               
         LA    R1,TLSTLQ                                                        
         LA    RE,TLST                                                          
         LA    RF,TLSTLQ                                                        
         MVCL  RE,R0                                                            
         B     EXIT                                                             
***********************************************************************         
        GETELN R6,DATADISP,ELCODE,6                                             
***********************************************************************         
        GETELN R4,DATADISP,ELCODE,4                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INTERFACE WITH TSAR BUFFER                               *         
* NTRY: R1 = REQUESTED ACTION                                         *         
*                                                                     *         
* EXIT: CC LOW FOR END OF FILE ERROR                                  *         
*       CC HIGH FOR RECORD NOT FOUND                                  *         
***********************************************************************         
TSAR     NTR1                                                                   
B2       USING TSARD,R3                                                         
         L     R3,ATLBUFF          TSAR BUFFER                                  
         STCM  R1,1,TLACTN         REQUESTED ACTION                             
         TM    LSTSINDS,LSTSIRES   TEST TEMPEST BUFFER RESTORED                 
         BNZ   TSAR04                                                           
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   TSAR00                                                           
*                                                                               
         MVC   B2.TSABUF,TBUFFADR  SET UP A(TSAR BUFFER)                        
         MVC   B2.TSAREC,TBUFFLEN  LENGTH OF BUFFER                             
         MVI   B2.TSOFFACT,TSAINI  SET INITIALISE                               
         B     TSAR01                                                           
*                                                                               
TSAR00   DS    0H                                                               
         MVI   B2.TSACTN,TSAINI    SET INITIALISE                               
         MVI   B2.TSPAGN,12        EXPAND BUFFER FOR ONLINE REPORT              
         B     TSAR0101                                                         
*                                                                               
TSAR01   DS    0H                                                               
         MVI   B2.TSPAGN,0                                                      
TSAR0101 DS    0H                                                               
         MVC   B2.TSACOM,ACOMFACS     SET A(COMFACS)                            
         MVI   B2.TSINDS,TSINODSK     SET TO CORE ONLY                          
         MVI   B2.TSKEYL,L'TLKEY      SET KEY LENGTH                            
         MVI   B2.TSRECI,TSRVAR       SET VARIABLE                              
         MVC   B2.TSRECL,=Y(2000)     SET MAXIMUM RECORD LENGTH                 
*                                                                               
         CLI   TLACTN,TSASAV       WANT TO SAVE BEFORE RESTORING?               
         BE    TSARX                                                            
*                                                                               
TSAR02   GOTOX VTSAR,B2.TSARD      CALL TO INITIALISE/RESTORE                   
         BE    *+6                                                              
         DC    H'0'                ABEND                                        
*                                                                               
         OI    LSTSINDS,LSTSIINI+LSTSIRES                                       
*                                                                               
TSAR04   DS    0H                                                               
         LA    R0,TXREC                                                         
         ST    R0,B2.TSAREC        SET A(RECORD)                                
*                                                                               
         CLI   OFFLINE,C'Y'        SET ACTION NUMBER                            
         BE    *+14                                                             
         MVC   B2.TSACTN,TLACTN                                                 
         B     *+10                                                             
         MVC   B2.TSOFFACT,TLACTN                                               
*                                                                               
         CLI   TLACTN,TSAINI       EXPLICIT INITIALISE?                         
         BE    TSARX                                                            
         CLI   TLACTN,TSARES       EXPLICIT RESTORE?                            
         BE    TSARX                                                            
         CLI   TLACTN,TSASAV       SAVE?                                        
         BNE   TSAR06              NO                                           
         NI    LSTSINDS,FF-LSTSIRES                                             
         GOTOX VTSAR,B2.TSARD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TSAR06   MVC   B2.TSRNUM,TXNUM     SET TSAR NUMBER                              
         GOTOX VTSAR,B2.TSARD                                                   
         MVC   TXNUM,B2.TSRNUM     SET RECORD LIST NUMBER                       
         BE    TSAR10                                                           
*                                                                               
         CLI   TLACTN,TSAADD       TEST ADDING                                  
         BE    TSAR08                                                           
         CLI   TLACTN,TSARDH       TEST READ-HIGH/NEXT                          
         BE    TSAR08                                                           
         CLI   TLACTN,TSANXT                                                    
         BE    TSAR08                                                           
         DC    H'0'                                                             
*                                                                               
TSAR08   CLI   TLACTN,TSAADD       CHECK IF END-OF-FILE ERROR                   
         BNE   TSAR09                                                           
         TM    B2.TSERRS,TSEEOF    DON'T DIE, DISPLAY ERROR INSTEAD             
         BZ    TSAR09              ASK USER TO REQUEST REPORT OFFLINE           
         MVC   RERROR,=AL2(801)                                                 
         L     R2,ATWA                                                          
         LA    R2,CONRECH-CONHEADH+64(R2)                                       
         GOTO1 MYERROR                                                          
*                                                                               
TSAR09   DS    0H                                                               
         TM    B2.TSERRS,TSEEOF    RETURN CC=LOW FOR END-OF-FILE ERROR          
         BO    EXITL                                                            
         TM    B2.TSERRS,TSERNF    RETURN CC=HIGH IF RECORD NOT FOUND           
         BO    EXITH                                                            
         DC    H'0'                                                             
*                                                                               
TSAR10   DS    0H                  COMPLETELY SUPERFLUOUS LABEL                 
*                                                                               
TSARX    B     EXITOK                                                           
         DROP  B2                                                               
         EJECT                                                                  
***********************************************************************         
* REPORT CONTRACT SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,56,REQUESTOR                                                  
         PSPEC H1,120,PAGE                                                      
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT LINE HEADINGS - 132 CHARACTER LENGTH                                    
***********************************************************************         
*                                                                               
*                         1         2         3         4         5             
*                12345678901234567890123456789012345678901234567890             
BUYTITL1 DC    C'         BUYLINE                                  '            
*                                                                               
*                                                                 1             
*                5        6         7         8         9         0             
*                12345678901234567890123456789012345678901234567890             
         DC    C'    EFFECTIVE     PER    RATE/     TOTAL      FLIG'            
*                                                                               
*                1        1         1         1                                 
*                0        1         2         3                                 
*                12345678901234567890123456789012                               
         DC    C'HT                              '                              
*                                                                               
*                         1         2         3         4         5             
*                12345678901234567890123456789012345678901234567890             
BUYTITL2 DC    C'            #         DAYS         TIMES     LENGT'            
* DATA AREAS     -----      ---    ------------  -----------  -----             
*                                                                               
*                                                                 1             
*                5        6         7         8         9         0             
*                12345678901234567890123456789012345678901234567890             
         DC    C'H     DATES      WEEK     SPOT     SPOTS       TOT'            
* DATA AREAS     -  ------------   ---  ----------  ----- ---------             
*                                                                               
*                1        1         1         1                                 
*                0        1         2         3                                 
*                12345678901234567890123456789012                               
         DC    C'AL        CLASS                 '                              
* DATA AREAS     --------  -----                                                
*                                                                               
***********************************************************************         
* CONSTANTS                                                                     
***********************************************************************         
UMAGYMSG DC    C'AGENCY BUYLINES WITH NO MATCHING REP BUYLINE'                  
         DC    C' (ADD TO SCHEDULE):'                                           
UMAGYMLQ EQU   *-UMAGYMSG                                                       
UMREPMSG DC    C'REP BUYLINES WITH NO MATCHING AGENCY BUYLINE:'                 
         DC    C' (CANCELED FROM SCHEDULE):'                                    
UMREPMLQ EQU   *-UMREPMSG                                                       
SOFTMSG  DS    C'SOFT MATCHED BUYLINES:'                                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATCON,DMCB,(5,0),(8,H1+90)                                      
*                                                  GET TODAY'S DATE             
         THMS  DDSTIME=YES                         GET CURRENT TIME             
         ST    R1,DUB                                                           
         ZAP   PRTTIME,DUB(4)                                                   
         ST    R0,DUB              ADDJUST FOR DDS TIME                         
         AP    PRTTIME,DUB(4)                                                   
*                                                                               
         MVC   H1+99(2),=C'AT'                                                  
         UNPK  DUB,PRTTIME                                                      
         MVC   H1+102(2),DUB+2     TIME                                         
         MVI   H1+104,C'.'                                                      
         MVC   H1+105(2),DUB+4                                                  
*                                                                               
         MVC   H2+46(40),=C'****** DARE AGENCY REVISION REPORT *****'           
*                                                                               
         TM    MISCFLAG,MSFTRADE                                                
         BZ    *+10                                                             
         MVC   H2(9),=C'* TRADE *'                                              
*                                                                               
         MVC   H3(11),=C'REVISION #:'                                           
         EDIT  REVNUM,(3,H3+12),ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
         MVC   H3+32(27),=C'REVISION PROCESSING METHOD:'                        
         MVC   H3+60(06),=C'MANUAL'                                             
         CLI   METHOD,1                                                         
         BNE   *+10         12345678901234567890                                
         MVC   H3+60(20),=C'CANCEL AND SUPERCEDE'                               
         CLI   METHOD,2                                                         
         BNE   *+10         1234567890123456789012345678901234567890            
         MVC   H3+60(37),=C'ADD LINE FOR INCREASE IN SPOTS/FLIGHT'              
         CLI   METHOD,3                                                         
         BNE   *+10         12345678901234567890                                
         MVC   H3+60(20),=C'CHANGE EXISTING LINE'                               
*                                                                               
         MVC   H3+90(19),=C'REVISION DATE/TIME:'                                
         GOTO1 DATCON,DMCB,(2,REVDATE),(5,H3+110)                               
         EDIT  (B2,REVTIME),(4,MYWORK)                                          
         MVC   H3+119(2),MYWORK   FORMAT TO HH:MM                               
         MVI   H3+121,C':'                                                      
         MVC   H3+122(2),MYWORK+2                                               
*                                                                               
         TM    PRTSTAT,PRTPAGE1                                                 
         BNO   HOOKX                                                            
*                                                                               
*---------------------*                                                         
* REP CONTRACT NUMBER                                                           
*---------------------*                                                         
         MVC   H4(011),=C'CONTRACT #:'                                          
         ZAP   WORK+20(5),=P'0'                                                 
         MVO   WORK+20(5),CCONKNUM                                              
         EDIT  (P5,WORK+20),(8,H4+12),ALIGN=LEFT                                
*------------*                                                                  
* REP AGENCY                                                                    
*------------*                                                                  
         MVC   H5(011),=C'REP AGENCY:'                                          
         MVC   H5+12(L'CCONKAGY),CCONKAGY                                       
*                                                                               
         LA    RE,H5+12                                                         
         CLI   0(RE),C' '                                                       
         BNH   *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
*                                                                               
         CLC   CCONKAOF,SPACES     OFFICE?                                      
         BE    *+18                NO                                           
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),CCONKAOF    AGENCY OFFICE                                
         LA    RE,3(RE)                                                         
*                                                                               
         LA    RE,4(RE)                                                         
         MVC   0(20,RE),EAGYNAM1   AGENCY EXPANSION                             
*----------------*                                                              
* REP ADVERTISER                                                                
*----------------*                                                              
         MVC   H4+50(015),=C'REP ADVERTISER:'                                   
         MVC   H4+50+16(4),CCONKADV                                             
         MVC   H4+50+16+4+4(20),EADVNAME                                        
*-------------*                                                                 
* REP PRODUCT                                                                   
*-------------*                                                                 
         MVC   H5+50(012),=C'REP PRODUCT:'                                      
         MVC   H5+50+13(3),CCONPRD                                              
         OC    CCONPRD,CCONPRD                                                  
         BZ    *+14                                                             
         MVC   H5+50+13+3+4(20),EPRDNAME                                        
         B     *+10                                                             
         MVC   H5+50+13(20),EPRDNAME                                            
*                                                                               
         MVI   H6,C'*'             DIVIDER                                      
         MVC   H6+1(L'H6-1),H6                                                  
*                                  COLUMN HEADINGS                              
         MVC   H7(132),BUYTITL1                                                 
         MVC   H8(132),BUYTITL2                                                 
*                                                                               
         MVI   H9,C'-'                                                          
         MVC   H9+1(L'H9-1),H9                                                  
*                                                                               
HOOKX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ RECORDS INTO TSAR.                                                       
*  READ REP BUYS FIRST                                                          
*     IF LINKED TO ANGECY BUYLINE # ADD MATCHED KEY                             
*      ELSE ADD UNMATCHED KEY                                                   
*     IF MASTER BUY LINE ADD MASTER TSAR KEY FOR LINKING                        
*  NEXT READ AGENCY BUYS                                                        
*     READ FOR MATCHING REP BUY IN TSAR                                         
*       IF FOUND ADD MATCHED KEY                                                
*        ELSE ADD UNMATCHED KEY                                                 
*                                                                               
***********************************************************************         
READRECS NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING RBUYREC,RE                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,AGENCY                                                  
         PACK  RBUYKCON+0(1),CCONNUM+3(1)       REVERSED 9'S COMP               
         PACK  RBUYKCON+1(1),CCONNUM+2(1)                                       
         PACK  RBUYKCON+2(1),CCONNUM+1(1)                                       
         PACK  RBUYKCON+3(1),CCONNUM+0(1)                                       
         DROP  RE                                                               
*                                                                               
         NI    PRTSTAT,FF-PRTKBUY                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
RDRREP02 DS    0H                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   RDRREPX             NO REP BUYS                                  
*                                                                               
         OI    PRTSTAT,PRTKBUY                                                  
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RBUYREC,R6                                                       
*                                                                               
         CLI   RBUYAGBL,0          NEED TO BUILD HARD LINK RECORD?              
         BE    RDRREP08            NO                                           
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'X'                                                   
         MVC   T2.TLHKBUY,RBUYAGBL                                              
         GOTO1 TSAR,TSARDH         EXISTING HARD LINK FOR AGY BUY?              
         BNE   RDRREP04            NO - BUILD NEW HARD LINK RECORD              
*                                                                               
         ZICM  RE,T2.TLLEN,2                                                    
         LA    R0,1(RE)                                                         
         STCM  R0,3,T2.TLLEN                                                    
         LA    RE,T2.TLREC(RE)     POINT TO NEXT SPACE                          
         MVC   0(1,RE),RBUYKLIN                                                 
         GOTO1 TSAR,TSAPUT         WRITE BACK RECORD                            
         BE    RDRREP08                                                         
         DC    H'0'                EH?                                          
*                                                                               
RDRREP04 DS    0H                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'X'                                                   
         MVC   T2.TLHKBUY,RBUYAGBL                                              
         MVC   T2.TLHLINKS,RBUYKLIN                                             
         MVC   T2.TLLEN,=Y(TLHLNKLQ)                                            
         GOTO1 TSAR,TSAADD                                                      
         BE    *+6                                                              
         DC    H'0'                BUFFER FULL                                  
*                                                                               
RDRREP08 DS    0H                                                               
         MVC   BYTE,RBUYAGBL       BYTE GETS AGENCY BUY #                       
         MVC   BYTE2,RBUYKMLN      BYTE2 GETS CHAINED MASTER                    
*                                                                               
         CLC   RBUYKMLN,RBUYKLIN   NEED TO FIND MASTER?                         
         BE    RDRREP10            NO                                           
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'R'                                                   
         MVC   T2.TLLKMBUY,RBUYKMLN                                             
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                 MASTER SHOULD BE FOUND                       
         DC    H'0'                                                             
*                                                                               
         OI    T2.TLLFLAGS,TLLFMGAQ                                             
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                 HUH?                                         
         DC    H'0'                                                             
*                                                                               
         OC    T2.TLLMABUY,T2.TLLMABUY  MASTER LINE COULD POSSIBLY              
         BZ    *+10                     HAVE NO AGENCY LINK                     
         MVC   BYTE,T2.TLLMABUY    BYTE GETS AGENCY BUY #                       
*                                                                               
         MVC   BYTE2,T2.TLLCMAST   BYTE2 GETS CHAINED MASTER                    
         MVC   BYTE3,T2.TLLMAGBL   AGENCY BUY OF MASTER FOR CREDITS             
*                                                                               
RDRREP10 DS    0H                                                               
         BAS   RE,XCTLST           ADD BUY KEY FOR LINKING INFO                 
         MVC   T2.TLLEN,=Y(TLLMBYLQ)                                            
         MVI   T2.TLKTYP,C'R'                                                   
         MVC   T2.TLLKMBUY,RBUYKLIN                                             
         MVC   T2.TLLCMAST,BYTE2                                                
         MVC   T2.TLLMAST,RBUYKMLN                                              
         MVC   T2.TLLMABUY,BYTE                                                 
         MVC   T2.TLLMAGBL,RBUYAGBL                                             
         CLC   RBUYKMLN,RBUYKLIN   MAKEGOOD?                                    
         BE    *+8                 NO                                           
         OI    T2.TLLFLAGS,TLLFMGAQ                                             
         CLC   RBUYTSPT,=H'0'      ZERO SPOTS?                                  
         BNE   *+8                 NO                                           
         OI    T2.TLLFLAGS,TLLF0SPQ                                             
*                                                                               
         GOTO1 TSAR,TSAADD                                                      
         BNL   *+6                                                              
         DC    H'0'                TSAR BUFFER FULL                             
*                                                                               
         BAS   RE,XCTLST           ADD REP BUY KEY                              
         MVI   T2.TLKTYP,X'12'     UNMATCHED                                    
         CLI   BYTE,0              AGENCY BUY LINE?                             
         BZ    *+8                                                              
         MVI   T2.TLKTYP,X'01'     MATCHED                                      
*                                                                               
         MVC   T2.TLLEN,=Y(TLHDRLQ)                                             
         MVC   T2.TLKACMB,BYTE     AGENCY BUY #                                 
         MVI   T2.TLKSORT,X'01'                                                 
         MVI   T2.TLKAMBY,0                                                     
         MVI   T2.TLKABUY,0                                                     
         MVC   T2.TLKRCMB,BYTE2    REP CHAINED MASTER                           
         MVC   T2.TLKRMBY,RBUYKMLN                                              
         MVC   T2.TLKRBUY,RBUYKLIN                                              
         MVC   T2.TLDSKAD,KEY+28                                                
         MVC   T2.TLAGYB,RBUYAGBL                                               
         MVI   T2.TLFLG1,0                                                      
*                                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL6                                                        
         BNE   RDRREP12                                                         
*                                                                               
         CLC   =C'CR=',2(R6)       CREDIT?                                      
         BNE   *+14                NO                                           
         OI    T2.TLFLG1,TLF1CRQ                                                
         MVC   T2.TLAGYB,BYTE3     AGENCY BUY OF MASTER                         
*                                                                               
         CLC   =C'MG=',2(R6)       MAKE GOOD                                    
         BNE   *+8                 NO                                           
         OI    T2.TLFLG1,TLF1MGQ                                                
*                                                                               
RDRREP12 DS    0H                                                               
         L     R6,AIO                                                           
         MVC   HALF,RBUYTSPT                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         AP    TOTRSPTS,DUB                                                     
*                                                                               
         ICM   R0,15,RBUYTCOS                                                   
         CVD   R0,DUB                                                           
         AP    TOTRCOST,DUB                                                     
*                                                                               
         MVC   TLSVKEY,T2.TLKEY                                                 
         GOTO1 TSAR,TSAADD                                                      
         BNL   *+6                                                              
         DC    H'0'                TSAR BUFFER FULL                             
*                                                                               
         BAS   RE,XCTLST           BUILD BUY DETAIL REC0RD                      
         MVI   T2.TLKTYP,C'Z'                                                   
         MVI   T2.TLBKTYP,C'R'                                                  
         MVC   T2.TLBKBUY,TK1.TLKRBUY                                           
         MVI   T2.TLBFLG1,0                                                     
         LA    R2,T2.TLBDATA                                                    
*                                                                               
         L     R6,AIO              COPY BUY ELEMENT                             
         LA    R6,RBUYELEM-RBUYREC(R6)                                          
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R6)                                                    
         USING RBUYELEM,R2         CLEAR SOME STUFF WE DON'T NEED               
         XC    RBUYCREA,RBUYCREA                                                
         XC    RBUYKMOD,RBUYKMOD                                                
         XC    RBUYCHGI,RBUYCHGI                                                
         XC    RBUYTSPT,RBUYTSPT                                                
         XC    RBUYTCOS,RBUYTCOS                                                
         XC    RBUYTWKS,RBUYTWKS                                                
         XC    RBUYFLT,RBUYFLT                                                  
         XC    RBUYVER,RBUYVER                                                  
         XC    RBUYTYP,RBUYTYP                                                  
         XC    RBUYDPT,RBUYDPT                                                  
         XC    RBUYRTS,RBUYRTS                                                  
         DROP  R2                                                               
*                                                                               
         LA    R2,1(RE,R2)                                                      
*                                                                               
         L     R6,AIO              COPY DAY/TIME ELEMENTS                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL6                                                        
         BNE   RDRREP22                                                         
RDRREP20 DS    0H                                                               
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R6)                                                    
         LA    R2,1(RE,R2)                                                      
*                                                                               
         BAS   RE,NEXTEL6                                                       
         BE    RDRREP20                                                         
*                                                                               
RDRREP22 DS    0H                                                               
         L     R6,AIO              COPY EFFECTIVE DATE ELEMENTS                 
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL6                                                        
         BNE   RDRREP26                                                         
RDRREP24 DS    0H                                                               
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R6)                                                    
         LA    R2,1(RE,R2)                                                      
*                                                                               
         BAS   RE,NEXTEL6                                                       
         BE    RDRREP24                                                         
*                                                                               
RDRREP26 DS    0H                                                               
         XC    0(2,R2),0(R2)       EOR BYTE                                     
         LA    R2,1(R2)                                                         
*                                                                               
         LA    RE,T2.TLREC         RECORD LENGTH                                
         SR    R2,RE                                                            
         STCM  R2,3,T2.TLLEN                                                    
*                                  SPOT GRID - SKIP FOR NOW                     
*&&XX*&& GOTO1 =A(BLDGRID),DMCB,GRID,T2.TLBDATA,RR=Y                            
*&&XX*&& MVC   T2.TLBGRID(L'GRID),GRID                                          
*                                                                               
         L     R6,AIO              CHECK FOR CREDIT                             
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL6                                                        
         BNE   RDRREP32                                                         
*                                                                               
         CLC   =C'CR=',2(R6)       CREDIT?                                      
         BNE   *+8                                                              
         OI    T2.TLBFLG1,X'80'    YES - NEGATIVE SPOTS                         
*                                                                               
         L     R6,AIO              CHECK FOR MAKEGOOD APPLIED                   
         MVI   ELCODE,X'56'                                                     
         BAS   RE,GETEL6                                                        
         BNE   *+8                                                              
         OI    T2.TLFLG1,TLF1MAQ   YES - SET FLAG                               
*                                                                               
RDRREP28 DS    0H                                                               
         CLC   =C'P=',2(R6)        PROGRAM NAME                                 
         BNE   RDRREP30            NO                                           
*                                                                               
         ZIC   RE,1(R6)                                                         
         SH    RE,=Y(4+1)                                                       
         LTR   RE,RE                                                            
         BM    RDRREP32            NO LENGTH JUST GIVE UP                       
         CH    RE,=Y(L'TLBPROG)                                                 
         BL    *+8                                                              
         LA    RE,L'TLBPROG-1                                                   
         EX    RE,*+8                                                           
         B     RDRREP32                                                         
         MVC   T2.TLBPROG(0),4(R6)                                              
*                                                                               
RDRREP30 DS    0H                                                               
         BAS   RE,NEXTEL6                                                       
         BE    RDRREP28                                                         
*                                                                               
RDRREP32 DS    0H                                                               
         OC    T2.TLBPROG,SPACES                                                
         GOTO1 TSAR,TSAADD                                                      
         BNL   *+6                                                              
         DC    H'0'                TSAR BUFFER FULL                             
*                                                                               
         TM    T2.TLBFLG1,X'80'    CREDIT?                                      
         BZ    RDRREP40            NO                                           
*                                                                               
         GOTOX (1,MVCTLST)         KEEP THIS FOR COLLAPSING HELL                
*                                                                               
         L     R6,AIO              CHECK FOR CREDIT                             
         BAS   RE,XCTLST           MARK MASTER AS HAVING A CREDIT               
         MVI   T2.TLKTYP,C'Z'                                                   
         MVI   T2.TLBKTYP,C'R'                                                  
         MVC   T2.TLBKBUY,RBUYKMLN                                              
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                HAD TO BE THERE                              
*                                                                               
         OI    T2.TLBFLG1,X'40'                                                 
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
**JRD  COLLAPSE CREDIT INTO MASTER BUY???                                       
*                                                                               
*                                                                               
RDRREP40 DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     RDRREP02                                                         
*                                                                               
RDRREPX  DS    0H                                                               
         EJECT                                                                  
*------------------*                                                            
* READ AGENCY BUYS                                                              
*------------------*                                                            
RDRAGY00 DS    0H                                                               
         MVI   BUYNUM,0                                                         
*                                                                               
         XC    KEY,KEY             TRAILER HAS RECORD TOTALS                    
         LA    RE,KEY                                                           
         USING RDARKEY,RE                                                       
         MVC   RDARKEY(L'PRBUYKEY),PRBUYKEY                                     
         MVI   RDARKRT,X'50'       TYPE TRAILER                                 
         MVI   RDARKSEQ,0                                                       
         MVI   RDARKSRT,0                                                       
         DROP  RE                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   RDRAGYX             NO TRAILER? EXIT                             
*                                                                               
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        HEADER DESCRIPTION ELEMENT                   
         USING RDARCOD9,R6                                                      
         BAS   RE,GETEL6                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,RDARTSPT                                                    
         CVD   R0,DUB                                                           
         ZAP   TOTXSPTS,DUB                                                     
*                                                                               
         ZAP   TOTXCOST,RDARTDOL                                                
*                                                                               
RDRAGY02 DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING RDARKEY,RE                                                       
         MVC   RDARKEY(L'PRBUYKEY),PRBUYKEY                                     
         MVI   RDARKRT,X'40'       TYPE BUY                                     
         MVC   RDARKSEQ,BUYNUM                                                  
         DROP  RE                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   RDRAGYX             NO AGY BUYS                                  
*                                                                               
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDARKEY,R6                                                       
         MVC   PRBUYKEY,KEY                                                     
         MVC   BUYNUM,RDARKSEQ                                                  
*                                                                               
         GOTO1 =A(CHKDAREC),DMCB,(R6),RR=Y                                      
         BNE   RDRAGY50            RECORD IS DELETED - READ NEXT                
*                                                                               
         MVC   BYTE2,BUYNUM                                                     
         MVI   ELCODE,X'10'        REVISION LINK ELEMENT                        
         USING RDARRVEL,R6                                                      
         BAS   RE,GETEL6                                                        
         BNE   *+10                                                             
         MVC   BYTE2,RDARRVBY                                                   
*                                                                               
         MVC   BYTE3,BYTE2         NEED THIS LATER                              
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        HEADER DESCRIPTION ELEMENT                   
         USING RDARBYEL,R6                                                      
         BAS   RE,GETEL6                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   BYTE3,BUYNUM        NEED TO FIND MASTER?                         
         BE    RDRAGY10            NO                                           
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'A'                                                   
         MVC   T2.TLLKMBUY,BYTE3                                                
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                 MASTER SHOULD BE FOUND                       
         DC    H'0'                                                             
*                                                                               
         MVC   BYTE2,T2.TLLCMAST   BYTE2 GETS CHAINED MASTER                    
*                                                                               
         OI    T2.TLLFLAGS,TLLFMGAQ                                             
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                 HUH?                                         
         DC    H'0'                                                             
*                                                                               
         GOTOX (1,MVCTLST)                                                      
*                                                                               
         BAS   RE,XCTLST           MARK MASTER AS MAKEGOOD APPLIED              
         MVI   T2.TLKTYP,X'01'             CHECK MATCHED FIRST                  
         MVI   T2.TLKSORT,0                                                     
         MVC   T2.TLKACMB,TS1.TLLCMAST     CHAINED MASTER                       
         MVC   T2.TLKAMBY,TS1.TLLMAST      MASTER OF MASTER                     
         MVC   T2.TLKABUY,BYTE3            MASTER                               
*                                                                               
         GOTO1 TSAR,TSARDH                                                      
         BE    RDRAGY04                    MASTER FOUND                         
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,X'11'             CHECK UNMATCHED                      
         MVI   T2.TLKSORT,0                                                     
         MVC   T2.TLKACMB,TS1.TLLCMAST     CHAINED MASTER                       
         MVC   T2.TLKAMBY,TS1.TLLMAST      MASTER OF MASTER                     
         MVC   T2.TLKABUY,BYTE3            MASTER                               
*                                                                               
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                         MASTER SHOULD BE FOUND               
         DC    H'0'                                                             
*                                                                               
RDRAGY04 DS    0H                                                               
         OI    T2.TLFLG1,TLF1MAQ                                                
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                 HUH?                                         
         DC    H'0'                                                             
*                                                                               
RDRAGY10 DS    0H                                                               
         BAS   RE,XCTLST           ADD BUY KEY FOR LINKING INFO                 
         MVC   T2.TLLEN,=Y(TLLMBYLQ)                                            
         MVI   T2.TLKTYP,C'A'                                                   
         MVC   T2.TLLKMBUY,BUYNUM                                               
         MVC   T2.TLLCMAST,BYTE2                                                
         MVC   T2.TLLMAST,BYTE3                                                 
         CLC   BYTE3,BUYNUM        MAKEGOOD?                                    
         BE    *+8                 NO                                           
         OI    T2.TLLFLAGS,TLLFIMGQ                                             
*                                                                               
         GOTO1 TSAR,TSAADD                                                      
         BNL   *+6                                                              
         DC    H'0'                TSAR BUFFER FULL                             
*                                                                               
         MVI   BYTE,X'11'          UNMATCHED                                    
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'X'      READ FOR AGY->REP HARD LINK                  
         MVC   T2.TLHKBUY,BUYNUM                                                
         GOTO1 TSAR,TSARDH                                                      
         BE    RDRAGY12                                                         
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,X'01'     READ FOR REP BUY                             
         MVC   T2.TLKACMB,BYTE2    1ST CHECK CHAINED MASTER                     
         MVI   T2.TLKSORT,X'01'                                                 
         MVI   T2.TLKAMBY,0                                                     
         MVI   T2.TLKABUY,0                                                     
         MVC   TLSVKEY,T2.TLKEY                                                 
         GOTO1 TSAR,TSARDH                                                      
         CLC   T2.TLKEY(TLKABUY-TLKEY),TLSVKEY                                  
         BE    RDRAGY12                                                         
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,X'01'     READ FOR REP BUY                             
         MVC   T2.TLKACMB,BYTE3    AGENCY MASTER(UNCHAINED)                     
         MVI   T2.TLKSORT,X'01'                                                 
         MVC   TLSVKEY,T2.TLKEY                                                 
         GOTO1 TSAR,TSARDH                                                      
         CLC   T2.TLKEY(TLKABUY-TLKEY),TLSVKEY                                  
         BE    RDRAGY12                                                         
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,X'01'     READ FOR REP BUY                             
         MVC   T2.TLKACMB,BUYNUM   CHECK FOR AGENCY BUY                         
         MVI   T2.TLKSORT,X'01'                                                 
         MVC   TLSVKEY,T2.TLKEY                                                 
         GOTO1 TSAR,TSARDH                                                      
         CLC   T2.TLKEY(TLKABUY-TLKEY),TLSVKEY                                  
         BE    RDRAGY12                                                         
*                                                                               
         B     RDRAGY14            BUY NOT MATCHED                              
*                                                                               
RDRAGY12 DS    0H                                                               
         MVI   BYTE,X'01'          MATCHED                                      
*                                                                               
RDRAGY14 DS    0H                                                               
         BAS   RE,XCTLST              ADD AGY BUY KEY                           
         MVC   T2.TLLEN,=Y(TLHDRLQ)                                             
         MVC   T2.TLKTYP,BYTE                                                   
         MVC   T2.TLKACMB,BYTE2       AGENCY CHAINED MASTER                     
         MVI   T2.TLKSORT,X'00'                                                 
         MVC   T2.TLKAMBY,BYTE3       AGENCY MASTER                             
         MVC   T2.TLKABUY,BUYNUM      AGENCY BUY                                
         MVI   T2.TLKRCMB,0           NO REP INFO ON AGY BUY                    
         MVI   T2.TLKRMBY,0                                                     
         MVI   T2.TLKRBUY,0                                                     
         MVC   T2.TLDSKAD,KEY+28                                                
         MVC   T2.TLAGYB,BYTE3                                                  
         MVI   T2.TLFLG1,0                                                      
         CLC   BUYNUM,BYTE3           MAKEGOOD?                                 
         BE    *+8                    NO                                        
         OI    T2.TLFLG1,TLF1MGQ                                                
*&&XX                                                                           
         XC    KEY,KEY             DETAIL HAS NPW/SPW                           
         LA    RE,KEY                                                           
         USING RDARKEY,RE                                                       
         MVC   RDARKEY(L'PRBUYKEY),PRBUYKEY                                     
         MVI   RDARKSRT,X'30'      TYPE BUY DETAIL                              
         DROP  RE                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   RDRAGY18            NO DETAIL                                    
*                                                                               
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 =A(CHKDAREC),DMCB,(R6),RR=Y                                      
         BNE   RDRAGY18            RECORD IS DELETED - READ NEXT                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        DETAIL ELEMENT                               
         USING RDARBUEL,R6                                                      
         BAS   RE,GETEL6                                                        
         BNE   RDRAGY18                                                         
*                                                                               
RDRAGY16 DS    0H                                                               
         ZIC   R0,RDARBUWK                                                      
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),RDARBUSW                                               
         MH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         ZAP   PCKOF16B,DUB                                                     
         AP    TOTASPTS,DUB        ADD TO TOTAL SPOTS                           
         ICM   R0,15,RDARBU$$                                                   
         CVD   R0,DUB                                                           
         MP    PCKOF16B,DUB                                                     
         AP    TOTACOST,PCKOF16B   ADD TO TOTAL DOLLARS                         
*                                                                               
         BAS   RE,NEXTEL6          NEXT COST ELEMENT                            
         BE    RDRAGY16                                                         
*&&                                                                             
RDRAGY18 DS    0H                                                               
         MVC   TLSVKEY,T2.TLKEY                                                 
         GOTO1 TSAR,TSAADD                                                      
         BNL   *+6                                                              
         DC    H'0'                TSAR BUFFER FULL                             
*                                                                               
         BAS   RE,XCTLST           SET TKO DELETED FLAG IN LINK                 
         MVI   T2.TLKTYP,C'A'                                                   
*******  MVC   T2.TLLKMBUY,BYTE3   <- OOPS, MESSED UP WITH ZC                   
         MVC   T2.TLLKMBUY,BUYNUM                                               
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                 SHOULD BE FOUND                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(BUILDREP),RR=Y   BUILD REP BUYLINE IN AIO                     
         BE    *+8                                                              
         OI    T2.TLLFLAGS,TLLFTKOQ      TKO DELETED                            
*                                                                               
         CP    TOTBSPTS,=P'0'                                                   
         BNE   *+8                                                              
         OI    T2.TLLFLAGS,TLLF0SPQ      ZERO SPOTS ON BUY                      
*                                                                               
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                                                              
         DC    H'0'                HUH?                                         
*                                                                               
         TM    T2.TLLFLAGS,TLLFTKOQ      TKO DELETED?                           
         BNZ   RDRAGY50                  YES -  NEXT BUY                        
*                                                                               
RDRAGY20 DS    0H                                                               
         L     R6,ALSTAREA         ELEMENTS BUFFERED HERE                       
         MVI   BYTE2,1                                                          
RDRAGY30 DS    0H                                                               
         CLI   0(R6),X'01'         HEADER?                                      
         BE    *+6                                                              
         DC    H'0'                NO -- WHY ARE WE HERE?                       
*                                                                               
         BAS   RE,XCTLST           BUILD BUY DETAIL RECORD                      
         MVI   T2.TLKTYP,C'Z'                                                   
         MVI   T2.TLBKTYP,C'A'                                                  
         MVC   T2.TLBKBUY,BUYNUM                                                
         MVC   T2.TLBKSEQ,BYTE2                                                 
         ZIC   RE,BYTE2                                                         
         LA    RE,1(RE)                                                         
         STC   RE,BYTE2                                                         
         MVI   T2.TLBFLG1,0                                                     
         LA    R2,T2.TLBDATA                                                    
*                                                                               
         CLI   1(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RE,1(R6)            COPY BUY ELEM                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R6)                                                    
         LA    R2,1(RE,R2)                                                      
*                                                                               
RDRAGY32 DS    0H                                                               
         LA    R6,1(RE,R6)         NEXT ELEMENT                                 
         CLI   0(R6),0             EOR?                                         
         BE    RDRAGY34            YES                                          
         CLI   0(R6),X'01'         NEW BUY RECORD?                              
         BE    RDRAGY34            YES                                          
*                                                                               
         CLI   1(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RE,1(R6)            COPY ELEM                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R6)                                                    
         LA    R2,1(RE,R2)                                                      
         B     RDRAGY32                                                         
*                                                                               
RDRAGY34 DS    0H                  ADD RECORD                                   
         XC    0(2,R2),0(R2)       EOR BYTE                                     
         LA    R2,1(R2)                                                         
*                                                                               
         LA    RE,T2.TLREC         RECORD LENGTH                                
         SR    R2,RE                                                            
         STCM  R2,3,T2.TLLEN                                                    
*                                  SPOT GRID - SKIP FOR NOW                     
*&&XX*&& GOTO1 =A(BLDGRID),DMCB,GRID,T2.TLBDATA,RR=Y                            
*&&XX*&& MVC   T2.TLBGRID(L'GRID),GRID                                          
*                                                                               
         MVC   T2.TLBPROG,MYWORK                                                
         OC    T2.TLBPROG,SPACES                                                
*                                                                               
         GOTO1 TSAR,TSAADD                                                      
         BNL   *+6                                                              
         DC    H'0'                TSAR BUFFER FULL                             
*                                                                               
         CLI   0(R6),X'01'         ANOTHER RECORD?                              
         BE    RDRAGY30            YES                                          
*                                                                               
RDRAGY50 DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         ZIC   RE,BUYNUM                                                        
         LA    RE,1(RE)                                                         
         STC   RE,BUYNUM                                                        
         B     RDRAGY02                                                         
*                                                                               
RDRAGYX  DS    0H                                                               
*---------------------*                                                         
* MAKE A CLEANUP PASS                                                           
*    FOR LOST REP GRANDPARENTS ETC                                              
*---------------------*                                                         
         BAS   RE,XCTLST                                                        
         GOTO1 TSAR,TSARDH         ANY RECORDS?                                 
         BL    RDRECX              NO                                           
*                                                                               
RDREC02  DS    0H                                                               
         CLI   T2.TLKTYP,X'01'     BLOCKED RECORDS?                             
         BNE   RDRECX              NO - ALL DONE THEN                           
*                                                                               
         MVC   TK1.TLKEY,T2.TLKEY                                               
         CLI   T2.TLKSORT,0        AGENCY LINE?                                 
         BNE   RDREC10             NO                                           
         B     RDREC20             SKIP TO NEXT AGENCY                          
*                                                                               
RDREC10  DS    0H                                                               
         CLI   T2.TLKSORT,X'01'    REP LINE?                                    
         BE    *+6                 MUST BE, AND ITS A LINKING ERROR             
         DC    H'0'                                                             
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'A'      GET AGENCY LINK INFO                         
         MVC   T2.TLLKMBUY,TK1.TLKACMB                                          
         GOTO1 TSAR,TSARDH                                                      
         BNE   RDREC20             ITS REALLY GONE - SKIP TO NEXT AGY           
*                                                                               
         GOTOX (1,MVCTLST)                                                      
*                                                                               
         BAS   RE,XCTLST           RESTORE SEQUENCE                             
         MVC   T2.TLKEY,TK1.TLKEY                                               
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RDREC12  DS    0H                                                               
         CLC   T2.TLKACMB,TK1.TLKACMB   STILL IN BLOCK?                         
         BNE   RDREC20                  NO - READ NEXT AGENCY                   
*                                                                               
         GOTO1 TSAR,TSADEL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   T2.TLKACMB,TS1.TLLCMAST  NEW AGENCY GRANDPARENT                  
         GOTO1 TSAR,TSAADD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TK2.TLKEY,T2.TLKEY                                               
         BAS   RE,XCTLST                 UPDATE REP LINK INFO                   
         MVI   T2.TLKTYP,C'R'                                                   
         MVC   T2.TLLKMBUY,TK2.TLKRBUY                                          
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                      HAD TO BE THERE                        
         MVC   T2.TLLMABUY,TS1.TLLCMAST  NEW AGENCY GRANDPARENT                 
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                                                              
         DC    H'0'                      HUH?                                   
*                                                                               
         BAS   RE,XCTLST           GET NEXT RECORD                              
         MVC   T2.TLKEY,TK1.TLKEY                                               
         GOTO1 TSAR,TSARDH                                                      
         BNL   RDREC12                                                          
         B     RDRECX              EOF                                          
*                                                                               
RDREC20  DS    0H                                                               
         BAS   RE,XCTLST           SKIP TO NEXT AGENCY                          
         ZIC   RE,TK1.TLKACMB                                                   
         LA    RE,1(RE)                                                         
         MVC   T2.TLKTYP,TK1.TLKTYP                                             
         STC   RE,T2.TLKACMB                                                    
*                                                                               
         GOTO1 TSAR,TSARDH                                                      
         BNL   RDREC02                                                          
*                                                                               
RDRECX   DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD REP BUYLINES FOR AGENCY BUYLINE                                         
*                                                                               
*   ALL BUILDREP SUBROUTINES SHOULD EITHER BE OF OF BUILDREPS RB,               
*    USE THIER ON BASE REGISTER, OR BE OFF OR R7.                               
*                                                                               
*   BUILDS REP BUYS INTO LISTAREAS AND PUTS THE PROGRAM NAME IN MYWORK          
***********************************************************************         
BUILDREP NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0               CLEAR LIST AREA(S)                           
         SR    R1,R1                                                            
         L     RE,ALSTAREA                                                      
         L     RF,=A(LISTARLQ)                                                  
         MVCL  RE,R0                                                            
         MVC   ANXTEL,ALSTAREA                                                  
         XC    A1STEL,A1STEL                                                    
         NI    MISCFLG1,FF-(MF1DTLP+MF1DAYL+MF1ORBD+MF1ORBP)                    
         MVI   ORBSTDAY,0                                                       
*                                                                               
         ZAP   TOTBSPTS,=P'0'                                                   
         ZAP   TOTBCOST,=P'0'                                                   
*                                                                               
         XC    KEY,KEY             REREAD AGENCY BUY                            
         LA    RE,KEY                                                           
         USING RDARKEY,RE                                                       
         MVC   RDARKEY(L'PRBUYKEY),PRBUYKEY                                     
         DROP  RE                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                WHERE DID THE RECORD GO?                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 =A(CHKDAREC),DMCB,(R6),RR=Y                                      
         BE    *+6                                                              
         DC    H'0'                SHOULDN'T BE HERE                            
*------------------*                                                            
* BUILD BUY HEADER                                                              
*------------------*                                                            
         LA    R6,RDARELEM-RDARREC(R6)                                          
         USING RDARBYEL,R6                                                      
         L     R4,ANXTEL                                                        
         ST    R4,A1STEL                                                        
         USING RBUYELEM,R4         <- INTO BUFFER AREA                          
         MVC   0(2,R4),=X'012B'                                                 
         MVC   RBUYCOS,RDARBYCO    INSERT COST                                  
         MVC   BUYCOST,RDARBYCO    SAVE COST FOR CALCULATIONS                   
         MVC   RBUYCLS(6),SPACES   SET CLASS/SECTION TO SPACES                  
*                                                                               
         MVC   MYWORK(L'TLBPROG),RDARBYPN   PROGRAM NAME                        
*                                                                               
         MVI   STARTDAY,0          CLEAR START/END DAYS OF WEEK                 
         MVI   ENDDAY,0                                                         
         GOTO1 =A(STARTEND),DMCB,RDARBYRO,RBUYSTED,RR=Y                         
         GOTO1 =A(EFFDATEL),DMCB,AIO,RR=Y                                       
*                                                                               
         MVC   RBUYDUR,RDARBYSL    INSERT TOTAL SPOT LENGTH                     
         CLI   RDARBYSL,C'M'       LENGTH IN MINUTES?                           
         BNE   *+8                 NO                                           
         OI    RBUYDUR,X'80'       YES - SET 'MINUTES' INDICATOR                
         MVC   RBUYAGBL,BUYNUM                                                  
         DROP  R4,R6                                                            
*                                                                               
         ZIC   RE,1(R4)                                                         
         LA    R4,0(RE,R4)         SET NEXT ELEMENT POS                         
         ST    R4,ANXTEL                                                        
*-------------------------*                                                     
* LOOP THROUGH AGENCY BUY                                                       
*-------------------------*                                                     
BLREP020 DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 READ NEXT RECORD                             
*                                                                               
         CLC   KEY(25),KEYSAVE     SAME KEY THROUGH RECORD TYPE?                
         BNE   BLREP200            NO - DONE WITH THIS BUY                      
         CLC   KEY+25(1),KEYSAVE+25                                             
         BNE   BLREP200            NO - DONE WITH THIS BUY                      
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 =A(CHKDAREC),DMCB,(R6),RR=Y                                      
         BNE   BLREP020            DELETED READ NEXT                            
*                                                                               
BLREP030 DS    0H                                                               
         CLI   KEY+26,X'10'        BUY ORBIT?                                   
         BNE   BLREP040            NO                                           
*                                                                               
         GOTO1 =A(BUYORBIT),DMCB,AIO,RR=Y                                       
         B     BLREP020            READ NEXT                                    
*                                                                               
BLREP040 DS    0H                                                               
         CLI   KEY+26,X'30'        BUY DETAIL?                                  
         BNE   BLREP050            NO                                           
*                                                                               
         GOTO1 =A(BUYDETL),DMCB,AIO,RR=Y                                        
         BE    BLREP020            READ NEXT                                    
*                                                                               
         SR    R0,R0               CLEAR LIST AREA(S)                           
         SR    R1,R1               FOR TKO DELETE                               
         L     RE,ALSTAREA                                                      
         L     RF,=A(LISTARLQ)                                                  
         MVCL  RE,R0                                                            
         B     EXITL                                                            
*                                                                               
BLREP050 DS    0H                                                               
         B     BLREP020            READ NEXT                                    
*-------------------------*                                                     
* DONE READING AGENCY BUY                                                       
*-------------------------*                                                     
BLREP200 DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
*                                                                               
*   STARTEND:  CONVERTS ROTATION MATRIX AND START DAY TO ONE-BYTE               
*        START/END DAY, INSERTS INTO ADDRESS                                    
*                                                                               
***********************************************************************         
STARTEND NTR1                                                                   
         L     R2,0(R1)            A(INPUT: ROTATION+START DAY)                 
         L     R3,4(R1)            A(RECEIVING FIELD)                           
*                                                                               
         ZIC   RF,7(R2)            ROTATION START DAY                           
         SLL   RF,28               STRIP OFF ZONE BITS                          
         SRL   RF,28               SHIFT START DAY BACK                         
         STC   RF,ROTATDAY         SAVE ROTATION START DAY                      
*                                                                               
         LA    R6,0(R2)            A(ROTATION FIELD)                            
         LR    RE,R6               CALCULATE END OF ROTATION FIELD              
         LA    RE,7(RE)                                                         
         AR    R6,RF               GET A(1ST DAY+1)                             
         BCTR  R6,0                BACK OFF TO A(1ST ENTRY)                     
         LR    RF,R6               SAVE A(1ST ENTRY)                            
*                                     FOR WHEN 1ST ENTRY IS ONLY ENTRY          
         LA    R0,7                SET LOOP CONTROL TO SIX DAYS                 
STEX0040 EQU   *                                                                
         CR    R6,RE               END OF ROTATION FIELD REACHED?               
         BNE   STEX0080            NO                                           
         LA    R6,0(R2)            YES  - GO BACK TO FIRST LOCATION             
STEX0080 EQU   *                                                                
         CLI   0(R6),C' '          ARRAY POSITION USED?                         
         BE    STEX0200            NO  - SKIP IT                                
         CLI   0(R6),0             ARRAY POSITION USED?                         
         BE    STEX0200            NO  - SKIP IT                                
         CLI   STARTDAY,0          ANYTHING IN START DAY?                       
         BNE   STEX0120            YES - DON'T REPLACE                          
         LA    R4,7                NO  - CALCULATE STARTDAY                     
         SR    R4,R0               SUBTRACT REMAINING DAYS                      
         ZIC   R1,ROTATDAY         OFFSET BY ROTATION START DAY                 
         AR    R4,R1                                                            
         CH    R4,=H'7'            WRAPAROUND?                                  
         BNH   STEX0100            NO                                           
         SH    R4,=H'7'            YES - SUBTRACT 7                             
STEX0100 EQU   *                                                                
         STC   R4,STARTDAY         SAVE CALCULATED STARTDAY                     
STEX0120 EQU   *                                                                
         OC    0(1,R3),0(R3)       RECEIVING FIELD ENTRY MADE?                  
         BNZ   STEX0160            YES - START DAY ENTERED                      
         SLL   R4,4                NO  - MOVE START DAY TO HIGH NYBBLE          
         STC   R4,0(R3)            INSERT INTO RECORD                           
STEX0160 EQU   *                                                                
         LR    RF,R6               YES - SAVE NEW ARRAY POSITION                
STEX0200 EQU   *                                                                
         LA    R6,1(R6)            BUMP TO NEXT ARRAY LOCATION                  
         BCT   R0,STEX0040         GO BACK AND CHECK NEXT                       
         LA    R6,0(R2)            A(START OF ARRAY)                            
         BCTR  R6,0                BACK UP 1 POSITION                           
         SR    RF,R6               CALCULATED DISPLACEMENT                      
         STC   RF,ENDDAY           SAVE END DAY FOR EFF DATE SETTING            
         ZIC   RE,0(R3)            RETRIEVE START DAY                           
         AR    RF,RE               ADD START TO END                             
         STC   RF,0(R3)            PUT IT BACK IN RECORD                        
         LA    R6,0(R2)            COUNT NUMBER OF DAYS                         
         SR    RF,RF                                                            
         LA    R0,7                                                             
STEX0240 EQU   *                                                                
         CLI   0(R6),C' '          DAY ACTIVE?                                  
         BE    STEX0280            NO                                           
         CLI   0(R6),0             DAY ACTIVE?                                  
         BE    STEX0280            NO                                           
         LA    RF,1(RF)            YES - ADD 1                                  
STEX0280 EQU   *                                                                
         LA    R6,1(R6)            BUMP TO NEXT POSITION                        
         BCT   R0,STEX0240         GO BACK AND CHECK NEXT                       
         C     RF,=F'1'            COUNT = 1?                                   
         BH    STEX0320            NO  - HIGHER - EXIT                          
         BE    *+6                 YES - SET START=END IN RECORD                
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                     MEANS EMPTY ARRAY!!!                      
         ZIC   RF,0(R3)            RETRIEVE START/END DAY                       
         SLL   RF,28               DROP START DAY                               
         SRL   RF,24               MOVE END DAY BACK TO HI NYBBLE               
         NI    0(R3),X'0F'         CLEAR START DAY                              
         ZIC   RE,0(R3)            RETRIEVE 0/END DAY                           
         AR    RE,RF               ADD NEW START DAY                            
         STC   RE,0(R3)            MOVE IT BACK                                 
STEX0320 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                               
*   EFFDATEL:  BUILD AN ALTERNATE X'02' ELEMENT IN EVENT THERE                  
*        ARE NO ORBIT RECORDS.(ELEMENT IS BUILT IN ELEM)                        
*                                                                               
***********************************************************************         
EFFDATEL NTR1                                                                   
         L     R3,0(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
         LA    R6,RDARELEM         SET A(X'01' ELEMENT)                         
         USING RDARBYEL,R6                                                      
*                                                                               
         XC    STARTDAY(2),STARTDAY                                             
*                                  CLEAR START/END DAY WORK AREA                
         XC    ELEM,ELEM           CLEAR ELEMENT BUILD AREA                     
         MVC   ELEM(2),=X'0209'    INSERT ELEMENT CODE/LENGTH                   
*                                  INSERT START/END DAY                         
         GOTO1 =A(STARTEND),DMCB,RDARBYRO,ELEM+2,RR=Y                           
                                                                                
         SR    RE,RE               INITIALIZE FINAL OUTPUT                      
         LA    RF,7                SET LOOP CONTROL                             
         LA    R1,RDARBYRO         SET A(ROTATION ARRAY)                        
EFFD0080 EQU   *                                                                
         CLI   0(R1),C' '          DAY SET?                                     
         BE    EFFD0120            NO                                           
         CLI   0(R1),0             DAY SET?                                     
         BE    EFFD0120            NO                                           
         LA    RE,1(RE)            YES - TURN ON LOW-ORDER BIT                  
EFFD0120 EQU   *                                                                
         SLL   RE,1                SHIFT UP 1 'DAY'                             
         LA    R1,1(R1)            BUMP TO NEXT ARRAY POSITION                  
         BCT   RF,EFFD0080         GO BACK AND TEST NEXT                        
         SRL   RE,1                SHIFT DOWN 1 'DAY' FOR                       
*                                     PROPER ALIGNMENT                          
         STC   RE,ELEM+3           INSERT DAYS INTO ELEMENT                     
         ZICM  RF,RDARBYST,2       CHECK START TIME                             
         C     RF,=F'2400'         AFTER MIDNIGHT?                              
         BNH   EFFD0160            NO  - LEAVE AS IS.....                       
         S     RF,=F'2400'         YES - SUBTRACT 2400 FROM FIGURE              
EFFD0160 EQU   *                                                                
         STCM  RF,3,ELEM+4         SAVE START TIME                              
         ZICM  RF,RDARBYET,2       CHECK END   TIME                             
         C     RF,=F'2400'         AFTER MIDNIGHT?                              
         BNH   EFFD0200            NO  - LEAVE AS IS.....                       
         S     RF,=F'2400'         YES - SUBTRACT 2400 FROM FIGURE              
EFFD0200 EQU   *                                                                
         STCM  RF,3,ELEM+6         SAVE END   TIME                              
*                                  INSERT START/END TIMES INTO ELEMENT          
         MVI   ELEM+8,1            INSERT WEIGHT OF 1                           
*                                  THIS DOESN'T SEEM TO CHANGE                  
*                                     NO IDEA WHY.....                          
         B     EXIT                                                             
         DROP  R6,R3                                                            
         EJECT                                                                  
**********************************************************************          
*                                                                               
*   BUYORBIT:  BUILDS DAY-TIME ELEMENT FROM THE ORBIT ELEMENTS                  
*        IN THE RECORD, INSERTS THEM INTO THE NEW BUY RECORD.                   
*                                                                               
*   NOTE:  DAY/TIME ELEMENTS ENTERED VIA ORBITS WILL BE DELETED IF              
*      BUY HAS BEEN ENTERED AS A 'DAILY' BUY.  THIS SHOULD NOT HAPPEN,          
*      BUT HAS BEEN HANDLED IF IT DOES.                                         
*                                                                               
**********************************************************************          
BUYORBIT NTR1                                                                   
         L     R6,0(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R6                                                       
         USING RDAROEEL,R6                                                      
         OI    MISCFLG1,MF1ORBP    SET 'ORBIT PRESENT' INDICATOR                
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL6                                                        
         BE    *+6                                                              
         DC    H'0'                SHOULD HAVE BEEN 1                           
*                                                                               
BORB0040 EQU   *                                                                
         L     R4,ANXTEL                                                        
         MVC   0(2,R4),=X'0209'    INSERT ELEMENT CODE/LENGTH                   
*                                                                               
         MVI   STARTDAY,0          CLEAR START/END DAYS                         
         MVI   ENDDAY,0                                                         
         GOTO1 =A(STARTEND),DMCB,RDAROERO,2(R4),RR=Y                            
*                                                                               
         CLI   ORBSTDAY,0          ANY ENTRY IN ORBIT START DAY?                
         BNZ   *+10                                                             
         MVC   ORBSTDAY,STARTDAY   NO  - SAVE FIRST ORBIT START DAY             
*                                                                               
         SR    RE,RE               INITIALIZE FINAL OUTPUT                      
         LA    RF,7                SET LOOP CONTROL                             
         LA    R1,RDAROERO         SET A(ROTATION ARRAY)                        
BORB0080 EQU   *                                                                
         CLI   0(R1),C' '          DAY SET?                                     
         BE    BORB0090            NO                                           
         CLI   0(R1),0             DAY SET?                                     
         BE    BORB0090            NO                                           
         LA    RE,1(RE)            YES - TURN ON LOW-ORDER BIT                  
*                                                                               
BORB0090 EQU   *                                                                
         SLL   RE,1                SHIFT UP 1 'DAY'                             
         LA    R1,1(R1)            BUMP TO NEXT ARRAY POSITION                  
         BCT   RF,BORB0080         GO BACK AND TEST NEXT                        
         SRL   RE,1                SHIFT BACK 1 'DAY'                           
         STC   RE,3(R4)            INSERT DAYS INTO ELEMENT                     
         MVC   4(4,R4),RDAROEST    INSERT START/END TIMES INTO ELEMENT          
         MVI   8(R4),1             INSERT WEIGHT OF 1                           
*                                  THIS DOESN'T SEEM TO CHANGE                  
*                                     NO IDEA WHY.....                          
         ZIC   R0,1(R4)                                                         
         AR    R4,R0               NEXT ELEMENT AREA                            
         ST    R4,ANXTEL                                                        
*                                                                               
         BAS   RE,NEXTEL6          MORE ORBITS?                                 
         BE    BORB0040            YES                                          
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*   BUYDETL:  BUILDS BUY EFFECTIVE DATE ELEMENTS, THEN INSERTS                  
*        THEM INTO THE NEW BUY RECORD.                                          
***********************************************************************         
BUYDETL  NTR1                                                                   
         L     R6,0(R1)            RESET A(X'41' RECORD)                        
*&&TK                                                                           
         OC    TKODATE,TKODATE     NEED TO CHOP ORDER?                          
         BZ    BDET0010            NO                                           
*&&DR                                                                           
         MVC   P(8),=C'PRE TKO:'                                                
         BAS   RE,PRINT                                                         
         GOTO1 =A(PRNTREC),(R6),RR=Y                                            
*&&                                                                             
         ST    R6,DMCB             SETUP CHOPPING CALL                          
         MVC   DMCB+4(3),TKODATE                                                
         MVC   DMCB+8(1),STARTDAY                                               
         MVC   DMCB+9(1),ENDDAY                                                 
         MVC   DMCB+10(1),ROTATDAY                                              
         MVC   DMCB+11(1),ORBSTDAY                                              
         XC    WORK,WORK                                                        
         MVC   WORK(4),DATCON                                                   
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),PERVERT                                               
         LA    RE,WORK                                                          
         ST    RE,DMCB+12                                                       
*                                                                               
         GOTO1 VREDARTK,DMCB                                                    
         BNE   EXITL               BUY DELETED, EXIT                            
*&&DR                                                                           
         MVC   P(9),=C'POST TKO:'                                               
         BAS   RE,PRINT                                                         
         GOTO1 =A(PRNTREC),(R6),RR=Y                                            
*&&                                                                             
*&&                                                                             
BDET0010 DS    0H                                                               
         LA    R6,RDARELEM-RDARREC(R6)                                          
         USING RDARBDEL,R6         BUY DETAIL DESCRIP ELEMENT                   
*                                                                               
         NI    MISCFLG1,FF-MF1DAYL                                              
         TM    RDARBDFL,X'80'      DAILY BUY?                                   
         BNO   *+8                                                              
         OI    MISCFLG1,MF1DAYL    YES - SET FLAG                               
*                                                                               
         TM    MISCFLG1,MF1DAYL    DAILY BUY?                                   
         BNO   BDET0030            NO                                           
         TM    MISCFLG1,MF1ORBP    ANY ORBIT RECORDS?                           
         BNO   BDET0040            NO                                           
*                                                                               
         L     R4,A1STEL           CLEAR ORBIT X'02' DAY/TIME ELTS              
         MVI   ELCODE,X'02'        R4 - GETS A(FIRST 02)                        
         BAS   R4,FRSTEL4                                                       
         BNE   BDET0028            NONE TO DELETE                               
         LR    RE,R4                                                            
*                                                                               
BDET0020 DS    0H                  RE - GETS END OF 02'S                        
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),X'02'         STILL 02 ELEMENTS?                           
         BE    BDET0020            YES                                          
*                                                                               
         LR    RF,RE               RF - GETS NED OF RECORD                      
BDET0022 DS    0H                                                               
         CLI   0(RF),0             EOR?                                         
         BE    BDET0024            YES                                          
         ZIC   R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     BDET0022                                                         
*                                                                               
BDET0024 DS    0H                                                               
         LR    R0,R4               MOVE OVER FIRST 02                           
*                                  FROM END OF 02'S                             
         SR    RF,RE               LENGTH FROM END OF 02'S TO EOR               
         LR    R1,RF                                                            
         MVCL  R0,RE               SQUISH                                       
*                                                                               
BDET0028 DS    0H                                                               
         NI    MISCFLG1,FF-MF1ORBP    SET ORBIT FLAG TO NO                      
         OI    MISCFLG1,MF1ORBD       SET 'ORBIT DROPPED' FLAG                  
         B     BDET0040                                                         
*                                                                               
BDET0030 EQU   *                                                                
         TM    MISCFLG1,MF1ORBP    ANY ORBIT RECORDS?                           
         BO    BDET0060            YES - DON'T NEED SECONDARY                   
*                                                                               
BDET0040 EQU   *                                                                
         CLI   ELEM+1,0                                                         
         BNE   *+6                                                              
         DC    H'0'                BAD LENGTH                                   
*                                                                               
         L     R4,ANXTEL                                                        
         ZIC   RE,ELEM+1                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),ELEM                                                     
         LA    R4,1(RE,R4)                                                      
         ST    R4,ANXTEL                                                        
         XC    ELEM,ELEM                                                        
*                                                                               
BDET0060 EQU   *                                                                
         MVI   ELCODE,X'02'        DETAIL ELEMENT                               
         BAS   RE,FRSTEL6                                                       
         USING RDARBUEL,R6                                                      
         BE    *+6                 NONE FOUND                                   
         DC    H'0'                                                             
*                                                                               
BDET0080 EQU   *                                                                
         TM    MISCFLG1,MF1DAYL    DAILY BUY?                                   
         BO    BDET0100            YES - SKIP CHECK FOR COST BREAK ->           
*                                     EACH ELEMENT BECOMES A BUY                
*                                                                               
         GOTO1 =A(DETLBRAK),DMCB,(R6),RR=Y                                      
*                                                                               
BDET0100 EQU   *                                                                
         OI    MISCFLG1,MF1DTLP    SET DETAILS TO YES                           
         L     RE,A1STEL                                                        
         USING RBUYELEM,RE                                                      
         OC    RBUYNW,RBUYNW       NUMBER/WEEK FILLED IN?                       
         BNZ   *+10                YES                                          
         MVC   RBUYNW,RDARBUSW     NO  - INSERT NUMBER PER WEEK                 
         MVC   BYTE,RBUYNW         NEED THIS LATER                              
         DROP  RE                                                               
*                                                                               
         L     R4,ANXTEL                                                        
         MVC   0(2,R4),=X'030B'    INSERT ELEMENT CODE/LENGTH                   
*                                  CONVERT START DATE TO EBCDIC                 
         GOTO1 DATCON,DMCB,(2,RDARBUSD),(0,WORK)                                
*                                                                               
         TM    MISCFLG1,MF1DAYL    DAILY BUY?                                   
         BNO   *+14                                                             
         MVC   WORK+12(6),WORK     YES - USE START AS IS, THEN                  
         B     BDET0190               SET END TO START                          
*                                                                               
         ZIC   RF,RDARBUWK         CALCULATE NUMBER OF DAYS                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         BCTR  RF,0                MAKE WEEKS ZERO RELATIVE                     
         MH    RF,=H'7'            MULTIPLY WEEKS BY 7                          
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
*                                                                               
         GOTO1 GETDAY,DMCB,WORK,WORK+12                                         
*                                  GET DAY OF START WEEK                        
         ZIC   RF,DMCB             GET DAY OF WEEK                              
         BCTR  RF,0                MAKE DAY ZERO RELATIVE                       
         LTR   RF,RF               MONDAY?                                      
         BZ    BDET0140            YES - LEAVE                                  
         LNR   RF,RF               NEGATE REGISTER                              
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                  BACK UP TO MONDAY                            
*                                                                               
* IN CASE OF OUT OF WEEK ROTATORS, COMPARE BUY START DAY AND ROTATION           
* START DAY TO GET THE CORRECT MONDAY DATE                                      
*                                                                               
         CLC   STARTDAY,ROTATDAY                                                
         BNL   BDET0135                                                         
         MVC   WORK(6),WORK+12                                                  
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                                                               
BDET0135 DS    0H                                                               
         MVC   WORK(6),WORK+12                                                  
*                                                                               
BDET0140 EQU   *                                                                
         CLI   ORBSTDAY,0          ANY ORBIT START DAY?                         
         BZ    BDET0160            NO  - USE HEADER START DAY                   
         ZIC   RF,ORBSTDAY         YES - USE IT                                 
         B     BDET0180                                                         
BDET0160 EQU   *                                                                
         ZIC   RF,STARTDAY         ADD START DAY                                
BDET0180 EQU   *                                                                
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                  BUMP TO START DAY IN WEEK                    
BDET0190 EQU   *                                                                
*                                  INSERT START DATE INTO ELEMENT               
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,2(R4))                                
*                                                                               
         TM    MISCFLG1,MF1DAYL    DAILY BUY?                                   
         BNO   *+14                                                             
         MVC   5(3,R4),2(R4)                                                    
         B     BDET0260        YES - SET END DATE = START DATE                  
*                                                                               
*                                  GET DAY OF END   WEEK                        
         GOTO1 GETDAY,DMCB,WORK+6,WORK+12                                       
*                                                                               
         ZIC   RF,DMCB             GET DAY OF WEEK                              
         BCTR  RF,0                MAKE DAY ZERO RELATIVE                       
         LTR   RF,RF               MONDAY?                                      
         BZ    BDET0240            YES - LEAVE                                  
         LNR   RF,RF               NEGATE REGISTER                              
         CLC   STARTDAY,ENDDAY     START/END ON SAME DAY?                       
*                                     (SINGLE-DAY BUY?)                         
         BE    BDET0220            YES - DON'T BUMP TO NEXT WEEK                
         BL    BDET0220            START < END DAY:  NOT AN                     
*                                     OOWR - DON'T BUMP                         
         LA    RF,7(RF)            BUMP IT INTO NEXT WEEK                       
*                                     OOWR - DON'T BUMP                         
BDET0220 EQU   *                                                                
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                  BACK UP TO MONDAY                            
*                                                                               
* IN CASE OF OUT OF WEEK ROTATORS, COMPARE BUY START DAY AND ROTATION           
* START DAY TO GET THE CORRECT MONDAY DATE                                      
*                                                                               
         CLC   STARTDAY,ROTATDAY                                                
         BNL   BDET0230                                                         
         MVC   WORK+6(6),WORK+12                                                
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                                                               
BDET0230 DS    0H                                                               
         MVC   WORK+6(6),WORK+12                                                
*                                                                               
BDET0240 EQU   *                                                                
         ZIC   RF,ENDDAY           ADD END   DAY                                
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                  BUMP TO END   DAY IN WEEK                    
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,5(R4))                                
*                                  INSERT END   DATE INTO ELEMENT               
*                                                                               
BDET0260 EQU   *                                                                
         MVI   8(R4),X'80'         SET TO 'EVERY WEEK'                          
         CLC   BYTE,RDARBUSW       HEADER #/WK = DETAIL #/WK?                   
         BE    *+8                                                              
         OI    8(R4),X'01'         NO  - PUT IN 'OVERRIDE' FLAG                 
*                                                                               
         CLI   RDARBUWK,0          JRD - FOR JODY                               
         BNE   *+8                                                              
         MVI   RDARBUWK,1                                                       
*                                                                               
         MVC   9(1,R4),RDARBUSW    INSERT NUMBER PER WEEK                       
         MVC   10(1,R4),RDARBUWK   INSERT NUMBER OF WEEKS                       
*                                                                               
         ZIC   R0,RDARBUWK         ACCUMULATE NUMBER OF WEEKS                   
         XC    HALF,HALF              AND CALC # SPOTS                          
         MVC   HALF+1(1),RDARBUSW  NUMBER SPOTS PER WEEK                        
         MH    R0,HALF             # SPOTS X # WEEKS                            
         CVD   R0,DUB                                                           
         ZAP   PCKOF16B,DUB                                                     
         AP    TOTASPTS,PCKOF16B   ADD TO TOTAL SPOTS                           
         AP    TOTBSPTS,PCKOF16B   ADD TO BUY SPOTS                             
*                                                                               
         ICM   R0,15,RDARBU$$                                                   
         CVD   R0,DUB                                                           
         MP    PCKOF16B,DUB                                                     
         AP    TOTACOST,PCKOF16B   ADD TO TOTAL DOLLARS                         
         AP    TOTBCOST,PCKOF16B   ADD TO BUY COST                              
*                                                                               
         TM    MISCFLG1,MF1DAYL    DAILY BUY?                                   
         BNO   BDET0300                                                         
*                                  YES - GENERATE A BUY RECORD                  
         ZIC   R0,1(R4)            NEXT ELEMENT AREA                            
         AR    R4,R0                                                            
         ST    R4,ANXTEL                                                        
*                                                                               
         GOTO1 =A(GENDAILY),DMCB,(R6),RR=Y                                      
         L     R4,ANXTEL           <- DO THIS, WHICH DOESN'T SET CC             
         BNE   BDET0320                                                         
*                                                                               
BDET0300 EQU   *                                                                
         ZIC   R0,1(R4)            NEXT ELEMENT AREA                            
         AR    R4,R0                                                            
         ST    R4,ANXTEL                                                        
*                                                                               
         MVI   ELCODE,X'02'        DETAIL ELEMENT                               
         BAS   RE,NEXTEL6                                                       
         BE    BDET0080            GO BACK FOR NEXT ELEMENT                     
*                                                                               
BDET0320 EQU   *                                                                
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*   GENDAILY:  FOR 'DAILY' BUYS, EACH DETAIL IS TO BE A SEPARATE BUY            
*        RECORD.  THE DETAIL'S DETAILS ARE INSERTED INTO THE HEADER,            
*        AND THE RECORD WRITTEN TO THE FILE.                                    
*                                                                               
***********************************************************************         
GENDAILY NTR1                                                                   
         L     R6,0(R1)                                                         
         USING RDARBUEL,R6                                                      
         L     R4,A1STEL                                                        
         USING RBUYELEM,R4                                                      
*                                                                               
         OI    RBUYFLG2,X'80'      SET 'DAILY ORDER' FLAG                       
         MVC   RBUYCOS,RDARBU$$    INSERT SPOT COST                             
         MVC   BUYCOST,RDARBU$$    SAVE   SPOT COST FOR CALCULATION             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 DATCON,DMCB,(2,RDARBUSD),(0,WORK)                                
*                                  CONVERT START DATE TO EBCDIC                 
         GOTO1 GETDAY,DMCB,WORK,WORK+12                                         
*                                  GET DAY OF WEEK OF START DAY                 
         ZIC   RF,DMCB             GET DAY OF WEEK:  START DAY                  
         SLL   RF,4                SHIFT TO LOW ORDER, HI NYBBLE                
         ZIC   RE,DMCB             GET DAY OF WEEK:  END   DAY                  
         AR    RF,RE               ADD END DAY TO START DAY                     
         STC   RF,RBUYSTED         INSERT START/END DAYS                        
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'02'        MORE DETAILS?                                
         BAS   RE,NEXTEL6                                                       
         BNE   GDAIXNE             NO DON'T ADD ANOTHER HEADER                  
*                                                                               
         L     R6,A1STEL                                                        
GDAI0020 EQU   *                                                                
         MVI   ELCODE,X'02'        DAY/TIME ELEMENT?                            
         USING RBUYDYEL,R6                                                      
         BAS   RE,NEXTEL6                                                       
         BE    *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
         MVC   RBUYDYIN,RBUYSTED   INSERT START/END FROM HDR                    
*                                                                               
         ZIC   RE,RBUYSTED         GET ST/END DAYS                              
         SRL   RE,4                DROP THE END DAY                             
         LA    R3,DAYTABLE                                                      
         AR    R3,RE               ADD DAY TO TABLE ADDR                        
         MVC   RBUYDAYS,0(R3)                                                   
         DROP  R4,R6                                                            
*                                                                               
         L     R4,ANXTEL           CREATE NEW RECORD                            
         L     RF,A1STEL                                                        
         LR    RE,RF                                                            
GDAI0032 DS    0H                                                               
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),X'02'         COPY 01 & 01'S                               
         BE    GDAI0032                                                         
*                                                                               
         SR    RE,RF               LENGTH                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(RF)                                                    
*                                                                               
         ST    R4,A1STEL                                                        
         LA    R4,1(RE,R4)         NEXT ELEMENT POSITION                        
         ST    R4,ANXTEL                                                        
*                                  RESET DETAIL COUNT TO NONE                   
         NI    MISCFLG1,FF-MF1DTLP                                              
*                                                                               
         B     EXITOK                                                           
*                                                                               
GDAIXNE  DS    0H                                                               
         B     EXITL                                                            
*                                                                               
DAYTABLE DC    X'8040201008040201'                                              
         EJECT                                                                  
***********************************************************************         
*                                                                               
*   DETLBRAK:  IF DETAIL COST IS DIFFERENT THAN BUYHDR COST, AND                
*      PREVIOUS DETAILS HAVE BEEN ENCOUNTERED WITHIN THE BUYHDR                 
*      GROUP, THE FOLLOWING STEPS MUST BE TAKEN:                                
*         1.  THE BUY MUST BE OUTPUT                                            
*         2.  THE NEXT BUYLINE NUMBER MUST BE CALCULATED                        
*         3.  THE RECORD MUST BE CLEARED OF DETAIL INFORMATION                  
*         4.  PROCESSING OF THE NEW DETAIL WILL THEN CONTINUE                   
*                                                                               
***********************************************************************         
DETLBRAK NTR1                                                                   
         L     R3,0(R1)            RESET A(DETAIL ELEMENT)                      
         USING RDARBUEL,R3         BUY DETAIL ELEMENT                           
*                                                                               
         L     R4,A1STEL           RESET A(RBUYREC)                             
         USING RBUYELEM,R4                                                      
*                                                                               
         TM    MISCFLG1,MF1DTLP    ANY DETAILS ENCOUNTERED?                     
         BO    DBRA0040                                                         
*                                                                               
         MVC   RBUYCOS,RDARBU$$    NO  - INSERT BUY COST IN CASE                
*                                     DIFFERENT FROM HEADER COST                
         MVC   BUYCOST,RBUYCOS     SAVE COST FOR CALCULATION                    
         MVC   RBUYNW,RDARBUSW     INSERT SPOTS/WK IN CASE DIFFERENT            
         B     DBRA0200            EXIT                                         
*                                                                               
DBRA0040 EQU   *                                                                
         CLC   RBUYCOS,RDARBU$$    BUY COST = DETAIL COST?                      
         BE    DBRA0200            YES - FINISHED                               
*                                                                               
DBRA0060 EQU   *                                                                
         L     R4,ANXTEL           CREATE NEW RECORD                            
         L     RF,A1STEL                                                        
         LR    RE,RF                                                            
DBRA0062 DS    0H                                                               
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),X'02'         COPY 01 & 02'S                               
         BE    DBRA0062                                                         
*                                                                               
         SR    RE,RF               LENGTH                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(RF)                                                    
*                                                                               
         MVC   RBUYCOS,RDARBU$$    INSERT NEW COST                              
         MVC   BUYCOST,RBUYCOS     SAVE COST FOR CALCULATION                    
         MVC   RBUYNW,RDARBUSW     INSERT SPOTS/WK IN CASE DIFFERENT            
*                                                                               
         ST    R4,A1STEL                                                        
         LA    R4,1(RE,R4)         NEXT ELEMENT POSITION                        
         ST    R4,ANXTEL                                                        
*                                  RESET DETAIL COUNT TO NONE                   
         NI    MISCFLG1,FF-MF1DTLP                                              
*                                                                               
DBRA0200 EQU   *                                                                
         B     EXIT                                                             
         DROP  R4,R3                                                            
*---------------------------*                                                   
* END OF BUILD REP ROUTINES *                                                   
*---------------------------*                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD A BUY GRID                                                              
* P1 = A(BUYGRID)                                                               
* P2 = A(BUY RECORD) TO BE GRIDED(TSAR)                                         
*                                                                               
* NOTE: THIS ROUTINE NEEDS TO BE CHANGED TO HANDLE CASES WHEN THE               
*  HEADER FLIGHT DATES CHANGE.  RIGHT NOW IT DUMPS AND SHOULD NOT BE            
*  USED!!!!!                                                                    
***********************************************************************         
BLDGRID  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(BUYGRID)                                   
         LR    R3,R2               SAVE OFF GRID STARTING POINT                 
         L     R6,4(R1)            A(BUY RECORD) TO BE GRIDED                   
         XC    0(L'GRID,R2),0(R2)  CLEAR GRID                                   
*                                                                               
         MVI   ELCODE,X'03'                                                     
         BAS   RE,FRSTEL6                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RBUYDTEL,R6                                                      
*                                                                               
*                                                                               
* FIND STARTING POINT IN GRID FOR NEXT SET OF EFFECTIVE DATES                   
*                                                                               
BGRID10  DS    0H                                                               
         XC    ELEM,ELEM                                                        
         XC    WORK2,WORK2                                                      
         GOTO1 DATCON,DMCB,(3,CCONDAT),(5,ELEM)                                 
         MVI   ELEM+8,C'-'                                                      
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(5,ELEM+9)                              
         GOTO1 PERVAL,DMCB,(17,ELEM),WORK2                                      
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WKD      USING PERVALD,WORK2                                                    
         ZICM  R1,WKD.PVALNWKS,2   # WEEKS ROUNDED UP                           
         BCTR  R1,0                ADJUST FOR ROUNDING                          
         DROP  WKD                                                              
*                                                                               
         LA    R2,0(R1,R3)         BUMP TO STARTING CELL FOR THIS DATE          
*                                                                               
         ZIC   R1,RBUYDTWK         NUMBER OF WEEKS                              
         LTR   R1,R1                                                            
         BZ    BGRID20             FILL EACH CELL WITH THE AGENCY BUY'S         
         MVC   0(1,R2),RBUYDTNW                                                 
         CLI   RBUYDTWK,1                                                       
         BE    BGRID20                                                          
         SH    R1,=H'2'                                                         
         EX    R1,*+8              NUMBER OF SPOTS                              
         B     *+10                                                             
         MVC   1(0,R2),0(R2)                                                    
*                                                                               
BGRID20  DS    0H                  INCASE OF ORBITS                             
         BAS   RE,NEXTEL6                                                       
         BE    BGRID10                                                          
         DROP  R6                                                               
*                                                                               
BGRIDX   DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
*   CHKDAREC:  CHECKS FOR TYPE OF DARE RECORD.  IF SOFT DELETE                  
*        BIT OR HARD DELETE BIT IS SET, CC IS SET TO NON-ZERO,                  
*        INDICATING THAT RECORD IS TO BE SKIPPED/NOT PROCESSED.                 
*        SOFT DELETE BIT (X'80') WILL ALWAYS BE SET WHEN HARD BIT               
*        (X'40') IS SET.  THEREFORE, ONLY SOFT BIT IS TESTED.                   
*                                                                               
***********************************************************************         
CHKDAREC NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            RESET A(DARE RECORD IN PROCESS)              
         USING RDARREC,R2                                                       
*                                                                               
         CLI   RDARKRT,X'10'       AGENCY ORDER HEADER?                         
         BNE   CDAR0040            NO                                           
         LA    R3,RDARELEM         YES -                                        
         USING RDARELEM,R3                                                      
*                                                                               
         TM    RDARDELS,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
*                                                                               
CDAR0040 EQU   *                                                                
         CLI   RDARKRT,X'40'       BUY?                                         
         BNE   CDAR0080            NO                                           
         CLI   RDARKSRT,X'00'      BUY HEADER?                                  
         BNE   CDAR0120            NO  - ORB/COMMT/DETAIL                       
*                                                                               
         LA    R3,RDARELEM         YES -                                        
         USING RDARBYEL,R3                                                      
*                                                                               
         ZIC   RF,RDARBYLN         CHECK LENGTH                                 
         LA    RE,RDARBYOL         SET OLD ELEMENT LENGTH                       
         CR    RF,RE               COMPARE OLD VS NEW                           
         BE    CDAR0900            EQUAL: OLD LENGTH FOUND -                    
*                                     NOT KEY-DELETED:  MUST NOT                
*                                     BE SOFT DELETED.                          
CDAR0050 EQU   *                                                                
         TM    RDARBYDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
CDAR0080 EQU   *                                                                
         BH    CDAR0800            SKIP OVER RECORD TYPES                       
*                                     50 (TRAILER) + 60 (EQUIVS)                
*                                        THESE AREN'T SOFT-DELETED.             
CDAR0120 EQU   *                                                                
*                                                                               
*                                  ONLY BUYS WILL HAVE RDARKSRT                 
*                                     SET - STANDARD AND ORDER                  
*                                        COMMENTS WILL NOT                      
         CLI   RDARKSRT,X'30'      BUY DETAIL?                                  
         BL    CDAR0280            NO  - ORB/COMMT                              
         LA    R3,RDARELEM         YES - CHECK FOR MULTI RATES                  
         USING RDARBDEL,R3                                                      
*                                                                               
         TM    RDARBDDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         B     CDAR0800            FINISHED                                     
         DROP  R3                                                               
*                                                                               
CDAR0280 EQU   *                                                                
*                                                                               
*   REMAINING RECORD TYPES ARE 15, 20, 30, 40/10, 40/20, AND                    
*        SOFT/HARD DELETE BYTE IS IN SAME PLACE IN ALL ELEMENTS.                
*        STANDARD COMMENT ELEMENT FORMAT USED, ARBITRARILY.                     
*                                                                               
         LA    R3,RDARELEM                                                      
         USING RDARELE2,R3                                                      
*                                                                               
         TM    RDARELDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
CDAR0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT =                                 
         B     CDAR1000                                                         
CDAR0900 EQU   *                                                                
         CR    R0,R0               SET CC =                                     
CDAR1000 EQU   *                                                                
         B     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT A REP RECORD FOR DEBUGGING                                              
***********************************************************************         
PRNTREC  NTR1  BASE=*,LABEL=*                                                   
         LR    R6,R1                                                            
         MVC   P(12),=C'RECORD DUMP:'                                           
         BAS   RE,PRINT                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R6),P+2,34                                         
         BAS   RE,PRINT                                                         
*                                                                               
         LA    R6,34(R6)           FIRST ELEMENT                                
PRNTR10  DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    PRNTR20                                                          
*                                                                               
         ZIC   R2,1(R6)                                                         
         LA    R4,0(R6)                                                         
         LA    R0,P+5                                                           
PRNTR12  DS    0H                                                               
         LR    R3,R2                                                            
         CH    R3,=H'60'                                                        
         BNH   *+8                                                              
         LA    R3,60                                                            
*                                                                               
         SR    R2,R3                                                            
         GOTO1 HEXOUT,DMCB,(R4),(R0),(R3)                                       
         BAS   RE,PRINT                                                         
*                                                                               
         AR    R4,R3                                                            
         LA    R0,P+10                                                          
         LTR   R2,R2                                                            
         BNZ   PRNTR12                                                          
*                                                                               
         ZIC   R2,1(R6)                                                         
         LA    R6,0(R2,R6)                                                      
         B     PRNTR10                                                          
*                                                                               
PRNTR20  DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT TSAR RECORDS FOR DEBUGGING                                              
*                                                                               
*  IF BYTE 0 OF RF = 0 PRINT THE ENTIRE BUFFER                                  
*                  = 1 PRINT A SINGLE RECORD AND EXIT                           
*                                                                               
***********************************************************************         
PRNTTSAR NTR1  BASE=*,LABEL=*                                                   
         LR    R2,RF                                                            
         CLM   R2,8,=AL1(0)        ENTIRE BUFFER MODE?                          
         BNE   PTSAR02             NO                                           
*                                                                               
         BAS   RE,XCTLST                                                        
         GOTO1 TSAR,TSARDH         ANY RECORDS?                                 
         BNL   PTSAR02             YES                                          
*                                                                               
         MVC   P(30),=CL30'**** NO RECORDS IN TSAR ****'                        
         BAS   RE,PRINT                                                         
         B     PTSARX                                                           
*                                                                               
PTSAR02  DS    0H                                                               
         L     RE,=A(RECTAB)                                                    
         A     RE,RELO                                                          
PTSAR04  CLI   4(RE),0             EOT?                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   T2.TLKTYP,4(RE)     TYPE MATCH?                                  
         BE    *+12                                                             
         LA    RE,RECTABLQ(RE)                                                  
         B     PTSAR04                                                          
*                                                                               
         ICM   RF,15,0(RE)                                                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
PTSAR08  DS    0H                                                               
         CLM   R2,8,=AL1(0)        ENTIRE BUFFER MODE?                          
         BNE   PTSARX              NO                                           
*                                                                               
         GOTOX TSAR,TSANXT                                                      
         BE    PTSAR02                                                          
*                                                                               
PTSARX   DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT TYPE 01, 02, 11, OR 12 TSAR RECORD FROM T2.TLREC                        
***********************************************************************         
PTSARREC NTR1  BASE=*,LABEL=*                                                   
         CLI   T2.TLKSORT,X'FF'    COMMENT LINE?                                
         BNE   *+14                NO                                           
         MVC   P(17),=CL17'COMMENT'                                             
         B     PTREC04                                                          
*                                                                               
         CLI   T2.TLKTYP,X'01'                                                  
         BNE   *+14                                                             
         MVC   P(17),=CL17'XXX MATCHED'                                         
         B     PTREC02                                                          
*                                                                               
         CLI   T2.TLKTYP,X'02'                                                  
         BNE   *+14                                                             
         MVC   P(17),=CL17'XXX SOFT MATCH'                                      
         B     PTREC02                                                          
*                                                                               
         CLI   T2.TLKTYP,X'11'                                                  
         BNE   *+14                                                             
         MVC   P(17),=CL17'AGY UNMATCHED'                                       
         B     PTREC04                                                          
*                                                                               
         CLI   T2.TLKTYP,X'12'                                                  
         BNE   *+14                                                             
         MVC   P(17),=CL17'REP UNMATCHED'                                       
         B     PTREC04                                                          
*                                                                               
PTREC02  DS    0H                                                               
         CLI   T2.TLKSORT,0        AGENCY LINE?                                 
         BNE   *+14                NO                                           
         MVC   P(3),=C'AGY'                                                     
         B     PTREC04                                                          
*                                                                               
         CLI   T2.TLKSORT,X'01'    REP LINE?                                    
         BNE   *+14                NO                                           
         MVC   P(3),=C'REP'                                                     
         B     PTREC04                                                          
*                                                                               
         DC    H'0'                HUH?                                         
*                                                                               
PTREC04  DS    0H                                                               
         EDIT  T2.TLNUM,(5,P+120),ZERO=NOBLANK                                  
         EDIT  T2.TLLEN,(5,P+126),ZERO=NOBLANK                                  
*                                                                               
         EDIT  T2.TLKACMB,(3,P+20),ZERO=NOBLANK                                 
         GOTO1 HEXOUT,DMCB,T2.TLKSORT,P+25,1                                    
*                                                                               
         CLI   T2.TLKSORT,X'FF'    COMMENT LINE?                                
         BE    PTREC30             YES                                          
*                                                                               
         CLI   T2.TLKRBUY,X'00'    REPLINE?                                     
         BNE   PTREC10             YES                                          
*                                                                               
         EDIT  T2.TLKAMBY,(3,P+30),ZERO=NOBLANK                                 
         EDIT  T2.TLKABUY,(3,P+35),ZERO=NOBLANK                                 
         B     PTREC20                                                          
*                                                                               
PTREC10  DS    0H                                                               
         EDIT  T2.TLKRCMB,(3,P+30),ZERO=NOBLANK                                 
         EDIT  T2.TLKRMBY,(3,P+35),ZERO=NOBLANK                                 
         EDIT  T2.TLKRBUY,(3,P+40),ZERO=NOBLANK                                 
*                                                                               
PTREC20  DS    0H                                                               
         GOTO1 HEXOUT,DMCB,T2.TLDSKAD,P+60,4                                    
         EDIT  T2.TLAGYB,(3,P+70),ZERO=NOBLANK                                  
         GOTO1 HEXOUT,DMCB,T2.TLFLG1,P+75,1                                     
         B     PTREC50                                                          
*                                                                               
PTREC30  DS    0H                                                               
         EDIT  T2.TLKAMBY,(3,P+30),ZERO=NOBLANK                                 
         EDIT  T2.TLKABUY,(3,P+35),ZERO=NOBLANK                                 
         EDIT  T2.TLKRCMB,(3,P+40),ZERO=NOBLANK                                 
         EDIT  T2.TLKRMBY,(3,P+45),ZERO=NOBLANK                                 
         EDIT  T2.TLKRBUY,(3,P+50),ZERO=NOBLANK                                 
         GOTO1 HEXOUT,DMCB,T2.TLKTYP,P+60,1                                     
         GOTO1 HEXOUT,DMCB,T2.TLCFLG1,P+75,1                                    
*                                                                               
         ZICM  RE,T2.TLLEN,2                                                    
         SH    RE,=Y(TLCOMLQ+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P2+20(0),T2.TLCCOM                                               
*                                                                               
PTREC50  DS    0H                                                               
         BAS   RE,PRINT                                                         
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT TYPE C'A' OR C'R' LINKING RECORD                                        
***********************************************************************         
PTSARLNK NTR1  BASE=*,LABEL=*                                                   
         CLI   T2.TLKTYP,C'A'                                                   
         BNE   *+14                                                             
         MVC   P(17),=CL17'AGY LINK INFO'                                       
         B     PTLNK02                                                          
*                                                                               
         CLI   T2.TLKTYP,C'R'                                                   
         BNE   *+14                                                             
         MVC   P(17),=CL17'REP LINK INFO'                                       
         B     PTLNK02                                                          
*                                                                               
         DC    H'0'                                                             
*                                                                               
PTLNK02  DS    0H                                                               
         EDIT  T2.TLNUM,(5,P+120),ZERO=NOBLANK                                  
         EDIT  T2.TLLEN,(5,P+126),ZERO=NOBLANK                                  
*                                                                               
         EDIT  T2.TLLKMBUY,(3,P+20),ZERO=NOBLANK                                
         EDIT  T2.TLLCMAST,(3,P+25),ZERO=NOBLANK                                
         EDIT  T2.TLLMAST,(3,P+30),ZERO=NOBLANK                                 
         EDIT  T2.TLLMABUY,(3,P+35),ZERO=NOBLANK                                
         EDIT  T2.TLLMAGBL,(3,P+40),ZERO=NOBLANK                                
         GOTO1 HEXOUT,DMCB,T2.TLLFLAGS,P+55,1                                   
*                                                                               
         BAS   RE,PRINT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT TYPE C'X' HARD LINK RECORD                                              
***********************************************************************         
PTSARHRD NTR1  BASE=*,LABEL=*                                                   
         CLI   T2.TLKTYP,C'X'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(17),=CL17'AGY->REP LINK'                                       
         EDIT  T2.TLNUM,(5,P+120),ZERO=NOBLANK                                  
         EDIT  T2.TLLEN,(5,P+126),ZERO=NOBLANK                                  
*                                                                               
         EDIT  T2.TLHKBUY,(3,P+20),ZERO=NOBLANK                                 
         ZICM  R2,T2.TLLEN,2                                                    
         LA    R2,T2.TLREC(R2)                                                  
         LA    R3,T2.TLHLINKS                                                   
         LA    R4,P+30                                                          
         SR    R6,R6                                                            
*                                                                               
PTHRD02  DS    0H                                                               
         EDIT  (B1,0(R3)),(3,(R4)),ZERO=NOBLANK                                 
         LA    R4,4(R4)                                                         
         LA    R6,1(R6)                                                         
         LA    R3,1(R3)                                                         
*                                                                               
         CLM   R6,1,=AL1(25)       NEED TO PRINT LINE?                          
         BL    PTHRD04             NO                                           
*                                                                               
         BAS   RE,PRINT                                                         
         SR    R6,R6                                                            
         LA    R4,P+30                                                          
*                                                                               
PTHRD04  DS    0H                                                               
         CR    R3,R2                                                            
         BL    PTHRD02                                                          
*                                                                               
         BAS   RE,PRINT                                                         
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT C'Z' BUY DETAIL RECORDS                                                 
***********************************************************************         
PTSARBYD NTR1  BASE=*,LABEL=*                                                   
         CLI   T2.TLKTYP,C'Z'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(17),=CL17'XXX BUY DETAIL'                                      
         CLI   T2.TLBKTYP,C'A'                                                  
         BNE   *+14                                                             
         MVC   P(3),=C'AGY'                                                     
         B     PTBYD02                                                          
*                                                                               
         CLI   T2.TLBKTYP,C'R'                                                  
         BNE   *+14                                                             
         MVC   P(3),=C'REP'                                                     
         B     PTBYD02                                                          
*                                                                               
         DC    H'0'                                                             
*                                                                               
PTBYD02  DS    0H                                                               
         EDIT  T2.TLNUM,(5,P+120),ZERO=NOBLANK                                  
         EDIT  T2.TLLEN,(5,P+126),ZERO=NOBLANK                                  
*                                                                               
         EDIT  T2.TLBKBUY,(3,P+20),ZERO=NOBLANK                                 
         EDIT  T2.TLBKSEQ,(3,P+25),ZERO=NOBLANK                                 
         GOTO1 HEXOUT,DMCB,T2.TLBKSTYP,P+30,1                                   
         GOTO1 HEXOUT,DMCB,T2.TLBFLG1,P+35,1                                    
         MVC   P+40(L'TLBPROG),T2.TLBPROG                                       
*                                                                               
         LA    R2,T2.TLBDATA                                                    
PTBYD10  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    PTBYD20                                                          
*                                                                               
         BAS   RE,PRINT                                                         
         ZIC   R0,1(R2)                                                         
         GOTO1 HEXOUT,DMCB,(R2),P+20,(R0)                                       
         AR    R2,R0                                                            
         B     PTBYD10                                                          
*                                                                               
PTBYD20  DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         LA    R3,L'GRID                                                        
         LA    R2,T2.TLBGRID                                                    
         LA    R4,P+20                                                          
         SR    R6,R6                                                            
PTBYD22  DS    0H                                                               
         EDIT  (B1,0(R2)),(3,(R4)),ZERO=NOBLANK                                 
         LA    R4,4(R4)                                                         
         LA    R2,1(R2)                                                         
         LA    R6,1(R6)                                                         
*                                                                               
         CLM   R6,1,=AL1(25)       NEED TO PRINT LINE?                          
         BL    PTBYD24             NO                                           
*                                                                               
         BAS   RE,PRINT                                                         
         SR    R6,R6                                                            
         LA    R4,P+20                                                          
*                                                                               
PTBYD24  DS    0H                                                               
         BCT   R3,PTBYD22                                                       
*                                                                               
PTBYDX   DS    0H                                                               
         MVI   P2,0                SKIP A LINE                                  
         BAS   RE,PRINT                                                         
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE RECORDS IN TSAR AND BUILD LINKS                                       
***********************************************************************         
CMPRECS  NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,XCTLST                                                        
         GOTO1 TSAR,TSARDH         ANY RECORDS?                                 
         BNL   CREC000             YES                                          
*                                                                               
         MVC   P(30),=CL30'**** NO BUYS IN ORDER ****'                          
         BAS   RE,PRINT                                                         
         B     CMPRX                                                            
CREC000  DS    0H                                                               
         BAS   RE,XCTLST           DRIVE OFF OFF LINKING DATA TO MAKE           
         MVI   T2.TLKTYP,C'A'       LIFE EASIER                                 
         GOTO1 TSAR,TSARDH                                                      
         BL    CMPRX               AGENCY SUCKS                                 
         CLI   T2.TLKTYP,C'A'                                                   
         BNE   CMPRX               THEY DELETED EVERYTHING                      
*                                                                               
CREC012  DS    0H                                                               
         CLI   T2.TLKTYP,C'A'                                                   
         BNE   CMPRX               ALL DONE!!                                   
*                                                                               
         MVC   BUYNUM,T2.TLLKMBUY           SETUP NEXT REORD READ               
*                                                                               
         TM    T2.TLLFLAGS,TLLFTKOQ         TKO DELETED?                        
         BO    CREC500                      YES - NEXT RECORD                   
*&&ZC                                                                           
         TM    T2.TLLFLAGS,TLLFMGAQ         MAKEGOOD APPLIED?                   
         BO    *+12                         YES - SKIP ZERO SPOT CHECK          
         TM    T2.TLLFLAGS,TLLF0SPQ         ZERO SPOTS?                         
         BO    CREC500                      YES - NEXT RECORD                   
*&&                                                                             
         GOTOX (2,MVCTLST)                                                      
         XC    WORK2,WORK2         BUILD LIST OF POSSIBLE REP MATCHES           
         NI    MISCFLG2,FF-(MF2HLINK+MF2BLOCK+MF2SOFTM)                         
*..........................*                                                    
* 1ST CHECK FOR HARD LINKS                                                      
*''''''''''''''''''''''''''*                                                    
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'X'                                                   
         MVC   T2.TLHKBUY,BUYNUM                                                
         GOTO1 TSAR,TSARDH                                                      
         BNE   CREC014             NONE TO BE HAD                               
*                                                                               
         ZICM  RE,T2.TLLEN,2                                                    
         SH    RE,=Y((TLHLINKS-TLREC)+1)                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),T2.TLHLINKS                                             
         OI    MISCFLG2,MF2HLINK                                                
         B     CREC200             HARDLINKS HANDLED DIFFERENTLY                
*.....................*                                                         
* CHECK FOR IN BLOCK                                                            
*'''''''''''''''''''''*                                                         
CREC014  DS    0H                                                               
         MVI   BYTE,X'01'                                                       
CREC014A DS    0H                                                               
         BAS   RE,XCTLST           READ FOR BUY IN MATCHED BLOCK                
         MVC   T2.TLKTYP,BYTE                                                   
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         GOTO1 TSAR,TSARDH                                                      
         BL    CREC015             EOF                                          
*                                                                               
         CLC   BYTE,T2.TLKTYP                                                   
         BNE   CREC015                                                          
         CLC   T2.TLKACMB,TS2.TLLCMAST                                          
         BNE   CREC015                                                          
*                                                                               
         BAS   RE,XCTLST           VERIFY BUY IN BLOCK                          
         MVC   T2.TLKTYP,BYTE                                                   
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVC   T2.TLKAMBY,TS2.TLLMAST                                           
         MVC   T2.TLKABUY,BUYNUM                                                
*                                                                               
         GOTO1 TSAR,TSARDH                                                      
         BE    CREC016             FOUND BUY                                    
*                                                                               
CREC014Q DS    0H                                                               
         BAS   RE,XCTLST           MUST HAVE BEEN ORPHANNED                     
         MVI   T2.TLKTYP,X'11'     READ FOR UNMATCHED GRANDPARENT               
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         GOTO1 TSAR,TSARDH                                                      
         BL    CREC016             WHO KNOWS WHERE IT WENT (PRAY)               
*                                                                               
         CLI   T2.TLKTYP,X'11'                                                  
         BNE   CREC016                                                          
         CLC   T2.TLKACMB,TS2.TLLCMAST                                          
         BNE   CREC016                                                          
*                                                                               
         GOTO1 TSAR,TSADEL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   T2.TLKTYP,BYTE      MOVE TO CORRECT BLOCK                        
         GOTO1 TSAR,TSAADD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     CREC014Q            EOF                                          
*                                                                               
CREC015  DS    0H                                                               
         CLI   BYTE,X'02'          ALREADY CHEKING SOFT BLOCK?                  
         BE    CREC020             NO BLOCK TO FIND                             
*                                                                               
         MVI   BYTE,X'02'          CHECK FOR SOFT BLOCK                         
         B     CREC014A                                                         
*                                                                               
CREC016  DS    0H                                                               
         OI    MISCFLG2,MF2BLOCK                                                
         CLI   BYTE,X'02'          SOFT BLOCK?                                  
         BNE   *+8                                                              
         OI    MISCFLG2,MF2SOFTM   YES                                          
*                                                                               
         BAS   RE,XCTLST           BUILD LIST OF REP BUYS                       
         MVC   T2.TLKTYP,BYTE                                                   
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKSORT,X'01'                                                 
         GOTO1 TSAR,TSARDH                                                      
         BL    CREC022                                                          
*                                                                               
         LA    R2,WORK2                                                         
CREC017  DS    0H                                                               
         CLC   T2.TLKTYP,BYTE                                                   
         BNE   CREC018                                                          
         CLC   T2.TLKACMB,TS2.TLLCMAST                                          
         BNE   CREC018                                                          
         CLI   T2.TLKSORT,X'01'                                                 
         BNE   CREC018                                                          
*                                                                               
         CLI   T2.TLAGYB,0         MATCHED?                                     
         BNE   *+14                YES                                          
         MVC   0(1,R2),T2.TLKRBUY                                               
         LA    R2,1(R2)                                                         
*                                                                               
         GOTO1 TSAR,TSANXT                                                      
         BE    CREC017                                                          
*                                                                               
CREC018  DS    0H                                                               
         B     CREC022                                                          
*................................*                                              
* FALL BACK TO UNMATCHED REP BUYS                                               
*''''''''''''''''''''''''''''''''*                                              
CREC020  DS    0H                                                               
         SR    R0,R0               CLEAR LIST AREA(S)                           
         SR    R1,R1               ALSTAREA2 WILL HAVE TEMP. LIST               
         L     RE,ALSTAREA          OF MATCHED REP BUYS                         
         L     RF,=A(LISTARLQ)                                                  
         MVCL  RE,R0                                                            
*                                                                               
         BAS   RE,XCTLST           FIRST BUILD LIST OF UNMATCHED REP            
         MVI   T2.TLKTYP,X'12'      IN LISTAREA.                                
         GOTO1 TSAR,TSARDH                                                      
         BL    CREC010             EASY ENOUGH NO RECORDS                       
*                                                                               
         L     R2,ALSTAREA                                                      
CREC002  DS    0H                                                               
         CLI   T2.TLKTYP,X'12'     UNMATCHED REP?                               
         BNE   CREC010             NO ALL DONE                                  
*                                                                               
         MVC   0(1,R2),T2.TLKRBUY                                               
         LA    R2,1(R2)                                                         
         GOTO1 TSAR,TSANXT                                                      
         BE    CREC002                                                          
*                                                                               
CREC010  DS    0H                                                               
         L     RE,ALSTAREA                                                      
         MVC   WORK2,0(RE)                                                      
*....................*                                                          
* DO "SOFT" MATCHING                                                            
*''''''''''''''''''''*                                                          
CREC022  DS    0H                                                               
         OC    WORK2,WORK2                                                      
         BZ    CREC100             NO MATCH POSSIBLE ADD COMMENT                
*                                                                               
         BAS   RE,XCTLST           GET AGENCY BUY DETAILS                       
         MVI   T2.TLKTYP,C'Z'                                                   
         MVI   T2.TLBKTYP,C'A'                                                  
         MVC   T2.TLBKBUY,BUYNUM                                                
         MVC   TK1.TLKEY,T2.TLKEY                                               
         GOTO1 TSAR,TSARDH                                                      
         CLC   T2.TLKEY(TLBKSEQ-TLKEY),TK1.TLKEY                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOX (1,MVCTLST)                                                      
*                                                                               
         NI    MISCFLG1,FF-MF1MULTI      CHECK FOR MULTI LINE                   
         GOTO1 TSAR,TSANXT                                                      
         BNE   *+18                                                             
         CLC   T2.TLKEY(TLBKSEQ-TLKEY),TK1.TLKEY                                
         BNE   *+8                                                              
         OI    MISCFLG1,MF1MULTI                                                
*                                                                               
         GOTOX (11,MVCTLST)                                                     
*                                                                               
CREC030  DS    0H                  LOOP THROUGH AGENCY DETAILS                  
         GOTOX (1,MVCTLST)                                                      
*                                                                               
         OC    WORK2,WORK2                                                      
         BZ    CREC100             NO MATCH POSSIBLE ADD COMMENT                
*                                                                               
         LA    R2,WORK2            POINT TO LIST OF REP BUYS                    
         MVI   BYTE3,0             MATCHED REP BUY NUMBER HERE                  
         XC    ELEM,ELEM                                                        
CREC040  DS    0H                                                               
         BAS   RE,XCTLST           GET REP BUY DETAILS                          
         MVI   T2.TLKTYP,C'Z'                                                   
         MVI   T2.TLBKTYP,C'R'                                                  
         MVC   T2.TLBKBUY,0(R2)                                                 
         MVC   TK2.TLKEY,T2.TLKEY                                               
         GOTO1 TSAR,TSARDH                                                      
         CLC   T2.TLKEY(TLBKSEQ-TLKEY),TK2.TLKEY                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(CHKDTLS),RR=Y                                                 
         BL    CREC042             NO MATCH - NEXT DETAIL                       
         BNE   *+20                                                             
         MVC   BYTE3,0(R2)         EXACT MATCH(CC EQUAL)                        
         MVC   MYWORK(L'TLBPROG),T2.TLBPROG                                     
         B     CREC054              - BREAK OUT OF REP LOOP                     
*                                                                               
         CLI   BYTE3,0             MATCH WITH WEEKS CHANGED(CC HIGH)            
         BNE   CREC042             ONLY KEEP FIRST MATCH                        
*                                                                               
         MVC   BYTE3,0(R2)                                                      
         LA    RE,ELEM                                                          
         USING MATCHD,RE                                                        
         MVC   MTCWKSD,WEEKSCHG                                                 
         MVC   MTCSPSD,SPOTSCHG                                                 
         MVC   MTCCSTD,COSTCHG                                                  
         MVC   MTCFLGS,T2.TLBFLG1                                               
         DROP  RE                                                               
*                                                                               
         MVC   MYWORK(L'TLBPROG),T2.TLBPROG                                     
*                                                                               
CREC042  DS    0H                                                               
         LA    R2,1(R2)                                                         
         CLI   0(R2),0             ANOTHER REP BUY?                             
         BNE   CREC040                                                          
         EJECT                                                                  
*---------------------------------------------------------------------*         
         CLI   BYTE3,0             MATCH FOR AGENCY DETAIL?                     
         BE    CREC100             NO - BREAK AGY DETAIL LOOP                   
*                                                                               
         LA    RE,ELEM             RESTORE CHANGES                              
         USING MATCHD,RE                                                        
         MVC   WEEKSCHG,MTCWKSD                                                 
         MVC   SPOTSCHG,MTCSPSD                                                 
         MVC   COSTCHG,MTCCSTD                                                  
         MVC   FLGSAV,MTCFLGS                                                   
         DROP  RE                                                               
*                                                                               
         LA    R2,WORK2            MATCH WITH DATE CHANGE                       
CREC052  DS    0H                                                               
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                HAD TO BE IN LIST                            
         CLC   BYTE3,0(R2)         FOUND REP ENTRY IN LIST?                     
         BE    *+12                YES                                          
         LA    R2,1(R2)                                                         
         B     CREC052                                                          
*                                                                               
CREC054  DS    0H                  <- EXACT MATCH ENTERS HERE                   
         LA    RE,1(R2)            REMOVE ENTRY FROM LIST                       
         LA    R0,WORK2                                                         
         SR    RE,R0                                                            
         LNR   RE,RE               -(LENGTH TO ENTRY) +                         
         AH    RE,=Y(L'WORK2)       TOTAL LENGTH = LENGTH OF MOVE               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),1(R2)                                                    
         MVI   WORK2+(L'WORK2-1),0                                              
*                                                                               
         L     RE,ALSTARE2         ADD ENTRY TO MATCHED LIST                    
         USING MATCHD,RE                                                        
CREC056  DS    0H                                                               
         CLI   MTCRBUY,0           END OF LIST?                                 
         BE    *+12                YES                                          
         LA    RE,MTCLQ(RE)                                                     
         B     CREC056                                                          
*                                                                               
         MVC   MTCRBUY,BYTE3                                                    
         MVC   MTCWKSD,WEEKSCHG                                                 
         MVC   MTCSPSD,SPOTSCHG                                                 
         MVC   MTCCSTD,COSTCHG                                                  
         MVC   MTCFLGS,FLGSAV                                                   
         DROP  RE                                                               
         EJECT                                                                  
******************************                                                  
* READ NEXT AGENCY DETAIL                                                       
******************************                                                  
         TM    MISCFLG1,MF1MULTI   MULTI DETAIL BUY?                            
         BZ    CREC060             NO - NOTHING TO READ                         
*                                                                               
         BAS   RE,XCTLST                                                        
         MVC   T2.TLKEY,TS1.TLKEY  REREAD LAST DETAIL                           
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
*                                                                               
         GOTO1 TSAR,TSANXT         ANOTHER DETAIL?                              
         BNE   *+14                                                             
         CLC   T2.TLKEY(TLBKSEQ-TLKEY),TS1.TLKEY                                
         BNE   CREC060                                                          
         B     CREC030             YES - PROCESS                                
*                                                                               
******************************************                                      
* DETAILS FOR BUY COMPLETE - ADD COMMENTS                                       
******************************************                                      
CREC060  DS    0H                                                               
         L     R2,ALSTARE2                                                      
         USING MATCHD,R2                                                        
         CLI   MTCRBUY,0                                                        
         BNE   *+6                                                              
         DC    H'0'                HAD TO BE ONE                                
*                                                                               
CREC062  DS    0H                                                               
         CLI   MTCRBUY,0           END OF LIST?                                 
         BE    CREC082             YES                                          
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'R'      READ REP LINK INFO                           
         MVC   T2.TLLKMBUY,MTCRBUY                                              
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BYTE,T2.TLLCMAST                                                 
         MVC   BYTE2,T2.TLLMAST                                                 
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,X'11'                                                  
         TM    MISCFLG2,MF2BLOCK                                                
         BZ    *+8                                                              
         MVI   T2.TLKTYP,X'01'                                                  
         TM    MISCFLG2,MF2SOFTM                                                
         BZ    *+8                                                              
         MVI   T2.TLKTYP,X'02'                                                  
*                                                                               
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKSORT,X'FF'                                                 
         MVC   T2.TLKAMBY,TS2.TLLMAST                                           
         MVC   T2.TLKABUY,BUYNUM      AGENCY BUY                                
         MVC   T2.TLKRCMB,BYTE                                                  
         MVC   T2.TLKRMBY,BYTE2                                                 
         MVC   T2.TLKRBUY,MTCRBUY                                               
         LA    RE,T2.TLCCOM                                                     
         L     RF,=A(AGYAGREP)                                                  
         A     RF,RELO                                                          
         MVC   0(L'AGYAGREP,RE),0(RF)                                           
         EDIT  BUYNUM,(3,11(RE)),ZERO=NOBLANK                                   
         EDIT  BYTE3,(3,31(RE)),ZERO=NOBLANK                                    
         LA    R6,L'AGYAGREP(RE)                                                
*                                                                               
         TM    MISCFLG2,MF2DTCHG   DATE CHANGED?                                
         BO    CREC064             YES                                          
*&&PN                                                                           
         CLC   TS1.TLBPROG,MYWORK  PROGRAM NAME CHANGE?                         
         BE    CREC063             NO                                           
*                                                                               
         MVC   0(19,R6),=C'PROGRAM NAME CHANGE'                                 
         LA    R6,19(R6)                                                        
         B     CREC066                                                          
*&&                                                                             
*-------                           MARK RECORD DELTED AND SKIP COMMENT          
*                                                                               
CREC063  DS    0H                                                               
         MVC   0(10,R6),=C'NO CHANGES'                                          
         LA    R6,10(R6)                                                        
*                                                                               
         OI    T2.TLCFLG1,TLCF1DL  DELETED                                      
*                                                                               
         LA    RE,T2.TLREC                                                      
         SR    R6,RE                                                            
         STCM  R6,3,T2.TLLEN                                                    
*&&DB                                                                           
         GOTO1 TSAR,TSAADD                        *DEBUG **************         
         BE    *+6                                *DEBUG **************         
         DC    H'0'                               *DEBUG **************         
*&&                                                                             
         BAS   RE,XCTLST           MARK REP RECORD DELETED                      
         MVI   T2.TLKTYP,X'12'                                                  
*                                                                               
         TM    MISCFLG2,MF2BLOCK   IN A BLOCK?                                  
         BZ    *+14                NO                                           
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKTYP,X'01'                                                  
*                                                                               
         TM    MISCFLG2,MF2SOFTM   IN A SOFT MATCHED BLOCK?                     
         BZ    *+8                 NO                                           
         MVI   T2.TLKTYP,X'02'                                                  
*                                                                               
         MVI   T2.TLKSORT,X'01'                                                 
         MVC   T2.TLKRCMB,BYTE                                                  
         MVC   T2.TLKRMBY,BYTE2                                                 
         MVC   T2.TLKRBUY,MTCRBUY                                               
*                                                                               
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    T2.TLFLG1,TLF1DLQ                                                
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,XCTLST           MARK AGENCY RECORD DELETED                   
         MVI   T2.TLKTYP,X'11'                                                  
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
*                                                                               
         TM    MISCFLG2,MF2BLOCK   IN A BLOCK?                                  
         BZ    *+8                 NO                                           
         MVI   T2.TLKTYP,X'01'                                                  
*                                                                               
         TM    MISCFLG2,MF2SOFTM   IN A SOFT MATCHED BLOCK?                     
         BZ    *+8                 NO                                           
         MVI   T2.TLKTYP,X'02'                                                  
*                                                                               
         MVI   T2.TLKSORT,0                                                     
         MVC   T2.TLKAMBY,TS2.TLLMAST                                           
         MVC   T2.TLKABUY,BUYNUM                                                
*                                                                               
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    T2.TLFLG1,TLF1DLQ                                                
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     CREC067                                                          
*                                                                               
CREC064  DS    0H                                                               
         MVC   0(14,R6),=C'DATES CHANGED('                                      
         LA    R6,14(R6)                                                        
*                                                                               
         TM    MTCFLGS,X'40'       CREDIT APPLIED?                              
         BZ    *+18                NO                                           
         BCTR  R6,0                                                             
         MVI   0(R6),C' '                                                       
         LA    R6,1(R6)                                                         
         B     CREC066                                                          
*                                                                               
         MVI   0(R6),C'+'                                                       
         ICM   R0,2,MTCWKSD                                                     
         BNM   *+8                                                              
         MVI   0(R6),C'-'                                                       
         LA    R6,1(R6)                                                         
         EDIT  MTCWKSD,(5,0(R6)),ZERO=NOBLANK,ALIGN=LEFT                        
         AR    R6,R0                                                            
         MVC   1(6,R6),=C'WEEKS,'                                               
         LA    R6,8(R6)                                                         
*                                                                               
         MVI   0(R6),C'+'                                                       
         ICM   R0,2,MTCSPSD                                                     
         BNM   *+8                                                              
         MVI   0(R6),C'-'                                                       
         LA    R6,1(R6)                                                         
         EDIT  MTCSPSD,(5,0(R6)),ZERO=NOBLANK,ALIGN=LEFT                        
         AR    R6,R0                                                            
         MVC   1(6,R6),=C'SPOTS,'                                               
         LA    R6,8(R6)                                                         
*                                                                               
         MVI   0(R6),C'+'                                                       
         CP    MTCCSTD,=P'0'                                                    
         BNL   *+8                                                              
         MVI   0(R6),C'-'                                                       
         LA    R6,1(R6)                                                         
         EDIT  MTCCSTD,(13,0(R6)),2,ZERO=NOBLANK,FLOAT=$,ALIGN=LEFT             
         AR    R6,R0                                                            
         MVI   1(R6),C')'                                                       
         LA    R6,2(R6)                                                         
*&&PN                                                                           
         CLC   TS1.TLBPROG,MYWORK  PROGRAM NAME CHANGE?                         
         BE    CREC066             NO                                           
*                                                                               
         MVI   0(R6),C','                                                       
         MVC   2(19,R6),=C'PROGRAM NAME CHANGE'                                 
         LA    R6,21(R6)                                                        
*&&                                                                             
CREC066  DS    0H                                                               
         LA    RE,T2.TLREC                                                      
         SR    R6,RE                                                            
         STCM  R6,3,T2.TLLEN                                                    
*                                                                               
         GOTO1 TSAR,TSAADD                                                      
         BE    *+6                                                              
         DC    H'0'                BUFFER FULL                                  
*                                                                               
CREC067  DS    0H                                                               
         TM    MISCFLG2,MF2BLOCK   PREVIOUSLY UNMATCHED?                        
         BNZ   CREC072             NO - NOTHING TO MOVE                         
*                                                                               
         BAS   RE,XCTLST           LOOP AND MOVE REP BLOCK                      
         MVI   T2.TLKTYP,X'12'                                                  
         MVI   T2.TLKSORT,X'01'                                                 
         MVC   T2.TLKRCMB,BYTE                                                  
         GOTO1 TSAR,TSARDH                                                      
         BNL   *+6                                                              
         DC    H'0'                HAD TO BE ONE                                
         CLI   T2.TLKTYP,X'12'                                                  
         BE    *+6                                                              
         DC    H'0'                HAD TO BE ONE                                
         CLI   T2.TLKSORT,X'01'                                                 
         BE    *+6                                                              
         DC    H'0'                HAD TO BE ONE                                
         CLC   T2.TLKRCMB,BYTE                                                  
         BE    *+6                                                              
         DC    H'0'                HAD TO BE ONE                                
*                                                                               
CREC068  DS    0H                                                               
         CLI   T2.TLKTYP,X'12'                                                  
         BNE   CREC070                                                          
         CLC   T2.TLKRCMB,BYTE                                                  
         BNE   CREC070                                                          
*                                                                               
         GOTO1 TSAR,TSADEL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   T2.TLKTYP,X'02'                                                  
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         GOTO1 TSAR,TSAADD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,XCTLST           GET NEXT RECORD                              
         MVI   T2.TLKTYP,X'12'                                                  
         MVI   T2.TLKSORT,X'01'                                                 
         MVC   T2.TLKRCMB,BYTE                                                  
         GOTO1 TSAR,TSARDH                                                      
         BNL   CREC068                                                          
*                                                                               
CREC070  DS    0H                  MOVE WORK2 TO DEFAULT REP LIST               
         BAS   RE,XCTLST           LOOP AND MOVE REP BLOCK                      
         B     CREC074                                                          
*                                                                               
CREC072  DS    0H                  MARK REP BUY AS LINKED                       
         BAS   RE,XCTLST           LOOP AND MOVE REP BLOCK                      
         MVI   T2.TLKTYP,X'01'                                                  
         TM    MISCFLG2,MF2SOFTM                                                
         BZ    *+8                                                              
CREC074  MVI   T2.TLKTYP,X'02'                                                  
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKSORT,X'01'                                                 
         MVC   T2.TLKRCMB,BYTE                                                  
         MVC   T2.TLKRMBY,BYTE2                                                 
         MVC   T2.TLKRBUY,MTCRBUY                                               
*                                                                               
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                HAD TO BE THERE                              
*                                                                               
         MVC   T2.TLAGYB,BUYNUM                                                 
         OI    T2.TLFLG1,TLF1SMQ                                                
*                                                                               
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,MTCLQ(R2)                                                     
         B     CREC062                                                          
*                                                                               
CREC082  DS    0H                                                               
         TM    MISCFLG2,MF2BLOCK   PREVIOUSLY UNMATCHED?                        
         BNZ   CREC086             NO - NOTHING TO MOVE                         
*                                                                               
         BAS   RE,XCTLST           LOOP AND MOVE AGENCY BLOCK                   
         MVI   T2.TLKTYP,X'11'                                                  
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         GOTO1 TSAR,TSARDH                                                      
         BNL   *+6                                                              
         DC    H'0'                HAD TO BE ONE                                
         CLI   T2.TLKTYP,X'11'                                                  
         BE    *+6                                                              
         DC    H'0'                HAD TO BE ONE                                
         CLC   T2.TLKACMB,TS2.TLLCMAST                                          
         BE    *+6                                                              
         DC    H'0'                HAD TO BE ONE                                
*                                                                               
CREC084  DS    0H                                                               
         CLI   T2.TLKTYP,X'11'                                                  
         BNE   CREC086                                                          
         CLC   T2.TLKACMB,TS2.TLLCMAST                                          
         BNE   CREC086                                                          
*                                                                               
         GOTO1 TSAR,TSADEL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   T2.TLKTYP,X'02'                                                  
         GOTO1 TSAR,TSAADD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,XCTLST           GET NEXT RECORD                              
         MVI   T2.TLKTYP,X'11'                                                  
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         GOTO1 TSAR,TSARDH                                                      
         BNL   CREC084                                                          
*                                                                               
CREC086  DS    0H                                                               
         B     CREC120                                                          
         DROP  R2                                                               
         EJECT                                                                  
******************************                                                  
* NO MATCH FOR AGENCY RECORD                                                    
******************************                                                  
CREC100  DS    0H                                                               
*&&XX                                                                           
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,X'11'                                                  
         TM    MISCFLG2,MF2BLOCK                                                
         BZ    *+8                                                              
         MVI   T2.TLKTYP,X'01'                                                  
         TM    MISCFLG2,MF2SOFTM                                                
         BZ    *+8                                                              
         MVI   T2.TLKTYP,X'02'                                                  
*                                                                               
         CLC   TS2.TLLCMAST,BUYNUM  AGENCY MAKEGOOD?                            
         BNE   CREC102              YES                                         
*                                                                               
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKSORT,X'FF'                                                 
         MVC   T2.TLKAMBY,TS2.TLLMAST                                           
         MVC   T2.TLKABUY,BUYNUM                                                
         MVI   T2.TLKRCMB,0                                                     
         MVI   T2.TLKRMBY,0                                                     
         MVI   T2.TLKRBUY,0                                                     
         LA    RE,T2.TLCCOM                                                     
         L     RF,=A(AGYNOREP)                                                  
         A     RF,RELO                                                          
         MVC   0(L'AGYNOREP,RE),0(RF)                                           
         EDIT  BUYNUM,(3,30(RE)),ZERO=NOBLANK                                   
         MVC   T2.TLLEN,=Y(TLCOMLQ+L'AGYNOREP)                                  
         B     CREC104                                                          
*                                                                               
CREC102  DS    0H                                                               
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKSORT,X'FF'                                                 
         MVC   T2.TLKAMBY,TS2.TLLMAST                                           
         MVC   T2.TLKABUY,BUYNUM                                                
         MVI   T2.TLKRCMB,0                                                     
         MVI   T2.TLKRMBY,0                                                     
         MVI   T2.TLKRBUY,0                                                     
         LA    RE,T2.TLCCOM                                                     
         L     RF,=A(AMGNOREP)                                                  
         A     RF,RELO                                                          
         MVC   0(L'AMGNOREP,RE),0(RF)                                           
         EDIT  BUYNUM,(3,35(RE)),ZERO=NOBLANK                                   
         MVC   T2.TLLEN,=Y(TLCOMLQ+L'AMGNOREP)                                  
*                                                                               
CREC104  DS    0H                                                               
         GOTO1 TSAR,TSAADD                                                      
         BE    *+6                                                              
         DC    H'0'                BUFFER FULL                                  
*&&                                                                             
         BAS   RE,XCTLST           MARK AGENCY BUY UNMATCHED                    
         MVI   T2.TLKTYP,X'11'                                                  
         TM    MISCFLG2,MF2BLOCK                                                
         BZ    *+8                                                              
         MVI   T2.TLKTYP,X'01'                                                  
         TM    MISCFLG2,MF2SOFTM                                                
         BZ    *+8                                                              
         MVI   T2.TLKTYP,X'02'                                                  
*                                                                               
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKSORT,0                                                     
         MVC   T2.TLKAMBY,TS2.TLLMAST                                           
         MVC   T2.TLKABUY,BUYNUM                                                
         MVI   T2.TLKRCMB,0                                                     
         MVI   T2.TLKRMBY,0                                                     
         MVI   T2.TLKRBUY,0                                                     
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                HAD TO BE FOUND                              
*                                                                               
         OI    T2.TLFLG1,TLF1NMQ                                                
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     CREC120                                                          
         EJECT                                                                  
*----------------------------*                                                  
CREC120  DS    0H                                                               
         L     RE,ALSTARE2         CLEAR MATCHED LIST                           
         LA    RF,LST1ARLQ                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         B     CREC500             NEXT AGENCY BUY                              
         EJECT                                                                  
********************                                                            
* HARD LINKED BUYS *                                                            
********************                                                            
CREC200  DS    0H                                                               
         GOTO1 =A(CHLINK),RR=Y                                                  
*                                                                               
*----------------------------*                                                  
CREC500  DS    0H                                                               
         BAS   RE,XCTLST           READ NEXT AGY BUY                            
         MVI   T2.TLKTYP,C'A'                                                   
         ZIC   RE,BUYNUM                                                        
         LA    RE,1(RE)                                                         
         STC   RE,T2.TLLKMBUY                                                   
         GOTO1 TSAR,TSARDH                                                      
         BNL   CREC012                                                          
         SPACE 3                                                                
*----------------------------*                                                  
CMPRX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DO THE COMPARE FOR HARD LINKED BUYS                                           
**********************************************************************          
CHLINK   NTR1  BASE=*,LABEL=*                                                   
         NI    MISCFLG2,FF-MF2AGCHG                                             
         OC    WORK2,WORK2                                                      
         BNZ   *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
*                                                                               
         BAS   RE,XCTLST           GET AGENCY BUY DETAILS                       
         MVI   T2.TLKTYP,C'Z'                                                   
         MVI   T2.TLBKTYP,C'A'                                                  
         MVC   T2.TLBKBUY,BUYNUM                                                
         MVC   TK1.TLKEY,T2.TLKEY                                               
         GOTO1 TSAR,TSARDH                                                      
         CLC   T2.TLKEY(TLBKSEQ-TLKEY),TK1.TLKEY                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTOX (1,MVCTLST)                                                      
*                                                                               
         NI    MISCFLG1,FF-MF1MULTI      CHECK FOR MULTI LINE                   
         GOTO1 TSAR,TSANXT                                                      
         BNE   *+18                                                             
         CLC   T2.TLKEY(TLBKSEQ-TLKEY),TK1.TLKEY                                
         BNE   *+8                                                              
         OI    MISCFLG1,MF1MULTI                                                
*                                                                               
         GOTOX (11,MVCTLST)                                                     
*                                                                               
CREC210  DS    0H                  LOOP THROUGH AGENCY DETAILS                  
         GOTOX (1,MVCTLST)                                                      
*                                                                               
         OC    WORK2,WORK2                                                      
         BZ    CREC230             NO MATCH POSSIBLE                            
*                                                                               
         LA    R2,WORK2            POINT TO LIST OF REP BUYS                    
         XC    ELEM,ELEM           COMMENT TEXT WILL GO HERE                    
         MVI   BYTE3,0             MATCHED REP BUY #                            
CREC220  DS    0H                                                               
         BAS   RE,XCTLST           GET REP BUY DETAILS                          
         MVI   T2.TLKTYP,C'Z'                                                   
         MVI   T2.TLBKTYP,C'R'                                                  
         MVC   T2.TLBKBUY,0(R2)                                                 
         MVC   TK2.TLKEY,T2.TLKEY                                               
         GOTO1 TSAR,TSARDH                                                      
         CLC   T2.TLKEY(TLBKSEQ-TLKEY),TK2.TLKEY                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(CHKDTLS),RR=Y                                                 
         BL    CREC224             NO MATCH - NEXT DETAIL                       
         BNE   CREC222                                                          
*                                                                               
         XC    ELEM,ELEM           EXACT MATCH(CC EQUAL)                        
         MVC   BYTE3,0(R2)                                                      
         MVC   MYWORK(L'TLBPROG),T2.TLBPROG                                     
         B     CREC244              - BREAK OUT OF REP LOOP                     
*                                                                               
CREC222  DS    0H                                                               
         OI    MISCFLG2,MF2AGCHG   SET CHANGES                                  
         CLI   BYTE3,0             MATCH WITH WEEKS CHANGED(CC HIGH)            
         BNE   CREC224             ONLY KEEP FIRST MATCH                        
*                                                                               
         MVC   BYTE3,0(R2)                                                      
         LA    RE,ELEM                                                          
         USING MATCHD,RE                                                        
         MVC   MTCWKSD,WEEKSCHG                                                 
         MVC   MTCSPSD,SPOTSCHG                                                 
         MVC   MTCCSTD,COSTCHG                                                  
         MVC   MTCFLGS,T2.TLBFLG1                                               
         DROP  RE                                                               
*                                                                               
         MVC   MYWORK(L'TLBPROG),T2.TLBPROG                                     
*                                                                               
CREC224  DS    0H                                                               
         LA    R2,1(R2)                                                         
         CLI   0(R2),0             ANOTHER REP BUY?                             
         BNE   CREC220                                                          
         EJECT                                                                  
*--------------------------------------------------------------------*          
         CLI   BYTE3,0             MATCH FOR AGENCY DETAIL?                     
         BNE   CREC240             YES                                          
*                                                                               
CREC230  DS    0H                                                               
         L     RE,ALSTARE2         NO - ADD TO UNMATCHED AGENCY LIST            
CREC232  DS    0H                                                               
         CLI   0(RE),0             END OF LIST?                                 
         BE    *+12                YES                                          
         LA    RE,L'TLKEY(RE)                                                   
         B     CREC232                                                          
*                                                                               
         MVC   0(L'TLKEY,RE),TS1.TLKEY                                          
         B     CREC250             NEXT DETAIL                                  
*---------------------------------------------------------------------*         
CREC240  DS    0H                                                               
         LA    R2,WORK2            MATCH WITH DATE CHANGE                       
CREC242  DS    0H                                                               
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                HAD TO BE IN LIST                            
         CLC   BYTE3,0(R2)         FOUND REP ENTRY IN LIST?                     
         BE    *+12                YES                                          
         LA    R2,1(R2)                                                         
         B     CREC242                                                          
*                                                                               
CREC244  DS    0H                  <- EXACT MATCH ENTERS HERE                   
         LA    RE,1(R2)            REMOVE ENTRY FROM LIST                       
         LA    R0,WORK2                                                         
         SR    RE,R0                                                            
         LNR   RE,RE               -(LENGTH TO ENTRY) +                         
         AH    RE,=Y(L'WORK2)       TOTAL LENGTH = LENGTH OF MOVE               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),1(R2)                                                    
         MVI   WORK2+(L'WORK2-1),0                                              
*                                  ADD COMMENT                                  
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'R'      READ REP LINK INFO                           
         MVC   T2.TLLKMBUY,BYTE3                                                
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*TESTING                                                                        
*        CLI   BYTE3,35                                                         
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*TESTING                                                                        
*                                                                               
         MVC   BYTE,T2.TLLCMAST                                                 
         MVC   BYTE2,T2.TLLMAST                                                 
         MVC   ELCODE,T2.TLLMABUY                                               
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,X'01'                                                  
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKSORT,X'FF'                                                 
         MVC   T2.TLKAMBY,TS2.TLLMAST                                           
         MVC   T2.TLKABUY,BUYNUM      AGENCY BUY                                
         MVC   T2.TLKRCMB,BYTE                                                  
         MVC   T2.TLKRMBY,BYTE2                                                 
         MVC   T2.TLKRBUY,BYTE3                                                 
         LA    RE,T2.TLCCOM                                                     
         L     RF,=A(AGYAGREP)                                                  
         A     RF,RELO                                                          
         MVC   0(L'AGYAGREP,RE),0(RF)                                           
         EDIT  BUYNUM,(3,11(RE)),ZERO=NOBLANK                                   
         EDIT  BYTE3,(3,31(RE)),ZERO=NOBLANK                                    
         LA    R6,L'AGYAGREP(RE)                                                
*                                                                               
         OC    ELEM,ELEM           MOD COMMENT?                                 
         BNZ   CREC246             YES                                          
*&&PN                                                                           
         CLC   TS1.TLBPROG,MYWORK  PROGRAM NAME CHANGE?                         
         BE    CREC245             NO                                           
*                                                                               
         OI    MISCFLG2,MF2AGCHG   SET CHANGES                                  
         MVC   0(19,R6),=C'PROGRAM NAME CHANGE'                                 
         LA    R6,19(R6)                                                        
         B     CREC248                                                          
*&&                                                                             
*-------                           MARK RECORD DELTED AND SKIP COMMENT          
*                                                                               
CREC245  DS    0H                                                               
         MVC   0(10,R6),=C'NO CHANGES'                                          
         LA    R6,10(R6)                                                        
*                                                                               
         OI    T2.TLCFLG1,TLCF1DL  DELETED                                      
*                                                                               
         LA    RE,T2.TLREC                                                      
         SR    R6,RE                                                            
         STCM  R6,3,T2.TLLEN                                                    
*&&DB                                                                           
         GOTO1 TSAR,TSAADD                        *DEBUG **************         
         BE    *+6                                *DEBUG **************         
         DC    H'0'                               *DEBUG **************         
*&&                                                                             
         BAS   RE,XCTLST           MARK REP RECORD DELETED                      
         MVI   T2.TLKTYP,X'01'                                                  
         MVC   T2.TLKACMB,ELCODE                                                
         MVI   T2.TLKSORT,X'01'                                                 
         MVC   T2.TLKRCMB,BYTE                                                  
         MVC   T2.TLKRMBY,BYTE2                                                 
         MVC   T2.TLKRBUY,BYTE3                                                 
*                                                                               
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    T2.TLFLG1,TLF1DLQ                                                
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     CREC249                                                          
*                                                                               
CREC246  DS    0H                                                               
         LA    R2,ELEM                                                          
         USING MATCHD,R2                                                        
         MVC   0(14,R6),=C'DATES CHANGED('                                      
         LA    R6,14(R6)                                                        
*                                                                               
         TM    MTCFLGS,X'40'       CREDIT APPLIED?                              
         BZ    *+18                NO                                           
         BCTR  R6,0                                                             
         MVI   0(R6),C' '                                                       
         LA    R6,1(R6)                                                         
         B     CREC248                                                          
*                                                                               
         MVI   0(R6),C'+'                                                       
         ICM   R0,2,MTCWKSD                                                     
         BNM   *+8                                                              
         MVI   0(R6),C'-'                                                       
         LA    R6,1(R6)                                                         
         EDIT  MTCWKSD,(5,0(R6)),ZERO=NOBLANK,ALIGN=LEFT                        
         AR    R6,R0                                                            
         MVC   1(6,R6),=C'WEEKS,'                                               
         LA    R6,8(R6)                                                         
*                                                                               
         MVI   0(R6),C'+'                                                       
         ICM   R0,2,MTCSPSD                                                     
         BNM   *+8                                                              
         MVI   0(R6),C'-'                                                       
         LA    R6,1(R6)                                                         
         EDIT  MTCSPSD,(5,0(R6)),ZERO=NOBLANK,ALIGN=LEFT                        
         AR    R6,R0                                                            
         MVC   1(6,R6),=C'SPOTS,'                                               
         LA    R6,8(R6)                                                         
*                                                                               
         MVI   0(R6),C'+'                                                       
         CP    MTCCSTD,=P'0'                                                    
         BNL   *+8                                                              
         MVI   0(R6),C'-'                                                       
         LA    R6,1(R6)                                                         
         EDIT  MTCCSTD,(13,0(R6)),2,ZERO=NOBLANK,FLOAT=$,ALIGN=LEFT             
         AR    R6,R0                                                            
         MVI   1(R6),C')'                                                       
         LA    R6,2(R6)                                                         
         DROP  R2                                                               
*&&PN                                                                           
         CLC   TS1.TLBPROG,MYWORK  PROGRAM NAME CHANGE?                         
         BE    CREC248             NO                                           
*                                                                               
         MVI   0(R6),C','                                                       
         MVC   2(19,R6),=C'PROGRAM NAME CHANGE'                                 
         LA    R6,21(R6)                                                        
*&&                                                                             
CREC248  DS    0H                                                               
         LA    RE,T2.TLREC                                                      
         SR    R6,RE                                                            
         STCM  R6,3,T2.TLLEN                                                    
*                                                                               
         GOTO1 TSAR,TSAADD                                                      
         BE    *+6                                                              
         DC    H'0'                BUFFER FULL                                  
*                                                                               
CREC249  DS    0H                                                               
         EJECT                                                                  
******************************                                                  
* READ NEXT AGENCY DETAIL                                                       
******************************                                                  
CREC250  DS    0H                                                               
         TM    MISCFLG1,MF1MULTI   MULTI DETAIL BUY?                            
         BZ    CREC300             NO - NOTHING TO READ                         
*                                                                               
         BAS   RE,XCTLST                                                        
         MVC   T2.TLKEY,TS1.TLKEY  REREAD LAST DETAIL                           
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
*                                                                               
         GOTO1 TSAR,TSANXT         ANOTHER DETAIL?                              
         BNE   CREC300                                                          
*                                                                               
         CLC   T2.TLKEY(TLBKSEQ-TLKEY),TS1.TLKEY                                
         BE    CREC210             YES - PROCESS                                
         EJECT                                                                  
******************************                                                  
* DEAL WITH UNMATCHED RECORDS                                                   
******************************                                                  
CREC300  DS    0H                                                               
         LA    R2,WORK2            POINT TO UNMATCHED REP DETAILS               
         L     R3,ALSTARE2         POINT TO UNMATCHED AGENCY DETAILS            
CREC302  DS    0H                                                               
         OC    0(L'TLKEY,R3),0(R3) MORE AGENCY BUYS?                            
         BZ    CREC310             NO - COMMENT REMAINING REP DETAILS           
         CLI   0(R2),0             MORE REP BUYS?                               
         BE    CREC320             NO - COMMENT REMAINING AGENCY DTLS           
*                                                                               
         OI    MISCFLG2,MF2AGCHG   SET CHANGES                                  
*                                                                               
         BAS   RE,XCTLST           READ AGENCY DETAIL                           
         MVC   T2.TLKEY,0(R3)                                                   
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                HAD TO BE THERE                              
*                                                                               
         GOTOX (1,MVCTLST)                                                      
         BAS   RE,XCTLST           READ REP DETAIL                              
         MVI   T2.TLKTYP,C'Z'                                                   
         MVI   T2.TLBKTYP,C'R'                                                  
         MVC   T2.TLBKBUY,0(R2)                                                 
         MVC   TK2.TLKEY,T2.TLKEY                                               
         GOTO1 TSAR,TSARDH                                                      
         CLC   T2.TLKEY(TLBKSEQ-TLKEY),TK2.TLKEY                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(GETMODC),RR=Y    BUILDS COMMENT IN ELEM                       
*                                                                               
         MVC   MYWORK(L'TLBPROG),T2.TLBPROG                                     
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'R'      READ REP LINK INFO                           
         MVC   T2.TLLKMBUY,0(R2)                                                
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BYTE,T2.TLLCMAST                                                 
         MVC   BYTE2,T2.TLLMAST                                                 
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,X'01'                                                  
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKSORT,X'FF'                                                 
         MVC   T2.TLKAMBY,TS2.TLLMAST                                           
         MVC   T2.TLKABUY,BUYNUM      AGENCY BUY                                
         MVC   T2.TLKRCMB,BYTE                                                  
         MVC   T2.TLKRMBY,BYTE2                                                 
         MVC   T2.TLKRBUY,0(R2)                                                 
         LA    RE,T2.TLCCOM                                                     
         L     RF,=A(AGYAGREP)                                                  
         A     RF,RELO                                                          
         MVC   0(L'AGYAGREP,RE),0(RF)                                           
         EDIT  BUYNUM,(3,11(RE)),ZERO=NOBLANK                                   
         EDIT  (B1,0(R2)),(3,31(RE)),ZERO=NOBLANK                               
         LA    R6,L'AGYAGREP(RE)                                                
*                                                                               
         ZIC   RE,ELEM             BYTE 1 CONTAINS EX LENGTH                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),ELEM+1                                                   
         LA    R6,1(RE,R6)                                                      
*&&PN                                                                           
         CLC   TS1.TLBPROG,MYWORK  PROGRAM NAME CHANGE?                         
         BE    CREC304             NO                                           
*                                                                               
         MVI   0(R6),C','                                                       
         MVC   2(19,R6),=C'PROGRAM NAME CHANGE'                                 
         LA    R6,21(R6)                                                        
*&&                                                                             
CREC304  DS    0H                                                               
         LA    RE,T2.TLREC                                                      
         SR    R6,RE                                                            
         STCM  R6,3,T2.TLLEN                                                    
*                                                                               
         GOTO1 TSAR,TSAADD                                                      
         BE    *+6                                                              
         DC    H'0'                BUFFER FULL                                  
*                                                                               
         LA    R2,1(R2)                                                         
         LA    R3,L'TLKEY(R3)                                                   
         B     CREC302                                                          
         EJECT                                                                  
************************                                                        
* LEFT OVER REP DETAILS                                                         
************************                                                        
CREC310  DS    0H                                                               
         CLI   0(R2),0             MORE REP DETAILS?                            
         BE    CREC330             NO                                           
*                                                                               
         OI    MISCFLG2,MF2AGCHG   SET CHANGES                                  
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'R'      READ REP LINK INFO                           
         MVC   T2.TLLKMBUY,0(R2)                                                
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BYTE,T2.TLLCMAST                                                 
         MVC   BYTE2,T2.TLLMAST                                                 
         MVC   ELCODE,T2.TLLMABUY                                               
*                                                                               
         BAS   RE,XCTLST           READ REP RECORD                              
         MVI   T2.TLKTYP,X'01'                                                  
         MVC   T2.TLKACMB,ELCODE                                                
         MVI   T2.TLKSORT,X'01'                                                 
         MVC   T2.TLKRCMB,BYTE                                                  
         MVC   T2.TLKRMBY,BYTE2                                                 
         MVC   T2.TLKRBUY,0(R2)                                                 
*                                                                               
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                HAD TO BE THERE                              
*                                                                               
         MVI   T2.TLAGYB,0         SET TO UNMATCHED REP                         
*                                                                               
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*&&XX                                                                           
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,C'R'      READ REP LINK INFO                           
         MVC   T2.TLLKMBUY,0(R2)                                                
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BYTE,T2.TLLCMAST                                                 
         MVC   BYTE2,T2.TLLMAST                                                 
*                                                                               
         BAS   RE,XCTLST                                                        
         MVI   T2.TLKTYP,X'01'                                                  
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKSORT,X'FF'                                                 
         MVC   T2.TLKAMBY,TS2.TLLMAST                                           
         MVC   T2.TLKABUY,BUYNUM      AGENCY BUY                                
         MVC   T2.TLKRCMB,BYTE                                                  
         MVC   T2.TLKRMBY,BYTE2                                                 
         MVC   T2.TLKRBUY,0(R2)                                                 
         LA    RE,T2.TLCCOM                                                     
         L     RF,=A(REPNOAGY)                                                  
         A     RF,RELO                                                          
         MVC   0(L'REPNOAGY,RE),0(RF)                                           
         EDIT  (B1,0(R2)),(3,30(RE)),ZERO=NOBLANK                               
         LA    R6,L'REPNOAGY(RE)                                                
*                                                                               
         LA    RE,T2.TLREC                                                      
         SR    R6,RE                                                            
         STCM  R6,3,T2.TLLEN                                                    
*                                                                               
         GOTO1 TSAR,TSAADD                                                      
         BE    *+6                                                              
         DC    H'0'                BUFFER FULL                                  
*&&                                                                             
         LA    R2,1(R2)                                                         
         B     CREC310                                                          
         EJECT                                                                  
************************                                                        
* LEFT OVER AGY DETAILS                                                         
************************                                                        
CREC320  DS    0H                                                               
         OC    0(L'TLKEY,R3),0(R3) MORE AGY DETAILS?                            
         BZ    CREC330             NO                                           
*                                                                               
         OI    MISCFLG2,MF2AGCHG   SET CHANGES                                  
*                                                                               
         MVI   T2.TLKTYP,X'01'                                                  
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKSORT,X'FF'                                                 
         MVC   T2.TLKAMBY,TS2.TLLMAST                                           
         MVC   T2.TLKABUY,BUYNUM                                                
         MVI   T2.TLKRCMB,0                                                     
         MVI   T2.TLKRMBY,0                                                     
         MVI   T2.TLKRBUY,0                                                     
         LA    RE,T2.TLCCOM                                                     
         L     RF,=A(AGYADREP)                                                  
         A     RF,RELO                                                          
         MVC   0(AGYADRLQ,RE),0(RF)                                             
         EDIT  BUYNUM,(3,11(RE)),ZERO=NOBLANK                                   
         MVC   T2.TLLEN,=Y(TLCOMLQ+AGYADRLQ)                                    
*                                                                               
         GOTO1 TSAR,TSAADD                                                      
         BE    *+6                                                              
         DC    H'0'                BUFFER FULL                                  
*                                                                               
         EJECT                                                                  
*----------------------------*                                                  
CREC330  DS    0H                                                               
         TM    MISCFLG2,MF2AGCHG   CHANGES?                                     
         BNZ   CREC340             YES DON'T DELETE AGENCY RECORD               
*                                                                               
         BAS   RE,XCTLST           MARK AGENCY RECORD DELETED                   
         MVI   T2.TLKTYP,X'01'                                                  
         MVC   T2.TLKACMB,TS2.TLLCMAST                                          
         MVI   T2.TLKSORT,0                                                     
         MVC   T2.TLKAMBY,TS2.TLLMAST                                           
         MVC   T2.TLKABUY,BUYNUM                                                
*                                                                               
         GOTO1 TSAR,TSARDH                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    T2.TLFLG1,TLF1DLQ                                                
         GOTO1 TSAR,TSAPUT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CREC340  DS    0H                                                               
         L     RE,ALSTARE2         CLEAR UNMATCHED LIST                         
         LA    RF,LST1ARLQ                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* COMPARE REP AND AGENCY DETAILS                                                
*        T2 = REP DETAIL                                                        
*       TS1 = AGENCY DETAIL                                                     
**********************************************************************          
CHKDTLS  NTR1  BASE=*,LABEL=*                                                   
         XC    WEEKSCHG,WEEKSCHG   WILL CONTAIN DIFFERENCE VALUES               
         XC    SPOTSCHG,SPOTSCHG                                                
         ZAP   COSTCHG,=P'0'                                                    
*                                                                               
         LA    R4,TS1.TLBDATA      R4 IS AGY RECORD                             
AGY      USING RBUYELEM,R4                                                      
         LA    R6,T2.TLBDATA       R6 IS REP RECORD                             
REP      USING RBUYELEM,R6                                                      
*                                                                               
* COMPARE RATE                                                                  
         CLC   AGY.RBUYCOS,REP.RBUYCOS                                          
         BNE   CDTL050                                                          
*                                                                               
         ICM   RE,15,AGY.RBUYCOS                                                
         CVD   RE,DUB                                                           
         ZAP   PCKOF08B,DUB        FOR CHANGE CALC.                             
*                                                                               
* COMPARE LENGTH                                                                
         CLC   AGY.RBUYDUR,REP.RBUYDUR                                          
         BNE   CDTL050                                                          
*                                                                               
         MVI   ELCODE,X'02'                                                     
REP      USING RBUYDYEL,R6                                                      
         BAS   RE,NEXTEL6                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
AGY      USING RBUYDYEL,R4                                                      
         BAS   RE,NEXTEL4                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* COMPARE DAY                                                                   
CDTL030  DS    0H                                                               
         CLC   AGY.RBUYDYIN(2),REP.RBUYDYIN                                     
         BNE   CDTL050             RECORDS DON'T MATCH                          
*                                                                               
* COMPARE TIME                                                                  
         CLC   AGY.RBUYDYT1(4),REP.RBUYDYT1                                     
         BNE   CDTL050             RECORDS DON'T MATCH                          
*                                                                               
         BAS   RE,NEXTEL6          INCASE OF MULTIPLE DAY/TIME ORBITS           
         BNE   *+16                                                             
         BAS   RE,NEXTEL4                                                       
         BE    CDTL030                                                          
         B     CDTL050             RECORDS DON'T MATCH                          
*                                                                               
         BAS   RE,NEXTEL4                                                       
         BE    CDTL050             RECORDS DON'T MATCH                          
*                                                                               
         MVI   ELCODE,X'03'                                                     
         LA    R4,TS1.TLBDATA      R4 IS AGY RECORD                             
AGY      USING RBUYDTEL,R4                                                      
         BAS   RE,FRSTEL4                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,T2.TLBDATA       R6 IS REP RECORD                             
REP      USING RBUYDTEL,R6                                                      
         BAS   RE,FRSTEL6                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* COMPARE NUMBER OF SPOTS                                                       
         NI    MISCFLG2,FF-MF2DTCHG                                             
CDTL032  DS    0H                                                               
         CLI   REP.RBUYDTNW,0      SKIP FOR PARTIAL CANCEL                      
         BE    CDTL036                                                          
         CLC   AGY.RBUYDTNW,REP.RBUYDTNW                                        
         BNE   CDTL050             RECORDS DON'T MATCH                          
*                                                                               
* COMPARE START AND END DATES                                                   
         CLC   AGY.RBUYDTST(6),REP.RBUYDTST                                     
         BE    CDTL034                                                          
*                                                                               
         OI    MISCFLG2,MF2DTCHG                                                
         ZIC   RE,AGY.RBUYDTWK     NUMBER OF WEEKS IN AGY BUY                   
         XC    HALF,HALF                                                        
         MVC   HALF+1,REP.RBUYDTWK REP WEEKS                                    
         SH    RE,HALF             DIFFERENCE(COULD BE NEGATIVE)                
         LR    R0,RE                                                            
         AH    R0,WEEKSCHG                                                      
         STH   R0,WEEKSCHG                                                      
*                                                                               
         XC    HALF,HALF           SPOTS/WEEK                                   
         MVC   HALF+1,AGY.RBUYDTNW                                              
         MH    RE,HALF             TOTAL SPOT DIFFERENCE                        
         LR    R0,RE                                                            
         AH    R0,SPOTSCHG                                                      
         STH   R0,SPOTSCHG                                                      
*                                                                               
         CVD   RE,DUB                                                           
         ZAP   PCKOF16B,PCKOF08B   <- COST SAVED IN PCKOF08 EARLIER             
         MP    PCKOF16B,DUB        TOTAL COST DIFFERENCE                        
         AP    COSTCHG,PCKOF16B                                                 
*                                                                               
CDTL034  DS    0H                                                               
         BAS   RE,NEXTEL4          INCASE OF ORBITS                             
         BNE   CDTL038                                                          
*                                                                               
CDTL036  DS    0H                  PARTIAL CANCEL - REP                         
         BAS   RE,NEXTEL6                                                       
         BE    CDTL032                                                          
*                                                                               
CDTL037  DS    0H                                                               
         CLI   AGY.RBUYDTNW,0      AGENCY CANCEL?                               
         BNE   CDTL050             NO - RECORDS DON'T MATCH                     
*                                                                               
         BAS   RE,NEXTEL4                                                       
         BE    CDTL037                                                          
         B     CDTL040                                                          
*                                                                               
CDTL038  DS    0H                                                               
         BAS   RE,NEXTEL6                                                       
         BE    CDTL050             RECORDS DON'T MATCH                          
*                                                                               
CDTL040  DS    0H                                                               
         TM    MISCFLG2,MF2DTCHG                                                
         BZ    EXITOK              EXACT MATCH                                  
         B     EXITH               DATE CHANGED                                 
*                                                                               
CDTL050  DS    0H                                                               
         B     EXITL                                                            
         DROP  AGY,REP                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* COMPARE REP AND AGENCY DETAILS TO CREATE CHANGES COMMENT                      
*        T2 = REP DETAIL                                                        
*       TS1 = AGENCY DETAIL                                                     
*                                                                               
* OUTPUT IN ELEM:  BYTE 0   = COMMENT EX LENGTH                                 
*                  BYTE 1-N = COMMENT                                           
**********************************************************************          
GETMODC  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM+1                                                        
*                                                                               
         LA    R4,TS1.TLBDATA      R4 IS AGY RECORD                             
AGY      USING RBUYELEM,R4                                                      
         LA    R6,T2.TLBDATA       R6 IS REP RECORD                             
REP      USING RBUYELEM,R6                                                      
*                                                                               
* COMPARE RATE                                                                  
         CLC   AGY.RBUYCOS,REP.RBUYCOS                                          
         BE    GMOD002                                                          
*                                                                               
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
*                                                                               
         ICM   R0,15,AGY.RBUYCOS                                                
         ICM   RE,15,REP.RBUYCOS                                                
         SR    R0,RE                                                            
         MVC   0(12,R2),=C'RATE CHANGE('                                        
         LA    R2,12(R2)                                                        
         MVI   0(R2),C'+'                                                       
         LTR   R0,R0                                                            
         BNM   *+8                                                              
         MVI   0(R2),C'-'                                                       
         LA    R2,1(R2)                                                         
         EDIT  (R0),(13,0(R2)),2,ZERO=NOBLANK,FLOAT=$,ALIGN=LEFT                
         AR    R2,R0                                                            
         MVC   0(2,R2),=C'),'                                                   
         LA    R2,1(R2)                                                         
*                                                                               
* COMPARE LENGTH                                                                
GMOD002  DS    0H                                                               
         CLC   AGY.RBUYDUR,REP.RBUYDUR                                          
         BE    GMOD004                                                          
*                                                                               
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(14,R2),=C'LENGTH CHANGE,'                                      
         LA    R2,13(R2)                                                        
*                                                                               
GMOD004  DS    0H                                                               
         MVI   ELCODE,X'02'                                                     
REP      USING RBUYDYEL,R6                                                      
         BAS   RE,NEXTEL6                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
AGY      USING RBUYDYEL,R4                                                      
         BAS   RE,NEXTEL4                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* COMPARE DAY                                                                   
GMOD020  DS    0H                                                               
         CLC   AGY.RBUYDYIN(2),REP.RBUYDYIN                                     
         BNE   GMOD024             RECORDS DON'T MATCH                          
*                                                                               
* COMPARE TIME                                                                  
         CLC   AGY.RBUYDYT1(4),REP.RBUYDYT1                                     
         BNE   GMOD024             RECORDS DON'T MATCH                          
*                                                                               
         BAS   RE,NEXTEL6          INCASE OF MULTIPLE DAY/TIME ORBITS           
         BNE   GMOD022                                                          
         BAS   RE,NEXTEL4                                                       
         BE    GMOD020                                                          
         BNE   GMOD024             RECORDS DON'T MATCH                          
*                                                                               
GMOD022  DS    0H                                                               
         BAS   RE,NEXTEL4                                                       
         BNE   GMOD026             RECORDS MATCH                                
*                                                                               
GMOD024  DS    0H                                                               
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(16,R2),=C'DAY/TIME CHANGE,'                                    
         LA    R2,15(R2)                                                        
*                                                                               
GMOD026  DS    0H                                                               
         MVI   ELCODE,X'03'                                                     
         LA    R4,TS1.TLBDATA      R4 IS AGY RECORD                             
AGY      USING RBUYDTEL,R4                                                      
         BAS   RE,FRSTEL4                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,T2.TLBDATA       R6 IS REP RECORD                             
REP      USING RBUYDTEL,R6                                                      
         BAS   RE,FRSTEL6                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* COMPARE NUMBER OF SPOTS                                                       
GMOD030  DS    0H                                                               
         CLI   REP.RBUYDTNW,0      SKIP FOR PARTIAL CANCEL                      
         BE    GMOD034                                                          
         CLC   AGY.RBUYDTNW,REP.RBUYDTNW                                        
         BNE   GMOD038             RECORDS DON'T MATCH                          
*                                                                               
GMOD032  DS    0H                                                               
         BAS   RE,NEXTEL4          INCASE OF ORBITS                             
         BNE   GMOD036                                                          
*                                                                               
GMOD034  DS    0H                                                               
         BAS   RE,NEXTEL6                                                       
         BE    GMOD030                                                          
*                                                                               
GMOD035  DS    0H                                                               
         CLI   AGY.RBUYDTNW,0      AGENCY CANCEL?                               
         BNE   GMOD038             NO - RECORDS DON'T MATCH                     
*                                                                               
         BAS   RE,NEXTEL4                                                       
         BE    GMOD035                                                          
         B     GMOD040                                                          
*                                                                               
GMOD036  DS    0H                                                               
         BAS   RE,NEXTEL6                                                       
         BNE   GMOD040             RECORDS MATCH                                
*                                                                               
GMOD038  DS    0H                                                               
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(16,R2),=C'SPOTS/WK CHANGE,'                                    
         LA    R2,15(R2)                                                        
*                                                                               
GMOD040  DS    0H                                                               
         MVI   ELCODE,X'03'                                                     
         LA    R4,TS1.TLBDATA      R4 IS AGY RECORD                             
AGY      USING RBUYDTEL,R4                                                      
         BAS   RE,FRSTEL4                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,T2.TLBDATA       R6 IS REP RECORD                             
REP      USING RBUYDTEL,R6                                                      
         BAS   RE,FRSTEL6                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* COMPARE START AND END DATES                                                   
GMOD042  DS    0H                                                               
         CLI   REP.RBUYDTNW,0      SKIP FOR PARTIAL CANCEL                      
         BE    GMOD044                                                          
         CLC   AGY.RBUYDTST(6),REP.RBUYDTST                                     
         BNE   GMOD048                                                          
*                                                                               
         BAS   RE,NEXTEL4          INCASE OF ORBITS                             
         BNE   GMOD046                                                          
*                                                                               
GMOD044  DS    0H                                                               
         BAS   RE,NEXTEL6                                                       
         BE    GMOD042                                                          
*                                                                               
GMOD045  DS    0H                                                               
         CLI   AGY.RBUYDTNW,0      AGENCY CANCEL?                               
         BNE   GMOD048             NO - RECORDS DON'T MATCH                     
*                                                                               
         BAS   RE,NEXTEL4                                                       
         BE    GMOD045                                                          
         B     GMOD050                                                          
*                                                                               
         B     GMOD048             RECORDS DON'T MATCH                          
*                                                                               
GMOD046  DS    0H                                                               
         BAS   RE,NEXTEL6                                                       
         BNE   GMOD050             RECORDS MATCH                                
*                                                                               
GMOD048  DS    0H                                                               
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(12,R2),=C'DATE CHANGE,'                                        
         LA    R2,11(R2)                                                        
*                                                                               
GMOD050  DS    0H                                                               
         CLI   0(R2),C','                                                       
         BE    *+10                                                             
         MVI   0(R2),0                                                          
         BCTR  R2,0                                                             
*                                                                               
         BCTR  R2,0                                                             
         LA    RE,ELEM+1                                                        
         SR    R2,RE                                                            
         STC   R2,ELEM                                                          
*                                                                               
         B     EXIT                                                             
         DROP  AGY,REP                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
REPNOAGY DC    C'NO AGENCY BUY AGAINST REP BUY XXX'                             
AGYNOREP DC    C'NO REP BUY AGAINST AGENCY BUY XXX'                             
AMGNOREP DC    C'NO REP BUY AGAINST AGENCY MAKEGOOD XXX'                        
AGYAGREP DC    C'AGENCY BUY XXX AGAINST REP BUY XXX: '                          
AGYCANCL DC    C'***AGENCY BUY XXX CANCELED'                                    
AGYADREP DC    C'AGENCY BUY XXX REQUIRES ADDITONAL REP'                         
         DC    C' BUYS FOR COMPLETION'                                          
AGYADRLQ EQU   *-AGYADREP                                                       
ORDOK    DC    C'AGENCY ORDER MATCHED AGAINST REP CONTRACT '                    
         DC    C'SUCCESSFULLY'                                                  
ORDOKLQ  EQU   *-ORDOK                                                          
*---------------------------------------------------------------------          
         SPACE 3                                                                
RECTAB   DS    0A                                                               
         DC    A(PTSARREC),X'01'                                                
         DS    0A                                                               
RECTABLQ EQU   *-RECTAB                                                         
         DC    A(PTSARREC),X'02'                                                
         DC    A(PTSARREC),X'11'                                                
         DC    A(PTSARREC),X'12'                                                
         DC    A(PTSARLNK),C'A'                                                 
         DC    A(PTSARLNK),C'R'                                                 
         DC    A(PTSARHRD),C'X'                                                 
         DC    A(PTSARBYD),C'Z'                                                 
         DC    A(00000000),X'00'                                                
         SPACE 3                                                                
***********************************************************************         
* APPLICATION STORAGE AREA                                                      
***********************************************************************         
MYAREAD  DSECT                                                                  
RELO     DS    A                                                                
VTSAR    DS    A                   V(TSAR)                                      
REGENPBY DS    A                                                                
ALSTAREA DS    A                                                                
ALSTARE2 DS    A                                                                
*                                                                               
* ADDRESS BLOCK FOR CALL TO REGENDAB                                            
*                                                                               
         DS    0A                  <- DON'T MOVE THIS                           
PARMS    DS    0XL(6*4)              OR THIS GETS MESSED UP                     
PCOMFACS DS    A                                                                
PUNDAY   DS    A                                                                
PUNTIME  DS    A                                                                
PDEMOCON DS    A                                                                
*                                                                               
* ADDRESS BLOCK FOR CALL TO REGENPBY                                            
*                                                                               
         DS    0A                                                               
PARMS2   DS    0XL(6*4)                                                         
P2UNDAY  DS    A                                                                
P2UNTIME DS    A                                                                
P2DATCON DS    A                                                                
P2ADDAY  DS    A                                                                
*                                                                               
SVCONNUM DS    XL4                 SAVED CONTRACT NUMBER                        
PRBUYKEY DS    CL32                AGENCY BUY KEY                               
BUYNUM   DS    X                                                                
REVNUM   DS    XL(L'RDARRNUM)                                                   
REVDATE  DS    XL(L'RDARDATE)                                                   
REVTIME  DS    XL(L'RDARTIME)                                                   
*                                                                               
TBUFFADR DS    A                                                                
TBUFFLEN DS    H                                                                
*                                                                               
PRTSTAT  DS    X                   STATUS FLAG FOR PRINTING                     
PRTONE   EQU   X'80'               ONLY ONE REPORT WILL BE PRINTED              
PRTNEWPG EQU   X'40'               EJECT PAGE FOR NEXT REPORT                   
PRTCLOSE EQU   X'20'               WE'RE ALL DONE, CLOSE PQ AND EXIT            
PRTDAILY EQU   X'10'               BUY IS DAILY                                 
PRTPAGE1 EQU   X'08'               PAGE 1 IS PRINTED                            
PRTKBUY  EQU   X'01'               CONTRACT BUYS FOUND, PRINT CONTRACT          
*                                   BUYS SIDE-BY-SIDE WITH AGENCY'S             
*                                                                               
CMTFLAG  DS    X                   COMMENT PRINT FLAG                           
CMTSTAND EQU   X'80'               THERE ARE STANDARD COMMENTS                  
CMTORD   EQU   X'40'               THERE ARE ORDER COMMENTS                     
CMTREJ   EQU   X'20'               THERE ARE REJECTION COMMENTS                 
*                                                                               
MISCFLAG DS    X                                                                
MSFTRADE EQU   X'80'               ORDER IS TRADE, SHOW INDICATORS              
*                                                                               
MISCFLG1 DS    X                   MISC. FLAGS 1                                
MF1UNMA  EQU   X'80'                - UNMATCHED AGENCY TAG PRINTED              
MF1UNMR  EQU   X'40'                - UNMATCHED REP TAG PRINTED                 
MF1SFTM  EQU   X'20'                - SOFT MATCH TAG PRINTED                    
MF1DAYL  EQU   X'10'                - DAILY BUY                                 
MF1ORBD  EQU   X'08'                - ORBITS DELETED                            
MF1ORBP  EQU   X'04'                - ORBITS PRESENT                            
MF1DTLP  EQU   X'02'                - DETAILS PRESENT                           
MF1MULTI EQU   X'01'                - LINE HAS MULTIPLE PARTS                   
*                                                                               
MISCFLG2 DS    X                   MISC. FLAGS 2                                
MF2DTCHG EQU   X'80'                - EFFECTIVE DATE CHANGE ON MATCH            
MF2INCMT EQU   X'40'                - INCOMPLETE MATCHING IN PROGRESS           
MF2HLINK EQU   X'20'                - MATCHING AGAINST HARD LINKS               
MF2BLOCK EQU   X'10'                - MATCHING AGAINST BLOCK                    
MF2SOFTM EQU   X'08'                - MATCHING BLOCK IS SOFT TYPE               
MF2INCOM EQU   X'04'                - IN COMMENT BLOCK ALREADY                  
MF2AGCHG EQU   X'02'                - CHANGE AGAINST AGENCY DETAIL              
MF2CHGS  EQU   X'01'                - CHANGES PRINTED ON REPORT                 
*                                                                               
HALF2    DS    H                                                                
BYTE2    DS    X                                                                
BYTE3    DS    X                                                                
LASTMAST DS    2X                                                               
LASTAGY  DS    X                                                                
*                                                                               
TOTRSPTS DS    PL8                                                              
TOTRCOST DS    PL16                                                             
TOTASPTS DS    PL8                                                              
TOTACOST DS    PL16                                                             
TOTXSPTS DS    PL8                                                              
TOTXCOST DS    PL16                                                             
*                                                                               
TOTBSPTS DS    PL8                 TOTALS FOR SINGLE AGENCY LINE                
TOTBCOST DS    PL16                TO CHECK FOR CANCEL                          
*                                                                               
PCKOF16B DS    PL16                                                             
PCKOF08B DS    PL08                                                             
*                                                                               
SPLKEYAD DS    CL133               EXTENDED SPOOLKEY AREA                       
FAKEHDR  DS    CL16                A FAKE HEADER FOR VCON                       
ORDSTAT  DS    CL8                 ORDER STATUS                                 
MYWORK   DS    CL64                                                             
PRTTIME  DS    XL4                 PRINT TIME                                   
*--------------*                                                                
* FROM/FOR REDAR21                                                              
*--------------*                                                                
STARTDAY DS    XL1                 START DAY FOR BUY                            
ENDDAY   DS    XL1                 END DAY FOR BUY                              
ORBSTDAY DS    XL1                 ORBIT START DAY                              
ROTATDAY DS    XL1                 ROTATION START DAY                           
BUYCOST  DS    F                   COST OF SPOTS IN BUY                         
TKODATE  DS    XL3                 TAKEOVER DATE                                
*                                                                               
A1STEL   DS    A                   ADDRESS OF 1ST ELEMENT IN BUFFER             
ANXTEL   DS    A                   ADDRESS OF NEXT ELEMENT POSTION              
ATLBUFF  DS    A                   ADDRESS OF TSARD BLOCK                       
METHOD   DS    X                                                                
GRID     DS    XL53                GRID OF SPOTS/WK ACROSS FLIGHT               
WORK2    DS    XL256                                                            
*                                                                               
WEEKSCHG DS    H                   AGY VS REP CHANGES                           
SPOTSCHG DS    H                                                                
COSTCHG  DS    PL8                                                              
FLGSAV   DS    X                                                                
         SPACE 3                                                                
*****************************                                                   
** STUFF FOR TSAR BUFFER 2 **                                                   
*****************************                                                   
LSTSINDS DS    XL1                 TSAR BUFFER 2 INDICATORS                     
LSTSIINI EQU   X'80'               INITIALISED                                  
LSTSIRES EQU   X'40'               RESTORED                                     
LSTSLOWP DS    XL1                 LOW TSAR PAGE NUMBER                         
LSTSNUMP DS    XL1                 NUMBER OF PAGES ALLOCATED                    
*                                                                               
TLACTN   DS    XL1                 REQUESTED ACTION                             
TLST     DS    XL(TLSTLQ)          TSAR2 RECORD BUFFER                          
         ORG   TLST                                                             
TXNUM    DS    XL2                                                              
TXREC    DS    0X                                                               
TXLEN    DS    XL2                                                              
TXKEY    DS    0X                                                               
         ORG                                                                    
*                                                                               
TLBUFF   DS    XL(TSARDL)          BUFFER FOR SECOND TSAR BUFFER                
*                                                                               
LISTAREA DS    32CL81              FOR AGENCY                                   
LISTARE2 DS    32CL81              FOR CONTRACT                                 
LISTARLQ EQU   *-LISTAREA                                                       
LST1ARLQ EQU   *-LISTARE2          LENGTH OF SINGLE BUFFER                      
*                                                                               
*---------------------------------------------------------------------*         
MYAREAD2 EQU   *                   SECOND PORTION OF WORKING STORAGE            
TLSVKEY  DS    XL(L'TLKEY)         SAVED TSAR KEY                               
TLSVKEY2 DS    XL(L'TLKEY)         2ND SAVED TSAR KEY                           
TLSTSAV  DS    XL(TLSTLQ)          SAVED TSAR RECORD BUFFER                     
TLSTSAV2 DS    XL(TLSTLQ)          2ND SAVED TSAR RECORD BUFFER                 
*                                                                               
WORKLQ   EQU   *-MYAREAD                                                        
         EJECT                                                                  
*----------------------------------------------------------------------         
TLSTD    DSECT                     TSAR RECORD DSECT                            
TLNUM    DS    XL2                 RECORD NUMBER                                
TLREC    DS    0XL2000             *** TSAR RECORD ***                          
TLLEN    DS    XL2                 RECORD LENGTH                                
TLKEY    DS    0XL32               *** TSAR KEY ***                             
TLKTYP   DS    XL1                 TSAR RECORD TYPE                             
*                                    - X'01' MATCHED AGENCY/REP BUYS            
*                                    - X'02' SOFT MATCHED BUYS                  
*                                    - X'11  UNMATCHED AGENCY BUYS              
*                                    - X'12' UNMATCHED REP BUYS                 
*                                    - C'A'  AGY BUY KEYS - FOR LINKING         
*                                    - C'R'  REP BUY KEYS - FOR LINKING         
*                                    - C'X'  REP -> AGENCY HARD LINK            
*                                    - C'Z'  BUY DETAILS                        
*                                                                               
TLKUSR   DS    XL(L'TLKEY-(*-TLKEY)) KEY DATA DEFINED BY TYPE                   
TLDATA   DS    XL(L'TLREC-(*-TLREC)) TSAR RECORD DATA                           
TLRECLQ  EQU   *-TLREC                                                          
TLSTLQ   EQU   *-TLSTD                                                          
         EJECT                                                                  
*---------------------------------------*                                       
* KEY FOR RECORDS TYPES 01, 02, 11 & 12                                         
*---------------------------------------*                                       
         ORG   TLKUSR                                                           
TLKACMB  DS    XL1                 CHAINED AGY MASTER BUYLINE #                 
TLKSORT  DS    XL1                 SORT 00=AGY 01=REP FF=COMMENT                
TLKAMBY  DS    XL1                 AGY MASTER BUYLINE #                         
TLKABUY  DS    XL1                 AGENCY BUY #                                 
TLKRCMB  DS    XL1                 CHAINED REP MASTER BUYLINE #                 
TLKRMBY  DS    XL1                 REP MASTER BUYLINE #                         
TLKRBUY  DS    XL1                 REP BUYLINE #                                
*........................*                                                      
* DATA FOR HEADER RECORD                                                        
*''''''''''''''''''''''''*                                                      
         ORG   TLDATA                                                           
TLDSKAD  DS    XL4                 DISK ADDRESS OF RECORD                       
TLAGYB   DS    XL1                 AGENCY BUY OF REP BUYLINE                    
TLFLG1   DS    XL1                 FLAGS                                        
TLF1MGQ  EQU   X'80'                - BUY IS MAKEGOOD                           
TLF1CRQ  EQU   X'40'                - BUY IS CREDIT                             
TLF1SMQ  EQU   X'20'                - BUY WAS SOFT MATCHED                      
TLF1NMQ  EQU   X'10'                - AGENCY BUY WAS NOT MATCHED                
TLF1MAQ  EQU   X'08'                - BUY HAS MAKEGOOD APPLIED                  
TLF1DLQ  EQU   X'01'                - LINE IS DELETED                           
TLHDRLQ  EQU   *-TLREC                                                          
*                                                                               
*........................*                                                      
* DATA FOR COMMENT RECORD                                                       
*''''''''''''''''''''''''*                                                      
         ORG   TLDATA                                                           
TLCFLG1  DS    XL1                 FLAGS                                        
TLCF1DL  EQU   X'01'                - LINE IS DELETED                           
TLCOMLQ  EQU   *-TLREC                                                          
TLCCOM   DS    0C                                                               
*                                                                               
         EJECT                                                                  
*-------------------------------------*                                         
* KEY FOR C'A' OR C'R' LINKING RECORD                                           
*-------------------------------------*                                         
         ORG   TLKUSR                                                           
TLLKMBUY DS    XL1                 BUY #                                        
*.........................*                                                     
* DATA FOR LINKING RECORD                                                       
*''''''''''''''''''''''''''*                                                    
         ORG   TLDATA                                                           
TLLCMAST DS    XL1                 CHAINED MASTER OF THIS BUY                   
TLLMAST  DS    XL1                 MASTER OF THIS BUY                           
TLLMABUY DS    XL1                 AGENCY BUY # OF THIS BUY(REP ONLY)           
TLLMAGBL DS    XL1                 RBUYAGBL - UNCHAINED AGENCY BUY #            
TLLFLAGS DS    XL1                 LINK FLAGS                                   
TLLFTKOQ EQU   X'80'                - RECORD WAS TKO DELETED                    
TLLFMGAQ EQU   X'40'                - RECORD HAS MAKEGOOD APPLIED               
TLLFIMGQ EQU   X'20'                - RECORD IS A MAKEGOOD                      
TLLF0SPQ EQU   X'10'                - RECORD HAS ZERO SPOTS                     
TLLMBYLQ EQU   *-TLREC                                                          
         EJECT                                                                  
*-----------------------------------*                                           
* KEY FOR C'X' AGY -> REP HARD LINK                                             
*-----------------------------------*                                           
         ORG   TLKUSR                                                           
TLHKBUY  DS    XL1                 HARD LINKED AGENCY BUY #                     
*.........................*                                                     
* DATA FOR LINKING RECORD                                                       
*''''''''''''''''''''''''''*                                                    
         ORG   TLDATA                                                           
TLHLINKS DS    X                   HARD LINKED REP BUYS                         
TLHLNKLQ EQU   *-TLREC             INITIAL LENGTH                               
         EJECT                                                                  
*-----------------------------------*                                           
* KEY FOR C'Z' BUY DETALS RECORD                                                
*-----------------------------------*                                           
         ORG   TLKUSR                                                           
TLBKTYP  DS    XL1                 C'A'=AGY C'R'=REP                            
TLBKBUY  DS    XL1                 BUY #                                        
TLBKSEQ  DS    XL1                 SEQUENCE # FOR SUB RECORDS                   
TLBKSTYP DS    XL1                 SUB RECORD TYPE                              
*                                    - X'00' BUY DETAILS & GRID                 
*                                                                               
*........................*                                                      
* DATA FOR BUY DETAILS                                                          
*''''''''''''''''''''''''*                                                      
         ORG   TLDATA                                                           
TLBGRID  DS    (L'GRID)X           SPOTS PER WEEK ACROSS FLIGHT                 
TLBFLG1  DS    X                   MATCHING FLAGS                               
*               X'80'               -SPOTS ARE NEGATIVE                         
*               X'40'               -REP BUY HAS CREDIT APPLIED                 
TLBPROG  DS    CL34                PROGRAM NAME                                 
TLBDATA  EQU   *                   START OF 'REP' ELEMENTS                      
*                                                                               
*    THE DATA SECTION OF THIS TSAR RECORD WILL CONTAIN THE                      
* SPOT COST AND THE REP BUY X'02' AND X'03' ELEMENTS FOR SOFT MATCHING.         
* IF AN AGENCY BUY WOULD BREAK INTO MULTIPLE REP BUYS THEY ARE BUILT            
* USING MULTIPLE 'RECORD DATA' RECORDS AND A SEQUENCE NUMBER.                   
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
MATCHD   DSECT                                                                  
MTCRBUY  DS    X                   MATCHED REP BUY #                            
MTCWKSD  DS    XL2                 DIFFERENCE IN WEEKS                          
MTCSPSD  DS    XL2                 DIFFERENCE IN SPOTS                          
MTCCSTD  DS    PL8                 DIFFERENCE IN COST                           
MTCFLGS  DS    X                   FROM TLBFLG1                                 
MTCLQ    EQU   *-MATCHD                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
PLINED   DSECT                                                                  
P1TAG    DS    CL6                 LINE TYPE TAG                                
         DS    CL2                                                              
P1XTAG   DS    CL11                EXTRA LINE TAG                               
         ORG   P1XTAG                                                           
         DS    CL3                                                              
P1BUYLN  DS    CL3                 BUYLINE #                                    
         DS    CL2                                                              
         DS    CL2                                                              
P1DAYS   DS    CL12                DAYS                                         
         DS    CL2                                                              
P1TIMES  DS    CL11                TIMES                                        
         DS    CL2                                                              
P1LEN    DS    CL6                 SPOT LENGTH                                  
         DS    CL2                                                              
P1DATES  DS    CL12                EFFECITVE DATES                              
         DS    CL2                                                              
         DS    CL1                                                              
P1NPW    DS    CL3                 NUMBER OF SPOTS PER WEEK                     
         DS    CL2                                                              
P1SPRATE DS    CL10                RATE PER SPOT                                
         DS    CL2                                                              
P1TOTSPT DS    CL5                 TOTAL SPOTS                                  
         DS    CL2                                                              
P1TOTRAT DS    CL17                TOTAL RATE ACROSS BUY FLIGHT                 
         DS    CL2                                                              
P1CLASS  DS    CL3                 CLASS - NUMBER OF WEEKS                      
         DS    CL5                                                              
P1REMARK DS    CL12                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
* BUY LIST LINE                                                                 
*                                                                               
LBUYD    DSECT                                                                  
LBUYMC   DS    CL2                                                              
         DS    CL1                                                              
LBUYLINE DS    CL3                                                              
LBUYTYPE DS    CL1                                                              
LBUYDAYS DS    CL12                                                             
         DS    CL1                                                              
LBUYTIME DS    CL11                                                             
LBUYLENM DS    CL1                                                              
LBUYLEN  DS    CL3                                                              
         DS    CL1                                                              
LBUYDATE DS    CL12                                                             
         DS    CL1                                                              
LBUYNW   DS    CL3                                                              
         DS    CL9                                                              
LBUYNPW  DS    CL3                                                              
         DS    CL1                                                              
LBUYRATE DS    CL10                                                             
LBUYSPT  DS    CL4                                                              
         DS    CL2                                                              
LBUYLENQ EQU   *-LBUYMC                                                         
         ORG   LBUYDAYS+2                                                       
LBUYPROG DS    CL34                                                             
         ORG   LBUYDAYS                                                         
LBUYCMMT DS    CL71                                                             
         EJECT                                                                  
* DDSPOOLD          (GENERAL PRINT AREAS)                                       
* DDSPLWORKD        (GENERAL CONTROLLER AREAS)                                  
* DMPRTQL                                                                       
* DDCOMFACS                                                                     
* FAFACTS                                                                       
* FATIOB                                                                        
* REDARFFD                                                                      
* DDGENTWA                                                                      
* DDDARTWA                                                                      
* REDARWORKD                                                                    
* REDARDSECT                                                                    
* REGENPBYD                                                                     
* DDCOREQUS                                                                     
* DDTSARD                                                                       
* DDPERVALD                                                                     
* RECNTPROF                                                                     
* REGENSTA                                                                      
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REDARFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REDARF0D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE REDARTWA                                                       
       ++INCLUDE REDARWORKD                                                     
       ++INCLUDE REDARDSECT                                                     
       ++INCLUDE REGENPBYD                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080REDAR31   11/24/03'                                      
         END                                                                    
