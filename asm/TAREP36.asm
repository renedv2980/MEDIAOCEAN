*          DATA SET TAREP36    AT LEVEL 021 AS OF 06/24/13                      
*PHASE T70336C,*                                                                
         TITLE 'T70336 - RETRO REPORT'                                          
T70336   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70336                                                         
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
         USING TRR,R7                                                           
         LA    R6,TRSRTREC         A(SORT RECORD)                               
         USING SORTD,R6                                                         
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE SCREEN                              
         BE    VKEY                                                             
*                                                                               
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
*                                                                               
VKEY     DS    0H                                                               
         BAS   RE,INIT                                                          
         LA    R2,SRRAGYH          AGENCY                                       
         TM    4(R2),X'20'                                                      
         BO    VK05                                                             
         NI    SRRCLIH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SRRAGYNH                        
*                                                                               
VK05     OI    4(R2),X'20'                                                      
         LA    R2,SRRCLIH          CLIENT                                       
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SRRPRDH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    SRRCLIN,SRRCLIN                                                  
         OI    SRRCLINH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SRRCLINH                        
*                                                                               
VK10     OI    4(R2),X'20'                                                      
         LA    R2,SRRPRDH          PRODUCT                                      
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         NI    SRRCIDH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    SRRPRDN,SRRPRDN                                                  
         OI    SRRPRDNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'08',(R2)),SRRPRDNH                        
*                                                                               
VK20     OI    4(R2),X'20'                                                      
         LA    R2,SRRCIDH          COMMERCIAL ID                                
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         NI    SRRINVSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         XC    SRRCIDN,SRRCIDN                                                  
         OI    SRRCIDNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),SRRCIDNH                       
*                                                                               
VK30     OI    4(R2),X'20'                                                      
         LA    R2,SRRINVSH         START INVOICE NUMBER                         
         CLI   SRRINVEH+5,0                                                     
         BE    VK40                                                             
         GOTO1 ANY                                                              
*                                                                               
VK40     LA    R3,STRTINV                                                       
         TM    4(R2),X'20'                                                      
         BO    VK45                                                             
         NI    SRRINVEH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,VALINV           VALIDATE INVOICE NUMBER                      
*                                                                               
VK45     OI    4(R2),X'20'                                                      
         LA    R2,SRRINVEH         END INVOICE NUMBER                           
         LA    R3,ENDINV                                                        
         TM    4(R2),X'20'                                                      
         BO    VK47                                                             
         NI    SRRFESTH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,VALINV           VALIDATE INVOICE NUMBER                      
*                                                                               
VK47     OI    4(R2),X'20'                                                      
         OC    ENDINV,ENDINV                                                    
         BNZ   *+10                                                             
         MVC   ENDINV,STRTINV      IF NO END INPUT THEN END=START               
*                                                                               
         LA    R2,SRRFESTH         FROM ESTIMATE                                
         TM    4(R2),X'20'                                                      
         BO    VK50                                                             
         NI    SRRTESTH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         XC    FROMEST,FROMEST                                                  
         CLI   5(R2),0                                                          
         BE    VK50                                                             
         CLI   5(R2),3                                                          
         BNE   *+14                                                             
         CLC   =C'ANY',8(R2)       IF INPUT C'ANY' THEN LEAVE                   
         BE    VK50                FROMEST CLEAR                                
         MVC   FROMEST,8(R2)                                                    
*                                                                               
VK50     OI    4(R2),X'20'                                                      
         LA    R2,SRRTESTH         TO ESTIMATE                                  
         TM    4(R2),X'20'                                                      
         BO    VK60                                                             
         NI    SRROPTH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         CLI   5(R2),0                                                          
         BNE   VK60                                                             
         CLI   SRRFESTH+5,0                                                     
         BNE   MISSERR             IF INPUT FROM - 'TO' IS REQUIRED             
         B     VK70                                                             
*                                                                               
VK60     OI    4(R2),X'20'                                                      
         CLI   SRRFESTH+5,0        INPUT ALLOWED ONLY IF 'FROM' INPUT           
         BE    INVERR                                                           
*                                                                               
VK70     BAS   RE,VALOPT           VALIDATE OPTIONS                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE INVOICE NUMBER                               
         SPACE 1                                                                
*                                  R2=A(FIELD)                                  
*                                  R3=A(W/S FOR COMP. INVOICE NUMBER)           
VALINV   NTR1                                                                   
         XC    0(6,R3),0(R3)                                                    
         CLI   5(R2),0                                                          
         BE    VIX                                                              
         GOTO1 TINVCON,DMCB,8(R2),(R3),DATCON                                   
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         XC    0(6,R3),=6X'FF'                                                  
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'84',(R3))                                 
         BNE   THEEND                                                           
*                                                                               
VIX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 1                                                                
VALOPT   NTR1                                                                   
         LA    R2,SRROPTH          VALIDATE OPTIONS                             
         TM    4(R2),X'20'                                                      
         BO    VOPTX                                                            
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
*                                                                               
VOPT10   DS    0H                                                               
         CLC   =C'TRACE',SCDATA1  TRACE                                         
         BNE   VOPT20                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    TROPTS,TRTRACE      SET TRACE ON                                 
         B     VOPT100                                                          
*                                                                               
VOPT20   DS    0H                                                               
         B     INVERR                                                           
*                                                                               
VOPT100  LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10           AND CONTINUE                                 
*                                                                               
VOPTX    OI    4(R2),X'20'                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        INITIALIZE                                                             
*                                                                               
INIT     NTR1                                                                   
         LH    RF,=AL2(TRLNQ)      CLEAR LOCAL STORAGE                          
         XCEFL TRR                                                              
         LA    R1,ACCUMS                                                        
         LA    R2,NACCUMS                                                       
*                                                                               
IN10     ZAP   0(8,R1),=P'0'                                                    
         LA    R1,L'ACCUMS(R1)                                                  
         BCT   R2,IN10                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SET UP & CALL SYSIO                                      
*                                                                               
PREP     NTR1                                                                   
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    TIFAGY,TIFAGY       CLEAR FILTERS                                
         XC    TIFCLI,TIFCLI                                                    
         XC    TIFPRD,TIFPRD                                                    
         XC    TIFCID,TIFCID                                                    
*                                                                               
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVI   TIREAD,TLINBCDQ     OPEN ITEM POINTERS                           
*                                                                               
         OI    TIQFLAGS,TIQFDIR    WANT DIRECTORY HOOK                          
         OI    TIFINS2Y,TAINSRTH   ONLY GET RETRO PAYMENTS                      
         MVC   TIFAGY,TGAGY        SET OTHER FILTERS                            
         CLI   SRRCLIH+5,0                                                      
         BE    *+10                                                             
         MVC   TIFCLI,TGCLI                                                     
         CLI   SRRPRDH+5,0                                                      
         BE    *+10                                                             
         MVC   TIFPRD,TGPRD                                                     
         CLI   SRRCIDH+5,0                                                      
         BE    *+10                                                             
         MVC   TIFCID,TGCID                                                     
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
*                                                                               
         BAS   RE,REPORT           PRINT OUT REPORT                             
*                                                                               
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         MVI   SPACING,2           PRINT OUT AGENCY TOTALS                      
         BAS   RE,PRNTIT                                                        
         MVC   LINEST,=CL16'AGENCY TOTALS'                                      
         EDIT  (P8,AYINVS),(5,LINUTYP),COMMAS=YES,ZERO=NOBLANK                  
         EDIT  (P8,AYAPPL),(12,LINAPPL),2,MINUS=YES  APPLIED AMOUNT             
         EDIT  (P8,AYPAYI),(12,LINPAYI),2,MINUS=YES  INDIV. PAYMENT             
         EDIT  (P8,AYPAYC),(12,LINPAYC),2,MINUS=YES  CORP. PAYMENT              
         EDIT  (P8,AYSPNH),(12,LINSPNH),2,MINUS=YES  SUBJ. TO PNH               
         EDIT  (P8,AYPNH),(12,LINPNH),2,MINUS=YES    PNH                        
         BAS   RE,PRNTIT                                                        
         BAS   RE,BXBOT                                                         
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
*              PROCESS RECORDS FROM SYSIO                                       
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR                                                   
         BNE   IOH5                                                             
         LA    R4,TIKEY            R4=A(INVOICE KEY)                            
         USING TLINPD,R4                                                        
                                                                                
         MVC   WORK(4),TGTODY20                                                 
         NC    WORK(4),=X'0F0F0F0F'   STRIP OFF X'F0'                           
                                                                                
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         IC    RF,WORK                                                          
         SLL   RF,4                                                             
         IC    RE,WORK+1                                                        
         AR    RF,RE                                                            
         STC   RF,WORK                                                          
         IC    RF,WORK+2                                                        
         SLL   RF,4                                                             
         IC    RE,WORK+3                                                        
         AR    RF,RE                                                            
         STC   RF,WORK+1                                                        
                                                                                
         XC    WORK(2),=X'FFFF'    COMPLEMENT IT                                
         CLC   TLINBINV(2),WORK    RESTRICT TO THIS YEAR'S INVOICES             
         BNE   NO                                                               
                                                                                
         OC    STRTINV,STRTINV     IF START INVOICE INPUT                       
         BZ    *+14                                                             
         CLC   TLINBINV,STRTINV    MUST BE LE (BECAUSE IT'S COMP.)              
         BH    NO                                                               
*                                                                               
         OC    ENDINV,ENDINV       IF END INVOICE INPUT                         
         BZ    *+14                                                             
         CLC   TLINBINV,ENDINV     MUST BE GE (BECAUSE IT'S COMP.)              
         BL    NO                                                               
         B     YES                 SET CC                                       
*                                                                               
IOH5     CLI   TIMODE,PROCREC                                                   
         BNE   IOHX                                                             
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         USING TLIND,R4                                                         
         CLI   SRRFESTH+5,0        SKIP IF THERE'S NO ESTIMATE CHANGE           
         BE    IOH200                                                           
         MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TANUELQ      GET ESTIMATE ELEMENT                         
         MVI   WORK,TANUTEST                                                    
         GOTO1 GETL,DMCB,(1,WORK)                                               
         BNE   IOH120                                                           
         OC    FROMEST,FROMEST     IF FROM 'ANY' THEN GO DELETE IT              
         BZ    IOH110                                                           
         USING TANUD,R4                                                         
         L     R4,TGELEM                                                        
         ZIC   R1,SRRFESTH+5       ELSE SET FOR COMPARE FOR L'INPUT             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FROMEST(0),TANUMBER                                              
         BNE   IOHX                                                             
*                                                                               
IOH110   GOTO1 DELL,DMCB,(1,WORK)  REMOVE EXISTING ELEMENT                      
         B     IOH150                                                           
*                                                                               
IOH120   OC    FROMEST,FROMEST     IF NO ELEMENT FOUND & REQUESTING             
         BNZ   IOHX                   A SPECIFIC INVOICE - EXIT                 
*                                                                               
IOH150   XC    ELEM,ELEM           ADD THE NEW ELEMENT                          
         LA    R4,ELEM                                                          
         USING TANUD,R4                                                         
         MVI   TANUEL,TANUELQ                                                   
         ZIC   R1,SRRTESTH+5         L'NEW ESTIMATE NUMBER                      
         LA    R1,TANULNQ(R1)        +L'BEG. OF ELEMENT                         
         STC   R1,TANULEN            =L'ELEMENT                                 
         MVI   TANUTYPE,TANUTEST     TYPE=ESTIMATE                              
         MVC   TANUMBER(16),SRRTEST  NEW ESTIMATE NUMBER                        
         GOTO1 ADDELEM                                                          
*                                                                               
IOH200   L     R4,TIAREC                                                        
         MVI   ELCODE,TAINELQ      GET INVOICE ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R4                                                         
         NI    TAINSTA2,X'FF'-TAINSRTH   TURN OFF RETRO BIT                     
*                                                                               
         GOTO1 MYTRACE,DMCB,=C'INVOICE RECORD',TIAREC                           
*                                                                               
         BAS   RE,SETSORT          SET SORT RECORD                              
*                                                                               
         CLI   TWAWRITE,C'N'       IF WRITE = NO                                
         BE    IOHX                EXIT                                         
         MVI   TIMODE,PROCPTRC     ELSE ASK SYSIO TO PUTREC                     
*                                                                               
IOHX     MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET SORT RECORD                                                        
*                                                                               
SETSORT  NTR1                                                                   
         XC    TRSRTREC,TRSRTREC                                                
         L     R4,TIAREC                                                        
         USING TLIND,R4                                                         
         MVC   WORK(6),TLININV                                                  
         XC    WORK(6),=6X'FF'                                                  
         GOTO1 TINVCON,DMCB,WORK,SORTINV,DATCON  INVOICE                        
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         USING TAPDD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   SSRT20                                                           
         MVC   SORTCLI,TAPDCLI                                                  
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         MVC   SORTUSE,TGUSCDE     USE CODE                                     
         MVC   SORTUTYP,TGUSTYCD   USE TYPE                                     
*                                                                               
         MVC   SORTAPPL,TAPDAPPL   SAVE AMOUNTS FROM INVOICE                    
         MVC   SORTPAYI,TAPDPAYI                                                
         MVC   SORTPAYC,TAPDPAYC                                                
         MVC   SORTSPNH,TAPDSPNH                                                
         MVC   SORTPNH,TAPDPNH                                                  
*                                                                               
SSRT20   L     R4,TIAREC                                                        
         MVI   ELCODE,TACOELQ                                                   
         USING TACOD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   SSRT30                                                           
         MVC   SORTCID,TACOCID                                                  
*                                                                               
SSRT30   MVC   SORTEST,SPACES                                                   
         MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TANUELQ      GET ESTIMATE ELEMENT                         
         MVI   WORK,TANUTEST                                                    
         GOTO1 GETL,DMCB,(1,WORK)                                               
         BNE   SSRT40                                                           
         USING TANUD,R4                                                         
         L     R4,TGELEM                                                        
         ZIC   R1,TANULEN                                                       
         SH    R1,=AL2(TANULNQ+1)                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SORTEST(0),TANUMBER                                              
*                                                                               
SSRT40   BAS   RE,PUTSORT                                                       
*                                                                               
SSRTX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        GET SORT RECORD & PRINT REPORT                                         
*                                                                               
REPORT   NTR1                                                                   
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         TM    TRSTAT,TRSORTNG     TEST SOMETHING WAS PUT TO SORT               
         BNO   REPX                                                             
*                                                                               
REP10    BAS   RE,GETSORT          GET SORT RECORD                              
         BNE   REP100                                                           
*                                                                               
         CLC   SORTKEY(SORTPLNQ),TRLSTREC  IF NEW PAGE REQUIRED                 
         BE    REP20                                                            
         BAS   RE,CLITOT           PRINT OUT CLIENT TOTALS                      
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         BAS   RE,NEWCLI           GET NEW CLIENT NAME                          
*                                                                               
REP20    MVC   LININV,SORTINV                                                   
         MVC   LINCID,SORTCID                                                   
         MVC   LINEST,SORTEST                                                   
         MVC   LINUSE,SORTUSE                                                   
         MVC   LINUTYP,SORTUTYP                                                 
*                                                                               
         EDIT  (4,SORTAPPL),(12,LINAPPL),2,MINUS=YES  APPLIED AMOUNT            
         EDIT  (4,SORTPAYI),(12,LINPAYI),2,MINUS=YES  INDIV. PAYMENT            
         EDIT  (4,SORTPAYC),(12,LINPAYC),2,MINUS=YES  CORP. PAYMENT             
         EDIT  (4,SORTSPNH),(12,LINSPNH),2,MINUS=YES  SUBJ. TO PNH              
         EDIT  (4,SORTPNH),(12,LINPNH),2,MINUS=YES    PNH                       
*                                                                               
         BAS   RE,PRNTIT           AND PRINT THE LINE NOW                       
         BAS   RE,ADDACC           ADD TO ACCUMULATORS                          
         MVC   TRLSTREC,TRSRTREC   SAVE LAST RECORD                             
         B     REP10                                                            
*                                                                               
REP100   BAS   RE,CLITOT           PRINT OUT CLIENT TOTALS                      
         GOTO1 SORTER,DMCB,=C'END' CLOSE SORTER & SET NOT ACTIVE                
         NI    TRSTAT,X'FF'-TRSORTNG                                            
*                                                                               
REPX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE HANDLES NEW CLIENT                                       
*                                                                               
NEWCLI   NTR1                                                                   
         MVC   TRCLNAME,NOREC                                                   
         MVC   TGCLI,SORTCLI                                                    
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'20',0)                                    
         BNE   XIT                                                              
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   TRCLNAME,TGNAME                                                  
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        PRINT OUT CLIENT TOTALS                                                
*                                                                               
CLITOT   NTR1                                                                   
         OC    TRLSTREC,TRLSTREC   IF THIS IS NOT THE FIRST REC                 
         BZ    CLITOTX                                                          
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         MVC   LINEST,=CL16'CLIENT TOTALS'                                      
         EDIT  (P8,INVS),(5,LINUTYP),COMMAS=YES,ZERO=NOBLANK                    
         EDIT  (P8,APPL),(12,LINAPPL),2,MINUS=YES  APPLIED AMOUNT               
         EDIT  (P8,PAYI),(12,LINPAYI),2,MINUS=YES  INDIV. PAYMENT               
         EDIT  (P8,PAYC),(12,LINPAYC),2,MINUS=YES  CORP. PAYMENT                
         EDIT  (P8,SPNH),(12,LINSPNH),2,MINUS=YES  SUBJ. TO PNH                 
         EDIT  (P8,PNH),(12,LINPNH),2,MINUS=YES    PNH                          
         BAS   RE,PRNTIT                                                        
*                                                                               
         BAS   RE,AGYACC           ADD ACCUMS TO AGENCY ACCUMS                  
         ZAP   INVS,=P'0'          CLEAR CLIENT ACCUMS                          
         ZAP   APPL,=P'0'                                                       
         ZAP   PAYI,=P'0'                                                       
         ZAP   PAYC,=P'0'                                                       
         ZAP   SPNH,=P'0'                                                       
         ZAP   PNH,=P'0'                                                        
*                                                                               
CLITOTX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ADD TO ACCUMULATORS                                              
*                                                                               
ADDACC   NTR1                                                                   
         AP    INVS,=P'1'                                                       
         L     R0,SORTAPPL                                                      
         CVD   R0,DUB                                                           
         AP    APPL,DUB                                                         
         L     R0,SORTPAYI                                                      
         CVD   R0,DUB                                                           
         AP    PAYI,DUB                                                         
         L     R0,SORTPAYC                                                      
         CVD   R0,DUB                                                           
         AP    PAYC,DUB                                                         
         L     R0,SORTSPNH                                                      
         CVD   R0,DUB                                                           
         AP    SPNH,DUB                                                         
         L     R0,SORTPNH                                                       
         CVD   R0,DUB                                                           
         AP    PNH,DUB                                                          
*                                                                               
ADX      B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*              ADD TO AGENCY ACCUMULATORS                                       
*                                                                               
AGYACC   NTR1                                                                   
         AP    AYINVS,INVS                                                      
         AP    AYAPPL,APPL                                                      
         AP    AYPAYI,PAYI                                                      
         AP    AYPAYC,PAYC                                                      
         AP    AYSPNH,SPNH                                                      
         AP    AYPNH,PNH                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              SORTER ROUTINES                                                  
*                                                                               
PUTSORT  NTR1                                                                   
         TM    TRSTAT,TRSORTNG     TEST SORT INITIALIZED ALREADY                
         BO    PUTS10                                                           
         LA    R0,SORTKLNQ         SET SOFT L'SORT KEY                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
*                                                                               
         LA    R0,SORTLNQ          SET SOFT L'SORT RECORD                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB+6(2)                                           
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OI    TRSTAT,TRSORTNG                                                  
*                                                                               
PUTS10   GOTO1 SORTER,DMCB,=C'PUT',TRSRTREC                                     
         B     XIT                                                              
*                                                                               
GETSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   RF,15,4(R1)                                                      
         BZ    NO                                                               
         MVC   TRSRTREC,0(RF)      MOVE TO LOCAL W/S                            
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*        SET BOTTOM BOX                                                         
*                                                                               
BXBOT    NTR1                                                                   
         MVI   SPACING,2                                                        
         L     R4,ABOX             CLOSE BOX                                    
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'B'                                                       
         MVI   BOXINIT,0                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET INFO FOR TRACE ROUTINE                                             
*                                                                               
MYTRACE  NTR1                                                                   
         TM    TROPTS,TRTRACE      TRACE                                        
         BZ    XIT                                                              
         LM    R2,R3,0(R1)         R2=A(LITERAL), R3=A(I/O AREA)                
         ZIC   R4,0(R1)            R4=L'LITERAL                                 
         GOTO1 TRACE,DMCB,(R3),0,(R2),(R4)                                      
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*              ROUTINE TO PRINT A LINE                                          
*                                                                               
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SPACING,1                                                        
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   HEAD3+8(6),TGAGY                                                 
         MVC   HEAD3+15(36),SRRAGYN                                             
         MVC   HEAD4+8(6),SORTCLI                                               
         MVC   HEAD4+15(36),TRCLNAME                                            
*                                                                               
         CLI   SRRFESTH+5,0        IF CHANGING ESTIMATES                        
         BE    HK20                                                             
         MVC   HEAD4+99(13),=C'FROM ESTIMATE'                                   
         MVC   HEAD4+114(16),SRRFEST                                            
         MVC   HEAD5+99(11),=C'TO ESTIMATE'                                     
         MVC   HEAD5+114(16),SRRTEST                                            
*                                                                               
HK20     L     R4,ABOX             SET UP BOXES                                 
         LTR   R4,R4                                                            
         BZ    HKX                                                              
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         USING LINED,R2                                                         
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC7,C'C'                                                         
         MVI   BC8,C'C'                                                         
         MVI   BC9,C'C'                                                         
         MVI   BR,C'R'                                                          
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
*                                                                               
         MVI   BOXYORN,C'Y'        INITIALIZE REMAINING FLDS                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
HKX      B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
INVERR   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
THEEND   GOTO1 ERREX                                                            
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         LTORG                                                                  
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=000'                                   
         SPACE 2                                                                
NOREC    DC    CL36'******* RECORD NOT FOUND *******'                           
CNTTAB   DS    0CL28                                                            
INVOICES DC    PL8'0',CL20'INVOICES'                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
AYCNTTAB DS    0CL28                                                            
AGYINV   DC    PL8'0',CL20'INVOICES'                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SPACE 1                                                                
         SSPEC H1,54,C'RETROACTIVE REPORT'                                      
         SSPEC H2,54,18X'BF'                                                    
         SPACE 1                                                                
         SSPEC H3,2,C'AGENCY'                                                   
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H8,2,C'INVOICE  COMML ID        ESTIMATE       USE'              
         SSPEC H9,2,C'NUMBER'                                                   
         SPACE 1                                                                
         SSPEC H8,50,C'  APPLIED    INDIVIDUAL    CORPORATE'                    
         SSPEC H9,50,C'  AMOUNT       PAYMENT      PAYMENT'                     
         SPACE 1                                                                
         SSPEC H8,90,C'SUBJ TO      PENSION    '                                
         SSPEC H9,90,CL21' P && H       && HEALTH'                              
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
TRR      DSECT                                                                  
TROPTS   DS    XL1                 OPTIONS                                      
TRTRACE  EQU   X'80'               TRACE ACTIVE                                 
*                                                                               
TRSTAT   DS    XL1                 STATUS                                       
TRSORTNG EQU   X'80'               SORT ACTIVE                                  
*                                                                               
FROMEST  DS    CL16                FROM ESTIMATE NUMBER                         
*                                                                               
TRCLNAME DS    CL36                CLIENT NAME                                  
*                                                                               
STRTINV  DS    XL6                 SAVED START INVOICE NUMBER                   
ENDINV   DS    XL6                 SAVED END INVOICE NUMBER                     
*                                                                               
ACCUMS   DS    0PL8                                                             
INVS     DS    PL8                                                              
APPL     DS    PL8                                                              
PAYI     DS    PL8                                                              
PAYC     DS    PL8                                                              
SPNH     DS    PL8                                                              
PNH      DS    PL8                                                              
NCLIACC  EQU   (*-INVS)/8                                                       
*                                                                               
AYINVS   DS    PL8                                                              
AYAPPL   DS    PL8                                                              
AYPAYI   DS    PL8                                                              
AYPAYC   DS    PL8                                                              
AYSPNH   DS    PL8                                                              
AYPNH    DS    PL8                                                              
NAGYACC  EQU   (*-AYINVS)/8                                                     
NACCUMS  EQU   (*-ACCUMS)/8                                                     
*                                                                               
TRSRTREC DS    CL(SORTLNQ)         SORT RECORD                                  
TRLSTREC DS    CL(SORTLNQ)         LAST SORT RECORD                             
*                                                                               
TRLNQ    EQU   *-TRR                                                            
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
LINED    DSECT                                                                  
BL       DS    CL1                                                              
LININV   DS    CL6                 INVOICE                                      
         DS    CL1                                                              
BC1      DS    CL1                                                              
LINCID   DS    CL12                COMMERCIAL ID                                
BC2      DS    CL1                                                              
LINEST   DS    CL16                ESTIMATE NUMBER                              
BC3      DS    CL1                                                              
LINUSE   DS    CL3                 USE CODE                                     
LINUTYP  DS    CL5                 USE TYPE                                     
BC5      DS    CL1                                                              
LINAPPL  DS    CL12                APPLIED AMOUNT                               
BC6      DS    CL1                                                              
LINPAYI  DS    CL12                INDIV PAYMENT                                
BC7      DS    CL1                                                              
LINPAYC  DS    CL12                CORP PAYMENT                                 
BC8      DS    CL1                                                              
LINSPNH  DS    CL12                SUBJ TO P&H                                  
BC9      DS    CL1                                                              
LINPNH   DS    CL12                P&H                                          
BR       DS    CL1                                                              
         EJECT                                                                  
*              DSECT TO COVER SORT RECORD                                       
         SPACE 1                                                                
SORTD    DSECT                                                                  
SORTKEY  EQU   *                   BEGINNING OF KEY                             
SORTAGY  DS    CL6                 AGENCY                                       
SORTCLI  DS    CL6                 CLIENT                                       
SORTPLNQ EQU   *-SORTD             PAGE BREAK AT KEY CHANGE                     
SORTINV  DS    XL6                 INVOICE NUMBER                               
SORTKLNQ EQU   *-SORTD             KEY LENGTH                                   
*                                                                               
SORTCID  DS    CL12                COMMERCIAL ID                                
SORTEST  DS    CL16                ESTIMATE NUMBER                              
SORTUSE  DS    CL3                 USE CODE                                     
SORTUTYP DS    CL5                 USE TYPE CODE                                
*                                                                               
         DS    0F                                                               
SORTAPPL DS    F                   AMOUNT APPLIED                               
SORTPAYI DS    F                   INDIVIDUAL PAYMENT                           
SORTPAYC DS    F                   CORPORATE PAYMENT                            
SORTSPNH DS    F                   SUBJECT TO P&H                               
SORTPNH  DS    F                   P&H                                          
SORTLNQ  EQU   *-SORTD                                                          
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPD6D                                                       
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
**PAN#1  DC    CL21'021TAREP36   06/24/13'                                      
         END                                                                    
