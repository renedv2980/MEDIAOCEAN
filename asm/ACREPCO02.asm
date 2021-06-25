*          DATA SET ACREPCO02  AT LEVEL 017 AS OF 08/16/00                      
*PHASE ACCO02A,*                                                                
*INCLUDE CONVMOS                                                                
         SPACE 2                                                                
         TITLE 'CLOSE OUT PROGRAM'                                              
***********************************************************************         
* QOPT1 - 'U' = UNCLOSE                                               *         
* QOPT2 - 'T' = TRANSACTION LEVEL REPORT                              *         
* QOPT3 - 'D' = DRAFT RUN                                             *         
***********************************************************************         
         SPACE 1                                                                
ACCO02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACLO**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         MVC   LANG,MCLANG                                                      
         MVC   CTRY,MCCTRY                                                      
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVC   AOFACC,ACMAOFA      SAVE A(OFFICE ACCOUNT RECORD BUFFER)         
         DROP  RF                                                               
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF00                                                           
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF00                                                           
         CLI   MODE,PROCACC                                                     
         BE    PACC00                                                           
         CLI   MODE,PROCOFA                                                     
         BE    POFA00                                                           
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN00                                                           
         CLI   MODE,ACCLAST                                                     
         BE    LACC00                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***************************************************************                 
* REQFRST PROCESSING                                          *                 
***************************************************************                 
         SPACE 1                                                                
REQF00   MVI   RCFLAG1,RCFREPLC                                                 
         MVC   PAGE,=H'1'                                                       
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAY) TODAY'S DATE (PWO)              
         GOTO1 (RF),(R1),(4,RCDATE),(2,TODAY2)                                  
         GOTO1 (RF),(R1),,(3,TODAY3)                                            
*                                                                               
         GOTO1 ADDICTAT,DMCB,C'LU  ',DCLIST,DSLIST                              
*                                                                               
         GOTO1 INACCUM,3                                                        
*                                                                               
         MVC   REPCPY,SPACES                                                    
         L     R2,ADCOMP                                                        
         USING CPYRECD,R2                                                       
         XOUT  CPYKCPY,REPCPY,1                                                 
*                                                                               
         L     R2,ADCMPNAM                                                      
         USING NAMELD,R2                                                        
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   REPCPY+5(0),NAMEREC                                              
*                                                                               
         L     R2,ADCMPEL                                                       
         USING CPYELD,R2                                                        
         MVI   PROCIND,0                                                        
         TM    CPYSTAT4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    *+8                                                              
         OI    PROCIND,NEWOFFQ                                                  
         DROP  R2                                                               
*                                                                               
         L     RF,ADBXAREA                                                      
         USING BOXD,RF                                                          
         CLI   QOPT2,C'T'          TEST TRANS LEVEL REPORT REQUIRED             
         BE    REQF02                                                           
         MVI   RCSUBPRG,1                                                       
         TM    PROCIND,NEWOFFQ                                                  
         BZ    *+12                                                             
         MVI   RCSUBPRG,2                                                       
         MVI   BOXCOLS+(P1X0-P1LIN),C'C'                                        
         MVI   BOXCOLS+(P1XL-P1LIN),C'L'                                        
         MVI   BOXCOLS+(P1X1-P1LIN),C'C'                                        
         MVI   BOXCOLS+(P1X2-P1LIN),C'C'                                        
         MVI   BOXCOLS+(P1X3-P1LIN),C'C'                                        
         MVI   BOXCOLS+(P1X4-P1LIN),C'C'                                        
         MVI   BOXCOLS+(P1X5-P1LIN),C'C'                                        
         MVI   BOXCOLS+(P1XR-P1LIN),C'R'                                        
         B     REQF04                                                           
*                                                                               
REQF02   MVI   RCSUBPRG,3                                                       
         TM    PROCIND,NEWOFFQ                                                  
         BZ    *+12                                                             
         MVI   RCSUBPRG,4                                                       
         MVI   BOXCOLS+(P2X0-P2LIN),C'C'                                        
         MVI   BOXCOLS+(P2XL-P2LIN),C'L'                                        
         MVI   BOXCOLS+(P2X1-P2LIN),C'C'                                        
         MVI   BOXCOLS+(P2X2-P2LIN),C'C'                                        
         MVI   BOXCOLS+(P2X3-P2LIN),C'C'                                        
         MVI   BOXCOLS+(P2X4-P2LIN),C'C'                                        
         MVI   BOXCOLS+(P2X5-P2LIN),C'C'                                        
         MVI   BOXCOLS+(P2X6-P2LIN),C'C'                                        
         MVI   BOXCOLS+(P2XR-P2LIN),C'R'                                        
REQF04   MVI   BOXROWS+05,C'T'                                                  
         MVI   BOXROWS+07,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         DROP  RF                                                               
*                                  CONVERT REQUEST MONTH TO PWOS                
         MVC   DOUBLE(L'QMOSEND),QMOSEND                                        
         MVC   DOUBLE+L'QMOSEND(2),=C'01'                                       
         GOTO1 DATCON,DMCB,(0,DOUBLE),(1,THREE)                                 
         MVC   REQMON,THREE                                                     
         CLI   QOPT1,C'U'          TEST UNCLOSING                               
         BNE   EXIT                                                             
         L     R0,=F'-1'           FIND MONTH BEFORE REQ START                  
         GOTO1 ADDAY,(R1),DOUBLE,DOUBLE,(R0)                                    
         GOTO1 DATCON,(R1),(0,DOUBLE),(1,THREE)                                 
         MVC   NEWCLOS,THREE                                                    
         B     EXIT                                                             
         EJECT                                                                  
***************************************************************                 
* LEDGFRST PROCESSING                                         *                 
***************************************************************                 
         SPACE 1                                                                
LDGF00   MVC   REPLGR,SPACES                                                    
         CP    LEDGDR,PZEROS       TEST TOTALS FOR PREVIOUS LEDGER              
         BNE   *+14                                                             
         CP    LEDGCR,PZEROS                                                    
         BE    LDGF02              NO                                           
         L     RF,ADBXAREA         YES - CLOSE BOXES                            
         USING BOXD,RF                                                          
         MVI   BOXREQ,C'B'                                                      
         MVI   BOXBLANK,C'N'                                                    
         MVC   P1ACN,AC@TLDG       'TOTALS FOR LEDGER'                          
         LA    R2,LEDGDR                                                        
         LA    R3,P1DRS                                                         
         BAS   R7,PREDIT           EDIT OUT TOTALS                              
         LA    R2,LEDGCR                                                        
         BAS   R7,PREDIT                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
LDGF02   GOTO1 INACCUM,2           INITIALISE ACCUMULATORS                      
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R2,ADLDGEL                                                       
         USING LDGELD,R2                                                        
         MVC   LDGLCLO,LDGCLOS     EXTRACT LEDGER'S CLOSE OUT TYPE              
         CLI   LDGLCLO,0           TEST TYPE NOT SET                            
         BNE   *+8                                                              
         MVI   LDGLCLO,LDGCBBF     SET DEFAULT                                  
         CLI   LDGLCLO,LDGCPRD     TEST PRODUCTION TYPE                         
         BE    *+8                                                              
         OI    PROCIND,NOTPRDQ     NO - SET INDICATOR                           
*                                                                               
         L     R2,ADLEDGER         BUILD LEDGER HEADLINE                        
         USING LDGRECD,R2                                                       
         MVC   REPLGR(2),LDGKUNT   UNIT/LEDGER CODE                             
*                                                                               
         L     R2,ADLDGNAM                                                      
         USING NAMELD,R2                                                        
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   REPLGR+5(0),NAMEREC  LEDGER NAME                                 
*                                                                               
         LA    RF,REPLGR+7(RF)                                                  
         LA    RE,CLOSNAMS                                                      
LDGF04   CLC   LDGLCLO,0(RE)                                                    
         BE    LDGF06                                                           
         LA    RE,L'CLOSNAMS(RE)                                                
         CLI   0(RE),0                                                          
         BE    EXIT                EOT                                          
         B     LDGF04                                                           
LDGF06   MVC   0(40,RF),1(RE)      LEDGER TYPE FOR CLOSE OUT                    
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***************************************************************                 
* PROCACC PROCESSING                                          *                 
***************************************************************                 
         SPACE 1                                                                
PACC00   MVC   CLOSTYPE,LDGLCLO    SET LEDGER LVL CLOSE TYPE                    
         TM    PROCIND,BYOFFCQ     IF PROCESSING BY OFFICE ACCOUNT              
         BNO   PACC02                                                           
         L     R0,VOFFTAB          CLEAR OFFICE TABLE                           
         LH    R1,=Y(OFFTMAX*OFFTABL)                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     PACC08                                                           
*                                                                               
PACC02   L     RF,ADACC                                                         
         AH    RF,DATADISP                                                      
         SR    R0,R0                                                            
*                                                                               
PACC04   CLI   0(RF),0                                                          
         BE    EXIT                                                             
         CLI   0(RF),APOELQ        FIND PEEL-OFF ELEM                           
         BE    PACC06                                                           
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     PACC04                                                           
*                                                                               
PACC06   ST    RF,ADPEEL           SET PEEL-OFF ELEM ADCON                      
         USING APOELD,RF                                                        
         MVC   LATCLOS,APOCMOS     EXTRACT LATEST CLOSED MOS                    
         MVC   FSTPOST,APOLMOS                                                  
*                                                                               
PACC08   L     RF,ADACCSTA                                                      
         MVI   ACCLOVR,C' '                                                     
         USING RSTELD,RF                                                        
         TM    RSTSTAT4,RSTSOCOT   TEST CLOSE OUT TYPE IS OVERRIDDEN            
         BZ    PACC10              AT ACCOUNT LEVEL                             
         MVI   ACCLOVR,C'*'                                                     
         MVC   CLOSTYPE,RSTSTAT4                                                
         NI    CLOSTYPE,RSTSOCOT   SET CLOSE OUT TYPE FOR THIS ACCOUNT          
         DROP  RF                                                               
*                                                                               
PACC10   BAS   RE,FIND             ROUT FOR THIS LEDGER AT THIS LEVEL           
         BZ    *+6                 EXIT - NO ROUTINE TO BRANCH TO               
         BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 2                                                                
* PROCACC CLOSE OUT - GENERAL                                                   
*                                                                               
ACC      NTR1  ,                                                                
         TM    PROCIND,NEWOFFQ     TEST COMPANY ON NEW OFFICES                  
         BNZ   ACC2                YES                                          
*                                                                               
         MVI   FCRDTRNS,C'Y'                                                    
         CLC   LATCLOS,REQMON      TEST LATEST CLOSED MOS VS REQUESTED          
         BL    ACC2                IF < REQUESTED MOS, THEN PROCEED             
         MVI   FCRDTRNS,C'N'       ELSE FORGET TRANSACTION READ                 
         B     ACC2                BECAUSE IT IS ALREADY CLOSED                 
*                                                                               
ACC2     GOTO1 INACCUM,1                                                        
         B     EXIT                                                             
         SPACE 2                                                                
* PROCACC UNCLOSE OUT - GENERAL                                                 
*                                                                               
UNACC    NTR1  ,                                                                
         TM    PROCIND,NEWOFFQ     TEST COMPANY USES NEW OFFICES                
         BNZ   UNACC2                                                           
         MVI   FCRDTRNS,C'N'       SET TO SKIP TRANSACTIONS                     
         CLC   REQMON,FSTPOST      TEST REQ MOS < FIRST POSTED MOS              
         BL    UNACCX              YES-REJECT                                   
         CLC   LATCLOS,REQMON      TEST LATEST CLOSE MOS > OR = START           
         BL    UNACCX                                                           
         MVI   FCRDTRNS,C'Y'       READ TRANSACTIONS                            
*                                                                               
UNACC2   GOTO1 INACCUM,1                                                        
*                                                                               
UNACCX   B     EXIT                                                             
         EJECT                                                                  
***************************************************************                 
* PROCOFA PROCESSING - BUILD OFFICE TABLE ENTRY FOR EACH      *                 
* OFFICE FOR THIS ACCOUNT                                     *                 
***************************************************************                 
         SPACE 1                                                                
POFA00   L     R2,AOFACC                                                        
         USING OFARECD,R2                                                       
         L     RF,VOFFTAB                                                       
         USING OFFTABD,RF                                                       
POFA02   CLI   OFFTOFF,0           LOCATE NEXT FREE ENTRY                       
         BE    POFA04                                                           
         CLI   OFFTOFF,255         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    RF,OFFTABL(RF)                                                   
         B     POFA02                                                           
*                                  SAVE OFFICE DETAILS                          
POFA04   MVC   OFFTOFF,OFAKOFF                                                  
         MVC   OFFCMOS,OFARCMOS                                                 
         MVC   OFFLMOS,OFARLMOS                                                 
         ZAP   OFFCLDR,PZEROS                                                   
         ZAP   OFFCLCR,PZEROS                                                   
         B     EXIT                                                             
         DROP  R2,RF                                                            
         EJECT                                                                  
***************************************************************                 
* PROCTRNS PROCESSING                                         *                 
***************************************************************                 
         SPACE 1                                                                
PTRN00   L     R2,ADTRANS                                                       
         SH    R2,=Y(ACCORFST)                                                  
         ST    R2,ATRNREC          SAVE A(TRANSACTION RECORD)                   
         GOTO1 VCONVMOS,DMCB,(0,ADTRANS),PTRNMOS                                
*                                                                               
         TM    PROCIND,NEWOFFQ     IF NEW OFFICES, FIND OFFTAB ENTRY            
         BZ    PTRN06                                                           
         L     RF,AOFFTNT          CHECK SAME AS CURRENT                        
         USING OFFTABD,RF                                                       
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         CLC   OFFTOFF,TRNKOFF                                                  
         BE    PTRN06              YES - DONE                                   
         L     RF,VOFFTAB          NO - START AT FIRST ENTRY                    
PTRN02   CLI   OFFTOFF,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   OFFTOFF,255                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OFFTOFF,TRNKOFF                                                  
         BE    PTRN04                                                           
         LA    RF,OFFTABL(RF)                                                   
         B     PTRN02                                                           
*                                                                               
PTRN04   ST    RF,AOFFTNT          SAVE ADDRESS OF ENTRY                        
         MVC   LATCLOS,OFFCMOS     SET LATEST CLOSED MONTH                      
         MVC   FSTPOST,OFFLMOS     SET FIRST POSTED MONTH                       
*                                                                               
PTRN06   BAS   RE,FIND                                                          
         BZ    *+6                                                              
         BASR  RE,RF                                                            
         B     EXIT                                                             
         DROP  R2,RF                                                            
         SPACE 2                                                                
* PROCTRNS UNCLOSE OUT - GENERAL                                                
*                                                                               
UNTRNS   NTR1  ,                                                                
         CLC   PTRNMOS,LATCLOS                                                  
         BH    UNTRNSX                                                          
         CLC   PTRNMOS,REQMON                                                   
         BL    UNTRNSX                                                          
*                                                                               
         GOTO1 UPBUCK,DMCB,ADTRANS                                              
         CLI   QOPT2,C'T'                                                       
         BNE   *+8                                                              
         BAS   RE,PRNTTRN                                                       
UNTRNSX  B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
***************************************************************                 
* ACCLAST PROCESSING                                          *                 
***************************************************************                 
         SPACE 1                                                                
LACC00   BAS   RE,FIND                                                          
         BZ    *+6                                                              
         BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 2                                                                
* ACCLAST CLOSE OUT - GENERAL                                                   
*                                                                               
ACCL     NTR1  ,                                                                
         BAS   RE,UPACC            UPDATE ACCOUNT RECORD                        
         CLI   QOPT3,C'D'          TEST DRAFT RUN                               
         BE    *+8                                                              
         MVI   MODE,WRITACC        WRITE BACK ACCOUNT RECORD                    
         B     EXIT                                                             
         SPACE 2                                                                
* ACCLAST UNCLOSE OUT - GENERAL                                                 
*                                                                               
UNACCL   NTR1  ,                                                                
         BAS   RE,UPUNACC                                                       
         CLI   QOPT3,C'D'          TEST DRAFT RUN                               
         BE    *+8                                                              
         MVI   MODE,WRITACC                                                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO FIND AN ENTRY IN CLOSTAB DEPENDING ON THE COMBINATION*         
* OF LEDGER TYPE, MONACC MODE, AND CLOSE/UNCLOSE REQUEST.             *         
* THERE ARE THREE POSSIBLE (VALID) RESULTS:                           *         
* RF=A(0) - NO PROCESSING IS NECESSARY FOR THIS COMBINATION,          *         
* RF=A(GENERALISED S/R) - PROCESS ACCORDING TO MODE/UN/CLOSE          *         
* RF=A(LDGR-SPECIFIC S/R) - PROCESS ACCORDING TO LEDGER/MODE/UN/CLOSE *         
* ON EXIT, CC=ZERO: NO BRANCH ADDRESS, CC=NON-ZERO: BRANCH ADDRESS SET*         
***********************************************************************         
         SPACE 1                                                                
FIND     LA    R2,CLOSTAB                                                       
         USING CLOSTABD,R2                                                      
         LA    R0,NCLOSTYP         R0=LOOP COUNTER                              
         SR    RF,RF                                                            
         CLC   CLOSTYPE,0(R2)      MATCH ON LEDGER TYPE                         
         BE    FIND2                                                            
         LA    R2,L'CLOSTAB(R2)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
FIND2    LA    R0,NMODES                                                        
         LA    R1,MODETAB                                                       
         CLC   MODE,0(R1)                                                       
         BE    FIND4                                                            
         LA    R1,L'MODETAB(R1)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
FIND4    SR    RF,RF                                                            
         ZIC   R0,1(R1)            GET DISP TO ROUTINES                         
         LR    R1,R2                                                            
         AR    R1,R0               INDEX TO ROUTINES                            
         CLI   QOPT1,C'U'          TEST FOR UNCLOSE                             
         BNE   *+8                                                              
         LA    R1,2(R1)                                                         
         ICM   RF,3,0(R1)          GET DISP TO ROUTINE                          
         BZR   RE                  NOWHERE TO BRANCH TO EXIT                    
         LA    RF,ACCO02(RF)       INDEX TO ROUTINE                             
         LTR   RF,RF               SET CC                                       
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
*****************************************************************               
* MATCHED BALANCE FORWARD LEDGER CLOSE OUT                      *               
*                                                               *               
* TRANSACTION -- TRANSACTION'S MOS MUST BE > LATEST CLOSED MOS  *               
*                AND MUST BE < OR = REQUESTED MOS               *               
*                TRNSTAT MUST HAVE THE TRNSMTCH BIT ON          *               
*****************************************************************               
         SPACE 1                                                                
MBF      NTR1  ,                                                                
         CLI   MODE,PROCTRNS                                                    
         BE    MBF10                                                            
         B     MBFX                                                             
         SPACE 1                                                                
* TRANSACTION PROCESSING                                                        
*                                                                               
MBF10    L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         CLC   PTRNMOS,LATCLOS     MUST BE > LATEST CLOSE                       
         BNH   MBFX                NO-DO NOT CLOSE                              
         CLC   PTRNMOS,REQMON      MUST BE < OR = REQUESTED MOS                 
         BH    MBFX                NO                                           
         L     R2,ADTRANS          R2=A(TRANSACTION ELEMENT)                    
         USING TRNELD,R2                                                        
         TM    TRNSTAT,TRNSMTCH    TEST THAT TRANSACTION IS MATCHED             
         BZ    MBFX                NO-CANNOT BE CLOSED                          
*                                                                               
MBF12    GOTO1 UPBUCK,DMCB,ADTRANS                                              
         CLI   QOPT2,C'T'                                                       
         BNE   *+8                                                              
         BAS   RE,PRNTTRN                                                       
         B     MBFX                                                             
         SPACE 1                                                                
MBFX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*****************************************************************               
* MATCHED BALANCE FORWARD LEDGER UNCLOSE OUT                    *               
*                                                               *               
* ACCOUNT -- REQUESTED MOS (START) MUST BE > OR = FIRST         *               
*            POSTING MONTH AND LATEST CLOSED MONTH MUST         *               
*            BE > OR = REQUESTED MOD (OLD OFFICES ONLY)         *               
*                                                               *               
* OFFICE/ACCOUNT -- SAME LOGIC AS FOR ACCOUNT (NEW OFFICES ONLY)*               
*                                                               *               
* TRANSACTION -- TRANSACTION'S MOS MUST BE < OR = LATEST CLOSED *               
*                MOS AND MUST BE > OR = REQUESTED MOS           *               
*                TRNSTAT MUST HAVE THE TRNSMTCH BIT ON          *               
*****************************************************************               
         SPACE 1                                                                
UNMBF    NTR1  ,                                                                
         CLI   MODE,PROCTRNS                                                    
         BE    UNMBF10                                                          
         B     UNMBFX                                                           
         SPACE 1                                                                
* TRANSACTION PROCESSING                                                        
*                                                                               
UNMBF10  L     R2,ADTRANS                                                       
         USING TRNELD,R2                                                        
         CLC   PTRNMOS,LATCLOS                                                  
         BH    UNMBFX                                                           
         CLC   PTRNMOS,REQMON                                                   
         BL    UNMBFX                                                           
         TM    TRNSTAT,TRNSMTCH    MUST BE MATCHED                              
         BZ    UNMBFX                                                           
*                                                                               
UNMBF12  GOTO1 UPBUCK,DMCB,ADTRANS                                              
         CLI   QOPT2,C'T'                                                       
         BNE   *+8                                                              
         BAS   RE,PRNTTRN                                                       
         B     UNMBFX                                                           
         SPACE 1                                                                
UNMBFX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***************************************************************                 
* CASH LEDGER CLOSE OUT                                       *                 
* TRANSACTION --  MOS MUST BE > OR = LATEST CLOSED MOS AND    *                 
*                 < OR = REQUESTED MOS                        *                 
*                 IF ACCOUNT SPECIFIES CLOSING ONLY           *                 
*                 RECONCILED DEBITS AND/OR CREDITS, THEN      *                 
*                 TRNSTAT MUST HAVE TRNSBREC BIT ON           *                 
***************************************************************                 
         SPACE 1                                                                
CSH      NTR1  ,                                                                
         CLI   MODE,PROCTRNS                                                    
         BE    CSH10                                                            
         B     CSHX                                                             
         SPACE 1                                                                
* TRANSACTION PROCESSING                                                        
*                                                                               
CSH10    CLC   PTRNMOS,LATCLOS                                                  
         BNH   CSHX                CANNOT CLOSE IT                              
         CLC   PTRNMOS,REQMON                                                   
         BH    CSHX                                                             
         L     R3,ADACCSTA                                                      
         USING RSTELD,R3                                                        
         L     R2,ADTRANS                                                       
         USING TRNELD,R2                                                        
         TM    TRNSTAT,TRNSDR      TEST FOR DEBIT                               
         BO    CSH12               YES                                          
*                                                                               
         TM    RSTSTAT3,RSTSPRCR   TEST CLOSE RECONCILED CREDITS                
         BO    CSH14               YES-APPLY TEST                               
         B     CSH15               NO-CLOSE TRANSACTION                         
*                                                                               
CSH12    TM    RSTSTAT3,RSTSPRDR   TEST CLOSE RECONCILED DEBITS                 
         BZ    CSH15               NO-CLOSE TRANSACTION                         
*                                                                               
CSH14    TM    TRNSTAT,TRNSBREC    TEST TRANSACTION HAS BEEN RECON.             
         BZ    CSHX                NO-CANNOT CLOSE IT                           
*                                                                               
CSH15    GOTO1 UPBUCK,DMCB,ADTRANS                                              
         CLI   QOPT2,C'T'                                                       
         BNE   *+8                                                              
         BAS   RE,PRNTTRN                                                       
*                                                                               
CSHX     B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***************************************************************                 
* PAYABLE LEDGER CLOSE OUT                                    *                 
* TRANSACTION --  MOS MUST BE > OR = LATEST CLOSED MOS AND    *                 
*                 < OR = REQUESTED MOS                        *                 
*                 USED MOS MUST BE PRESENT AND MEET ABOVE     *                 
*                 TESTS                                       *                 
***************************************************************                 
         SPACE 1                                                                
PBL      NTR1  ,                                                                
         CLI   MODE,PROCTRNS                                                    
         BE    PBL10                                                            
         B     PBLX                                                             
         SPACE 1                                                                
* TRANSACTION PROCESSING                                                        
*                                                                               
PBL10    L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         TM    TRNRSTA2,TRNSUSED   TEST FOR USED MOS                            
         BZ    PBLX                NO-CANNOT CLOSE IT                           
*                                                                               
         CLC   PTRNMOS,LATCLOS                                                  
         BNH   PBLX                CANNOT CLOSE IT                              
         CLC   PTRNMOS,REQMON                                                   
         BH    PBLX                                                             
*                                                                               
         CLC   TRNRSUSE,LATCLOS                                                 
         BNH   PBLX                CANNOT CLOSE IT                              
         CLC   TRNRSUSE,REQMON                                                  
         BH    PBLX                                                             
*                                                                               
         GOTO1 UPBUCK,DMCB,ADTRANS                                              
         CLI   QOPT2,C'T'                                                       
         BNE   *+8                                                              
         BAS   RE,PRNTTRN                                                       
*                                                                               
PBLX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***************************************************************                 
* PAYABLE LEDGER UNCLOSE OUT                                  *                 
***************************************************************                 
         SPACE 1                                                                
UNPBL    NTR1  ,                                                                
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         TM    TRNRSTA2,TRNSUSED   TEST FOR USED MOS                            
         BZ    UNPBLX                                                           
*                                                                               
         CLC   PTRNMOS,LATCLOS     TEST IF TRANS IS CLOSED                      
         BH    UNPBLX                                                           
*                                                                               
         CLC   PTRNMOS,REQMON      TEST NOT LATER THAN REQUESTED MOS            
         BH    UNPBLX                                                           
         CLC   TRNRSUSE,REQMON                                                  
         BH    UNPBLX                                                           
*                                                                               
         GOTO1 UPBUCK,DMCB,ADTRANS                                              
         CLI   QOPT2,C'T'                                                       
         BNE   *+8                                                              
         BAS   RE,PRNTTRN                                                       
*                                                                               
UNPBLX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**************************************************************                  
* BALANCE BROUGHT FORWARD LEDGER CLOSE OUT                   *                  
* TRANSACTIONS -- MOS MUST BE > LATEST CLOSED MOS AND        *                  
*                 < OR = REQUESTED MOS                       *                  
*                 + TRNSMTCH BIT ON IF CLOSE TYPE OVERRIDDEN *                  
**************************************************************                  
         SPACE 1                                                                
BBF      NTR1  ,                                                                
         CLI   MODE,PROCTRNS                                                    
         BE    BBF10                                                            
         B     BBFX                                                             
         SPACE 1                                                                
* TRANSACTION PROCESSING                                                        
*                                                                               
BBF10    CLC   PTRNMOS,LATCLOS                                                  
         BNH   BBFX                                                             
         CLC   PTRNMOS,REQMON                                                   
         BH    BBFX                                                             
BBF12    GOTO1 UPBUCK,DMCB,ADTRANS                                              
         CLI   QOPT2,C'T'                                                       
         BNE   *+8                                                              
         BAS   RE,PRNTTRN                                                       
*                                                                               
BBFX     B     EXIT                                                             
         EJECT                                                                  
**************************************************************                  
* BALANCE BROUGHT FORWARD LEDGER UNCLOSE OUT                 *                  
* TRANSACTIONS -- MOS MUST BE < OR = LATEST CLOSED MOS AND   *                  
*                 > OR = REQUESTED MOS                       *                  
*                 + TRNSMTCH BIT ON IF CLOSE TYPE OVERRIDDEN *                  
**************************************************************                  
         SPACE 1                                                                
UNBBF    NTR1  ,                                                                
         CLI   MODE,PROCTRNS                                                    
         BE    UNBBF10                                                          
         B     BBFX                                                             
         SPACE 1                                                                
* TRANSACTION PROCESSING                                                        
*                                                                               
UNBBF10  CLC   PTRNMOS,LATCLOS                                                  
         BH    UNBBFX                                                           
         CLC   PTRNMOS,REQMON                                                   
         BL    UNBBFX                                                           
UNBBF12  GOTO1 UPBUCK,DMCB,ADTRANS                                              
         CLI   QOPT2,C'T'                                                       
         BNE   *+8                                                              
         BAS   RE,PRNTTRN                                                       
*                                                                               
UNBBFX   B     EXIT                                                             
         EJECT                                                                  
**************************************************************                  
* PRODUCTION LEDGER CLOSE OUT                                *                  
* ACCOUNT (JOB) - BALANCE MUST BE 0, NO UNMATCHED ('**')     *                  
*                 ORDERS, STATUS NOT = CLOSED                *                  
**************************************************************                  
         SPACE 1                                                                
PLAC     NTR1  ,                                                                
         CLI   MODE,PROCACC                                                     
         BE    PL10                                                             
         B     PLX                                                              
         SPACE 1                                                                
* ACCOUNT PROCESSING                                                            
*                                                                               
PL10     MVI   FCRDTRNS,C'N'                                                    
         MVI   PLCLOSE,C'N'                                                     
         L     R2,ADACCBAL                                                      
         USING ABLELD,R2                                                        
         CP    ABLDR,ABLCR         LOOKING FOR ZERO BALANCE                     
         BNE   PLX                                                              
         CP    ABLDR,PZEROS        BUT NOT ZERO                                 
         BE    PLX                                                              
         ZAP   ACCDR,ABLDR         SAVE VALUES FOR UPDATE AT ACCLAST            
         ZAP   ACCCR,ABLCR                                                      
         L     R2,ADACCSTA                                                      
         USING RSTELD,R2                                                        
         TM    RSTSTAT,RSTSACIC    TEST ACCOUNT STATUS                          
         BO    PLX                 ACCOUNT IS ALREADY CLOSED                    
         CLC   RSTTDATE(L'REQMON),REQMON  TEST ACTIVE AFTER REQUEST END         
         BH    PLX                                                              
         L     R2,VIOBUFF                                                       
         USING ISDAD,R2                                                         
         LA    R2,ISDADR           READ FOR UNMATCHED ORDERS                    
         USING CACRECD,R2                                                       
         MVC   CACKEY,SPACES                                                    
         L     RF,ADACC                                                         
         MVC   CACKCULA,0(RF)                                                   
         MVC   CACKWRK,=C'**'                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,CACKEY,CACKEY                         
         CLC   CACKWRK,=C'**'                                                   
         BE    PLX                 UNMATCHED ORDER EXISTS - CAN'T CLOSE         
         L     R2,ADACCSTA                                                      
         USING RSTELD,R2                                                        
         OI    RSTSTAT,RSTSACIC    SET CLOSED STATUS                            
         BAS   RE,UPACC            UPDATE ACCOUNT RECORD                        
         CLI   QOPT3,C'D'          TEST DRAFT RUN                               
         BE    *+8                                                              
         MVI   MODE,WRITACC                                                     
         B     PLX                                                              
*                                                                               
PLX      B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***************************************************************                 
* PRODUCTION LEDGER UNCLOSE OUT                               *                 
* ACCOUNT (JOB) -  MUST HAVE CLOSED STATUS, CLOSED MOS MUST   *                 
*                  BE > OR = REQUESTED UNCLOSE MOS            *                 
***************************************************************                 
         SPACE 1                                                                
UNPLAC   NTR1  ,                                                                
         CLI   MODE,PROCACC                                                     
         BNE   UNPLX                                                            
         L     R2,ADACCSTA                                                      
         USING RSTELD,R2                                                        
         TM    RSTSTAT,RSTSACIC    JOB MUST HAVE CLOSED STATUS                  
         BNO   UNPLX                                                            
         L     R3,ADPEEL                                                        
         USING APOELD,R3                                                        
         OC    APOCMOS,APOCMOS     PEEL ELEM MUST BE SET                        
         BZ    UNPLX                                                            
         CLC   APOCMOS,REQMON      AND CLOSED MOS MATCH REQUEST                 
         BNE   UNPLX                                                            
         XC    APOPLDT(APOLN2Q-2),APOPLDT     CLEAR ELEMENT                     
         NI    RSTSTAT,X'FF'-RSTSACIC         UNSET CLOSED STATUS               
         BAS   RE,UPUNACC                                                       
         CLI   QOPT3,C'D'          TEST DRAFT RUN                               
         BE    *+8                                                              
         MVI   MODE,WRITACC                                                     
UNPLX    B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
**************************************************************                  
* RECEIVABLE LEDGER CLOSE OUT                                *                  
* ACCOUNT - TRANSACTIONS WITH COMMON CONTRA A/C, OFFICE      *                  
*           (IF ON NEW OFFICES), DATE AND REFERENCE, WITH    *                  
*           A MOS < OR = TO REQUESTED MOS ARE MATCHED OFF.   *                  
*           FILE IS READ FOR ACCOUNTS, SUBACCS AND           *                  
*           TRANSACTIONS BY THIS ROUTINE                     *                  
**************************************************************                  
         SPACE 1                                                                
RCV      NTR1  ,                                                                
         L     R2,ADACC                                                         
         MVC   SAVEACC,0(R2)       SAVE FOR L'C/U/L/ACCOUNT+OFFICE CODE         
         L     R2,VIOBUFF                                                       
         USING ISDAD,R2                                                         
         MVC   ISDADR,SPACES                                                    
         MVC   ISDADR(L'SAVEACC),SAVEACC                                        
         XC    RDSTS,RDSTS                                                      
         MVI   PASS,1                                                           
         LA    R4,TRIOB                                                         
         USING TRNELD,R4                                                        
         LA    R3,TRIOA                                                         
         USING CACELD,R3           CONTRA HEADER ELEMENT                        
         L     R5,ABILBLOC                                                      
         USING BIND,R5                                                          
         XC    BININ,BININ         CLEAR NUMBER IN TABLE                        
*                                                                               
RCV20    XC    TRIOA,TRIOA                                                      
         XC    TRIOB,TRIOB                                                      
RCV30    GOTO1 DATAMGR,DMCB,(RDSTS,DMRSEQ),ACCDIR,ISDADR,ISDADR                 
         CLC   SAVEACC,ISDADR      CHECK FOR ACCOUNT END                        
         BNE   RCV40                                                            
         LA    R1,ISDADR                                                        
         MVC   ISDADA,ACCKDA-ACCRECD(R1)                                        
         GOTO1 DATAMGR,DMCB,(RDSTS,GETREC),ACCMST,ISDADA,ISDABF,ISDAWK          
         LA    R1,ISDABF                                                        
         MVC   TRIOB,ACCRFST-ACCRECD(R1)                                        
         CLI   TRIOB,TRNELQ        TRANSACTION ELEMENT                          
         BE    RCV50                                                            
         CLI   TRIOB,CACELQ        CONTRA A/C HDR ELEMENT                       
         BNE   RCV30                                                            
         MVC   TRIOA,TRIOB         SAVE CONTRA HDR                              
         B     RCV30                                                            
*                                  AT ACCOUNT END, RESTORE KEY AND              
RCV40    MVC   ISDADR,SPACES       RE-READ ACCOUNT LEVEL REC                    
         MVC   ISDADR(L'SAVEACC),SAVEACC                                        
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,ISDADR,ISDADR                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,ISDADR                                                        
         MVC   ISDADA,ACCKDA-ACCRECD(R1)                                        
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,ISDADA,ISDABF,ISDAWK                  
         CLI   PASS,2                                                           
         BE    RCV70               END OF PROCESS                               
         MVI   PASS,2                                                           
         MVI   RDSTS,X'80'         SET STATUS TO READ-FOR-UPDATE                
         B     RCV20               ON SECOND PASS                               
         SPACE 1                                                                
*                                  PROCESS A TRANSACTION                        
         USING TRNELD,R4                                                        
RCV50    CLC   TRNMOS,REQMON       TRANS MOS MUST BE < OR = REQUEST             
         BH    RCV30                                                            
         CLC   TRNMOS,LATCLOS      AND > LAST CLOSED MOS                        
         BNH   RCV30                                                            
         LA    R6,WORK                                                          
         USING BILD,R6                                                          
         XC    WORK,WORK                                                        
         MVC   BILCON,CACCNTA      BUILD BINSRCH KEY                            
         MVC   BILDTE,TRNDATE                                                   
         MVC   BILREF,TRNREF                                                    
         L     R5,ABILBLOC                                                      
         CLI   PASS,2              BINSRCH ACTION - GET OR PUT                  
         BE    RCV60               PASS 2 - GET ITEM FROM TABLE                 
         ZAP   DUB,TRNAMNT         PASS 1 - PUT ITEM TO TABLE                   
         TM    TRNSTAT,X'80'       ADD DEBITS                                   
         BO    *+10                                                             
         MP    DUB,=P'-1'          SUBTRACT CREDITS                             
         ZAP   BILAMT,DUB                                                       
         GOTO1 BINADD,DMCB,(R6),(R5)                                            
         B     RCV30                                                            
*                                                                               
RCV60    GOTO1 BINGET,DMCB,(R6),(R5)                                            
         L     R6,DMCB                                                          
         CLC   BILD(BILKLEN),WORK                                               
         BE    *+6                                                              
         DC    H'0'                ITEM NOT FOUND ON PASS 2                     
         CP    BILAMT,PZEROS       IF THE BILL IS NET                           
         BNE   RCV30               ZERO, DELETE THE TRANSACTION                 
         GOTO1 UPBUCK,DMCB,(R4)                                                 
         LA    R1,ISDABF           DELETE MATCHED TRANSACTION                   
         USING TRNRECD,R1                                                       
         OI    TRNRSTAT,TRNSDELT                                                
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,ISDADA,ISDABF,ISDAWK                  
         ORG   *-2                                                              
         CLI   QOPT3,C'D'          TEST DRAFT RUN                               
         BE    *+6                 DON'T UPDATE                                 
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,ISDADR                                                        
         OI    TRNKSTAT,TRNSDELT                                                
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,ISDADR,ISDADR                          
         ORG   *-2                                                              
         CLI   QOPT3,C'D'          TEST DRAFT RUN                               
         BE    *+6                 DON'T UPDATE                                 
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BE    RCV30                                                            
         DC    H'0'                                                             
*                                                                               
RCV70    BAS   RE,UPACC                                                         
         B     RCVX                                                             
*                                                                               
RCVX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************                      
* SUB-ROUTINE TO UPDATE THE BUCKETS FOR A TRANSACTION    *                      
* AT ENTRY, P1=A(TRANSACTION ELEMENT)                    *                      
**********************************************************                      
         SPACE 1                                                                
UPBUCK   ST    RE,SAVERE                                                        
         L     RE,0(R1)            RE=A(TRANSACTION ELEMENT)                    
         USING TRNELD,RE                                                        
         LA    R0,NACCUMS          R0=LOOP COUNTER                              
         LA    R1,ACCUMS           R1=A(ACCUMULATORS)                           
         TM    TRNSTAT,TRNSDR      TEST FOR DEBIT                               
         BO    *+8                 YES                                          
         LA    R1,L'ACCUMS(R1)     NO-NEXT ACCUMULATOR                          
*                                                                               
         AP    0(L'ACCUMS,R1),TRNAMNT                                           
         LA    R1,L'ACCUMS*2(R1)   NEXT PAIR OF ACCUMS                          
         BCT   R0,*-10                                                          
         TM    PROCIND,NEWOFFQ     IF NEW OFFICES                               
         BZ    UPBUCKX                                                          
*                                                                               
         L     RF,AOFFTNT          MAINTAIN OFFICE ACCUMS TABLE ENTRY           
         USING OFFTABD,RF                                                       
         LA    R1,OFFCLDR                                                       
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R1,OFFCLCR                                                       
         AP    0(L'OFFCLDR,R1),TRNAMNT                                          
         DROP  RE,RF                                                            
*                                                                               
UPBUCKX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************                     
* SUB-ROUTINE TO CLEAR ACCUMULATORS                       *                     
* AT ENTRY, R1=N'ACCUMULATOR PAIRS TO CLEAR               *                     
***********************************************************                     
         SPACE 1                                                                
INACCUM  LA    RF,ACCUMS                                                        
INACCUM2 ZAP   0(L'ACCUMS,RF),PZEROS                                            
         ZAP   L'ACCUMS(L'ACCUMS,RF),PZEROS                                     
         LA    RF,L'ACCUMS*2(RF)                                                
         BCT   R1,INACCUM2                                                      
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
* SUB-ROUTINE TO UPDATE AN ACCOUNT OR OFFICE ACCOUNTS TO    *                   
* REFLECT A CLOSE-OUT.                                      *                   
*************************************************************                   
         SPACE 1                                                                
UPACC    NTR1  ,                                                                
         L     RF,ADBXAREA                                                      
         MVI   BOXREQ-BOXD(RF),C'B'  SET BOX BOTTOM                             
         TM    PROCIND,BYOFFCQ     TEST PROCESSING OFFICE ACCOUNTS              
         BO    UPACC10                                                          
*                                                                               
UPACC02  L     R2,ADACCBAL         R2=A(BALANCE ELEMENT)                        
         USING ABLELD,R2                                                        
         ZAP   REPBBF,ABLFRWD      REPORT BAL B/F BEFORE UPDATE                 
         AP    ABLFRWD,ACCDR       ADD DEBITS TO BBF                            
         SP    ABLFRWD,ACCCR       SUBTRACT CREDITS FROM BBF                    
         SP    ABLDR,ACCDR                                                      
         SP    ABLCR,ACCCR                                                      
         ZAP   REPCDR,ACCDR        REPORT CLOSED DEBITS                         
         ZAP   REPCCR,ACCCR        REPORT CLOSED CREDITS                        
         ZAP   REPBCF,ABLFRWD      REPORT BAL B/F AFTER UPDATE                  
         BAS   RE,PRNTACC          PRINT THE DETAILS                            
*                                                                               
         L     R2,ADPEEL           UPDATE PEEL-OFF ELEM                         
         USING APOELD,R2                                                        
         ZAP   APODR,ACCDR         DEBITS SO FAR                                
         ZAP   APOCR,ACCCR         CREDITS SO FAR                               
         MVC   APOCMOS,REQMON      NOTE CLOSE OUT MONTH                         
         OC    APOLBDT,APOLBDT     TEST LAST PEEL-OFF SET                       
         BNZ   UPACC04                                                          
         MVC   APOPLDT,TODAY                                                    
         MVC   APOLBDT,TODAY                                                    
         B     UPACC06                                                          
UPACC04  MVC   APOLBDT,APOPLDT     SET LAST PEEL-OFF                            
         MVC   APOPLDT,TODAY                                                    
*                                                                               
UPACC06  L     R2,ADACCSTA                                                      
         USING RSTELD,R2                                                        
         MVC   RSTBDATE,TODAY      NOTE NEWEST PEEL-OFF                         
*                                                                               
         MVI   MODE,WRITACC                                                     
         B     UPACCX                                                           
         DROP  R2                                                               
UPACC10  BAS   RE,UPOFACS                                                       
*                                                                               
UPACCX   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
* SUB-ROUTINE TO UPDATE AN ACCOUNT OR OFFICE ACCOUNTS TO    *                   
* REFLECT AN UNCLOSE.                                       *                   
*************************************************************                   
         SPACE 1                                                                
UPUNACC  NTR1  ,                                                                
         TM    PROCIND,BYOFFCQ     TEST PROCESSING OFFICE ACCOUNTS              
         BO    UPUN10                                                           
*                                                                               
UPUN02   L     R2,ADACCBAL         R7=A(BALANCE ELEMENT)                        
         USING ABLELD,R2                                                        
         ZAP   REPBBF,ABLFRWD                                                   
         SP    ABLFRWD,ACCDR       REDUCE BBF                                   
         AP    ABLFRWD,ACCCR                                                    
         AP    ABLDR,ACCDR         INCREASE CURRENT BALANCE                     
         AP    ABLCR,ACCCR                                                      
         ZAP   REPCDR,ACCDR        REPORT CLOSED DEBITS                         
         ZAP   REPCDR,ACCCR        REPORT CLOSED CREDITS                        
         ZAP   REPBCF,ABLFRWD      REPORT BAL B/F AFTER UPDATE                  
         BAS   RE,PRNTACC          PRINT THE DETAILS                            
*                                                                               
         CLI   CLOSTYPE,LDGCPRD    IF PRODUCTION LEDGER                         
         BE    UPUNACCX            HANDLE PEEL-OFF ELEM SEPARATELY              
*                                                                               
         L     R2,ADPEEL           UPDATE PEEL-OFF ELEM                         
         USING APOELD,R2                                                        
         ZAP   APODR,PZEROS        DEBITS SO FAR                                
         ZAP   APOCR,PZEROS        CREDITS SO FAR                               
         MVC   APOCMOS,NEWCLOS     SET CLOSE OUT MONTH                          
         MVC   APOPLDT,APOLBDT                                                  
*                                                                               
         MVI   MODE,WRITACC                                                     
         B     UPUNACCX                                                         
         DROP  R2                                                               
*                                                                               
UPUN10   BAS   RE,UPOFACS          UPDATE OFFICE ACCOUNT RECORDS                
*                                                                               
UPUNACCX B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
* UPDATE ALL OFFICE ACCOUNT RECORDS FOR AN ACCOUNT TO       *                   
* REFLECT A CLOSE OR UNCLOSE                                *                   
*************************************************************                   
         SPACE 1                                                                
UPOFACS  ST    RE,SAVERE                                                        
         L     RF,ADBXAREA                                                      
         MVI   BOXREQ-BOXD(RF),C'B'  SET BOX BOTTOM                             
         L     R4,VIOBUFF                                                       
         USING ISDAD,R4                                                         
         L     R2,VOFFTAB                                                       
         USING OFFTABD,R2                                                       
UPOF02   CLI   OFFTOFF,0                                                        
         BE    UPOFX                                                            
         CLI   OFFTOFF,255                                                      
         BE    UPOFX                                                            
         B     UPOF06                                                           
UPOF04   LA    R2,OFFTABL(R2)                                                   
         B     UPOF02                                                           
*                                                                               
UPOF06   LA    R3,ISDADR           BUILD OFFICE ACCOUNT KEY                     
         USING OFARECD,R3                                                       
         L     RF,ADACC                                                         
         MVC   OFAKEY,SPACES                                                    
         MVC   OFAKCULA,ACTKCULA-ACTRECD(RF)                                    
         MVC   OFAKOFF,OFFTOFF                                                  
         MVC   REPOFF,OFFTOFF      SET OFFICE CODE FOR REPORT                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,OFAKEY,OFAKEY                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ISDADA,OFAKDA                                                    
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,ISDADA,ISDABF,ISDAWK                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,ISDABF           R3=A(DATA REC)                               
         LA    RF,OFARFST                                                       
         USING ABLELD,RF                                                        
         SR    R0,R0                                                            
UPOF08   CLI   ABLEL,0             UPDATE OFFICE ACC BALANCE ELEMENT            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ABLEL,ABLELQ                                                     
         BE    UPOF10                                                           
         IC    R0,ABLLN                                                         
         AR    RF,R0                                                            
         B     UPOF08                                                           
*                                                                               
UPOF10   ZAP   REPBBF,ABLFRWD      SAVE BAL B/F                                 
         CLI   QOPT1,C'U'          TEST UNCLOSING                               
         BE    UPOF12                                                           
*                                  * CLOSE OUT *                                
         AP    ABLFRWD,OFFCLDR     ADD DEBITS TO BBF                            
         SP    ABLFRWD,OFFCLCR     SUBTRACT CREDITS FROM BBF                    
         SP    ABLDR,OFFCLDR       SUBTRACT FROM CURRENT DEBITS                 
         SP    ABLCR,OFFCLCR       AND CREDITS                                  
         B     UPOF14                                                           
*                                  * UNCLOSE OUT *                              
UPOF12   SP    ABLFRWD,OFFCLDR     SUBTRACT DEBITS FROM BBF                     
         AP    ABLFRWD,OFFCLCR     ADD CREDITS TO BBF                           
         AP    ABLDR,OFFCLDR       ADD TO CURRENT DEBITS                        
         AP    ABLCR,OFFCLCR       AND CREDITS                                  
*                                                                               
UPOF14   ZAP   REPCDR,OFFCLDR      REPORT UN/CLOSED DEBITS                      
         ZAP   REPCCR,OFFCLCR      REPORT UN/CLOSED CREDITS                     
         ZAP   REPBCF,ABLFRWD      REPORT BAL B/F AFTER UPDATE                  
         BAS   RE,PRNTACC          PRINT THE DETAILS                            
         MVC   OFARCMOS,REQMON                                                  
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,ISDADA,ISDABF,ISDAWK                  
         ORG   *-2                                                              
         CLI   QOPT3,C'D'          TEST DRAFT MODE                              
         BE    *+6                 DON'T UPDATE                                 
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,ISDADR           UPDATE KEY                                   
         MVC   OFAKCMOS,REQMON                                                  
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,OFAKEY,OFAKEY                          
         ORG   *-2                                                              
         CLI   QOPT3,C'D'          TEST DRAFT MODE                              
         BE    *+6                 DON'T UPDATE                                 
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     UPOF04                                                           
*                                                                               
UPOFX    L     RF,ADBXAREA                                                      
         MVI   BOXREQ-BOXD(RF),C'B'  SET BOX BOTTOM                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R2,R3,R4,RF                                                      
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO FORMAT AND PRINT REPORT                      *                 
***************************************************************                 
         SPACE 1                                                                
PRNTACC  NTR1  ,                                                                
         MVC   HEAD3+15(L'REPCPY),REPCPY                                        
         MVC   HEAD4+15(L'REPLGR),REPLGR                                        
         CLI   QOPT2,C'T'          TEST TRANSACTION LEVEL REPORT                
         BE    PRAC4               YES - PRINT ACCOUNT TOTAL                    
*                                                                               
         L     R2,ADACC            ACCOUNT CODE                                 
         USING ACTRECD,R2                                                       
         MVC   P1ACC(L'ACTKACT),ACTKACT                                         
         MVC   P1ACC+L'P1ACC-1(L'ACCLOVR),ACCLOVR                               
         TM    PROCIND,BYOFFCQ     IF PROCESSING OFFICE ACCOUNTS                
         BNO   PRAC2                                                            
         MVC   P1OFF,REPOFF        PRINT OFFICE CODE                            
*                                                                               
PRAC2    L     R2,ADACCNAM         ACCOUNT NAME                                 
         USING NAMELD,R2                                                        
         SR    R3,R3                                                            
         IC    R3,NAMLN                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+4                                                           
         MVC   P1ACN(0),NAMEREC                                                 
         DROP  R2                                                               
*                                  FORMAT TOTALS                                
         LA    R3,P1BBF                                                         
         LA    R2,REPBBF           BALANCE B/F                                  
         BAS   R7,PREDIT                                                        
*                                                                               
         LA    R2,REPCDR           CLOSED DEBITS                                
         BAS   R7,PREDIT                                                        
*                                                                               
         LA    R2,REPCCR           CLOSED CREDITS                               
         BAS   R7,PREDIT                                                        
*                                                                               
         LA    R2,REPBCF           BALANCE C/F                                  
         BAS   R7,PREDIT                                                        
         B     PRAC6                                                            
*                                                                               
PRAC4    L     RF,ADBXAREA                                                      
         USING BOXD,RF                                                          
         MVC   SCOLS,BOXCOLS                                                    
         TM    PROCIND,BYOFFCQ                                                  
         BNO   *+8                                                              
         MVI   BOXCOLS+(P2X0-P2LIN),0                                           
         MVI   BOXCOLS+(P2X1-P2LIN),0                                           
         MVI   BOXCOLS+(P2X2-P2LIN),0                                           
         MVI   BOXCOLS+(P2X3-P2LIN),0                                           
         MVI   BOXCOLS+(P2X4-P2LIN),0                                           
         LA    R2,REPCDR                                                        
         LA    R3,P2DRS                                                         
         BAS   R7,PREDIT                                                        
         LA    R2,REPCCR                                                        
         BAS   R7,PREDIT                                                        
         L     RF,ADACC                                                         
         MVC   P2ACC,ACTKACT-ACTRECD(RF)                                        
         TM    PROCIND,BYOFFCQ                                                  
         BNO   *+10                                                             
         MVC   P2OFF,REPOFF                                                     
         MVC   P2CAC(L'AC@TACC),AC@TACC                                         
         GOTO1 ACREPORT                                                         
         L     RF,ADBXAREA                                                      
         MVI   BOXBLANK,C'N'                                                    
         MVC   BOXCOLS,SCOLS                                                    
         TM    PROCIND,BYOFFCQ                                                  
         BO    EXIT                                                             
         MVI   BOXREQ,C'B'                                                      
*                                                                               
PRAC6    GOTO1 ACREPORT                                                         
         B     EXIT                                                             
*                                                                               
PREDIT   CURED (P8,0(R2)),(13,0(R3)),2,MINUS=YES                                
         LA    R3,14(R3)           NEXT PRINT POSITION                          
         BR    R7                                                               
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO FORMAT AND PRINT TRANS LEVEL REPORT          *                 
***************************************************************                 
         SPACE 1                                                                
PRNTTRN  NTR1  ,                                                                
         MVC   HEAD3+15(L'REPCPY),REPCPY                                        
         MVC   HEAD4+15(L'REPLGR),REPLGR                                        
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         MVC   P2ACC(L'TRNKACT),TRNKACT                                         
         TM    PROCIND,BYOFFCQ                                                  
         BNO   PRNTT02                                                          
         MVC   P2OFF,TRNKOFF                                                    
PRNTT02  MVC   P2CAC(2+L'TRNKCACT),TRNKCUNT                                     
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(8,P2DAT)                               
         MVC   P2REF,TRNKREF                                                    
         XOUT  TRNKSBR,P2SUB,1                                                  
         L     R2,ADTRANS                                                       
         USING TRNELD,R2                                                        
         LA    R3,P2DRS                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    R3,P2CRS                                                         
         CURED (P6,TRNAMNT),(13,0(R3)),2,MINUS=YES                              
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
* SUB-ROUTINE TO ADD AN ENTRY TO THE BINSRCH TABLE          *                   
* AT ENTRY, P1=A(ENTRY TO BE ADDED)                         *                   
*           P2=A(BINSRCH PARAMETER BLOCK)                   *                   
*************************************************************                   
         SPACE 1                                                                
BINADD   NTR1  ,                                                                
         USING BIND,R5                                                          
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         L     R3,0(R1)            A(RECORD)                                    
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    BINAX               NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         AP    0(6,R4),0(6,R3)     ADD NEW TO OLD                               
         B     BINAX                                                            
*                                                                               
BINAX    B     EXIT                                                             
         SPACE 2                                                                
*************************************************************                   
* SUB-ROUTINE TO RETRIEVE AN ENTRY FROM THE BINSRCH TABLE   *                   
* AT ENTRY,P1=A(ENTRY KEY)                                  *                   
*          P2=A(BINSRCH PARAMETER BLOCK)                    *                   
*************************************************************                   
         SPACE 1                                                                
BINGET   NTR1  ,                                                                
         L     R2,0(R1)                                                         
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ                                                 
         LA    R3,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(2,(R2)),(R3)                                       
         B     EXIT                                                             
         EJECT                                                                  
DCLIST   DS    0X                                                               
         DCDDL AC#LGR,12,L                                                      
         DCDDL AC#UNIT,4,L                                                      
         DCDDL AC#REQ,11,L                                                      
         DCDDL AC#TLDG,36,L                                                     
         DCDDL AC#TACC,20,L                                                     
         DC    X'00'                                                            
         SPACE 1                                                                
DSLIST   DS    0X                                                               
         DSDDL PRINT=YES                                                        
         SPACE 1                                                                
LDGTAB   DS    0CL1                                                             
*&&UK*&& DC    C'VXFT'                                                          
*&&US*&& DC    C'PQSTUVWXY'                                                     
         DC    X'FF'                                                            
         SPACE 1                                                                
ABILBLOC DC    A(BILBLOC)                                                       
VIOBUFF  DC    V(IOBUFF)                                                        
VOFFTAB  DC    V(OFFTAB)                                                        
VCONVMOS DC    V(CONVMOS)                                                       
         SPACE 1                                                                
PZEROS   DC    P'0'                                                             
         SPACE 1                                                                
* DATAMGR LITERALS                                                              
*                                                                               
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC '                                                       
         EJECT                                                                  
* TABLE OF CLOSE OUT TYPES                                                      
*                                                                               
CLOSTAB  DS    0XL(CLOSTABL)                                                    
*                                                                               
         DC    AL1(LDGCBBF)                                                     
         DC    AL2(ACC-ACCO02),AL2(UNACC-ACCO02)                                
         DC    AL2(BBF-ACCO02),AL2(UNBBF-ACCO02)                                
         DC    AL2(ACCL-ACCO02),AL2(UNACCL-ACCO02)                              
*                                                                               
         DC    AL1(LDGCPBL)                                                     
         DC    AL2(ACC-ACCO02),AL2(UNACC-ACCO02)                                
         DC    AL2(PBL-ACCO02),AL2(UNPBL-ACCO02)                                
         DC    AL2(ACCL-ACCO02),AL2(UNACCL-ACCO02)                              
*                                                                               
         DC    AL1(LDGCZBF)                                                     
         DC    AL2(ACC-ACCO02),AL2(UNACC-ACCO02)                                
         DC    AL2(BBF-ACCO02),AL2(UNTRNS-ACCO02)                               
         DC    AL2(ACCL-ACCO02),AL2(UNACCL-ACCO02)                              
*                                                                               
         DC    AL1(LDGCRCV)                                                     
         DC    AL2(RCV-ACCO02),AL2(0)                                           
         DC    AL2(0),AL2(0)                                                    
         DC    AL2(ACCL-ACCO02),AL2(0)                                          
*                                                                               
         DC    AL1(LDGCPRD)                                                     
         DC    AL2(PLAC-ACCO02),AL2(UNPLAC-ACCO02)                              
         DC    AL2(0),AL2(0)                                                    
         DC    AL2(0),AL2(0)                                                    
*                                                                               
         DC    AL1(LDGCCSH)                                                     
         DC    AL2(ACC-ACCO02),AL2(UNACC-ACCO02)                                
         DC    AL2(CSH-ACCO02),AL2(UNTRNS-ACCO02)                               
         DC    AL2(ACCL-ACCO02),AL2(UNACCL-ACCO02)                              
*                                                                               
         DC    AL1(LDGCMBF)                                                     
         DC    AL2(ACC-ACCO02),AL2(UNACC-ACCO02)                                
         DC    AL2(MBF-ACCO02),AL2(UNMBF-ACCO02)                                
         DC    AL2(ACCL-ACCO02),AL2(UNACCL-ACCO02)                              
*                                                                               
NCLOSTYP EQU   (*-CLOSTAB)/L'CLOSTAB                                            
         SPACE 2                                                                
* TABLE OF DISPLACEMENTS INTO CLOSTAB FOR EACH MODE                             
*                                                                               
MODETAB  DS    0XL2                                                             
         DC    AL1(PROCACC),AL1(CLOSACC-CLOSTABD)                               
         DC    AL1(PROCTRNS),AL1(CLOSTRNS-CLOSTABD)                             
         DC    AL1(ACCLAST),AL1(CLOSACLS-CLOSTABD)                              
NMODES   EQU   (*-MODETAB)/L'MODETAB                                            
         SPACE 2                                                                
CLOSNAMS DS    0CL41                                                            
         DC    AL1(LDGCBBF)                                                     
         DC    CL40'(BALANCE BROUGHT FORWARD)'                                  
*                                                                               
         DC    AL1(LDGCPBL)                                                     
         DC    CL40'(PAYABLES - SEMI OPEN ITEM)'                                
*                                                                               
         DC    AL1(LDGCZBF)                                                     
         DC    CL40'(ZERO BALANCE BROUGHT FORWARD)'                             
*                                                                               
         DC    AL1(LDGCRCV)                                                     
         DC    CL40'(RECEIVABLES - OPEN ITEM)'                                  
*                                                                               
         DC    AL1(LDGCPRD)                                                     
         DC    CL40'(PRODUCTION - OPEN ITEM)'                                   
*                                                                               
         DC    AL1(LDGCCSH)                                                     
         DC    CL40'(BALANCE BROUGHT FORWARD)'                                  
*                                                                               
         DC    AL1(LDGCMBF)                                                     
         DC    CL40'(MATCHED BALANCE FORWARD)'                                  
*                                                                               
         DC    AL1(0)              EOT                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* DSECT TO COVER WORKING STORAGE                                                
*                                                                               
WORKD    DSECT                                                                  
AOFACC   DS    A                   A(OFFICE ACCOUNT RECORD)                     
ATRNREC  DS    A                   A(TRANSACTION RECORD)                        
AOFFTNT  DS    A                   A(CURRENT OFFTAB ENTRY)                      
ADPEEL   DS    A                   A(ACCOUNT RECORD PEEL-OFF ELEMENT)           
SAVERE   DS    A                                                                
PROCIND  DS    X                   PROCESSING INDICATOR                         
NEWOFFQ  EQU   X'80'               COMPANY IS ON NEW OFFICES                    
NOTPRDQ  EQU   X'40'               LEDGER IS NOT PRODUCTION TYPE                
BYOFFCQ  EQU   NEWOFFQ+NOTPRDQ     CLOSE OUT BY OFFICE A/C, NOT ACCOUNT         
CLOSTYPE DS    X                   CLOSE OUT TYPE                               
LDGLCLO  DS    X                   CLOSE OUT TYPE FROM LEDGER                   
ACCLOVR  DS    C                   '*'=LDGLCLO OVERRIDDEN ON THIS A/C           
PLCLOSE  DS    C                   Y/N=OK TO CLOSE PRODUCTION JOB               
PASS     DS    X                   1/2=READING RCV TRANSACTIONS TWICE           
RDSTS    DS    X                   X'80'=READ FOR UPDATE                        
SAVEACC  DS    CL(OFAKEND)                                                      
TRIOA    DS    CL255                                                            
TRIOB    DS    CL255                                                            
SCOLS    DS    XL(L'BOXCOLS)                                                    
REQMON   DS    PL2                 PWOS REQUEST MONTH                           
PTRNMOS  DS    PL2                 TRANS MOS FROM CONVMOS                       
FSTPOST  DS    PL2                 FIRST POSTED MOS                             
LATCLOS  DS    PL2                 LATEST CLOSED MOS                            
TODAY    DS    PL3                 TODAY'S DATE                                 
NEWCLOS  DS    PL2                 NEW CLOSE OUT MONTH OF UNCLOSE               
REPCPY   DS    CL40                CPY CODE + NAME FOR REPORT                   
REPLGR   DS    CL80                LGR  "      "   + CLOSE TYPE                 
*                                                                               
ACCUMS   DS    0PL8                                                             
ACCDR    DS    PL(L'ACCUMS)        ACCOUNT DEBITS                               
ACCCR    DS    PL(L'ACCUMS)        ACCOUNT CREDITS                              
LEDGDR   DS    PL(L'ACCUMS)        LEDGER DEBITS                                
LEDGCR   DS    PL(L'ACCUMS)        LEDGER CREDITS                               
UNITDR   DS    PL(L'ACCUMS)        UNIT DEBITS                                  
UNITCR   DS    PL(L'ACCUMS)        UNIT CREDITS                                 
NACCUMS  EQU   (*-ACCUMS)/(L'ACCUMS*2)  N'ACCUMULATOR PAIRS                     
*                                                                               
REPBBF   DS    PL(L'ACCUMS)        BAL B/F BEFORE UPDATE                        
REPCDR   DS    PL(L'ACCUMS)        CLOSED DEBITS                                
REPCCR   DS    PL(L'ACCUMS)        CLOSED CREDITS                               
REPBCF   DS    PL(L'ACCUMS)        BAL B/F AFTER UPDATE                         
REPOFF   DS    CL2                 OFFICE CODE FOR REPORT                       
*                                                                               
DATE3    DS    CL3                                                              
ENDAY    DS    CL3                                                              
TODAY3   DS    CL3                                                              
TODAY2   DS    CL2                                                              
END2     DS    XL2                                                              
END3     DS    PL3                                                              
ELCODE   DS    CL1                                                              
TEMP     DS    CL60                                                             
LANG     DS    XL1                                                              
CTRY     DS    XL1                                                              
WORKX    DS    0X                                                               
         EJECT                                                                  
CLOSTABD DSECT                                                                  
CLOSTYP  DS    X                   CLOSE OUT TYPE                               
CLOSACC  DS    XL4                 DISP TO PROCACC ROUTINES                     
CLOSTRNS DS    XL4                 TRANSACTIONS                                 
CLOSACLS DS    XL4                 LAST FOR ACCOUNT                             
CLOSTABL EQU   *-CLOSTABD                                                       
         SPACE 2                                                                
OFFTABD  DSECT                                                                  
OFFTOFF  DS    CL(L'OFAKOFF)                                                    
OFFCMOS  DS    PL(L'OFARCMOS)                                                   
OFFLMOS  DS    PL(L'OFARLMOS)                                                   
OFFCLDR  DS    PL8                 CLOSED/UNCLOSED DEBITS                       
OFFCLCR  DS    PL8                 CLOSED/UNCLOSED CREDITS                      
OFFTABL  EQU   *-OFFTABD                                                        
OFFTMAX  EQU   1024                MAXIMUM NUMBER OF TABLE ENTRIES              
         SPACE 2                                                                
BILD     DSECT                                                                  
BILCON   DS    CL12                CONTRA ACCOUNT                               
BILDTE   DS    CL3                 DATE                                         
BILREF   DS    CL6                 REFERENCE                                    
BILKLEN  EQU   *-BILD                                                           
BILBK    EQU   *                                                                
BILAMT   DS    PL6                 AMOUNT                                       
BILBKCNT EQU   (*-BILBK)/6                                                      
BILEN    EQU   *-BILD                                                           
BILMAX   EQU   20000                                                            
         SPACE 2                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTAT  DS    CL1                 X'80' BINARY DATA                            
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         SPACE 2                                                                
ISDAD    DSECT                                                                  
ISDADA   DS    XL4                 DISK ADDRESS                                 
ISDADR   DS    XL(ACCKLEN)         RECEIVE AREA FOR DIRECTORY REC               
ISDAWK   DS    8D                  DATAMGR WORK AREA                            
ISDABF   DS    2048X               BUFFER AREA FOR DATA REC                     
ISDADL   EQU   *-ISDAD                                                          
         SPACE 2                                                                
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
         PRINT ON                                                               
* ACLANGEQU                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACLANGEQU                                                      
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* DDEBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         SPACE 2                                                                
ACWORKD  DSECT                                                                  
         ORG   P                                                                
P1LIN    DS    0CL(L'P)            ACC LEVEL PRINT LINE (STANDARD)              
P1XL     DS    XL1                                                              
P1ACC    DS    CL17                                                             
P1X1     DS    XL1                                                              
P1ACN    DS    CL36                                                             
P1X0     DS    XL1                 EXTRA COLUMN IF ON OFFICES                   
P1OFF    DS    CL2                                                              
P1X2     DS    XL1                                                              
P1BBF    DS    CL13                                                             
P1X3     DS    XL1                                                              
P1DRS    DS    CL13                                                             
P1X4     DS    XL1                                                              
P1CRS    DS    CL13                                                             
P1X5     DS    XL1                                                              
P1BCF    DS    CL13                                                             
P1XR     DS    XL1                                                              
         ORG   P                                                                
P2LIN    DS    0CL(L'P)            TRANS LEVEL PRINT LINE (SPECIAL)             
P2XL     DS    XL1                                                              
P2ACC    DS    CL16                                                             
P2X0     DS    XL1                                                              
P2OFF    DS    CL2                                                              
P2X1     DS    XL1                                                              
P2CAC    DS    CL16                                                             
P2TOTL   EQU   *-P2XL                                                           
         ORG   P2ACC                                                            
P2TLI    DS    CL20                                                             
         DS    CL1                                                              
P2TAC    DS    CL12                                                             
         DS    CL1                                                              
P2TOF    DS    CL2                                                              
         ORG   P2ACC+P2TOTL                                                     
P2X2     DS    XL1                                                              
P2DAT    DS    CL8                                                              
P2X3     DS    XL1                                                              
P2REF    DS    CL6                                                              
P2X4     DS    XL1                                                              
P2SUB    DS    CL2                                                              
P2X5     DS    XL1                                                              
P2DRS    DS    CL13                                                             
P2X6     DS    XL1                                                              
P2CRS    DS    CL13                                                             
P2XR     DS    XL1                                                              
         ORG                                                                    
         EJECT                                                                  
BILBLOC  CSECT                                                                  
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(BILEN)          RECORD LENGTH                                
         DC    AL4(BILKLEN)        DISP. TO KEY / KEY LENGTH                    
         DC    AL4(BILMAX)         MAX. NUMBER OF RECORDS                       
         DC    AL1(BILBKCNT)       NUMBER OF BUCKETS                            
         DC    AL1(BILBK-BILD)     DISP TO BUCKETS                              
         DC    AL2(0)                                                           
         DS    (BILMAX*BILEN)C     TABLE                                        
         SPACE 2                                                                
IOBUFF   CSECT                                                                  
         DS    (ISDADL)X                                                        
OFFTAB   CSECT                                                                  
         DS    (OFFTMAX*OFFTABL)X                                               
         DC    AL1(255)                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACREPCO02 08/16/00'                                      
         END                                                                    
