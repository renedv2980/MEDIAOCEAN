*          DATA SET ACREPI702  AT LEVEL 008 AS OF 05/01/02                      
*PHASE ACI702A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'ACI702 - GL FEED AND PROD TAPES - OGILVY'                       
ACI702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACI7**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACI7D,RC                                                         
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINES EXECUTED AT FIRST FOR RUN                               
*--------------------------------------------------------------------*          
         CLI   MODE,RUNFRST                                                     
         BNE   REQF100                                                          
         RELOC RELO                                                             
         ZAP   COUNT,=P'0'           INIT # OF TAPES REQUESTED                  
         L     RF,=A(BUFFALOC)                                                  
         A     RF,RELO                                                          
         ST    RF,ABUFF                                                         
         L     RF,=A(CSTAB)                                                     
         A     RF,RELO                                                          
         ST    RF,ACSTAB                                                        
         L     RF,=A(ACBUFF)                                                    
         A     RF,RELO                                                          
         ST    RF,ACCBUFF                                                       
         L     RF,=V(PRNTBL)                                                    
         A     RF,RELO                                                          
         ST    RF,PRNTBL                                                        
         L     RF,GETOPT             NOOP GETOPT (BCR OPCODE)                   
         MVC   0(2,RF),=X'07FE'                                                 
*                                                                               
         BAS   RE,BLDCOST          BUILD THE COST TABLE                         
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
*--------------------------------------------------------------------*          
*              ROUTINES EXECUTED AT FIRST FOR REQUEST                           
*--------------------------------------------------------------------*          
REQF100  CLI   MODE,REQFRST                                                     
         BNE   LEDG100                                                          
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'Y'                 Y:SIGNALS ACTUAL CREATION OF          
         BNE   REQF120                      TAPE, ELSE IS JUST A DRAFT          
         AP    COUNT,=P'1'                COUNT USED LATER FOR TAPE             
         CVB   R4,COUNT                     REC COUNT                           
         MVC   DSNAME+13(2),ALPHAID                                             
         GOTO1 DYNALLOC,DMCB,(0,=CL8'TAPE'),((R4),DSNAME)                       
         OPEN  (TAPE,OUTPUT)                                                    
         ZAP   COUNT,=P'0'                  RESET TO ZERO                       
*                                                                               
REQF120  DS    0H                                                               
         LH    R5,COMPNUM                 MAX POSSIBLE CODES                    
         LA    R6,COMPTAB                 TABLE OF OGILVY'S CMPNY CODES         
         L     R4,ADCOMP                  ADDRESS OF COMPANY REC                
         USING COMPTABD,R6                                                      
REQF122  CLC   0(1,R4),CMPDDS                                                   
         BE    REQF125                                                          
         LA    R6,L'COMPNTR(R6)                                                 
         BCT   R5,REQF122                                                       
REQF125  MVC   RQCOMP,CMPOGL              COMPANY TO BUFFREC                    
*                                                                               
*              CHANGE QSTART AND QEND TO PACKED UNSIGNED AND COMPRESSED         
         CLC   QSTART,SPACES                                                    
         BE    REQF140                                                          
         GOTO1 DATCON,DMCB,(0,QSTART),(2,QSTRCOMP)                              
         GOTO1 (RF),(R1),,(1,QSTPK)                                             
         CLC   QEND,SPACES                                                      
         BNE   REQF130                                                          
         MVC   QENDCOMP,QSTRCOMP            IF QEND=BLANK, QEND=QSTART          
         MVC   QENDPK,QSTPK                                                     
         B     XIT                                                              
REQF130  GOTO1 DATCON,DMCB,(0,QEND),(2,QENDCOMP)                                
         GOTO1 (RF),(R1),,(1,QENDPK)                                            
         B     XIT                                                              
*                                                                               
*              IF QSTART IS BLANK, SET QSTART AND QEND TO TODAY'S DATE          
REQF140  GOTO1 DATCON,DMCB,(5,QSTART),(2,QSTRCOMP)                              
         GOTO1 (RF),(R1),,(1,QSTPK)                                             
         MVC   QENDCOMP,QSTRCOMP                                                
         MVC   QENDPK,QSTPK                                                     
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        NEED LEVEL A LENGTH FOR PROCESSING SR                                  
*--------------------------------------------------------------------*          
LEDG100  CLI   MODE,LEDGFRST         WHEN PROCESSING SR AM SENDING              
         BNE   SBAC100               OUT ONLY LEVEL A LENGTH OF                 
         L     R6,ADLDGHIR           ACCOUNT                                    
         USING ACHEIRD,R6                                                       
         MVC   SRLEVA,ACHRLEVA                                                  
         B     XIT                                                              
         SPACE 2                                                                
*--------------------------------------------------------------------*          
*        NEED TO READ NUM2 COST CODE FOR CERTAIN LEDGERS                        
*--------------------------------------------------------------------*          
SBAC100  CLI   MODE,SBACFRST              GET COST CODE OFF SJ                  
         BNE   PRCT100                    FOR THOSE U/L THAT                    
         USING TRANSD,R2                  REQUIRE IT                            
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
*                                                                               
         CLC   ACKEYACC+1(2),=C'SJ'          COST CODE NEEDED FOR               
         BE    SBAC120                       SJ, SI, SE                         
         CLC   ACKEYACC+1(2),=C'SI'                                             
         BNE   SBAC140                                                          
SBAC120  BAS   RE,CSTCODE                    READ COSTCODE                      
SBAC140  B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ALWAYS PROCESSING ONLY UNIT S - GOTO APPROPRIATE ROUTINE               
*        FOR CORRESPONDING LEDGER                                               
*--------------------------------------------------------------------*          
PRCT100  CLI   MODE,PROCTRNS                                                    
         BNE   REQL100                                                          
*                                                                               
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
*                                                                               
         BAS   RE,PRODINEX              DETERMINE IF TRANSACTION IS             
         TM    INCLSW,X'01'             TO BE PROCESSED                         
         BNO   XIT                                                              
         BAS   RE,COMMRTN               SET INFO THAT IS COMMON TO ALL          
*                                       JOURNAL TYPES                           
*              PROCESS EACH LEDGER                                              
         USING LEDGRTND,R5              TABLE OF LEDGERS AND CORRESPND          
         LA    R5,LEDGRTN               ROUTINE ADDRESSES                       
PRCT200  CLI   LEDGCHAR,X'FF'                                                   
         BE    XIT                                                              
         CLC   LEDGCHAR,ACKEYACC+1                                              
         BE    PRCT220                                                          
         LA    R5,L'LEDGLEN(R5)                                                 
         B     PRCT200                                                          
*                                                                               
PRCT220  DS    0H                                                               
         MVC   FULL,LEDGADDR            EXECUTE ROUTINE THAT                    
         L     RF,FULL                  CORRESPONDS TO THIS LEDGER              
***      ICM   RF,15,LEDGADDR           EXECUTE ROUTINE THAT                    
         BAS   RE,0(RF)                 CORRESPONDS TO THIS LEDGER              
PRCT300  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ROUTINES EXECUTED AT LAST FOR REQUEST                            
*--------------------------------------------------------------------*          
REQL100  CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
*                                                                               
         XC    BUFAREA,BUFAREA                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'01',ABUFF),BUFAREA,1                    
         B     REQL210                                                          
REQL200  GOTO1 BUFFALO,DMCB,=C'SEQ',(X'01',ABUFF),BUFAREA,1                     
REQL210  CLI   BUFAREA,X'01'              ONLY WANT TYPE 01                     
         BNE   REQL500                                                          
         TM    DMCB+8,X'80'                                                     
         BO    REQL500                                                          
*                                                                               
         MVC   TAPEAREA,SPACES            CLEAR TAPE REC AREA                   
         UNPK  TPAMT1,BFAMT1              CHANGE AMT TO TAPE FORMAT             
         MVC   TPKEY,BFKEY+1                                                    
         MVC   TPCLNT,BFKCLT                                                    
         MVC   TPPROD,BFKPRD                                                    
         MVC   TPNAME,BFNAME                                                    
         MVC   TPCOSTCD,BFCOSTCD                                                
         MVC   TPDEPTCD,BFDEPTCD                                                
         MVC   TPYYMM,BFYYMM                                                    
*                                                                               
         CLI   QOPT1,C'Y'                 AM I CREATING TAPE?                   
         BNE   REQL200                                                          
         PUT   TAPE,TAPEAREA              PUT REC TO TAPE                       
         B     REQL200                                                          
*                                                                               
*              NOW THAT ALL RECORDS HAVE BEEN PUT TO TAPE AND ALL               
*              SUMMARY RECS PUT TO BUFFALO - PRINT ALL SUMMARY REPORTS          
REQL500  DS    0H                                                               
*        PRINT ALL GENERAL LEDGER SUMMARIES                                     
         MVI   FORCEHED,C'Y'         J-TYPE/UL/ACCT/OFFICE                      
         MVI   SUMTP,SUMTYP1         GENERAL LEDGER FEED                        
         MVI   RCSUBPRG,1                                                       
         BAS   RE,SUMONE                                                        
*                                                                               
         MVI   FORCEHED,C'Y'         OFFICE/J-TYPE/ACCT/COSTCODE                
         MVI   SUMTP,SUMTYP2         GENERAL LEDGER FEED                        
         MVI   RCSUBPRG,2                                                       
         BAS   RE,SUMTWO                                                        
*                                                                               
         MVI   FORCEHED,C'Y'         UL/ACCT/ACCT NAME                          
         MVI   SUMTP,SUMTYP3         GENERAL LEDGER FEED                        
         MVI   RCSUBPRG,3                                                       
         BAS   RE,SUMTHREE                                                      
*                                                                               
*        PRINT ALL PRODUCTION SUMMARIES                                         
         MVI   FORCEHED,C'Y'         J-TYPE/UL/ACCT/OFFICE                      
         MVI   SUMTP,SUMTYP5         PRODUCTION FEED                            
         MVI   RCSUBPRG,5                                                       
         BAS   RE,SUMONE                                                        
*                                                                               
         MVI   FORCEHED,C'Y'         OFFICE/J-TYPE/UL/ACCT/DEPT/                
         MVI   SUMTP,SUMTYP4         COSTCODE                                   
         MVI   RCSUBPRG,4            PRODUCTION FEED                            
         BAS   RE,SUMFOUR                                                       
*                                                                               
         MVI   FORCEHED,C'Y'         UL/ACCT/ACCT NAME                          
         MVI   SUMTP,SUMTYP6         GENERAL LEDGER FEED                        
         MVI   RCSUBPRG,6                                                       
         BAS   RE,SUMTHREE                                                      
*                                                                               
*        PRINT ALL PAYABLES SUMMARIES                                           
         MVI   FORCEHED,C'Y'         J-TYPE/UL/ACCT/OFFICE                      
         MVI   SUMTP,SUMTYP8         PAYABLE FEED                               
         MVI   RCSUBPRG,8                                                       
         BAS   RE,SUMONE                                                        
*                                                                               
         MVI   FORCEHED,C'Y'         OFFICE/J-TYPE/ACCT/COSTCODE                
         MVI   SUMTP,SUMTYP9         GENERAL LEDGER FEED                        
         MVI   RCSUBPRG,9                                                       
         BAS   RE,SUMTWO                                                        
*                                                                               
         MVI   FORCEHED,C'Y'         UL/ACCT/ACCT NAME                          
         MVI   SUMTP,SUMTYP10        GENERAL LEDGER FEED                        
         MVI   RCSUBPRG,10                                                      
         BAS   RE,SUMTHREE                                                      
*                                                                               
         CLI   QOPT7,C'Y'                                                       
         BNE   REQLXIT                                                          
         MVI   FORCEHED,C'Y'         UNIT/LEDGER                                
         MVI   SUMTP,SUMTYP7                                                    
         MVI   RCSUBPRG,7                                                       
         BAS   RE,SUMSEVEN                                                      
*                                                                               
REQLXIT  DS    0H                                                               
         CLI   QOPT1,C'Y'            IF TAPE WAS REQUESTED                      
         BNE   XIT                   CLOSE TAPE                                 
         CLOSE (TAPE)                                                           
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER R                                                   
*--------------------------------------------------------------------*          
PROCSR   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
         CLI   ACKEYACC+3,C'S'           FOR ACCTS THAT BEGIN WITH              
         BNE   PRSR120                   AN 'S', THE ENTIRE ACCT CODE           
         MVC   BFACCT,ACKEYACC+3         IS REQUIRED, ELSE IT IS JUST           
         L     R6,ADACCNAM               LEVEL A LENGTH OF ACCT CODE            
         B     PRSR150                                                          
*                                                                               
PRSR120  ZIC   R6,SRLEVA                                                        
         BCTR  R6,0                                                             
         EXMVC R6,BFACCT,ACKEYACC+3                                             
         L     R6,ADLVANAM                                                      
         USING ACNAMED,R6                                                       
PRSR150  ZIC   R5,ACNMLEN                NAME FROM ACCT LEVEL I AM              
         SH    R5,=H'03'                 USING                                  
         EXMVC R5,BFNAME,ACNMNAME                                               
*                                                                               
PRSR180  MVC   BFJTYPE,=C'SRD'           DEBITS                                 
         MVI   BFATYPE,C'D'                                                     
         TM    TRNSSTAT,X'80'            OR                                     
         BO    PRSR200                                                          
         MVC   BFJTYPE,=C'SRC'           CREDITS                                
         MVI   BFATYPE,C'C'                                                     
*                                                                               
PRSR200  DS    0H                                                               
         BAS   RE,BUFPUT                 BUILD ALL BUFFALO RECS                 
         BAS   RE,ULPUT                  SEND ONE REC FOR UNIT/LEDG             
         B     XIT                       SUMMARY                                
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER C                                                   
*--------------------------------------------------------------------*          
PROCSC   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
*                                                                               
         TM    INCLSW,X'10'                TEST PAYABLE SWITCH                  
         BO    PRSC120                                                          
         MVI   BFATYPE,C'D'                TEST                                 
         MVC   BFJTYPE,=C'SCD'             FOR                                  
         TM    TRNSSTAT,X'80'              DEBIT                                
         BO    PRSC150                     OR                                   
         MVI   BFATYPE,C'C'                CREDIT                               
         MVC   BFJTYPE,=C'SCC'                                                  
         B     PRSC150                                                          
*                                                                               
PRSC120  MVI   BFATYPE,C'C'                PROCESS PAYABLES                     
         MVC   BFJTYPE,=C'SCP'                                                  
*                                                                               
PRSC150  MVC   BFACCT,ACKEYACC+3                                                
         L     R6,ADACCNAM                                                      
         USING ACNAMED,R6                                                       
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'03'                                                        
         EXMVC R5,BFNAME,ACNMNAME                                               
         BAS   RE,BUFPUT                 BUILD ALL BUFFALO RECS                 
         BAS   RE,ULPUT                  SEND ONE REC FOR UNIT/LEDG             
         B     XIT                       SUMMARY                                
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER E                                                   
*        STILL NEED DEPT FROM 2/D, NUM2 CST CODE, CLIENT, PRODUCT               
*--------------------------------------------------------------------*          
PROCSE   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
*                                                                               
         MVI   BFATYPE,C'D'                                                     
         MVC   BFJTYPE,=C'SED'                                                  
         TM    TRNSSTAT,X'80'              DEBIT                                
         BO    PRSE150                     OR                                   
         MVI   BFATYPE,C'C'                CREDIT                               
         MVC   BFJTYPE,=C'SEC'                                                  
*                                                                               
PRSE150  MVC   BFACCT,ACKEYACC+3                                                
         L     R6,ADACCNAM                 ACCT NAME                            
         USING ACNAMED,R6                                                       
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'03'                                                        
         EXMVC R5,BFNAME,ACNMNAME                                               
*                                                                               
         USING TRANSD,R2                 GET NUM2 COSTCODE FROM                 
         L     R2,ADTRANS                TRANSACTION NARRATIVE                  
         CLC   TRNSNARR(5),=C'NUM2='                                            
         BNE   PRSE160                                                          
         MVC   BFCOSTCD,TRNSNARR+5                                              
*                                                                               
PRSE160  L     R3,ADTRANS                                                       
         MVI   ELCODE,X'C0'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PRSE500                                                          
         ZIC   R5,2(R3)                  NUMBER OF MINI ELEMENTS                
         LA    R3,3(R3)                  POINT TO MINI ELEMENTS                 
PRSE170  ZIC   R6,0(R3)                  NUMBER OF MINI ELEMENTS                
         CLC   2(2,R3),=C'2D'            FIND 2D POSTING                        
         BE    PRSE180                                                          
         AR    R3,R6                     BUMP POINTER                           
         BCT   R5,PRSE170                                                       
PRSE180  MVC   BFDEPTCD,6(R3)                                                   
*                                                                               
PRSE500  BAS   RE,BUFPUT                 BUILD ALL BUFFALO RECS                 
         BAS   RE,ULPUT                  SEND ONE REC FOR UNIT/LEDG             
         B     XIT                       SUMMARY                                
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER I                                                   
*--------------------------------------------------------------------*          
PROCSI   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
*                                         COMMON TO ALL JOURNAL TYPES           
         MVC   BFACCT,ACKEYACC+3          ACCOUNT                               
         MVC   BFCOSTCD,COSTCODE          COST CODE                             
         CLC   BFCOSTCD,SPACES                                                  
         BNE   PRSI110                                                          
         MVC   BFKCLT,ACKEYCON+3          CLIENT-SI(ONLY) USES CLT,PRD          
         MVC   BFKPRD,ACKEYCON+6          PRODUCT               IN KEY          
PRSI110  L     R6,ADLVCNAM                ACCOUNT NAME                          
         USING ACNAMED,R6                                                       
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'3'                                                         
         EXMVC R5,BFNAME,ACNMNAME                                               
*                                           IF ACCT BEGINS WITH:                
         CLC   ACKEYACC+3(3),=C'PRI'        'PRI'                               
         BNE   PRSI115                                                          
         MVC   BFJTYPE,=C'PRI'                                                  
         B     PRSI125                                                          
PRSI115  CLC   ACKEYACC+3(3),=C'SFR'        'SFR'                               
         BNE   PRSI120                                                          
         MVC   BFJTYPE,=C'SFR'                                                  
         B     PRSI125                                                          
PRSI120  CLC   ACKEYACC+3(3),=C'SFF'        'SFF'                               
         BNE   PRSI140                                                          
         MVC   BFJTYPE,=C'SFF'                                                  
PRSI125  ZAP   BFAMT1,TRNSAMNT              TRANSACTION AMOUNT                  
         MVI   BFATYPE,C'C'                 MARK AS CREDIT                      
         B     PRSI900                      PUT TO BUFFALO                      
*                                                                               
         USING ACSTATD,R3                                                       
PRSI140  L     R3,ADACCSTA                  CHECK FILTER 2                      
         CLI   ACSTFILT+1,C'I'                                                  
         BE    PRSI150                                                          
         CLI   ACSTFILT+1,C'R'                                                  
         BE    PRSI200                                                          
         CLI   ACSTFILT+1,C'F'                                                  
         BE    PRSI300                                                          
*                                                                               
         MVC   BFJTYPE,=C'SIX'              ANYTHING NOT FITTIN INTO            
         MVI   BFATYPE,C'D'                 THE ABOVE CATEGORIES WILL           
         TM    TRNSSTAT,X'80'               BE LUMPED INTO JOURNAL              
         BO    *+8                          TYPE 'SIX'                          
         MVI   BFATYPE,C'C'                 MARKED AS A CREDIT                  
         ZAP   BFAMT1,TRNSAMNT                                                  
         B     PRSI900                                                          
*                                                                               
PRSI150  MVC   BFJTYPE,=C'IC '           ACCTS WITH F2=I:                       
         MVI   BFATYPE,C'D'              PUT OUT JOURNAL TYPE=IC                
         ZAP   BFAMT1,=P'0'              AS A CREDIT -                          
         L     R3,ADTRANS                AMOUNT WILL BE EQUAL TO                
         MVI   ELCODE,X'50'              GROSS BILLING AMT MINUS                
PRSI170  BAS   RE,NEXTEL                 TRNS AMNT (NET AMT)                    
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRCASHD,R3                                                       
         CLI   TRCSTYPE,C'G'                                                    
         BNE   PRSI170                                                          
         ZAP   BFAMT1,TRCSAMNT                                                  
         SP    BFAMT1,TRNSAMNT                                                  
         CLI   QOPT7,C'Y'          NET OPTION                                   
         BNE   *+10                                                             
         ZAP   BFAMT1,=P'0'                                                     
         BAS   RE,BUFPUT                                                        
*                                                                               
         MVC   BFJTYPE,=C'IB '           AND PUT OUT JOURNAL TYPE=IB            
         MVI   BFATYPE,C'C'              AS A DEBIT -                           
         ZAP   BFAMT1,TRCSAMNT           AMT WIL BE EQUAL TO GROSS              
         CLI   QOPT7,C'Y'          NET OPTION                                   
         BNE   *+10                                                             
         ZAP   BFAMT1,TRNSAMNT                                                  
         B     PRSI900                   BILLING AMT                            
*                                                                               
PRSI200  MVC   BFJTYPE,=C'IR '           ACCTS WITH F2=R:                       
         MVI   BFATYPE,C'D'              TEST FOR                               
         TM    TRNSSTAT,X'80'            DEBIT                                  
         BO    *+8                       OR                                     
         MVI   BFATYPE,C'C'              CREDIT                                 
         ZAP   BFAMT1,TRNSAMNT           USE TRANSACTION AMT                    
         B     PRSI900                                                          
*                                                                               
PRSI300  MVC   BFJTYPE,=C'BFS'           ACCTS WITH F2=F:                       
         MVI   BFATYPE,C'C'              MARK AS CREDIT                         
         ZAP   INTERNAL,=P'0'                                                   
*                                                                               
         CLC   ACKEYACC+3(3),=C'PRC'                                            
         BNE   PRSI315                                                          
         L     R3,ADTRANS                                                       
         MVI   ELCODE,X'1A'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PRSI315                                                          
         USING ACMTD,R3                                                         
         ICM   R0,15,ACMTINTL                                                   
         CVD   R0,WORK                                                          
         ZAP   INTERNAL,WORK(8)                                                 
         MP    INTERNAL,=P'-1'                                                  
***      BAS   RE,DMPGET                                                        
*                                                                               
PRSI315  ZAP   BFAMT1,TRNSAMNT                                                  
         L     R3,ADTRANS                                                       
         MVI   ELCODE,X'50'                                                     
PRSI320  BAS   RE,NEXTEL                                                        
         BNE   PRSI350                                                          
         USING TRCASHD,R3                                                       
         CLI   TRCSTYPE,C'G'                                                    
         BNE   PRSI320                                                          
         ZAP   BFAMT1,TRCSAMNT           USE GROSS BILLING AMT                  
         CLI   QOPT7,C'Y'                NET OPTION                             
         BNE   *+10                                                             
         ZAP   BFAMT1,TRNSAMNT                                                  
*                                                                               
PRSI350  AP    BFAMT1,INTERNAL           ADJUST FOR INTERNAL INCOME             
         BAS   RE,BUFPUT                 PUT TO BUFFALO                         
*                                                                               
         MVC   BFJTYPE,=C'CFS'           ALSO PUT OUT A DEBIT WITH              
         MVI   BFATYPE,C'D'              NET AMNT                               
         L     R2,ADTRANS                                                       
         USING TRANSD,R2                                                        
         SP    BFAMT1,TRNSAMNT           GROSS BILLING - TRNS AMT               
         CLI   QOPT7,C'Y'                NET OPTION                             
         BNE   *+10                                                             
         ZAP   BFAMT1,=P'0'                                                     
*                                                                               
PRSI900  DS    0H                                                               
         BAS   RE,BUFPUT                 BUILD ALL BUFFALO RECS                 
         BAS   RE,ULPUT                  SEND ONE REC FOR UNIT/LEDG             
PRSI950  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER J                                                   
*        GENERAL LEDGER FEED PLUS PRODUCTION FEED                               
*--------------------------------------------------------------------*          
PROCSJ   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
         MVI   SENTSJ,C'N'              INITIALIZE (FOR U/L SUMMARY)            
         ZAP   BFAMT1,TRNSAMNT          ALWAYS USE TRNS AMNT                    
         MVC   CONTRAUL,ACKEYCON+1                                              
*                                                                               
         MVI   BFATYPE,C'D'             TEST FOR                                
         TM    TRNSSTAT,X'80'           DEBIT                                   
         BO    *+8                      OR                                      
         MVI   BFATYPE,C'C'             CREDIT                                  
         CLC   ACKEYWRK,=C'99'          IF NOT WORKCODE=99                      
         BNE   PRSJ050                  (PRODUCTION BILLING) THEN               
         MVC   BFJTYPE,=C'SJ '                  PROCESS AS A PART               
         MVC   BFACCT,=CL12'161010      '       OF PRODUCTION                   
         MVI   SENTSJ,C'Y'                      JOURNAL TYPES                   
         B     PRSJ900                                                          
*                                                                               
*              ALL OF THE FOLLOWING ARE PRODUCTION ACCTS                        
PRSJ050  DS    0H                                                               
         L     R3,ADHEIRB                                                       
         BAS   RE,CSTCODE                  GET NUM2 COST CODE                   
*                                     CHECK FOR THESE CONTRA ACCOUNTS:          
         CLC   ACKEYCON+1(2),=C'SV'                                             
         BE    PRSJ100                                                          
         CLC   ACKEYCON+1(2),=C'SC'                                             
         BE    PRSJ120                                                          
         CLC   ACKEYCON+1(2),=C'SE'                                             
         BE    PRSJ140                                                          
         CLC   ACKEYCON+1(2),=C'SR'                                             
         BE    PRSJ160                                                          
         CLC   ACKEYCON+1(2),=C'SB'                                             
         BE    PRSJ160                                                          
         CLC   ACKEYCON+1(2),=C'SK'                                             
         BE    PRSJ160                                                          
         CLC   ACKEYCON+1(2),=C'SI'                                             
         BE    PRSJ160                                                          
*                                                                               
         MVC   P(L'FORGOT),FORGOT           I SHOULD IN THEORY BE               
         MVC   P+L'FORGOT+2(42),ACKEYACC    PRCSSNG EVERY TRNS I'M              
         GOTO1 ACREPORT                     PASSED, IF NOT PRINT IT             
         B     PRSJ260                      OUT                                 
*                                                                               
*              CONTRA ACCT U/L = SV                                             
PRSJ100  MVC   BFJTYPE,=C'VP '                                                  
         MVC   BFACCT,=CL12'327010      '                                       
         B     PRSJ240                                                          
*                                                                               
*              CONTRA ACCT U/L = SC AND ACCT = 324CCSV                          
PRSJ120  CLC   ACKEYCON+3(7),=C'324CCSV'   CASH RECEIPTS                        
         BNE   PRSJ160                                                          
         MVC   BFJTYPE,=C'CR '                                                  
         MVC   BFACCT,=CL12'324CCSV     '                                       
         B     PRSJ240                                                          
*                                                                               
*              PROCESS CONTRA U/L = SE                                          
         USING TRSDESCD,R3                                                      
PRSJ140  MVC   BFJTYPE,=C'WO '                                                  
         MVC   BFACCT,ACKEYCON+3                                                
         MVC   BFCOSTCD,COSTCODE                                                
         LR    R3,R2                                                            
         MVI   ELCODE,X'4C'                   GET DEPARTMENT CODE               
         BAS   RE,NEXTEL                      FROM '4C' ELEM                    
         BNE   PRSJ240                                                          
         CLC   TRSDACCS+1(2),=C'2D'                                             
         BNE   PRSJ240                                                          
         MVC   BFDEPTCD,TRSDACCS+5            DEPARTMENT CODE                   
         B     PRSJ240                                                          
*                                                                               
*              CONTRA-ACCOUNT U/L = SR,SB,SK,SI                                 
*                             U/L = SC (EXCEPT CONTRA ACCT=324CCSV)             
PRSJ160  MVC   BFJTYPE,=C'NV '                                                  
         MVC   BFCOSTCD,COSTCODE                                                
         MVC   BFACCT,ACKEYCON+3                                                
         CLC   ACKEYCON+1(2),=C'SK'                                             
         BNE   PRSJ240                                                          
         MVC   BFACCT,=CL12'161020      '                                       
         CLC   ACKEYCON+3(2),=C'SF'                 GROUP SKSF INTO             
         BNE   PRSJ240                              ONE ACCOUNT                 
         MVC   BFACCT,=CL12'161030      '                                       
         B     PRSJ240                                                          
*                                                                               
PRSJ240  BAS   RE,BUFPUT               PUT RECORD TO BUFFALO                    
         MVI   SENTSJ,C'Y'             MARK THAT TRNS HAS BEEN SENT             
         CLI   QOPT7,C'Y'              DONOVAN SUMMARY CHECK PURPOSES           
         BE    PRSJ950                                                          
*                                                                               
PRSJ260  CLI   ACKEYACC+9,C'X'         CHECK FOR X-JOB                          
         BNE   PRSJ950                                                          
         MVC   CONTRAUL,=C'SJ'                                                  
         MVC   BFJTYPE,=C'XJ '                                                  
         MVC   BFKJOB,ACKEYACC+9       JOB NUMBER                               
         L     R3,ADHEIRC              LEVEL - IF THERE ISN'T ONE               
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRSJ262                 IF NO 25 ELEM, NO COST CODE              
         USING ACNOD,R3                                                         
         MVC   COSTCODE,SPACES                                                  
         ZIC   R1,ACNOLEN              CHECK LENGTH, ONLY GOING TO              
         SH    R1,=H'3'                MOVE IN A MAX OF 4 CHARS                 
         CH    R1,=H'3'                (FIRST FOUR MAX)                         
         BNH   *+8                                                              
         LA    R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   COSTCODE,ACNO                                                    
*                                                                               
PRSJ262  MVC   BFCOSTCD,COSTCODE                                                
*                                                                               
         USING ACUFD,R3                                                         
         MVC   BFACCT,SPACES                                                    
         L     R3,ADACC                                                         
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         BAS   RE,NEXTEL                                                        
         BNE   PRSJ280                                                          
         CLC   ACUFCODE,=C'AN'                                                  
         BNE   *-14                    GET NEXT A2 ELEMENT                      
         CLI   ACUFLEN,X'20'           SEE IF L'ACUFDATA = 0                    
         BE    PRSJ280                                                          
         ZIC   R5,ACUFLEN                                                       
         SH    R5,=H'33'                                                        
         CLI   ACUFLEN,X'29'           SEE IF L'ACUFDATA > 9                    
         BNH   *+8                                                              
         LA    R5,8                    IF YES, JUST MOVE 9                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BFACCT(0),ACUFDATA                                               
PRSJ280  BAS   RE,BUFPUT               PUT X-JOB TO BUFFALO                     
         CLC   ACKEYCON+1(2),=C'SI'    ALSO BUILD XF JOURNAL                    
         BE    PRSJ290                 TYPE IF CONTRA ACCT IS                   
         CLC   ACKEYCON+1(2),=C'SK'    UNIT/LEDG SI OR SK                       
         BNE   PRSJ950                                                          
PRSJ290  MVC   BFJTYPE,=C'XF '                                                  
         MVC   CONTRAUL,ACKEYCON+1                                              
         CLC   ACKEYCON+3(6),=C'PRI035'                                         
         BNE   *+10                                                             
         MVC   BFACCT,=CL12'161020      '                                       
         BAS   RE,BUFPUT               PUT RECORD TO BUFFALO                    
         B     PRSJ950                                                          
*                                                                               
PRSJ900  DS    0H                                                               
         BAS   RE,BUFPUT                                                        
PRSJ950  DS    0H                                                               
         CLI   SENTSJ,C'Y'                                                      
         BNE   XIT                                                              
         BAS   RE,ULPUT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER K                                                   
*--------------------------------------------------------------------*          
PROCSK   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
         TM    TRNSSTAT,X'80'                  PROCESS DEBITS ONLY              
         BNO   XIT                                                              
         MVI   BFATYPE,C'D'                    MARK AS DEBIT                    
         CLC   ACKEYACC+3(2),=C'SF'            ALL ACCTS SHOULD BEGIN           
         BNE   PRSK100                         WITH EITHER SF OR PRI -          
         MVC   BFJTYPE,=C'SFC'                 IF NEITHER PRINT OUT             
         MVC   BFACCT,=CL12'161030      '      FORGOT MESSAGE AND               
         B     PRSK200                         TRANSACTION KEY                  
PRSK100  CLC   ACKEYACC+3(3),=C'PRI'                                            
         BNE   PRSK300                                                          
         MVC   BFJTYPE,=C'PRK'                                                  
         MVC   BFACCT,=CL12'161020      '                                       
*                                                                               
PRSK200  DS    0H                                                               
         L     R6,ADLVANAM                                                      
         USING ACNAMED,R6                                                       
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'03'                                                        
         EXMVC R5,BFNAME,ACNMNAME                                               
         BAS   RE,BUFPUT                                                        
         BAS   RE,ULPUT                                                         
         B     XIT                                                              
*                                                                               
PRSK300  DS    0H                                                               
         MVC   BFJTYPE,=C'SKX'                                                  
         MVC   BFACCT,ACKEYACC+3                                                
         BAS   RE,BUFPUT                                                        
         BAS   RE,ULPUT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER V                                                   
*--------------------------------------------------------------------*          
PROCSV   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
         L     R6,ADLVANAM                                                      
         USING ACNAMED,R6                                                       
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'03'                                                        
         EXMVC R5,BFNAME,ACNMNAME                                               
*                                                                               
         TM    TRNSSTAT,X'80'                ALL DEBITS ARE GROUPED             
         BO    PRSV200                       INTO PAYABLES                      
         TM    INCLSW,X'10'                  IF CREDIT IS IT TO BE              
         BO    PRSV300                       GROUPED INTO PAYABLES              
         MVI   BFATYPE,C'C'                                                     
         MVC   BFJTYPE,=C'IP '                                                  
         MVC   BFACCT,=CL12'326010      '                                       
         MVC   BFNAME,SPACES                                                    
         B     PRSV500                                                          
*                                                                               
PRSV200  MVI   BFATYPE,C'D'                  DEBITS IN PAYABLES                 
         MVC   BFJTYPE,=C'SVD'                                                  
         MVC   BFACCT,=CL12'327010      '                                       
         BAS   RE,BUFPUT                                                        
         CLI   TRNSTYPE,129                 TAKE CARE OF UL=SC,                 
         BNE   PRSV520                      TRANSTYPE=3 DISCREP                 
         MVC   BFJTYPE,=C'SCP'                                                  
         MVI   BFATYPE,C'C'                  DEBITS IN PAYABLES                 
         MVC   BFACCT,=CL12'112FWAPV    '                                       
         B     PRSV500                                                          
*                                                                               
PRSV300  MVI   BFATYPE,C'C'                  DEBITS IN PAYABLES                 
         MVC   BFJTYPE,=C'SVC'                                                  
         MVC   BFACCT,=CL12'327010      '                                       
*                                                                               
PRSV500  BAS   RE,BUFPUT                                                        
PRSV520  BAS   RE,ULPUT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER Z                                                   
*--------------------------------------------------------------------*          
PROCSZ   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
*                                                                               
         TM    INCLSW,X'10'                SEE IF THIS TRANS IS GROUPED         
         BO    PRSZ120                     WITH PAYABLES                        
         USING ACSTATD,R3                                                       
         L     R3,ADACCSTA                                                      
         MVI   BFATYPE,C'D'                TEST                                 
         MVC   BFJTYPE,=C'MPD'             DEBIT                                
         TM    TRNSSTAT,X'80'              OR                                   
         BO    PRSZ150                     CREDIT                               
         MVI   BFATYPE,C'C'                                                     
         MVC   BFJTYPE,=C'MPC'                                                  
         B     PRSZ150                                                          
*                                                                               
PRSZ120  MVI   BFATYPE,C'D'                                                     
         MVC   BFJTYPE,=C'SZD'                                                  
         TM    TRNSSTAT,X'80'                                                   
         BO    PRSZ150                                                          
         MVI   BFATYPE,C'C'                                                     
         MVC   BFJTYPE,=C'SZC'                                                  
*                                                                               
PRSZ150  MVC   BFACCT,ACKEYACC+3                                                
         L     R6,ADACCNAM                                                      
         USING ACNAMED,R6                                                       
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'03'                                                        
         EXMVC R5,BFNAME,ACNMNAME                                               
         BAS   RE,BUFPUT                                                        
         BAS   RE,ULPUT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER B                                                   
*--------------------------------------------------------------------*          
PROCSB   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
         MVI   BFATYPE,C'D'                                                     
         MVC   BFJTYPE,=C'SBD'                                                  
         TM    TRNSSTAT,X'80'                                                   
         BO    PRSB120                                                          
         MVC   BFJTYPE,=C'SBC'                                                  
         MVI   BFATYPE,C'C'                                                     
PRSB120  MVC   BFACCT,ACKEYACC+3                                                
         L     R6,ADACCNAM                                                      
         USING ACNAMED,R6                                                       
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'03'                                                        
         EXMVC R5,BFNAME,ACNMNAME                                               
         BAS   RE,BUFPUT                                                        
         BAS   RE,ULPUT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER P                                                   
*--------------------------------------------------------------------*          
PROCSP   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
         MVI   BFATYPE,C'D'                                                     
         MVC   BFJTYPE,=C'SPD'                                                  
         TM    TRNSSTAT,X'80'                                                   
         BO    PRSP120                                                          
         MVC   BFJTYPE,=C'SPC'                                                  
         MVI   BFATYPE,C'C'                                                     
*                                                                               
PRSP120  DS    0H                                                               
         USING SACTABD,R6                                                       
         LA    R6,SPACTAB                                                       
PRSP125  CLI   SOACCT,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SDACCT,ACKEYACC+3                                                
         BE    PRSP130                                                          
         LA    R6,L'SACCTS(R6)                                                  
         B     PRSP125                                                          
PRSP130  MVC   BFACCT,SOACCT                                                    
*                                                                               
         L     R6,ADACCNAM                                                      
         USING ACNAMED,R6                                                       
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'03'                                                        
         EXMVC R5,BFNAME,ACNMNAME                                               
         BAS   RE,BUFPUT                                                        
*                                                                               
         TM    TRNSSTAT,X'80'                                                   
         BNO   PRSP150                                                          
         CLI   TRNSTYPE,129                   FOR ALL TRAN TYPES 129            
         BNE   PRSP150                        SEND OUT A CREDIT OF              
         MVC   BFJTYPE,=C'SCP'                EQUAL AMOUNT TO JOURNAL           
         MVI   BFATYPE,C'C'                   TYPE SCP                          
         MVC   BFACCT,=CL12'112FWAPP    '                                       
         BAS   RE,BUFPUT                                                        
*                                                                               
PRSP150  BAS   RE,ULPUT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER S                                                   
*--------------------------------------------------------------------*          
PROCSS   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
         MVI   BFATYPE,C'D'                                                     
         MVC   BFJTYPE,=C'SSD'                                                  
         TM    TRNSSTAT,X'80'                                                   
         BO    PRSS120                                                          
         MVC   BFJTYPE,=C'SSC'                                                  
         MVI   BFATYPE,C'C'                                                     
*                                                                               
PRSS120  DS    0H                                                               
         USING SACTABD,R6                                                       
         LA    R6,SSACTAB               TABLE OF ACCT CODES                     
PRSS125  CLI   SOACCT,X'FF'             IF POSITION ONE OF ACCT IS NOT          
         BNE   *+6                      IN TABLE THEN FILE PROBLEM              
         DC    H'0'                                                             
         CLC   SDACCT,ACKEYACC+3                                                
         BE    PRSS130                                                          
         LA    R6,L'SACCTS(R6)                                                  
         B     PRSS125                                                          
PRSS130  MVC   BFACCT,SOACCT                                                    
*                                                                               
         L     R6,ADACCNAM                   MOVE IN ACCT NAME                  
         USING ACNAMED,R6                                                       
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'03'                                                        
         EXMVC R5,BFNAME,ACNMNAME                                               
         BAS   RE,BUFPUT                                                        
*                                                                               
         TM    TRNSSTAT,X'80'                FOR ALL TRAN TYPES 129             
         BNO   PRSS150                       SEND OUT A CREDIT OF               
         CLI   TRNSTYPE,129                  EQUAL AMOUNT TO JOURNAL            
         BNE   PRSS150                       TYPE SCP                           
         MVC   BFJTYPE,=C'SCP'                                                  
         MVI   BFATYPE,C'C'                                                     
         MVC   BFACCT,=CL12'112FWAPS    '                                       
         BAS   RE,BUFPUT                                                        
*                                                                               
PRSS150  BAS   RE,ULPUT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS ALL LEDGER U                                                   
*--------------------------------------------------------------------*          
PROCSU   NTR1                                                                   
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
         MVI   BFATYPE,C'D'                                                     
         MVC   BFJTYPE,=C'SUD'                                                  
         TM    TRNSSTAT,X'80'                                                   
         BO    PRSU120                                                          
         MVC   BFJTYPE,=C'SUC'                                                  
         MVI   BFATYPE,C'C'                                                     
*                                                                               
PRSU120  DS    0H                                                               
         USING SACTABD,R6                                                       
         LA    R6,SUACTAB                                                       
PRSU125  CLI   SOACCT,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SDACCT,ACKEYACC+3                                                
         BE    PRSU130                                                          
         LA    R6,L'SACCTS(R6)                                                  
         B     PRSU125                                                          
PRSU130  MVC   BFACCT,SOACCT                                                    
*                                                                               
         L     R6,ADACCNAM                                                      
         USING ACNAMED,R6                                                       
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'03'                                                        
         EXMVC R5,BFNAME,ACNMNAME                                               
         BAS   RE,BUFPUT                                                        
*                                                                               
         TM    TRNSSTAT,X'80'                                                   
         BNO   PRSU150                                                          
         CLI   TRNSTYPE,129                  FOR ALL TRAN TYPES 129             
         BNE   PRSU150                       SEND OUT A CREDIT OF               
         MVC   BFJTYPE,=C'SCP'               EQUAL AMOUNT TO JOURNAL            
         MVI   BFATYPE,C'C'                  TYPE SCP                           
         MVC   BFACCT,=CL12'112BOMP     '                                       
         BAS   RE,BUFPUT                                                        
*                                                                               
PRSU150  BAS   RE,ULPUT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------                             
*              FIRST  SUMMARY REPORT              |                             
*--------------------------------------------------                             
SUMONE   NTR1                                                                   
         LA    R5,8                       INITIALLY CLEAR ALL                   
         LA    R4,SUMTT1D                 INTERMEDIATE AND REPORT               
PRTS050  ZAP   0(8,R4),=P'0'              ACCUMULATORS                          
         LA    R4,L'SUMTT1D(R4)                                                 
         BCT   R5,PRTS050                                                       
*                                                                               
         USING SUMPRNT,R5                                                       
         LA    R5,P                                                             
         LA    R4,BUFAREA2                                                      
         USING SORTD1,R4                                                        
         XC    SVJTYPE,SVJTYPE             REPORT BREAKS                        
         XC    SVUL,SVUL                   FIELDS FOR CHECKING FOR              
         XC    SVACCT,SVACCT                                                    
         XC    BUFAREA2,BUFAREA2                                                
         GOTO1 BUFFALO,DMCB,=C'HIGH',(SUMTP,ABUFF),BUFAREA2,1                   
         B     PRTS120                                                          
PRTS110  GOTO1 BUFFALO,DMCB,=C'SEQ',(SUMTP,ABUFF),BUFAREA2,1                    
PRTS120  CLC   BUFAREA2(1),SUMTP                                                
         BNE   PRTS150                                                          
         TM    DMCB+8,X'80'                                                     
         BO    PRTS150                                                          
*                                                                               
*              CHECK FOR ACCOUNT BREAK                                          
         OC    SVACCT,SVACCT                                                    
         BZ    PRTS121                                                          
         CLC   SR1ACCT,SVACCT                                                   
         BE    PRTS121                                                          
         MVC   P+5(13),=C'ACCOUNT TOTAL'                                        
         EDIT  (P8,SUMTT4D),(17,P1AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT4C),(17,P1AMTC),2,COMMAS=YES,MINUS=YES                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         ZAP   SUMTT4D,=P'0'                                                    
         ZAP   SUMTT4C,=P'0'                                                    
*                                                                               
*              CHECK FOR UNIT/LEDGER BREAK                                      
PRTS121  OC    SVUL,SVUL                                                        
         BZ    PRTS122                                                          
         CLC   SR1UL,SVUL                                                       
         BE    PRTS122                                                          
         MVC   P+5(17),=C'UNIT/LEDGER TOTAL'                                    
         EDIT  (P8,SUMTT3D),(17,P1AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT3C),(17,P1AMTC),2,COMMAS=YES,MINUS=YES                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         ZAP   SUMTT3D,=P'0'                                                    
         ZAP   SUMTT3C,=P'0'                                                    
*                                                                               
*              CHECK FOR J-TYPE BREAK                                           
PRTS122  OC    SVJTYPE,SVJTYPE                                                  
         BZ    PRTS130                                                          
         CLC   SR1JTYPE,SVJTYPE                                                 
         BE    PRTS130                                                          
         MVC   P+5(13),=C'J-TYPE  TOTAL'                                        
         EDIT  (P8,SUMTT2D),(17,P1AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT2C),(17,P1AMTC),2,COMMAS=YES,MINUS=YES                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         ZAP   SUMTT2D,=P'0'                                                    
         ZAP   SUMTT2C,=P'0'                                                    
*                                INDIVIDUAL OFFICE AMOUNT LINES                 
PRTS130  DS    0H                                                               
         MVC   P1JTYPE,SR1JTYPE                                                 
         MVC   P1UL,SR1UL                                                       
         MVC   P1ACCT,SR1ACCT                                                   
         MVC   P1CSTCD,SR1CSTCD                                                 
         MVC   P1OFFC,SR1OFFC                                                   
         MVC   P1JOBNM,SR1JOBNM                                                 
PRTS132  CLI   SR1ATYPE,C'D'                                                    
         BNE   PRTS135                                                          
         EDIT  (P8,SR1AMT),(17,P1AMTD),2,COMMAS=YES,MINUS=YES                   
         AP    SUMTT1D,SR1AMT                                                   
         AP    SUMTT2D,SR1AMT                                                   
         AP    SUMTT3D,SR1AMT                                                   
         AP    SUMTT4D,SR1AMT                                                   
         B     PRTS140                                                          
PRTS135  EDIT  (P8,SR1AMT),(17,P1AMTC),2,COMMAS=YES,MINUS=YES                   
         AP    SUMTT1C,SR1AMT                                                   
         AP    SUMTT2C,SR1AMT                                                   
         AP    SUMTT3C,SR1AMT                                                   
         AP    SUMTT4C,SR1AMT                                                   
PRTS140  GOTO1 ACREPORT                                                         
         MVC   SVUL,SR1UL                                                       
         MVC   SVJTYPE,SR1JTYPE                                                 
         MVC   SVACCT,SR1ACCT                                                   
         B     PRTS110                                                          
*                                LAST ACCT TOTAL AND REPORT TOTAL               
PRTS150  DS    0H                                                               
         EDIT  (P8,SUMTT4D),(17,P1AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT4C),(17,P1AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+5(13),=C'ACCOUNT TOTAL'                                        
         GOTO1 ACREPORT                                                         
         EDIT  (P8,SUMTT3D),(17,P1AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT3C),(17,P1AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+5(13),=C'    U/L TOTAL'                                        
         GOTO1 ACREPORT                                                         
         EDIT  (P8,SUMTT2D),(17,P1AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT2C),(17,P1AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+5(13),=C' J-TYPE TOTAL'                                        
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         EDIT  (P8,SUMTT1D),(17,P1AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT1C),(17,P1AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+5(13),=C' REPORT TOTAL'                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------                             
*              SECOND SUMMARY REPORT              |                             
*--------------------------------------------------                             
SUMTWO   NTR1                                                                   
         XC    SVCSTCD,SVCSTCD                                                  
         XC    SVACCT,SVACCT                                                    
         XC    SVJTYPE,SVJTYPE                                                  
         XC    SVOFFC,SVOFFC                                                    
         LA    R5,8                       INITIALLY CLEAR ALL                   
         LA    R4,SUMTT1D                 INTERMEDIATE AND REPORT               
PRTS205  ZAP   0(8,R4),=P'0'              ACCUMULATORS                          
         LA    R4,L'SUMTT1D(R4)                                                 
         BCT   R5,PRTS205                                                       
*                                                                               
         USING SORTD2,R4                                                        
         USING SUMPRNT,R5                                                       
         LA    R5,P                                                             
         LA    R4,BUFAREA2                                                      
         XC    BUFAREA2,BUFAREA2                                                
         GOTO1 BUFFALO,DMCB,=C'HIGH',(SUMTP,ABUFF),BUFAREA2,1                   
         B     PRTS220                                                          
PRTS210  GOTO1 BUFFALO,DMCB,=C'SEQ',(SUMTP,ABUFF),BUFAREA2,1                    
PRTS220  CLC   BUFAREA2(1),SUMTP                                                
         BNE   PRTS400                                                          
         TM    DMCB+8,X'80'                                                     
         BO    PRTS400                                                          
*                                                                               
*              CHECK FOR COST CODE BREAK                                        
         OC    SVCSTCD,SVCSTCD                IS IT FIRST TIME                  
         BZ    PRTS300                                                          
         CLC   SVCSTCD,SR2CSTCD               IF COST CODE HAS CHANGED          
         BE    PRTS300                        PRINT TOTAL                       
         CLC   SVCSTCD,SPACES                                                   
         BE    PRTS240                                                          
         MVC   P+10(9),=C'TOTAL FOR'                                            
         MVC   P+21(2),SVCSTCD                                                  
         EDIT  (P8,SUMTT4D),(17,P2AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT4C),(17,P2AMTC),2,COMMAS=YES,MINUS=YES                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
PRTS240  ZAP   SUMTT4D,=P'0'                  RESET ACCUMULATORS                
         ZAP   SUMTT4C,=P'0'                                                    
*                                                                               
*              CHECK FOR JOURNAL TYPE BREAK                                     
         OC    SVJTYPE,SVJTYPE                IS IT FIRST TIME                  
         BZ    PRTS300                                                          
         CLC   SVJTYPE,SR2JTYPE               HAS J-TYPE CHANGED                
         BE    PRTS300                        IF YES PRINT TOTAL                
         MVC   P+10(9),=C'TOTAL FOR'                                            
         MVC   P+20(3),SVJTYPE                                                  
         EDIT  (P8,SUMTT3D),(17,P2AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT3C),(17,P2AMTC),2,COMMAS=YES,MINUS=YES                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         ZAP   SUMTT3D,=P'0'                                                    
         ZAP   SUMTT3C,=P'0'                                                    
*                                                                               
*              CHECK FOR OFFICE BREAK                                           
         OC    SVOFFC,SVOFFC                   IS IT FIRST TIME                 
         BZ    PRTS300                                                          
         CLC   SVOFFC,SR2OFFC                  HAS OFFICE CHANGED               
         BE    PRTS300                         IF YES PRINT TOTAL               
         MVC   P+10(16),=C'TOTAL FOR OFFICE'                                    
         MVC   P+28(2),SVOFFC                                                   
         EDIT  (P8,SUMTT2D),(17,P2AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT2C),(17,P2AMTC),2,COMMAS=YES,MINUS=YES                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         ZAP   SUMTT2D,=P'0'                   RESET ACCUMULATORS               
         ZAP   SUMTT2C,=P'0'                                                    
*                                                                               
PRTS300  MVC   P2OFFC,SR2OFFC                  MOVE BUFFALO FIELDS              
         MVC   P2ACCT,SR2ACCT                  TO PRINT LINE                    
         MVC   P2CSTCD,SR2CSTCD                                                 
         MVC   P2JTYPE,SR2JTYPE                                                 
         CLI   SR2ATYPE,C'D'                                                    
         BNE   PRTS320                                                          
         EDIT  (P8,SR2AMT),(17,P2AMTD),2,COMMAS=YES,MINUS=YES                   
         AP    SUMTT4D,SR2AMT                  TOTAL INTO DEBIT                 
         AP    SUMTT3D,SR2AMT                  ACCUMULATORS                     
         AP    SUMTT2D,SR2AMT                                                   
         AP    SUMTT1D,SR2AMT                                                   
         B     PRTS325                                                          
PRTS320  EDIT  (P8,SR2AMT),(17,P2AMTC),2,COMMAS=YES,MINUS=YES                   
         AP    SUMTT4C,SR2AMT                  TOTAL INTO CREDIT                
         AP    SUMTT3C,SR2AMT                  ACCUMULATORS                     
         AP    SUMTT2C,SR2AMT                                                   
         AP    SUMTT1C,SR2AMT                                                   
PRTS325  GOTO1 ACREPORT                                                         
         MVC   SVCSTCD,SR2CSTCD                MOVE BUFFALO FIELDS              
         MVC   SVACCT,SR2ACCT                  INTO CONTROL BREAK               
         MVC   SVOFFC,SR2OFFC                  FIELDS                           
         MVC   SVJTYPE,SR2JTYPE                                                 
         B     PRTS210                                                          
*                                                                               
PRTS400  DS    0H                              AT LAST REC FROM BUFFALO         
         CLC   SVCSTCD,SPACES                  PRINT FINAL TOTALS               
         BNH   PRTS420                                                          
         EDIT  (P8,SUMTT4D),(17,P2AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT4C),(17,P2AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+10(9),=C'TOTAL FOR'                                            
         MVC   P+21(2),SVCSTCD                                                  
         GOTO1 ACREPORT                                                         
PRTS420  DS    0H                                                               
         EDIT  (P8,SUMTT2D),(17,P2AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT2C),(17,P2AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+10(16),=C'TOTAL FOR OFFICE'                                    
         MVC   P+28(2),SVOFFC                                                   
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         EDIT  (P8,SUMTT1D),(17,P2AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT1C),(17,P2AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+10(12),=C'REPORT TOTAL'                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------                             
*              THIRD  SUMMARY REPORT              |                             
*--------------------------------------------------                             
SUMTHREE NTR1                                                                   
         USING SORTD3,R4                                                        
         USING SUMPRNT,R5                                                       
         LA    R5,P                                                             
         LA    R4,BUFAREA2                                                      
         XC    SVUL,SVUL                                                        
         ZAP   SUMTT1D,=P'0'                  RESET REPORT TOTALS               
         ZAP   SUMTT1C,=P'0'                                                    
         ZAP   SUMTT2D,=P'0'                                                    
         ZAP   SUMTT2C,=P'0'                                                    
         XC    BUFAREA2,BUFAREA2                                                
         GOTO1 BUFFALO,DMCB,=C'HIGH',(SUMTP,ABUFF),BUFAREA2,1                   
         B     PRTS520                                                          
PRTS510  GOTO1 BUFFALO,DMCB,=C'SEQ',(SUMTP,ABUFF),BUFAREA2,1                    
PRTS520  CLC   BUFAREA2(1),SUMTP                                                
         BNE   PRTS550                                                          
         TM    DMCB+8,X'80'                                                     
         BO    PRTS550                                                          
*                                                                               
*                                                                               
*              CHECK FOR UNIT/LEDGER BREAK                                      
         OC    SVUL,SVUL                                                        
         BZ    PRTS530                                                          
         CLC   SR3UL,SVUL                                                       
         BE    PRTS530                                                          
         MVC   P+2(17),=C'UNIT/LEDGER TOTAL'                                    
         EDIT  (P8,SUMTT2D),(17,P3AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT2C),(17,P3AMTC),2,COMMAS=YES,MINUS=YES                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         ZAP   SUMTT2D,=P'0'                                                    
         ZAP   SUMTT2C,=P'0'                                                    
*                                                                               
PRTS530  MVC   P3UL,SR3UL                                                       
         MVC   P3ACCT,SR3ACCT                 MOVE ACCT AND NAME TO             
         MVC   P3ACCTNM,SR3NAME               PRINT LINE                        
         CLI   SR3ATYPE,C'D'                  DEBITS OR CREDITS                 
         BNE   PRTS540                                                          
         EDIT  (P8,SR3AMT),(17,P3AMTD),2,COMMAS=YES,MINUS=YES                   
         AP    SUMTT1D,SR3AMT                 ADD TO DEBITS TO ACCUMS           
         AP    SUMTT2D,SR3AMT                 ADD TO DEBITS TO ACCUMS           
         B     PRTS545                                                          
PRTS540  EDIT  (P8,SR3AMT),(17,P3AMTC),2,COMMAS=YES,MINUS=YES                   
         AP    SUMTT1C,SR3AMT                 ADD TO CREDITS TO ACCUMS          
         AP    SUMTT2C,SR3AMT                 ADD TO CREDITS TO ACCUMS          
PRTS545  GOTO1 ACREPORT                                                         
         B     PRTS510                                                          
*                                                                               
PRTS550  GOTO1 ACREPORT                       REPORT TOTAL                      
         EDIT  (P8,SUMTT1D),(17,P3AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT1C),(17,P3AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+5(12),=C'REPORT TOTAL'                                         
         GOTO1 ACREPORT                                                         
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------*            
*              ALTERNATE SET OF SUMMARY REPORTS FOR ALL PRODUCTION              
*------------------------------------------------------------------*            
*-------------------------------------------------*                             
*              FORTH  SUMMARY REPORT                                            
*-------------------------------------------------*                             
SUMFOUR  NTR1                                                                   
         XC    SVJTYPE,SVJTYPE                                                  
         XC    SVOFFC,SVOFFC                                                    
         XC    SVUL,SVUL                                                        
         LA    R5,8                       INITIALLY CLEAR ALL                   
         LA    R4,SUMTT1D                 INTERMEDIATE AND REPORT               
PROD205  ZAP   0(8,R4),=P'0'              ACCUMULATORS                          
         LA    R4,L'SUMTT1D(R4)                                                 
         BCT   R5,PROD205                                                       
*                                                                               
         USING SORTD4,R4                                                        
         USING SUMPRNT,R5                                                       
         LA    R5,P                                                             
         LA    R4,BUFAREA2                                                      
         XC    BUFAREA2,BUFAREA2                                                
         GOTO1 BUFFALO,DMCB,=C'HIGH',(SUMTP,ABUFF),BUFAREA2,1                   
         B     PROD220                                                          
PROD210  GOTO1 BUFFALO,DMCB,=C'SEQ',(SUMTP,ABUFF),BUFAREA2,1                    
PROD220  CLC   BUFAREA2(1),SUMTP                                                
         BNE   PROD400                                                          
         TM    DMCB+8,X'80'                                                     
         BO    PROD400                                                          
*                                                                               
*              CHECK FOR CONTRA/UNIT LEDGER BREAK                               
         OC    SVUL,SVUL                     IS IT FIRST TIME                   
         BZ    PROD240                                                          
         CLC   SVUL,SR4CONUL                 HAS UNIT/LEDG CHANGED              
         BE    PROD240                       IF YES PRINT TOTAL                 
         MVC   P+10(13),=C'TOTAL FOR U/L'                                       
         MVC   P+26(2),SVUL                                                     
         EDIT  (P8,SUMTT4C),(17,P4AMTC),2,COMMAS=YES,MINUS=YES                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         ZAP   SUMTT4C,=P'0'                                                    
*                                                                               
*              CHECK FOR JOURNAL TYPE BREAK                                     
PROD240  OC    SVJTYPE,SVJTYPE               IS IT FIRST TIME                   
         BZ    PROD300                                                          
         CLC   SVJTYPE,SR4JTYPE              HAS J-TYPE CHANGED                 
         BE    PROD300                       IF YES PRINT TOTALS                
         MVC   P+10(9),=C'TOTAL FOR'                                            
         MVC   P+20(3),SVJTYPE                                                  
         EDIT  (P8,SUMTT3C),(17,P4AMTC),2,COMMAS=YES,MINUS=YES                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         ZAP   SUMTT3C,=P'0'                                                    
*                                                                               
*              CHECK FOR OFFICE BREAK                                           
         OC    SVOFFC,SVOFFC                 IS IT FIRST TIME                   
         BZ    PROD300                                                          
         CLC   SVOFFC,SR4OFFC                HAS OFFICE CHANGED                 
         BE    PROD300                       IF YES PRINT TOTALS                
         MVC   P+10(16),=C'TOTAL FOR OFFICE'                                    
         MVC   P+28(2),SVOFFC                                                   
         EDIT  (P8,SUMTT2C),(17,P4AMTC),2,COMMAS=YES,MINUS=YES                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         ZAP   SUMTT2C,=P'0'                                                    
*                                                                               
PROD300  MVC   P4OFFC,SR4OFFC                MOVE BUFFALO FIELDS TO             
         MVC   P4JTYPE,SR4JTYPE              PRINT LINE                         
         MVC   P4CONUL,SR4CONUL                                                 
         MVC   P4ACCT,SR4ACCT                                                   
         MVC   P4DEPT,SR4DEPT                                                   
         MVC   P4CSTCD,SR4CSTCD                                                 
         EDIT  (P8,SR4AMT),(17,P4AMTC),2,COMMAS=YES,MINUS=YES                   
         AP    SUMTT4C,SR4AMT                ADD TO ALL CREDIT                  
         AP    SUMTT3C,SR4AMT                ACCUMULATORS                       
         AP    SUMTT2C,SR4AMT                                                   
         AP    SUMTT1C,SR4AMT                                                   
PROD320  GOTO1 ACREPORT                                                         
         MVC   SVOFFC,SR4OFFC                UPDATE CONTROL BREAK               
         MVC   SVJTYPE,SR4JTYPE              FIELDS                             
         MVC   SVUL,SR4CONUL                                                    
         B     PROD210                                                          
*                                                                               
*              AT LAST REC OF THIS TYPE FROM BUFF ,PRINT FINAL TOTALS           
PROD400  DS    0H                                                               
         EDIT  (P8,SUMTT4C),(17,P4AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+10(13),=C'TOTAL FOR U/L'                                       
         MVC   P+26(2),SVOFFC                                                   
         GOTO1 ACREPORT                                                         
         EDIT  (P8,SUMTT3C),(17,P4AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+10(16),=C'TOTAL FOR J-TYPE'                                    
         MVC   P+28(2),SVOFFC                                                   
         GOTO1 ACREPORT                                                         
         EDIT  (P8,SUMTT2C),(17,P4AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+10(16),=C'TOTAL FOR OFFICE'                                    
         MVC   P+28(2),SVOFFC                                                   
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         EDIT  (P8,SUMTT1C),(17,P4AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+10(12),=C'REPORT TOTAL'                                        
         GOTO1 ACREPORT                                                         
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------*            
*              UNIT/LEDGER SUMMARY REPORT - TOTALLING TRANSACTION               
*              AMOUNTS ONLY BY UNIT AND LEDGER - (EXCEPT U/L SIG,               
*              THAT IS A TOTAL OF GROSS BILLING AMTS ON SI)                     
*------------------------------------------------------------------*            
SUMSEVEN NTR1                                                                   
         USING SORTD7,R4                                                        
         USING SUMPRNT,R5                                                       
         LA    R5,P                                                             
         LA    R4,BUFAREA2                                                      
         MVI   SUMTP,SUMTYP7          BUF REC TYPE FOR SUMMARY                  
         MVI   FORCEHED,C'Y'                                                    
         ZAP   SUMTT1D,=P'0'           CLEAR REPORT TOTAL AREA                  
         ZAP   SUMTT1C,=P'0'                                                    
         XC    BUFAREA2,BUFAREA2                                                
         GOTO1 BUFFALO,DMCB,=C'HIGH',(SUMTP,ABUFF),BUFAREA2,1                   
         B     ULSM120                                                          
ULSM110  GOTO1 BUFFALO,DMCB,=C'SEQ',(SUMTP,ABUFF),BUFAREA2,1                    
ULSM120  CLC   BUFAREA2(1),SUMTP                                                
         BNE   ULSM150                                                          
         TM    DMCB+8,X'80'                                                     
         BO    ULSM150                                                          
*                                                                               
         MVC   P7UL,SR7UL                  PUT UNIT/LEDG TO PRINT LINE          
         MVC   P7SPEC,SR7SPEC                                                   
         EDIT  (P8,SR7AMT),(17,P7AMTD),2,COMMAS=YES,MINUS=YES                   
         EDIT  (P8,SR7AMT2),(17,P7AMTC),2,COMMAS=YES,MINUS=YES                  
         AP    SUMTT1D,SR7AMT              ADD TO ACCUMULATORS                  
         AP    SUMTT1C,SR7AMT2                                                  
         GOTO1 ACREPORT                                                         
         B     ULSM110                                                          
*                                                                               
ULSM150  GOTO1 ACREPORT                   PRINT REPORT TOTALS                   
         EDIT  (P8,SUMTT1D),(17,P7AMTD),2,COMMAS=YES,MINUS=YES                  
         EDIT  (P8,SUMTT1C),(17,P7AMTC),2,COMMAS=YES,MINUS=YES                  
         MVC   P+5(12),=C'REPORT TOTAL'                                         
         GOTO1 ACREPORT                                                         
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PUT RECS OUT TO BUFFALO                                                
*--------------------------------------------------------------------*          
BUFPUT   NTR1                                                                   
         CLI   QOPT2,C'Y'                                                       
         BNE   ARND35                                                           
         ZAP   DUB,BFAMT1                                                       
         OI    DUB+7,X'03'                                                      
         UNPK  PSECOND+5(16),DUB                                                
         MVC   P(L'BUFAREA),BUFAREA          ** TEST CHECK **                   
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
ARND35   DS    0H                                                               
*                                                                               
         MVC   SVJOB,BFKJOB                                                     
         CLC   BFJTYPE(2),=C'XJ'                                                
         BNE   *+10                                                             
         MVC   BFKJOB,SPACES                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA                               
         MVC   BFKJOB,SVJOB                                                     
*                                                                               
         CLC   BFJTYPE(2),=C'SS'            PAYABLE JOURNAL TYPES               
         BE    BUFP300                                                          
         CLC   BFJTYPE(2),=C'SU'                                                
         BE    BUFP300                                                          
         CLC   BFJTYPE(2),=C'SP'                                                
         BE    BUFP300                                                          
         CLC   BFJTYPE(2),=C'SV'                                                
         BE    BUFP300                                                          
         CLC   BFJTYPE(2),=C'SZ'                                                
         BE    BUFP300                                                          
         CLC   BFJTYPE,=C'SCP'                                                  
         BE    BUFP300                                                          
*                                                                               
         CLC   BFUL,=C'SJ'                SEND PRODUCTION RECS OUT TO           
         BNE   BUFP280                    BUILD ALTERNATE(OLD) SUMMS            
         CLC   BFJTYPE(2),=C'SJ'          (ALL OF U/L SJ WITHOUT                
         BE    BUFP280                     J-TYPE SJ)                           
         BAS   RE,ALTRSUM                                                       
         B     XIT                                                              
BUFP280  BAS   RE,SUMPUT                  BUILD FOR SUMMARIES AND PUT           
         B     XIT                        TO BUFFALO                            
BUFP300  BAS   RE,PAYBSUM                                                       
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PUT RECS OUT TO BUFFALO FOR UNIT/LEDGER SUMMARY ONLY                   
*--------------------------------------------------------------------*          
ULPUT    NTR1                                                                   
         CLI   QOPT7,C'Y'                                                       
         BNE   XIT                                                              
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         USING SORTD7,R4                                                        
         LA    R4,BUFAREA2                                                      
         XC    BUFAREA2,BUFAREA2                                                
         MVI   SR7TYPE,SUMTYP7                                                  
         MVC   SR7UL,BFUL                                                       
         ZAP   SR7AMT,BFAMT1                                                    
         ZAP   SR7AMT2,=P'0'                                                    
         TM    TRNSSTAT,X'80'                                                   
         BO    UL140                                                            
         ZAP   SR7AMT,=P'0'                                                     
         ZAP   SR7AMT2,BFAMT1                                                   
UL140    GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA2                              
*                                                                               
         CLC   BFUL,=C'SI'                                                      
         BNE   XIT                                                              
         MVI   SR7SPEC,C'G'                                                     
         L     R3,ADTRANS                   R3: NEED FOR NEXTEL                 
         MVI   ELCODE,X'50'                                                     
UL220    BAS   RE,NEXTEL                                                        
         BNE   UL250                                                            
         USING TRCASHD,R3                                                       
         CLI   TRCSTYPE,C'G'                GROSS BUCKET TYPE                   
         BNE   UL220                                                            
         ZAP   SR7AMT,=P'0'                                                     
         ZAP   SR7AMT2,TRNSAMNT                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA2                              
UL250    B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PUT RECS OUT TO BUFFALO FOR SUMMARY REPORTS                            
*        NEED THREE DIFFERENT TYPES OF BUFFALO RECORDS FOR GENERAL              
*        LEDGER FEED SUMMARY REPORTS, ONE FOR EACH SUMMARY                      
*--------------------------------------------------------------------*          
SUMPUT   NTR1                              FIRST SUMMARY IS:                    
         USING SORTD1,R4                   JOURNAL TYPE                         
         LA    R4,BUFAREA2                 UNIT/LEDGER                          
         XC    BUFAREA2,BUFAREA2           ACCOUNT CODE                         
         MVI   SR1TYPE,SUMTYP1             OFFICE                               
         MVC   SR1JTYPE,BFJTYPE                                                 
         MVC   SR1UL,BFUL                                                       
         MVC   SR1ACCT,BFACCT                                                   
         MVC   SR1OFFC,BFOFFCD                                                  
         MVC   SR1ATYPE,BFATYPE                                                 
         MVC   SR1AMT,BFAMT1                                                    
         ZAP   SR1AMT2,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA2                              
*                                                                               
         USING SORTD2,R4                   SECOND SUMMARY IS:                   
         XC    BUFAREA2,BUFAREA2           OFFICE                               
         MVI   SR2TYPE,SUMTYP2             JOURNAL TYPE                         
         MVC   SR2OFFC,BFOFFCD             ACCOUNT CODE                         
         MVC   SR2JTYPE,BFJTYPE            COST CODE                            
         MVC   SR2ACCT,BFACCT                                                   
         MVC   SR2CSTCD,BFCOSTCD                                                
         MVC   SR2ATYPE,BFATYPE                                                 
         MVC   SR2AMT,BFAMT1                                                    
         ZAP   SR2AMT2,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA2                              
*                                                                               
         USING SORTD3,R4                   THIRD SUMMARY IS:                    
         XC    BUFAREA2,BUFAREA2           UNIT/LEDGER                          
         MVI   SR3TYPE,SUMTYP3             ACCOUNT CODE                         
         MVC   SR3UL,BFUL                  ACCOUNT NAME                         
         MVC   SR3ACCT,BFACCT                                                   
         MVC   SR3NAME,BFNAME                                                   
         MVC   SR3ATYPE,BFATYPE                                                 
         MVC   SR3AMT,BFAMT1                                                    
         ZAP   SR3AMT2,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA2                              
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PUT RECS OUT TO BUFFALO FOR ALT(OLD) SUMMARY REPORTS                   
*        NEED THREE DIFFERENT TYPES OF BUFFALO RECORDS FOR PRODUCTION           
*        SUMMARY REPORTS, ONE FOR EACH SUMMARY                                  
*--------------------------------------------------------------------*          
ALTRSUM  NTR1                                                                   
         USING SORTD4,R4                   SUMMARY BY:                          
         LA    R4,BUFAREA2                 OFFICE CODE                          
         XC    BUFAREA2,BUFAREA2           JOURNAL TYPE                         
         MVI   SR4TYPE,SUMTYP4             ACCOUNT CODE                         
         MVC   SR4OFFC,BFOFFCD             DEPARTMENT CODE                      
         MVC   SR4JTYPE,BFJTYPE            COST CODE                            
         MVC   SR4ACCT,BFACCT              CONTRA UNIT LEDGER                   
         MVC   SR4DEPT,BFDEPTCD                                                 
         MVC   SR4CSTCD,BFCOSTCD                                                
         MVC   SR4CONUL,CONTRAUL                                                
         MVC   SR4ATYPE,BFATYPE                                                 
         ZAP   SR4AMT,BFAMT1                                                    
         ZAP   SR4AMT2,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA2                              
*                                                                               
         USING SORTD1,R4                   JOURNAL TYPE                         
         LA    R4,BUFAREA2                 UNIT/LEDGER                          
         XC    BUFAREA2,BUFAREA2           ACCOUNT CODE                         
         MVI   SR1TYPE,SUMTYP5             OFFICE                               
         MVC   SR1JTYPE,BFJTYPE                                                 
         MVC   SR1UL,CONTRAUL                                                   
         MVC   SR1ACCT,BFACCT                                                   
         CLC   SR1JTYPE(2),=C'XJ'                                               
         BNE   *+10                                                             
         MVC   SR1CSTCD,BFCOSTCD                                                
         MVC   SR1OFFC,BFOFFCD                                                  
         MVC   SR1ATYPE,BFATYPE                                                 
         MVC   SR1AMT,BFAMT1                                                    
         ZAP   SR1AMT2,=P'0'                                                    
         CLC   SR1JTYPE(2),=C'XJ'                                               
         BNE   *+10                                                             
         MVC   SR1JOBNM,BFKJOB            JOB NUM WAS PASSED IN CLI/PRD         
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA2                              
*                                                                               
         USING SORTD3,R4                   THIRD SUMMARY IS:                    
         XC    BUFAREA2,BUFAREA2                                                
         MVI   SR3TYPE,SUMTYP6                                                  
         MVC   SR3UL,BFUL                  ACCOUNT NAME                         
         MVC   SR3ACCT,BFACCT                                                   
         MVC   SR3NAME,BFNAME                                                   
         MVC   SR3ATYPE,BFATYPE                                                 
         MVC   SR3AMT,BFAMT1                                                    
         ZAP   SR3AMT2,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA2                              
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PUT RECS OUT TO BUFFALO FOR SUMMARY REPORTS                            
*        NEED THREE DIFFERENT TYPES OF BUFFALO RECORDS FOR GENERAL              
*        LEDGER FEED SUMMARY REPORTS, ONE FOR EACH SUMMARY                      
*--------------------------------------------------------------------*          
PAYBSUM  NTR1                                                                   
         USING SORTD1,R4                   JOURNAL TYPE                         
         LA    R4,BUFAREA2                 UNIT/LEDGER                          
         XC    BUFAREA2,BUFAREA2           ACCOUNT CODE                         
         MVI   SR1TYPE,SUMTYP8             OFFICE                               
         MVC   SR1JTYPE,BFJTYPE                                                 
         MVC   SR1UL,BFUL                                                       
         MVC   SR1ACCT,BFACCT                                                   
         MVC   SR1OFFC,BFOFFCD                                                  
         MVC   SR1ATYPE,BFATYPE                                                 
         MVC   SR1AMT,BFAMT1                                                    
         ZAP   SR1AMT2,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA2                              
*                                                                               
         USING SORTD2,R4                   SECOND SUMMARY IS:                   
         XC    BUFAREA2,BUFAREA2                                                
         MVI   SR2TYPE,SUMTYP9                                                  
         MVC   SR2OFFC,BFOFFCD                                                  
         MVC   SR2JTYPE,BFJTYPE                                                 
         MVC   SR2ACCT,BFACCT                                                   
         MVC   SR2CSTCD,BFCOSTCD                                                
         MVC   SR2ATYPE,BFATYPE                                                 
         MVC   SR2AMT,BFAMT1                                                    
         ZAP   SR2AMT2,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA2                              
*                                                                               
         USING SORTD3,R4                   THIRD SUMMARY IS:                    
         XC    BUFAREA2,BUFAREA2                                                
         MVI   SR3TYPE,SUMTYP10                                                 
         MVC   SR3UL,BFUL                  ACCOUNT NAME                         
         MVC   SR3ACCT,BFACCT                                                   
         MVC   SR3NAME,BFNAME                                                   
         MVC   SR3ATYPE,BFATYPE                                                 
         MVC   SR3AMT,BFAMT1                                                    
         ZAP   SR3AMT2,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA2                              
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE COMMON TO ALL LEDGER TYPES                                     
*        PROCESS COMPANY CODE AND OFFICE CODE - CHANGE OUR AGENCY               
*        CODE TO THEIRS - ALSO MOVE IN UNIT AND LEDGER                          
*--------------------------------------------------------------------*          
COMMRTN  NTR1                                                                   
         MVC   BUFAREA,SPACES                                                   
         MVI   BFTYPE,X'01'             TYPE 01 IS ALL INFO TO BE               
         ZAP   BFAMT1,=P'0'                                                     
         ZAP   BFAMT2,=P'0'                                                     
         MVC   BFCOMP,RQCOMP                                                    
*                                                                               
         USING ACKEYD,R4                                                        
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         MVC   BFOFFCD,TRNSANAL           OFFICE TO BUFFREC                     
         CLC   ACKEYACC+1(2),=C'SJ'                                             
         BNE   COMP160                                                          
         L     R3,ADPROFIL                                                      
         USING ACPROFD,R3                                                       
         MVC   BFOFFCD,ACPROFFC                                                 
*                                                                               
COMP160  MVC   BFUL,ACKEYACC+1            UNIT AND LEDGER                       
         MVC   BFYYMM,TRNSBTCH            BATCH YEAR,MONTH                      
         CLC   ACKEYACC+1(2),=C'SI'       FOR LEDGERS I AND J:                  
         BE    XIT                        -AMOUNTS WILL BE PROCESSED            
         CLC   ACKEYACC+1(2),=C'SJ'        WITHIN INDIVIDUAL ROUTINES           
         BE    XIT                                                              
         ZAP   BFAMT1,TRNSAMNT            TRANSACTION AMOUNT                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------*          
*        DETERMINE USING ACKEY UNIT/LEDGER AND TRANSACTION TYPE                 
*        WHETHER OR NOT TRANSACTION IS TO BE PROCESSED                          
*        AND IF SO WHICH GROUP IT FALLS INTO                                    
*           TRANSACTIONS ARE GROUPED UNDER THREE HEADINGS:                      
*           PAYABLES, GENERAL LEDGER(NON-PRODUCTION), AND PRODUCTION            
*--------------------------------------------------------------------*          
PRODINEX NTR1                                                                   
         USING TRANSD,R2           SET UP ADDRESSABILITY                        
         L     R2,ADTRANS                                                       
         USING ACKEYD,R4                                                        
         LR    R4,R2                                                            
         SH    R4,DATADISP                                                      
         MVI   INCLSW,X'00'             INITIALIZE                              
         CLC   ACKEYWRK,=C'**'                                                  
         BE    XIT                                                              
         MVI   INCLSW,X'11'             INITIALIZE                              
         CLC   ACKEYACC+1(2),=C'SS'     NO TABLE CHECK NEEDED FOR               
         BE    XIT                      THESE U/L'S - ALL TRANS WILL            
         CLC   ACKEYACC+1(2),=C'SU'     BE INCLUDED IN PAYABLES                 
         BE    XIT                      SECTION                                 
         CLC   ACKEYACC+1(2),=C'SP'                                             
         BE    XIT                                                              
*                                                                               
         CLC   ACKEYACC+1(2),=C'SV'     SV DEBITS BYPASS ALL CHECKS             
         BNE   PINC140                  AND ARE GROUPED WITH PAYABLES           
         TM    TRNSSTAT,X'80'                                                   
         BO    XIT                                                              
         LA    R6,PAYBLTYP              TABLE OF PAYABLE TYPES                  
PINC120  CLI   0(R6),X'FF'              IF NOT IN TABLE PROCESS AS              
         BE    PINC190                  BEFORE                                  
         CLC   TRNSTYPE,0(R6)                                                   
         BE    XIT                                                              
         LA    R6,1(R6)                                                         
         B     PINC120                                                          
*                                                                               
PINC140  CLC   ACKEYACC+1(2),=C'SZ'                                             
         BNE   PINC170                                                          
         LA    R6,PAYBLTYP              TABLE OF PAYABLE TYPES                  
PINC150  CLI   0(R6),X'FF'              IF NOT IN TABLE PROCESS AS              
         BE    PINC160                  BEFORE                                  
         CLC   TRNSTYPE,0(R6)                                                   
         BE    XIT                                                              
         LA    R6,1(R6)                                                         
         B     PINC150                                                          
*                                                                               
         LA    R6,PRODTTYP              TABLE OF PRODUCTION TYPES               
PINC160  CLI   0(R6),X'FF'              IF NOT IN TABLE PROCESS AS              
         BE    PINC190                  BEFORE                                  
         CLC   TRNSTYPE,0(R6)                                                   
         BE    XIT                                                              
         LA    R6,1(R6)                                                         
         B     PINC160                                                          
*                                                                               
PINC170  CLC   ACKEYACC+1(2),=C'SC'                                             
         BNE   PINC190                                                          
         TM    TRNSSTAT,X'80'                                                   
         BO    PINC190                                                          
         CLI   TRNSTYPE,37                                                      
         BE    XIT                                                              
         CLI   TRNSTYPE,33                                                      
         BE    XIT                                                              
         CLI   TRNSTYPE,34                                                      
         BE    XIT                                                              
         CLI   TRNSTYPE,49                                                      
         BE    XIT                                                              
         CLI   TRNSTYPE,50                                                      
         BE    XIT                                                              
         B     PINC190                                                          
*                                                                               
PINC190  NI    INCLSW,X'0F'             TURN OFF PAYABLE SWITCH                 
PINC300  LA    R6,PRODTTYP         TABLE OF TRANSACTION TYPES                   
         CLC   ACKEYACC+1(2),=C'SJ'                                             
         BNE   PEXC200                                                          
         CLC   ACKEYWRK,=C'99'     FOR '99' SKIP TYPE CHECK AND                 
         BE    XIT                 MARK AS INCLUDED                             
         CLI   TRNSTYPE,19                                                      
         BE    XIT                                                              
*                                                                               
PINC320  CLI   0(R6),X'FF'         PROCESS PRODUCTION PORTION                   
         BNE   PINC340             IF TRNSTYPE NOT IN TABLE, PUT OUT            
         MVC   P(L'FORGOT),FORGOT               KEY OF RECORD AND               
         MVC   P+L'FORGOT+2(42),ACKEYACC        LABEL AS FORGOTTEN              
         GOTO1 ACREPORT                                                         
         NI    INCLSW,X'00'        TURN OFF INCLUDE INDICATOR                   
         B     XIT                                                              
*                                                                               
PINC340  CLC   TRNSTYPE,0(R6)                                                   
         BE    XIT                                                              
         LA    R6,1(R6)                                                         
         B     PINC320                                                          
*                                                                               
PEXC200  DS    0H                                                               
         MVI   INCLSW,X'01'        TURN ON INCLUDE INDICATOR                    
PEXC220  CLI   0(R6),X'FF'         PROCESS GENERAL LEDGER PORTION               
         BE    XIT                 IF EXIT WITH SWITCH SET TO EXCLUDE           
         CLC   TRNSTYPE,0(R6)      TRANSACTION                                  
         BE    PEXC240                                                          
         LA    R6,1(R6)                                                         
         B     PEXC220                                                          
PEXC240  NI    INCLSW,X'00'        TURN OFF INCLUDE INDICATOR                   
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        BUILD A TABLE OF COST CODES FROM SJ                                    
*--------------------------------------------------------------------*          
BLDCOST  NTR1                                                                   
         L     R4,ACCBUFF                                                       
         MVC   0(42,R4),SPACES                                                  
         USING ACKEYD,R4                                                        
*                                                                               
         MVC   0(1,R4),RCCOMPFL                                                 
         MVC   1(2,R4),=C'SJ'                                                   
         MVI   3(R4),1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',(R4),(R4)                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,ACSTAB           R5 TO COST TABLE                             
         USING CSTTABD,R5                                                       
         MVI   0(R5),X'FF'                                                      
*                                                                               
BLD200   DS    0H                                                               
         LR    R3,R4                                                            
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   BLD250                     IF NO 25 ELEM, NO COST CODE           
         MVC   CSTENTR,SPACES             CLEAR ENTRY SPACE                     
         MVC   CSTKEY,ACKEYACC+3          CLIENT AND PRODUCT                    
         USING ACNOD,R3                                                         
         ZIC   R1,ACNOLEN                 CHECK LENGTH, ONLY GOING TO           
         SH    R1,=H'3'                   MOVE IN A MAX OF 4 CHARS              
         CH    R1,=H'3'                   (FIRST FOUR MAX)                      
         BNH   *+8                                                              
         LA    R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSTNUM2(0),ACNO            MOVE COSTCODE INTO TABLE              
         LA    R5,L'CSTENTR(R5)                                                 
         MVI   CSTKEY,X'FF'               MARK NEW END OF TABLE                 
*                                                                               
BLD250   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCOUNT',(R4),(R4)                    
BLD260   CLC   0(1,R4),RCCOMPFL                                                 
         BNE   XIT                                                              
         CLC   1(2,R4),=C'SJ'                                                   
         BNE   XIT                                                              
*                                                                               
         CLC   ACKEYACC+9(6),SPACES                                             
         BE    BLD200                                                           
         MVI   ACKEYACC+9,X'FF'           SKIP THE JOBS                         
         MVC   ACKEYACC+10(32),SPACES                                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',(R4),(R4)                    
         B     BLD260                                                           
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        GET THE NUM2 COSTCODE FROM TABLE BUILT AT RUNFRST                      
*--------------------------------------------------------------------*          
CSTCODE  NTR1                                                                   
         MVC   COSTCODE,SPACES                                                  
         L     R5,ACSTAB                  A(COST TABLE)                         
         USING CSTTABD,R5                                                       
*                                                                               
         LA    R1,ACKEYACC+3              MUST BE ACCOUNT OR CONTRA             
         CLC   ACKEYACC+1(2),=C'SJ'       ACCOUNT OF SJ TO BE ABLE              
         BE    CST200                     TO GET COST CODE                      
         LA    R1,ACKEYCON+3                                                    
         CLC   ACKEYCON+1(2),=C'SJ'                                             
         BE    CST200                                                           
         B     XIT                                                              
*                                                                               
CST200   CLI   CSTKEY,X'FF'               END OF TABLE                          
         BE    XIT                        NOT FOUND                             
         CLC   CSTCLI,0(R1)                                                     
         BH    XIT                        PAST THE CLIENT                       
         CLC   CSTPRD,SPACES              IS IT A CLIENT RECORD                 
         BE    CST220                     IF IT IS, MATCH CLIENT CODE           
         CLC   CSTKEY,0(R1)               MATCH CLIENT, PRODUCT                 
         BNE   CST225                                                           
         MVC   COSTCODE,CSTNUM2           GOT THE COST CODE                     
         B     XIT                                                              
*                                                                               
CST220   CLC   CSTCLI,0(R1)               MATCH THE CLIENT                      
         BNE   CST225                                                           
         MVC   COSTCODE,CSTNUM2           GOT THE COST CODE                     
CST225   LA    R5,L'CSTENTR(R5)                                                 
         B     CST200                                                           
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        DUMP RECORDS                                                           
*--------------------------------------------------------------------*          
DMPGET   NTR1                                                                   
         LA    R6,=C'GET'                                                       
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         LA    R6,=C'PUT'                                                       
*                                                                               
DUMP     SR    R3,R3                                                            
         ICM   R3,3,ACLENGTH                                                    
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R4),C'DUMP',(R3),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        LITERALS                                                               
*--------------------------------------------------------------------*          
TAPE     DCB   DSORG=PS,MACRF=PM,DDNAME=TAPE,BLKSIZE=80,LRECL=80                
SORTCARD DC    CL80'SORT FIELDS=(1,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(59)'                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        CONSTANTS                                                              
*--------------------------------------------------------------------*          
*                                                                               
ENDING   DC    C'TOTAL NUMBER OF RECORDS ='                                     
DSNAME   DC    CL20'ACCTAPE.AC0I7  1'                                           
COMPNUM  DC    H'3'                OGILVY SPLIT INTO THREE ID'S                 
*                                                                               
COMPTAB  DS    0C                  DDS COMPANY CODES AND TRANSLATIONS           
         DC    X'B3',C'01'         TO OGILVY COMPANY CODES                      
         DC    X'C8',C'37'                                                      
         DC    X'C3',C'71'                                                      
*                                                                               
*                                                                               
PRODTTYP DS    0C                  PRODUCTION TRANSACTION TYPES                 
         DC    AL1(01),AL1(03),AL1(08),AL1(14),AL1(15),AL1(18)                  
         DC    AL1(33),AL1(34),AL1(46),AL1(47),AL1(49),AL1(50)                  
         DC    X'FF'                                                            
*                                                                               
PAYBLTYP DS    0C                  PAYABLE TRANSACTION TYPES                    
         DC    AL1(37),AL1(129),AL1(00)                                         
         DC    X'FF'                                                            
*                                                                               
LEDGRTN  DS    0H                  TABLE OF ADDRESSES OF ALL LEDGER             
         DC    CL2'SR',AL4(PROCSR)                         ROUTINES             
         DC    CL2'SC',AL4(PROCSC)                                              
         DC    CL2'SE',AL4(PROCSE)                                              
         DC    CL2'SI',AL4(PROCSI)                                              
         DC    CL2'SJ',AL4(PROCSJ)                                              
         DC    CL2'SK',AL4(PROCSK)                                              
         DC    CL2'SV',AL4(PROCSV)                                              
         DC    CL2'SZ',AL4(PROCSZ)                                              
         DC    CL2'SB',AL4(PROCSB)                                              
         DC    CL2'SP',AL4(PROCSP)                                              
         DC    CL2'SS',AL4(PROCSS)                                              
         DC    CL2'SU',AL4(PROCSU)                                              
         DC    XL2'FFFF'                                                        
*                                                                               
SPACTAB  DS    0H                                                               
         DC    CL1'M',CL12'321230      '                                        
         DC    CL1'N',CL12'321030      '                                        
         DC    CL1'O',CL12'321930      '                                        
         DC    CL1'S',CL12'321230      '                                        
         DC    CL1'T',CL12'321230      '                                        
         DC    XL1'FF'                                                          
*                                                                               
SSACTAB  DS    0H                                                               
         DC    CL1'R',CL12'321430      '                                        
         DC    CL1'T',CL12'321630      '                                        
         DC    CL1'X',CL12'321730      '                                        
         DC    XL1'FF'                                                          
*                                                                               
SUACTAB  DS    0H                                                               
         DC    CL1'N',CL12'321830      '                                        
         DC    XL1'FF'                                                          
*                                                                               
FORGOT   DC    CL28'***** YOU FORGOT TO PROCESS '                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CSTAB    DS    20000CL10                                                        
*                                                                               
ACBUFF   DS    CL2000                                                           
*                                                                               
         BUFF  LINES=2000,ROWS=1,COLUMNS=2,COMMENT=43,FLAVOR=PACKED,   X        
               KEYLIST=(37,A)                                                   
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              LOCAL WORKING STORAGE                                            
*--------------------------------------------------------------------*          
ACI7D    DSECT                                                                  
RELO     DS    A                                                                
*              AREA FOR BUILDING BUFFALO RECS                                   
BUFAREA  DS    0CL96            BUFFALO RECORD:                                 
BFKEY    DS    0CL37            BUFFALO KEY:                                    
BFTYPE   DS    CL1                 TYPE CODE                                    
BFCOMP   DS    CL2                 COMPANY CODE                                 
BFOFFCD  DS    CL2                 OFFICE CODE                                  
BFJTYPE  DS    CL3                 JOURNAL TYPE                                 
BFUL     DS    CL2                 UNIT/LEDGER                                  
BFACCT   DS    CL12                ACCOUNT CODE                                 
BFDEPTCD DS    CL3                 DEPARTMENT CODE                              
BFCOSTCD DS    CL4                 COST CODE                                    
BFKJOB   DS    0CL6                EITHER JOB NUMBER OR CLIENT/PRODUCT          
BFKCLT   DS    CL3                 CLIENT CODE -NEED IN KEY FOR SI ONLY         
BFKPRD   DS    CL3                 PRODUCT CODE-ALL OTHERS BLANK                
BFYYMM   DS    CL2                 MONTH OF SERVICE                             
BFCMMNT  DS    0CL43            BUFFALO TEXT:                                   
BFATYPE  DS    CL1                 AMOUNT TYPE(D-DEBIT,C-CREDIT)                
BFCLNT   DS    CL3                 CLIENT CODE                                  
BFPROD   DS    CL3                 PRODUCT CODE                                 
BFNAME   DS    CL36                ACCOUNT NAME                                 
BFAMTS   DS    0PL16            BUFFALO AMOUNTS:                                
BFAMT1   DS    PL8                 DEBITS                                       
BFAMT2   DS    PL8                 CREDITS                                      
BFLNGTH  EQU   *-BFTYPE                                                         
*                                                                               
*              AREA FOR BUILDING SUMMARY RECORDS FROM INITIAL BUFF REC          
BUFAREA2 DS    CL(BFLNGTH)                                                      
*                                                                               
*              AREA FOR BUILDING RECS TO BE WRITTEN TO TAPE                     
TAPEAREA DS    0CL80                 TAPE RECORD:                               
TPKEY    DS    0CL30                 TAPE KEY:                                  
TPCOMP   DS    CL2                      COMPANY CODE                            
TPOFFCD  DS    CL2                      OFFICE CODE                             
TPJTYPE  DS    CL3                      JOURNAL TYPE                            
TPUL     DS    CL2                      UNIT/LEDGER                             
TPACCT   DS    CL12                     ACCOUNT CODE                            
TPDEPTCD DS    CL3                      DEPARTMENT CODE                         
TPCOSTCD DS    CL4                      COST CODE                               
TPYYMM   DS    CL2                      MONTH OF SERVICE                        
TPAMT1   DS    CL12                     AMOUNT                                  
TPCLNT   DS    CL3                      CLIENT CODE                             
TPPROD   DS    CL3                      PRODUCT CODE                            
TPNAME   DS    CL32                     ACCOUNT NAME                            
*                                                                               
INCLSW   DS    CL1                                                              
INCLTRAN EQU   X'01'               INCLUDE THIS TRANSACTION                     
PAYBTRAN EQU   X'10'               INCLUDE IN PAYABLES GROUP                    
*                                                                               
*                                                                               
QSTRCOMP DS    CL2                 QSTART IN COMPRESSED FORMAT                  
QENDCOMP DS    CL2                 QEND IN COMPRESSED FORMAT                    
QSTPK    DS    CL3                 QSTART IN PACKED UNSIGNED FORMAT             
QENDPK   DS    CL3                 QEND IN PACKED UNSIGNED FORMAT               
SENTSJ   DS    CL1                 HAVE I SENT A BUFREC FOR SJ?                 
ELCODE   DS    XL1                 GETEL ELEMENT CODE                           
SRLEVA   DS    CL1                 NEED LEVEL A LENGTH FOR UL=SR                
COSTCODE DS    CL4                 NUM2 COST CODE                               
RQCOMP   DS    CL2                 OGILVY'S 2 CHAR COMPANY CODE                 
CONTRAUL DS    CL2                 CONTRA ACCT UNIT/LEDG ON SJ                  
*                                                                               
SUMTP    DS    CL1                 SUMMARY TYPE:                                
SUMTYP1  EQU   X'02'                   GENERAL LEDGER SUMMARY ONE               
SUMTYP2  EQU   X'03'                                          TWO               
SUMTYP3  EQU   X'04'                                          THREE             
SUMTYP4  EQU   X'05'                   PRODUCTION SUMMARY ONE                   
SUMTYP5  EQU   X'06'                                      TWO                   
SUMTYP6  EQU   X'07'                                      THREE                 
SUMTYP7  EQU   X'08'                   UNIT/LEDGER/TRAN AMT SUMMARY             
SUMTYP8  EQU   X'09'                                    ONE                     
SUMTYP9  EQU   X'0A'                                    TWO                     
SUMTYP10 EQU   X'0B'                                    THREE                   
*                                                                               
SVOFFC   DS    CL2                     SUMMARY CONTROL BREAKS                   
SVACCT   DS    CL12                                                             
SVCSTCD  DS    CL2                                                              
SVJTYPE  DS    CL3                                                              
SVUL     DS    CL2                                                              
SVJOB    DS    CL6                                                              
SVKEY    DS    CL42                                                             
*                                                                               
SUMTT1D  DS    PL8                 SUMMARY TOTALS                               
SUMTT1C  DS    PL8                                                              
SUMTT2D  DS    PL8                                                              
SUMTT2C  DS    PL8                                                              
SUMTT3D  DS    PL8                                                              
SUMTT3C  DS    PL8                                                              
SUMTT4D  DS    PL8                                                              
SUMTT4C  DS    PL8                                                              
*                                                                               
INTERNAL DS    PL8                                                              
*                                                                               
COUNT    DS    D                   # OF TAPES REQUESTED                         
ABUFF    DS    A                                                                
ACSTAB   DS    A                   ADDRESS OF COSTCODE TABLE                    
ACCBUFF  DS    A                                                                
PRNTBL   DS    A                                                                
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        DSECTS                                                                 
*--------------------------------------------------------------------*          
*              COVER LEDGER ROUTING TABLE                                       
LEDGRTND DSECT                                                                  
LEDGLEN  DS    0CL6                   L' ONE ENTRY                              
LEDGCHAR DS    CL2                    UNIT AND LEDGER CHARACTER                 
LEDGADDR DS    CL4                    ADDR OF ROUTINE FOR THIS LEDGER           
*                                                                               
*              COVER COMPANY TABLE                                              
COMPTABD DSECT                                                                  
COMPNTR  DS    0CL3                   L' ONE ENTRY                              
CMPDDS   DS    CL1                    DDS COMPANY CODE                          
CMPOGL   DS    CL2                    OGILY'S COMPANY CODE                      
*                                                                               
*              COVER COST CODE TABLE                                            
CSTTABD  DSECT                                                                  
CSTENTR  DS    0CL10                  L' ONE ENTRY                              
CSTKEY   DS    0CL6                   KEY (CLIENT/PRODUCT)                      
CSTCLI   DS    CL3                    CLIENT                                    
CSTPRD   DS    CL3                    PRODUCT                                   
CSTNUM2  DS    CL4                    NUM2 COST CODE                            
*                                                                               
*              COVER PAYABLE ACCOUNT TABLE                                      
SACTABD  DSECT                                                                  
SACCTS   DS    0CL13                  L' ONE ENTRY                              
SDACCT   DS    CL1                    FIRST CHAR OF DONOVAN'S ACCT              
SOACCT   DS    CL12                   OGILVY'S ACCOUNT CODE                     
*                                                                               
*              SORTREC TYPE=1 FOR FIRST SUMMARY REPORT                          
*              ACCOUNT/OFFICE                                                   
SORTD1   DSECT                                                                  
SORTREC1 DS    0CL96                                                            
SRKEY1   DS    0CL37                                                            
SR1TYPE  DS    CL1                                                              
SR1JTYPE DS    CL3                                                              
SR1UL    DS    CL2                                                              
SR1ACCT  DS    CL12                                                             
SR1CSTCD DS    CL4                                                              
SR1JOBNM DS    CL6                                                              
SR1OFFC  DS    CL2                                                              
SR1ATYPE DS    CL1                                                              
         DS    CL6                                                              
SR1TEXT  DS    0CL43                                                            
SR1NAME  DS    CL32                                                             
         DS    CL11                                                             
SR1AMTS  DS    0PL16                                                            
SR1AMT   DS    PL8                                                              
SR1AMT2  DS    PL8                                                              
*                                                                               
*              SORTREC TYPE=2 FOR SECOND SUMMARY REPORT                         
*              OFFICE/ACCOUNT/NUM2 COST CODE                                    
SORTD2   DSECT                                                                  
SORTREC2 DS    0CL96                                                            
SRKEY2   DS    0CL37                                                            
SR2TYPE  DS    CL1                                                              
SR2OFFC  DS    CL2                                                              
SR2JTYPE DS    CL3                                                              
SR2ACCT  DS    CL12                                                             
SR2CSTCD DS    CL4                                                              
SR2ATYPE DS    CL1                                                              
         DS    CL14                                                             
SR2TEXT  DS    0CL43                                                            
SR2NAME  DS    CL32                                                             
         DS    CL11                                                             
SR2AMTS  DS    0PL16                                                            
SR2AMT   DS    PL8                                                              
SR2AMT2  DS    PL8                                                              
*                                                                               
*              SORTREC TYPE=3 FOR THIRD SUMMARY REPORT                          
*              ACCOUNT/ACCOUNT NAME                                             
SORTD3   DSECT                                                                  
SORTREC3 DS    0CL96                                                            
SRKEY3   DS    0CL37                                                            
SR3TYPE  DS    CL1                                                              
SR3UL    DS    CL2                                                              
SR3ACCT  DS    CL12                                                             
SR3ATYPE DS    CL1                                                              
         DS    CL21                                                             
SR3TEXT  DS    0CL43                                                            
SR3NAME  DS    CL32                                                             
         DS    CL11                                                             
SR3AMTS  DS    0PL16                                                            
SR3AMT   DS    PL8                                                              
SR3AMT2  DS    PL8                                                              
*              SORTREC TYPE=4 FOR FIRST OF ALT(OLD-PRODUCTION)                  
*              OFFICE/J-TYPE/ACCOUNT/DEPT/COSTCODE/                             
SORTD4   DSECT                                                                  
SORTREC4 DS    0CL96                                                            
SRKEY4   DS    0CL37                                                            
SR4TYPE  DS    CL1                                                              
SR4OFFC  DS    CL2                                                              
SR4JTYPE DS    CL3                                                              
SR4ACCT  DS    CL12                                                             
SR4DEPT  DS    CL3                                                              
SR4CSTCD DS    CL4                                                              
SR4CONUL DS    CL2                                                              
SR4ATYPE DS    CL1                                                              
         DS    CL9                                                              
SR4TEXT  DS    0CL43                                                            
         DS    CL43                                                             
SR4AMTS  DS    0PL16                                                            
SR4AMT   DS    PL8                                                              
SR4AMT2  DS    PL8                                                              
*                                                                               
*                                                                               
*              SUMMARY BY UNIT/LEDGER WITH TRANSACTION AMTS ONLY                
SORTD7   DSECT                                                                  
SORTREC7 DS    0CL96                                                            
SRKEY7   DS    0CL37                                                            
SR7TYPE  DS    CL1                                                              
SR7UL    DS    CL2                                                              
SR7SPEC  DS    CL1                                                              
         DS    CL33                                                             
SR7TEXT  DS    0CL43                                                            
         DS    CL43                                                             
SR7AMTS  DS    0PL16                                                            
SR7AMT   DS    PL8                                                              
SR7AMT2  DS    PL8                                                              
*                                                                               
*                                                                               
*              COVER PRINT LINE                                                 
SUMPRNT  DSECT                                                                  
         DS    C                                                                
P1BEGIN  DS    0C                                                               
P1JTYPE  DS    CL3                     GENERAL LEDGER SUMMARY ONE               
         DS    CL5                                                              
P1UL     DS    CL2                                                              
         DS    CL3                                                              
P1ACCT   DS    CL12                                                             
         DS    CL2                                                              
P1CSTCD  DS    CL4                                                              
         DS    CL6                                                              
P1OFFC   DS    CL2                                                              
         DS    CL4                                                              
P1JOBNM  DS    CL6                                                              
         DS    CL3                                                              
P1AMTD   DS    CL18                                                             
         DS    CL1                                                              
P1AMTC   DS    CL18                                                             
*                                                                               
         ORG   P1BEGIN                                                          
P2OFFC   DS    CL2                                    SUMMARY TWO               
         DS    CL6                                                              
P2JTYPE  DS    CL3                                                              
         DS    CL5                                                              
P2ACCT   DS    CL12                                                             
         DS    CL2                                                              
P2CSTCD  DS    CL4                                                              
         DS    CL3                                                              
P2AMTD   DS    CL18                                                             
         DS    CL1                                                              
P2AMTC   DS    CL18                                                             
*                                                                               
         ORG   P1BEGIN                                                          
P3UL     DS    CL2                                                              
         DS    CL3                                                              
P3ACCT   DS    CL12                                   SUMMARY THREE             
         DS    CL2                                                              
P3ACCTNM DS    CL32                                                             
         DS    CL3                                                              
P3AMTD   DS    CL18                                                             
         DS    CL1                                                              
P3AMTC   DS    CL18                                                             
*                                                                               
         ORG   P1BEGIN                                                          
P4OFFC   DS    CL2                                                              
         DS    CL6                                                              
P4JTYPE  DS    CL3                     PRODUCTION SUMMARY ONE                   
         DS    CL5                                                              
P4CONUL  DS    CL2                                                              
         DS    CL3                                                              
P4ACCT   DS    CL12                                                             
         DS    CL2                                                              
P4DEPT   DS    CL3                                                              
         DS    CL8                                                              
P4CSTCD  DS    CL4                                                              
         DS    CL17                                                             
P4AMTC   DS    CL18                                                             
*                                                                               
         ORG   P1BEGIN                                                          
P7UL     DS    CL2                     UNIT/LEDGER SUMMARY                      
P7SPEC   DS    CL1                                                              
         DS    CL12                                                             
P7AMTD   DS    CL18                                                             
         DS    CL1                                                              
P7AMTC   DS    CL18                                                             
*                                                                               
*                                                                               
*        ACGENBOTH                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPI702 05/01/02'                                      
         END                                                                    
