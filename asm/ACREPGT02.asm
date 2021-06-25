*          DATA SET ACREPGT02  AT LEVEL 083 AS OF 12/17/12                      
*PHASE ACGT02A                                                                  
*INCLUDE UNDERLIN                                                               
         TITLE 'ACGT02 - GST REPORT'                                            
*                                                                               
ACGT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,**ACGT**                                                     
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACGTD,RC                                                         
*                                                                               
         CLI   MODE,PROCTRNS                                                    
         BE    AGTTRNS                                                          
         CLI   MODE,PROCHIST                                                    
         BE    AGTTRNS                                                          
         CLI   MODE,SBACFRST                                                    
         BE    AGTSBAF                                                          
         CLI   MODE,SBACLAST                                                    
         BE    AGTSBAL                                                          
         CLI   MODE,PROCACC                                                     
         BE    AGTACC                                                           
         CLI   MODE,ACCLAST                                                     
         BE    AGTACCL                                                          
         CLI   MODE,RUNFRST                                                     
         BE    AGTRUNF                                                          
         CLI   MODE,REQFRST                                                     
         BE    AGTREQF                                                          
         CLI   MODE,REQLAST                                                     
         BE    AGTREQL                                                          
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                                     
***********************************************************************         
AGTRUNF  LA    RF,VTYPES                                                        
         LA    RE,VADDRS                                                        
         LA    R0,RELO                                                          
         S     R0,RELO                                                          
         ST    R0,RELO                                                          
*                                                                               
RNF020   CLI   0(RF),X'FF'                                                      
         BE    RNF030                                                           
         L     R0,0(RF)                                                         
         A     R0,RELO                                                          
         ST    R0,0(RE)                                                         
         LA    RF,4(RF)                                                         
         LA    RE,4(RE)                                                         
         B     RNF020                                                           
*                                                                               
         USING ACMD,R2                                                          
RNF030   MVC   EDITOR,ADEDITOR                                                  
         L     R2,AMONACC                                                       
         L     R0,=A(ACMCOBNL*MAXCONOF)                                         
         OC    ACMACOB,ACMACOB     TEST IF BUFFER WAS ACQUIRED                  
         BNZ   EXIT                YES                                          
         GETMAIN R,LV=(0)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ACMACOB          SAVE A(BUFFER)                               
         MVC   ACMMXCOB,=A(MAXCONOF)                                            
         DROP  R2                                                               
         B     EXIT                                                             
***********************************************************************         
* REQUEST FIRST                                                                 
***********************************************************************         
AGTREQF  MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    PKQSTR,PKQSTR                                                    
         MVC   PKQEND,=X'FFFFFF'                                                
*                                                                               
*                                                                               
         USING ACMD,R3                                                          
         L     R3,AMONACC                                                       
         MVC   BKSTR,ACMMSTR       USE MOA FILTER FOR BUCKETS                   
         MVC   BKEND,ACMMEND       UNLESS TDATE FILTERS ARE SPECIFIED           
*                                                                               
         CLC   QSTART,SPACES                                                    
         BE    RQF020                                                           
*                                                                               
         MVC   WORK(L'QSTART),QSTART                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,PKQSTR)                                  
         MVC   ACMTSTR,PKQSTR      HAVE MONACC FILTER TDATES                    
         MVC   BKSTR,PKQSTR        AND USE THIS DATE TO FILTER BUCKETS          
*                                                                               
RQF020   CLC   QEND,SPACES                                                      
         BE    RQF030                                                           
         MVC   WORK(L'QEND),QEND                                                
         GOTO1 DATCON,DMCB,(0,WORK),(1,PKQEND)                                  
         MVC   ACMTEND,PKQEND      HAVE MONACC FILTER TDATES                    
         MVC   BKEND,PKQEND        AND USE THIS DATE TO FILTER BUCKETS          
*                                                                               
RQF030   LA    RF,ACCUMS                                                        
         LA    RE,ACCLNQ/8                                                      
RQF040   ZAP   0(8,RF),=P'0'                                                    
         LA    RF,8(RF)                                                         
         BCT   RE,RQF040                                                        
*                                                                               
         ZAP   THISVAT,=P'0'                                                    
         ZAP   THISNET,=P'0'                                                    
         MVC   DIFRQCR(16),=2PL8'0'                                             
         MVC   DIFRQDR(16),=2PL8'0'                                             
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFC                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
AGTACC   MVC   ACCT,SPACES                                                      
         MVC   ACCTNM,SPACES                                                    
         MVI   ACTIVITY,C'N'                                                    
         L     RF,ADACC                                                         
         MVC   ACCT,3(RF)                                                       
         L     R8,ADACCNAM                                                      
         LA    RF,ACCTNM                                                        
         BAS   RE,NAMOUT                                                        
         MVC   SBACCT,SPACES                                                    
*                                                                               
         MVI   RCSUBPRG,CREDIT                                                  
         L     RE,ADACCSTA                                                      
         USING RSTELD,RE           TEST STATUS BYTE FOR I/P - O/P.              
         TM    RSTSTAT2,RSTSIVAT                                                
         BNO   *+8                                                              
*                                                                               
         MVI   RCSUBPRG,DEBIT      DEBIT BALANCE.                               
         DROP  RE                                                               
*                                                                               
         CLI   QOPT1,C' '                                                       
         BE    EXIT                                                             
*                                                                               
         CLI   QOPT1,C'T'          CONTRA ACCOUNT SUMMARY                       
         BNE   *+8                                                              
         OI    RCSUBPRG,4          USE RCSUBS 4-5                               
*                                                                               
         CLI   QOPT1,C'D'          DETAIL REPORT                                
         BNE   *+8                                                              
         OI    RCSUBPRG,X'02'      USE RCSUBS 2-3                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
AGTSBAF  CLI   QOPT1,C' '                                                       
         BE    EXIT                                                             
         L     R4,ADSUBAC                                                       
         USING CACELD,R4                                                        
         MVC   SBACCT,CACCNT+1                                                  
         MVC   P+1(L'SBACCT),SBACCT                                             
         XC    SBACITEM,SBACITEM                                                
         B     EXIT                                                             
*                                                                               
AGTSBAL  CLI   QOPT1,C' '                                                       
         BE    EXIT                                                             
*                                                                               
         CLI   PROGPROF+1,C'Y'     ARE WE SORTING BEFORE PRINTING?              
         BNE   *+8                 NO                                           
         BAS   RE,GETTRAN          YES, GET AND PRINT TRANSACTIONS              
*                                                                               
         CLC   SBACITEM,=H'0'                                                   
         BE    EXIT                NO ITEMS, NO PRINTING.                       
*                                                                               
         LA    R2,ACCCON           PRINT C/A TOTAL PRINTING                     
         BAS   RE,FORMAT           SET AND CLEAR TOTALS.                        
*                                                                               
         CLI   QOPT1,C'T'          CONTRA TOTAL ONLY                            
         BNE   SBL030                                                           
         MVC   P+1(L'SBACCT),SBACCT                                             
         B     SBL070                                                           
*                                                                               
SBL030   CLC   SBACITEM,=H'1'      MORE THAN 1 ITEM, PRINT TOTAL LINE,          
         BH    SBL060              1 ONLY, SPACE A LINE.                        
*                                                                               
SBL040   MVC   P,SPACES                                                         
         BAS   RE,REPORT                                                        
         B     EXIT                                                             
*                                                                               
SBL060   MVC   P+2(2),=C'**'                                                    
         MVCDD P+4(17),AC#TCTRA,L  TOTAL FOR CONTRA                             
         MVC   P+22(L'SBACCT),SBACCT                                            
*                                                                               
SBL070   MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
AGTTRNS  L     R8,ADTRANS          TRANSACTION ELEMENT                          
         USING TRNELD,R8                                                        
*                                                                               
         CLI   TRNEL,TRNELQ                                                     
         BNE   EXIT                                                             
*                                                                               
         TM    RCSUBPRG,DEBIT                                                   
         BZ    *+16                IT'S A CREDIT (OUTPUT) ACCOUNT.              
         TM    TRNSTAT,TRNSDR      IT'S A DEBIT (INPUT) ACCOUNT.                
         BZ    EXIT                IT'S A CREDIT ITEM, IGNORE IT.               
         B     TRN010              IT'S A DEBIT ITEM.                           
*                                                                               
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   EXIT                IT'S A DEBIT ITEM, IGNORE IT.                
*                                                                               
         USING ACMD,R3                                                          
TRN010   EQU   *                                                                
         L     R3,AMONACC                                                       
         CLC   TRNDATE,ACMTSTR     FILTER ON TRANSACTION DATE                   
         BL    EXIT                                                             
         CLC   TRNDATE,ACMTEND                                                  
         BH    EXIT                                                             
*                                                                               
*                                                                               
TRN030   BAS   RE,SAVETRN          SAVE TRANSACTION DATA                        
*                                                                               
         ZAP   THISVAT,TRVAT       BUMP TRAN TOTALS INTO ACCUMS                 
         ZAP   THISNET,TRNET                                                    
         BAS   RE,ROLLIT                                                        
         CLI   QOPT1,C' '                                                       
         BE    EXIT                                                             
*                                                                               
         CLC   LINE,MAXLINES                                                    
         BL    *+10                NO NEED TO REPEAT SBACCT YET.                
         MVC   P+1(L'SBACCT),SBACCT                                             
*                                                                               
         ZICM  RF,SBACITEM,2       BUMP CON-ACC'T ITEM COUNT.                   
         LA    RF,1(RF)                                                         
         STCM  RF,3,SBACITEM                                                    
*                                                                               
         CLI   QOPT1,C'D'          DETAIL REPORT                                
         BNE   TRN090              NO                                           
*                                                                               
         CLI   PROGPROF+1,C'Y'     SORT TRANSACTIONS BY NUMBER/DATE             
         BE    TRN050              YES                                          
         BAS   RE,TRNREPT          NO, REPORT 'EM AS YOU GET 'EM                
         B     EXIT                                                             
*                                                                               
TRN050   BAS   RE,TRNBUFF          PUT TRAN TO BUFFALO                          
         B     EXIT                                                             
*                                                                               
TRN090   MVC   P,SPACES                                                         
         B     EXIT                                                             
         EJECT                                                                  
AGTACCL  CLI   QOPT1,C' '                                                       
         BNE   ACL060                                                           
*                                                                               
         CLI   QOPT2,C' '          SUPRESS INACTIVE ACCOUNTS                    
         BE    ACL010              NO                                           
*                                                                               
         CLI   ACTIVITY,C'Y'       IS THERE ACTIVITY HERE                       
         BNE   ACL020              NO                                           
*                                                                               
ACL010   XC    BUFREC,BUFREC                                                    
         MVC   BUFTYPE,RCSUBPRG                                                 
         MVC   BUFACCT,ACCT                                                     
         MVC   BUFNAME,ACCTNM                                                   
         MVC   BUFAMNT(16),ACCACC                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFC,BUFREC,0                              
ACL020   ZAP   ACCACC,=P'0'                                                     
         ZAP   ACCACC+8,=P'0'                                                   
         B     EXIT                                                             
*                                                                               
ACL060   CLI   QOPT2,C' '          SUPRESS INACTIVE ACCOUNTS                    
         BE    ACL070              NO                                           
         CLI   ACTIVITY,C'Y'       IS THERE ACTIVITY HERE                       
         BNE   EXIT                NO                                           
*                                                                               
ACL070   MVC   P,SPACES                                                         
         MVCDD P+2(17),AC#TACC,L   TOTAL FOR ACCOUNT                            
         MVC   P+20(12),ACCT                                                    
         LA    R2,ACCACC                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         B     EXIT                                                             
         EJECT                                                                  
AGTREQL  BAS   RE,SAVEDIFF                                                      
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C' '                                                       
         BNE   RQL040              DETAIL REPORT, NO BUFFALO.                   
*                                                                               
         L     RF,ABUFC                                                         
         USING BUFFALOD,RF                                                      
         OC    BUFFSOFA,BUFFSOFA                                                
         BZ    EXIT                NO REPORT.                                   
         DROP  RF                                                               
         MVI   RCSUBPRG,CREDIT                                                  
         XC    BUFREC,BUFREC                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFC,BUFREC,0                             
         B     RQL022                                                           
*                                                                               
RQL020   GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFC,BUFREC,0                              
RQL022   CLI   P3,0                                                             
         BNE   RQL040                                                           
         CLC   BUFTYPE,RCSUBPRG                                                 
         BE    RQL030                                                           
         BAS   RE,REPORT                                                        
         BAS   RE,TOTLYN                                                        
         BAS   RE,REPORT                                                        
         MVC   RCSUBPRG,BUFTYPE    RESET HEADINGS.                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
RQL030   MVC   P+1(12),BUFACCT                                                  
         MVC   P+18(36),BUFNAME                                                 
         LA    R2,BUFAMNT                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         B     RQL020                                                           
*                                                                               
RQL040   CLI   QOPT1,C' '                                                       
         BNE   RQL060                                                           
*                                                                               
         BAS   RE,REPORT                                                        
         BAS   RE,TOTLYN                                                        
         BAS   RE,REPORT                                                        
*                                                                               
RQL060   BAS   RE,REPORT                                                        
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R0,MAXLINES                                                      
         IC    R1,LINE                                                          
         SR    R0,R1                                                            
         CH    R0,=H'5'                                                         
         BH    *+8                                                              
         MVI   LINE,99                                                          
         MVI   RCSUBPRG,CREDIT                                                  
*        MVCDD P+60(14),AC#GSTBA,R  CLIENT COST                                 
*        MVCDD P+75(14),AC#VATO,R   VAT OUTPUT                                  
*        MVCDD P+94(10),AC#PCT,R    PERCENTAGE OF VAT TO CLIENT COST            
*        BAS   RE,REPORT                                                        
*        BAS   RE,TOTLYN                                                        
*        MVI   SPACING,2                                                        
*        BAS   RE,REPORT                                                        
*        MVI   RCSUBPRG,DEBIT                                                   
*        MVCDD P+60(15),AC#GSTBA,R  NET PRE VAT                                 
*        MVCDD P+76(14),AC#VATI,R   VAT INPUT                                   
*        MVCDD P+95(10),AC#PCT,R    PERCENTAGE OF VAT TO NET                    
*        BAS   RE,REPORT                                                        
*        BAS   RE,TOTLYN                                                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         BAS   RE,PRTDIFF                                                       
         B     EXIT                                                             
         EJECT                                                                  
         GETEL R1,DATADISP,ELCODE                                               
*                                                                               
ROLLIT   NTR1                                                                   
         LA    RF,ACCUMS                                                        
         LA    RE,3                                                             
*                                                                               
ROL020   AP    0(8,RF),THISVAT     TRNAMNT                                      
         AP    8(8,RF),THISNET     SCIAMNT                                      
         LA    RF,16(RF)                                                        
         BCT   RE,ROL020                                                        
*                                                                               
         LA    RF,ACCRQDR          DR                                           
         LA    RE,DIFRQDR                                                       
         TM    RCSUBPRG,DEBIT                                                   
         BO    *+12                                                             
         LA    RF,ACCRQCR          CR                                           
         LA    RE,DIFRQCR                                                       
         AP    0(8,RF),THISVAT                                                  
         AP    8(8,RF),THISNET                                                  
         AP    0(8,RE),THISVAT     SAVE TOTALS FOR DIFF REPORT                  
         AP    8(8,RE),THISNET                                                  
         ZAP   THISVAT,=P'0'                                                    
         ZAP   THISNET,=P'0'                                                    
         B     EXIT                                                             
*                                                                               
FORMAT   NTR1                                                                   
         CP    8(8,(R2)),=P'0'     REPLACE '0' WITH NIL                         
         BE    FORMAT1                                                          
         CURED (P8,8(R2)),(14,P+60),2,MINUS=YES THISNET                         
         B     *+10                                                             
FORMAT1  MVCDD P+60(13),AC#NIL,R                                                
         CP    0(8,(R2)),=P'0'     REPLACE '0' WITH NIL                         
         BE    FORMAT2                                                          
         CURED (P8,0(R2)),(14,P+75),2,MINUS=YES THISVAT                         
         B     *+10                                                             
FORMAT2  MVCDD P+75(13),AC#NIL,R                                                
         CP    0(8,(R2)),=P'0'     IF 0 THEN MOVE IN NIL %                      
         BE    FORMAT3                                                          
         CP    8(8,(R2)),=P'0'                                                  
         BE    FORMAT3                                                          
         ZAP   PCT,0(8,R2)         VAT                                          
         SRP   PCT,5,0             VAT *100(%) *100(2.DP) *10 ROUND             
         DP    PCT,8(8,R2)         NET                                          
         CLC   TRDATE,=X'B30101'   ARE WE RUNNING PRIOR TO JAN01/13?            
         BNL   FORMAT2B                                                         
         SRP   PCT(8),64-1,5       ROUND                                        
         CP    PCT(8),=P'99999'    IF SILLY %GE THEN MOVE IN '***'              
         BH    FORMAT4                                                          
         CURED (P8,PCT),(6,P+96),2                                              
         MVI   P+102,C'%'                                                       
         B     FORMAT3B                                                         
*                                                                               
FORMAT2B CP    PCT(8),=P'99999'    IF SILLY %GE THEN MOVE IN '***'              
         BH    FORMAT4                                                          
         CURED (P8,PCT),(6,P+96),3                                              
         MVI   P+102,C'%'                                                       
         B     FORMAT3B                                                         
*                                                                               
FORMAT3  MVCDD P+99(4),AC#NIL,L                                                 
FORMAT3B B     FORMAT4B                                                         
FORMAT4  MVC   P+99(3),=C'***'     DUFF PERCENTAGE                              
FORMAT4B ZAP   0(8,R2),=P'0'                                                    
         ZAP   8(8,R2),=P'0'                                                    
         B     EXIT                                                             
         EJECT                                                                  
NAMOUT   NTR1                                                                   
         USING NAMELD,R8                                                        
         XR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     EXIT                                                             
         MVC   0(0,RF),NAMEREC                                                  
*                                                                               
REPORT   NTR1                                                                   
         MVC   HEAD1+51(21),=C'GST HISTORICAL REPORT'                           
         MVC   HEAD2+51(21),=C'---------------------'                           
         CLI   QOPT1,C' '                                                       
         BE    REPRYT                                                           
*                                                                               
         CLI   QOPT1,C'D'                                                       
         BNE   *+16                                                             
         MVC   HEAD1+51(21),=C'  GST DETAIL REPORT  '                           
         MVC   HEAD2+51(21),=C'  -----------------  '                           
*                                                                               
         CLI   QOPT1,C'T'                                                       
         BNE   *+16                                                             
         MVC   HEAD1+48(25),=C'GST CONTRA SUMMARY REPORT'                       
         MVC   HEAD2+48(25),=C'-------------------------'                       
*                                                                               
         CLI   MODE,REQLAST                                                     
         BE    REPRYT                                                           
*                                                                               
         MVC   HEAD5+2(7),=C'ACCOUNT'                                           
         MVC   HEAD5+12(12),ACCT                                                
         MVC   HEAD5+25(36),ACCTNM                                              
         GOTO1 ADSQUASH,DMCB,HEAD6+12,49                                        
REPRYT   GOTO1 ACREPORT                                                         
         B     EXIT                                                             
*                                                                               
TOTLYN   NTR1                                                                   
         MVC   P+2(20),=C'TOTAL FOR GST OUTPUT'                                 
         LA    R2,ACCRQCR                                                       
         TM    RCSUBPRG,DEBIT                                                   
         BZ    *+14                                                             
         MVC   P+2(20),=C'TOTAL FOR GST INPUT '                                 
         LA    R2,ACCRQDR                                                       
         BAS   RE,FORMAT                                                        
         B     EXIT                                                             
         EJECT                                                                  
PRTDIFF  NTR1                                                                   
         MVC   P+2(20),=C'TOTAL FOR GST OUTPUT'                                 
         LA    R2,DIFRQCR                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         MVC   P+2(20),=C'TOTAL FOR GST INPUT '                                 
         LA    R2,DIFRQDR                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         MVC   P+2(26),=C'TOTAL FOR OUTPUT VS. INPUT'                           
         LA    R2,DIFFBUCK                                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         B     EXIT                                                             
         EJECT                                                                  
SAVEDIFF NTR1                                                                   
         ZAP   DIFFBUCK,ACCRQCR                                                 
         ZAP   DIFFBUCK+L'DIFFBUCK,ACCRQCR+L'ACCRQCR                            
         SP    DIFFBUCK,ACCRQDR                                                 
         SP    DIFFBUCK+L'DIFFBUCK,ACCRQDR+L'ACCRQDR                            
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
*        FILL TRDATA WITH DATA FROM THIS TRAN                                   
*---------------------------------------------------------------------          
*                                                                               
SAVETRN  NTR1                                                                   
         MVI   ACTIVITY,C'Y'                                                    
         L     R8,ADTRANS          TRANSACTION ELEMENT                          
         USING TRNELD,R8                                                        
         USING TRNRECD,R7                                                       
         LR    R7,R8                                                            
         SH    R7,DATADISP                                                      
*                                                                               
         MVC   TRDATA(TRDATALN),SPACES                                          
         MVC   TRSUB,TRNKSBR       SAVE SUB REF TO MAINTAIN UNIQUENESS          
         MVC   TRREF,TRNREF                                                     
         MVC   TRDATE,TRNDATE                                                   
         MVC   TRBATCH,TRNBTCH                                                  
         MVC   TRANAL,TRNANAL                                                   
         ZAP   TRVAT,TRNAMNT                                                    
         ZAP   TRNET,=P'0'                                                      
*                                                                               
         MVI   ELCODE,SCIELQ       SUBSIDIARY CASH ELEMENT                      
         LR    R1,R8                                                            
SVTR20   BAS   RE,NEXTEL                                                        
         BNE   SVTR30                                                           
*                                                                               
         USING SCIELD,R1                                                        
         CLI   SCITYPE,SCITGLEV    GROSS GST                                    
         BNE   SVTR20                                                           
         ZAP   TRNET,SCIAMNT                                                    
         DROP  R1                                                               
*                                                                               
SVTR30   B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------          
*        GET SORTED TRANSACTIONS FROM BUFFALO                                   
*---------------------------------------------------------------------          
GETTRAN  NTR1                                                                   
         L     RF,ABUFC                                                         
         USING BUFFALOD,RF                                                      
         OC    BUFFSOFA,BUFFSOFA                                                
         BZ    EXIT                NO REPORT.                                   
         DROP  RF                                                               
         XC    BUFREC,BUFREC                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFC,BUFREC,0                             
         B     GTR022                                                           
*                                                                               
GTR020   GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFC,BUFREC,0                              
GTR022   CLI   P3,0                                                             
         BNE   GTR040                                                           
*                                                                               
         MVC   TRDATA(TRDATALN),BUFTDATA                                        
         BAS   RE,TRNREPT                                                       
         B     GTR020                                                           
*                                                                               
GTR040   GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFC,(X'80',1)                           
         B     EXIT                                                             
*                                                                               
*---------------------------------------------------------------------          
*        REPORT ON THE TRANSACTION IN TRDATA                                    
*---------------------------------------------------------------------          
TRNREPT  NTR1                                                                   
         MVC   P+16(6),TRREF                                                    
         GOTO1 DATCON,DMCB,(1,TRDATE),(5,P+26)                                  
         MVC   P+38(6),TRBATCH                                                  
         MVC   P+51(2),TRANAL                                                   
*                                                                               
         ZAP   ACCTRNS,TRVAT                                                    
         ZAP   ACCTRNS+8,TRNET                                                  
*                                                                               
         LA    R2,ACCTRNS                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
*        PUT THE TRANSACTION IN TRDATA TO BUFFALO                               
*---------------------------------------------------------------------          
TRNBUFF  NTR1                                                                   
         MVC   BUFKEY,SPACES                                                    
         MVC   BUFTREF,TRREF                                                    
         MVC   BUFTDATE,TRDATE                                                  
         MVC   BUFTSUB,TRSUB                                                    
         MVC   BUFTDATA,TRDATA                                                  
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFC,BUFREC,0                              
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*        NOTE: CODE BELOW IS NO-OPED, CAN'T READ BUCKETS BECAUSE OF             
*              OFFICE FILTERING                                                 
*                                                                               
*        USING BUKELD,R8                                                        
*GTHIST  CLI   BUKEL,BUKELQ       HISTORY BUCKETS                               
*        BNE   EXIT                                                             
*        LA    R2,THISVAT                                                       
*        CLI   BUCKTYPE,C'N'                                                    
*        BNE   *+8                                                              
*        LA    R2,THISNET                                                       
*        CLC   BUKMOS,BKSTR                                                     
*        BL    EXIT                DATE CHECK.                                  
*        CLC   BUKMOS,BKEND                                                     
*        BH    EXIT                                                             
*                                                                               
*ST060   CLI   QOPT1,C' '                                                       
*        BNE   EXIT                                                             
*        TM    RCSUBPRG,DEBIT      TAKE BUCKET APPROPRIATE TO VATTYPE.          
*        BZ    *+14                IT'S A CREDIT (VATTYPE=O)                    
*        ZAP   0(6,R2),BUKDR       IT'S A DEBIT  (VATTYPE=I)                    
*        B     *+10                                                             
*        ZAP   0(6,R2),BUKCR                                                    
*        BAS   RE,ROLLIT                                                        
*        B     EXIT                                                             
*        DROP  R8                                                               
*                                                                               
         SPACE 3                                                                
         DS    0F                                                               
         LTORG                                                                  
*                                                                               
RELO     DC    A(*)                                                             
VTYPES   DS    0A                                                               
         DC    V(BUFFALOC)                                                      
         DC    V(UNDERLIN)                                                      
         DC    AL1(255)                                                         
*                                                                               
         BUFF  LINES=500,                                              *        
               ROWS=1,                                                 *        
               COLUMNS=2,                                              *        
               FLAVOR=PACKED,                                          *        
               COMMENT=36,                                             *        
               KEYLIST=(13,A)                                                   
         EJECT                                                                  
ACGTD    DSECT                                                                  
MAXCONOF EQU   70000                                                            
VADDRS   DS    0A                                                               
ABUFC    DS    A                                                                
UNDERLIN DS    A                                                                
EDITOR   DS    V                                                                
*                                                                               
SBACITEM DS    H                                                                
*                                                                               
BUFREC   DS    0CL65                                                            
BUFKEY   DS    0CL13                                                            
BUFTYPE  DS    CL1                                                              
BUFACCT  DS    CL12                                                             
*                                                                               
         ORG   BUFKEY                                                           
BUFTREF  DS    CL6                 WHEN BUFFALOING TRANSACTIONS ON              
BUFTDATE DS    CL3                 DETAIL REPORT                                
BUFTSUB  DS    CL1                                                              
         ORG   BUFKEY+L'BUFKEY                                                  
*                                                                               
BUFNAME  DS    CL36                                                             
         ORG   BUFNAME                                                          
BUFTDATA DS    CL(TRDATALN)        TRDATA SAVED IN BUFFALO                      
         ORG   BUFNAME+L'BUFNAME                                                
*                                                                               
BUFAMNT  DS    0PL8                                                             
         DS    PL8                                                              
         DS    PL8                                                              
ACCT     DS    CL12                ACCOUNT                                      
ACCTNM   DS    CL36                NAME                                         
SBACCT   DS    CL14                SUB ACCOUNT                                  
*                                                                               
TRDATA   DS    0C                                                               
TRREF    DS    CL6                                                              
TRDATE   DS    CL3                                                              
TRBATCH  DS    CL6                                                              
TRTYPE   DS    CL1                                                              
TRANAL   DS    CL2                                                              
TRSUB    DS    CL1                                                              
TRVAT    DS    PL6                                                              
TRNET    DS    PL6                                                              
TRDATALN EQU   *-TRDATA                                                         
*                                  DATES                                        
BKSTR    DS    CL2                 DATES TO FILTER BUCKETS                      
BKEND    DS    CL2                                                              
PKQSTR   DS    CL3                 PACKED QSTART OR X'00'S                      
PKQEND   DS    CL3                 QEND OF X'FF'S                               
SAVEKEY  DS    CL42                                                             
*                                                                               
ELCODE   DS    CL1                                                              
ACTIVITY DS    CL1                                                              
*                                                                               
ACCUMS   DS    0PL8                                                             
ACCTRNS  DS    2PL8                                                             
ACCCON   DS    2PL8                                                             
ACCACC   DS    2PL8                                                             
ACCRQCR  DS    2PL8                                                             
ACCRQDR  DS    2PL8                                                             
ACCLNQ   EQU   *-ACCUMS                                                         
THISVAT  DS    PL6                                                              
THISNET  DS    PL6                                                              
PCT      DS    PL16                PERCENTAGE OF VAT TO NET                     
*                                                                               
DIFRQCR  DS    2PL8                                                             
DIFRQDR  DS    2PL8                                                             
DIFFBUCK DS    2PL8                                                             
DEBIT    EQU   1                                                                
CREDIT   EQU   0                                                                
         EJECT                                                                  
*        ACGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*        ACMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*        DDBUFFALOD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
*        DDREPXTRAD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*        DDEBLOCK                                                               
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
*        ACDDEQUS                                                               
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083ACREPGT02 12/17/12'                                      
         END                                                                    
