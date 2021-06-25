*          DATA SET ACBAT65    AT LEVEL 010 AS OF 06/08/00                      
*PHASE T61B65A                                                                  
BAT65    TITLE '- BATCH PROGRAM REPORT PRINTING'                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* LEVEL  009   FIXED YEAR 2000 BUG                              AHYD  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 1                                                                
BAT65    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BA65**,R7,R6,RR=RE                                           
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA                                                          
         LH    R8,=Y(BSDICTU-TWAD)                                              
         LA    R8,TWAD(R8)                                                      
         USING BSDICTU,R8          R8=A(UPPER CASE DICTIONARY WORDS)            
         L     RC,AOVERWRK                                                      
         USING OVERWRK,RC                                                       
         USING REPD,R5                                                          
         ST    RE,BORELO                                                        
*                                                                               
         CLI   CSOIND1,255         TEST CALL FROM BAT/CLOSE & UPDATE            
         BNE   INIT01                                                           
         MVC   SVRFORM,CSRFORM                                                  
         MVI   CSRFORM,RFOSUM+RFPLST SET OFFICE SUMM & POSTING LIST             
         MVI   BOBYTE2,0           INITIALISE RETURN VALUE                      
         B     INIT02                                                           
*                                                                               
INIT01   CLI   CSRFORM,0           SET DEFAULT FORMAT                           
         BNE   INIT02                                                           
         MVI   CSRFORM,RFOSUM      OFFICE SUMMARY (GERMANY ONLY)                
         CLI   CUCTRY,CTRYGER                                                   
         BE    INIT02                                                           
         MVI   CSRFORM,RFPLST      POSTING LIST                                 
*                                                                               
INIT02   GOTO1 VDICTAT,BOPARM,C'LU  ',DDIN1,DDOUT1                              
         GOTO1 VDICTAT,BOPARM,C'LL  ',DDIN2,DDOUT2                              
*                                                                               
         CLI   CSACT,ACTRPT                                                     
         BNE   *+12                                                             
         BAS   RE,REPORT                                                        
         B     XIT                                                              
*                                                                               
         CLI   CSACT,ACTPRT                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,PRINT                                                         
*                                                                               
         CLI   CSOIND1,255         TEST CALL FROM BAT/CLOSE & UPDATE            
         BNE   *+14                                                             
         MVC   CSRFORM,SVRFORM     RESTORE SAVED CSRFORM VALUE                  
         B     INIT12                                                           
*                                                                               
         L     R5,AREP             BUILD REPORTS SPOOLED MESSAGE                
         MVC   BOELEM(L'FVMSGNO),=AL2(AI$RPSPL)                                 
         MVC   BOELEM+2(L'FVOMTYP),=AL1(GTMINF)                                 
         TM    BOINDS1,BOIMSGOK    TEST FOR MULTIPLE REPORTS                    
         BO    INIT04                                                           
         MVI   BOELEM+3,17         MAX LENGTH OF SUBSTITUTION                   
         MVC   BOELEM+4(17),BCSPACES  CLEAR SUBSTITUTION AREA                   
         LA    RF,BOELEM+4                                                      
         MVC   0(L'REPSUBID,RF),REPSUBID                                        
         MVC   L'REPSUBID(1,RF),BCCOMMA                                         
         LA    RF,L'REPSUBID+1(RF)                                              
         B     INIT10                                                           
INIT04   MVC   BOELEM(L'FVMSGNO),=AL2(AI$RPSSP)                                 
         LA    RF,BOELEM+9                                                      
         LA    R1,5                                                             
INIT06   CLI   0(RF),C'-'                                                       
         BE    INIT08                                                           
         CLI   0(RF),C' '                                                       
         BNH   *+14                                                             
         LA    RF,1(RF)                                                         
         BCT   R1,INIT06                                                        
         DC    H'0'                                                             
         MVI   0(RF),C'-'                                                       
INIT08   LA    RF,1(RF)                                                         
INIT10   EDIT  (2,REPREPNO),(5,(RF)),ALIGN=LEFT                                 
         OI    BOINDS1,BOIMSGOK                                                 
INIT12   GOTO1 AXITSES                                                          
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* MAINTAIN REPORT FORMAT VALUES                                       *         
***********************************************************************         
         SPACE 1                                                                
REPORT   NTR1  ,                                                                
         CLI   TWASCRN,RPRTSCRN    TEST REPORT FORMAT SCREEN DISPLAYED          
         BE    RPT02                                                            
         GOTO1 AOVRSCR,BOPARM,('RPRTSCRN',BASOLY1H)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RPT02    TM    RPIND,RPIOLD        TEST OLD FORMAT ALREADY DISPLAYED            
         BO    RPT10                                                            
*                                                                               
         LA    R0,RFTABN                                                        
         LA    RE,RFTAB                                                         
RPT06    LH    RF,1(RE)                                                         
         LA    RF,TWAD(RF)                                                      
         MVC   8(1,RF),BC@NO                                                    
         MVC   NEWRFRM,CSRFORM                                                  
         NC    NEWRFRM,0(RE)                                                    
         BZ    *+10                                                             
         MVC   8(1,RF),BC@YES                                                   
         OI    FLDOIND-FLDHDRD(RF),FOUTTRN                                      
         LA    RE,RFTABQ(RE)                                                    
         BCT   R0,RPT06                                                         
*                                                                               
RPT08    OI    RPIND,RPIOLD                                                     
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RFDEC)  FORMAT DISPLAYED - ENTER CHANGES         
         B     RPTX                                                             
*                                                                               
RPT10    MVI   NEWRFRM,0           RESET NEW FORMAT                             
*                                                                               
         LA    R0,RFTABN                                                        
         LA    RE,RFTAB                                                         
RPT12    LH    RF,1(RE)                                                         
         LA    RF,TWAD(RF)                                                      
         CLC   8(1,RF),BC@YES                                                   
         BNE   *+14                                                             
         OC    NEWRFRM,0(RE)                                                    
         B     *+10                                                             
         MVC   8(1,RF),BC@NO                                                    
         OI    FLDOIND-FLDHDRD(RF),FOUTTRN                                      
         LA    RE,RFTABQ(RE)                                                    
         BCT   R0,RPT12                                                         
*                                                                               
         CLI   NEWRFRM,0                                                        
         BE    RPT14                                                            
         MVC   CSRFORM,NEWRFRM                                                  
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RFCHA)  RECORD FORMAT CHANGED                    
         B     RPTX                                                             
RPT14    MVI   FVOMTYP,GTMERR                                                   
         MVC   FVMSGNO,=AL2(AE$INVRF)  INVALID RECORD FORMAT - MAKE             
         B     RPTX                    AT LEAST ONE SELECTION                   
*                                                                               
RPTX     LA    R0,RPTOSUMH                                                      
         ST    R0,BOCURSOR                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALISE REPORT AREA                                              *         
***********************************************************************         
         SPACE 1                                                                
PRINT    NTR1  ,                                                                
         LA    RF,BUFFER1                                                       
         ST    RF,ABUFF1                                                        
         LA    RF,BUFFER2                                                       
         ST    RF,ABUFF2                                                        
         LA    RF,BUFFER3                                                       
         ST    RF,ABUFF3                                                        
         LA    RF,BUFFER4                                                       
         ST    RF,ABUFF4                                                        
*                                                                               
         L     R5,AREP                                                          
         MVC   REPDESC(L'AC@PMREP),AC@PMREP                                     
         MVC   REPSUBID,=C'INP'                                                 
         OC    CSREPID,CSREPID     TEST REPORT ID KNOWN                         
         BZ    *+10                                                             
         MVC   REPSUBID,CSREPID                                                 
         MVI   REPACTN,REPAINI     INITIALISE                                   
         GOTO1 VREPORT,REPD                                                     
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   REPACTN,REPAPUT                                                  
         MVC   REPPAGE,=Y(1)                                                    
         LA    RE,PMRSPECS                                                      
         ST    RE,REPAPHS                                                       
         BAS   RE,SUMMARY          SUMMARY FORMATS                              
         BAS   RE,LIST             LIST FORMATS                                 
         MVI   REPACTN,REPACLO     CLOSE REPORT                                 
         CLI   CSOIND1,255         TEST CALL FROM BAT/CLOSE & UPDATE            
         BE    PRINTX                                                           
         GOTO1 VREPORT,REPD                                                     
PRINTX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD LEDGER, OFFICE & CURRENCY TABLES FOR SUMMARY REPORTS          *         
***********************************************************************         
         SPACE 1                                                                
SUMMARY  NTR1  ,                                                                
         TM    CSRFORM,RFLSUM                                                   
         BZ    SUM02                                                            
         L     RE,ABUFF1           CLEAR LEDGER BUFFER                          
         L     RF,=A(BSIZE1)                                                    
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         OI    RIND1,R1LACT                                                     
*                                                                               
SUM02    TM    CSRFORM,RFOSUM                                                   
         BZ    SUM04                                                            
         TM    BCCPYST1,CPYSOROE   TEST COMPANY ON OFFICES                      
         BZ    SUM04                                                            
         L     RE,ABUFF2           CLEAR OFFICE BUFFER                          
         L     RF,=A(BSIZE1)                                                    
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         OI    RIND1,R1OACT                                                     
*                                                                               
SUM04    DS    0H                                                               
*&&UK                                                                           
         TM    CSRFORM,RFCSUM                                                   
         BZ    *+12                                                             
         TM    CSCURULE,CSCUFCUR   TEST COMPANY USES CURRENCY BILLING           
         BNZ   *+16                                                             
         TM    CSRFORM,RFLSUM+RFOSUM                                            
         BNZ   SUM08                                                            
         B     SUMX                                                             
         L     RE,ABUFF4           CLEAR CURRENCY BUFFER                        
         L     RF,=A(BSIZE1)                                                    
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         OI    RIND1,R1CACT                                                     
         MVC   DEFCURC,BCCPYEL+(CPYCURR-CPYELD)                                 
         CLC   DEFCURC,BCSPACES    TEST CURRENCY CODE SET                       
         BH    SUM08                                                            
         LA    RF,DEFCURT          FIND DEFAULT CURRENCY CODE FOR CTRY          
SUM06    CLC   CUCTRY,0(RF)                                                     
         BE    *+8                                                              
         CLI   0(RF),0                                                          
         BE    *+12                                                             
         LA    RF,DEFCURTL(RF)                                                  
         B     SUM06                                                            
         MVC   DEFCURC,1(RF)                                                    
*&&                                                                             
SUM08    MVC   IODAOVER,CSLSTCUR+(LSTTDA-LSTTABD)                               
         L     R1,=A(IORD+IOACCMST+IO8)                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO8                                                          
         USING TBARECD,R2                                                       
         MVC   IOKEY,TBAKEY                                                     
         MVC   BATTYPE,TBAKBTYP                                                 
*                                                                               
SUM10    L     R1,=A(IORD+IOACCDIR+IO9)                                         
         GOTO1 AIO                 GET BATCH HEADER RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(IOSEQ+IOACCDIR+IO9)                                        
         GOTO1 AIO                    GET BATCH ITEM RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEYSAV(L'TBAKEY-L'TBAKTSEQ),IOKEY                              
         BNE   SUM62                                                            
         L     R1,=A(IOGET+IOACCMST+IO9)                                        
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO9             R2=A(BATCH ITEM RECORD)                      
         MVC   BATIKEY,TBAKEY                                                   
         TM    TBARESTA,TBAESLDE   TEST LOGICALLY DELETED                       
         BZ    *+14                                                             
         MVC   IOKEY,BATIKEY                                                    
         B     SUM10                                                            
         LA    R2,TBARFST                                                       
         DROP  R2                                                               
         OI    RIND2,R2FITEM       FIRST TX ON ITEM                             
SUM12    CLI   0(R2),0             TEST EOR                                     
         BNE   *+14                                                             
         MVC   IOKEY,BATIKEY                                                    
         B     SUM10                                                            
         CLI   0(R2),ASKELQ                                                     
         BE    SUM16                                                            
         CLI   0(R2),GINELQ                                                     
         BNE   SUM60                                                            
SUM14    BAS   RE,PASSGIN          PASS GROUP INVOICE TXS                       
         TM    RIND3,R3GIN                                                      
         BO    SUM18                                                            
         B     SUM60                                                            
*                                                                               
SUM16    MVC   IOKEY,ASKKEY-ASKELD(R2)  READ TX RECD                            
         L     R1,=A(IORD+IOACCDIR+IOA)                                         
         GOTO1 AIO                                                              
         BNE   SUM60                                                            
         L     R1,=A(IOGET+IOACCMST+IOA)                                        
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SUM18    TM    RIND1,R1LACT        TEST LEDGER REPORT ACTIVE                    
         BZ    SUM30                                                            
         L     R3,AIOA             R3=A(TRANSACTION RECORD)                     
         USING TRNRECD,R3                                                       
*                                                                               
         L     RE,ABUFF1           RE=A(LEDGER TABLE)                           
         USING LTABD,RE                                                         
         LR    R1,RE                                                            
         AH    R1,=Y(BSIZE1-LTLNQ)                                              
SUM20    OC    LTNTRY(LTLNQ),LTNTRY                                             
         BZ    SUM24               ADD NEW TABLE ENTRY                          
         CLC   LTUL(L'LTUL),TRNKULA                                             
         BE    SUM26               ADD TO AN EXISTING ENTRY                     
SUM22    LA    RE,LTLNQ(RE)                                                     
         CR    R1,RE                                                            
         BH    SUM20                                                            
         DC    H'0'                TABLE IS FULL                                
*                                                                               
SUM24    MVC   LTUL,TRNKULA                                                     
         ZAP   LTDRS,=P'0'                                                      
         ZAP   LTCRS,=P'0'                                                      
SUM26    LA    RF,TRNRFST                                                       
         CLI   0(RF),TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRNELD,RF                                                        
SUM28    LA    R1,LTDRS                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+14                                                             
         AP    0(LTLAMT,R1),TRNAMNT                                             
         B     SUM30                                                            
         AP    LTLAMT(LTLAMT,R1),TRNAMNT                                        
         DROP  RF                                                               
*                                                                               
SUM30    TM    RIND1,R1OACT        TEST OFFICE REPORT ACTIVE                    
         BZ    SUM44                                                            
         L     R3,AIOA             R3=A(TRANSACTION RECORD)                     
         USING TRNRECD,R3                                                       
         CLI   TRNKUNT,C'G'        TEST GENERAL UNIT                            
         BE    SUM32                                                            
         CLI   TRNKUNT,C'S'        TEST SUBSIDIARY UNIT                         
         BNE   SUM44                                                            
         CLC   BCCPYPRD,TRNKUNT    TEST PRODUCTION                              
         BNE   *+14                                                             
         MVC   PRODCULA,TRNKCULA                                                
         BAS   RE,PUNASET          SET WORK TO UNIT FOR ANALYSIS                
*                                                                               
SUM32    LA    RF,TRNRFST                                                       
         XR    R0,R0                                                            
SUM34    CLI   0(RF),TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRNELD,RF                                                        
         L     RE,ABUFF2           RE=A(OFFICE TABLE)                           
         USING OTABD,RE                                                         
         LR    R1,RE                                                            
         AH    R1,=Y(BSIZE1-LTLNQ)                                              
SUM36    OC    OTNTRY(OTLNQ),OTNTRY                                             
         BZ    SUM40               ADD NEW TABLE ENTRY                          
         CLC   BCCPYPRD,TRNKUNT                                                 
         BNE   *+18                                                             
         CLC   OTOFF,BOWORK1                                                    
         BE    SUM42                                                            
         B     SUM38                                                            
         CLC   OTOFF,TRNOFFC                                                    
         BE    SUM42               ADD TO AN EXISTING ENTRY                     
SUM38    LA    RE,OTLNQ(RE)                                                     
         CR    R1,RE                                                            
         BH    SUM36                                                            
         DC    H'0'                TABLE IS FULL                                
*                                                                               
SUM40    MVC   OTOFF,TRNOFFC                                                    
         CLC   BCCPYPRD,TRNKUNT                                                 
         BNE   *+10                                                             
         MVC   OTOFF,BOWORK1                                                    
         ZAP   OTDRS,=P'0'                                                      
         ZAP   OTCRS,=P'0'                                                      
*                                                                               
SUM42    LA    R1,OTDRS                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+14                                                             
         AP    0(OTLAMT,R1),TRNAMNT                                             
         B     *+10                                                             
         AP    OTLAMT(OTLAMT,R1),TRNAMNT                                        
         DROP  RF                                                               
*                                                                               
SUM44    DS    0H                                                               
*&&UK                                                                           
         TM    RIND1,R1CACT        TEST CURRENCY SUMMARY ACTIVE                 
         BZ    SUM58                                                            
         L     R3,AIOA             R3=A(TRANSACTION RECORD)                     
         USING TRNRECD,R3                                                       
         LA    RF,TRNRFST                                                       
*                                                                               
         TM    RIND2,R2FITEM       TEST FOR ACTUAL CURRENCY ON 1ST TX           
         BZ    SUM50                                                            
         NI    RIND2,X'FF'-R2FITEM                                              
SUM46    XR    R0,R0                                                            
         MVC   ACTCURC,DEFCURC                                                  
SUM48    CLI   0(RF),0                                                          
         BE    SUM50                                                            
         CLI   0(RF),AFCELQ        FOREIGN CURRENCY DATA ELEMENT                
         BE    *+14                                                             
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     SUM48                                                            
         MVC   ACTCURC,AFCCURR-AFCELD(RF)                                       
*                                                                               
SUM50    L     RE,ABUFF4           RE=A(CURRENCY TABLE)                         
         USING OTABD,RE                                                         
         LR    R1,RE                                                            
         AH    R1,=Y(BSIZE1-LTLNQ)                                              
SUM52    OC    OTNTRY(OTLNQ),OTNTRY                                             
         BZ    SUM54               ADD NEW TABLE ENTRY                          
         CLC   OTCUR,ACTCURC                                                    
         BE    SUM56               ADD TO AN EXISTING ENTRY                     
         LA    RE,OTLNQ(RE)                                                     
         CR    R1,RE                                                            
         BH    SUM52                                                            
         DC    H'0'                TABLE IS FULL                                
*                                                                               
SUM54    MVC   OTCUR,ACTCURC                                                    
         ZAP   OTDRS,=P'0'                                                      
         ZAP   OTCRS,=P'0'                                                      
*                                                                               
SUM56    LA    RF,TRNRFST                                                       
         CLI   0(RF),TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRNELD,RF                                                        
         LA    R1,OTDRS                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+14                                                             
         AP    0(OTLAMT,R1),TRNAMNT                                             
         B     *+10                                                             
         AP    OTLAMT(OTLAMT,R1),TRNAMNT                                        
         DROP  RF                                                               
*&&                                                                             
SUM58    TM    RIND3,R3GIN                                                      
         BO    SUM14                                                            
SUM60    XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     SUM12                                                            
*                                                                               
SUM62    TM    RIND1,R1OACT                                                     
         BZ    SUM64                                                            
         MVI   REPSUBPG,1          OFFICE SUMMARY                               
         OI    REPHEADI,REPHFRCE                                                
         GOTO1 ABLDDET,X'80'       PRINT SCREEN STYLE BATCH DETAILS             
         BAS   RE,OFFSUM                                                        
         NI    RIND1,X'FF'-R1OACT                                               
SUM64    TM    RIND1,R1LACT                                                     
         BZ    SUM66                                                            
         MVI   REPSUBPG,2          LEDGER/BATCH TYPE SUMMARY                    
         OI    REPHEADI,REPHFRCE                                                
         GOTO1 ABLDDET,X'80'       PRINT SCREEN STYLE BATCH DETAILS             
         BAS   RE,LEDGSUM                                                       
         NI    RIND1,X'FF'-R1LACT                                               
SUM66    DS    0H                                                               
*&&UK                                                                           
         TM    RIND1,R1CACT                                                     
         BZ    SUMX                                                             
         MVI   REPSUBPG,3          CURRENCY SUMMARY                             
         OI    REPHEADI,REPHFRCE                                                
         GOTO1 ABLDDET,X'80'       PRINT SCREEN STYLE BATCH DETAILS             
         BAS   RE,OFFSUM                                                        
         NI    RIND1,X'FF'-R1CACT                                               
*&&                                                                             
SUMX     B     XIT                                                              
         DROP  R3,RE                                                            
         EJECT                                                                  
***********************************************************************         
* LEDGER SUMMARY REPORT                                               *         
***********************************************************************         
         SPACE 1                                                                
LEDGSUM  NTR1  ,                                                                
         L     R2,ABUFF1           SORT TABLE                                   
         USING LTABD,R2                                                         
         OC    0(LTLNQ,R2),0(R2)   TEST FOR NO ENTRIES                          
         BZ    LEGSX                                                            
LEGS02   LA    RF,LTLNQ(R2)                                                     
         OC    0(LTLNQ,RF),0(RF)   TEST FOR ONE ENTRY                           
         BZ    LEGS06                                                           
LEGS04   CLC   0(L'LTUL,R2),0(RF)                                               
         BNH   *+22                                                             
         XC    0(LTLNQ,R2),0(RF)                                                
         XC    0(LTLNQ,RF),0(R2)                                                
         XC    0(LTLNQ,R2),0(RF)                                                
         LA    RF,LTLNQ(RF)                                                     
         OC    0(LTLNQ,RF),0(RF)                                                
         BNZ   LEGS04                                                           
         LA    R2,LTLNQ(R2)                                                     
         OC    0(LTLNQ,R2),0(R2)                                                
         BNZ   LEGS02                                                           
*                                                                               
LEGS06   L     R2,ABUFF1                                                        
         LA    R3,REPP1                                                         
         USING LSUMD,R3                                                         
         ZAP   DTBACD,=P'0'        CLEAR DR/CR TOTALS                           
         ZAP   DTBACC,=P'0'                                                     
         ZAP   DTBAND,=P'0'                                                     
         ZAP   DTBANC,=P'0'                                                     
LEGS10   OC    0(LTLNQ,R2),0(R2)   IS THIS THE END OF THE TABLE?                
         BZ    LEGS12                                                           
         MVC   BOHALF1,LTUL                                                     
         BAS   RE,LONAME                                                        
         CURED LTDRS,(L'LSDR,LSDR),2,MINUS=YES                                  
         CURED LTCRS,(L'LSCR,LSCR),2,MINUS=YES                                  
         CLC   =C'1',LTUL                                                       
         BH    *+20                                                             
         AP    DTBAND,LTDRS                                                     
         AP    DTBANC,LTCRS                                                     
         B     *+16                                                             
         AP    DTBACD,LTDRS                                                     
         AP    DTBACC,LTCRS                                                     
         GOTO1 VREPORT,REPD                                                     
         LA    R2,LTLNQ(R2)        NEXT TABLE ENTRY                             
         B     LEGS10                                                           
LEGS12   LA    R3,REPP2                                                         
         MVC   0(L'AC@ANATS,R3),AC@ANATS                                        
         CURED DTBAND,(L'LSDR,LSDR),2,MINUS=YES                                 
         CURED DTBANC,(L'LSCR,LSCR),2,MINUS=YES                                 
         LA    R3,L'REPP1(R3)                                                   
         MVC   0(L'AC@ACCTS,R3),AC@ACCTS                                        
         CURED DTBACD,(L'LSDR,LSDR),2,MINUS=YES                                 
         CURED DTBACC,(L'LSCR,LSCR),2,MINUS=YES                                 
         GOTO1 VREPORT,REPD                                                     
LEGSX    B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* OFFICE/CURRENCY SUMMARY REPORT                                      *         
***********************************************************************         
         SPACE 1                                                                
OFFSUM   NTR1  ,                                                                
         L     R2,ABUFF2         SORT TABLE                                     
         CLI   REPSUBPG,1                                                       
         BE    *+8                                                              
         L     R2,ABUFF4                                                        
         USING OTABD,R2                                                         
         OC    0(OTLNQ,R2),0(R2) TEST FOR NO ENTRIES                            
         BZ    OFFSX                                                            
OFFS02   LA    RF,OTLNQ(R2)                                                     
         OC    0(OTLNQ,RF),0(RF)   TEST FOR ONE ENTRY                           
         BZ    OFFS06                                                           
OFFS04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)                                                    
         BNH   *+22                                                             
         XC    0(OTLNQ,R2),0(RF)                                                
         XC    0(OTLNQ,RF),0(R2)                                                
         XC    0(OTLNQ,R2),0(RF)                                                
         LA    RF,OTLNQ(RF)                                                     
         OC    0(OTLNQ,RF),0(RF)                                                
         BNZ   OFFS04                                                           
         LA    R2,OTLNQ(R2)                                                     
         OC    0(OTLNQ,R2),0(R2)                                                
         BNZ   OFFS02                                                           
*                                                                               
OFFS06   L     R2,ABUFF2                                                        
         CLI   REPSUBPG,1                                                       
         BE    *+8                                                              
         L     R2,ABUFF4                                                        
         LA    R3,REPP1                                                         
         USING OSUMD,R3                                                         
         ZAP   BOPL81,=P'0'        CLEAR OVERALL TOTALS                         
         ZAP   BOPL82,=P'0'                                                     
OFFS10   OC    0(OTLNQ,R2),0(R2)   IS THIS THE END OF THE TABLE?                
         BZ    OFFS16                                                           
         TM    RIND1,R1OACT                                                     
         BZ    OFFS12                                                           
         MVC   BOHALF1,OTOFF                                                    
         CLC   OTOFF,BCSPACES                                                   
         BH    *+10                                                             
         MVC   BOHALF1,=C'??'                                                   
         BAS   RE,LONAME           DISPLAY OFFICE CODE AND NAME                 
         B     OFFS14                                                           
OFFS12   MVC   OSCUR,OTCUR                                                      
OFFS14   CURED OTDRS,(L'OSDR,OSDR),2,MINUS=YES                                  
         CURED OTCRS,(L'OSCR,OSCR),2,MINUS=YES                                  
         CP    OTDRS,OTCRS                                                      
         BE    *+8                                                              
         OI    BOBYTE2,1           SET OFFICES NOT IN BALANCE                   
         AP    BOPL81,OTDRS                                                     
         AP    BOPL82,OTCRS                                                     
         SP    OTDRS,OTCRS                                                      
         CURED OTDRS,(L'OSBAL,OSBAL),2,MINUS=YES                                
         GOTO1 VREPORT,REPD                                                     
         LA    R2,OTLNQ(R2)        NEXT TABLE ENTRY                             
         B     OFFS10                                                           
OFFS16   LA    R3,REPP2                                                         
         MVC   0(L'AC@TOTLS,R3),AC@TOTLS                                        
         CURED BOPL81,(L'OSDR,OSDR),2,MINUS=YES                                 
         CURED BOPL82,(L'OSCR,OSCR),2,MINUS=YES                                 
         SP    BOPL81,BOPL82                                                    
         CURED BOPL81,(L'OSBAL,OSBAL),2,MINUS=YES                               
         GOTO1 VREPORT,REPD                                                     
OFFSX    B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY LEDGER/OFFICE CODE AND NAME                                 *         
***********************************************************************         
         SPACE 1                                                                
LONAME   NTR1  ,                                                                
         OC    BOHALF1,BOHALF1                                                  
         BZ    LONX                                                             
         CLI   REPSUBPG,2          TEST LEDGER OR OFFICE SUMMARY                
         BNE   *+14                                                             
         USING LSUMD,R3                                                         
         MVC   LSUL,BOHALF1        LEDGER CODE                                  
         B     LON02                                                            
         USING OSUMD,R3                                                         
         MVC   OSOFF,BOHALF1       OFFICE CODE                                  
         DROP  R3                                                               
         TM    BCCPYST4,CPYSOFF2   OLD OR NEW OFFICES?                          
         BZ    LON02                                                            
*                                                                               
         LA    RF,IOKEY            READ OFFICE RECORD (NEW OFFICES)             
         USING OFFRECD,RF                                                       
         MVC   OFFKEY,BCSPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUABIN                                                   
         MVC   OFFKOFF,BOHALF1                                                  
         L     R1,=A(IORD+IOACCMST+IO9)                                         
         GOTO1 AIO                                                              
         BNE   LONX                                                             
         L     RF,AIO9                                                          
         GOTO1 LONEX,OFFRFST                                                    
         USING OSUMD,R3                                                         
         MVC   OSOFFN,BOWORK1                                                   
         DROP  R3                                                               
         B     LONX                                                             
*                                                                               
LON02    LA    RF,IOKEY            READ DEPARTMENT RECORD (OLD OFFICES)         
         USING ACTRECD,RF          OR LEDGER RECORD                             
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         CLI   REPSUBPG,2                                                       
         BNE   *+14                                                             
         MVC   ACTKUNT(2),BOHALF1                                               
         B     *+16                                                             
         MVC   ACTKUNT(2),=C'2D'                                                
         MVC   ACTKACT(1),BOHALF1                                               
         L     R1,=A(IORD+IOACCMST+IO9)                                         
         GOTO1 AIO                                                              
         BNE   LONX                                                             
         L     RF,AIO9                                                          
         GOTO1 LONEX,ACTRFST                                                    
         CLI   REPSUBPG,2                                                       
         BNE   *+14                                                             
         USING LSUMD,R3                                                         
         MVC   LSNAME,BOWORK1                                                   
         B     LONX                                                             
         USING OSUMD,R3                                                         
         MVC   OSOFFN,BOWORK1                                                   
         DROP  R3,RF                                                            
*                                                                               
LONX     B     XIT                                                              
*                                                                               
         USING NAMELD,R1                                                        
LONEX    XR    RF,RF                                                            
         MVC   BOWORK1,BCSPACES                                                 
LONE02   CLI   NAMEL,0                                                          
         BER   RE                                                               
         IC    RF,NAMLN                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+10                                                             
         AR    R1,RF                                                            
         B     LONE02                                                           
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   BOWORK1(0),NAMEREC                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SET PRODUCTION LEDGER UNIT FOR ANLYSIS (OFFICE)                     *         
***********************************************************************         
         SPACE 1                                                                
PUNASET  NTR1  ,                                                                
         MVC   BOWORK1(2),BCSPACES PRESET UNIT FOR ANALYSIS TO SPACES           
         XR    RF,RF                                                            
         ICM   RF,3,BUFFMAX        RF=MAX NUMBER OF ENTRIES                     
         BNZ   PUNA02                                                           
         XR    RE,RE                                                            
         L     R0,ABUFF3                                                        
         LH    R1,=Y(BSIZE2)                                                    
         MVCL  R0,RE               CLEAR TABLE                                  
         IC    RE,BCPROLEN                                                      
         LA    RE,1(RE)                                                         
         TM    BCCPYST4,CPYSOFF2   TEST TWO CHARACTER OFFICES                   
         BZ    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,BUFFNTRY         SAVE ENTRY LENGTH                            
         LR    R0,RE                                                            
         XR    RE,RE                                                            
         LH    RF,=Y(BSIZE1)                                                    
         DR    RE,R0                                                            
         STCM  RF,3,BUFFMAX        SAVE MAX NUMBER OF TABLE ENTRIES             
         B     *+10                                                             
*                                                                               
PUNA02   XR    R0,R0                                                            
         IC    R0,BUFFNTRY         RE=ENTRY LENGTH                              
         XR    R1,R1                                                            
         IC    R1,BCPROLEN                                                      
         BCTR  R1,0                                                             
         L     R2,ABUFF3                                                        
PUNA04   CLI   0(R2),0                                                          
         BE    PUNA06                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PRODACT(0),0(R2)                                                 
         BNE   *+10                                                             
         XR    R3,R3               SET ENTRY FOUND IN TABLE                     
         B     PUNA16                                                           
         AR    R2,R0                                                            
         BCT   RF,PUNA04                                                        
         L     R2,ABUFF3           NO MORE ROOM - OVERWRITE FIRST ENTRY         
*                                                                               
PUNA06   LA    R0,2                TRY PRODUCT, THEN CLIENT                     
         LA    R3,BCPROLEN                                                      
         XC    IOKEY,IOKEY                                                      
PUNA08   LA    RF,IOKEY                                                         
         USING ACTRECD,RF                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,PRODCULA                                                 
         XR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         LA    R1,L'BCCPYPRD-1(R1)                                              
         EX    R1,*+4                                                           
         MVC   ACTKUNT(0),PRODCUL+1                                             
         L     R1,=A(IOHI+IOACCMST+IO8)                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO8                                                          
         LA    R1,ACTRFST                                                       
         XR    RF,RF                                                            
         USING PPRELD,R1                                                        
PUNA10   IC    RF,PPRLN            BUMP TO NEXT ELEMENT                         
         AR    R1,RF                                                            
         CLI   PPREL,0             TEST END OF RECORD                           
         BE    PUNA14                                                           
         CLI   PPREL,PPRELQ        TEST PRODUCTION PROFILE ELEMENT              
         BNE   PUNA10                                                           
         XC    BOWORK1(2),BOWORK1                                               
         CLC   PPRUFORA,BCSPACES   TEST OLD UNIT FOR ANALYSIS                   
         BNH   *+10                                                             
         MVC   BOWORK1(L'PPRUFORA),PPRUFORA EXTRACT IT                          
         OC    PPRGAOFF,PPRGAOFF   TEST NEW OFFICE CODE                         
         BZ    PUNA12                                                           
         MVC   BOWORK1(L'PPRGAOFF),PPRGAOFF EXTRACT IT                          
         TM    BCCPYST4,CPYSOFF2   TEST TWO CHARACTER OFFICES                   
         BO    *+8                                                              
         MVI   BOWORK1+1,C' '      NO - CLEAR SECOND CHARACTER                  
PUNA12   CLC   BOWORK1(2),BCSPACES TEST ANYTHING FOUND                          
         BH    *+10                                                             
PUNA14   BCTR  R3,0                R3=A(PRODLA) FOR SECOND ATTEMPT              
         BCT   R0,PUNA08           TRY AGAIN AT CLIENT LEVEL                    
         XR    R1,R1                                                            
         IC    R1,BCPROLEN         SET CLI/PRO OFFICE CODE                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R2),PRODACT                                                  
*                                                                               
PUNA16   LA    R2,1(R1,R2)         A(OFFICE CODE)                               
         LA    RF,BOWORK1                                                       
         LTR   R3,R3               TEST WE ARE EXTRACTING                       
         BZ    *+10                YES                                          
         XR    RF,R2               ELSE WANT TO SET IT IN BUFFER3               
         XR    R2,RF                                                            
         XR    RF,R2                                                            
         XR    R1,R1               SET FOR ONE CHARACTER OFFICE                 
         TM    BCCPYST4,CPYSOFF2   TEST TWO CHARACTER OFFICES                   
         BZ    *+8                                                              
         LA    R1,1(R1)            SET FOR TWO CHARACTER OFFICE                 
         EX    R1,*+4                                                           
         MVC   0(0,RF),0(R2)       SET/EXTRACT THE OFFICE                       
*                                                                               
         CLC   BOWORK1(2),BCSPACES TEST FOR BAD OFFICE CODES                    
         BNL   *+10                                                             
         MVC   BOWORK1(2),=C'??'   YES - CHANGE CODE TO HIGHLIGHT ERROR         
         B     XIT                                                              
         DROP  R1,RF                                                            
         EJECT                                                                  
***********************************************************************         
* ITEM AND POSTING LIST REPORTS                                       *         
***********************************************************************         
         SPACE 1                                                                
LIST     NTR1  ,                                                                
         TM    CSRFORM,RFILST      LIST OF ITEMS                                
         BZ    LIST02                                                           
         OI    RIND1,R1IACT                                                     
         MVI   REPSUBPG,4                                                       
         B     LIST04                                                           
LIST02   TM    CSRFORM,RFPLST      LIST OF POSTINGS                             
         BZ    LISTX                                                            
         OI    RIND1,R1PACT                                                     
         MVI   REPSUBPG,5                                                       
         MVC   LASTPAGE,REPPAGE                                                 
         ZAP   DTBACD,=P'0'        CLEAR DR/CR TOTALS                           
         ZAP   DTBACC,=P'0'                                                     
         ZAP   DTBAND,=P'0'                                                     
         ZAP   DTBANC,=P'0'                                                     
LIST04   OI    REPHEADI,REPHFRCE                                                
         GOTO1 ABLDDET,X'80'       PRINT SCREEN STYLE BATCH DETAILS             
*                                                                               
         MVC   IODAOVER,CSLSTCUR+(LSTTDA-LSTTABD)                               
         L     R1,=A(IORD+IOACCMST+IO8)                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO8                                                          
         USING TBARECD,R2                                                       
         MVC   IOKEY,TBAKEY                                                     
         MVC   BATTYPE,TBAKBTYP                                                 
*                                                                               
         TM    TBAHRIND,TBAHIGDJ   TEST FOR A NON-INPUT BATCH                   
         BZ    LIST06                                                           
         OI    RIND2,R2NON                                                      
*                                                                               
LIST06   L     R1,=A(IORD+IOACCDIR+IO9)                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=A(IOSEQ+IOACCDIR+IO9)                                        
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEYSAV(L'TBAKEY-L'TBAKTSEQ),IOKEY                              
         BNE   LIST68                                                           
         L     R1,=A(IOGET+IOACCMST+IO9)                                        
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO9             R2=A(BATCH ITEM RECORD)                      
         MVC   BATIKEY,TBAKEY                                                   
         CLI   REPSUBPG,4                                                       
         BNE   LIST08                                                           
         LA    R1,REPP1                                                         
         USING LITED,R1                                                         
         XR    R0,R0               DEFAULT SEQUENCE NUMBER                      
         ICM   R0,3,TBAKTSEQ                                                    
         CVD   R0,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  BOWORK1(3),BODUB1                                                
         LA    RE,2                REMOVE LEADING ZEROS                         
         LA    RF,BOWORK1                                                       
         CLI   0(RF),C'0'                                                       
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         BCT   RE,*-12                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LINO(0),0(RF)                                                    
         TM    TBARESTA,TBAESLDE   TEST LOGICALLY DELETED                       
         BZ    LIST08                                                           
         MVC   LISTA(L'AC@DEL),AC@DEL                                           
         DROP  R1                                                               
LIST08   LA    R2,TBARFST                                                       
         DROP  R2                                                               
         OI    RIND2,R2FITEM       FIRST TX ON ITEM                             
LIST10   ST    R2,AITELM           A(CURRENT ITEM ELEMENT)                      
         CLI   0(R2),0             TEST EOR                                     
         BNE   LIST12                                                           
         MVC   IOKEY,BATIKEY                                                    
         CLC   LASTPAGE,REPPAGE    PRINT BLANK LINE BETWEEN ITEMS               
         BE    LIST06              UNLESS THIS IS THE TOP OF THE PAGE           
         GOTO1 VREPORT,REPD                                                     
         B     LIST06                                                           
LIST12   CLI   REPSUBPG,4                                                       
         BNE   LIST14                                                           
         CLI   0(R2),BIAELQ        TEST FOR SCREEN/LINE SEQUENCE NOS.           
         BNE   LIST14                                                           
         USING BIAELD,R2                                                        
         CLI   BIALN,BIALN2Q                                                    
         BL    LIST14                                                           
         LA    R1,REPP1                                                         
         USING LITED,R1                                                         
         XR    R0,R0                                                            
         ICM   R0,3,BIASNO                                                      
         CVD   R0,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  LINO(3),BODUB1                                                   
         MVC   LINO+3(1),BCSLASH                                                
         XR    R0,R0                                                            
         ICM   R0,1,BIASIS                                                      
         CVD   R0,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  LINO+4(2),BODUB1                                                 
         DROP  R1                                                               
LIST14   CLI   0(R2),ASKELQ                                                     
         BNE   *+14                                                             
         MVC   IOKEY,ASKKEY-ASKELD(R2)                                          
         B     LIST18                                                           
         CLI   0(R2),GINELQ                                                     
         BNE   LIST66                                                           
LIST16   BAS   RE,PASSGIN          PASS GROUP INVOICE TXS                       
         TM    RIND3,R3GIN                                                      
         BZ    LIST66                                                           
         MVC   IOKEY,LGINAKEY                                                   
*                                                                               
LIST18   LA    R1,ANAUNIT          TEST FOR AN ANALYSIS ACCOUNT                 
LIST20   CLI   0(R1),0                                                          
         BE    LIST22                                                           
         CLC   0(1,R1),IOKEY+1                                                  
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     LIST20                                                           
         OI    RIND3,R3ANAL                                                     
*                                                                               
LIST22   TM    RIND3,R3GIN                                                      
         BO    LIST25                                                           
*                                                                               
         CLI   REPSUBPG,4                                                       
         BE    LIST23                                                           
         L     R1,=A(IORD+IOACCDIR+IOA)                                         
         GOTO1 AIO                    READ TX RECORD                            
         BNE   LIST66                                                           
         B     LIST24                                                           
LIST23   L     R1,=A(IORDD+IOACCDIR+IOA)                                        
         GOTO1 AIO                                                              
         BE    LIST24                                                           
         TM    IOERR,IOEDEL                                                     
         BZ    LIST64                                                           
LIST24   L     R1,=A(IOGET+IOACCMST+IOA)                                        
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LIST25   L     R3,AIOA             R3=A(TRANSACTION RECORD)                     
         ST    R3,ATRNREC                                                       
         LA    R2,REPP1                                                         
         CLI   REPSUBPG,4                                                       
         BE    LIST60                                                           
*                                                                               
         USING TRNRECD,R3                                                       
         USING LPSTD,R2                                                         
         MVC   LPACC,TRNKULA                                                    
         MVC   LPCAC,TRNKULC                                                    
         GOTO1 VDATCON,BOPARM,(1,TRNKDATE),(17,LPDATE)                          
         LA    R3,TRNRFST                                                       
         CLI   0(R3),TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRNELD,R3                                                        
         MVC   LPREF,TRNREF                                                     
         TM    TRNSTAT,TRNSDR                                                   
         BO    LIST26                                                           
         CURED TRNAMNT,(L'LPCR,LPCR),2,MINUS=YES                                
         TM    RIND3,R3ANAL                                                     
         BO    *+14                                                             
         AP    DTBACC,TRNAMNT                                                   
         B     LIST28                                                           
         AP    DTBANC,TRNAMNT                                                   
         B     LIST28                                                           
LIST26   CURED TRNAMNT,(L'LPDR,LPDR),2,MINUS=YES                                
         TM    RIND3,R3ANAL                                                     
         BO    *+14                                                             
         AP    DTBACD,TRNAMNT                                                   
         B     LIST28                                                           
         AP    DTBAND,TRNAMNT                                                   
         DROP  R3                                                               
*                                                                               
LIST28   DS    0H                                                               
*&&UK                                                                           
         LA    R3,IOKEY            READ ACCOUNT RECORD                          
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,BCSPACES                                                  
         LA    RF,ASKKEY-ASKELD(R2)                                             
         TM    RIND3,R3GIN                                                      
         BZ    *+8                                                              
         LA    RF,LGINAKEY                                                      
         MVC   ACTKCULA(L'ACTKCULA),0(RF)                                       
         L     R1,=A(IORD+IOACCDIR+IO8)                                         
         GOTO1 AIO                                                              
         BNE   LIST36                                                           
         L     R1,=A(IOGET+IOACCMST+IO8)                                        
         GOTO1 AIO                                                              
         BNE   LIST36                                                           
*                                                                               
         L     R3,AIO8             R3=A(ACCOUNT RECORD)                         
         LA    R3,ACTRFST                                                       
LIST30   CLI   0(R3),0                                                          
         BE    LIST36                                                           
         CLI   0(R3),RSTELQ                                                     
         BE    LIST34                                                           
LIST32   XR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     LIST30                                                           
*                                                                               
         USING RSTELD,R3                                                        
LIST34   CLI   RSTLN,RSTLN3Q                                                    
         BL    LIST36                                                           
         TM    RSTSTAT5,RSTSSUND   SUNDRY CREDITOR                              
         BZ    LIST36                                                           
         OI    RIND3,R3SUNDRY                                                   
*&&                                                                             
LIST36   L     R3,ATRNREC                                                       
         BAS   RE,EXTRATX          GET EXTRA TX DETAILS                         
         NI    RIND2,X'FF'-R2FITEM                                              
         GOTO1 VREPORT,REPD        PRINT MAIN ACCOUNT DETAILS                   
*                                                                               
LIST38   L     RE,DETSTART         START OF TX DETAILS LIST                     
         OC    0(REPWSCRQ-2,RE),0(RE)                                           
         BZ    LIST58                                                           
LIST40   LA    RF,REPWSCRQ-2       RF=L'DETAILS WINDOW                          
         LA    R0,REPPN            NUMBER OF PRINTLINES                         
         LA    R4,REPP1            R4=A(DETAILS WINDOW)                         
         ST    R4,SPLIT            PRESET DUMMY BREAK TO START OF LINE          
*                                                                               
LIST42   LA    R1,0(RF,RE)                                                      
LIST44   CLI   0(R1),C')'          FIND END OF DETAILS                          
         BNE   *+12                                                             
         LA    R1,1(R1)                                                         
         B     LIST52                                                           
         CLI   0(R1),C','          OR LAST REAL BREAK IN LINE                   
         BE    LIST52                                                           
         C     R4,SPLIT            HAS LAST DUMMY BREAK BEEN SET?               
         BNE   LIST46                                                           
         CLI   0(R1),C' '          NO - SET IF WE HAVE A SPACE                  
         BNE   *+12                                                             
         ST    R1,SPLIT                                                         
         B     LIST46                                                           
         CLI   0(R1),C'='               OR AN EQUALS SIGN                       
         BNE   LIST46                                                           
         ST    R1,SPLIT                                                         
         SR    R1,RF                    (EQUALS CAN'T BE END CHR)               
         CR    R1,RE                                                            
         L     R1,SPLIT                                                         
         BL    LIST46                                                           
         ST    R4,SPLIT                                                         
LIST46   CR    R1,RE                                                            
         BNH   *+8                                                              
         BCT   R1,LIST44                                                        
*                                                                               
         C     R4,SPLIT            MOVE WHOLE LINE IF NO DUMMY BREAK            
         BE    LIST48                                                           
         L     R1,SPLIT            MOVE LINE AS FAR AS DUMMY BREAK              
         CLI   0(R1),C'='                                                       
         BNE   LIST52                                                           
         SR    R1,RE                                                            
         B     LIST50                                                           
LIST48   LR    R1,RF                                                            
         BCTR  R1,0                                                             
LIST50   EX    R1,*+4                                                           
         MVC   0(0,R4),0(RE)                                                    
         LA    RE,1(R1,RE)                                                      
         B     LIST54                                                           
*                                                                               
LIST52   SR    R1,RE               MOVE LINE AS FAR AS REAL BREAK               
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R4),0(RE)                                                    
         LA    RE,2(R1,RE)                                                      
*                                                                               
LIST54   L     R1,DETEND           END OF DETAILS IN BUFFER                     
         CR    RE,R1                                                            
         BNL   LIST56                                                           
         LA    R4,L'REPP1(R4)                                                   
         ST    R4,SPLIT            SET DUMMY BREAK TO START NEXT LINE           
         BCT   R0,LIST42                                                        
LIST56   ST    RE,DETSTART                                                      
*                                                                               
LIST58   GOTO1 VREPORT,REPD        PRINT EXTRA TX DETAILS                       
         L     RE,DETSTART         ANY MORE TX DETAILS?                         
         C     RE,DETEND                                                        
         BL    LIST40              YES - GET NEXT BLOCK                         
         B     LIST64                                                           
         DROP  R2                                                               
*                                                                               
         USING TRNRECD,R3                                                       
         USING LITED,R2                                                         
LIST60   MVC   LIACC1,TRNKULA                                                   
         MVC   LIACC2,TRNKULC                                                   
         GOTO1 VDATCON,BOPARM,(1,TRNKDATE),(17,LIDATE)                          
         LA    R3,TRNRFST                                                       
         CLI   0(R3),TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRNELD,R3                                                        
         MVC   LIREF,TRNREF                                                     
         CURED TRNAMNT,(L'LIAMT,LIAMT),2,MINUS=YES                              
LIST62   GOTO1 VREPORT,REPD                                                     
         NI    RIND3,X'FF'-(R3ANAL+R3SUNDRY)                                    
         MVC   IOKEY,BATIKEY                                                    
         B     LIST06                                                           
         DROP  R2                                                               
*                                                                               
LIST64   TM    RIND3,R3GIN                                                      
         BZ    LIST66                                                           
         NI    RIND3,X'FF'-R3ANAL                                               
         B     LIST16                                                           
LIST66   NI    RIND3,X'FF'-(R3ANAL+R3SUNDRY)                                    
         L     R2,AITELM                                                        
         XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     LIST10                                                           
*                                                                               
LIST68   CLI   REPSUBPG,4                                                       
         BNE   *+20                                                             
         NI    RIND1,X'FF'-R1IACT                                               
         NI    RIND2,X'FF'-R2NON                                                
         NI    RIND3,X'FF'-R3GIN+R3SPT                                          
         B     LIST02                                                           
         LA    R2,REPP1                                                         
         USING LPSTD,R2                                                         
         MVC   0(L'AC@ANATS,R2),AC@ANATS                                        
         CURED DTBAND,(L'LPDR,LPDR),2,MINUS=YES                                 
         CURED DTBANC,(L'LPCR,LPCR),2,MINUS=YES                                 
         LA    R2,L'REPP1(R2)                                                   
         MVC   0(L'AC@ACCTS,R2),AC@ACCTS                                        
         CURED DTBACD,(L'LPDR,LPDR),2,MINUS=YES                                 
         CURED DTBACC,(L'LPCR,LPCR),2,MINUS=YES                                 
         GOTO1 VREPORT,REPD                                                     
         NI    RIND1,X'FF'-R1PACT                                               
         NI    RIND2,X'FF'-R2NON                                                
         NI    RIND3,X'FF'-R3GIN+R3SPT                                          
         DROP  R2                                                               
LISTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD TRANSACTION DETAILS AND SUBSIDIARY INFORMATION BLOCK          *         
*                                                                     *         
* NTRY - R3=A(TRANSACTION RECORD) - AIOA                              *         
***********************************************************************         
         SPACE 1                                                                
EXTRATX  NTR1  ,                                                                
         L     RE,ABUFF1           CLEAR PRIMARY TX DETAILS BUFFER              
         ST    RE,DETSTART         CURRENT START OF DETAILS LIST                
         L     RF,=A(BSIZE1)                                                    
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R2,ABUFF1           R2=A(PRIMARY TX DETAILS BUFFER)              
         ST    R2,DETEND           CURRENT END OF DETAILS LIST                  
*                                                                               
         USING TRNRECD,R3                                                       
         LA    R4,TRNRFST                                                       
EXTRA002 CLI   0(R4),0                                                          
         BE    EXTRA008                                                         
         CLI   0(R4),TRNELQ        44 - TRANSACTION DETAILS                     
         BE    EXTRA020                                                         
         CLI   0(R4),MDTELQ        1A - MEDIA TRANSFER DETAILS                  
         BE    EXTRA090                                                         
*&&UK*&& CLI   0(R4),NAMELQ        20 - SUNDRY CREDITOR NAME                    
*&&UK*&& BE    EXTRA100                                                         
*&&UK*&& CLI   0(R4),ADRELQ        22 - SUNDRY CREDITOR ADDRESS                 
*&&UK*&& BE    EXTRA110                                                         
         CLI   0(R4),OTHELQ        23 - SUBREFERENCE                            
         BE    EXTRA120                                                         
         CLI   0(R4),FFNELQ        25 - PURCHASE ORDER DETAILS                  
         BE    EXTRA130                                                         
*&&US*&& CLI   0(R4),PRTELQ        40 - PERSONNEL RATE DETAILS                  
*&&US*&& BE    EXTRA140                                                         
         CLI   0(R4),PXDELQ        4E - POSTING TRANSFER DETAILS                
         BE    EXTRA150                                                         
         CLI   0(R4),CPJELQ        4F - SOURCE ACCOUNT (OLD)                    
         BE    EXTRA160                                                         
         CLI   0(R4),SCIELQ        50 - SUBSIDIARY CASH DETAILS                 
         BE    EXTRA170                                                         
         CLI   0(R4),PCIELQ        51 - PROJECT CONTROL DETAILS                 
         BE    EXTRA180                                                         
*&&US*&& CLI   0(R4),SUTELQ        5F - SALES/USE TAX DETAILS                   
*&&US*&& BE    EXTRA190                                                         
*&&US*&& CLI   0(R4),TRSELQ        60 - TIMESHEET DETAILS                       
*&&US*&& BE    EXTRA200                                                         
*&&UK*&& CLI   0(R4),MPYELQ        64 - MANUAL PAYMENT DETAILS                  
*&&UK*&& BE    EXTRA210                                                         
         CLI   0(R4),ANOELQ        65 - ANALYSIS OFFICE DETAILS                 
         BE    EXTRA220                                                         
*&&UK*&& CLI   0(R4),AFCELQ        7A - FOREIGN CURRENCY DETAILS                
*&&UK*&& BE    EXTRA230                                                         
*&&UK*&& CLI   0(R4),SORELQ        7B - SOURCE ACCOUNT (NEW)                    
*&&UK*&& BE    EXTRA240                                                         
*&&US*&& CLI   0(R4),UNPELQ        7C - UNIT PRICE DETAILS                      
*&&US*&& BE    EXTRA250                                                         
*&&US*&& CLI   0(R4),UFSELQ        A2 - SPECIAL NUMBER DETAILS                  
*&&US*&& BE    EXTRA260                                                         
         CLI   0(R4),FFTELQ        DB - PAYEE NAME/TAX CODE/INV. NO.            
         BE    EXTRA270                                                         
         B     EXTRA006                                                         
*                                                                               
EXTRA004 L     RF,DETEND           REMOVE TRAILING BLANKS                       
         CR    R2,RF                                                            
         BNH   EXTRA006                                                         
         BCTR  R2,0                                                             
         B     *+10                                                             
         CR    R2,RF                                                            
         BNH   EXTRA006                                                         
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-14                                                          
         MVI   1(R2),C','          SEPERATE DETAILS WITH COMMA                  
         LA    R2,2(R2)                                                         
         ST    R2,DETEND                                                        
EXTRA006 XR    RF,RF               GET NEXT ELEMENT                             
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     EXTRA002                                                         
*                                                                               
EXTRA008 C     R2,DETSTART         ANY TX DETAILS?                              
         BNH   EXTRAX                                                           
         L     RE,ABUFF2           YES - CLEAR SECONDARY BUFFER                 
         L     RF,=A(BSIZE1)                                                    
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     RE,ABUFF2                                                        
         L     RF,DETSTART                                                      
         MVI   0(RE),C'('                                                       
         LA    RE,1(RE)                                                         
EXTRA010 CLI   0(RF),C','                                                       
         BE    EXTRA014                                                         
EXTRA012 MVC   0(1,RE),0(RF)       TRANSFER DETAILS TO SECONDARY BUFFER         
         LA    RF,1(RF)                                                         
         B     EXTRA016                                                         
EXTRA014 CLI   1(RF),C' '          REMOVE BLANKS AFTER COMMAS                   
         BH    EXTRA012                                                         
         MVC   0(1,RE),0(RF)                                                    
         LA    RF,2(RF)                                                         
EXTRA016 C     RF,DETEND                                                        
         BNL   EXTRA018                                                         
         LA    RE,1(RE)                                                         
         B     EXTRA010                                                         
EXTRA018 MVI   0(RE),C')'                                                       
         LA    RE,1(RE)                                                         
         ST    RE,DETEND                                                        
         L     RF,ABUFF2                                                        
         ST    RF,DETSTART                                                      
         B     EXTRAX                                                           
*                                                                               
         USING TRNELD,R4                                                        
EXTRA020 MVC   STATUS,TRNSTAT      SAVE STATUS DETAIL FOR TRANSACTION           
         TM    RIND3,R3SPT         IS THIS A SPLIT INVOICE ITEM?                
         BNO   EXTRA032                                                         
         NI    RIND3,X'FF'-R3MPS                                                
EXTRA022 XR    RF,RF               SEARCH FOR GINEL                             
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         CLI   TRNEL,GINELQ                                                     
         BNE   EXTRA022                                                         
         CLI   GINTYP-GINEL(R4),GINTMAC   MAIN ACCOUNTING POSTING?              
         BNE   EXTRA040                                                         
         OI    RIND3,R3MPS         YES - SET MAIN POSTING INDICATOR             
         LA    R4,TRNRFST          AND PRINT NARRATIVE                          
         B     *+12                                                             
*                                                                               
EXTRA032 TM    RIND2,R2FITEM+R2NON 1ST TX ON ITEM OR NON-INPUT BATCH?           
         BZ    EXTRA040            NO - DON'T WANT NARRATIVE                    
         XR    RF,RF                                                            
         IC    RF,TRNLN                                                         
         SH    RF,=Y(TRNLN1Q+1)    NARRATIVE LENGTH                             
         BM    EXTRA040                                                         
         EX    RF,*+8              CHECK THERE IS SOMETHING TO DISPLAY          
         B     *+10                                                             
         CLC   TRNNARR(0),BCSPACES                                              
         BNH   EXTRA040                                                         
         CLI   BATTYPE,BT06        FIXED LENGTH FOR MANUAL BILLS                
         BE    EXTRA036                                                         
         CLI   BATTYPE,BT07        AND CLIENT BILLS                             
         BE    EXTRA036                                                         
         CLI   BATTYPE,BT99        AND WFM POSTINGS                             
         BNE   *+16                                                             
EXTRA036 CH    RF,=H'14'                                                        
         BL    *+8                                                              
         LH    RF,=H'14'                                                        
         EX    RF,*+4                                                           
         MVC   0(0,R2),TRNNARR                                                  
*&&US                                                                           
         CLI   BATTYPE,BT06        BLANK OUT BILL NO. FOR MANUAL BILLS          
         BNE   EXTRA038                                                         
         EX    RF,*+4                                                           
         MVC   0(0,R2),BCSPACES                                                 
*&&                                                                             
EXTRA038 LA    RF,1(RF,R2)                                                      
         BAS   RE,EXBLANK                                                       
*                                                                               
EXTRA040 OC    TRNOFFC,TRNOFFC                                                  
         BZ    EXTRA042                                                         
         MVC   REPP1+(LPOFF-LPSTD)(L'TRNOFFC),TRNOFFC    WC/OFFICE              
*                                                                               
EXTRA042 TM    TRNSTAT,TRNSNOCM    HIGHLIGHT NON-COMMISSIONABLE WCS             
         BZ    EXTRA044                                                         
         BAS   RE,EXCOMMA                                                       
         MVC   0(L'LC@NCOM,R2),LC@NCOM                                          
         LA    RF,L'LC@NCOM-1(R2)                                               
         BAS   RE,EXBLANK                                                       
*                                                                               
EXTRA044 TM    RIND2,R2FITEM+R2NON 1ST TX ON ITEM OR NON-INPUT BATCH?           
         BZ    EXTRA004            NO - GET NEXT ELEMET                         
         TM    RIND3,R3SPT         TEST FOR A SPLIT INVOICE                     
         BZ    EXTRA046                                                         
         BAS   RE,EXCOMMA                                                       
         MVC   0(5,R2),=C'SPLIT'                                                
         LA    R2,5(R2)                                                         
*                                                                               
EXTRA046 TM    TRNSTAT,TRNSURG                                                  
         BZ    EXTRA048                                                         
         BAS   RE,EXCOMMA                                                       
         MVC   0(3,R2),=C'*U*'     URGENT TRANSACTION                           
         LA    R2,3(R2)                                                         
*                                                                               
EXTRA048 DS    0H                                                               
*&&UK                                                                           
         CLI   BATTYPE,BT72        TEST FOR AN INVOICE BATCH - BT72             
         BE    *+12                                                             
         CLI   BATTYPE,BT70        OR BT70                                      
         BNE   EXTRA050                                                         
         CLI   TRNTYPE,BT01        BILLABLE OR NON-BILLABLE?                    
         BE    EXTRA052                                                         
         B     EXTRA054                                                         
EXTRA050 CLI   BATTYPE,BT71        TEST FOR A CASH BATCH                        
         BNE   EXTRA056                                                         
         CLI   TRNTYPE,BT03        BILLABLE OR NON-BILLABLE?                    
         BE    EXTRA052                                                         
         B     EXTRA054                                                         
EXTRA052 TM    RIND3,R3SPT         SPLIT INVOICE?                               
         BNZ   EXTRA056                                                         
         BAS   RE,EXCOMMA                                                       
         MVC   0(L'AC@BLB,R2),AC@BLB                                            
         LA    R2,L'AC@BLB(R2)                                                  
         B     EXTRA056                                                         
EXTRA054 TM    RIND3,R3SPT         SPLIT INVOICE?                               
         BNZ   EXTRA004                                                         
         BAS   RE,EXCOMMA                                                       
         MVC   0(L'AC@NBLB,R2),AC@NBLB                                          
         LA    R2,L'AC@NBLB(R2)                                                 
         B     EXTRA056                                                         
*&&                                                                             
EXTRA056 B     EXTRA004                                                         
*                                                                               
         USING MDTELD,R4                                                        
EXTRA090 MVC   BOWORK1,BCSPACES                                                 
         LA    RF,BOWORK1                                                       
*&&US                                                                           
         OC    MDTGRS,MDTGRS       BILLING (GROSS)                              
         BZ    EXTRA091                                                         
         MVC   0(7,RF),=C'ABASIS='                                              
         CURED MDTGRS,(10,0(RF)),2,MINUS=YES,ALIGN=LEFT                         
         LA    RF,BOWORK1+7                                                     
         AR    RF,R0                                                            
         OC    MDTCLI,MDTCLI                                                    
         BZ    EXTRASQ                                                          
         B     *+14                                                             
*&&                                                                             
EXTRA091 OC    MDTCLI,MDTCLI                                                    
         BZ    EXTRA006                                                         
         MVC   0(L'LC@CLI,RF),LC@CLI                                            
         MVI   L'LC@CLI(RF),C'='                                                
         MVC   L'LC@CLI+1(L'MDTCLI,RF),MDTCLI                                   
         LA    RF,L'LC@CLI+L'MDTCLI+2(RF)                                       
*                                                                               
         OC    MDTPRD,MDTPRD                                                    
         BZ    EXTRA092                                                         
         MVC   0(L'LC@PRO,RF),LC@PRO                                            
         MVI   L'LC@PRO(RF),C'='                                                
         MVC   L'LC@PRO+1(L'MDTPRD,RF),MDTPRD                                   
         LA    RF,L'LC@PRO+L'MDTPRD+2(RF)                                       
*                                                                               
         OC    MDTJOB,MDTJOB                                                    
         BZ    EXTRA092                                                         
         LA    R1,AC@JOEST                                                      
         LA    RE,L'AC@JOEST-1                                                  
         CLI   BATTYPE,BT06                                                     
         BE    *+12                                                             
         CLI   BATTYPE,BT51                                                     
         BNE   *+12                                                             
         LA    R1,AC@EST                                                        
         LA    RE,L'AC@EST-1                                                    
         EX    RE,*+4                                                           
         MVC   0(0,RF),0(R1)                                                    
         LA    RF,1(RE,RF)                                                      
         MVI   0(RF),C'='                                                       
         MVC   1(L'MDTJOB,RF),MDTJOB                                            
         LA    RF,L'AC@JOEST+L'MDTJOB+2(RF)                                     
*                                                                               
*XTRA092 CLI   MDTMOS,X'99'                                                     
*        BH    EXTRASQ                                                          
EXTRA092 MVC   0(L'AC@MOS,RF),AC@MOS                                            
         MVI   L'AC@MOS(RF),C'='                                                
         LA    RF,L'AC@MOS+1(RF)                                                
         MVC   BODUB1,MDTMOS                                                    
         MVI   BODUB1+L'MDTMOS,1                                                
         GOTO1 VDATCON,BOPARM,(1,BODUB1),(6,(RF))                               
         B     EXTRASQ                                                          
*&&UK                                                                           
         USING NAMELD,R4                                                        
EXTRA100 TM    RIND3,R3SUNDRY      SUNDRY CREDITOR NAME                         
         BZ    EXTRA006                                                         
         MVC   0(L'AC@CROR,R2),AC@CROR                                          
         MVI   L'AC@CROR(R2),C'='                                               
         LA    R2,L'AC@CROR+1(R2)                                               
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         LTR   RF,RF                                                            
         BM    EXTRA006                                                         
         EX    RF,*+4                                                           
         MVC   0(0,R2),NAMEREC                                                  
         LA    R2,1(RF,R2)                                                      
         B     EXTRA004                                                         
*                                                                               
         USING ADRELD,R4                                                        
EXTRA110 TM    RIND3,R3SUNDRY      SUNDRY CREDITOR ADDRESS                      
         BZ    EXTRA006                                                         
         XR    R1,R1                                                            
         IC    R1,ADRNUM                                                        
         LTR   R1,R1                                                            
         BZ    EXTRA006                                                         
         LA    RE,ADRADD1                                                       
EXTRA112 MVC   0(L'ADRADD1,R2),0(RE)                                            
         LR    R0,R2                                                            
         LA    R2,L'ADRADD1-1(R2)                                               
         B     *+10                                                             
EXTRA114 CR    R0,R2                                                            
         BNL   EXTRA116                                                         
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,EXTRA114                                                      
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
         LA    RE,L'ADRADD1(RE)                                                 
EXTRA116 BCT   R1,EXTRA112                                                      
         BCTR  R2,0                                                             
         MVI   0(R2),C' '                                                       
         B     EXTRA004                                                         
*&&                                                                             
         USING OTHELD,R4                                                        
EXTRA120 CLI   BATTYPE,BT07        SUPRESS SUBREF FOR CREATIVE BILLING          
         BE    EXTRA006                                                         
         CLI   BATTYPE,BT26        AND FOR MEDIA/SPEC BILL                      
         BE    EXTRA006                                                         
         CLI   BATTYPE,BT33        AND FOR BILLABLE TIME BATHCES                
         BE    EXTRA006                                                         
         CLI   BATTYPE,BT51        AND FOR INTER AGENCY                         
         BE    EXTRA006                                                         
         CLI   BATTYPE,BT55        AND FOR INCOME ACCRUAL                       
         BE    EXTRA006                                                         
*&&US                                                                           
         CLI   BATTYPE,BT03        AND FOR CHEQUES IF SAME AS MAIN REF          
         BNE   *+14                                                             
         CLC   TRNKREF,OTHNUM                                                   
         BE    EXTRA006                                                         
*                                                                               
         MVC   BOWORK1,BCSPACES                                                 
         LA    RF,BOWORK1                                                       
         CLC   BCCPYPRD,TRNKULC    TEST FOR PRODUCTION LEDGER                   
         BNE   EXTRA006                                                         
         XR    RE,RE                                                            
         MVC   0(L'LC@CLI,RF),LC@CLI                                            
         MVI   L'LC@CLI(RF),C'='                                                
         IC    RE,BCCLILEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   L'LC@CLI+1(0,RF),TRNKCACT                                        
         LA    RF,L'LC@CLI+3(RE,RF)                                             
         LA    R1,OTHNUM                                                        
         IC    RE,BCPROLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         CLC   0(0,R1),BCSPACES                                                 
         BNH   EXTRASQ                                                          
         MVC   0(L'LC@PRO,RF),LC@PRO                                            
         MVI   L'LC@PRO(RF),C'='                                                
         EX    RE,*+4                                                           
         MVC   L'LC@PRO+1(0,RF),0(R1)                                           
         LA    RF,L'LC@PRO+3(RE,RF)                                             
         IC    RE,BCJOBLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         CLC   6(0,R1),BCSPACES                                                 
         BNH   EXTRASQ                                                          
         MVC   0(L'LC@JOB,RF),LC@JOB                                            
         MVI   L'LC@JOB(RF),C'='                                                
         EX    RE,*+4                                                           
         MVC   L'LC@JOB+1(0,RF),6(R1)                                           
         B     EXTRASQ                                                          
*&&                                                                             
*&&UK                                                                           
         TM    RIND3,R3MPS         MAIN ACC GINEL POSTING?                      
         BO    *+12                YES - PRINT SUBREF                           
         TM    RIND2,R2FITEM       SHOW SUBREF ON FIRST TX ONLY                 
         BZ    EXTRA006                                                         
         MVC   0(L'LC@SUBR,R2),LC@SUBR                                          
         LA    RF,L'LC@SUBR-1(R2)                                               
         BAS   RE,EXBLANK                                                       
         MVI   0(R2),C'='                                                       
         MVC   1(L'OTHNUM-2,R2),OTHNUM                                          
         LA    R2,L'OTHNUM-1(R2)                                                
         B     EXTRA004                                                         
*&&                                                                             
         USING FFNELD,R4                                                        
*&&UK                                                                           
EXTRA130 MVC   0(2,R2),=C'#='                                                   
         MVC   2(L'FFNONUM,R2),FFNONUM                                          
         LA    R2,L'FFNONUM+2(R2)                                               
*&&                                                                             
*&&US                                                                           
EXTRA130 MVC   0(4,R2),=C'PO#='                                                 
         MVC   4(L'FFNONUM,R2),FFNONUM  ORDER NUMBER                            
         LA    R2,L'FFNONUM+4(R2)                                               
*&&                                                                             
         CLI   FFNSTAT,FFNSPRTQ                                                 
         BNE   EXTRA004                                                         
         MVI   0(R2),FFNSPRTQ                                                   
         LA    R2,1(R2)                                                         
         B     EXTRA004                                                         
*&&US                                                                           
         USING PRTELD,R4                                                        
EXTRA140 MVC   0(L'LC@TIME,R2),LC@TIME                                          
         LA    R2,L'LC@TIME(R2)                                                 
         MVC   0(2,R2),=C'=B'      BILLABLE                                     
         TM    PRTSTAT,PRTSNOTQ    NON-BILLABLE                                 
         BZ    *+8                                                              
         MVI   1(R2),C'N'                                                       
         TM    PRTSTAT,PRTSRTEQ    SPECIAL NON-BILLABLE                         
         BZ    *+8                                                              
         MVI   1(R2),C'R'                                                       
         TM    PRTSTAT,PRTSNOTQ    SKIP RATE IF NON-BILLABLE                    
         BNO   EXTRA142                                                         
*        CLI   SDPROF+4,YES        PRINT RATE ?                                 
*        BE    *+12                                                             
         LA    R2,2(R2)                                                         
         B     EXTRA004                                                         
*                                                                               
EXTRA142 MVC   3(1,R2),AC@RATE                                                  
         MVI   4(R2),C'='                                                       
         LA    R2,5(R2)                                                         
         CURED PRTRATE,(8,(R2)),2,MINUS=YES,ALIGN=LEFT                          
         AR    R2,R0                                                            
         TM    PRTSTAT,X'01'       WAS RATE ADJUSTED ?                          
         BZ    EXTRA004            NO                                           
         MVI   0(R2),C'A'                                                       
         B     EXTRA004                                                         
*&&                                                                             
         USING PXDELD,R4                                                        
EXTRA150 MVC   BOWORK1,BCSPACES                                                 
         LA    RF,BOWORK1                                                       
         MVC   0(L'AC@TO,RF),AC@TO                                              
         LA    RE,L'AC@TO(RF)                                                   
         CLI   PXDTYPE,PXDTTO                                                   
         BE    *+14                                                             
         MVC   0(L'AC@FROM,RF),AC@FROM                                          
         LA    RE,L'AC@FROM(RF)                                                 
         MVC   1(L'PXDFRTOA,RE),PXDFRTOA                                        
         B     EXTRASQ                                                          
*                                                                               
         USING CPJELD,R4                                                        
EXTRA160 MVC   BOWORK1,BCSPACES                                                 
         LA    RF,BOWORK1                                                       
         MVI   0(RF),C'S'          SOURCE ACCOUNT (OLD)                         
         MVC   1(L'CPJTYPE,RF),CPJTYPE                                          
         CLI   CPJTYPE,CPJTOTH     CHANGE TYPE 'OTHER' FROM 'X' TO 'O'          
         BNE   *+8                                                              
         MVI   1(RF),C'O'                                                       
         MVI   1+L'CPJTYPE(RF),C'='                                             
         LA    RF,L'CPJTYPE+2(RF)                                               
         XR    R1,R1                                                            
         IC    R1,CPJLN                                                         
         SH    R1,=Y(CPJDATA-CPJELD+1)                                          
         EX    R1,*+4                                                           
         MVC   0(0,RF),CPJCLI                                                   
         CLI   CPJTYPE,CPJTJOB                                                  
         BNE   EXTRA162                                                         
         IC    R1,CPJLN                                                         
         SH    R1,=Y(CPJWRK-CPJELD)                                             
         BNP   EXTRA162                                                         
         CLC   CPJWRK,BCSPACES                                                  
         BNH   EXTRA162                                                         
         MVC   CPJCPJLQ(L'CPJWRK,RF),BCSPACES                                   
         MVC   CPJCPJLQ+1(L'CPJWRK,RF),CPJWRK                                   
EXTRA162 EQU   *                                                                
*&&US                                                                           
         CLI   CPJTYPE,CPJTCOM     IF COMMERCIAL                                
         BNE   EXTRASQ                                                          
         CLI   CPJCOMU,C'T'        AND UNIT=T,SHOW DPS COMPANY CODE             
         BNE   EXTRASQ                                                          
         GOTO1 VHEXOUT,BOPARM,CPJCOML,CPJCOML-CPJDATA(RF),1                     
*&&                                                                             
         B     EXTRASQ                                                          
*                                                                               
         USING SCIELD,R4                                                        
EXTRA170 DS    0H                                                               
EXTRA172 MVC   0(L'SCITYPE,R2),SCITYPE  SUBSIDIARY CASH                         
         MVI   L'SCITYPE(R2),C'='                                               
*&&UK*&& LA    R2,L'SCITYPE+1(R2)                                               
*&&US                                                                           
         CLI   SCITYPE,SCITCDSC                                                 
         BE    *+12                                                             
         LA    R2,L'SCITYPE+1(R2)                                               
         B     EXTRA174                                                         
         MVC   0(L'LC$CSHDS,R2),LC$CSHDS                                        
         MVI   L'LC$CSHDS(R2),C'='                                              
         LA    R2,L'LC$CSHDS+1(R2)                                              
*                                                                               
EXTRA174 DS    0H                                                               
*&&                                                                             
*&&UK                                                                           
         CLI   SCITYPE,SCITMILE    MILEAGE STORED IN WHOLE UNITS                
         BNE   *+10                                                             
         SRP   SCIAMNT,2,0                                                      
*&&                                                                             
         CURED SCIAMNT,(13,0(R2)),2,MINUS=YES,ALIGN=LEFT                        
         AR    R2,R0                                                            
         B     EXTRA004                                                         
*                                                                               
         USING PCIELD,R4                                                        
EXTRA180 CLI   PCILN,PCILN2Q                                                    
         BL    EXTRA006                                                         
         XR    RE,RE                                                            
         MVC   BOWORK1,BCSPACES                                                 
         LA    RF,BOWORK1                                                       
*                                                                               
         IC    RE,BCCLILEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         CLC   PCIPRJT+3(0),BCSPACES                                            
         BNH   EXTRA006                                                         
         MVC   0(L'LC@CLI,RF),LC@CLI                                            
         MVI   L'LC@CLI(RF),C'='                                                
         EX    RE,*+4                                                           
         MVC   L'LC@CLI+1(0,RF),PCIPRJT+3                                       
         LA    RF,L'LC@CLI+3(RE,RF)                                             
*                                                                               
         IC    RE,BCPROLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         CLC   PCIPRJT+6(0),BCSPACES                                            
         BNH   EXTRASQ                                                          
         MVC   0(L'LC@PRO,RF),LC@PRO                                            
         MVI   L'LC@PRO(RF),C'='                                                
         EX    RE,*+4                                                           
         MVC   L'LC@PRO+1(0,RF),PCIPRJT+6                                       
         LA    RF,L'LC@PRO+3(RE,RF)                                             
*                                                                               
         IC    RE,BCJOBLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         CLC   PCIPRJT+9(0),BCSPACES                                            
         BNH   EXTRASQ                                                          
         MVC   0(L'LC@JOB,RF),LC@JOB                                            
         MVI   L'LC@JOB(RF),C'='                                                
         EX    RE,*+4                                                           
         MVC   L'LC@JOB+1(0,RF),PCIPRJT+9                                       
         LA    RF,L'LC@JOB+3(RE,RF)                                             
*                                                                               
         CLC   PCITSK,BCSPACES                                                  
         BNH   EXTRASQ                                                          
         MVC   0(L'AC@TASK,RF),AC@TASK                                          
         MVI   L'AC@TASK(RF),C'='                                               
         MVC   L'AC@TASK+1(L'PCITSK,RF),PCITSK                                  
         B     EXTRASQ                                                          
*                                                                               
         USING SUTELD,R4                                                        
EXTRA190 TM    STATUS,TRNSDR       CREDITS ONLY                                 
         BO    EXTRA006                                                         
         MVC   0(L'AC@BASIS,R2),AC@BASIS                                        
         LA    R2,L'AC@BASIS(R2)                                                
         MVI   0(R2),C'='                                                       
         CURED SUTBAS,(10,1(R2)),2,MINUS=YES,ALIGN=LEFT                         
         AR    R2,R0                                                            
         MVC   2(L'AC@RATE,(R2)),AC@RATE                                        
         LA    R2,L'AC@RATE+2(R2)                                               
         MVI   0(R2),C'='                                                       
         CURED SUTRTE,(8,1(R2)),4,ALIGN=LEFT                                    
         AR    R2,R0                                                            
         MVC   2(L'AC@LOC,(R2)),AC@LOC                                          
         MVI   L'AC@LOC+2(R2),C'='                                              
         MVC   L'AC@LOC+3(8,(R2)),SUTLOC+3                                      
         LA    R2,L'AC@LOC+11(R2)                                               
         B     EXTRA004                                                         
*&&US                                                                           
         USING TRSELD,R4                                                        
EXTRA200 TM    TRSSTAT2,TRSSTIME+TRSSTADJ+TRSSTMSS                              
         BZ    EXTRA006                                                         
         MVC   0(L'LC@TMSTT,R2),LC@TMSTT                                        
         LA    R2,L'LC@TMSTT(R2)                                                
         MVC   0(2,R2),=C'=T'      REGULAR                                      
         TM    TRSSTAT2,TRSSTADJ   ADJUSTMENT                                   
         BZ    *+8                                                              
         MVI   1(R2),C'A'                                                       
         TM    TRSSTAT2,TRSSTMSS   MISSING                                      
         BZ    *+8                                                              
         MVI   1(R2),C'M'                                                       
         LA    R2,2(R2)                                                         
         B     EXTRA004                                                         
*&&                                                                             
*&&UK                                                                           
         USING MPYELD,R4                                                        
EXTRA210 CLI   BATTYPE,BT70        INVOICE BATCHES ONLY                         
         BNE   EXTRA006                                                         
         CLC   MPYBNK,BCSPACES                                                  
         BNH   EXTRA006                                                         
         MVC   0(L'LC@CHK,R2),LC@CHK  CHEQUE NUMBER/DATE                        
         MVI   L'LC@CHK(R2),C'='                                                
         MVC   L'LC@CHK+1(L'MPYNO,R2),MPYNO                                     
         LA    R2,L'LC@CHK+L'MPYNO+1(R2)                                        
         MVI   0(R2),C'/'                                                       
         GOTO1 VDATCON,BOPARM,(X'82',MPYDTE),(17,1(R2))                         
         XR    RF,RF                                                            
         IC    RF,4(R1)                                                         
         LA    R2,1(RF,R2)                                                      
         B     EXTRA004                                                         
*&&                                                                             
         USING ANOELD,R4                                                        
EXTRA220 CLI   BATTYPE,BT21        EXPENSE TRANSFERS                            
         BE    *+12                                                             
         CLI   BATTYPE,BT22        CASH RECEIPTS                                
         BNE   EXTRA006                                                         
         MVC   0(L'AC@ANLC,R2),AC@ANLC  ANALYSIS CODE                           
         LA    R2,L'AC@ANLC(R2)                                                 
         MVI   0(R2),C'='                                                       
         MVC   1(L'ANOOFFC,R2),ANOOFFC                                          
         LA    R2,L'ANOOFFC+1(R2)                                               
         B     EXTRA004                                                         
*&&UK                                                                           
         USING AFCELD,R4                                                        
EXTRA230 GOTO1 VBLDCUR,BOPARM,AFCCURR,(X'00',BOWORK1),ACOM                      
         CLI   0(R1),0                                                          
         BNE   EXTRA006                                                         
         MVC   0(L'AC@CURRY,R2),AC@CURRY  CURRENCY                              
         LA    R2,L'AC@CURRY(R2)                                                
         MVI   0(R2),C'='                                                       
         CURED AFCAMNT,(15,1(R2)),BOWORK1,MINUS=YES,ALIGN=LEFT,        X        
               CURSYMB=YES                                                      
         AR    R2,R0                                                            
         LA    R2,1(R2)                                                         
         TM    RIND2,R2FITEM       FIRST POSTING ONLY                           
         BZ    EXTRA004                                                         
         MVC   1(2,R2),=C'ER'      EXCHANGE RATE                                
         LA    R2,3(R2)                                                         
         MVI   0(R2),C'='                                                       
         XC    BODUB1,BODUB1                                                    
         MVO   BODUB1,AFCXRATE                                                  
         OI    BODUB1+L'BODUB1-1,X'0C' CONVERT TO PACKED                        
         CURED (P8,BODUB1),(10,1(R2)),5,ALIGN=LEFT                              
         AR    R2,R0                                                            
         LA    R2,1(R2)                                                         
         LA    RF,10(R2)                                                        
EXTRA232 CLI   0(RF),C','          DROP TRAILING ZEROS                          
         BE    *+8                                                              
         CLI   0(RF),C'.'                                                       
         BNE   *+12                                                             
         MVI   1(RF),C'0'                                                       
         B     EXTRA004                                                         
         CLI   0(RF),C'0'                                                       
         BH    EXTRA004                                                         
         MVI   0(RF),C' '                                                       
         BCT   RF,EXTRA232                                                      
         B     EXTRA004                                                         
*                                                                               
         USING SORELD,R4                                                        
EXTRA240 CLI   SORSYS,SORSACC      SOURCE A/C (NEW)                             
         BNE   EXTRA006                                                         
         CLC   =C'SJ',SORAUNT                                                   
         BE    EXTRA242                                                         
         CLC   =C'SE',SORAUNT                                                   
         BE    EXTRA242                                                         
         MVC   0(2,R2),=C'SO'                                                   
         MVI   2(R2),C'='                                                       
         MVC   3(L'SORAULA,R2),SORAULA                                          
         LA    R2,3+L'SORAULA(R2)                                               
         B     EXTRA004                                                         
EXTRA242 MVC   0(L'SORAUNT+L'SORALDG,R2),SORAUNT                                
         MVI   L'SORAUNT+L'SORALDG(R2),C'='                                     
         MVC   L'SORAUNT+L'SORALDG+1(L'SORAACT,R2),SORAACT                      
         LA    R2,L'SORAUNT+L'SORALDG+L'SORAACT+1(R2)                           
         B     EXTRA004                                                         
*&&                                                                             
*&&US                                                                           
         USING UNPELD,R4                                                        
EXTRA250 MVC   0(2,R2),=C'U='    UNIT PRICING                                   
         CURED UNPUNIT,(6,2(R2)),0,MINUS=YES,ALIGN=LEFT                         
         AR    R2,R0                                                            
         LA    R2,3(R2)                                                         
         MVC   0(2,R2),=C'P='                                                   
         CURED UNPRICE,(8,2(R2)),2,MINUS=YES,ALIGN=LEFT                         
         AR    R2,R0                                                            
         LA    R2,2(R2)                                                         
         B     EXTRA004                                                         
*                                                                               
         USING UFSELD,R4                                                        
EXTRA260 CLC   =C'SN',UFSCODE    "SPECIAL NUMBER" INDICATOR                     
         BE    *+14                                                             
         CLC   =C'E2',UFSCODE    AOR                                            
         BNE   EXTRA006                                                         
         XR    R1,R1                                                            
         IC    R1,UFSLN                                                         
         SH    R1,=Y(UFSLN1Q+1)                                                 
         BM    EXTRA006                                                         
         MVC   0(L'UFSCODE,R2),UFSCODE                                          
         MVI   L'UFSCODE(R2),C'='                                               
         EX    R1,*+4                                                           
         MVC   L'UFSCODE+1(0,R2),UFSDATA                                        
         LA    R2,L'UFSCODE+2(R1,R2)                                            
         B     EXTRA004                                                         
*&&                                                                             
*                                                                               
         USING FFTELD,R4                                                        
EXTRA270 CLI   FFTTYPE,FFTTINVN    SUPPLIER INVOICE NUMBER                      
         BNE   EXTRA272                                                         
         XR    RF,RF                                                            
         ICM   RF,1,FFTDLEN                                                     
         BZ    EXTRA006                                                         
         MVC   0(L'AC@INV,R2),AC@INV                                            
         MVI   L'AC@INV(R2),C'='                                                
         LA    R2,L'AC@INV+1(R2)                                                
         B     EXTRA276                                                         
EXTRA272 CLI   FFTTYPE,FFTTTAXI    TAX CODE (DUTCH SE ACCOUNTS)                 
         BNE   EXTRA274                                                         
         XR    RF,RF                                                            
         ICM   RF,1,FFTDLEN                                                     
         BZ    EXTRA006                                                         
         MVC   0(L'LC@VPB,R2),LC@VPB                                            
         MVI   L'LC@VPB(R2),C'='                                                
         LA    R2,L'LC@VPB+1(R2)                                                
         B     EXTRA276                                                         
EXTRA274 CLI   BATTYPE,BT71        CASH BATCHES ONLY                            
         BNE   EXTRA006                                                         
         TM    RIND2,R2FITEM       FIRST POSTING ONLY                           
         BZ    EXTRA006                                                         
         CLI   FFTTYPE,FFTTPNAM    PAYEE NAME                                   
         BNE   EXTRA006                                                         
         XR    RF,RF                                                            
         ICM   RF,1,FFTDLEN                                                     
         BZ    EXTRA006                                                         
         MVC   0(L'AC@PAYEE,R2),AC@PAYEE                                        
         MVI   L'AC@PAYEE(R2),C'='                                              
         LA    R2,L'AC@PAYEE+1(R2)                                              
EXTRA276 BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R2),FFTDATA                                                  
         LA    R2,1(RF,R2)                                                      
         B     EXTRA004                                                         
*                                                                               
EXCOMMA  C     R2,DETEND           TEST COMMA REQUIRED                          
         BER   RE                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BR    RE                                                               
*                                                                               
EXBLANK  CLI   0(RF),C' '          REMOVE TRAILING BLANKS                       
         BH    *+12                                                             
         CR    RF,R2                                                            
         BNHR  RE                                                               
         BCT   RF,*-12                                                          
         LA    R2,1(RF)                                                         
         BR    RE                                                               
*                                                                               
EXTRASQ  GOTO1 VSQUASH,BOPARM,BOWORK1,L'BOWORK1                                 
         XR    RF,RF                                                            
         IC    RF,7(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R2),BOWORK1                                                  
         LA    R2,1(RF,R2)                                                      
         B     EXTRA004                                                         
*                                                                               
EXTRAX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PASS GROUP INVOICE TRANSACTIONS DISCRETELY                          *         
* NTRY - R2=A(GROUP INVOICE ELEMENT)                                  *         
* EXIT - AIOA=GROUP INVOICE TRANSACTION RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
PASSGIN  NTR1  ,                                                                
         NI    RIND3,X'FF'-R3SPT                                                
         MVC   IOKEY,BATIKEY       READ THE BATCH ITEM RECORD                   
         L     R1,=A(IORD+IOACCDIR+IO9)                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO9                                                          
         USING TBARECD,R4                                                       
         LA    R4,TBARFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING SFSELD,R4           LOOK FOR SCREEN FIELD SAVE ELEMENT           
PGIN02   CLI   SFSEL,0                                                          
         BE    PGIN06                                                           
         CLI   SFSEL,SFSELQ                                                     
         BNE   PGIN04                                                           
         CLI   SFSFIELD,C'*'       IS THIS A SPLIT INVOICE?                     
         BNE   PGIN06                                                           
         OI    RIND3,R3SPT         YES - SET INDICATOR                          
         B     PGIN06                                                           
PGIN04   IC    R0,SFSLN                                                         
         AR    R4,R0                                                            
         B     PGIN02                                                           
*                                                                               
PGIN06   TM    RIND3,R3GIN                                                      
         BZ    PGIN08                                                           
         MVC   IOKEY,LGINPKEY                                                   
         L     R1,=A(IORD+IOACCDIR+IO9)                                         
         GOTO1 AIO                RE-READ TO ESTABLISH SEQUENCE                 
         BE    PGIN10                                                           
         DC    H'0'                                                             
PGIN08   LA    RF,IOKEY           BUILD GROUP INVOICE PASSIVE KEY               
         USING GINPASD,RF                                                       
         XC    GINPKEY,GINPKEY                                                  
         MVI   GINPTYP,GINPTYPQ                                                 
         MVC   GINPCPY,CUABIN                                                   
         MVC   GINPINV,GININV-GINELD(R2)                                        
         L     R1,=A(IOHI+IOACCDIR+IOA)                                         
         B     *+8                                                              
PGIN10   L     R1,=A(IOSEQ+IOACCDIR+IOA)                                        
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(GINPISN-GINPKEY),IOKEYSAV                                  
         BE    *+12                                                             
         NI    RIND3,X'FF'-R3GIN                                                
         B     PGINX                                                            
         L     R1,=A(IOGET+IOACCMST+IOA)                                        
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LGINPKEY,IOKEY     SAVE PASSIVE KEY                              
         L     RF,AIOA                                                          
         MVC   LGINAKEY,0(RF)                                                   
         OI    RIND3,R3GIN                                                      
PGINX    B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
RFTAB    DS    0X                      REPORT FORMAT TABLE                      
         DC    AL1(RFOSUM),AL2(RPTOSUMH-TWAD)                                   
RFTABQ   EQU   *-RFTAB                                                          
         DC    AL1(RFLSUM),AL2(RPTLSUMH-TWAD)                                   
*&&UK*&& DC    AL1(RFCSUM),AL2(RPTCSUMH-TWAD)                                   
         DC    AL1(RFILST),AL2(RPTILSTH-TWAD)                                   
         DC    AL1(RFPLST),AL2(RPTPLSTH-TWAD)                                   
RFTABN   EQU   (*-RFTAB)/RFTABQ                                                 
         SPACE 1                                                                
RFOSUM   EQU   X'80'               OFFICE SUMMARY                               
RFLSUM   EQU   X'40'               LEDGER SUMMARY                               
*&&UK                                                                           
RFCSUM   EQU   X'20'               CURRENCY SUMMARY                             
*&&                                                                             
RFILST   EQU   X'10'               LIST OF ITEMS                                
RFPLST   EQU   X'08'               LIST OF POSTINGS                             
         SPACE 1                                                                
ANAUNIT  DC    C'1',C'2'           ANALYSIS UNITS                               
         DC    X'00'                                                            
         SPACE 1                                                                
DEFCURT  DS    0H                                                               
       ++INCLUDE ACCURTAB                                                       
DEFCURTL EQU   4                                                                
         EJECT                                                                  
***********************************************************************         
* UPPER CASE DICTIONARY LIST                                          *         
***********************************************************************         
         SPACE 1                                                                
DDIN1    DCDDL AC#MOS,L'AC@MOS                                                  
         DC    X'00'                                                            
         SPACE 1                                                                
***********************************************************************         
* MIXED CASE DICTIONARY LIST                                          *         
***********************************************************************         
         SPACE 1                                                                
DDIN2    DCDDL AC#PMREP,L'AC@PMREP                                              
         DCDDL AC#TOTLS,L'AC@TOTLS                                              
         DCDDL AC#ANATS,L'AC@ANATS                                              
         DCDDL AC#ACCTS,L'AC@ACCTS                                              
         DCDDL AC#BLB,L'AC@BLB                                                  
         DCDDL AC#NBLB,L'AC@NBLB                                                
         DCDDL AC#CURRY,L'AC@CURRY                                              
         DCDDL AC#DEL,L'AC@DEL                                                  
         DCDDL AC#JOEST,L'AC@JOEST                                              
         DCDDL AC#EST,L'AC@EST                                                  
         DCDDL AC#CROR,L'AC@CROR                                                
         DCDDL AC#TO,L'AC@TO                                                    
         DCDDL AC#FROM,L'AC@FROM                                                
         DCDDL AC#TASK,L'AC@TASK                                                
         DCDDL AC#BASIS,L'AC@BASIS                                              
         DCDDL AC#RATE,L'AC@RATE                                                
         DCDDL AC#LOC,L'AC@LOC                                                  
         DCDDL AC#ANLC,L'AC@ANLC                                                
         DCDDL AC#PAYEE,L'AC@PAYEE                                              
         DCDDL AC#INV,L'AC@INV                                                  
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REPORT SPECS                                                        *         
***********************************************************************         
         SPACE 1                                                                
PMRSPECS DS    0X                                                               
         SPROG 1,2,3,4,5                                                        
         SPEC  H1,1,RUN                                                         
         SPEC  H1,71,PAGE                                                       
         SPROG 1                                                                
         SPEC  H1,32,AC#OFFSU,19                                                
         SPEC  H2,32,AC#OFFSU,19,LU                                             
         SPEC  M1,1,AC#OFF,6                                                    
         SPEC  M2,1,AC#OFF,6,LU                                                 
         SPROG 2                                                                
         SPEC  H1,32,AC#LGRSU,19                                                
         SPEC  H2,32,AC#LGRSU,19,LU                                             
         SPEC  M1,1,AC#LGR,6                                                    
         SPEC  M2,1,AC#LGR,6,LU                                                 
         SPEC  M1,41,AC#DRS,13,R                                                
         SPEC  M2,41,AC#DRS,13,RU                                               
         SPEC  M1,65,AC#CRS,13,R                                                
         SPEC  M2,65,AC#CRS,13,RU                                               
         SPROG 3                                                                
         SPEC  H1,32,AC#CURRS,19                                                
         SPEC  H2,32,AC#CURRS,19,LU                                             
         SPEC  M1,1,AC#CURRY,8                                                  
         SPEC  M2,1,AC#CURRY,8,LU                                               
         SPROG 1,3                                                              
         SPEC  M1,35,AC#DRS,13,R                                                
         SPEC  M2,35,AC#DRS,13,RU                                               
         SPEC  M1,50,AC#CRS,13,R                                                
         SPEC  M2,50,AC#CRS,13,RU                                               
         SPEC  M1,65,AC#BAL,13,R                                                
         SPEC  M2,65,AC#BAL,13,RU                                               
         SPROG 4                                                                
         SPEC  H1,33,AC#LSTIT,17                                                
         SPEC  H2,33,AC#LSTIT,17,LU                                             
         SPEC  M1,1,AC#ITEM,6                                                   
         SPEC  M2,1,AC#ITEM,6,LU                                                
         SPEC  M1,8,AC#REFN,6                                                   
         SPEC  M2,8,AC#REFN,6,LU                                                
         SPEC  M1,15,AC#DATE,8                                                  
         SPEC  M2,15,AC#DATE,8,LU                                               
         SPEC  M1,24,AC#AMT,13,R                                                
         SPEC  M2,24,AC#AMT,13,RU                                               
         SPEC  M1,39,AC#ACC1,14                                                 
         SPEC  M2,39,AC#ACC1,14,LU                                              
         SPEC  M1,54,AC#ACC2,14                                                 
         SPEC  M2,54,AC#ACC2,14,LU                                              
         SPEC  M1,69,AC#STT,7                                                   
         SPEC  M2,69,AC#STT,7,LU                                                
         SPROG 5                                                                
         SPEC  H1,32,AC#LSTPS,19                                                
         SPEC  H2,32,AC#LSTPS,19,LU                                             
         SPEC  M1,1,AC#ACC,8                                                    
         SPEC  M2,1,AC#ACC,8,LU                                                 
         SPEC  M1,16,AC#CTRA,10                                                 
         SPEC  M2,16,AC#CTRA,10,LU                                              
         SPEC  M1,30,AC#OFF,3                                                   
         SPEC  M2,30,AC#OFF,3,LU                                                
         SPEC  M1,34,AC#DATE,8                                                  
         SPEC  M2,34,AC#DATE,8,LU                                               
         SPEC  M1,43,AC#REFN,6                                                  
         SPEC  M2,43,AC#REFN,6,LU                                               
         SPEC  M1,50,AC#DRS,13,R                                                
         SPEC  M2,50,AC#DRS,13,RU                                               
         SPEC  M1,65,AC#CRS,13,R                                                
         SPEC  M2,65,AC#CRS,13,RU                                               
PLSTSX   DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* LIST OF POSTINGS LINE DSECT                                         *         
***********************************************************************         
         SPACE 1                                                                
LPSTD    DSECT                                                                  
LPACC    DS    CL14                ACCOUNT                                      
         DS    CL1                                                              
LPCAC    DS    CL14                CONTRA-ACCOUNT                               
         DS    CL1                                                              
LPOFF    DS    CL2                 OFFICE CODE                                  
         DS    CL1                                                              
LPDATE   DS    CL8                 TRANSACTION DATE                             
         DS    CL1                                                              
LPREF    DS    CL6                 REFERENCE NUMBER                             
         DS    CL1                                                              
LPDR     DS    CL14                DEBITS                                       
         DS    CL1                                                              
LPCR     DS    CL14                CREDITS                                      
         SPACE 1                                                                
***********************************************************************         
* LIST OF ITEMS LINE DSECT                                            *         
***********************************************************************         
         SPACE 1                                                                
LITED    DSECT                                                                  
LINO     DS    CL3                 NUMBER                                       
         DS    CL4                                                              
LIREF    DS    CL6                 REFERENCE                                    
         DS    CL1                                                              
LIDATE   DS    CL8                 DATE                                         
         DS    CL1                                                              
LIAMT    DS    CL14                AMOUNT                                       
         DS    CL1                                                              
LIACC1   DS    CL14                ACCOUNT#1                                    
         DS    CL1                                                              
LIACC2   DS    CL14                ACCOUNT#2                                    
         DS    CL1                                                              
LISTA    DS    CL7                                                              
         EJECT                                                                  
***********************************************************************         
* LEDGER SUMMARY LINE DSECT                                           *         
***********************************************************************         
         SPACE 1                                                                
LSUMD    DSECT                                                                  
LSUL     DS    CL2                 UNIT/LEDGER                                  
         DS    CL1                                                              
LSNAME   DS    CL36                LEDGER NAME                                  
         DS    CL1                                                              
LSDR     DS    CL14                DEBITS                                       
         DS    CL10                                                             
LSCR     DS    CL14                CREDITS                                      
         SPACE 1                                                                
***********************************************************************         
* OFFICE/CURRENCY SUMMARY LINE                                        *         
***********************************************************************         
         SPACE 1                                                                
OSUMD    DSECT                                                                  
OSOFF    DS    CL2                 OFFICE CODE                                  
         DS    CL1                                                              
OSOFFN   DS    CL30                OFFICE NAME                                  
         DS    CL1                                                              
         ORG   OSOFF                                                            
OSCUR    DS    CL3                 CURRENCY CODE                                
         DS    CL1                                                              
OSCURN   DS    CL29                CURRENCY NAME                                
         DS    CL1                                                              
OSDR     DS    CL14                DEBITS                                       
         DS    CL1                                                              
OSCR     DS    CL14                CREDITS                                      
         DS    CL1                                                              
OSBAL    DS    CL14                BALANCE                                      
         EJECT                                                                  
**********************************************************************          
* BUFFER DSECTS                                                      *          
**********************************************************************          
         SPACE 1                                                                
LTABD    DSECT                     ** LEDGER SUMMARY TABLE **                   
LTNTRY   DS    0X                                                               
LTUL     DS    0CL2                UNIT/LEDGER                                  
LTUNIT   DS    CL1                 UNIT                                         
LTLEDGER DS    CL1                 LEDGER                                       
LTDRS    DS    PL8                 DEBITS                                       
LTCRS    DS    PL8                 CREDITS                                      
LTLAMT   EQU   *-LTCRS                                                          
LTLNQ    EQU   *-LTNTRY                                                         
         SPACE 1                                                                
OTABD    DSECT                     ** OFFICE/CURRENCY SUMMARY TABLE **          
OTNTRY   DS    0X                                                               
OTOFF    DS    CL2                 OFFICE CODE                                  
         DS    CL1                                                              
         ORG   OTOFF                                                            
OTCUR    DS    CL3                 CURRENCY CODE                                
OTDRS    DS    PL8                 DEBITS                                       
OTCRS    DS    PL8                 CREDITS                                      
OTLAMT   EQU   *-OTCRS                                                          
OTLNQ    EQU   *-OTNTRY                                                         
         EJECT                                                                  
***********************************************************************         
* INCLUDED DSECTS                                                     *         
***********************************************************************         
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACBATWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBATWORK                                                      
         PRINT ON                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BASOLY1H                                                         
       ++INCLUDE ACBATA2D                                                       
         ORG   OSVALS                                                           
RPIND    DS    XL1                 REPORT PRINTING INDICATOR                    
RPIOLD   EQU   X'80'               OLD FORMAT DISPLAYED                         
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                     OVERLAY W/S                                  
         ORG   OVERWRK                                                          
         SPACE 1                                                                
DMCB     DS    6D                                                               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
*                                                                               
ABUFF1   DS    A                   A(BUFFER 1)                                  
ABUFF2   DS    A                   A(BUFFER 2)                                  
ABUFF3   DS    A                   A(BUFFER 3)                                  
ABUFF4   DS    A                   A(BUFFER 4)                                  
DETSTART DS    A                   TX DETAILS START                             
DETEND   DS    A                   TX DETAILS END                               
SPLIT    DS    A                   DUMMY LINE BREAK                             
AITELM   DS    A                   CURRENT ITEM ELEMENT                         
ATRNREC  DS    A                   TRANSACTION RECORD                           
BATIKEY  DS    XL(L'IOKEY)         BATCH ITEM KEY                               
LGINAKEY DS    XL(L'IOKEY)         LAST GROUP INVOICE ACTIVE KEY                
LGINPKEY DS    XL(L'IOKEY)         LAST GROUP INVOICE PASSIVE KEY               
*                                                                               
SVRFORM  DS    XL(L'CSRFORM)       SAVED CSRFORM VALUE                          
*                                                                               
RIND1    DS    XL1                 REPORT FLAG 1                                
R1OACT   EQU   X'80'               OFFICE SUMMARY ACTIVE                        
R1LACT   EQU   X'40'               LEDGER SUMMARY ACTIVE                        
R1CACT   EQU   X'20'               CURRENCY SUMMARY ACTIVE                      
R1IACT   EQU   X'10'               LIST OF ITEMS ACTIVE                         
R1PACT   EQU   X'08'               LIST OF POSTINGS ACTIVE                      
RIND2    DS    XL1                 REPORT FLAG 2                                
R2FITEM  EQU   X'80'               FIRST TRANSACTION ON ITEM                    
R2NON    EQU   X'40'               NON-INPUT BATCH                              
RIND3    DS    XL1                 REPORT FLAG 3                                
R3ANAL   EQU   X'80'               ANALYSIS TRANSACTION                         
R3SUNDRY EQU   X'40'               SUNDRY CREDITOR ACCOUNT                      
R3GIN    EQU   X'20'               GROUP INVOICE TRANSACTION                    
R3MPS    EQU   X'08'               MAIN ACC GINEL POSTING                       
R3SPT    EQU   X'04'               SPLIT INVOICE TRANSACTION                    
*                                                                               
DTBACD   DS    PL8                 ACCOUNT DEBITS                               
DTBACC   DS    PL8                 ACCOUNT CREDITS                              
DTBAND   DS    PL8                 ANALYSIS DEBITS                              
DTBANC   DS    PL8                 ANALYSIS CREDITS                             
*                                                                               
BATTYPE  DS    XL1                 BATCH TYPE                                   
STATUS   DS    XL1                 TRANSACTION STATUS                           
NEWRFRM  DS    XL1                 NEW REPORT FORMAT                            
BUFFMAX  DS    XL2                 NO. OF ENTRIES IN BUFFER                     
BUFFNTRY DS    XL1                 BUFFER ENTRY LENGTH                          
PRODCULA DS    0XL15                                                            
PRODCUL  DS    CL3                                                              
PRODACT  DS    CL12                                                             
DEFCURC  DS    CL3                 DEFAULT CURRENCY CODE                        
ACTCURC  DS    CL3                 ACTIVE CURRENCY CODE                         
LASTPAGE DS    XL2                 LAST PAGE PRINTED                            
         EJECT                                                                  
***********************************************************************         
* UPPER CASE DICTIONARY                                               *         
***********************************************************************         
         SPACE 1                                                                
DDOUT1   DS    0C                                                               
         SPACE 1                                                                
AC@MOS   DS    CL3                 MOS                                          
         SPACE 1                                                                
***********************************************************************         
* MIXED CASE DICTIONARY                                                         
***********************************************************************         
         SPACE 1                                                                
DDOUT2   DS    0C                                                               
         SPACE 1                                                                
AC@PMREP DS    CL11                PM REPORT                                    
AC@TOTLS DS    CL6                 TOTALS                                       
AC@ANATS DS    CL20                ANALYSIS POSTINGS                            
AC@ACCTS DS    CL20                ACCOUNT POSTINGS                             
AC@BLB   DS    CL8                 BILLABLE                                     
AC@NBLB  DS    CL12                NON-BILLABLE                                 
AC@CURRY DS    CL8                 CURRENCY                                     
AC@DEL   DS    CL3                 DELETED                                      
AC@JOEST DS    CL7                 JOB/EST                                      
AC@EST   DS    CL3                 ESTIMATE                                     
AC@CROR  DS    CL8                 CREDITOR                                     
AC@TO    DS    CL3                 TO                                           
AC@FROM  DS    CL4                 FROM                                         
AC@TASK  DS    CL4                 TASK                                         
AC@BASIS DS    CL5                 BASIS                                        
AC@RATE  DS    CL4                 RATE                                         
AC@LOC   DS    CL8                 LOCATION                                     
AC@ANLC  DS    CL13                ANALYSIS CODE                                
AC@PAYEE DS    CL5                 PAYEE                                        
AC@INV   DS    CL3                 INVOICE                                      
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                                       
***********************************************************************         
         SPACE 1                                                                
BSIZE1   EQU   750                                                              
BSIZE2   EQU   500                                                              
BUFFER1  DS    (BSIZE1)X                                                        
BUFFER2  DS    (BSIZE1)X                                                        
BUFFER3  DS    (BSIZE2)X                                                        
BUFFER4  DS    (BSIZE1)X                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACBAT65   06/08/00'                                      
         END                                                                    
