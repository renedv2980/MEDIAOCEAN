*          DATA SET ACREPXC02  AT LEVEL 030 AS OF 05/01/02                      
*PHASE ACXC02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE QSORT                                                                  
         TITLE 'DELETE REVERSED CHECK POSTINGS'                                 
ACXC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXC**,RR=R5                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXCD,RC                                                         
         ST    R5,RELO                                                          
         EJECT                                                                  
*                                                                               
****LAP1--READING VENDOR/SC RECORDS TO PERFORM VARIOUS CHECKS                   
****LAP2--READING VENDOR/SC RECORDS TO MARK DELETED.                            
*                                                                               
********************************************************************            
*  RUNFRST                                                                      
********************************************************************            
         SPACE 1                                                                
RUNF20   CLI   MODE,RUNFRST                                                     
         BNE   REQF20                                                           
         MVC   VTYPES(VTYPLNQ),EXTADDS                                          
         ZAP   RUNDB,=P'0'         CLEAR ACCUMS                                 
         ZAP   RUNCR,=P'0'                                                      
         ZAP   RUNCNT,=P'0'                                                     
         ZAP   DMPCNT,=P'0'                                                     
         ZAP   NETVEND,=P'0'       REINITIALIZE ACCUMULATORS                    
         ZAP   NETCASH,=P'0'                                                    
         ZAP   TTLVEND,=P'0'                                                    
         ZAP   TTLCASH,=P'0'                                                    
         ZAP   SVCHKTOT,=P'0'                                                   
         ZAP   TOTCHKRN,=P'0'                                                   
         XC    TABCOUNT,TABCOUNT                                                
         MVC   SVSCACNT,SPACES                                                  
         MVC   CHKDISP,=H'0'                                                    
         MVI   PREVSTAT,0          CLEAR OVERALL STATUS BYTE                    
         MVI   BITS,0                                                           
         MVI   ERRBITS,0                                                        
         MVI   SEQBIT,0                                                         
         OI    SEQBIT,SEQLAP1V     START OF LAP1/VENDOR SIDE                    
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
********************************************************************            
*  REQFRST                                                                      
********************************************************************            
         SPACE 1                                                                
REQF20   CLI   MODE,REQFRST                                                     
         BNE   LEDG20                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(2,STDATEC)                               
         GOTO1 DATCON,DMCB,(0,QEND),(2,ENDATEC)                                 
*                                                                               
         XC    SVCHKNUM,SVCHKNUM                                                
         TM    SEQBIT,SEQLAP1V+SEQLAP2V                                         
         BZ    *+8                                                              
         OI    BITS,FRSTRANS       FIRST TIME THROUGH LAP1 OR 2                 
*                                                                               
         TM    SEQBIT,SEQLAP1V             IS THIS LAP1-VENDOR SIDE?            
         BZ    XIT                                                              
         TM    BITS,FRSTRANS               AND FIRST TIME THROUGH?              
         BZ    XIT                         IF SO SAVE REQUEST CARD              
         MVC   SVQUNIT,QUNIT               UNIT                                 
         MVC   SVQLEDGE,QLEDGER            LEDGER                               
         MVC   SVQACCNT,QACCOUNT           ACCOUNT                              
         OC    SVQACCNT,SPACES                                                  
         B     XIT                                                              
         EJECT                                                                  
********************************************************************            
*  LEDGFRST                                                                     
********************************************************************            
         SPACE 1                                                                
LEDG20   CLI   MODE,LEDGFRST                                                    
         BNE   PROCA20                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   CURRSTAT,0          CLEAR STATUS BYTE                            
         ZAP   LEDGDB,=P'0'        CLEAR ACCUMS                                 
         ZAP   LEDGCR,=P'0'                                                     
         ZAP   LEDGCNT,=P'0'                                                    
*                                                                               
         LA    RE,LEDGTAB          SET LEDGER STATUS BYTE                       
LEDG25   CLI   0(RE),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                INVALID LEDGER                               
         CLC   QLEDGER,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'LEDGTAB(RE)    BUMP TO NEXT                                 
         B     LEDG25                                                           
         ZIC   RF,1(RE)            LEDGER EQUATE                                
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    CURRSTAT,0          HAS LEDGER TYPE ALREADY BEEN DONE            
         BZ    *+6                                                              
         DC    H'0'                INVALID LEDGERS REQUESTED                    
         EX    RF,*+8                                                           
         B     *+8                                                              
         OI    CURRSTAT,0          SET NEW LEDGER                               
*                                                                               
         TM    BITS,FRSTRANS       IS THIS THE FIRST TIME THROUGH               
         BZ    LEDG30                                                           
         TM    CURRSTAT,CASH       AND A CASH LEDGER?                           
         BZ    LEDG30                                                           
         OI    BITS,NOCSHLDG       OOPS-NOT ALLOWED                             
*                                                                               
LEDG30   CLI   QOPT6,C'R'          SPECIAL RECOVER OPTION                       
         BNE   XIT                                                              
         TM    PREVSTAT,OPENED     HAVE WE OPENED TAPE YET                      
         BO    XIT                                                              
         OPEN  (RCVTAPE,(OUTPUT))                                               
         OI    CURRSTAT,OPENED                                                  
         B     XIT                                                              
         EJECT                                                                  
********************************************************************            
*  PROCACC                                                                      
********************************************************************            
         SPACE 1                                                                
PROCA20  CLI   MODE,PROCACC                                                     
         BNE   PROCS20                                                          
         ZAP   ACCDB,=P'0'         CLEAR ACCUMS                                 
         ZAP   ACCCR,=P'0'                                                      
         NI    CURRSTAT,X'FF'-(ACCTACTV+PRINTED) TURN OFF ACTIVITY BITS         
         L     RF,ADACC            SAVE ACCOUNT CODE                            
         MVC   ACCOUNT,0(RF)                                                    
         MVC   ACCTNAME,SPACES                                                  
         MVC   SVACCNT,SPACES      CLEAR OUT SAVE ACCOUNT                       
         L     RF,ADACCNAM         AND NAME                                     
         ZIC   RE,1(RF)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     XIT                                                              
         MVC   ACCTNAME(0),2(RF)                                                
         SPACE 3                                                                
         EJECT                                                                  
********************************************************************            
*  PROCSBAC                                                                     
********************************************************************            
         SPACE 1                                                                
         USING TRNRECD,R3                                                       
PROCS20  CLI   MODE,PROCSBAC                                                    
         BNE   PROCT20                                                          
         L     R3,ADSUBAC                                                       
         MVC   CONTRA,TRNKCULC     SAVE CONTRA-ACCOUNT CODE                     
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
********************************************************************            
*  PROCTRNS                                                                     
********************************************************************            
         SPACE 1                                                                
PROCT20  CLI   MODE,PROCTRNS                                                    
         BNE   ACCL20                                                           
*                                                                               
PROCT30  L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         ZAP   TRNSDB,=P'0'                                                     
         ZAP   TRNSCR,=P'0'                                                     
         ZAP   TRANSAMT,=P'0'                                                   
         ZAP   SVTRAMT,=P'0'                                                    
         MVC   TRANSAMT,TRNAMNT    SAVE TRANSACTION AMOUNT                      
         TM    CURRSTAT,CASH       IS THIS A CASH ACCOUNT                       
         BZ    PROCT40                                                          
         TM    TRNSTAT,TRNSREV    THEN ITEMS MUST BE A REVERSAL                 
         BZ    XIT                                                              
         LA    R3,TRNREF          R3=A(CHECK NUMBER)                            
         B     PROCT45                                                          
*                                                                               
PROCT40  TM    TRNSTAT,TRNSDR     PAYABLES POSTINGS MUST BE DEBITS              
         BNO   XIT                                                              
         OI    BITS,PSTDEBIT                                                    
         LA    R3,TRNNARR          SAVE A(CHECK NUMBER)                         
         DROP  R4                                                               
*                                                                               
         USING TRSELD,R4                                                        
PROCT45  MVI   ELCODE,TRSELQ       TRANSACTION STATUS ELEMENT X'60'             
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
*        GOTO1 DATCON,DMCB,(2,TRSDATE),(0,WORK)    ****THIS CODE                
*        CLC   QSTART,WORK         START DATE (DATE IN)   DID NOT               
*        BE    *+14                                    WORK FOR Y2K***          
*        CLC   QEND,WORK           OR END DATE                                  
*        BNE   XIT                                                              
*        GOTO1 DATCON,DMCB,(2,TRSREVD),(0,WORK)                                 
*        CLC   QEND,WORK           OR END DATE (REVERSAL DATE)                  
*        BNE   XIT                                                              
*        DROP  R4                                                               
         CLC   STDATEC,TRSDATE     STAR DATE (DATE IN)                          
         BE    *+14                                                             
         CLC   ENDATEC,TRSDATE     OR END DATE                                  
         BNE   XIT                                                              
         CLC   ENDATEC,TRSREVD     OR REVERSAL DATE                             
         BNE   XIT                                                              
         DROP  R4                                                               
*                                                                               
PROCT50  CLC   0(6,R3),QSRTAREA    STARTING CHECK NUMBER                        
         BL    XIT                                                              
         CLC   0(6,R3),QSELECT     ENDING CHECK NUMBER                          
         BH    XIT                                                              
*                                                                               
         TM    SEQBIT,SEQLAP1C+SEQLAP2C IS THIS CASH SIDE                       
         BNZ   PROCT55                                                          
         USING MPYELD,R4                                                        
         MVI   ELCODE,MPYELQ       MANUAL PAYMENT ELEM X'64'                    
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         TM    BITS,FRSTRANS       IS THIS THE FIRST TIME THROUGH               
         BZ    *+10                                                             
         MVC   SVSCACNT,MPYBNK     SAVE CASH ACCOUNT FOR TEST                   
         CLC   SVSCACNT,MPYBNK     CASH ACCOUNT MUST BE THE SAME                
         BE    *+6                 FOR EACH REC BEING PROCESSED                 
         DC    H'0'                                                             
         MVC   SVCHKTOT,MPYAMNT    SAVE CHECK TOTAL                             
         DROP  R4                                                               
*                                                                               
PROCT55  TM    BITS,PSTDEBIT       IS THIS THE VENDOR SIDE?                     
         BO    *+12                YES                                          
         BAS   RE,PSTCASH          POST TO CASH ACCUMS/TOTALS                   
         B     *+8                                                              
         BAS   RE,PSTVEND          POST TO VENDOR ACCUMS/TOTALS                 
         BAS   RE,BLDCHKRN     ADD ENTRY TO CHECK RUN TABLE                     
*                                                                               
         OI    CURRSTAT,ACCTACTV   SET ACCOUNT ACTIVE                           
         AP    ACCDB,TRNSDB        ADD TO HIGHER LEVEL                          
         AP    ACCCR,TRNSCR                                                     
         LA    R2,TRNSDB                                                        
         TM    SEQBIT,SEQLAP2V+SEQLAP2C IS THIS LAP2?                           
         BZ    *+8                 NO-DO NOT PRINT YET                          
         BAS   RE,PRNTIT                                                        
         AP    LEDGCNT,=P'1'       ADD TO RECORD COUNT                          
*                                                                               
         NI    BITS,X'FF'-FRSTRANS NO LONGER FIRST TIME THROUGH                 
         L     R3,ADTRANS          NOW POINT TO RECORD                          
         SH    R3,DATADISP                                                      
         CLI   QOPT6,C'R'          SPECIAL RECOVER OPTION                       
         BNE   *+12                                                             
         BAS   RE,PUTREC                                                        
         B     XIT                                                              
*                                                                               
         USING TRNRECD,R3                                                       
         TM    ERRBITS,ERRCHECK    IS THERE AN ERROR?                           
         BZ    *+8                                                              
         MVI   RCWRITE,C'N'        DO NOT DELETE REC BUT STILL PRINT            
         CLI   RCWRITE,C'N'        WRITE=NO?                                    
         BE    XIT                                                              
         TM    SEQBIT,SEQLAP2V+SEQLAP2C     IS THIS LAP2?                       
         BZ    XIT                          NO-DO NOT DELETE                    
         OI    TRNRSTAT,X'80'               DELETE RECORD                       
         MVI   MODE,WRITRANS                AND WRITE IT BACK                   
         B     XIT                                                              
         EJECT                                                                  
********************************************************************            
*  ACCLAST                                                                      
********************************************************************            
         SPACE 1                                                                
ACCL20   CLI   MODE,ACCLAST                                                     
         BNE   LEDGL20                                                          
         TM    CURRSTAT,ACCTACTV   ANYTHING FOR THIS ACCOUNT                    
         BZ    XIT                                                              
         AP    LEDGDB,ACCDB        ADD TO HIGHER LEVEL                          
         AP    LEDGCR,ACCCR                                                     
*                                                                               
         LA    R2,ACCDB                                                         
         TM    SEQBIT,SEQLAP2V+SEQLAP2C IS THIS LAP2?                           
         BZ    *+8                                                              
         BAS   RE,PRNTIT           PRINT SOME AMOUNTS                           
         B     XIT                                                              
         EJECT                                                                  
********************************************************************            
*  LEDGLAST                                                                     
********************************************************************            
         SPACE 1                                                                
LEDGL20  CLI   MODE,LEDGLAST                                                    
         BNE   REQL20                                                           
         AP    RUNDB,LEDGDB        ADD TO HIGHER LEVEL                          
         AP    RUNCR,LEDGCR                                                     
         AP    RUNCNT,LEDGCNT                                                   
         LA    R2,LEDGDB                                                        
         TM    SEQBIT,SEQLAP2V+SEQLAP2C IS THIS LAP2?                           
         BZ    *+8                                                              
         BAS   RE,PRNTIT           PRINT SOME TOTALS                            
         OC    PREVSTAT,CURRSTAT   SAVE STATUS                                  
         B     XIT                                                              
         EJECT                                                                  
********************************************************************            
*  REQLAST                                                                      
********************************************************************            
         SPACE 1                                                                
         USING PLINED,R4                                                        
REQL20   LA    R4,P                                                             
         CLI   MODE,REQLAST                                                     
         BNE   RUNL20                                                           
*                                                                               
         TM    BITS,NOCSHLDG       WAS THE REQUEST MADE OFF OF THE CASH         
         BZ    REQL22              LEDGER?                                      
         MVI   SPACING,2                                                        
         MVC   P(46),=C'ERROR*NOT ABLE TO REQUEST OFF OF A CASH LEDGER'         
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
*                                                                               
REQL22   TM    SEQBIT,SEQLAP1V     LAP1/VENDOR SIDE?                            
         BZ    REQL30              NO                                           
*                                                                               
         CP    TTLVEND,=P'0'       ARE THERE ANY TRANSACTIONS?                  
         BNE   REQL25                                                           
         OI    BITS,NOVENDTR                                                    
         MVI   SPACING,2                                                        
         MVC   P(43),=C'NO TRANSACTIONS FOUND ON THE VENDOR SIDE-- '            
         MVC   P+43(25),=C'DID NOT PROCESS CASH SIDE'                           
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
REQL25   NI    SEQBIT,X'FF'-SEQLAP1V     TURN OFF LAP1/VENDOR BIT               
         OI    SEQBIT,SEQLAP1C     AND TURN ON LAP1/CASH BIT                    
         NI    BITS,X'FF'-PSTDEBIT RESET DEBIT/CREDIT BIT                       
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,REQFRST     RESET REQFRST                                
         MVC   QUNIT(L'SVSCACNT),SVSCACNT  MOVE IN SAVED SC ACCOUNT             
         MVI   RCREQREP,C'N'                                                    
         B     XIT                                                              
*                                                                               
*                                                                               
REQL30   TM    SEQBIT,SEQLAP1C     LAP1/CASH SIDE?                              
         BZ    REQL40              NO                                           
*                                                                               
*                                                                               
REQL30A  CP    NETVEND,=P'0'       VENDOR NET TOTAL MUST BE ZERO                
         BE    REQL31                                                           
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         MVC   P(37),=C'**ERROR**VENDOR DEBITS DO NOT TOTAL 0'                  
         OI    ERRBITS,ERRCHECK                                                 
         GOTO1 ACREPORT                                                         
*                                                                               
REQL31   CP    NETCASH,=P'0'       CASH NET TOTAL MUST ALSO BE ZERO             
         BE    REQL34                                                           
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         MVC   P(36),=C'**ERROR**CASH CREDITS DO NOT TOTAL 0'                   
         OI    ERRBITS,ERRCHECK                                                 
         GOTO1 ACREPORT                                                         
*                                                                               
REQL34   CP    TTLVEND,TTLCASH     TOTAL CHECK AMOUNT TO BE DELETED             
         BE    REQL35              MUST BE SAME FOR VENDOR AND CASH             
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         MVC   P(42),=C'**ERROR**TOTAL CHECK AMOUNT TO BE DELETED '             
         MVC   P+42(38),=C'FROM VENDOR/CASH LEDGERS ARE NOT EQUAL'              
         GOTO1 ACREPORT                                                         
         MVC   P(15),=C'VENDOR TOTAL = '                                        
         EDIT  (P8,TTLVEND),(12,P+15),2,COMMAS=YES,ZERO=NOBLANK                 
         MVC   P+30(13),=C'CASH TOTAL = '                                       
         EDIT  (P8,TTLCASH),(12,P+43),2,COMMAS=YES,ZERO=NOBLANK                 
         OI    ERRBITS,ERRCHECK                                                 
         GOTO1 ACREPORT                                                         
*                                                                               
* DOING IT ALL AGAIN, BUT THIS TIME FOR REAL (IF NO ERRORS OCCURRED)            
*                                                                               
REQL35   NI    SEQBIT,X'FF'-SEQLAP1C     TURN OFF LAP1/CASH BIT                 
         OI    SEQBIT,SEQLAP2V     TURN ON LAP2/VENDOR BIT                      
         BAS   RE,CLRTOTAL         CLEAR SOME TOTALS                            
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,REQFRST     RESET TO RUNFRST                             
         MVC   QUNIT(L'SVREQ),SVQUNIT                                           
         B     XIT                 AGAIN TO DELETE RECORDS                      
*                                                                               
REQL40   TM    SEQBIT,SEQLAP2V     IS THIS LAP2/VENDOR SIDE?                    
         BZ    XIT                 NO-FINISHED PROCESSING                       
*                                                                               
         NI    SEQBIT,X'FF'-SEQLAP2V     TURN OFF LAP2/VENDOR BIT               
         OI    SEQBIT,SEQLAP2C     TURN ON LAP2/CASH BIT                        
         NI    BITS,X'FF'-PSTDEBIT RESET DEBIT/CREDIT BIT                       
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,REQFRST     RESET REQFRST                                
         MVC   QUNIT(L'SVSCACNT),SVSCACNT  MOVE IN SAVED SC ACCOUNT             
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
********************************************************************            
*  RUNLAST                                                                      
********************************************************************            
         SPACE 1                                                                
RUNL20   CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
*                                                                               
         TM    BITS,NOCSHLDG       CASH LEDGER REQUESTED?                       
         BO    RUNL25                                                           
         TM    BITS,NOVENDTR       NO VENDOR TRANSACTIONS                       
         BO    RUNL25                                                           
         TM    PREVSTAT,CASH+PAYABLE ENSURE PROPER REQUESTS                     
         BO    *+6                                                              
         DC    H'0'                MISSING A LEDGER                             
RUNL25   LA    R2,RUNDB                                                         
         BAS   RE,PRNTIT                                                        
         BAS   RE,PRCHKRUN         PRINT CHECK RUN TABLE                        
RUNL30   TM    PREVSTAT,OPENED     SPECIAL RECOVER OPTION                       
         BZ    XIT                                                              
         CLOSE (RCVTAPE)                                                        
         B     XIT                                                              
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
********************************************************************            
*  POST TO VENDOR ACCUMULATORS AND TOTALS                                       
********************************************************************            
         SPACE 1                                                                
PSTVEND  NTR1                                                                   
         AP    TRNSDB,TRANSAMT                                                  
         AP    NETVEND,TRANSAMT                                                 
         B     XIT                                                              
         EJECT                                                                  
********************************************************************            
*  POST TO CASH ACCUMULATORS AND TOTALS                                         
********************************************************************            
         SPACE 1                                                                
PSTCASH  NTR1                                                                   
         AP    TRNSCR,TRANSAMT                                                  
         AP    NETCASH,TRANSAMT                                                 
         B     XIT                                                              
         EJECT                                                                  
********************************************************************            
*  ADD ENTRY TO CHECK RUN TABLE (MAYBE)                                         
*  R3=A(CHECK NUMBER)                                                           
********************************************************************            
         SPACE 1                                                                
BLDCHKRN NTR1                                                                   
         USING CHKBLKD,R6                                                       
         L     R6,ACHKBLK                                                       
         LR    R5,R6                                                            
         LR    R2,R6               TO FIGURE DISPLACEMENT LATER                 
         AH    R6,CHKDISP                                                       
         CLC   TABCOUNT,=A(MAXCOUNT)  END OF TABLE REACHED                      
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    BITS,FRSTRANS       FIRST TIME THROUGH                           
         BO    BLD10               NO NEED TO CHECK IF OKAY                     
*                                                                               
         TM    SEQBIT,SEQLAP1C+SEQLAP2C CREDIT SIDE                             
         BZ    *+18                                                             
         CLC   0(6,R3),SVCHKNUM         SAME CHECK # AS PREV                    
         BE    BLDX                     YES NO NEED TO GO FURTHER               
         B     BLD30                                                            
*                                                                               
         CLC   SVACCNT,ACCOUNT+3   COMP ACC W/0 CO CODE AND UNIT/LEDG           
         BNE   BLD05                                                            
BLD02    CLC   0(L'CHKNUM,R5),0(R3)                                             
         BE    BLDX                                                             
         LA    R5,CHKLN(R5)                                                     
         CR    R5,R6                                                            
         BNL   BLD05               OKAY TO ADD                                  
         B     BLD02                                                            
*                                                                               
BLD05    TM    SEQBIT,SEQLAP1C+SEQLAP2C                                         
         BNZ   BLD30                                                            
*                                                                               
*                                                                               
BLD10    MVC   CHKNUM,0(R3)                                                     
         MVC   CHKAMT,SVCHKTOT                                                  
         LA    R6,CHKLN(R6)                                                     
         SR    R6,R2                                                            
         STH   R6,CHKDISP                                                       
         AP    TOTCHKRN,SVCHKTOT                                                
         AP    TTLVEND,SVCHKTOT                                                 
         L     RF,TABCOUNT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,TABCOUNT                                                      
         B     BLDX                                                             
*                                                                               
BLD30    AP    TTLCASH,TRANSAMT                                                 
*                                                                               
BLDX     MVC   SVACCNT,ACCOUNT+3                                                
         MVC   SVCHKNUM,0(R3)                                                   
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
********************************************************************            
*  CLEAR SOME TOTALS AND FIELDS BEFORE STARTING FOR REAL                        
********************************************************************            
         SPACE 1                                                                
CLRTOTAL NTR1                                                                   
         ZAP   RUNDB,=P'0'         CLEAR ACCUMS                                 
         ZAP   RUNCR,=P'0'                                                      
         ZAP   RUNCNT,=P'0'                                                     
         ZAP   DMPCNT,=P'0'                                                     
         ZAP   NETVEND,=P'0'       REINITIALIZE ACCUMULATORS                    
         ZAP   NETCASH,=P'0'                                                    
         ZAP   TTLVEND,=P'0'                                                    
         ZAP   TTLCASH,=P'0'                                                    
         ZAP   SVCHKTOT,=P'0'                                                   
         ZAP   TOTCHKRN,=P'0'                                                   
         XC    TABCOUNT,TABCOUNT                                                
         MVC   SVSCACNT,SPACES                                                  
         MVC   CHKDISP,=H'0'                                                    
         MVI   PREVSTAT,0          CLEAR OVERALL STATUS BYTE                    
         MVI   BITS,0                                                           
*                                                                               
CLRX     B     XIT                                                              
********************************************************************            
*  FORMAT AND PRINT A LINE OF OUTPUT                                            
*  R2=A(ACCUMS)                                                                 
*  R3=A(CHECK NUMBER)                                                           
********************************************************************            
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         USING PLINED,R4                                                        
         LA    R4,P                                                             
         MVI   BYTE,0                                                           
         CLI   MODE,PROCTRNS                                                    
         BNE   PRNT2                                                            
         TM    CURRSTAT,PRINTED    PRINT ACCOUNT ONCE                           
         BO    PRNT1                                                            
         MVC   PACCCODE,ACCOUNT+3   ACCOUNT CODE                                
         MVC   PACCNAME,ACCTNAME   AND NAME                                     
         OI    CURRSTAT,PRINTED                                                 
         TM    CURRSTAT,CASH       IF THIS IS CASH ACCOUNT                      
         BZ    PRNT1                                                            
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT            PRINT ACCOUNT FIRST                          
PRNT1    MVC   PCHKNUM,0(R3)       CHECK NUMBER                                 
         TM    CURRSTAT,CASH       IF THIS IS CASH ACCOUNT                      
         BZ    PRNT4                                                            
         CLC   CONTRA,SPACES       AND THE NAME'S AROUND                        
         BE    PRNT4                                                            
         MVC   PCONHEAD,=C'C/A='                                                
         MVC   PCONTRA,CONTRA+1   SET TO PRINT CONTRA                           
         MVC   CONTRA,SPACES                                                    
         B     PRNT4                                                            
PRNT2    MVC   P+1(22),=C'  *TOTALS FOR ACCOUNT*'                               
         MVI   SPACING,2                                                        
         CLI   MODE,RUNLAST                                                     
         BNE   PRNT3                                                            
         MVI   BYTE,1                                                           
         MVC   P+15(8),=C'REPORT* '                                             
PRNT3    CLI   MODE,LEDGLAST                                                    
         BNE   PRNT4                                                            
         MVI   BYTE,1                                                           
         MVC   P+15(8),=C'LEDGER* '                                             
PRNT4    LA    R3,2                                                             
         LA    R4,PDEBNCR          PRINT AREA FOR DEBITS AND CREDITS            
PRNT5    TM    CURRSTAT,CASH       IF CASH ACCOUNT                              
         BZ    *+16                                                             
         CH    R3,=H'1'            THEN PRINT ZERO CREDITS                      
         BE    PRNT7                                                            
         B     PRNT6                                                            
         CH    R3,=H'2'            FOR PAYABLES,                                
         BE    PRNT7               PRINT ZERO DEBITS                            
PRNT6    CP    0(8,R2),=P'0'                                                    
         BE    PRNT8                                                            
PRNT7    EDIT  (P8,(R2)),(14,(R4)),2,MINUS=YES,COMMAS=YES                       
PRNT8    LA    R2,8(R2)                                                         
         LA    R4,16(R4)                                                        
         BCT   R3,PRNT5                                                         
         CLI   BYTE,1              DO I NEED TO PRINT RECORD COUNT              
         BNE   PRNT9                                                            
         EDIT  (P4,(R2)),(10,(R4)),COMMAS=YES                                   
         MVC   11(7,R4),=C'RECORDS'                                             
PRNT9    MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
********************************************************************            
*  PRINT THE CHECK RUN TABLE                                                    
********************************************************************            
         SPACE 1                                                                
         USING PRUND,R4                                                         
         USING CHKBLKD,R6                                                       
PRCHKRUN NTR1                                                                   
*                                                                               
         L     R2,TABCOUNT                                                      
         GOTO1 =V(QSORT),DMCB,(0,ACHKBLK),(R2),CHKLN,L'CHKNUM,         X        
               CHKNUM-CHKBLKD                                                   
         L     R6,ACHKBLK                                                       
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   PRUNHED,=C'CHECK RUN TABLE'                                      
         GOTO1 ACREPORT                                                         
         MVC   PRUNHEDL,=C'---------------'                                     
         GOTO1 ACREPORT                                                         
         MVC   PRUNCHD,=C'CHECK NO.'                                            
         MVC   PRUNAHD,=C'CHECK AMT'                                            
         GOTO1 ACREPORT                                                         
         MVC   PRUNCHDL,=C'---------'                                           
         MVC   PRUNAHDL,=C'---------'                                           
         GOTO1 ACREPORT                                                         
*                                                                               
PRCHK10  OC    0(CHKLN,R6),0(R6)   ANY ENTRY                                    
         BZ    PRCHK20                                                          
*                                                                               
         MVC   PRUNCHK,CHKNUM                                                   
         EDIT  (P6,CHKAMT),PRUNAMT,2,COMMAS=YES                                 
         GOTO1 ACREPORT                                                         
         LA    R6,CHKLN(R6)                                                     
         B     PRCHK10                                                          
*                                                                               
PRCHK20  GOTO1 ACREPORT                                                         
         MVC   PRUNTHD,=C'  *TOTAL AMOUNT FOR ALL CHECKS*'                      
         EDIT  (P10,TOTCHKRN),PRUNTOT,2,COMMAS=YES                              
         GOTO1 ACREPORT                                                         
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
         DROP  R4,R6                                                            
********************************************************************            
*  WRITE RECORDS TO RECOVERY FILE                                               
*  R3=A(RECORD)                                                                 
********************************************************************            
         SPACE 1                                                                
         USING TRNRECD,R3                                                       
PUTREC   NTR1                                                                   
         XC    OUTREC(28),OUTREC                                                
         LH    RF,TRNRLEN                                                       
         LA    R1,28(RF)           ADD ON L'HEADER+4 TO L'REC                   
         STH   R1,OUTREC                                                        
         MVC   OUTREC+4(2),=X'6103' INDICATE ACCOUNT/ADD                        
         LA    R0,OUTREC+28        MOVE RECORD TO OUTPUT BUFFER                 
         LA    R1,1000                                                          
         LR    RE,R3                                                            
         MVCL  R0,RE                                                            
         PUT   RCVTAPE,OUTREC                                                   
         CLI   QOPT7,C'D'          DUMP RECORDS                                 
         BNE   XIT                                                              
         CP    DMPCNT,MAXDMP                                                    
         BE    XIT                                                              
         LH    RF,OUTREC                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'OUTREC',OUTREC,C'DUMP',(RF),=C'2D',  X        
               (C'P',PRINT)                                                     
         AP    DMPCNT,=P'1'                                                     
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
*  TABLES AND DSECTS                                                            
********************************************************************            
*                                                                               
* EXTERNAL ADDRESSES                                                            
*                                                                               
EXTADDS  DS    0F                                                               
         DC    A(CHKBLK)                                                        
*                                                                               
*  EQUATES FOR STATUS BYTES                                                     
*                                                                               
         SPACE 2                                                                
CASH     EQU   X'80'                                                            
PAYABLE  EQU   X'40'                                                            
UPRINT   EQU   X'40'                                                            
CPRINT   EQU   X'40'                                                            
SPOT     EQU   X'40'                                                            
NET      EQU   X'40'                                                            
CSPOT    EQU   X'40'                                                            
PROD     EQU   X'40'                                                            
CPROD    EQU   X'40'                                                            
EXP      EQU   X'40'                                                            
CEXP     EQU   X'40'                                                            
ACCTACTV EQU   X'10'                                                            
PRINTED  EQU   X'08'                                                            
OPENED   EQU   X'04'                                                            
         SPACE 2                                                                
*                                                                               
*  TABLE OF VALID LEDGERS                                                       
*                BYTE 1 = LEDGER CODE                                           
*                BYTE 2 = LEDGER EQUATE                                         
         SPACE 1                                                                
LEDGTAB  DS    0CL2                                                             
         DC    C'C',AL1(CASH)                                                   
         DC    C'P',AL1(UPRINT)                                                 
         DC    C'Q',AL1(CPRINT)                                                 
         DC    C'S',AL1(SPOT)                                                   
         DC    C'T',AL1(CSPOT)                                                  
         DC    C'U',AL1(NET)                                                    
         DC    C'V',AL1(PROD)                                                   
         DC    C'W',AL1(CPROD)                                                  
         DC    C'X',AL1(EXP)                                                    
         DC    C'Y',AL1(CEXP)                                                   
         DC    X'FF'                                                            
         SPACE 1                                                                
MAXDMP   DC    P'50'                                                            
         SPACE 2                                                                
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,RECFM=VB,DSORG=PS,MACRF=(PM),            X        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
         SPACE 2                                                                
CHKBLK   DS    0D                  CHECK RUN TABLE                              
         DS    36000C                                                           
         EJECT                                                                  
*********************************************************************           
*  LTORG                                                                        
*********************************************************************           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*  WORKING STORAGE                                                              
*********************************************************************           
         SPACE 1                                                                
ACXCD    DSECT                                                                  
RELO     DS    F                                                                
ELCODE   DS    XL1                                                              
CURRSTAT DS    CL1                 SEE EQUATES                                  
PREVSTAT DS    CL1                 SEE EQUATES                                  
*                                                                               
BITS     DS    XL1                                                              
PSTDEBIT EQU   X'80'               POSTING IS A DEBIT                           
FRSTRANS EQU   X'40'               FIRST TIME GOING THROUGH PROCTRNS            
NOCSHLDG EQU   X'20'               TRIED TO REQUEST OFF CASH LEDGER             
NOVENDTR EQU   X'10'               NO VENDOR TRANSACTIONS FOUND                 
*                                                                               
SEQBIT   DS    XL1                 WHICH SEQUENCE ARE WE RUNNING UNDER          
SEQLAP1V EQU   X'80'               LAP1/VENDOR SIDE                             
SEQLAP1C EQU   X'40'               LAP1/CASH SIDE                               
SEQLAP2V EQU   X'20'               LAP2/VENDOR SIDE                             
SEQLAP2C EQU   X'10'               LAP2/CASH SIDE                               
*                                                                               
ERRBITS  DS    XL1                                                              
ERRCHECK EQU   X'80'               AN ERROR HAS OCCURRED                        
*                                                                               
TRANSAMT DS    PL6                 TRANSACTION AMOUNT                           
SVTRAMT  DS    PL6                                                              
SVCHKTOT DS    PL6                 SAVED CHECK TOTAL FOR TABLE                  
SVSCACNT DS    CL14                SAVED CASH ACCOUNT TO TEST                   
SVCHKNUM DS    CL6                 SAVE CHECK NUMBER                            
TOTCHKRN DS    PL10                TOTAL CHECK RUN AMOUNT                       
*                                                                               
SVREQ    DS    0CL14                                                            
SVQUNIT  DS    CL1                                                              
SVQLEDGE DS    CL1                 SAVED REQUEST CARD FOR VENDOR SIDE           
SVQACCNT DS    CL12                                                             
STDATEC  DS    XL2                 SAVED COMPRESSED QSTART DATE                 
ENDATEC  DS    XL2                 SAVED COMPRESSED QEND DATE                   
*                                                                               
CHKDISP  DS    H                   DISPLACEMENT INTO TABLE                      
TABCOUNT DS    F                   COUNT OF TABLE ENTRIES                       
ACCOUNT  DS    CL15                CURRENT ACCOUNT                              
SVACCNT  DS    CL12                SAVED CURRENT ACCOUNT                        
ACCTNAME DS    CL36                        AND NAME                             
CONTRA   DS    CL15                CURRENT CONTRA ACCOUNT                       
NETVEND  DS    PL8                 NET VENDOR ACCUM                             
NETCASH  DS    PL8                 NET CASH ACCUM                               
TTLVEND  DS    PL8                 TOTAL CHECK RUN FOR VENDOR SIDE              
TTLCASH  DS    PL8                 TOTAL CHECK RUN FOR CASH SIDE                
TRNSDB   DS    PL8                 TRANSACTION LEVEL AMOUNTS                    
TRNSCR   DS    PL8                                                              
ACCDB    DS    PL8                 ACCOUNT LEVEL TOTALS                         
ACCCR    DS    PL8                                                              
LEDGDB   DS    PL8                 LEDGER LEVEL TOTALS                          
LEDGCR   DS    PL8                                                              
LEDGCNT  DS    PL4                                                              
RUNDB    DS    PL8                 RUN LEVEL TOTALS                             
RUNCR    DS    PL8                                                              
RUNCNT   DS    PL4                                                              
DMPCNT   DS    PL3                 N'RECORDS DUMPED SO FAR                      
         DS    0D                                                               
*                                                                               
VTYPES   DS    0A                                                               
ACHKBLK   DS    A                  ADDRESS OF CHECK RUN TABLE                   
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
MAXCOUNT EQU   3000                MAX NUMBER OF ENTRIES IN TABLE               
OUTREC   DS    1028C               BUFFER FOR RECOVERY RECORDS                  
*                                                                               
         EJECT                                                                  
*********************************************************************           
*  CHECK RUN TABLE DSECT                                                        
*********************************************************************           
         SPACE 1                                                                
CHKBLKD  DSECT                                                                  
CHKNUM   DS    CL6                 CHECK NUMBER                                 
CHKAMT   DS    PL6                 CHECK AMOUNT                                 
CHKLN    EQU   *-CHKBLKD                                                        
         EJECT                                                                  
*********************************************************************           
*  PRINT CHECK RUN TABLE DSECT                                                  
*********************************************************************           
         SPACE 1                                                                
PRUND    DSECT                                                                  
         DS    CL1                                                              
PRUNHED  DS    CL15                'CHECK RUN TABLE'                            
         ORG   PRUNHED                                                          
PRUNHEDL DS    CL15                                                             
         ORG   PRUNHEDL                                                         
PRUNCHD  DS    CL9                 'CHECK NO.'                                  
         DS    CL12                                                             
PRUNAHD  DS    CL9                 'CHECK AMOUNT'                               
         ORG   PRUNCHD                                                          
PRUNCHDL DS    CL9                                                              
         DS    CL12                                                             
PRUNAHDL DS    CL9                                                              
         ORG   PRUNCHD                                                          
PRUNCHK  DS    CL6                                                              
         DS    CL15                                                             
PRUNAMT  DS    CL10                                                             
         ORG   PRUNCHD                                                          
PRUNTHD  DS    CL31                'TOTAL AMOUNT FOR ALL CHECKS'                
         DS    CL3                                                              
PRUNTOT  DS    CL13                                                             
         SPACE 3                                                                
*********************************************************************           
*  PRINT LINE DSECT                                                             
*********************************************************************           
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL1                                                              
PACCCODE DS    CL12                                                             
         DS    CL2                                                              
PACCNAME DS    CL36                                                             
         ORG   PACCNAME                                                         
PCONHEAD DS    CL4                                                              
PCONTRA  DS    CL14                                                             
         DS    CL18                                                             
         DS    CL2                                                              
PCHKNUM  DS    CL6                                                              
         DS    CL1                                                              
PDEBNCR  DS    CL35                                                             
         EJECT                                                                  
*********************************************************************           
*  INCLUDES                                                                     
*********************************************************************           
         SPACE 1                                                                
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACREPXC02 05/01/02'                                      
         END                                                                    
