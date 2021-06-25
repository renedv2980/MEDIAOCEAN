*          DATA SET PPREP0702  AT LEVEL 094 AS OF 05/18/10                      
*PHASE PP0702A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*  BPLA  05/10    EXPAND START AND END DATES                                    
*                                                                               
*  BPLA  12/02    UPDATIVE SOON AND GENERATION OF OVERNIGHT REQS                
*                                                                               
*  KWAN  08/00     NEW PBILLREC AND DSECT                                       
*                                                                               
*  SMYE  12/18/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PP0702  - PRINTPAK UNBILL PROGRAM'                              
*                                                                               
*              QOPT1 = S OLD SUMMARY BILLING  - DEFUNCT                         
*              QOPT1 = D OLD DETAIL BILLING   - DEFUNCT                         
*              QOPT1 = B NEW BILLING                                            
*              QOPT1 = M NEW MANUAL BILLING                                     
*              QOPT1 = R FINANCIAL REBATE BILLING                               
*                                                                               
*              QOPT2 = R OK TO UNBILL REVERSALS                                 
*                                                                               
*              QOPT3 = 4-7 NEW BILLING TYPE                                     
*                                                                               
*              QOPT4 = D  MARK BILLS DELETED                                    
*                      BLANK - CLOSED-OUT                                       
***********************************************************************         
*   QOPT6: 'S' = SUBMITTED AS AN UPDATIVE SOON                                  
*          'L' = THIS IS A LIVE (OVERNIGHT) REQUEST                             
*          ' ' = DRAFT (FOR PROGRAMMERS ONLY, VIA TSO)                          
*   QOPT7: 'Y' = PRINT TRACE OF FILE UPDATES - NOT CODED YET                    
***********************************************************************         
*                                                                               
*              QPAY(6) = BILLDATE  (ALL= ANY DATE)                              
*              QPUB+1(4) = INVOIVE NUMBER                                       
*              QPUB+5(4) = REVERSED INVOICE NUMBER WHEN UNBILLING               
*                          REVERSAL                                             
*                                                                               
PP0702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0702                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'       2ND DSECT                                    
         L     R8,PPWORK2C                                                      
         USING PPWORK2D,R8                                                      
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R9,PP0702+4095      2ND BASE REGISTER                            
         LA    R9,1(R9)                                                         
         USING PP0702+4096,R9                                                   
         LA    R6,P1                                                            
         USING UBLIND,R6                                                        
         LA    R7,SPACEND                                                       
         USING UBWRKD,R7                                                        
         CLI   MODE,PROCBUY                                                     
         BE    PRBY                                                             
         CLI   MODE,PROCBIL                                                     
         BE    PRBILL                                                           
         CLI   MODE,FBUYREQ                                                     
         BE    INIT                                                             
         CLI   MODE,LBUYREQ                                                     
         BE    LBYR                                                             
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
         CLI   MODE,RUNFRST                                                     
         BE    FIRST                                                            
         CLI   MODE,DISKERR                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XIT                                                                    
         EJECT                                                                  
*                                  INITIALIZATION                               
INIT     EQU   *                                                                
         MVI   ACTIVITY,C'N'                                                    
         MVI   ERRSW,0                                                          
         MVI   POSTSW,0                                                         
*                                                                               
         CLC   =C'ALL',QEST                                                     
         BNE   *+10                                                             
         MVC   QEST,SPACES                                                      
         MVI   FCRDACTV,C'N'                                                    
         CLI   QPRODUCT,C' '                                                    
         BE    INITA                                                            
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   *+8                                                              
INITA    DS    0H                                                               
         MVI   FCRDACTV,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
*                                  RUN DATE                                     
         PACK  DUB,RCDATE(2)                                                    
         CVB   R0,DUB                                                           
         STC   R0,TODAYB+1         MO                                           
         PACK  DUB,RCDATE+3(2)                                                  
         CVB   R0,DUB                                                           
         STC   R0,TODAYB+2         DA                                           
         PACK  DUB,RCDATE+6(2)                                                  
         CVB   R0,DUB                                                           
         STC   R0,TODAYB           YR                                           
         MVC   TODAY(2),RCDATE+6                                                
         MVC   TODAY+2(2),RCDATE                                                
         MVC   TODAY+4(2),RCDATE+3                                              
         XC    MANINV(6),MANINV                                                 
         XC    REVINV(6),REVINV                                                 
         CLI   QPUB+1,C' '                                                      
         BE    INIT1                                                            
         PACK  DUB,QPUB+1(4)                                                    
         CVB   R0,DUB                                                           
         STH   R0,MANINV                                                        
         MVC   MANINVC,QPUB+1                                                   
INIT1    CLI   QPUB+5,C' '                                                      
         BE    INIT1C                                                           
         PACK  DUB,QPUB+5(4)                                                    
         CVB   R0,DUB                                                           
         STH   R0,REVINV                                                        
         MVC   REVINVC,QPUB+5                                                   
*                                                                               
INIT1C   DS    0H                                                               
*                                  UNBILL DATE                                  
         CLI   QPAY,C'A'           ONLY NEEDED ON FIRST REQ                     
         BL    INIT2                                                            
         XC    UDATEB,UDATEB                                                    
         MVC   UDATEP(8),=C'ANY DATE'                                           
         CLC   QPAY(3),=C'ALL'                                                  
         BE    INIT1E                                                           
*                                                                               
*        GOTO1 DTCNV,DMCB,QPAY,(1,UDATEB)                                       
         GOTO1 DATCON,DMCB,(0,QPAY),(3,UDATEB)                                  
*                                                                               
*        GOTO1 DTCNV,DMCB,QPAY,(3,UDATEP)                                       
         GOTO1 DATCON,DMCB,(0,QPAY),(5,UDATEP)                                  
**Y2K FIX                                                                       
         GOTO1 DATCON,DMCB,(3,UDATEB),(0,QPAY)                                  
**Y2K FIX                                                                       
*                                                                               
INIT1E   MVC   UDATE,QPAY                                                       
INIT2    DS    0H                                                               
*                                  MONTH OF SERVICE                             
         MVC   STARTP(16),SPACES                                                
         XC    BSTART,BSTART                                                    
         MVC   BEND,=3X'FF'                                                     
         CLI   QSTART,C' '                                                      
         BNE   INIT3                                                            
*                                                                               
*        QSTART MISSING - SET TO TODAY LESS 1500 DAYS                           
*                                                                               
         GOTO1 ADDAY,DMCB,TODAY,QSTART,-1500                                    
*                                                                               
INIT3    DS    0H                                                               
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*        GOTO1 (RF),(R1),,(3,STARTP)                                            
         GOTO1 DATCON,(R1),,(5,STARTP)                                          
*                                                                               
         CLI   QEND,C' '                                                        
         BNE   INIT3A                                                           
*                                                                               
*        QEND MISSING - SET TO TODAY PLUS 900 DAYS                              
*                                                                               
         GOTO1 ADDAY,DMCB,TODAY,QEND,900                                        
*                                                                               
INIT3A   DS    0H                                                               
*        GOTO1 DTCNV,DMCB,QEND,(1,BEND)                                         
         GOTO1 DATCON,DMCB,(0,QEND),(3,BEND)                                    
*        GOTO1 (RF),(R1),,(3,ENDP)                                              
         GOTO1 DATCON,(R1),,(5,ENDP)                                            
INIT3B   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    BILTOT(36),BILTOT   CLEAR ALL ACCUMS                             
         MVI   FCRDBUY,C'N'                                                     
         MVI   RCSUBPRG,5                                                       
         CLI   QOPT1,C'S'          OLD SUMMARY                                  
         BE    INIT4                                                            
         MVI   RCSUBPRG,10         OLD  DETAIL                                  
         MVI   FCRDBUY,C'Y'                                                     
         CLI   QOPT1,C'D'                                                       
         BE    INIT4                                                            
         MVI   RCSUBPRG,20         NEW BILLING                                  
         CLI   QOPT1,C'B'                                                       
         BE    INIT4                                                            
         MVI   FCRDBUY,C'N'      DON'T READ BUYS FOR NEW MANAL BILLS            
         CLI   QOPT1,C'M'          NEW MAMUAL BILL                              
         BE    INIT4                                                            
         MVI   RCSUBPRG,30                                                      
         MVI   FCRDBUY,C'Y'                                                     
         CLI   QOPT1,C'R'          NEW REBATE BILL                              
         BNE   CRDERR                                                           
*                                                                               
INIT4    DS    0H                                                               
         MVI   QBPDATE,C'B'                                                     
         MVI   TYPE,0                                                           
         MVI   SOONSW,C'N'                                                      
         EJECT                                                                  
*                                                                               
         CLI   QOPT6,C'S'          SOON REQUEST?                                
         BNE   REQFX                                                            
         MVI   SOONSW,C'Y'         SOON RUN                                     
*                                                                               
         PACK  HEADER$P,QHEADER$+1(L'QHEADER$-1) TOTAL HEAD. $ EXPECTED         
         NI    HEADER$P+L'HEADER$P-1,X'FC'  ASSUME POSITIVE                     
         CLI   QHEADER$,C'-'                CREDIT AMOUNT?                      
         BNE   *+8                                                              
         OI    HEADER$P+L'HEADER$P-1,X'01'  YES: MAKE NEGATIVE                  
*                                                                               
         MVC   DETAIL$P,HEADER$P            ASSUME DETAILS SHOULD MATCH         
*                                                                               
         CLI   QOPT1,C'M'      SEE UNBILLING A MANUAL BILL                      
         BNE   *+10                                                             
         ZAP   DETAIL$P,=P'0'  NO DETAILS WILL BE FOUND                         
*                                                                               
         CLI   QDETAIL$+L'QDETAIL$-1,C' '   EXPECTED DETAIL $ GIVEN?            
         BE    REQFX                        NO                                  
*                                                                               
         PACK  DETAIL$P,QDETAIL$+1(L'QDETAIL$-1)  WE'RE OUT OF BALANCE          
         NI    DETAIL$P+L'DETAIL$P-1,X'FC'  ASSUME POSITIVE                     
         CLI   QDETAIL$,C'-'                CREDIT AMOUNT?                      
         BNE   REQFX                                                            
         OI    DETAIL$P+L'DETAIL$P-1,X'01'  YES: MAKE NEGATIVE                  
*                                                                               
REQFX    DS    0H                                                               
         CLI   QOPT2,C'R'          SEE IF UNBILLING A REVERSAL                  
         BNE   EXIT                                                             
         OC    MANINV,MANINV       INVOICE MUST BE GIVEN                        
         BZ    REVERR                                                           
         OC    REVINV,REVINV       REVERSED INVOICE ALSO                        
         BZ    REVERR                                                           
         B     EXIT                                                             
*                                                                               
CRDERR   EQU   *                                                                
         MVC   P1(80),QRECORD                                                   
         MVC   P1+85(16),=C'BAD CONTROL CARD'                                   
         BAS   RE,RPRT                                                          
         DC    H'0'                                                             
*                                                                               
REVERR   MVC   P1(41),=C'**MISSING INVOICE NUMBERS FOR REVERSALS**'             
         MVC   P2(35),=C'**NUMBER FOR UNBILLED INVOICE AND**'                   
         MVC   P3(37),=C'**REVERSED INVOICE MUST BE SUPPLIED**'                 
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
         MVI   MODE,LBUYREQ        KILL RUN                                     
         CLI   SOONSW,C'Y'        SOON REQUEST?                                 
         BE    REQL40                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                  PROCESS BILL                                 
PRBILL   EQU   *                                                                
         TM    KEY+25,X'80'                                                     
         BNZ   EXIT                                                             
         CLC   RCSVPRD,=C'ZZZ'     PREVENT DOING BILLS TWICE                    
         BE    EXIT                IF PRD=NO OR ALL RUN                         
         GOTO1 GETBILL             REREAD BILL TO BE SURE                       
*                                  IT WAS LAST RECORD READ                      
*                                                                               
         CLC   PBILKMOS,BSTART                                                  
         BL    PRMAN               GO SEE IF NEW MANUAL                         
         CLC   PBILKMOS,BEND                                                    
         BH    PRMAN               GO SEE IF NEW MANUAL                         
*                                                                               
         CLC   UDATE(3),=C'ALL'                                                 
         BE    PRBL1                                                            
*                                                                               
         CLC   PBILLDAT,UDATE                                                   
         BNE   PRBL6                                                            
*                                  BILL ADDED 'TODAY'                           
PRBL1    DS    0H                                                               
         CLI   QOPT1,C'D'                                                       
         BNE   PRBL2                                                            
         CLI   PBILLTYP,C'B'        IGNORE NEW BILLS                            
         BE    EXIT                                                             
         CLI   PBILLTYP,C'M'        IGNORE NEW MANUAL BILLS                     
         BE    EXIT                                                             
         CLI   PBILLTYP,C'R'        IGNORE NEW REBATE BILLS                     
         BE    EXIT                                                             
         CLI   PBILLTYP,C'4'                                                    
         BNE   PRBL6                                                            
         B     PRBL4                                                            
PRBL2    EQU   *                                                                
         CLI   QOPT1,C'S'          OLD SUMMARY                                  
         BNE   PRBL3                                                            
         CLI   PBILLTYP,C'4'                                                    
         BE    EXIT                                                             
         CLI   PBILLTYP,C'B'        IGNORE NEW BILLS                            
         BE    EXIT                                                             
         CLI   PBILLTYP,C'M'        IGNORE NEW MANUAL BILLS                     
         BE    EXIT                                                             
         CLI   PBILLTYP,C'R'        IGNORE NEW REBATE BILLS                     
         BE    EXIT                                                             
         B     PRBL4                                                            
*                                                                               
*                                  MUST BE DOING NEW BILLING                    
PRBL3    CLC   PBILLTYP(1),QOPT1   MATCH NEW BILLING TYPE                       
         BNE   PRMAN               GO SEE IF NEW MANUAL BILL                    
         CLI   QOPT3,C' '          TEST SPECIFIC BILLING TYPE                   
         BNH   PRBL4                                                            
         CLC   PBILLTYP+1(1),QOPT3   MATCH TYPE                                 
         BNE   EXIT                                                             
PRBL4    EQU   *                                                                
         OC    MANINV,MANINV                                                    
         BZ    PRBL5                                                            
         CLC   MANINV,PBILKBNO                                                  
         BNE   EXIT                                                             
PRBL5    DS    0H                                                               
*******  CLI   QOPT2,C'P'          POSTED OPTION                                
*******  BE    PRBL5B                                                           
*******  CLI   QOPT2,C'B'          POSTED OPTION                                
*******  BE    PRBL5B                                                           
*                                                                               
         OC    PBILPOST,PBILPOST                                                
         BZ    PRBL5B                                                           
         OI    POSTSW,X'01'        POSTED BILL FOUND                            
         B     PRBL5B                                                           
*                                                                               
******   BAS   RE,FMTBIL                                                        
******   BAS   RE,RPRT                                                          
******   MVC   P1(50),=C'**ATTEMPT TO UNBILL POSTED BILLS-CONTACT SYSTE         
******         MS**'                                                            
******   BAS   RE,RPRT                                                          
******   B     EXIT                                                             
*                                                                               
PRBL5B   DS    0H                                                               
         CLI   PBRETAIL,0        SEE IF RETAIL BILL                             
         BNE   PRBL5E            YES - SKIP THIS CHECK                          
         CLI   PBILLTYP,C'B'      NEW BILLS SOMETIME USE PBILLCAN               
         BE    PRBL5E             SO SKIP THIS CHK                              
         CLC   PBILLCAN(4),=C'0000'                                             
         BNH   PRBL5E                                                           
         BAS   RE,FMTBIL                                                        
         BAS   RE,RPRT                                                          
         MVC   P1(42),=C'**ATTEMPT TO UNBILL REVERSED MANUAL BILL**'            
         MVC   P2(11),=C'REVERSED BY'                                           
         MVC   P2+12(6),PBILLCAN                                                
         MVC   P2+20(2),=C'ON'                                                  
         GOTO1 DATCON,DMCB,(0,PBILLCAN+6),(5,P2+23)                             
         BAS   RE,RPRT                                                          
         OI    ERRSW,X'08'                                                      
         MVI   ACTIVITY,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
PRBL5E   DS    0H                                                               
         MVI   ACTIVITY,C'Y'                                                    
         TM    PBILCMSW,X'20'        SEE IF AOR BILL                            
         BO    PRBL5H                SKIP IN TOTALS                             
         CLI   PBRETAIL,X'41'        RETAIL SUMMARY                             
         BE    PRBL5H                SKIP IN TOTALS                             
*                                                                               
         ZAP   DUB,PBILLGRS                                                     
         CVB   R0,DUB                                                           
         A     R0,BILTOT                                                        
         ST    R0,BILTOT                                                        
         ZAP   DUB,PBILLBIL                                                     
         CVB   R0,DUB                                                           
         A     R0,BILTOT+4                                                      
         ST    R0,BILTOT+4                                                      
         ZAP   DUB,PBILLNET                                                     
         CVB   R0,DUB                                                           
         A     R0,BILTOT+8                                                      
         ST    R0,BILTOT+8                                                      
PRBL5H   DS    0H                                                               
         CLI   TYPE,C'D'                                                        
         BE    *+12                                                             
         MVI   TYPE,C'D'                                                        
         BAS   RE,RPRT                                                          
         MVC   P1(11),=C'**DELETED**'                                           
*                                                                               
         CLI   QOPT4,C'D'                                                       
         BE    *+10                                                             
         MVC   P1(12),=C'*CLOSED OUT*'                                          
*                                                                               
         BAS   RE,FMTBIL                                                        
         BAS   RE,RPRT                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   PRBL6                                                            
                                                                                
         CLI   QOPT6,C'L'                                                       
         BNE   PRBL6            NOT LIVE RUN                                    
                                                                                
         OI    KEY+25,X'80'                                                     
*                                                                               
         CLI   QOPT4,C'D'                                                       
         BE    *+8                                                              
         OI    KEY+25,X'C0'                                                     
*                                                                               
         GOTO1 WRT                                                              
         OI    PBILLCTL,X'80'                                                   
*                                                                               
         CLI   QOPT4,C'D'                                                       
         BE    *+8                                                              
         OI    PBILLCTL,X'C0'                                                   
*                                                                               
         LA    RF,PBILLREC                                                      
         ST    RF,AREC                                                          
         GOTO1 PUTPRT                                                           
         CLI   QOPT1,C'M'        SEE IF UNBILLING NEW MANUAL BILL               
         BNE   PRBL6                                                            
         MVC   MYKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(25),PBILLREC                                                 
         MVI   KEY+3,X'88'       GO DELETE PASSIVE POINTER ALSO                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(25),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                MUST FIND PASSIVE POINTER                    
         OI    KEY+25,X'80'                                                     
*                                                                               
         CLI   QOPT4,C'D'                                                       
         BE    *+8                                                              
         OI    KEY+25,X'C0'                                                     
*                                                                               
         GOTO1 WRT                                                              
*                                                                               
         MVC   KEY,MYKEY              SET KEY FOR SEQUENTIAL READ               
         GOTO1 HIGH                                                             
*                                                                               
PRBL6    EQU   *                                                                
         CLI   QOPT1,C'B'                                                       
         BE    PRMAN              GO CHK FOR MANUAL REVERSALS                   
         CLI   QOPT1,C'D'                                                       
         BNE   EXIT                                                             
         CLC   PBILLCDT,UDATE      CHECK REVERSED TODAY                         
         BNE   EXIT                                                             
         OC    MANINV,MANINV                                                    
         BZ    PRBL7                                                            
         CLC   MANINVC,PBILLCAN+2                                               
         BNE   EXIT                                                             
PRBL7    DS    0H                                                               
         ZAP   DUB,PBILLGRS                                                     
         CVB   R0,DUB                                                           
         A     R0,REVTOT                                                        
         ST    R0,REVTOT                                                        
         ZAP   DUB,PBILLBIL                                                     
         CVB   R0,DUB                                                           
         A     R0,REVTOT+4                                                      
         ST    R0,REVTOT+4                                                      
         ZAP   DUB,PBILLNET                                                     
         CVB   R0,DUB                                                           
         A     R0,REVTOT+8                                                      
         ST    R0,REVTOT+8                                                      
         CLI   TYPE,C'R'                                                        
         BE    *+12                                                             
         MVI   TYPE,C'R'                                                        
         BAS   RE,RPRT                                                          
         MVC   P1(14),=C'**UNREVERSED**'                                        
         BAS   RE,FMTBIL                                                        
         BAS   RE,RPRT                                                          
         MVI   PBILLCAN,C'0'                                                    
         MVC   PBILLCAN+1(11),PBILLCAN                                          
         LA    RF,PBILLREC                                                      
         ST    RF,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
                                                                                
         CLI   QOPT6,C'L'                                                       
         BNE   EXIT           NOT LIVE RUN                                      
                                                                                
         GOTO1 PUTPRT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
PRMAN    CLI   PBILLTYP,C'M'       SEE IF NEW MANUAL BILL                       
         BNE   EXIT                                                             
         CLI   QOPT1,C'B'                                                       
         BNE   EXIT                                                             
         CLC   PBILMCAN,ZEROS      SEE IF 'REBILLED'                            
         BE    EXIT                NO - SKIP                                    
*                                                                               
         CLC   UDATE(3),=C'ALL'    ANY DATE                                     
         BE    PRMAN3                                                           
         CLC   PBILMDAT,UDATE      CHK DATE                                     
         BNE   EXIT                                                             
*                                                                               
PRMAN3   CLI   MANINVC,C' '                                                     
         BNH   PRMAN5                                                           
         CLC   MANINVC,PBILMCAN+2  MATCH INVOICE NUMBER                         
         BNE   EXIT                                                             
*                                                                               
PRMAN5   MVI   ACTIVITY,C'Y'                                                    
         ZAP   DUB,PBILLGRS                                                     
         CVB   R0,DUB                                                           
         A     R0,REVTOT                                                        
         ST    R0,REVTOT                                                        
         ZAP   DUB,PBILLBIL                                                     
         CVB   R0,DUB                                                           
         A     R0,REVTOT+4                                                      
         ST    R0,REVTOT+4                                                      
         ZAP   DUB,PBILLNET                                                     
         CVB   R0,DUB                                                           
         A     R0,REVTOT+8                                                      
         ST    R0,REVTOT+8                                                      
         CLI   TYPE,C'M'                                                        
         BE    *+12                                                             
         MVI   TYPE,C'M'                                                        
         BAS   RE,RPRT                                                          
         MVC   P1(14),=C'**UNREV MANS**'                                        
         BAS   RE,FMTBIL                                                        
         BAS   RE,RPRT                                                          
         MVI   PBILMCAN,C'0'                                                    
         MVC   PBILMCAN+1(11),PBILMCAN                                          
         LA    RF,PBILLREC                                                      
         ST    RF,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
                                                                                
         CLI   QOPT6,C'L'                                                       
         BNE   EXIT             NOT LIVE RUN                                    
                                                                                
         GOTO1 PUTPRT                                                           
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
FMTBIL   NTR                                                                    
         MVC   UBSEP,=C'ADJ'                                                    
         CLI   PBILSEP,C'A'                                                     
         BE    FMTB5                                                            
         MVC   UBSEP,=C'CD '                                                    
         CLI   PBILSEP,C'C'                                                     
         BE    FMTB5                                                            
         MVC   UBSEP,=C'   '                                                    
FMTB5    DS    0H                                                               
         CLI   PBRETAIL,0                                                       
         BE    FMTB7                                                            
         MVC   UBRET,=C'COR'                                                    
         CLI   PBRETAIL,X'81'          CORPORATE CONTROL                        
         BE    FMTB7                                                            
         MVC   UBRET,=C'SUM'                                                    
         CLI   PBRETAIL,X'41'          RETAIL SUMMARY                           
         BE    FMTB7                                                            
         MVC   UBRET,=C'RET'                                                    
FMTB7    DS    0H                                                               
         TM    PBILCMSW,X'20'              SEE IF AOR BILL                      
         BNO   FMTB7D                                                           
         MVC   UBRET,=C'AOR'              CAN'T BE RETAIL AND AOR               
         B     FMTB8                                                            
*                                                                               
FMTB7D   DS    0H                                                               
         TM    PBILCMSW,X'01'              SEE IF COMMISSION BILL               
         BNO   FMTB7F                                                           
         MVC   UBRET,=C'UFC'              UP FRONT COMMISSION                   
         B     FMTB8                                                            
*                                                                               
FMTB7F   DS    0H                                                               
         TM    PBILCMSW,X'08'            SEE IF COMMISSION BILL- NET            
         BNO   FMTB8                                                            
         MVC   UBRET,=C'NET'              UP FRONT COMMISSION - NET             
         B     FMTB8                                                            
*                                                                               
FMTB8    DS    0H                                                               
         CLI   PBILLTYP,C'M'         SEE IF MANUAL                              
         BNE   FMTB10                                                           
         CLI   UBRET,C' '            CAN I DISPLAY HERE?                        
         BNE   FMTB9                                                            
         MVC   UBRET,=C'MAN'                                                    
         B     FMTB10                                                           
*                                                                               
FMTB9    MVC   UBRET+132(3),=C'MAN'   IN P2                                     
*                                                                               
FMTB10   MVC   UBMED,PBILKMED                                                   
         MVC   UBCLT,PBILKCLT                                                   
         MVC   UBPRD,PBILKPRD                                                   
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UBEST,DUB                                                        
*        GOTO1 DTCNV,DMCB,(1,PBILKMOS),(5,UBMOS)                                
         GOTO1 DATCON,DMCB,(3,PBILKMOS),(9,UBMOS)                               
*        GOTO1 (RF),(R1),(1,PBILKBMN),(5,UBBMN)                                 
         GOTO1 DATCON,(R1),(3,PBILKBMN),(9,UBBMN)                               
         MVC   HALF,PBILKBNO                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UBINV,DUB                                                        
         ZAP   DUB,PBILLGRS                                                     
         CVB   R0,DUB                                                           
         ST    R0,DMCB                                                          
         ZAP   DUB,PBILLBIL                                                     
         CVB   R0,DUB                                                           
         ST    R0,DMCB+4                                                        
         ZAP   DUB,PBILLNET                                                     
         CVB   R0,DUB                                                           
         ST    R0,DMCB+8                                                        
         LA    R2,DMCB                                                          
         BAS   RE,EDIT3                                                         
         EDIT  (P6,PBILLRCV),(15,P+113),2,COMMAS=YES,CR=YES                     
         XIT                                                                    
         EJECT                                                                  
*                                  PROCESS BUY                                  
PRBY     EQU   *                                                                
         GOTO1 GETBUY              REREAD BUY TO BE SURE IT WAS                 
*                                  LAST RECORD READ                             
*                                                                               
         CLI   QOPT1,C'D'          OLD DETAIL                                   
         BE    PRBY2                                                            
         CLI   QOPT1,C'B'          NEW BILLING                                  
         BE    PRBY2                                                            
         CLI   QOPT1,C'R'          NEW REBATE BILLING                           
         BNE   EXIT                                                             
PRBY2    MVI   BYSW,0                                                           
         XC    RVGROSS(12),RVGROSS                                              
         XC    UBGROSS(12),UBGROSS                                              
         LA    R3,PBUYREC+33                                                    
PRBY4    EQU   *                                                                
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
***R***                                                                         
         CLI   QOPT1,C'R'          CHK FOR REBATE BILLINGS                      
         BNE   PRBY4C                                                           
         CLI   0(R3),X'29'                                                      
         BE    PRBY6                                                            
         B     PRBY4E                                                           
***R***                                                                         
PRBY4C   CLI   0(R3),X'26'                                                      
         BE    PRBY6                                                            
         CLI   0(R3),X'28'         CHK FOR FINANCIAL ELEMS ALSO                 
         BE    PRBY6                                                            
PRBY4E   CLI   0(R3),0                                                          
         BNE   PRBY4                                                            
         B     PRBY10                                                           
**                                                                              
PRBY6    EQU   *                                                                
         USING PBILELEM,R3                                                      
         OC    PBLDATE,PBLDATE     CHK FOR A DATE                               
         BZ    PRBY4                                                            
*                                                                               
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    PRBY7                                                            
         CLI   QPRODUCT,C' '                                                    
         BE    PRBY7                                                            
         CLC   PBPRD,QPRODUCT                                                   
         BNE   PRBY4                                                            
PRBY7    DS    0H                                                               
         OC    REVINV,REVINV         SEE IF UNBILLING REVERSAL                  
         BZ    PRBY7A                                                           
         CLC   REVINV,PBINVNO       FIND BILL ELEM IT REVERSED                  
         BNE   PRBY7A                                                           
         TM    PBBILST,X'80'        SEE IF REVERSED                             
         BZ    PRBY7A                                                           
         NI    PBBILST,X'7F'        SET OFF REVERSED BYTE                       
******** L     R1,PBGROSS                                                       
         MVC   FULL,PBGROSS                                                     
         L     R1,FULL                                                          
         LCR   R1,R1                                                            
         A     R1,RVGROSS                                                       
         ST    R1,RVGROSS                                                       
*******  L     R1,PBGROSS+4                                                     
         MVC   FULL,PBGROSS+4                                                   
         L     R1,FULL                                                          
         LCR   R1,R1                                                            
         A     R1,RVGROSS+4                                                     
         ST    R1,RVGROSS+4                                                     
*******  L     R1,PBGROSS+8                                                     
         MVC   FULL,PBGROSS+8                                                   
         L     R1,FULL                                                          
         LCR   R1,R1                                                            
         A     R1,RVGROSS+8                                                     
         ST    R1,RVGROSS+8                                                     
         OI    BYSW,X'02'           SET FOUND REVERSED ELEM IND                 
         B     PRBY4                                                            
*                                                                               
PRBY7A   DS    0H                                                               
         OC    UDATEB,UDATEB         ANY DATE                                   
         BZ    PRBY7A5                                                          
*                                                                               
         CLC   PBLDATE,UDATEB                                                   
         BNE   PRBY4                                                            
*                                                                               
PRBY7A5  OC    MANINV,MANINV                                                    
         BZ    PRBY7B                                                           
         CLC   MANINV,PBINVNO                                                   
         BNE   PRBY4                                                            
PRBY7B   DS    0H                                                               
         MVI   ACTIVITY,C'Y'                                                    
         CLI   TYPE,C'B'                                                        
         BE    *+12                                                             
         MVI   TYPE,C'B'                                                        
         BAS   RE,RPRT                                                          
         MVC   P1(12),=C'**UNBILLED**'                                          
         CLI   PBILELEM,X'28'      FINANICAL OPEN BILLING ELEM                  
         BNE   *+10                                                             
         MVC   P1(12),=C'**OPEN UNB**'                                          
*                                                                               
         TM    PBBILST,X'01'       CHK UFC COMM                                 
         BZ    *+10                                                             
         MVC   P1+14(3),=C'UFC'                                                 
         TM    PBBILST,X'02'       CHK UFC NET                                  
         BZ    *+10                                                             
         MVC   P1+14(3),=C'NET'                                                 
*                                                                               
         MVC   UBMED,PBUYKMED                                                   
         MVC   UBCLT,PBUYKCLT                                                   
         MVC   UBPRD,PBPRD                                                      
         CLC   PBUYKPRD,=C'ZZZ'                                                 
         BNE   *+8                                                              
         MVI   UBPRD+4,C'Z'                                                     
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UBEST,DUB                                                        
*        GOTO1 DTCNV,DMCB,(1,PBUYKDAT),(4,UBYDAT)                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(7,UBYDAT)                              
         LA    R4,UBYDAT+5                                                      
         CLI   PBDFREQ,C'M'                                                     
         BNE   *+14                                                             
         MVC   UBYDAT+3(2),SPACES                                               
         LA    R4,UBYDAT+3                                                      
         CLI   PBUYKLIN,1                                                       
         BE    PRBY8                                                            
         SR    R5,R5                                                            
         IC    R5,PBUYKLIN                                                      
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R4),DUB                                                      
         MVI   0(R4),C'-'                                                       
PRBY8    EQU   *                                                                
         GOTO1 PUBEDIT,DMCB,PBUYKPUB,UBPUB                                      
*                                                                               
         XC    DMCB(12),DMCB                                                    
         TM    PBBILST,X'40'      SEE IF REVERSAL                               
         BZ    PRBY8B                                                           
         CLI   QOPT2,C'R'         SEE IF OK TO UNBILL REVERSALS                 
         BE    PRBY8A                                                           
*******  CLI   QOPT2,C'B'         SEE IF OK TO UNBILL REVERSALS                 
*******  BE    PRBY8A                                                           
         B     PRBY8BX                                                          
*                                                                               
PRBY8A   OC    REVINV,REVINV      SEE IF REVERSED INV NUMBER GIVEN              
         BZ    PRBY8BX            NO - CAUSE IMBALANCE                          
*                                                                               
PRBY8B   MVC   DMCB(12),PBGROSS                                                 
PRBY8BX  L     R5,DMCB                                                          
*                                                                               
         CLI   PBILELEM,X'28'     SEE IF DOING FINANCIAL BILL ELEM              
         BE    PRBY8C             YES - DON'T ROLL TO TOTALS                    
         LR    R0,R5                                                            
         A     R0,BUYTOT                                                        
         ST    R0,BUYTOT                                                        
PRBY8C   EDIT  (R5),(15,UGRS),2,COMMAS=YES,CR=YES                               
         S     R5,DMCB+8                                                        
*                                                                               
         CLI   PBILELEM,X'28'     SEE IF DOING FINANCIAL BILL ELEM              
         BE    PRBY8D             YES - DON'T ROLL TO TOTALS                    
         LR    R0,R5                                                            
         A     R0,BUYTOT+4                                                      
         ST    R0,BUYTOT+4                                                      
PRBY8D   EDIT  (R5),(15,UBIL),2,COMMAS=YES,CR=YES                               
         S     R5,DMCB+4                                                        
*                                                                               
         CLI   PBILELEM,X'28'     SEE IF DOING FINANCIAL BILL ELEM              
         BE    PRBY8E             YES - DON'T ROLL TO TOTALS                    
         LR    R0,R5                                                            
         A     R0,BUYTOT+8                                                      
         ST    R0,BUYTOT+8                                                      
PRBY8E   EDIT  (R5),(15,UNET),2,COMMAS=YES,CR=YES                               
         TM    PBBILST,X'40'                                                    
         BZ    PRBY9                                                            
         CLI   QOPT2,C'R'        SEE IF OK TO UNBILL REVERSALS                  
         BE    PRBY8F                                                           
******** CLI   QOPT2,C'B'        SEE IF OK TO UNBILL REVERSALS                  
******** BE    PRBY8F                                                           
         B     PRBY8G           NOT OK - DON'T ADD TO TOTALS                    
*                                                                               
PRBY8F   DS    0H                                                               
******** L     R1,PBGROSS                                                       
         MVC   FULL,PBGROSS                                                     
         L     R1,FULL                                                          
         A     R1,UBGROSS                                                       
         ST    R1,UBGROSS                                                       
*******  L     R1,PBGROSS+4                                                     
         MVC   FULL,PBGROSS+4                                                   
         L     R1,FULL                                                          
         A     R1,UBGROSS+4                                                     
         ST    R1,UBGROSS+4                                                     
*******  L     R1,PBGROSS+8                                                     
         MVC   FULL,PBGROSS+8                                                   
         L     R1,FULL                                                          
         A     R1,UBGROSS+8                                                     
         ST    R1,UBGROSS+8                                                     
PRBY8G   CLI   QOPT2,C'R'          OK TO DO REVERSALS                           
         BE    PRBY9                                                            
*******  CLI   QOPT2,C'B'                                                       
*******  BE    PRBY9                                                            
         LA    R6,P2                                                            
         MVC   UBYDAT(32),=C'** ATTEMPT TO UNBILL REVERSAL **'                  
         OI    ERRSW,X'01'                                                      
         LA    R6,P1                                                            
PRBY9    BAS   RE,RPRT                                                          
         OI    BYSW,X'01'                                                       
         MVI   PBBILST,0                                                        
         XC    PBLDATE,PBLDATE                                                  
         XC    PBGROSS(12),PBGROSS                                              
*                                                                               
         B     PRBY4                                                            
PRBY10   EQU   *                                                                
         TM    BYSW,X'03'                                                       
         BZ    EXIT                                                             
         OC    REVINV,REVINV                                                    
         BZ    PRBY15                                                           
         CLC   RVGROSS(12),UBGROSS                                              
         BE    PRBY12                                                           
         LA    R6,P2                                                            
         MVC   UBYDAT(32),=C'** UNEQUAL REVERSAL DOLLARS **  '                  
         OI    ERRSW,X'02'                                                      
         LA    R6,P1                                                            
         BAS   RE,RPRT                                                          
PRBY12   TM    BYSW,X'03'                                                       
         BO    PRBY15                                                           
         LA    R6,P2                                                            
         MVC   UBYDAT(32),=C'** REVERSAL MISMATCH **         '                  
         OI    ERRSW,X'04'                                                      
         LA    R6,P1                                                            
         BAS   RE,RPRT                                                          
PRBY15   LA    RF,PBUYREC                                                       
         ST    RF,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
                                                                                
         CLI   QOPT6,C'L'                                                       
         BNE   EXIT                 NOT LIVE RUN                                
                                                                                
         GOTO1 PUTPRT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                  LAST BUY FOR REQUEST                         
LBYR     EQU   *                                                                
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         MVI   SPACING,2                                                        
         MVC   P1(06),=C'TOTALS'                                                
         MVC   P2(06),=C'------'                                                
LBYR2    DS    0H                                                               
         BAS   RE,RPRT                                                          
         MVC   P1(13),=C'DELETED BILLS'                                         
         LA    R2,BILTOT                                                        
         BAS   RE,EDIT3                                                         
         BAS   RE,RPRT                                                          
         MVC   P1(16),=C'UNREVERSED BILLS'                                      
         LA    R2,REVTOT                                                        
         BAS   RE,EDIT3                                                         
         BAS   RE,RPRT                                                          
         MVC   P1(13),=C'UNBILLED BUYS'                                         
         LA    R2,BUYTOT                                                        
         BAS   RE,EDIT3                                                         
         BAS   RE,RPRT                                                          
         CLI   MODE,RUNLAST                                                     
         BE    LBYR2F                                                           
*                                                                               
         CLI   SOONSW,C'Y'          ONLY CHECK $ ON SOON REQ                    
         BNE   LBYR2C                                                           
         CLI   ACTIVITY,C'Y'        CHECK FOR ANY ACTIVITY                      
         BNE   REQL55                                                           
         CLI   ERRSW,0                                                          
         BNE   REQL40               ERROR ENCOUNTERED                           
         L     R2,BILTOT                                                        
         CVD   R2,DUB                                                           
         CP    DUB,HEADER$P         SEE IF I MATCH REQUEST $                    
         BNE   REQL40                                                           
         CP    DETAIL$P,=P'0'                                                   
         BE    REQL50                                                           
         L     R2,BUYTOT                                                        
         S     R2,REVTOT                                                        
         CVD   R2,DUB                                                           
         CP    DUB,DETAIL$P                                                     
         BNE   REQL40                                                           
         B     REQL50                                                           
         EJECT                                                                  
REQL40   DS    0H                                                               
         MVC   P1(44),=C'********************************************'          
         MVC   P2(44),=C'*** COMPUTED AND EXPECTED TOTALS UNEQUAL ***'          
         MVC   P3(44),=C'***   OR SOME OTHER ERROR ENCOUNTERED    ***'          
         MVC   P4(44),=C'***  NO LIVE REQUESTS HAVE BEEN WRITTEN  ***'          
         MVC   P5(44),=C'********************************************'          
         GOTO1 REPORT              PRINT WARNING FOR DATA CONTROL               
         B     REQLX                                                            
*                                                                               
REQL50   DS    0H                                                               
         CLI   SOONSW,C'Y'         SUBMITTED VIA REQUEST PROGRAM?               
         BNE   REQLX               YES: IT'S AN UPDATIVE SOON                   
         CLI   ACTIVITY,C'Y'       ANY RECORDS TO UPDATE?                       
         BE    REQL60              YES                                          
*                                                                               
REQL55   MVC   P1(34),=C'**********************************'                    
         MVC   P2(34),=C'*** NO RECORDS FOUND TO UNBILL ***'                    
         MVC   P3(34),=C'**********************************'                    
         GOTO1 REPORT              PRINT WARNING FOR DATA CONTROL               
         B     REQLX                                                            
*                                                                               
REQL60   DS    0H                  BUILD UNBILLING REQUEST                      
         XC    REQHDR,REQHDR       26-BYTE REQUEST HEADER                       
         MVC   REQUEST,QRECORD             COPY THE SOON REQUEST...             
         MVI   QOPT6-QRECORD+REQUEST,C'L'  ...SET THE 'LIVE' FLAG               
         MVI   QCONTREQ-QRECORD+REQUEST,C' ' ...SINGLE-CARD REQUEST             
*                                                                               
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   REQL70              NO                                           
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'PREQUEST',WORK,REQHDR                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P1(41),=C'*****************************************'             
         MVC   P2(41),=C'*** LIVE UNBILLING WILL RUN OVERNIGHT ***'             
         MVC   P3(41),=C'*****************************************'             
         MVI   P4,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
REQL70   DS    0H                                                               
         MVC   P1(24),=C'UNBILLING REQUEST CARD: '                              
         MVC   P1+24(80),REQUEST                                                
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
REQL80   DS    0H                                                               
         CLI   POSTSW,X'01'        POSTED BILL UNBILLED?                        
         BNE   REQLX                                                            
*                                                                               
         MVC   REQUEST,SPACES                                                   
         LA    RF,REQUEST                                                       
         MVC   QPROG-QRECORD(,RF),=C'MY'      SET TO 'MY' REQUEST               
         MVC   QAGENCY-QRECORD(,RF),QAGENCY   AGENCY                            
         MVC   QMEDIA-QRECORD(,RF),QMEDIA     MEDIA                             
         MVC   QCLIENT-QRECORD(,RF),QCLIENT   CLIENT                            
         MVC   QPRODUCT-QRECORD(,RF),QPRODUCT PRODUCT                           
         MVC   QPUBALL-QRECORD(3,RF),=C'ALL'   BILLING TYPES                    
         MVI   QOPT1-QRECORD(RF),C'R'         'REVERSE' OPTION                  
****     MVI   QOPT2-QRECORD(RF),C'N'         DEFAULT SUB-MEDIA TO 'N'          
****     CLI   BLMED,C' '                                                       
****     BNH   *+10                                                             
****     MVC   QOPT2-QRECORD(,RF),BLMED       SUB-MEDIA                         
         MVI   QOPT4-QRECORD(RF),C'U'         'UNPOST' OPTION                   
         MVC   QUESTOR-QRECORD(,RF),QUESTOR   REQUESTOR NAME                    
         XC    REQHDR,REQHDR       CLEAR UNPOSTING REQUEST HEADER               
         MVC   REQDEST,=AL2(17)    FORCE UNPOSTING REPORT TO SJR                
         CLI   RCWRITE,C'Y'        WRITE=YES?                                   
         BNE   REQL90              NO                                           
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',WORK,REQHDR                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P1(41),=C'*****************************************'             
         MVC   P2(41),=C'*** LIVE UNPOSTING WILL RUN OVERNIGHT ***'             
         MVC   P3(41),=C'*****************************************'             
         MVI   P4,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
REQL90   DS    0H                                                               
         MVC   P1(24),=C'UNPOSTING REQUEST CARD: '                              
         MVC   P1+24(80),REQUEST                                                
         GOTO1 REPORT                                                           
*                                                                               
REQLX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
LBYR2C   CLI   QOPT1,C'D'                                                       
         BE    LBYR2F                                                           
         CLI   QOPT1,C'B'            OR NEW BILLING                             
         BE    LBYR2F                                                           
         CLI   QOPT1,C'R'            OR REBATE BILLING                          
         BNE   LBYR2D                                                           
*                                                                               
LBYR2F   L     R2,BILTOT                                                        
         A     R2,REVTOT                                                        
         C     R2,BUYTOT                                                        
         BNE   LBYR2ERR                                                         
         L     R2,BILTOT+4                                                      
         A     R2,REVTOT+4                                                      
         C     R2,BUYTOT+4                                                      
         BNE   LBYR2ERR                                                         
         L     R2,BILTOT+8                                                      
         A     R2,REVTOT+8                                                      
         C     R2,BUYTOT+8                                                      
         BE    LBYR2D                                                           
*                                                                               
LBYR2ERR MVC   P1(25),=C'** TOTALS DO NOT AGREE **'                             
         BAS   RE,RPRT                                                          
*                                                                               
LBYR2D   DS    0H                                                               
*                                  ROLL TO REPORT TOTALS                        
         LA    R0,9                                                             
         LA    R1,BILTOT                                                        
LBYR3    DS    0H                                                               
         L     RF,0(R1)                                                         
         A     RF,36(R1)                                                        
         ST    RF,36(R1)                                                        
         LA    R1,4(R1)                                                         
         BCT   R0,LBYR3                                                         
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
LAST     DS    0H                                                               
         CLI   SOONSW,C'Y'       SOON RUN?                                      
         BE    EXIT              JUST EXIT - ONLY ONE REQUEST                   
*                                SKIP TOTALS                                    
         MVI   FORCEHED,C'Y'                                                    
         MVC   BILTOT(36),TBILTOT                                               
         MVC   P1(10),=C'RUN TOTALS'                                            
         MVC   P2(10),=C'----------'                                            
         MVI   SPACING,2                                                        
         MVC   STARTP(16),SPACES                                                
         MVC   UDATEP,SPACES                                                    
         B     LBYR2                                                            
         SPACE 2                                                                
FIRST    DS    0H                                                               
         XC    TBILTOT(36),TBILTOT                                              
         B     EXIT                                                             
         SPACE 3                                                                
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD3(6),=C'CLIENT'                                              
         MVC   HEAD4(7),=C'PRODUCT'                                             
         MVC   HEAD5(8),=C'ESTIMATE'                                            
         MVC   HEAD6(9),=C'START-END'                                           
         MVC   HEAD7(9),=C'BILL DATE'                                           
         MVC   HEAD3+10(3),QCLIENT                                              
         MVC   HEAD4+10(3),QPRODUCT                                             
         MVC   HEAD5+10(3),QEST                                                 
         CLI   QESTEND,C' '                                                     
         BE    *+14                                                             
         MVI   HEAD5+13,C'-'                                                    
         MVC   HEAD5+14(3),QESTEND                                              
         MVC   HEAD6+10(8),STARTP                                               
         MVC   HEAD6+19(8),ENDP                                                 
         CLC   HEAD6+10(17),SPACES                                              
         BE    *+8                                                              
         MVI   HEAD6+18,C'-'                                                    
         MVC   HEAD7+10(8),UDATEP                                               
*                                                                               
         MVC   HEAD5+47(15),=C'** WRITE=NO  **'                                 
         CLI   SOONSW,C'Y'         SOON REQ?                                    
         BE    RPRT2                                                            
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+56(3),=C'YES'                                              
RPRT2    MVI   RCSUBPRG,15                                                      
         CLI   MODE,RUNLAST                                                     
         BE    RPRT4                                                            
         MVI   RCSUBPRG,5                                                       
         CLI   QOPT1,C'S'          OLD SUMMARY                                  
         BE    RPRT4                                                            
         MVI   RCSUBPRG,10         OLD  DETAIL                                  
         CLI   QOPT1,C'D'                                                       
         BE    RPRT4                                                            
         MVI   RCSUBPRG,20         NEW BILLING                                  
         CLI   QOPT1,C'B'                                                       
         BE    RPRT4                                                            
         CLI   QOPT1,C'M'          NEW MAMUAL BILL                              
         BE    RPRT4                                                            
         MVI   RCSUBPRG,30                                                      
         CLI   QOPT1,C'R'          NEW REBATE BILLING                           
         BNE   CRDERR                                                           
*                                                                               
RPRT4    GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
EDIT3    EQU   *                                                                
         LA    R3,3                                                             
         LA    R4,UGRS                                                          
EDIT3A   EQU   *                                                                
         L     R0,0(R2)                                                         
         EDIT  (R0),(15,0(R4)),2,COMMAS=YES,CR=YES                              
         LA    R2,4(R2)                                                         
         LA    R4,16(R4)                                                        
         BCT   R3,EDIT3A                                                        
         BR    RE                                                               
         SPACE 3                                                                
ZEROS    DC    30C'0'                                                           
         LTORG                                                                  
         SPACE 3                                                                
UBWRKD   DSECT                                                                  
ACTIVITY DS    CL1                                                              
ERRSW    DS    XL1               X'01' - ATTEMPT TO UNBILL REVERSAL             
*                                X'02' - UNEQUAL REVERSAL $                     
*                                X'04' - REVERSAL MISMATCH                      
*                                X'08' - ATTEMPT TO UNBILL REV. MANUAL          
POSTSW   DS    XL1               X'01' - UNBILLING POSTED BILLS                 
*                                                                               
SOONSW   DS    CL1               Y = SOON RUN                                   
*                                                                               
HEADER$P DS    PL8               EXPECTED HEADER $                              
DETAIL$P DS    PL8               EXPECTED DETAIL $                              
*                                                                               
UBWORK   DS    0F                                                               
BILTOT   DS    3F                                                               
REVTOT   DS    3F                                                               
BUYTOT   DS    3F                                                               
TBILTOT  DS    3F                                                               
TREVTOT  DS    3F                                                               
TBUYTOT  DS    3F                                                               
STARTP   DS    CL8                                                              
ENDP     DS    CL8                                                              
BSTART   DS    XL3                                                              
BEND     DS    CL3                                                              
BYSW     DS    X                                                                
UDATE    DS    CL6                                                              
UDATEB   DS    XL3                                                              
UDATEP   DS    CL8                                                              
MYKEY    DS    CL32                                                             
TYPE     DS    X                                                                
MANINV   DS    H                                                                
MANINVC  DS    CL4                                                              
REVINV   DS    H            WHEN UNBILLING REVERSAL MUST SPECIFY                
REVINVC  DS    CL4          REVERSED INVOICE NUMBER                             
*                                                                               
         DS    0F                                                               
RVGROSS  DS    CL12                                                             
UBGROSS  DS    CL12                                                             
*                                                                               
REQHDR   DS    0XL26                                                            
       ++INCLUDE DMREQHDR                                                       
REQUEST  DS    CL80                BUILD OVERNIGHT REQUEST CARD HERE            
         SPACE 3                                                                
UBLIND   DSECT                                                                  
         DS    CL18                                                             
UBMED    DS    CL1                                                              
         DS    CL1                                                              
UBCLT    DS    CL3                                                              
         DS    CL1                                                              
UBPRD    DS    CL3                                                              
         DS    CL3                                                              
UBEST    DS    CL3                                                              
         DS    CL1                                                              
UBMOS    DS    CL6                                                              
         DS    CL1                                                              
UBBMN    DS    CL6                                                              
         DS    CL1                                                              
UBINV    DS    CL4                                                              
         DS    CL2                                                              
UBSEP    DS    CL3              ADJ/CD                                          
         DS    CL2                                                              
UBRET    DS    CL3              RET/SUM/COR/AOR                                 
         DS    CL3                                                              
UGRS     DS    CL15                                                             
         DS    CL1                                                              
UBIL     DS    CL15                                                             
         DS    CL1                                                              
UNET     DS    CL15                                                             
         ORG   UBMOS                                                            
UBYDAT   DS    CL8                                                              
         DS    CL1                                                              
UBPUB    DS    CL15                                                             
         DS    CL1                                                              
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
         PRINT ON                                                               
         ORG   Q2USER                                                           
QHEADER$ DS    CL12              BILL HEADER EXPECTED TOTAL                     
QDETAIL$ DS    CL12              DETAIL EXPECTED TOTAL (OPTIONAL)               
*                                SUPPLY IF IMBALANCE EXPECTED                   
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE PNNEWFILE         HAVE NEW DSECT FOR PPBILLREC                 
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094PPREP0702 05/18/10'                                      
         END                                                                    
