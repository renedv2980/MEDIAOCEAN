*          DATA SET PPREP0202T AT LEVEL 091 AS OF 08/27/03                      
*PHASE PP0202T,+0,NOAUTO                                                        
*                                                                               
************  CHANGE LOG  ************                                          
*                                                                               
*  SMYE  12/18/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
         TITLE 'PP0202  - PRINTPAK BILLING TAX CHECKER'                         
*        THIS A CLONE OF THE UNBILLING PROGRAM - PPREP0702                      
*                                                                               
*              QOPT1 = S OLD SUMMARY BILLING                                    
*              QOPT1 = D OLD DETAIL BILLING                                     
*              QOPT1 = B NEW BILLING                                            
*              QOPT1 = M NEW MANUAL BILLING                                     
*              QOPT1 = R FINANCIAL REBATE BILLING                               
*                                                                               
*              QOPT2 = P OK TO UNBILL POSTED BILLS                              
*                      R OK TO UNBILL REVERSALS                                 
*                      B OK TO UNBILL BOTH                                      
*                                                                               
*              QOPT3 = 4-7 NEW BILLING TYPE                                     
*                                                                               
*              QOPT4 = D  MARK BILLS DELETED                                    
*                      BLANK - CLOSED-OUT                                       
*                                                                               
*              QPAY(6) = BILLING START DATE                                     
*                        "ALL" = ANY DATE                                       
*              QPUB+1(4) = INVOIVE NUMBER                                       
*              QPUB+5(4) = REVERSED INVOICE NUMBER WHEN UNBILLING               
*                          REVERSAL                                             
*                                                                               
PP0202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**UNBILL                                                       
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,PP0202+4095                                                   
         LA    R8,1(R8)                                                         
         USING PP0202+4096,R8                                                   
*                                                                               
         LA    R9,P                                                             
         USING UBLIND,R9                                                        
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
         MVC   TODAY(2),RCDATE+6                                                
         MVC   TODAY+2(2),RCDATE                                                
         MVC   TODAY+4(2),RCDATE+3                                              
*                                                                               
         GOTO1 DATCON,DMCB,(0,TODAY),(3,TODAYB)                                 
*                                                                               
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
*                                                                               
INIT1E   MVC   UDATE,QPAY                                                       
INIT2    DS    0H                                                               
*                                  MONTH OF SERVICE                             
         MVC   STARTP(16),SPACES                                                
         XC    BSTART,BSTART                                                    
         MVC   BEND,=3X'FF'                                                     
         CLI   QSTART,C'0'                                                      
         BNH   INIT3                                                            
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*        GOTO1 (RF),(R1),,(3,STARTP)                                            
         GOTO1 DATCON,(R1),,(5,STARTP)                                          
INIT3    DS    0H                                                               
         CLI   QEND,C'0'                                                        
         BNH   INIT3B                                                           
*        GOTO1 DTCNV,DMCB,QEND,(1,BEND)                                         
         GOTO1 DATCON,DMCB,(0,QEND),(3,BEND)                                    
*        GOTO1 (RF),(R1),,(3,ENDP)                                              
         GOTO1 DATCON,(R1),,(5,ENDP)                                            
INIT3B   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    BILTOT(48),BILTOT   CLEAR ALL ACCUMS                             
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
         B     EXIT                                                             
CRDERR   EQU   *                                                                
         MVC   P(80),QRECORD                                                    
         MVC   P+85(16),=C'BAD CONTROL CARD'                                    
         BAS   RE,RPRT                                                          
         DC    H'0'                                                             
         EJECT                                                                  
*                                  PROCESS BILL                                 
PRBILL   EQU   *                                                                
         TM    KEY+25,X'80'                                                     
         BNZ   EXIT                                                             
         CLC   RCSVPRD,=C'ZZZ'     PREVENT DOING BILLS TWICE                    
         BE    EXIT                IF PRD=NO OR ALL RUN                         
         GOTO1 GETBILL             REREAD BILL TO BE SURE                       
         ZAP   MBILLTAX,=P'0'                                                   
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
         BL    PRBL6                                                            
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
         CLI   QOPT2,C'P'          POSTED OPTION                                
         BE    PRBL5B                                                           
         CLI   QOPT2,C'B'          POSTED OPTION                                
         BE    PRBL5B                                                           
*                                                                               
         OC    PBILPOST,PBILPOST                                                
         BZ    PRBL5B                                                           
*                                                                               
         BAS   RE,FMTBIL                                                        
         BAS   RE,RPRT                                                          
         MVC   P(50),=C'**ATTEMPT TO UNBILL POSTED BILLS-CONTACT SYSTEMX        
               S**'                                                             
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
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
         MVC   P(46),=C'**ATTEMPT TO UNBILL REVERSED ORI OR MAN BILL**'         
         MVC   PSECOND(12),PBILLCAN                                             
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
*                                                                               
PRBL5E   ZAP   DUB,PBILLGRS                                                     
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
*                                                                               
         BAS   RE,GETBTAX            CALC TAX FROM BILLREC                      
*                                    RETURNED IN MBILLTAX                       
         CVB   R0,MBILLTAX                                                      
         A     R0,BILTOT+12                                                     
         ST    R0,BILTOT+12                                                     
*                                                                               
         CLI   TYPE,C'D'                                                        
         BE    *+12                                                             
         MVI   TYPE,C'D'                                                        
         BAS   RE,RPRT                                                          
         MVC   P(11),=C'**DELETED**'                                            
*                                                                               
         CLI   QOPT4,C'D'                                                       
         BE    *+10                                                             
         MVC   P(12),=C'*CLOSED OUT*'                                           
*                                                                               
         BAS   RE,FMTBIL                                                        
         BAS   RE,RPRT                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   PRBL6                                                            
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
         BL    EXIT                                                             
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
         MVC   P(14),=C'**UNREVERSED**'                                         
         BAS   RE,FMTBIL                                                        
         BAS   RE,RPRT                                                          
         MVI   PBILLCAN,C'0'                                                    
         MVC   PBILLCAN+1(11),PBILLCAN                                          
         LA    RF,PBILLREC                                                      
         ST    RF,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
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
         BL    EXIT                                                             
*                                                                               
PRMAN3   CLI   MANINVC,C' '                                                     
         BNH   PRMAN5                                                           
         CLC   MANINVC,PBILMCAN+2  MATCH INVOIVE NUMBER                         
         BNE   EXIT                                                             
PRMAN5   ZAP   DUB,PBILLGRS                                                     
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
         MVC   P(14),=C'**UNREV MANS**'                                         
         BAS   RE,FMTBIL                                                        
         BAS   RE,RPRT                                                          
         MVI   PBILMCAN,C'0'                                                    
         MVC   PBILMCAN+1(11),PBILMCAN                                          
         LA    RF,PBILLREC                                                      
         ST    RF,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
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
         MVC   UBRET,=C'COM'              UP FRONT COMMISSION                   
         B     FMTB8                                                            
*                                                                               
FMTB7F   DS    0H                                                               
         TM    PBILCMSW,X'02'            SEE IF COMMISSION BILL- NET            
         BNO   FMTB8                                                            
         MVC   UBRET,=C'NET'              UP FRONT COMMISSION - NET             
         B     FMTB8                                                            
*                                                                               
FMTB8    MVC   UBMED,PBILKMED                                                   
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
         EDIT  (P8,MBILLTAX),(15,P+113),2,COMMAS=YES,CR=YES                     
         XIT                                                                    
         EJECT                                                                  
*                                  PROCESS BUY                                  
PRBY     EQU   *                                                                
         GOTO1 GETBUY              REREAD BUY TO BE SURE IT WAS                 
*                                  LAST RECORD READ                             
*                                                                               
         LA    RF,PBUYREC                                                       
         ST    RF,ADBUY                                                         
*                                                                               
         CLI   QOPT1,C'D'          OLD DETAIL                                   
         BE    PRBY2                                                            
         CLI   QOPT1,C'B'          NEW BILLING                                  
         BE    PRBY2                                                            
         CLI   QOPT1,C'R'          NEW REBATE BILLING                           
         BNE   EXIT                                                             
PRBY2    MVI   BYSW,0                                                           
         XC    RVGROSS(16),RVGROSS                                              
         XC    UBGROSS(16),UBGROSS                                              
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
*                                                                               
         BAS   RE,GETETAX            CALC TAX FROM ELEM                         
*                                    RETURNED IN MELEMTAX                       
*                                                                               
         OC    REVINV,REVINV         SEE IF UNBILLING REVERSAL                  
         BZ    PRBY7A                                                           
         CLC   REVINV,PBINVNO       FIND BILL ELEM IT REVERSED                  
         BNE   PRBY7A                                                           
         TM    PBBILST,X'80'        SEE IF REVERSED                             
         B     PRBY7A                                                           
*************    THIS REPORT DOESN'T CARE IF REVERSED                           
         BZ    PRBY7A                                                           
         NI    PBBILST,X'7F'        SET OFF REVERSED BYTE                       
         L     R1,PBGROSS                                                       
         LCR   R1,R1                                                            
         A     R1,RVGROSS                                                       
         ST    R1,RVGROSS                                                       
         L     R1,PBGROSS+4                                                     
         LCR   R1,R1                                                            
         A     R1,RVGROSS+4                                                     
         ST    R1,RVGROSS+4                                                     
         L     R1,PBGROSS+8                                                     
         LCR   R1,R1                                                            
         A     R1,RVGROSS+8                                                     
         ST    R1,RVGROSS+8                                                     
         CVB   R1,MELEMTAX                                                      
         LCR   R1,R1                                                            
         A     R1,RVGROSS+12                                                    
         ST    R1,RVGROSS+12                                                    
*                                                                               
         OI    BYSW,X'02'           SET FOUND REVERSED ELEM IND                 
         B     PRBY4                                                            
*                                                                               
PRBY7A   DS    0H                                                               
         OC    UDATEB,UDATEB         ANY DATE                                   
         BZ    PRBY7A5                                                          
*                                                                               
         CLC   PBLDATE,UDATEB                                                   
         BL    PRBY4                                                            
*                                                                               
PRBY7A5  OC    MANINV,MANINV                                                    
         BZ    PRBY7B                                                           
         CLC   MANINV,PBINVNO                                                   
         BNE   PRBY4                                                            
PRBY7B   DS    0H                                                               
         CLI   TYPE,C'B'                                                        
         BE    *+12                                                             
         MVI   TYPE,C'B'                                                        
         BAS   RE,RPRT                                                          
         MVC   P(12),=C'**UNBILLED**'                                           
         CLI   PBILELEM,X'28'      FINANICAL OPEN BILLING ELEM                  
         BNE   *+10                                                             
         MVC   P(12),=C'**OPEN UNB**'                                           
*                                                                               
         TM    PBBILST,X'01'       CHK UFC COMM                                 
         BZ    *+10                                                             
         MVC   P+14(3),=C'COM'                                                  
         TM    PBBILST,X'02'       CHK UFC NET                                  
         BZ    *+10                                                             
         MVC   P+14(3),=C'NET'                                                  
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
         XC    DMCB(16),DMCB                                                    
         TM    PBBILST,X'40'      SEE IF REVERSAL                               
         B     PRBY8B                                                           
************** THIS REPORT DOESN'T CARE IF REVERSAL                             
         BZ    PRBY8B                                                           
         CLI   QOPT2,C'R'         SEE IF OK TO UNBILL REVERSALS                 
         BE    PRBY8A                                                           
         CLI   QOPT2,C'B'         SEE IF OK TO UNBILL REVERSALS                 
         BE    PRBY8A                                                           
         B     PRBY8BX                                                          
*                                                                               
PRBY8A   OC    REVINV,REVINV      SEE IF REVERSED INV NUMBER GIVEN              
         BZ    PRBY8BX            NO - CAUSE IMBALANCE                          
*                                                                               
PRBY8B   MVC   DMCB(12),PBGROSS                                                 
         CVB   R0,MELEMTAX                                                      
         ST    R0,DMCB+12                                                       
*                                                                               
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
*                                                                               
         L     R5,DMCB+12                                                       
         CLI   PBILELEM,X'28'     SEE IF DOING FINANCIAL BILL ELEM              
         BE    PRBY8E5            YES - DON'T ROLL TO TOTALS                    
         LR    R0,R5                                                            
         A     R0,BUYTOT+12        BILLED TAX                                   
         ST    R0,BUYTOT+12                                                     
PRBY8E5  EDIT  (R5),(15,P+113),2,COMMAS=YES,CR=YES                              
*                                                                               
         TM    PBBILST,X'40'                                                    
         B     PRBY9                                                            
**************** THIS REPORT DOESN'T CARE IF REVERSAL                           
         BZ    PRBY9                                                            
         CLI   QOPT2,C'R'        SEE IF OK TO UNBILL REVERSALS                  
         BE    PRBY8F                                                           
         CLI   QOPT2,C'B'        SEE IF OK TO UNBILL REVERSALS                  
         BE    PRBY8F                                                           
         B     PRBY8G           NOT OK - DON'T ADD TO TOTALS                    
*                                                                               
PRBY8F   L     R1,PBGROSS                                                       
         A     R1,UBGROSS                                                       
         ST    R1,UBGROSS                                                       
         L     R1,PBGROSS+4                                                     
         A     R1,UBGROSS+4                                                     
         ST    R1,UBGROSS+4                                                     
         L     R1,PBGROSS+8                                                     
         A     R1,UBGROSS+8                                                     
         ST    R1,UBGROSS+8                                                     
*                                                                               
         CVB   R1,MELEMTAX                                                      
         A     R1,UBGROSS+12                                                    
         ST    R1,UBGROSS+12                                                    
*                                                                               
PRBY8G   CLI   QOPT2,C'R'          OK TO DO REVERSALS                           
         BE    PRBY9                                                            
         CLI   QOPT2,C'B'                                                       
         BE    PRBY9                                                            
         LA    R9,PSECOND                                                       
         MVC   UBYDAT(32),=C'** ATTEMPT TO UNBILL REVERSAL **'                  
         LA    R9,P                                                             
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
         CLC   RVGROSS(16),UBGROSS                                              
         BE    PRBY12                                                           
         LA    R9,PSECOND                                                       
         MVC   UBYDAT(32),=C'** UNEQUAL REVERSAL DOLLARS **  '                  
         LA    R9,P                                                             
         BAS   RE,RPRT                                                          
PRBY12   TM    BYSW,X'03'                                                       
         BO    PRBY15                                                           
         LA    R9,PSECOND                                                       
         MVC   UBYDAT(32),=C'** REVERSAL MISMATCH **         '                  
         LA    R9,P                                                             
         BAS   RE,RPRT                                                          
PRBY15   LA    RF,PBUYREC                                                       
         ST    RF,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
         GOTO1 PUTPRT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                  LAST BUY FOR REQUEST                         
LBYR     EQU   *                                                                
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         MVI   SPACING,2                                                        
         MVC   P(06),=C'TOTALS'                                                 
         MVC   PSECOND(06),=C'------'                                           
LBYR2    DS    0H                                                               
         BAS   RE,RPRT                                                          
         MVC   P(13),=C'DELETED BILLS'                                          
         LA    R2,BILTOT                                                        
         BAS   RE,EDIT4                                                         
         BAS   RE,RPRT                                                          
         MVC   P(16),=C'UNREVERSED BILLS'                                       
         LA    R2,REVTOT                                                        
         BAS   RE,EDIT4                                                         
         BAS   RE,RPRT                                                          
         MVC   P(13),=C'UNBILLED BUYS'                                          
         LA    R2,BUYTOT                                                        
         BAS   RE,EDIT4                                                         
         BAS   RE,RPRT                                                          
         CLI   MODE,RUNLAST                                                     
         BE    LBYR2F                                                           
         CLI   QOPT1,C'D'                                                       
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
         BNE   LBYR2ERR                                                         
*                                                                               
         L     R2,BILTOT+12                                                     
         A     R2,REVTOT+12                                                     
         C     R2,BUYTOT+12                                                     
         BE    LBYR2D                                                           
*                                                                               
LBYR2ERR MVC   P(25),=C'** TOTALS DO NOT AGREE **'                              
         BAS   RE,RPRT                                                          
*                                                                               
LBYR2D   DS    0H                                                               
*                                  ROLL TO REPORT TOTALS                        
         LA    R0,12                                                            
         LA    R1,BILTOT                                                        
LBYR3    DS    0H                                                               
         L     RF,0(R1)                                                         
         A     RF,48(R1)                                                        
         ST    RF,48(R1)                                                        
         LA    R1,4(R1)                                                         
         BCT   R0,LBYR3                                                         
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
LAST     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   BILTOT(48),TBILTOT                                               
         MVC   P(10),=C'RUN TOTALS'                                             
         MVC   PSECOND(10),=C'----------'                                       
         MVI   SPACING,2                                                        
         MVC   STARTP(16),SPACES                                                
         MVC   UDATEP,SPACES                                                    
         B     LBYR2                                                            
         SPACE 2                                                                
FIRST    DS    0H                                                               
         XC    TBILTOT(48),TBILTOT                                              
         B     EXIT                                                             
         EJECT                                                                  
GETETAX  NTR1                                                                   
         ZAP   MELEMTAX,=P'0'                                                   
*                                                                               
         L     R0,PBGROSS                                                       
         S     R0,PBAGYCOM                                                      
         ST    R0,DUB                                                           
*                                                                               
*        DUB HAS NET FROM BILL ELEM                                             
*                                                                               
         L     RF,ADBUY                                                         
         OC    PBDTAX-PBUYREC(3,RF),PBDTAX-PBUYREC(RF)                          
         BZ    NTAXCX                 IF NO TAX - DONE                          
*                                                                               
NTAXC2   TM    PBDCNDA-PBUYREC(RF),X'80'   SEE IF CANADIAN                      
         BZ    NTAXC40                NO USE OLD ROUTINE                        
*                                                                               
         MVC   FULL(1),PBDGST-PBUYREC(RF)                                       
         LA    RE,PGSTTAB                                                       
         CLI   FULL,0                                                           
         BNE   *+8                                                              
         MVI   FULL,C'S'           DEFAULT TO STANDARD                          
*                                                                               
NTAXC4   CLC   0(1,RE),FULL                                                     
         BE    NTAXC5                                                           
         LA    RE,8(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   NTAXC4                                                           
         B     NTAXC40         INVALID TAX CODE - TREAT LIKE NO GST             
*                                                                               
NTAXC5   OC    2(2,RE),2(RE)      CHK FOR GST PCT                               
         BZ    NTAXC40                                                          
         TM    1(RE),X'01'        SEE IF SALES TAX BASED ON NET                 
         BO    NTAXC40                                                          
         CLC   PBUYKDAT-PBUYREC(3,RF),4(RE)                                     
         BL    NTAXC40                                                          
         CLC   PBDPDATE-PBUYREC(3,RF),4(RE)                                     
         BL    NTAXC40                                                          
*                                                                               
NTAXC7   DS    0H                                                               
*        DUB = NET + SALES TAX % (NET + GST % OF NET)                           
*     SO DUB = NET + SALES TAX % (1+GST% X NET)                                 
*     SO DUB = NET + (SALES TAX % X (1+GST%)) NET                               
*     SO DUB = (1 + (SALES TAX % X (1+GST %))) X NET                            
*  THEREFORE NET = DUB DIVIDED BY (1+(SALES TAX % X (1+GST %)))                 
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),2(RE)   GST %                                          
         L     RE,FULL                                                          
         A     RE,=F'100000'     1 + GST % (3 DECIMALS)                         
         XC    FULL2,FULL2                                                      
         MVC   FULL2+1(3),PBDTAX-PBUYREC(RF)                                    
         L     RF,FULL2                                                         
         MR    RE,RE                                                            
         D     RE,=F'1000000' SINCE PBDTAX IS 4 DECIMALS                        
*                             RF = SALES TAX % X (1+GST%)                       
         A     RF,=F'100000'  RF = 1 + (SALES % X (1+GST%))                     
         ST    RF,FULL                                                          
         L     RF,DUB                                                           
         M     RE,=F'100000'                                                    
         SLDL  RE,1                                                             
         D     RE,FULL                                                          
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,DUB            SHOULD NOW BE NET                              
         B     TAXC3                                                            
*                                                                               
*                                                                               
NTAXC40  DS    0H                                                               
         XC    FULL2,FULL2                                                      
         MVC   FULL2+1(3),PBDTAX-PBUYREC(RF)                                    
*                                                                               
         L     RE,FULL2           N+TAX= N + TAX PCT(N)                         
         A     RE,=F'1000000'     N = N+TAX/100.0000 + TAX PCT                  
         ST    RE,FULL2                                                         
*                                                                               
         L     RF,DUB                                                           
         M     RE,=F'1000000'                                                   
         SLDL  RE,1                                                             
         D     RE,FULL2            SINCE PBDTAX HAS 4 DECIMALS                  
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,DUB                                                           
NTAXCX   DS    0H                                                               
*                                                                               
TAXC3    XC    FULL,FULL                                                        
         L     RF,ADBUY                                                         
         OC    PBDTAX-PBUYREC(3,RF),PBDTAX-PBUYREC(RF)                          
         BZ    TAXCX                                                            
**GST                                                                           
         TM    PBDCNDA-PBUYREC(RF),X'80'     SEE IF CANADIAN BUY                
         BZ    TAXC8                                                            
         MVC   FULL(1),PBDGST-PBUYREC(RF)                                       
         LA    RE,PGSTTAB                                                       
         CLI   FULL,0                                                           
         BNE   *+8                                                              
         MVI   FULL,C'S'           DEFAULT TO STANDARD                          
*                                                                               
TAXC4    CLC   0(1,RE),FULL                                                     
         BE    TAXC5                                                            
         LA    RE,8(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   TAXC4                                                            
         B     TAXC7              INVALID TAX CODE                              
*                                                                               
TAXC5    OC    2(2,RE),2(RE)      CHK FOR GST PCT                               
         BZ    TAXC7                                                            
*                                                                               
TAXC6    TM    1(RE),X'01'        SEE IF TAX BASED ON NET                       
         BO    TAXC7              TREAT AS NO GST                               
*                                                                               
TAXC6C   CLC   PBUYKDAT-PBUYREC(3,RF),4(RE)                                     
         BL    TAXC7                                                            
         CLC   PBDPDATE-PBUYREC(3,RF),4(RE)                                     
         BL    TAXC7                                                            
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),2(RE)                                                  
         L     RE,FULL                                                          
         L     RF,DUB                                                           
         MR    RE,RE                                                            
         SLDL  RE,1                                                             
         D     RE,=F'100000'          GST HAS 3 DECIMALS                        
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         A     RF,DUB                 ADD GST TO DUB (NET)                      
         ST    RF,DUB                                                           
*                                                                               
TAXC7    XC    FULL,FULL                                                        
TAXC8    DS    0H                                                               
         L     RF,ADBUY                                                         
         MVC   FULL+1(3),PBDTAX-PBUYREC(RF)                                     
*                                                                               
         L     RE,FULL                                                          
         L     RF,DUB                                                           
         MR    RE,RE                                                            
         SLDL  RE,1                                                             
         D     RE,=F'1000000'      SINCE PBDTAX HAS 4 DECIMALS                  
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,FULL            SALES TAX                                     
         CVD   RF,MELEMTAX                                                      
*                                                                               
TAXCX    XIT1                                                                   
         EJECT                                                                  
GETBTAX  NTR1            CALC TAX FROM BILLREC                                  
*                                                                               
*                        FIRST LOOK FOR TAX IN ELEM                             
*                                                                               
         LA    R3,PBILLREC+33                                                   
GETB5    CLI   0(R3),X'09'                                                      
         BE    GETB10                                                           
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    GETB20                                                           
         B     GETB5                                                            
*                                                                               
GETB10   CLI   1(R3),32                                                         
         BL    GETB20                                                           
         USING PBILOTH,R3                                                       
         L     R0,PBILLTAX                                                      
         CVD   R0,MBILLTAX                                                      
         B     GETBX                                                            
         DROP  R3                                                               
*                                                                               
GETB20   LA    R3,PBILLREC+33                                                   
GETB25   DS    0H                                                               
         CLI   0(R3),X'0A'                                                      
         BE    GETB30                                                           
         CLI   0(R3),0                                                          
         BE    GETBX                                                            
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETB25                                                           
*                                                                               
GETB30   DS    0H                                                               
         USING PBILVEL,R3                                                       
         ZAP   MBILLTAX,PBILLRCV                                                
         L     R0,PBILLVBS                                                      
         CVD   R0,DUB                                                           
         SP    MBILLTAX,DUB                                                     
*                                                                               
         DROP  R3                                                               
GETBX    XIT1                                                                   
**GST                                                                           
       ++INCLUDE PGSTTAB                                                        
         EJECT                                                                  
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD3(6),=C'CLIENT'                                              
         MVC   HEAD4(7),=C'PRODUCT'                                             
         MVC   HEAD5(8),=C'ESTIMATE'                                            
         MVC   HEAD6(9),=C'START-END'                                           
         MVC   HEAD7(8),=C'REQ DATE'                                            
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
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+56(3),=C'YES'                                              
         MVI   RCSUBPRG,15                                                      
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
EDIT4    EQU   *                                                                
         LA    R3,4                                                             
         LA    R4,UGRS                                                          
EDIT4A   EQU   *                                                                
         L     R0,0(R2)                                                         
         EDIT  (R0),(15,0(R4)),2,COMMAS=YES,CR=YES                              
         LA    R2,4(R2)                                                         
         LA    R4,16(R4)                                                        
         BCT   R3,EDIT4A                                                        
         BR    RE                                                               
         SPACE 3                                                                
ZEROS    DC    30C'0'                                                           
         LTORG                                                                  
         SPACE 3                                                                
UBWRKD   DSECT                                                                  
UBWORK   DS    0F                                                               
BILTOT   DS    4F                                                               
REVTOT   DS    4F                                                               
BUYTOT   DS    4F                                                               
TBILTOT  DS    4F                                                               
TREVTOT  DS    4F                                                               
TBUYTOT  DS    4F                                                               
*                                                                               
MBILLTAX DS    D                   BILLED TAX FROM BILLREC                      
MELEMTAX DS    D                   BILLED TAX FORM BILL ELEM                    
*                                                                               
TODAYB   DS    XL3                                                              
TODAY    DS    CL6                                                              
STARTP   DS    CL8                                                              
ENDP     DS    CL8                                                              
BSTART   DS    XL3                                                              
BEND     DS    CL3                                                              
ADBUY    DS    F                                                                
FULL2    DS    F                                                                
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
RVGROSS  DS    CL16                                                             
UBGROSS  DS    CL16                                                             
         SPACE 3                                                                
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
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091PPREP0202T08/27/03'                                      
         END                                                                    
