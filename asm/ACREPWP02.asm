*          DATA SET ACREPWP02  AT LEVEL 019 AS OF 03/23/15                      
*PHASE ACWP02A,+0                                                               
*INCLUDE DATVAL                                                                 
*INCLUDE SORTER                                                                 
         TITLE 'ACWP02 - WPP AGED DEBTORS REPORT'                               
ACWP02   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,**ACWP**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACWPD,RC                                                         
         ST    RA,SAVRA                                                         
         L     R6,VBIGPRNT                                                      
         USING BIGPRNTD,R6                                                      
         L     R7,ADBXAREA                                                      
         USING BOXD,R7                                                          
         MVC   BOXWIDTH,=F'198'                                                 
         EJECT                                                                  
*                                                                               
RNF00    CLI   MODE,RUNFRST                                                     
         BNE   RQF00                                                            
         RELOC RELO                                                             
         LA    RE,RELOTAB          RELOCATE THE A / V TYPES                     
         LA    R1,ATYPES                                                        
*                                                                               
RNF02    L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RNF02                                                            
*                                                                               
         MVC   HEADHOOK,ABXHOOK                                                 
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)                                                         
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         LA    R3,TOTSALE          CLEAR TOTAL SALES CATEGORY                   
         BAS   RE,CLR                                                           
         LA    R3,TOTPROV          CLEAR TOTAL PROVISIONS                       
         BAS   RE,CLR                                                           
         LA    R3,TOTDEBT          CLEAR TOTAL MAJOR DEBTORS                    
         BAS   RE,CLR                                                           
         LA    R3,TOTNDB           CLEAR TOTAL NET DEBTORS                      
         BAS   RE,CLR                                                           
         LA    R3,TOTPCT           CLEAR TOTAL PERCENT                          
         BAS   RE,CLR                                                           
         LA    R3,TOTLAST          CLEAR TOTAL LAST MONTH                       
         BAS   RE,CLR                                                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
RQF00    CLI   MODE,REQFRST        REQUEST FIRST                                
         BNE   ACF00                                                            
         MVI   AGY,OM              DEFAULT IS O&M                               
         CLC   ALPHAID,=C'HK'      HKNY                                         
         BE    RQF02                                                            
         CLC   ALPHAID,=C'HL'      HLTOC                                        
         BE    RQF02                                                            
         CLC   ALPHAID,=C'H8'      HQPQ                                         
         BNE   *+8                                                              
RQF02    MVI   AGY,HK                                                           
*                                                                               
         MVC   SAVPROF,PROGPROF                                                 
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FLNE,0                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         CLC   QEND,SPACES                                                      
         BE    *+10                                                             
         MVC   WORK(6),QEND        END IS YYMMDD                                
         GOTO1 DATCON,DMCB,(0,WORK),(5,ASOF+6)                                  
         MVI   LASTDAY,C'N'        IS IT LAST DAY OF MONTH                      
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'1'                                      
         CLC   WORK(4),WORK+6      SAME YYMM ?                                  
         BE    *+8                                                              
         MVI   LASTDAY,C'Y'        SET FOR LAST DAY OF MONTH                    
         LA    R3,CDATE                                                         
         BAS   RE,DATES            BUILD DATE LIST FOR CURRENT MONTH            
         MVC   SAVC_90,C_90                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(1,CDATE),(0,WORK)                                   
         BAS   RE,BKDATE           GET LAST MONTHS DATE                         
         LA    R3,LDATE                                                         
         BAS   RE,DATES            BUILD DATE LIST FOR LAST MONTH               
*                                                                               
         CLI   SAVPROF,C'Y'        BUILD MONTH BREAKOUT FOR DATE                
         BNE   RQF20               TABLES AND HEADLINES                         
         BAS   RE,MONBRK                                                        
*                                                                               
RQF20    GOTO1 DATCON,DMCB,(1,LDATE),(0,WORK)                                   
         BAS   RE,BKDATE           GET PRIOR DATE                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,PDATE)                                   
         BAS   RE,BKDATE           GET PRIOR DATE                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,XDATE)                                   
*                                                                               
         XC    LASTINV,LASTINV                                                  
         XC    THISINV,THISINV                                                  
*                                                                               
         LA    R1,SRTKLNQ          KEY FOR SORT                                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   SORTCARD+16(2),DUB+1  KEY LENGTH FOR SORT RECORD                 
*                                                                               
         LA    R1,SRTLNQ           RECORD LENGTH                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   RECCARD+22(3),DUB    FOR RECCARD                                 
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                                                               
*                                  BUILD ACCRUAL DATE FILTERS                   
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   MOA,ACMMEND            MOS END                                   
         MVC   MOA_1,ACMMEND                                                    
         MVC   MOA_2,ACMMEND                                                    
         CLI   MOA,X'FF'                                                        
         BE    RQF24                                                            
         GOTO1 GETMTH,DMCB,MOA,MOA_1,F'-1'                                      
         GOTO1 GETMTH,DMCB,MOA,MOA_2,F'-2'                                      
*                                                                               
RQF24    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
ACF00    CLI   MODE,PROCACC        PROCESS ACCOUNT                              
         BNE   PTRN00                                                           
         CLI   AGY,OM                                                           
         BNE   ACF05                                                            
         MVI   FCRDTRNS,C'N'       DEFAULT IS TO BYPASS TRANSACTIONS            
         L     R4,ADACC                                                         
         USING ACKEYD,R4                                                        
         LA    R1,FLTAB            MATCH HIGH LEVEL TO FILTER LIST              
*                                                                               
ACF03    CLI   0(R1),X'FF'                                                      
         BE    XIT                 NOT IN TABLE SKIP IT                         
         CLC   ACKEYACC+3(1),0(R1) ACCOUNT TO TABLE                             
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     ACF03                                                            
*                                                                               
ACF05    MVI   FCRDTRNS,C'Y'                                                    
         L     R4,ADHEIRA          LEVEL A CODE                                 
         MVC   LVA,3(R4)                                                        
         L     R4,ADLVANAM         GEL LEVEL A NAME                             
         BAS   RE,NAMOUT                                                        
         MVC   LVANME,WORK                                                      
*                                                                               
         L     R4,ADHEIRB          LEVEL B CODE - OFFICE                        
         MVC   LVB,3(R4)                                                        
         L     R4,ADLVBNAM         GEL LEVEL B NAME                             
         BAS   RE,NAMOUT                                                        
         MVC   LVBNME,WORK                                                      
*                                                                               
         L     R4,ADHEIRC          LEVEL C CODE - CLIENT                        
         MVC   LVC,3(R4)                                                        
         L     R4,ADLVCNAM         GET LEVEL C NAME                             
         BAS   RE,NAMOUT                                                        
         MVC   LVCNME,WORK                                                      
*                                                                               
         LA    R3,TOTCURR          CLEAR A ACCOUNT TOTAL RECORD                 
         BAS   RE,CLR                                                           
         LA    R3,TOTLAST          LAST MONTHS TOTAL                            
         BAS   RE,CLR                                                           
         ZAP   TOTPRIOR,=P'0'      PRIOR                                        
         ZAP   DR3,=P'0'                                                        
         ZAP   INVCURR,=P'0'                                                    
         ZAP   INVPREV,=P'0'                                                    
         ZAP   INVPRIOR,=P'0'                                                   
         XC    AGE,AGE                                                          
         XC    DUE,DUE                                                          
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS TRANSACTIONS                                             
*                                                                               
PTRN00   CLI   MODE,PROCTRNS                                                    
         BNE   SBL00                                                            
         L     R3,ADTRANS                                                       
         USING TRANSD,R3                                                        
         CLI   0(R3),X'44'         MUST BE A TRANSACTION                        
         BNE   XIT                                                              
*                                                                               
         CLI   TRNSTYPE,56         IGNORE REVERSALS                             
         BE    XIT                                                              
         CLC   TRNSDATE,CDATE                                                   
         BH    XIT                                                              
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVC   THISMOA,ACMMDTE                                                  
*                                                                               
PTRN07   MVC   THISREF,TRNSREF     CURRENT REFERENCE                            
         MVC   THISDATE,TRNSDATE   AND DATE                                     
         CLC   LASTINV,THISINV     IS THIS THE SAME INVOICE                     
         BE    PTRN08                                                           
         OC    LASTINV,LASTINV     FIRST TIME                                   
         BZ    PTRN08                                                           
         BAS   RE,PTB              POST LAST BILL TO TABLE                      
*                                                                               
PTRN08   MVC   LASTINV,THISINV     SAVE THE CURRENT BILL                        
         ZAP   DUB,TRNSAMNT        GET THE AMOUNT                               
         LR    R4,R3                                                            
         MVC   ADDED,TRNSDATE                                                   
         TM    TRNSSTAT,X'80'      IS IT A DEBIT                                
         BNO   PTRN19                                                           
         MVI   ELCODE,X'60'        GET DATE ADDED                               
         BAS   RE,NEXTEL                                                        
         BNE   PTRN10                                                           
         USING TRSTATD,R4                                                       
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(1,ADDED)                               
*                                                                               
PTRN10   OC    AGE,AGE             ALREADY HAVE AGE DATE                        
         BNZ   PTRN11                                                           
         MVC   AGE,THISDATE        DEFAULT IS BILL DATE                         
         CLI   QOPT1,C'D'          AGE BY DUE DATE                              
         BNE   PTRN11                                                           
         OC    DUE,DUE                                                          
         BNZ   PTRN11              ALREADY HAVE DUE DATE                        
         LR    R4,R3                                                            
         MVI   ELCODE,X'61'        GET DUE DATE                                 
         BAS   RE,NEXTEL                                                        
         BNE   PTRN11                                                           
         USING TRDUED,R4                                                        
         GOTO1 DATCON,DMCB,(2,TRDUDATE),(1,DUE)                                 
*                                                                               
PTRN11   CLC   TRNSDATE,SAVC_90                                                 
         BL    *+10                                                             
         AP    DR3,DUB             GET 90 DAYS DEBITS                           
         CLI   TRNSTYPE,55         TYPE 55'S ARE A LITTLE DIFFERENT             
         BE    PTRN13                                                           
         CLC   AGE,CDATE                                                        
         BH    PTRN30                                                           
         AP    INVCURR,DUB                                                      
         CLC   TRNSDATE,LDATE                                                   
         BH    PTRN30                                                           
         CLC   AGE,LDATE                                                        
         BH    PTRN30                                                           
         AP    INVPREV,DUB         ADD TO PREVIOUS MONTHS                       
         CLC   TRNSDATE,PDATE                                                   
         BH    PTRN30                                                           
         CLC   AGE,PDATE                                                        
         BH    PTRN30                                                           
         AP    INVPRIOR,DUB        ADD TO PRIOR MONTHS                          
         B     PTRN30                                                           
*                                                                               
*              HANDLE TYPE 55                                                   
PTRN13   CLC   AGE,CDATE           IF DATE WITHIN THIS PERIOD                   
         BH    PTRN30                                                           
         CLC   AGE,LDATE                                                        
         BNH   PTRN14                                                           
         CLI   MOA,X'FF'           IS THERE A MOA                               
         BE    *+14                                                             
         CLC   THISMOA,MOA                                                      
         BNE   PTRN14                                                           
         AP    INVCURR,DUB                                                      
         B     PTRN30                                                           
*                                                                               
PTRN14   CLC   AGE,PDATE                                                        
         BNH   PTRN15                                                           
         CLI   MOA_1,X'FF'           IS THERE A MOA                             
         BE    *+14                                                             
         CLC   THISMOA,MOA_1                                                    
         BNE   PTRN15                                                           
         AP    INVPREV,DUB         ADD TO PREVIOUS MONTHS                       
         B     PTRN30                                                           
*                                                                               
PTRN15   CLC   AGE,XDATE                                                        
         BNH   PTRN30                                                           
         CLI   MOA_2,X'FF'           IS THERE A MOA                             
         BE    *+14                                                             
         CLC   THISMOA,MOA_2                                                    
         BNE   PTRN30                                                           
         AP    INVPRIOR,DUB        ADD TO PRIOR MONTHS                          
         B     PTRN30                                                           
*                                                                               
*                                  SUBTRACT THE CREDITS                         
*                                  SCAN NARRATIVE FOR DATE                      
PTRN19   SR    R1,R1                                                            
         IC    R1,TRNSLEN                                                       
         SH    R1,=Y(TRNSLNQ+8)    COMPENSATE FOR L'DATE(MMMDD/YY)              
         BNP   PTRN25                                                           
         LA    R2,TRNSNARR(R1)     POINT TO END OF ELEMENT-8                    
         LR    R0,R1               GET NUMBER OF TIMES TO LOOP                  
*                                                                               
PTRN20   CLI   0(R2),C'S'          S/B AN ALPHA MONTH                           
         BH    PTRN21              LIKE SEP                                     
         CLI   0(R2),C'A'                                                       
         BL    PTRN21              OR APR                                       
         GOTO1 DATVAL,DMCB,(0,0(R2)),WORK                                       
         OC    0(4,R1),0(R1)       STOP IF FOUND A VALID DATE                   
         BNZ   PTRN22                                                           
PTRN21   SH    R2,=H'1'            BUMP BACK TO PREV CHAR                       
         BCT   R0,PTRN20                                                        
         B     PTRN25              DEFAULT TO TRNS DATE IF NO VALID DTE         
*                                                                               
PTRN22   GOTO1 DATCON,DMCB,(0,WORK),(1,ADDED)                                   
*                                                                               
*                                                                               
PTRN25   CLC   ADDED,CDATE         IF TRANSACTION AFTER END                     
         BH    PTRN30              SKIP IT                                      
         CLC   TRNSDATE,CDATE                                                   
         BH    PTRN30                                                           
         SP    INVCURR,DUB         SUBTRACT FROM CURRENT                        
         CLC   ADDED,LDATE         IF ADDED AFTER END OF LAST MONTH             
         BH    PTRN30                                                           
         CLC   TRNSDATE,LDATE                                                   
         BH    PTRN30                                                           
         SP    INVPREV,DUB                                                      
         CLC   ADDED,PDATE         SAME FOR PRIOR                               
         BH    PTRN30                                                           
         CLC   TRNSDATE,PDATE                                                   
         BH    PTRN30                                                           
         SP    INVPRIOR,DUB                                                     
*                                  ADD THIS ITEM TO INVOICE TABLE               
PTRN30   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*              SUB-ACCOUNT LAST                                                 
*                                                                               
SBL00    CLI   MODE,SBACLAST                                                    
         BNE   ACL00                                                            
         BAS   RE,PTB              ADD IN THE LAST BILL                         
         B     XIT                                                              
         EJECT                                                                  
*              ACCOUNT LAST                                                     
*                                                                               
ACL00    CLI   MODE,ACCLAST                                                     
         BNE   RQL00                                                            
         USING SRTD,R3                                                          
         LA    R3,TOTCURR          CURRENT TOTALS                               
         XC    SRTREC(SRTKLNQ),SRTREC                                           
         L     R4,ADHEIRB          FOR O&M                                      
         MVC   OFFC,4(R4)          LEVEL B IS OFFICE                            
         MVC   OFFNME,LVBNME                                                    
         CLI   AGY,OM                                                           
         BE    ACL03                                                            
         L     R4,ADHEIRA          FOR H&K                                      
         MVC   OFFC,3(R4)          LEVEL A IS LOCATION                          
         MVC   OFFNME,LVANME                                                    
*                                                                               
ACL03    MVC   SRTOFF,OFFC         OFFICE IS LEVEL 2                            
         CLI   LVB,0               ONLY WANT NAME RECORD                        
         BE    ACL05               ONE TIME                                     
         MVI   SRTTYP,SRTOFN       OFFICE NAME RECORD                           
         MVC   SRTNME,OFFNME                                                    
         GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         XC    LVB,LVB             CLEAR OFFICE CODE                            
*                                                                               
ACL05    MVI   SRTTYP,SRTSAL       SALES CATEGORY                               
         MVC   SRTACC,SPACES                                                    
         MVC   SRTCAT,LVA          LEVEL A (CATEGRY)                            
         MVC   SRTNME,LVANME       LEVEL A NAME                                 
         CLI   AGY,HK              FOR H&K LOACATION                            
         BNE   *+16                                                             
         XC    SRTCAT,SRTCAT       NO CATEGORY                                  
         XC    SRTNME,SRTNME                                                    
         ZAP   SRTDR3,DR3          90 DAYS DEBITS                               
         ZAP   SRTAR3,SRTTOT       BALANCE AT END OF EACH OF 3 MTHS             
         LA    RF,TOTLAST                                                       
         AP    SRTAR3,SRTTOT-SRTD(L'SRTAR3,RF)                                  
         AP    SRTAR3,TOTPRIOR                                                  
         CLI   AGY,HK              FOR H&K LOACATION                            
         BE    ACL07               NO WRITE-OFFS                                
         CLI   LVA,C'W'            WRITE-OFFS                                   
         BNE   ACL07                                                            
         MVI   SRTTYP,SRTWO        SPECIFIC PROVISIONS                          
         ZAP   SRTDR3,=P'0'                                                     
         ZAP   SRTAR3,=P'0'                                                     
*                                                                               
ACL07    GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         MVC   SRTOFF,=X'FFFF'                                                  
         BASR  RE,RF               COMPANY TOTAL                                
         CLI   SRTTYP,SRTWO        FOR WRITEOFFS, THIS IS IT                    
         BE    XIT                                                              
*                                                                               
*              MAJOR DEBTORS                                                    
*                                                                               
         CLI   AGY,HK              FOR H&K LOACATION                            
         BE    *+12                ALL RECEIVABLES                              
         CLI   LVC,C'C'                                                         
         BNE   ACL08                                                            
         MVC   SRTOFF,OFFC                                                      
         MVI   SRTTYP,SRTDBT       MAJOR DEBTORS                                
         MVC   SRTCAT,SPACES                                                    
         MVC   SRTACC(3),LVC+3     ACCOUNT CODE                                 
         MVC   SRTNME,LVCNME       ACCOUNT NAME                                 
         BASR  RE,RF               OFFICE RECORD                                
         MVC   SRTOFF,=X'FFFF'                                                  
         BASR  RE,RF               COMPANY TOTAL                                
*                                                                               
ACL08    LA    R3,TOTLAST          LAST MONTHS TOTALS                           
         XC    SRTREC(SRTKLNQ),SRTREC                                           
         MVC   SRTOFF,OFFC                                                      
         MVI   SRTTYP,SRTPRV       PREVIOUS                                     
         MVC   SRTCAT,SPACES                                                    
         MVC   SRTACC,SPACES                                                    
         GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         MVC   SRTOFF,=X'FFFF'                                                  
         BASR  RE,RF               COMPANY TOTAL                                
*                                                                               
         BAS   RE,CLR              CLEAR ACCUMS                                 
         ZAP   SRTTOT,TOTPRIOR     PRIOR IS ONLY ONE AMOUNT(NOT AGED)           
         MVC   SRTOFF,OFFC                                                      
         MVI   SRTTYP,SRTPRO       SET TYPE TO PRIOR                            
         GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         MVC   SRTOFF,=X'FFFF'                                                  
         BASR  RE,RF               OFFICE TOTAL                                 
         B     XIT                                                              
         EJECT                                                                  
*              REQUEST LAST                                                     
*                                                                               
RQL00    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         XC    HLDSRT,HLDSRT                                                    
         LA    R3,HLDSRT                                                        
*                                                                               
         USING SRTD,R3                                                          
RQL03    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,4(R1)                                                         
         LTR   R2,R2               TEST EOF                                     
         BZ    RQL09                                                            
         CLI   SRTOFF,0            FIRST TIME                                   
         BE    RQL25               SAVE FIRST RECORD                            
         CLC   SRTREC(SRTKLNQ),0(R2)                                            
         BNE   RQL07               NOT SAME KEY                                 
         LA    R4,SRTBK-SRTD(R2)   ADD SORT RECORD R2 TO HLDSRT                 
         LA    R5,SRTBK                                                         
         LA    R0,SRTNUM                                                        
         AP    0(L'SRTBK,R5),0(L'SRTBK,R4)                                      
         LA    R4,L'SRTBK(R4)                                                   
         LA    R5,L'SRTBK(R5)                                                   
         BCT   R0,*-14                                                          
         B     RQL03               GET THE NEXT RECORD                          
*                                                                               
RQL07    CLC   SRTOFF,SRTOFF-SRTREC(R2) SAME OFFICE                             
         BE    RQL09                                                            
         BAS   RE,PRT              PRINT REPORTS                                
         B     RQL25                                                            
*                                                                               
RQL09    CLI   SRTOFF,0                                                         
         BE    XIT                 NOTHING HELD                                 
         CLI   SRTTYP,SRTOFN       OFFICE NAME RECORD                           
         BNE   RQL10                                                            
         MVC   OFFNME,SRTNME       SAVE THE OFFICE NAME                         
         MVI   OFFACTV,C'N'        TURN-OFF OFFICE ACTIVITY                     
         B     RQL25                                                            
*                                                                               
RQL10    CP    SRTTOT,=P'0'                                                     
         BE    *+8                                                              
         MVI   OFFACTV,C'Y'        TURN-ON OFFICE ACTIVITY                      
         CLI   SRTTYP,SRTSAL       SALES CATEGORY TO BIN TABLE                  
         BNE   RQL11                                                            
         GOTO1 BINADD,DMCB,(R3),ASALTAB                                         
         B     RQL25                                                            
*                                                                               
RQL11    CLI   SRTTYP,SRTDBT       DEBTORS                                      
         BNE   RQL13                                                            
         ZAP   DUB,SRTTOT          ACCOUNT TOTAL                                
         SRP   DUB,64-2,5          ROUND TO NEAREST DOLLAR                      
         CP    DUB,=P'0'                                                        
         BH    *+8                                                              
         OI    SRTRNK,X'80'        NEGATIVES                                    
         CVB   R1,DUB                                                           
         LCR   R1,R1                                                            
         STCM  R1,15,SRTRNK+1      RANK THEM BY TOTAL AMOUNT                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,(R3)                                  
         B     RQL25                                                            
*                                                                               
*                                                                               
RQL13    CLI   SRTTYP,SRTWO        WRITEOFF (SPECIFIC PROVISIONS)               
         BNE   RQL15                                                            
         GOTO1 BINADD,DMCB,(R3),APROTAB                                         
         B     RQL25                                                            
*                                                                               
RQL15    CLI   SRTTYP,SRTPRV       PREVIOUS MONTHS                              
         BNE   RQL17                                                            
         MVC   TOTLAST,0(R3)                                                    
         B     RQL25                                                            
*                                                                               
RQL17    CLI   SRTTYP,SRTPRO       PRIOR MONTHS                                 
         BE    *+6                                                              
         DC    H'0'                BUG                                          
         ZAP   TOTPRIOR,SRTTOT                                                  
*                                                                               
RQL25    LTR   R2,R2               END OF SORT FILE                             
         BZ    RQL30                                                            
         MVC   HLDSRT,0(R2)        SAVE NEXT RECORD                             
         B     RQL03                                                            
*                                                                               
RQL30    BAS   RE,PRT              PRINT LAST REPORT                            
         MVI   RCSUBPRG,3          PRINT THE FOOTLINES                          
         MVI   FLNE,55                                                          
         BAS   RE,REPORT           GET TO LINE 55                               
         MVC   XP+1(L'MSG1),MSG1                                                
         BAS   RE,REPORT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              POST THE INVOICE TO ACCOUNT TOTALS                               
*                                                                               
PTB      NTR1  ,                                                                
         USING SRTD,R3                                                          
PTB03    LA    R3,TOTCURR                                                       
         ZAP   DUB,INVCURR         SET CURRENT AMOUNT                           
         LA    R4,CDATE            CURRENT DATES                                
         BAS   RE,AGEIT            SET AGEING COLUMNS FOR CURRENT               
*                                                                               
         ZAP   DUB,INVPREV                                                      
         LA    R3,TOTLAST                                                       
         LA    R4,LDATE            LAST MONTHS AGEING                           
         BAS   RE,AGEIT                                                         
         AP    TOTPRIOR,INVPRIOR   PRIOR TOTAL                                  
*                                                                               
         ZAP   INVCURR,=P'0'                                                    
         ZAP   INVPREV,=P'0'                                                    
         ZAP   INVPRIOR,=P'0'                                                   
         XC    AGE,AGE                                                          
         XC    DUE,DUE                                                          
         B     XIT                                                              
         EJECT                                                                  
*              POST AMOUNT TO PROPER AGEING COLUMNS                             
*                                                                               
         USING SRTD,R3                                                          
AGEIT    NTR1  ,                                                                
         LA    R1,LASTDATE                                                      
         CLI   QOPT1,C'D'                                                       
         BNE   AGE03                                                            
         LA    R1,AGE                                                           
         OC    DUE,DUE                                                          
         BZ    AGE03                                                            
         LA    R1,DUE                                                           
*                                                                               
AGE03    CLC   0(3,R1),CDATE       IF HIGHER THAN END                           
         BH    XIT                 DROP IT                                      
*                                                                               
         AP    SRTTOT,DUB          ADD TO TOTAL                                 
         LA    R5,SRTAGE           R5 TO AGEING ACCUMS                          
         LA    R4,3(R4)            R4 TO DATES FOR AGEING                       
         LA    R0,5                                                             
*                                                                               
AGE05    CLC   0(3,R1),0(R4)       DUE DATE OR BILLDATE                         
         BNL   AGE07               DUE IN THIS PERIOD                           
         LA    R5,L'SRTBK(R5)      R5 TO NEXT ACCUMS                            
         LA    R4,3(R4)            R4 TO NEXT DATE                              
         BCT   R0,AGE05                                                         
*                                                                               
AGE07    AP    0(L'SRTBK,R5),DUB   ADD TO AGEING ACCUMULATOR                    
         CLI   QOPT7,C'T'          TRACE OPTION                                 
         BNE   XIT                                                              
*                                                                               
         L     R4,ADACC                                                         
         USING ACKEYD,R4                                                        
         MVC   XP+3(12),ACKEYACC+3  ACCOUNT  DATE REFERENCE                     
         GOTO1 DATCON,DMCB,(1,LASTDATE),(5,XP+17)                               
         MVC   XP+27(6),LASTINV                                                 
         LA    R4,XP                                                            
         MVI   PCTSW,C'N'                                                       
         BAS   RE,FRMT                                                          
         CLI   BYTE,C'N'           NOTHING TO PRINT                             
         BE    XIT                                                              
         BAS   RE,REPORT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL PRINTING OF REPORT                            
*                                                                               
PRT      NTR1  ,                                                                
         CLI   OFFACTV,C'Y'                                                     
         BNE   XIT                 NO ACTIVITY FOR THIS OFFICE                  
         LA    R0,MXDBT                                                         
         XC    BUFREC,BUFREC                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFREC,1                             
         TM    DMCB+8,X'80'                                                     
         BO    PRT05               NO DATA                                      
*                                                                               
PRT03    GOTO1 BINADD,DMCB,BUFREC,ADBTTAB                                       
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFREC,1                              
         TM    DMCB+8,X'80'                                                     
         BO    PRT05                                                            
         BCT   R0,PRT03                                                         
*                                                                               
PRT05    BAS   RE,SALES            PRINT SALES CATEGORY SECTION                 
         BAS   RE,DEBTR            PRINT MAJOR DEBTORS SECTION                  
         BAS   RE,PROVR            SPECIFIC PROVISIONS REPORT                   
         BAS   RE,NETDR            NET DEBITORS                                 
         BAS   RE,PREVM            PRINT PREVIOUS MONTH TOTALS                  
*                                                                               
PRT10    LA    R3,TOTSALE          CLEAR TOTAL SALES CATEGORY                   
         BAS   RE,CLR                                                           
         LA    R3,TOTPROV          CLEAR TOTAL PROVISIONS                       
         BAS   RE,CLR                                                           
         LA    R3,TOTDEBT          CLEAR TOTAL MAJOR DEBTORS                    
         BAS   RE,CLR                                                           
         LA    R3,TOTNDB           CLEAR TOTAL NET DEBTORS                      
         BAS   RE,CLR                                                           
         LA    R3,TOTPCT           CLEAR TOTAL PERCENT                          
         BAS   RE,CLR                                                           
         LA    R3,TOTLAST          CLEAR TOTAL LAST MONTH                       
         BAS   RE,CLR                                                           
*                                                                               
         USING BIND,R5                                                          
         L     R5,ASALTAB          CLEAR BINTABLES                              
         XC    BININ,BININ                                                      
         L     R5,ADBTTAB                                                       
         XC    BININ,BININ                                                      
         L     R5,APROTAB                                                       
         XC    BININ,BININ                                                      
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ABUFF                                     
         B     XIT                                                              
         EJECT                                                                  
*              PRINT SALES CATEGORY SECTION                                     
*                                                                               
SALES    NTR1  ,                                                                
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   BOXROWS,XSPACES                                                  
         MVI   FLNE,3              START AT LINE 3                              
         BAS   RE,REPORT                                                        
         SR    RF,RF                                                            
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         MVI   2(RF),C'M'                                                       
         LA    R3,HL1                                                           
         CLI   SAVPROF,C'Y'                 MONTHS FOR AGING BREAKS             
         BNE   SAL01                        INSTEAD OF DAY BREAKOUT             
         LA    R3,HL2                                                           
SAL01    MVC   XPSECOND(HL1LNQ),0(R3)       HEADLINES                           
         MVI   SPACING,2                                                        
*                                                                               
SAL03    BAS   RE,REPORT                                                        
         L     R5,ASALTAB          R5 TO CONTROL LIST FOR TABLE                 
         USING BIND,R5                                                          
         ICM   R0,15,BININ         R1 NUMBER IN TABLE                           
         BZ    SAL07               IT'S EMPTY, NOTHING TO DO                    
         LA    R3,BINTABLE         R2 TO THE TABLE                              
         USING SRTD,R3                                                          
*                                                                               
SAL04    LA    R4,XP                                                            
         MVI   PCTSW,C'N'                                                       
         BAS   RE,FRMT             PRINT THE CASH                               
         CLI   BYTE,C'N'           NOTHING TO PRINT                             
         BE    SAL05                                                            
         MVC   XP+1(L'SRTNME),SRTNME                                            
         BAS   RE,REPORT                                                        
*                                                                               
SAL05    LA    R1,SRTBK            R1 TO CURRENT CATEGORY BUCKETS               
         LA    R2,TOTSALE          R2 TO TOTAL SALE CATEGORY                    
         LA    R2,SRTBK-SRTD(R2)   BUCKETS                                      
         LA    RF,SRTNUM           RF SET TO NUMBER OF ACCUMS                   
*                                                                               
         AP    0(L'SRTBK,R2),0(L'SRTBK,R1)  ADD TO TOTAL                        
         LA    R1,L'SRTBK(R1)                                                   
         LA    R2,L'SRTBK(R2)                                                   
         BCT   RF,*-14                                                          
*                                                                               
         LA    R3,SRTLNQ(R3)       NEXT ACCOUNT ENTRY                           
         BCT   R0,SAL04                                                         
*                                                                               
SAL07    BAS   RE,REPORT                                                        
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         MVI   2(RF),C'B'                                                       
         MVC   XPSECOND+1(5),=C'TOTAL'                                          
         LA    R3,TOTSALE          PRINT TOTAL FOR SALES CATEGORY               
         MVI   SRTTYP,SRTSAL                                                    
         LA    R4,XPSECOND                                                      
         MVI   PCTSW,C'N'                                                       
         BAS   RE,FRMT                                                          
         BAS   RE,REPORT                                                        
*                                                                               
         MVI   BOXREQ,C'C'         TURN OFF BOXES FOR NOW                       
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,XSPACES                                                  
         MVC   BOXCOLS,XSPACES                                                  
         MVC   BOXCOLSR,XSPACES                                                 
         BAS   RE,REPORT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PRINT MAJOR DEBITORS SECTION                                     
*                                                                               
DEBTR    NTR1  ,                                                                
         MVI   RCSUBPRG,1                                                       
         GOTO1 ABXHOOK             TURN THE BOXES ON AGAIN                      
*                                                                               
         SR    RF,RF                                                            
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         MVC   XPSECOND+9(13),=C'MAJOR DEBTORS'                                 
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         L     R5,ADBTTAB          R5 TO DEBITORS                               
         USING BIND,R5                                                          
         ICM   R0,15,BININ         R1 NUMBER IN TABLE                           
         BZ    DBTR07              IT'S EMPTY, NOTHING TO DO                    
         CH    R0,=H'10'           NUMBER OF MAJOR DEBTORS                      
         BNH   *+8                                                              
         LA    R0,10               NOT MORE THAN 10                             
         LA    R3,BINTABLE         R2 TO THE TABLE                              
         USING SRTD,R3                                                          
*                                                                               
DBTR03   LA    R4,XP               PRINT THE CASH                               
         MVI   PCTSW,C'N'                                                       
         BAS   RE,FRMT                                                          
         CLI   BYTE,C'N'           NOTHING TO PRINT                             
         BE    DBTR05                                                           
         MVC   XP+1(L'SRTNME),SRTNME                                            
         BAS   RE,REPORT                                                        
*                                                                               
DBTR05   LA    R1,SRTBK            R1 TO CURRENT CATEGORY BUCKETS               
         LA    R2,TOTDEBT          R2 TO TOTAL DEBTORS                          
         LA    R2,SRTBK-SRTD(R2)   BUCKETS                                      
         LA    RF,SRTRNUM          RF SET TO NUMBER OF ACCUMS                   
*                                                                               
         AP    0(L'SRTBK,R2),0(L'SRTBK,R1)  ADD TO TOTAL                        
         LA    R1,L'SRTBK(R1)                                                   
         LA    R2,L'SRTBK(R2)                                                   
         BCT   RF,*-14                                                          
*                                                                               
         LA    R3,SRTLNQ(R3)       NEXT ACCOUNT ENTRY                           
         BCT   R0,DBTR03                                                        
*                                                                               
DBTR07   BAS   RE,REPORT           SKIP A LINE                                  
         SR    RF,RF                                                            
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         MVI   2(RF),C'M'                                                       
         MVC   XPSECOND+1(5),=C'TOTAL'                                          
         LA    R3,TOTDEBT                                                       
         LA    R4,XPSECOND                                                      
         MVI   PCTSW,C'N'                                                       
         BAS   RE,FRMT                                                          
         BAS   RE,REPORT                                                        
*                                  GET PERCENT OF TOTDEBT TO TOTSALE            
         LA    R1,TOTSALE          R1 TO TOTAL SALES                            
         LA    R1,SRTBK-SRTD(R1)   BUCKETS                                      
         LA    R2,TOTDEBT          R2 TO TOTAL DEBTORS                          
         LA    R2,SRTBK-SRTD(R2)   BUCKETS                                      
         LA    R3,TOTPCT           R3 TO TOTAL PERCENT                          
         LA    R3,SRTBK-SRTD(R3)   BUCKETS                                      
         LA    RF,SRTRNUM          RF SET TO NUMBER OF ACCUMS                   
*                                                                               
DBTR10   ZAP   WORK(16),0(L'SRTBK,R2)  DEBTORS TOTAL                            
         MP    WORK(16),=P'10000'      X 10000                                  
         CP    0(L'SRTBK,R1),=P'0'                                              
         BE    DBTR12                                                           
         DP    WORK(16),0(L'SRTBK,R1)  DIVIDED BY SALES TOTAL                   
         ZAP   0(L'SRTBK,R3),WORK(8)   GIVES PCT OF DEBTORS TO TOTAL            
*                                                                               
DBTR12   LA    R1,L'SRTBK(R1)                                                   
         LA    R2,L'SRTBK(R2)                                                   
         LA    R3,L'SRTBK(R3)                                                   
         BCT   RF,DBTR10                                                        
*                                                                               
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         MVI   2(RF),C'B'                                                       
         MVC   XPSECOND+1(22),=C'% OF THIS MONTHS TOTAL'                        
         LA    R3,TOTPCT                                                        
         LA    R4,XPSECOND                                                      
         MVI   PCTSW,C'Y'                                                       
         BAS   RE,FRMT                                                          
         BAS   RE,REPORT                                                        
*                                                                               
         MVI   BOXREQ,C'C'         TURN OFF BOXES FOR NOW                       
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,XSPACES                                                  
         MVC   BOXCOLS,XSPACES                                                  
         MVC   BOXCOLSR,XSPACES                                                 
         BAS   RE,REPORT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PRINT SPECIFIC PROVISIONS SECTION                                
*                                                                               
PROVR    NTR1  ,                                                                
         MVI   RCSUBPRG,1                                                       
         GOTO1 ABXHOOK                                                          
         SR    RF,RF                                                            
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         MVC   XPSECOND+9(19),=C'SPECIFIC PROVISIONS'                           
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         L     R5,APROTAB          R5 TO CONTROL LIST FOR TABLE                 
         USING BIND,R5                                                          
         ICM   R0,15,BININ         R1 NUMBER IN TABLE                           
         BZ    PRO07               IT'S EMPTY, NOTHING TO DO                    
         LA    R3,BINTABLE         R2 TO THE TABLE                              
         USING SRTD,R3                                                          
*                                                                               
PRO03    LA    R4,XP                                                            
         MVI   PCTSW,C'N'                                                       
         BAS   RE,FRMT             PRINT THE CASH                               
         CLI   BYTE,C'N'           NOTHING TO PRINT                             
         BE    PRO05                                                            
         MVC   XP+1(L'SRTNME),SRTNME                                            
         BAS   RE,REPORT                                                        
*                                                                               
PRO05    LA    R1,SRTBK            R1 TO CURRENT CATEGORY BUCKETS               
         LA    R2,TOTPROV          R2 TO TOTAL PROVISION                        
         LA    R2,SRTBK-SRTD(R2)   BUCKETS                                      
         LA    RF,SRTRNUM          RF SET TO NUMBER OF ACCUMS                   
*                                                                               
         AP    0(L'SRTBK,R2),0(L'SRTBK,R1)  ADD TO TOTAL                        
         LA    R1,L'SRTBK(R1)                                                   
         LA    R2,L'SRTBK(R2)                                                   
         BCT   RF,*-14                                                          
*                                                                               
         LA    R3,SRTLNQ(R3)       NEXT ACCOUNT ENTRY                           
         BCT   R0,PRO03                                                         
*                                                                               
PRO07    BAS   RE,REPORT                                                        
         SR    RF,RF                                                            
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         MVI   2(RF),C'B'                                                       
         MVC   XPSECOND+1(16),=C'TOTAL PROVISIONS'                              
         LA    R3,TOTPROV          PRINT TOTAL FOR PROVISIONS                   
         LA    R4,XPSECOND                                                      
         MVI   PCTSW,C'N'                                                       
         BAS   RE,FRMT                                                          
         BAS   RE,REPORT                                                        
*                                                                               
         MVI   BOXREQ,C'C'         TURN OFF BOXES FOR NOW                       
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,XSPACES                                                  
         MVC   BOXCOLS,XSPACES                                                  
         MVC   BOXCOLSR,XSPACES                                                 
         BAS   RE,REPORT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PRINT NET DEBITORS TOTALS                                        
*                                                                               
NETDR    NTR1  ,                                                                
         MVI   RCSUBPRG,2                                                       
         GOTO1 ABXHOOK          TURN THE BOXES ON AGAIN                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         MVC   XPSECOND+5(21),=C'NET DEBTORS BALANCES '                         
         LA    R3,TOTNDB                                                        
         MVC   TOTNDB,TOTSALE      TOTAL SALES TO NET                           
         MVI   SRTTYP,0                                                         
         LA    R1,TOTNDB           R1 TO NET SALES                              
         LA    R1,SRTBK-SRTD(R1)   BUCKETS                                      
         LA    R2,TOTPROV          R2 TO TOTAL PROVISIONS                       
         LA    R2,SRTBK-SRTD(R2)   BUCKETS                                      
         LA    RF,SRTRNUM          RF SET TO NUMBER OF ACCUMS                   
*                                                                               
         AP    0(L'SRTBK,R1),0(L'SRTBK,R2)                                      
         LA    R1,L'SRTBK(R1)                                                   
         LA    R2,L'SRTBK(R2)                                                   
         BCT   RF,*-14                                                          
*                                                                               
         LA    R4,XPSECOND                                                      
         MVI   PCTSW,C'N'                                                       
         BAS   RE,FRMT                                                          
         BAS   RE,REPORT                                                        
*                                                                               
         BAS   RE,PCT              GET PERCENT FOR AGE COLUMNS                  
*                                                                               
         MVI   BOXREQ,C'C'         TURN OFF BOXES                               
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,XSPACES                                                  
         MVC   BOXCOLS,XSPACES                                                  
         MVC   BOXCOLSR,XSPACES                                                 
         BAS   RE,REPORT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PRINT PREVIOUS MONTHS TOTALS                                     
*                                                                               
PREVM    NTR1  ,                                                                
         MVI   RCSUBPRG,2                                                       
         GOTO1 ABXHOOK          TURN THE BOXES ON AGAIN                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         MVC   XPSECOND+5(21),=C'PREVIOUS MONTHS TOTAL'                         
         LA    R3,TOTLAST                                                       
         LA    R4,XPSECOND                                                      
         MVI   PCTSW,C'N'                                                       
         BAS   RE,FRMT                                                          
         BAS   RE,REPORT                                                        
*                                                                               
         BAS   RE,PCT                                                           
*                                                                               
         MVI   BOXREQ,C'C'         TURN OFF BOXES                               
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,XSPACES                                                  
         MVC   BOXCOLS,XSPACES                                                  
         MVC   BOXCOLSR,XSPACES                                                 
         BAS   RE,REPORT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              BUILD LIST OF DATES FOR PERIOD (R3)                              
DATES    NTR1  ,                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(1,0(R3))                                   
         LA    R3,3(R3)            NEXT DATE FIELD                              
*                                                                               
DATES02  LA    R0,5                                                             
         LA    R4,DAYS             BACKUP X NUMBER OF DAYS                      
DATES03  L     R2,0(R4)            NUMBER OF DAYS TO SUBTRACT                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,0(R3))                                   
         LA    R3,3(R3)            NEXT DATE FIELD                              
         LA    R4,4(R4)                                                         
         BCT   R0,DATES03                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              GET DATE FOR A PREVIOUS MONTH                                    
BKDATE   NTR1  ,                                                                
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'-1'                              
         MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK+6),(10,WORK+6)                               
         B     XIT                                                              
*                                                                               
*        PROCESS MONTH INSTEAD OF DAY BREAKOUT                                  
MONBRK   NTR1                                                                   
         LA    R3,CDATE+3          BUILD NEW CURRENT DATE TABLE                 
         MVC   HOLDDAT,CDATE       NEED TO SET FOR MONDATES                     
         BAS   RE,MONDATES                                                      
         LA    R3,LDATE+3          BUILD NEW LAST DATE TABLE                    
         MVC   HOLDDAT,C_60        NEED TO SET FOR MONDATES                     
         BAS   RE,MONDATES                                                      
         BAS   RE,MONHEAD          BUILD NEW MONTH HEADLINES                    
         B     XIT                                                              
*                                                                               
*        BUILD DATE TABLES WITH MONHT BREAKOUT INSTEAD OF DAYS                  
MONDATES NTR1  ,                                                                
         LA    R0,5                                                             
         MVC   WORK(3),HOLDDAT                  USE CURRENT (RUN ON)            
         MVI   WORK+2,X'01'                     OR END DATE                     
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK)    WORK WITH YYMMDD                
MOND10   GOTO1 DATCON,DMCB,(0,WORK),(1,0(R3))  YYMMDD TO PWOS FOR TAB           
         LA    R3,3(R3)                         BUMP TO NEXT TAB ENTRY          
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,F'-1'                                
         BCT   R0,MOND10                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ADD/SUBTRACT X  NUMBER OF MONTHS TO/FROM INPUT DATE.             
*              RETURN RESULT DATE (YYMM).                                       
GETMTH   NTR1  ,                                                                
         ICM   R3,15,8(R1)         NUMBER OF MONTHS                             
         L     R4,0(R1)            A(INPUT DATE)                                
         L     R5,4(R1)            A(RESULT DATE)                               
         MVC   WORK(2),0(R4)       INPUT DATE IN WORK - YM  (PWOS)              
         MVI   WORK+2,X'01'                    FILLER -   D                     
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK)  YMD TO YYMMDD                     
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,(R3)                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK)  YYMMFF TO YMD                     
         MVC   0(2,R5),WORK        INPUT DATE IN WORK - YM  (PWOS)              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        BUILD ALTERNATE HEADLINES FOR MONTHS INSTEAD OF DAYS                   
MONHEAD  NTR1                                                                   
         LA    R5,HL2B                      POINT TO DATE COLUMNS               
         LA    R4,5                         DO FIRST FIVE COLUMNS               
         MVC   WORK(3),CDATE                USE CURRENT (RUN ON) DATE           
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK)    WORK WITH YYMMDD                
MONHD10  GOTO1 DATCON,DMCB,(0,WORK),(6,WORK+3)                                  
         MVC   4(6,R5),WORK+3                   MOVE IN DATE                    
         LA    R5,L'HL2B(R5)                    BUMP TO NEXT COL SPACE          
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,F'-1'                                
MONHD12  BCT   R4,MONHD10                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(6,WORK+3)                                  
         MVC   4(6,R5),WORK+3                   MOVE IN DATE                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              FORMAT A LINE OF ACCUMULATORS                                    
*                                                                               
         USING SRTD,R3                                                          
FRMT     NTR1  ,                                                                
         LA    R4,37(R4)                                                        
         CLI   PCTSW,C'Y'          PRINTING PERCENTS                            
         BE    *+8                                                              
         LA    R4,1(R4)            R4 POINTS TO XP OR XPSECOND                  
         MVI   BYTE,C'N'           SET FOR NOTHING TO PRINT                     
         CP    SRTTOT,=P'0'        IS THE TOTAL ZERO                            
         BE    FRMT05                                                           
         MVI   BYTE,C'Y'                                                        
         CLI   PCTSW,C'Y'                                                       
         BE    FRMT03                                                           
         EDIT  (P8,SRTTOT),(13,0(R4)),2,MINUS=YES                               
         B     FRMT05                                                           
*                                                                               
FRMT03   EDIT  (P8,SRTTOT),(13,0(R4)),2,MINUS=YES,TRAIL=C'%'                    
*                                                                               
FRMT05   LA    R5,SRTAGE           R5 TO AGEING BUCKETS                         
         LA    R2,6                                                             
         LA    R4,15(R4)                                                        
*                                                                               
FRMT07   CP    0(L'SRTBK,R5),=P'0'                                              
         BE    FRMT12                                                           
         MVI   BYTE,C'Y'                                                        
         CLI   PCTSW,C'Y'                                                       
         BE    FRMT09                                                           
         EDIT  (P8,0(R5)),(13,0(R4)),2,MINUS=YES                                
         B     FRMT12                                                           
*                                                                               
FRMT09   EDIT  (P8,0(R5)),(13,0(R4)),2,MINUS=YES,TRAIL=C'%'                     
*                                                                               
FRMT12   LA    R4,15(R4)                                                        
         LA    R5,L'SRTBK(R5)                                                   
         BCT   R2,FRMT07                                                        
*                                                                               
         CLI   SRTTYP,SRTSAL       SALES CATEGORY                               
         BNE   XIT                                                              
         B     FRMT13                                                           
*********TO DEBUG DSO ***********************                                   
         LA    R5,SRTAR3                                                        
         EDIT  (P8,0(R5)),(13,0(R4)),2,MINUS=YES                                
         BAS   RE,REPORT                                                        
         LA    R5,SRTDR3                                                        
         EDIT  (P8,0(R5)),(13,0(R4)),2,MINUS=YES                                
         BAS   RE,REPORT                                                        
**********************************************                                  
FRMT13   ZAP   WORK(16),SRTAR3     3 MONTHS A/R                                 
         CP    SRTDR3,=P'0'                                                     
         BE    XIT                                                              
         MP    WORK(16),=P'300'    X 30 DAYS                                    
         DP    WORK(16),SRTDR3     / 3 MONTHS DEBITS                            
         SRP   WORK(8),64-1,5                                                   
         ZAP   DUB,WORK(8)                                                      
         EDIT  (P8,DUB),(10,0(R4))                                              
         B     XIT                                                              
         EJECT                                                                  
*              GET PERCENTS FOR AGEING COLUMNS                                  
*                                                                               
PCT      NTR1  ,                                                                
         SR    RF,RF                                                            
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         MVI   2(RF),C'B'                                                       
         MVC   XPSECOND+42(8),=C'100.00 %'                                      
*                                                                               
         CP    SRTTOT,=P'0'        GET COLUMN AS % OF TOTAL                     
         BE    PCT10                                                            
         LA    R4,XPSECOND+52                                                   
         LA    R2,6                                                             
         LA    R5,SRTAGE                                                        
*                                                                               
PCT02    CP    0(L'SRTBK,R5),=P'0'                                              
         BE    PCT04                                                            
         ZAP   WORK(16),0(L'SRTBK,R5)                                           
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),SRTTOT                                                  
         EDIT  (P8,WORK),(13,0(R4)),2,MINUS=YES,TRAIL=C'%'                      
*                                                                               
PCT04    LA    R4,15(R4)                                                        
         LA    R5,L'SRTBK(R5)                                                   
         BCT   R2,PCT02                                                         
*                                                                               
PCT10    BAS   RE,REPORT                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
         USING ACNAMED,R4                                                       
NAMOUT   LTR   R4,R4                                                            
         BZR   RE                                                               
         MVC   WORK,SPACES                                                      
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   WORK(0),ACNMNAME                                                 
*                                                                               
         USING SRTD,R3                                                          
CLR      LA    R1,SRTBK            R1 TO ACCUMULATORS                           
         LA    R0,SRTNUM           R0 SET TO NUMBER OF ACCUMS                   
         ZAP   0(L'SRTBK,R1),=P'0' CLEAR ALL THE ACCUMULATORS                   
         LA    R1,L'SRTBK(R1)                                                   
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
*              PRINT THE REPORT                                                 
*                                                                               
REPORT   NTR1  ,                                                                
         MVC   XHEAD1+143(4),=C'MADA' MONTHLY                                   
         CLI   QOPT2,C'W'                                                       
         BNE   *+10                                                             
         MVC   XHEAD1+143(4),=C'WADA' WEEKLY                                    
*                                                                               
         MVC   XHEAD4+72(L'ASOF),ASOF                                           
         MVC   XHEAD4+143(L'BASEB),BASEB                                        
         CLI   QOPT1,C'D'                                                       
         BNE   *+10                                                             
         MVC   XHEAD4+143(L'BASED),BASED                                        
*                                                                               
         LA    R3,HLDSRT                                                        
         CLI   SRTOFF,X'FF'        IS IT THE OVERALL OR OFFICE TOTAL            
         BE    RPT09                                                            
         MVC   XHEAD4+12(36),OFFNME                                             
         MVC   XHEAD4+1(6),=C'OFFICE'                                           
         CLI   AGY,HK                                                           
         BNE   RPT09                                                            
         MVC   XHEAD4+1(8),=C'LOCATION'                                         
*                                                                               
RPT09    GOTO1 ACREPORT                                                         
         CLI   FLNE,0              LOOKING FOR FIXED LINE                       
         BE    XIT                                                              
         CLC   LINE,FLNE           AT SPECIFIED LINE                            
         BL    RPT09                                                            
         MVI   FLNE,0                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ADD ITEM TO BINARY TABLE                                         
*                                                                               
         USING BIND,R5                                                          
BINADD   NTR1  ,                                                                
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
         BE    XIT                 NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         ZIC   R7,BINNUMB          NUMBER OF BUCKETS                            
         TM    BINSTAT,X'80'                                                    
         BO    BINBIN              DATA IS BINARY                               
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R7,*-14                                                          
         B     XIT                                                              
*                                                                               
BINBIN   L     RE,0(R3)                                                         
         L     RF,0(R4)                                                         
         AR    RF,RE                                                            
         ST    RF,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R7,BINBIN                                                        
XIT      XIT1                                                                   
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS                                                        
*                                                                               
RELOTAB  DS    0A                                                               
         DC    V(DATVAL)           V(DATVAL)                                    
         DC    V(SORTER)           V(SORTER)                                    
         DC    A(BXHOOK)           A(BOX HOOK ROUTINE)                          
         DC    A(SALTAB)           A(SALES SUMMARY)                             
         DC    A(PROTAB)           A(PROVISION TABLE)                           
         DC    A(DBTTAB)           A(DEBITORS SUMMARY)                          
         DC    A(BUFFALOC)         A(BUFFALOC)                                  
         DC    X'FF'                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(01,00,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000)'                                 
*                                                                               
*                                                                               
*        FILTER ON HIGH LEVEL ACCOUNT                                           
*                                                                               
FLTAB    DC    C'CIAW',X'FF'                                                    
*                                                                               
ASOF     DC    C'AS OF XXX99/99'                                                
BASEB    DC    C'BASED ON BILL DATE'                                            
BASED    DC    C'BASED ON DUE DATE '                                            
*                                                                               
*                                                                               
HL1      DC    CL38'         SALES CATEGORY'                                    
         DC    CL15'    TOTAL       '                                           
         DC    CL15'  0 - 30 DAYS   '                                           
         DC    CL15' 31 - 60 DAYS   '                                           
         DC    CL15' 61 - 90 DAYS   '                                           
         DC    CL15' 91 - 120 DAYS  '                                           
         DC    CL15'121 - 180 DAYS  '                                           
         DC    CL15' OVER 180 DAYS  '                                           
         DC    CL4' '                                                           
         DC    CL15'   DSO          '                                           
HL1LNQ   EQU   *-HL1                                                            
*                                                                               
HL2      DC    CL38'         SALES CATEGORY'                                    
HL2A     DC    CL15'    TOTAL       '                                           
HL2B     DC    CL15'                '                                           
         DC    CL15'                '                                           
         DC    CL15'                '                                           
         DC    CL15'                '                                           
         DC    CL15'                '                                           
         DC    CL15'     AND PRIOR  '                                           
         DC    CL4' '                                                           
         DC    CL15'   DSO          '                                           
HL2LNQ   EQU   *-HL2                                                            
*                                                                               
DAYS     DC    4F'-30',F'-60'      NUMBER OF DAYS IN EACH PERIOD                
*                                                                               
MSG1     DC    C'* ALL OFFICES INCLUDED FOR MULTI-OFFICE CLIENTS'               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BXHOOK   NMOD1 0,*BXHOOK*                                                       
         L     RC,BOXRC                                                         
         L     RA,SAVRA            RESTORE RA                                   
         L     R7,ADBXAREA                                                      
         CLI   RCSUBPRG,3                                                       
         BE    BOXXIT              NO BOXES FOR FOOTLINE                        
         USING BOXD,R7                                                          
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'        TURN ON THE BOXES                            
         MVC   BOXCOLS,XSPACES                                                  
         MVC   BOXCOLSR,XSPACES                                                 
*                                                                               
         CLI   RCSUBPRG,0          SALES CATEGORY                               
         BNE   BXH03                                                            
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+37,C'C'                                                  
         MVI   BOXCOLS+52,C'C'                                                  
         MVI   BOXCOLS+67,C'C'                                                  
         MVI   BOXCOLS+82,C'C'                                                  
         MVI   BOXCOLS+97,C'C'                                                  
         MVI   BOXCOLS+112,C'C'                                                 
         MVI   BOXCOLS+127,C'C'                                                 
         MVI   BOXCOLS+142,C'C'                                                 
         MVI   BOXCOLS+143,C'C'                                                 
         MVI   BOXCOLS+162,C'R'                                                 
         B     BOXXIT                                                           
*                                                                               
BXH03    EQU   *                                                                
         CLI   RCSUBPRG,1          MAJOR DEBITORS AND                           
         BNE   BXH05               SPECIFIC PROVISIONS                          
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+37,C'C'                                                  
         MVI   BOXCOLS+52,C'C'                                                  
         MVI   BOXCOLS+67,C'C'                                                  
         MVI   BOXCOLS+82,C'C'                                                  
         MVI   BOXCOLS+97,C'C'                                                  
         MVI   BOXCOLS+112,C'C'                                                 
         MVI   BOXCOLS+127,C'C'                                                 
         MVI   BOXCOLS+142,C'C'                                                 
         MVI   BOXCOLS+143,C'R'                                                 
*                                                                               
BXH05    EQU   *                                                                
         CLI   RCSUBPRG,2          NET DEBITORS AND                             
         BNE   BOXXIT              AND PREVIOUS MONTH                           
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+37,C'C'                                                  
         MVI   BOXCOLS+52,C'C'                                                  
         MVI   BOXCOLS+67,C'C'                                                  
         MVI   BOXCOLS+82,C'C'                                                  
         MVI   BOXCOLS+97,C'C'                                                  
         MVI   BOXCOLS+112,C'C'                                                 
         MVI   BOXCOLS+127,C'C'                                                 
         MVI   BOXCOLS+142,C'C'                                                 
         MVI   BOXCOLS+143,C'R'                                                 
*                                                                               
BOXXIT   EQU   *                                                                
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
MXSAL    EQU   20                  INCREASED FROM 10 TO 20 1/15/04              
         DS    0D                  SALES CATEGORY                               
         DC    CL8'**SALTAB*'                                                   
SALTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(SRTLNQ)         RECORD LENGTH                                
         DC    AL4(SRTKLNQ)        DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXSAL)          MAX. IN TABLE                                
         DC    AL1(SRTNUM)         NUMBER OF BUCKETS                            
         DC    AL1(SRTBK-SRTD)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXSAL*SRTLNQ)C     TABLE                                        
*                                                                               
*                                                                               
MXPRO    EQU   10                                                               
         DS    0D                  SPECIFIC PROVISIONS                          
         DC    CL8'**PROTAB*'                                                   
PROTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(SRTLNQ)         RECORD LENGTH                                
         DC    AL4(SRTKLNQ)        DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXPRO)          MAX. IN TABLE                                
         DC    AL1(SRTRNUM)        NUMBER OF BUCKETS                            
         DC    AL1(SRTBK-SRTD)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXPRO*SRTLNQ)C     TABLE                                        
*                                                                               
MXDBT    EQU   10                                                               
         DS    0D                  DEBITORS SUMMARY                             
         DC    CL8'**DBTTAB*'                                                   
DBTTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(SRTLNQ)         RECORD LENGTH                                
         DC    AL4(SRTKLNQ)        DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXDBT)          MAX. IN TABLE                                
         DC    AL1(SRTRNUM)        NUMBER OF BUCKETS                            
         DC    AL1(SRTBK-SRTD)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXDBT*SRTLNQ)C        TABLE                                     
*                                                                               
*                                                                               
         BUFF  LINES=1000,ROWS=1,COLUMNS=7,FLAVOR=P,COMMENT=46,        X        
               KEYLIST=(5,A)                                                    
         EJECT                                                                  
*                                                                               
ACWPD    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
DATVAL   DS    V                   V(DATVAL)                                    
SORTER   DS    V                   V(SORTER)                                    
ABXHOOK  DS    A                   A(BOX HOOK ROUTINE)                          
ASALTAB  DS    A                   A(SALES SUMMARY)                             
APROTAB  DS    A                   A(SPECIFIC PROVISIONS)                       
ADBTTAB  DS    A                   A(DEBITORS SUMMARY)                          
ABUFF    DS    A                   A(BUFFALOC)                                  
*                                                                               
HLDSRT   DS    CL(SRTLNQ)          HELD SORT RECORD                             
TOTCURR  DS    CL(SRTLNQ)          ACCOUNT TOTAL CURRENT                        
TOTSALE  DS    CL(SRTLNQ)          SALES CATEGORY TOTALS                        
TOTPROV  DS    CL(SRTLNQ)          PROVISIONS TOTALS                            
TOTDEBT  DS    CL(SRTLNQ)          MAJOR DEBTORS TOTALS                         
TOTNDB   DS    CL(SRTLNQ)          NET DEBITORS BALANCE                         
TOTPCT   DS    CL(SRTLNQ)          PERCENT OF TOTDEBT TO TOTSALE                
TOTLAST  DS    CL(SRTLNQ)          LAST MONTHS TOTALS                           
*                                                                               
TOTPRIOR DS    PL8                 TOTAL PRIOR                                  
*                                                                               
BUFREC   DS    CL(SRTLNQ)          BUFFALO RECORD                               
*                                                                               
DR3      DS    PL8                 90 DAYS DEBITS                               
AGE      DS    CL3                                                              
DUE      DS    CL3                                                              
ADDED    DS    CL3                                                              
LASTDAY  DS    CL1                                                              
*                                                                               
LVA      DS    CL12                LEVEL A CODE (CATEGORY)                      
LVANME   DS    CL36                LEVEL A NAME                                 
LVB      DS    CL12                LEVEL B CODE (OFFICE)                        
LVBNME   DS    CL36                LEVEL B NAME                                 
LVC      DS    CL12                ACCOUNT CODE (CLIENT)                        
LVCNME   DS    CL36                ACCOUNT NAME                                 
*                                                                               
FLNE     DS    XL1                                                              
SAVRA    DS    F                   REGISTER RA                                  
ELCODE   DS    CL1                                                              
PCTSW    DS    CL1                                                              
OFFACTV  DS    CL1                 OFFICE ACTIVITY SWITCH                       
OFFC     DS    CL2                 OFFICE\LOCATION                              
OFFNME   DS    CL36                OFFICE\LOCATION NAME                         
*                                                                               
LASTINV  DS    0CL9                SAVED                                        
LASTREF  DS    CL6                 BILL NUMBER                                  
LASTDATE DS    CL3                 AND DATE                                     
*                                                                               
THISINV  DS    0CL9                CURRENT                                      
THISREF  DS    CL6                 BILL NUMBER                                  
THISDATE DS    CL3                 AND DATE                                     
THISMOA  DS    CL2                                                              
*                                                                               
INVCURR  DS    PL8                 BALANCE IN CURRENT PERIOD                    
INVPREV  DS    PL8                 BALANCE IN PREVIOUS PERIOD                   
INVPRIOR DS    PL8                 BALANCE IN PRIOR PERIOD                      
*                                                                               
CDATE    DS    CL3                 LAST DAY OF CURRENT MONTH (PWOS)             
C_30     DS    CL3                 DATES FOR AGEING COLUMNS                     
C_60     DS    CL3                                                              
C_90     DS    CL3                                                              
C_120    DS    CL3                                                              
C_180    DS    CL3                                                              
LDATE    DS    CL3                 LAST DAY OF LAST MONTH (PWOS)                
         DS    5CL3                DATES FOR AGEING COLUMNS                     
PDATE    DS    CL3                 LAST DAY OF PRIOR MONTH (PWOS)               
XDATE    DS    CL3                                                              
SAVC_90  DS    CL3                 NEED FOR CALCULATING DSO                     
MOA      DS    CL2                 END MONTH OF SERVICE                         
MOA_1    DS    CL2                  "   "    "    "     FOR PREVIOUS            
MOA_2    DS    CL2                  "   "    "    "     FOR PRIOR               
HOLDDAT  DS    CL3                 SET UP FOR MONDATE ROUTINE                   
SAVPROF  DS    CL16                SAVE PROFILE                                 
*                                                                               
AGY      DS    XL1                 AGENCY CODE                                  
OM       EQU   X'01'               O&M DEFAULT                                  
HK       EQU   X'02'               H&K                                          
         EJECT                                                                  
*               DSECT FOR SORT / BUFFALO RECORDS                                
*                                                                               
SRTD     DSECT                                                                  
SRTREC   DS    0C                                                               
SRTRNK   DS    BL5                 RANK FOR BUFFALO                             
SRTOFF   DS    CL2                 OFFICE                                       
SRTTYP   DS    XL1                 TYPE                                         
SRTOFN   EQU   0                   OFFICE NAME RECORD                           
SRTSAL   EQU   1                   SALES CATEGORY                               
SRTDBT   EQU   2                   MAJOR DEBITORS                               
SRTWO    EQU   3                   WRITEOFF (SPECFIC PROVISIONS)                
SRTPRV   EQU   4                   PREVIOUS MONTH TOTAL                         
SRTPRO   EQU   5                   PRIOR MONTHS TOTALS                          
SRTCAT   DS    CL1                 CATEGORY                                     
SRTACC   DS    CL6                 ACCOUNT                                      
SRTKLNQ  EQU   *-SRTD                                                           
SRTNME   DS    CL36                ACCOUNT/CATEGORY NAME                        
SRTBK    DS    0PL8                                                             
SRTTOT   DS    PL8                 TOTAL                                        
SRTAGE   DS    6PL8                AGED BUCKETS                                 
SRTRNUM  EQU   (*-SRTBK)/8                                                      
SRTDR3   DS    PL8                 90 DAYS DEBITS                               
SRTAR3   DS    PL8                 3 MONTHS RECEIVABLE BALANCE                  
SRTNUM   EQU   (*-SRTBK)/8                                                      
SRTLNQ   EQU   *-SRTD                                                           
         EJECT                                                                  
*              DSECT FOR THE BINSRCH LIST                                       
*                                                                               
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
         EJECT                                                                  
*                                                                               
*ACGENBOTH                                                                      
*ACREPWORKD                                                                     
*ACGENMODES                                                                     
*ACBIGPRNTD                                                                     
*ACMASTD                                                                        
*DDBIGBOX                                                                       
*DDREPMASTD                                                                     
*DDREPXTRAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACREPWP02 03/23/15'                                      
         END                                                                    
