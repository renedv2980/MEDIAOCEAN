*          DATA SET ACMRK42    AT LEVEL 103 AS OF 06/12/18                      
*PHASE T61642B                                                                  
ACMRK42 TITLE 'ROOT ROUTINES - 3'                                               
* ********************************************************************          
*                                                                    *          
* NMAL 103 12JUN18  REFRENCE NUMBER ISSUE                   SPEC-9193*          
* ********************************************************************          
* JFOX 077 OVERLAY CREATED FROM "ROUV" NMOD IN ROOT                             
* JFOX 078 SET AOCAEL.  HANDLE MULTIPLE AFCELS                                  
* JFOX 079 CALCULATE SECONDARY CURRENCY EXCHANGE DIFFERENCE                     
* DCUR 080 VALIDATE CHECK NUMBER FOR VALID NUMERIC.                             
*          ALLOW ONLY 13 CHARACTERS FOR CONTRA ACCOUNT ON HEADER SCR.           
* JFOX 081 FIX BUG CALLING BLDCUR                                               
* JFOX 082 VALIDATE USED DATE PERIOD ON HEADER SCREEN                           
* JFOX 083 US/UK COMPATIBILITY                                                  
* JFOX 084 US - ALLOW ALPHANUMERIC CHEQUE NUMBER (SEE DCUR LVL 80)              
* JFOX 085 MAY CLEAR ZOOM SCREEN EXCHANGE RATE TEXT FIELDS                      
* DCUR 086 ADDED VALHXNM ROUTINE TO VALCHQ TO CHECK FOR VALID HEX AND           
*          NUMERIC FOR THE CHECK NUMBER                                         
* JFOX 087 ...AND BETTER ERROR MESSAGE IF CHEQUE NUMBER IS INVALID              
ACMRK42  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MK42**,RA,R9,R8                                              
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         USING WORKD,RC                                                         
         USING TWAD,R6                                                          
         USING SAVED,R7                                                         
         L     R5,AIOCNTL                                                       
         USING IOCNTLD,R5                                                       
         SRL   RF,32-8             BRANCH INDEX HELD IN HOB RF                  
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     BLDBAL              BUILD ACCOUNT NAME AND BALANCE               
         B     EDTRAT              EDIT EXCHANGE RATE                           
*&&UK*&& B     EXCHDF              EXCHANGE DIFFERENCE                          
*&&US*&& DC    AL4(0)                                                           
         B     FLDVAL              GENERAL FIELD VALIDATION                     
         B     SETFORE             SET FOREIGN CURRENCY FROM CURRTAB            
         B     SETELAD             SET ELEMENT ADDRESSES                        
         B     VAL1FC              VALIDATE 1 FOREIGN CURRENCY                  
         B     VALACC              VALIDATE ACCOUNT                             
         B     VALADA              VALIDATE ACTIVITY DATE PERIOD                
         B     VALAMT              VALIDATE AMOUNT                              
         B     VALBMO              VALIDATE BATCH MONTH                         
         B     VALBNA              VALIDATE BATCH NAME                          
         B     VALBNK              VALIDATE BANK ACCOUNT                        
         B     VALBRF              VALIDATE BATCH REFERENCE                     
         B     VALCDT              VALIDATE CHEQUE DATE                         
         B     VALCHQ              VALIDATE CHEQUE NUMBER                       
         B     VALCHT              VALIDATE CHEQUE TYPE                         
         B     VALCLI              VALIDATE CLIENT                              
         B     VALCOB              VALIDATE CONTRA A/C IN BANK U/L              
         B     VALCON              VALIDATE CONTRA ACCOUNT                      
         B     VALCUR              VALIDATE CURRENCY FILTER                     
         B     VALDAT              VALIDATE SINGLE DATE                         
         B     VALICL              VALIDATE INCLUDE                             
         B     VALIRC              VALIDATE INCLUDE RECEIPTS                    
         B     VALITE              VALIDATE BATCH ITEM COUNT                    
         B     VALJOB              VALIDATE JOB                                 
         B     VALLDG              VALIDATE UNIT & LEDGER (VENDOR)              
         B     VALMOS              VALIDATE MONTH OF SERVICE RANGE              
         B     VALMRK              VALIDATE MARK FIELD                          
         B     VALOFF              VALIDATE OFFICE                              
         B     VALPER              VALIDATE TRANSACTION DATE PERIOD             
         B     VALPRO              VALIDATE PRODUCT                             
         B     VALREF              VALIDATE REFERENCE RANGE                     
         B     VALSRC              VALIDATE SOURCE ACCOUNT (CPJEL)              
         B     VALSUP              VALIDATE SUPPLIER                            
         B     VALWRK              VALIDATE WORKCODE                            
         B     VALZMRK             VALIDATE ZOOM MARK                           
         B     STATOUT             BUILD A STATUS LINE                          
         B     ZOOMFAC             ZOOM FACILITY                                
*&&UK*&& DC    AL4(0)                                                           
*&&US*&& B     VALPUB              VALIDATE SPECIAL PUBLICATION NUMBER          
         B     VALGAC              VALIDATE GENERAL ACCOUNT                     
         B     TSTEUR              TEST EURO MEMBER/EURO                        
         B     VALUDA              VALIDATE USED DATE PERIOD                    
                                                                                
ROUVL    MVI   DUB,0               SET CC LOW                                   
         B     ROUVCC                                                           
ROUVH    MVI   DUB,2               SET CC HIGH                                  
         B     ROUVCC                                                           
ROUVE    MVI   DUB,1               SET CC EQUAL                                 
ROUVCC   CLI   DUB,1                                                            
                                                                                
ROUVX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD ACCOUNT NAME AND BALANCE ROUTINE                              *         
***********************************************************************         
                                                                                
BLDBAL   LR    R3,R1               R3=A(ACCOUNT FIELD HEADER)                   
         MVC   L'FVIHDR(L'ACTKACT,R1),ACCOUNT+(ACTKACT-ACTKEY)                  
         LA    R1,L'FVIHDR+L'ACTKACT-1(R1)                                      
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'ACCNAME,R1),ACCNAME                                          
         CLC   FACTOR,SPACES       TEST FACTORING COMPANY PRESENT               
         BNH   BLDBAL02                                                         
         LA    R1,2+L'ACCNAME-1(R1)                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   2(R1),C'('                                                       
         MVC   3(L'AC@FACC,R1),AC@FACC                                          
         LA    R1,3+L'AC@FACC-1(R1)                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'='                                                       
         MVC   2(L'FACTOR,R1),FACTOR                                            
         LA    R1,2+L'FACTOR-1(R1)                                              
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C')'                                                       
                                                                                
BLDBAL02 MVC   TEMP,SPACES         BUILD (BAL=NNNNN.NN) EXPRESSION              
         MVI   TEMP,C'('                                                        
         MVC   TEMP+1(L'OP3BAL),OP3BAL                                          
         MVI   TEMP+1+L'OP3BAL,C'='                                             
         LA    R2,TEMP+2+L'OP3BAL  R2=A(OUTPUT) FOR CUREDIT                     
         CURED ACCBAL,(17,(R2)),2,ALIGN=LEFT,FLOAT=-                            
         AR    R2,R0               ADD SIGNIFICANT L'AMOUNT                     
         MVI   0(R2),C')'                                                       
*&&UK                                                                           
         OC    RCNBAL,RCNBAL       TEST RECONCILED BALANCE                      
         BZ    BLDBAL04                                                         
         MVI   0(R2),C' '                                                       
         MVC   1(L'OP3REC,R2),OP3REC                                            
         MVI   1+L'OP3REC(R2),C'='                                              
         LA    R2,2+L'OP3REC(R2)                                                
         CURED RCNBAL,(17,(R2)),2,ALIGN=LEFT,FLOAT=-                            
         AR    R2,R0               ADD SIGNIFICANT L'AMOUNT                     
         MVI   0(R2),C')'                                                       
*&&                                                                             
BLDBAL04 LA    RF,TEMP                                                          
         SR    R2,RF               R2=L'EXPRESSION-1                            
         XR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R3)                                             
         BCTR  RF,0                                                             
         TM    FVATRB-FVIHDR(R3),FVAXTND                                        
         BNO   *+8                                                              
         SH    RF,=Y(L'FVIHDR)                                                  
         AR    RF,R3               RF=A(LAST BYTE OF FIELD)                     
         SR    RF,R2               GO BACK N SPACES                             
         BCTR  RF,0                BACK ONE CHARACTER                           
         LA    R2,1(R2)            ADD ONE TO EXECUTE LENGTH                    
         LA    R1,BNHQ                                                          
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         LA    R1,BHQ                                                           
BLDBAL06 CLI   0(RF),C' '                                                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         NOP   BLDBAL08            BNH/BH                                       
         MVI   0(RF),C' '          MOVE IN A SPACE                              
         LA    R2,1(R2)            ADD ONE TO EXECUTE LENGTH                    
         BCT   RF,BLDBAL06         BACK ONE CHARACTER                           
         DC    H'0'                                                             
                                                                                
BLDBAL08 BNH   *+10                TEST POINTING TO END OF LAST WORD            
         LA    RF,1(RF)            YES - FORWARD ONE CHARACTER                  
         BCTR  R2,0                AND SUBTRACT ONE FROM EXECUTE LENGTH         
         BCTR  R2,0                SUBTRACT ONE FROM EXECUTE LENGTH             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),TEMP                                                     
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
BLDBALX  B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* EDIT EXCHANGE RATE INTO TWA FIELD                                   *         
* NTR1 P1 -> A(PL5 RATE)   P2 -> (L'OUTPUT,A(OUTPUT))                 *         
***********************************************************************         
                                                                                
EDTRAT   L     RF,0(R1)                                                         
         XR    R2,R2                                                            
         IC    R2,4(R1)                                                         
         L     R3,4(R1)                                                         
         ZAP   DUB2,PZERO                                                       
         MVO   DUB2,0(5,RF)                                                     
         CURED (P8,DUB2),((R2),(R3)),5,ALIGN=LEFT                               
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                                                             
EDTRAT02 CLI   0(R1),C','                                                       
         BE    *+8                                                              
         CLI   0(R1),C'.'                                                       
         BNE   *+12                                                             
         MVI   1(R1),C'0'                                                       
         B     EDTRATX                                                          
         CLI   0(R1),C'0'                                                       
         BH    EDTRATX                                                          
         MVI   0(R1),C' '                                                       
         BCT   R1,EDTRAT02                                                      
EDTRATX  B     ROUVE                                                            
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* CALCULATE EXCHANGE DIFFERENCE                                       *         
***********************************************************************         
                                                                                
EXCHDF   ZAP   TMPEXDF,PZERO       EXCHANGE DIFF                                
         ZAP   TMPEXDFS,PZERO      EXCHANGE DIFF (2ND CURRENCY)                 
         ZAP   PKWK16B,PZERO       EXCHANGE DIFF                                
         CP    TSARAFCA,PZERO      TEST AFC                                     
         BE    EXCHDFH                                                          
         CP    CHQAMC,PZERO        TEST BATCH                                   
         BE    EXCHDFH                                                          
                                                                                
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY ACTIVE               
         BZ    EXCHDF04                                                         
         ZAP   PKWK16A,CHQAMS      DETERMINE BATCH RATE (2ND)                   
         SRP   PKWK16A,RNDING,0    CHEQUE AMOUNT (2ND) * 10'RNDING              
         ZAP   DUB1,CHQAMC                                                      
         DP    PKWK16A,DUB1                                                     
         ZAP   PKWK16A,PKWK16A(L'PKWK16A-L'DUB1)                                
                                                                                
         ZAP   DUB1,TSARAFCA                                                    
         TM    TSARIND2,TSARGRSS                                                
         BNZ   *+10                                                             
         SP    DUB1,TSARFDIC                                                    
         MP    PKWK16A,DUB1        VALUE AT BATCH RATE (2ND)                    
                                                                                
         ZAP   PKWK16B,TSARSCUA    DETERMINE ACCURATE INVOICE RATE              
         SRP   PKWK16B,RNDING,0    TSAR AMOUNT (2ND) * 10'RNDING                
         ZAP   DUB1,TSARAFCA                                                    
         DP    PKWK16B,DUB1        CALCULATE INVOICE EXCHANGE RATE              
         ZAP   PKWK16B,PKWK16B(L'PKWK16B-L'DUB1)                                
                                                                                
         ZAP   DUB1,TSARAFCA                                                    
         TM    TSARIND2,TSARGRSS                                                
         BNZ   *+10                                                             
         SP    DUB1,TSARFDIC                                                    
         MP    PKWK16B,DUB1        VALUE AT INVOICE RATE (2ND)                  
                                                                                
         SP    PKWK16A,PKWK16B     ACCURATE EXCHANGE DIFFERENCE                 
                                                                                
         ZAP   PKWK16B,PKWK16A                                                  
         SRP   PKWK16A,64-RNDING,5                                              
         ZAP   TMPEXDFS,PKWK16A    ROUNDED EXCHANGE DIFFERENCE (2ND)            
         SRP   PKWK16A,RNDING,0                                                 
         SP    PKWK16B,PKWK16A     TEST ROUND UP OR DOWN                        
         BM    EXCHDF02                                                         
                                                                                
         CP    SAVROUNS,PONE       TEST ROUND UP BY ONE                         
         BL    EXCHDF04                                                         
         AP    TMPEXDFS,PONE                                                    
         SP    SAVROUNS,PONE                                                    
         B     EXCHDF04                                                         
                                                                                
EXCHDF02 CP    SAVROUNS,PONENEG    TEST ROUND DOWN BY ONE                       
         BH    EXCHDF04                                                         
         SP    TMPEXDFS,PONE                                                    
         AP    SAVROUNS,PONE                                                    
                                                                                
EXCHDF04 ZAP   PKWK16A,CHQAMT      DETERMINE BATCH RATE                         
         SRP   PKWK16A,RNDING,0    CHEQUE AMOUNT * 10'RNDING                    
         ZAP   DUB1,CHQAMC                                                      
         BZ    EXCHDFH                                                          
         DP    PKWK16A,DUB1                                                     
         ZAP   PKWK16A,PKWK16A(L'PKWK16A-L'DUB1)                                
                                                                                
         ZAP   DUB1,TSARAFCA                                                    
         TM    TSARIND2,TSARGRSS                                                
         BNZ   *+10                                                             
         SP    DUB1,TSARFDIC                                                    
         MP    PKWK16A,DUB1        VALUE AT BATCH RATE                          
                                                                                
         ZAP   PKWK16B,TSARAMNT    DETERMINE ACCURATE INVOICE RATE              
         SRP   PKWK16B,RNDING,0    TSAR AMOUNT * 10'RNDING                      
         ZAP   DUB1,TSARAFCA                                                    
         BZ    EXCHDF                                                           
         DP    PKWK16B,DUB1        CALCULATE INVOICE EXCHANGE RATE              
         ZAP   PKWK16B,PKWK16B(L'PKWK16B-L'DUB1)                                
                                                                                
         ZAP   DUB1,TSARAFCA                                                    
         TM    TSARIND2,TSARGRSS                                                
         BNZ   *+10                                                             
         SP    DUB1,TSARFDIC                                                    
         MP    PKWK16B,DUB1        VALUE AT INVOICE RATE                        
                                                                                
         SP    PKWK16A,PKWK16B     ACCURATE EXCHANGE DIFFERENCE                 
                                                                                
         ZAP   PKWK16B,PKWK16A                                                  
         SRP   PKWK16A,64-RNDING,5                                              
         ZAP   TMPEXDF,PKWK16A     ROUNDED EXCHANGE DIFFERENCE                  
         SRP   PKWK16A,RNDING,0                                                 
         SP    PKWK16B,PKWK16A     TEST ROUND UP OR DOWN                        
         BM    EXCHDF06                                                         
                                                                                
         CP    SAVROUND,PONE       TEST ROUND UP BY ONE                         
         BL    EXCHDF08                                                         
         AP    TMPEXDF,PONE                                                     
         SP    SAVROUND,PONE                                                    
         B     EXCHDF08                                                         
                                                                                
EXCHDF06 CP    SAVROUND,PONENEG    TEST ROUND DOWN BY ONE                       
         BH    EXCHDF08                                                         
         SP    TMPEXDF,PONE                                                     
         AP    SAVROUND,PONE                                                    
                                                                                
EXCHDF08 CP    TMPEXDF,PZERO                                                    
         BNE   EXCHDFX                                                          
         CP    TMPEXDFS,PZERO      TEST SECONDARY CURRENCY NON-ZERO             
         BNE   EXCHDFX             ALSO TRIGGER EXCHANGE DIFFERENCES            
EXCHDFH  B     ROUVH                                                            
EXCHDFX  B     ROUVE                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* STANDARD FIELD VALIDATION ROUTINE                                   *         
* NTRY - R1=A(FIELD HEADER)                                           *         
*        FVMINL=MINIMUM FIELD LENGTH OR 0 IF NOT REQUIRED             *         
*        FVMAXL=MAXIMUM FIELD LENGTH OR 0                             *         
*        FVUNLD=SEARCH TRIGGER AS FOLLOWS:                            *         
*               X'0000' DON'T CALL SEARCH FOR THIS FIELD              *         
*               C'UL'   SEARCH USING THIS UNIT/LEDGER                 *         
*               X'FFFF' SEARCH USING USER'S UNIT/LEDGER               *         
*        FVPREF=IF FVUNLD SET, PREFIX CHARACTER FOR OVERRIDE OR 0     *         
***********************************************************************         
                                                                                
FLDVAL   ST    R1,FVADDR           SET A(INPUT FIELD HEADER)                    
         OC    FVUNLD,FVUNLD                                                    
         BZ    FLDVAL02            X'0000' DON'T SEARCH FOR THIS FIELD          
         XR    RF,RF               PRESUME UNIT/LEDGER UNKNOWN                  
         CLC   FVUNLD,=X'FFFF'                                                  
         BE    *+12                X'FFFF' UNIT/LEDGER UNKNOWN                  
         LA    RF,FVUNLD           C'UL' FIXED UNIT/LEDGER                      
         ICM   RF,8,FVPREF         SET PREFIX CHARACTER FOR OVERRIDE            
                                                                                
         GOTO1 VACSRCHC,DMCB,(4,FVADDR),TWAD,(RF),ACOM,(FVMAMI,SRCHACC)         
         XC    SRCHACC,SRCHACC                                                  
         XC    FVUNLD,FVUNLD       CLEAR SEARCH TRIGGER                         
         XC    FVPREF,FVPREF       CLEAR PREFIX CHARACTER FOR OVERRIDE          
         XC    FVMAMI,FVMAMI       CLEAR MAXIMUM/MINIMUM LENGTH                 
         L     R1,FVADDR           RESTORE R1=A(FIELD HEADER)                   
                                                                                
FLDVAL02 MVC   FVIHDR,0(R1)        EXTRACT FIELD HEADER                         
         MVI   FVINDX,0            RESET INDEX & SUB-INDEX VALUES               
         MVI   FVSUBX,0                                                         
         MVC   FVIFLD,SPACES                                                    
         XR    RF,RF                                                            
         IC    RF,FVTLEN                                                        
         LA    R0,L'FVIHDR+1                                                    
         TM    FVATRB,FVAXTND                                                   
         BZ    *+8                                                              
         LA    R0,L'FVIHDR+L'FVIHDR+1                                           
         SR    RF,R0               RF=MAXIMUM INPUT LENGTH-1                    
         BNM   *+6                                                              
         DC    H'0'                THIS IS A BAD TWA FIELD                      
         EX    RF,*+8              EXTRACT FIELD DATA                           
         B     *+10                                                             
         MVC   FVIFLD(0),L'FVIHDR(R1)                                           
                                                                                
         LA    R1,FVIFLD(RF)       R1=A(END OF INPUT FIELD)                     
         LA    RF,1(RF)            RF=LOOP COUNT                                
FLDVAL04 CLI   0(R1),C' '          LOCATE LAST INPUT CHARACTER IN FIELD         
         BH    FLDVAL06                                                         
         MVI   0(R1),C' '          SET FUNNIES TO SPACES                        
         BCTR  R1,0                                                             
         BCT   RF,FLDVAL04                                                      
FLDVAL06 STC   RF,FVILEN           SET ACTUAL INPUT LENGTH                      
         MVC   FVMSGNO,=AL2(EGIFSHRT) ENSURE NOT TOO SHORT OR LONG              
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         CLM   RF,1,FVMINL                                                      
         BL    FLDVALX                                                          
         CLI   FVMAXL,0            IF FVMAXL=ZERO DON'T TEST LONG               
         BE    *+18                                                             
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         CLM   RF,1,FVMAXL                                                      
         BH    FLDVALX                                                          
         NI    FVIIND,255-FVINUM-FVIALF-FVIHEX                                  
         LTR   RF,RF               EXIT IF NO INPUT IN FIELD                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(IGOK)                                               
         B     FLDVAL16                                                         
*                                  SET FIELD VALIDITY BITS                      
         MVC   FVMSGNO,=AL2(IGOK) INDICATE FIELD IS OK                          
         OI    FVIIND,FVINUM+FVIALF+FVIHEX                                      
FLDVAL08 TM    FVIIND,FVINUM+FVIALF+FVIHEX                                      
         BZ    FLDVAL12                                                         
         CLI   0(R1),C'A'                                                       
         BNL   *+12                                                             
         NI    FVIIND,255-FVINUM-FVIALF-FVIHEX                                  
         B     FLDVAL10                                                         
         CLI   0(R1),C'Z'                                                       
         BNH   *+12                                                             
         NI    FVIIND,255-FVIALF                                                
         B     FLDVAL10                                                         
         NI    FVIIND,255-FVINUM                                                
         CLI   0(R1),C'F'                                                       
         BNH   *+8                                                              
         NI    FVIIND,255-FVIHEX                                                
FLDVAL10 BCTR  R1,0                                                             
         BCT   RF,FLDVAL08                                                      
FLDVAL12 IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         STC   RF,FVXLEN           SET EXECUTE LENGTH (INPUT LENGTH-1)          
         CLI   FVNUMER,1           TREAT AS NUMERIC IF FOUND AS NUMERIC         
         BNE   FLDVAL14                                                         
         TM    FVIIND,FVINUM                                                    
         BZ    FLDVAL14                                                         
         EX    RF,*+8              SET PACKED/BINARY NUMERIC VALUES             
         B     *+10                                                             
         PACK  DUB,FVIFLD(0)                                                    
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
FLDVAL14 MVC   FVMSGNO,=AL2(IGOK)                                               
                                                                                
FLDVAL16 MVI   FVHELP,0            RESET THIS TIME VALUES                       
         MVI   FVMINL,0                                                         
         MVI   FVMAXL,0                                                         
         MVI   FVNUMER,0                                                        
         MVC   FVXTRA,SPACES                                                    
         B     FLDVALX                                                          
*                                  HANDLE ERRORS HERE                           
FLDVALX  CLC   FVMSGNO,=AL2(EGIFMISS)                                           
         BE    FLDVALX2                                                         
         MVI   FVFLAG,0                                                         
         CLI   FVILEN,0                                                         
         BE    FLDVALXX                                                         
         MVI   FVFLAG,1                                                         
         CLC   FVMSGNO,=AL2(IGOK)                                               
         BE    FLDVALXX                                                         
FLDVALX2 MVI   FVFLAG,2                                                         
FLDVALXX CLI   FVFLAG,1            SET CONDITION CODE FOR CALLER                
         MVI   FVFLAG,0                                                         
         B     ROUVX               RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* SET FOREIGN CURRENCY                                                *         
***********************************************************************         
                                                                                
*&&UK                                                                           
SETFORE  TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    ROUVE                   CURRENCY CODE PROBLEM                    
         L     R2,ACURRTAB                                                      
         USING CURTABD,R2                                                       
         LA    R0,CURRTABN                                                      
         OC    FORECURT(L'CURTCUR),FORECURT  DEFAULT TO 1ST ENTRY               
         BNZ   *+10                                                             
         MVC   FORECURT(L'CURTCUR),CURTCUR                                      
SETF002  CLI   CURTCUR,EOT         SEARCH FOR FOREIGN CURRENCY ENTRY            
         BE    SETFOREX                                                         
         CLC   CURTCUR,FORECURT    TEST CURRENCY ENTRY                          
         BNE   *+10                                                             
         MVC   FORECURT(L'CURRTAB),CURTCUR   EXTRACT DETAILS                    
         LA    R2,L'CURRTAB(R2)                                                 
         BCT   R0,SETF002                                                       
SETFOREX CH    R0,=AL2(CURRTABN-1) TEST IF ONE ENTRY FOUND                      
         BNE   *+16                                                             
         TM    SAFCIND1,SAFCIGBP   AND ALL FOREIGN                              
         BO    *+8                                                              
         OI    SAFCIND1,SAFCI1SC+SAFCI1ON  SINGLE FOREIGN CURRENCY              
         B     ROUVE                                                            
         DROP  R2                                                               
*&&                                                                             
*&&US                                                                           
SETFORE  B     ROUVE                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* SET ADDRESSES OF REQUIRED ELEMENTS                                  *         
* NTR1 -> R1=A(RECORD)                                                *         
***********************************************************************         
                                                                                
SETELAD  L     R2,0(R1)            SAVE A(RECORD)                               
         LA    R0,AELEMS           CLEAR ELEMENT ADDRESSES                      
         LA    R1,AELEMSL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    OLDAFCEL,OLDAFCEL   CLEAR OLD AFCEL                              
         LR    R4,R2               R4=A(RECORD)                                 
         LA    R4,TRNRFST-TRNRECD(R4)                                           
                                                                                
         USING TRNELD,R4                                                        
*MN                                                                             
         CLI   TRNEL,GLPELQ        TEST GENERAL LEDGER POSTING ELEM             
         BNE   SETEL01                                                          
         XR    R0,R0                                                            
         IC    R0,TRNLN                                                         
         AR    R4,R0               NEXT ELEMENT                                 
         CLI   TRNEL,0             TEST LAST ELEMENT                            
         BE    SETELADX                                                         
*MN                                                                             
SETEL01  CLI   TRNEL,NAMELQ        TEST NAME ELEMENT FOR ACCOUNTS               
         BNE   *+12                                                             
         ST    R4,ANAMEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,TRNELQ                                                     
         BNE   SETELADX            TRANSACTION ELEMENT MUST BE FIRST            
         ST    R4,ATRNEL                                                        
                                                                                
SETEL02  XR    R0,R0                                                            
         IC    R0,TRNLN                                                         
         AR    R4,R0               NEXT ELEMENT                                 
         CLI   TRNEL,0             TEST LAST ELEMENT                            
         BNE   *+12                                                             
         ST    R4,ANXTEL           SET A(NEXT ELEMENT)                          
         B     SETEL60                                                          
                                                                                
         CLI   TRNEL,AFCELQ        TEST ACCOUNT FOREIGN CURRENCY                
         BNE   SETEL10                                                          
         TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    SETEL10             CURRENCY CODE PROBLEM                        
         ICM   RE,15,AAFCEL        TEST PREVIOUS AFCEL                          
         BZ    SETEL04                                                          
         USING AFCELD,RE                                                        
         TM    AFCXSTA2,AFCXSMEM   TEST PREVIOUS AFCEL IS MEMO                  
         BO    SETEL04             REPLACE A(PREVIOUS MEMO AFCEL)               
         LA    RE,TRNELD           PREVIOUS AFCEL IS LIVE                       
         TM    AFCXSTA2,AFCXSMEM   TEST IF PRESENT AFCEL IS MEMO                
         BO    SETEL02             LEAVE INTACT A(PREVIOUS LIVE AFCEL)          
         DROP  RE                                                               
SETEL04  ST    R4,AAFCEL           SET/OVERWRITE A(AFCEL)                       
*&&US*&& B     SETEL02                                                          
*&&UK                                                                           
         TM    SAFCIND1,SAFCIEUR   TEST EURO IS SINGLE FOREIGN CURRENCY         
         BZ    SETEL02                                                          
                                                                                
         USING AFCELD,R3                                                        
         L     R3,AAFCEL           R3=A(AFCEL)                                  
         GOTO1 ATSTEUR,AFCCURR                                                  
         BNE   SETEL02             CURRENCY IS NOT A EURO MEMBER                
                                                                                
         MVC   OLDAFCEL,AFCEL      SAVE AFCEL AND CONVERT TO EUROS              
         MVC   WORK(L'AFCCURR),AFCCURR                                          
         MVC   WORK+L'AFCCURR(L'AFCCURR),EURO                                   
         GOTO1 VCASHVAL,DMCB,(X'80',AFCAMNT),(X'24',0),WORK                     
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   AFCAMNT,12(8,R1)    SET EURO AMOUNT                              
         MVC   AFCCURR,EURO        SET CURRENCY CODE                            
         XC    AFCX,AFCX           CLEAR AND BUILD EXCHANGE RULES               
         L     RF,ATRNEL                                                        
         ZAP   DUB1,TRNAMNT-TRNELD(,RF)                                         
         ZAP   DUB2,AFCAMNT                                                     
T        USING CURTABD,TEMP        BUILD TEMPORARY CURTAB ENTRY                 
         MVC   T.CURTABD(CURTABL),FORECURT                                      
         CLC   T.CURTCUR,EURO      TEST SINGLE CURRENCY ESTABLISHED             
         BE    SETEL06                                                          
         MVC   T.CURTABD(CURTABL),PRSTCURT                                      
         CLC   T.CURTCUR,EURO                                                   
         BE    SETEL06                                                          
         MVC   T.CURTCUR,EURO                                                   
         GOTO1 VBLDCUR,DMCB,T.CURTCUR,T.CURTABD,ACOM                            
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
C        USING CURTABD,COMPCURT                                                 
W        USING EURKBLKD,WORK       BUILD EUREKA CONTROL BLOCK                   
SETEL06  XC    W.EURKBLKD(EURKBLKL),W.EURKBLKD                                  
         MVC   W.EURKCUFR,C.CURTCUR  FROM CURRENCY                              
         MVC   W.EURKCUTO,T.CURTCUR  TO CURRENCY                                
         GOTO1 VEUREKA,DMCB,('DERIVEQ',W.EURKBLKD),(C.CURTDECP,DUB1),  X        
               (T.CURTDECP,DUB2),0,0                                            
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AFCX,W.EURKRULE                                                  
         B     SETEL02                                                          
         DROP  R3,C,T,W                                                         
*&&                                                                             
SETEL10  OC    ABNDEL,ABNDEL       TEST FOR FIRST BNDEL                         
         BNZ   *+8                                                              
         CLI   TRNEL,BNDELQ                                                     
         BNE   *+12                                                             
         ST    R4,ABNDEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,CEDELQ        TEST CHEQUE EXTRA DETAIL ELEMENT             
         BNE   *+12                                                             
         ST    R4,ACEDEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,CPJELQ        TEST CLIENT/PRODUCT/JOB ELEMENTS             
         BNE   SETEL16                                                          
         LA    R0,ACPJELN                                                       
         LA    R1,ACPJELS                                                       
SETEL12  OC    0(L'ACPJELS,R1),0(R1)                                            
         BNZ   SETEL14                                                          
         ST    R4,0(R1)            SET A(CPJELS) 1-4                            
         ST    R4,ACPJEL           ALWAYS SET TO LAST ELEMENT                   
         B     SETEL02                                                          
SETEL14  LA    R1,L'ACPJELS(R1)                                                 
         BCT   R0,SETEL12                                                       
         ST    R4,ACPJEL           ALWAYS SET TO LAST ELEMENT                   
         B     SETEL02                                                          
                                                                                
SETEL16  CLI   TRNEL,DUEELQ        TEST DUE-DATE ELEMENTS                       
         BNE   *+12                                                             
         ST    R4,ADUEEL                                                        
         B     SETEL02                                                          
*                                                                               
         CLI   TRNEL,FFTELQ        TEST FREE FORM TEXT ELEMENT                  
         BNE   SETEL18                                                          
         ST    R4,AFFTEL                                                        
         CLI   TRNEL+(FFTTYPE-FFTELD),FFTTACUR                                  
         BNE   *+8                 TEST ASSOCIATED CURRENCY                     
         ST    R4,AFFTACUR         KEEP ADDRESS SEPARATE                        
         CLI   TRNEL+(FFTTYPE-FFTELD),FFTTKREF                                  
         BNE   *+8                 TEST KEY REFERENCE                           
         ST    R4,AFFTKREF         KEEP ADDRESS SEPARATE                        
         CLI   TRNEL+(FFTTYPE-FFTELD),FFTTOREF                                  
         BNE   *+8                 TEST KEY REFERENCE                           
         ST    R4,AFFTCREF         KEEP CR REF NUMBER                           
         CLI   TRNEL+(FFTTYPE-FFTELD),FFTTWRKC                                  
         BNE   *+8                 TEST WORK CODE                               
         ST    R4,AFFTKWRK         KEEP ADDRESS SEPARATE                        
                                                                                
         USING FFTELD,R4                                                        
         CLI   FFTTYPE,FFTTINVN                                                 
         BNE   SETEL02                                                          
         CLI   FFTLN,FFTLN1Q                                                    
         BNH   SETEL02                                                          
         ST    R4,AFFTLEL                                                       
         B     SETEL02                                                          
                                                                                
         USING TRNELD,R4                                                        
SETEL18  CLI   TRNEL,GINELQ        TEST GROUP INVOICE # ELEMENT                 
         BNE   *+12                                                             
         ST    R4,AGINEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,MPYELQ        TEST MANUAL PAYMENT ELEMENT                  
         BNE   SETEL24                                                          
         LA    R0,AMPYELN                                                       
         LA    R1,AMPYELS                                                       
SETEL20  OC    0(L'AMPYELS,R1),0(R1)                                            
         BNZ   SETEL22                                                          
         ST    R4,0(R1)            SET A(MPYELS) 1-4                            
         ST    R4,AMPYEL           LAST MANUAL PAYMENT ELEMENT                  
         MVC   AMPYEL(1),TRNLN     MOVE LENGTH INTO FIRST BYTE                  
         B     SETEL02                                                          
SETEL22  MVI   AMPYEL,FF                                                        
         LA    R1,L'AMPYELS(R1)                                                 
         BCT   R0,SETEL20                                                       
         ST    R4,AMPYEL           LAST MANUAL PAYMENT ELEMENT                  
         B     SETEL02                                                          
                                                                                
SETEL24  CLI   TRNEL,MRHELQ        TEST MEDIA RECONCILE HOLLAND                 
         BNE   *+12                                                             
         ST    R4,AMRHEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,MRXELQ        TEST MEDIA RECONCILE EXTRA DETAIL            
         BNE   *+12                                                             
         ST    R4,AMRXEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,MBIELQ        MEDIA BILL TRANSFER                          
         BNE   *+12                                                             
         ST    R4,AMBIEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,MXCELQ        TEST MEDIA EXTRA CHARGE ELEMENT              
         BNE   SETEL28                                                          
         LA    R0,AMXCELN                                                       
         LA    R1,AMXCELS                                                       
SETEL26  OC    0(L'AMXCELS,R1),0(R1)                                            
         BNZ   *+12                                                             
         ST    R4,0(R1)            SET A(MXCELS) 1-6                            
         B     SETEL02                                                          
         LA    R1,L'AMXCELS(R1)                                                 
         BCT   R0,SETEL26                                                       
         B     SETEL02                                                          
                                                                                
SETEL28  CLI   TRNEL,MXPELQ        TEST MEDLINE EXTRA PAYMENT ELEMENT           
         BNE   *+12                                                             
         ST    R4,AMXPEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,NOTELQ        TEST TRANSACTION NOTE ELEMENTS               
         BNE   SETEL32                                                          
         LA    R0,ANOTELN                                                       
         LA    R1,ANOTELS                                                       
SETEL30  OC    0(L'ANOTELS,R1),0(R1)                                            
         BNZ   *+12                                                             
         ST    R4,0(R1)            SET A(NOTELS) 1-4                            
         B     SETEL02                                                          
         LA    R1,L'ANOTELS(R1)                                                 
         BCT   R0,SETEL30                                                       
         B     SETEL02                                                          
                                                                                
SETEL32  CLI   TRNEL,OTHELQ        TEST OTHERS ELEMENT                          
         BNE   *+12                                                             
         ST    R4,AOTHEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,PTAELQ        TEST PRODUCTION TRANSACTION ACTIVITY         
         BNE   *+12                                                             
         ST    R4,APTAEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,RATEVATQ      TEST RATE OF VAT ELEMENT                     
         BNE   *+12                                                             
         ST    R4,ARAVEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,RATEDSCQ      TEST RATE OF DISCOUNT ELEMENT                
         BNE   *+12                                                             
         ST    R4,ARADEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,RRNELQ        TEST RECORD REVISION ELEMENT                 
         BNE   *+12                                                             
         ST    R4,ARRNEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,SIDELQ        TEST SUPPLIER INVOICE DETAIL ELEMENT         
         BNE   *+12                                                             
         ST    R4,ASIDEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,SCIELQ        TEST SUBSIDIARY CASH INFO ELEMENT            
         BNE   SETEL36                                                          
                                                                                
         LA    R0,ASCIELN                                                       
         LA    R1,ASCIELS                                                       
SETEL34  OC    0(L'ASCIELS,R1),0(R1)                                            
         BNZ   *+12                                                             
         ST    R4,0(R1)            SET A(SCIELS) 1-4                            
         B     *+12                                                             
         LA    R1,L'ASCIELS(R1)                                                 
         BCT   R0,SETEL34                                                       
                                                                                
         CLI   TRNEL+(SCITYPE-SCIELD),SCITCDSC                                  
         BNE   *+12                TEST CASH DISCOUNT TAKEN                     
         ST    R4,ASCICDSC         KEEP ADDRESS SEPARATE                        
         B     SETEL02                                                          
         CLI   TRNEL+(SCITYPE-SCIELD),SCITNOCD                                  
         BNE   *+12                TEST CASH DISCOUNT NOT TAKEN                 
         ST    R4,ASCINOCD         KEEP ADDRESS SEPARATE                        
         B     SETEL02                                                          
         CLI   TRNEL+(SCITYPE-SCIELD),SCITCHQT                                  
         BNE   *+12                TEST CHEQUE TOTAL                            
         ST    R4,ASCICHQT         KEEP ADDRESS SEPARATE                        
         B     SETEL02                                                          
         CLI   TRNEL+(SCITYPE-SCIELD),SCITVERN                                  
         BNE   *+12                                                             
         ST    R4,ASCIVERN         PRESERVE VERSCHIL NUMBER                     
         B     SETEL02                                                          
                                                                                
         B     SETEL02                                                          
                                                                                
SETEL36  CLI   TRNEL,SORELQ        TEST SOURCE ELEMENT                          
         BNE   SETEL38                                                          
         ST    R4,ASOREL                                                        
         CLI   TRNEL+(SORSYS-SORELD),SORSACC                                    
         BNE   SETEL02                                                          
         CLC   PRODUL,TRNEL+(SORAULA-SORELD)                                    
         BNE   SETEL02                                                          
         ST    R4,ASORPROD         SET ACCOUNTING PRODUCTION SOURCE             
         B     SETEL02                                                          
                                                                                
SETEL38  CLI   TRNEL,SPAELQ        TEST SPECIAL POSTING ACCOUNT                 
         BNE   SETEL40                                                          
         ST    R4,ASPAEL                                                        
         CLI   TRNEL+(SPATYPE-SPAELD),SPATCSHD                                  
         BNE   *+8                 TEST CASH DISCOUNT TAKEN                     
         ST    R4,ASPACDSC                                                      
         CLI   TRNEL+(SPATYPE-SPAELD),SPATITAX                                  
         BNE   *+8                 OR INPUT TAX A/C                             
         ST    R4,ASPAITAX         KEEP ADDRESS SEPARATE                        
         CLI   TRNEL+(SPATYPE-SPAELD),SPATCOSG                                  
         BNE   *+8                 OR COSTING ACCOUNT/GROUP                     
         ST    R4,ASPACOSG                                                      
         B     SETEL02                                                          
                                                                                
SETEL40  CLI   TRNEL,TRSELQ        TEST TRANSACTION STATUS ELEMENT              
         BNE   *+12                                                             
         ST    R4,ATRSEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,TRXELQ        TEST TRANSACTION EXTRA STATUS ELEM           
         BNE   *+12                                                             
         ST    R4,ATRXEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,XPYELQ        TEST EXTRA PAYMENT ELEMENT                   
         BNE   *+12                                                             
         ST    R4,AXPYEL                                                        
         B     SETEL02                                                          
                                                                                
         CLI   TRNEL,TRPELQ        TEST TRANSACTION POINTER ELEMENT             
         BNE   SETEL42                                                          
         OC    ATRPEL1,ATRPEL1     TEST A(FIRST TRPEL) SET                      
         BNZ   *+8                                                              
         ST    R4,ATRPEL1          SET A(FIRST TRPEL)                           
         B     SETEL02                                                          
                                                                                
         USING GDAELD,R4                                                        
SETEL42  CLI   GDAEL,GDAELQ        TEST GENERAL DATE ELEMENT                    
         BNE   SETEL44                                                          
         CLI   GDATYPE,GDATERPD    TEST EARLIEST PAYMENT DATE                   
         BNE   SETEL43                                                          
         ST    R4,AGDAERPD                                                      
         B     SETEL02                                                          
SETEL43  CLI   GDATYPE,GDATRECN                                                 
         BNE   SETEL43A                                                         
         ST    R4,AGDAERP2                                                      
         B     SETEL02                                                          
*                                                                               
SETEL43A CLI   GDATYPE,GDAMMOS     IS THIS MEDIA MOS                            
         BNE   SETEL43B                                                         
         ST    R4,AGDAMMOS        SAVE ADDRESS OF MEDIA MOS ELEMENT             
         B     SETEL02                                                          
*                                                                               
SETEL43B CLI   GDATYPE,GDAAPP      APPROVAL DATE?                               
         BNE   *+8                                                              
         ST    R4,AGDAAPPR         SAVE ADDRESS OF APPROVAL DATE ELM            
         B     SETEL02                                                          
*                                                                               
         USING APEELD,R4                                                        
SETEL44  CLI   APEEL,APEELQ        TEST ANALYSIS POINTER ELEMENT                
         BNE   SETEL46                                                          
         ST    R4,AAPEEL                                                        
         B     SETEL02                                                          
                                                                                
         USING OCAELD,R4                                                        
SETEL46  CLI   OCAEL,OCAELQ        TEST OTHER CURRENCY AMOUNTS ELEMENT          
         BNE   SETEL48                                                          
         ST    R4,AOCAEL                                                        
         B     SETEL02                                                          
                                                                                
SETEL48  DS    0H                  NEXT ELEMENT ADDRESS                         
         B     SETEL02                                                          
                                                                                
SETEL60  OC    ASPAITAX,ASPAITAX   TEST INPUT TAX ACCOUNT ELEMENT               
         BNZ   SETELADX                                                         
         ICM   R4,15,AAPEEL        TEST ANALYSIS POINTER ELEMENT                
         BZ    SETELADX                                                         
         USING APEELD,R4                                                        
         SR    R0,R0                                                            
         IC    R0,APENUM                                                        
         LA    R1,APENTRY                                                       
         USING APENTRY,R1                                                       
         SR    R2,R2                                                            
SETEL62  IC    R2,APENLEN                                                       
         CLC   APENACT(L'TAXAUL),TAXAUL                                         
         BE    SETEL64                                                          
         AR    R1,R2                                                            
         BCT   R0,SETEL62                                                       
         B     SETELADX                                                         
SETEL64  LA    RE,WRKSPAEL         BUILD DUMMY SPAEL IN W/S                     
         USING SPAELD,RE                                                        
         XC    SPAELD(SPALNQ),SPAELD                                            
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATITAX                                                 
         MVC   SPAAULA,SPACES                                                   
         SH    R2,=Y(APELN2Q+1)                                                 
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   SPAAULA(0),APENACT                                               
         ST    RE,ASPAITAX         SET A(DUMMY INPUT TAX SPAEL)                 
         OC    ASPAEL,ASPAEL       TEST NO SPAEL FOUND                          
         BNZ   *+8                                                              
         ST    RE,ASPAEL           SET A(DUMMY SPAEL),TOO                       
                                                                                
         USING TRNELD,R4                                                        
SETELADX B     ROUVE                                                            
         DROP  R1,R4,RE                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACCOUNT                                                    *         
***********************************************************************         
                                                                                
VALACC   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    ACCOUNT,ACCOUNT                                                  
         XC    ACCINDS,ACCINDS                                                  
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING ACTRECD,R2          R2=A(ACCOUNT KEY)                            
         MVC   ACTKCPY,COMPANY                                                  
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKUNT(0),FVIFLD                                                
         MVI   GETIND,GETIABLQ     ACCOUNT BALANCE ELEMENT REQUIRED             
         TM    XACTINDS,ACTIPOST   TEST ACTION POSTS                            
         BZ    *+8                                                              
         OI    GETIND,GETINLOK     GIVE ERROR IF ACCOUNT IS LOCKED              
         GOTO1 AGETACC,0                                                        
         BNE   ROUVH                                                            
         MVC   ACCOUNT,ACTKCULA                                                 
         GOTO1 APROFILE            EXTRACT TYPE/ACTION PROFILE                  
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE GENERAL ACCOUNT WITHOUT UPADATING STORAGE                  *         
***********************************************************************         
                                                                                
VALGAC   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         MVC   FVUNLD,LEDGER                                                    
         MVI   FVPREF,C'*'                                                      
         MVI   FVMAMI,0                                                         
         XC    SRCHACC,SRCHACC                                                  
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING ACTRECD,R2          R2=A(ACCOUNT KEY)                            
         MVC   ACTKCPY,COMPANY                                                  
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKUNT(0),FVIFLD                                                
         MVI   GETIND,GETIABLQ     ACCOUNT BALANCE ELEMENT REQUIRED             
         TM    XACTINDS,ACTIPOST   TEST ACTION POSTS                            
         BZ    *+8                                                              
         OI    GETIND,GETINLOK     GIVE ERROR IF ACCOUNT IS LOCKED              
         GOTO1 AGETACC,0                                                        
         BNE   ROUVH                                                            
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ONE FOREIGN CURRENCY                                       *         
***********************************************************************         
                                                                                
*&&UK                                                                           
VAL1FC   ZAP   TSARAFCA,PZERO                                                   
         TM    TSARIND2,TSARDIFP   TEST FOR EXCHANGE DIFFERENCE POSTING         
         BZ    VAL1FC02                                                         
         USING FFTELD,R2                                                        
         ICM   R2,15,AFFTACUR      R2=A(FREE FORM TEXT ELEMENT - CUR)           
         BZ    VAL1FC02                                                         
         MVC   TSARAFCC,FFTDATA    USE ASSOCIATED CURRENCY                      
         B     VAL1FC04                                                         
         USING AFCELD,R2                                                        
VAL1FC02 ICM   R2,15,AAFCEL        R2=A(ACC.FOREIGN CURRENCY ELEMENT)           
         BNZ   *+18                                                             
         MVC   TSARAFCX,ONERATE                                                 
         OI    SAFCIND1,SAFCIGBP                                                
         B     VAL1FCX                                                          
         ZAP   TSARAFCA,AFCAMNT    EXTRACT AMOUNT                               
         MVC   TSARAFCC,AFCCURR    EXTRACT CURRENCY CODE                        
         MVC   TSARAFCX,AFCX       EXTRACT EXCHANGE RATE RULE                   
VAL1FC04 GOTO1 ABLDCURT,DMCB,(X'00',TSARAFCC)   CREATE CURRENCY CODE            
VAL1FCX  B     ROUVE                                                            
         DROP  R2                                                               
*&&                                                                             
*&&US                                                                           
VAL1FC   B     ROUVE                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTIVITY DATE FILTERS                                      *         
* NOTE - L'PERIOD FIELD MUST EQUAL L'PVALCPER                         *         
***********************************************************************         
                                                                                
VALADA   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    ADASTA,ADASTA                                                    
         MVI   ADAEND,X'FF'                                                     
         MVC   ADAEND+1(L'ADAEND-1),ADAEND                                      
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(AGYLANG,WORK)                      
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         TM    4(R1),X'03'         START AND/OR END INVALID?                    
         BNZ   ROUVH                                                            
         BAS   RE,CLRFLD           CLEAR PERIOD FIELD                           
         LA    R2,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R2                                                       
         TM    PVALASSM,STARTASS   START ENTIRELY ASSUMED?                      
         BO    VALADA4             YES - JUST TAKE PERVAL END DATE              
         MVC   ADASTA,PVALCSTA     ACTIVITY START DATE FOR FILTERING            
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RF,FVIFLD(RF)                                                    
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VALADA2             DON'T SET END DATE                           
         MVC   ADAEND,PVALCEND     ACTIVITY END DATE FOR FILTERING              
         LA    RF,PVALCPER                                                      
         LA    R1,L'PVALCPER-1                                                  
         B     VALADAX             SHOW DDMMMYY-DDMMMYY                         
                                                                                
VALADA2  LA    RF,PVALCPER         TAKE PERVAL START DATE                       
         LA    R1,L'PVALCPER-1(RF)                                              
         CLI   0(R1),C'-'                                                       
         BE    *+10                                                             
         BCT   R1,*-8                                                           
         DC    H'0'                BAD OUTPUT FROM PERVAL                       
         SR    R1,RF                                                            
         B     VALADAX             SHOW DDMMMYY-                                
                                                                                
VALADA4  MVC   ADAEND,PVALCEND     ACTIVITY END DATE FOR FILTERING              
         LA    RF,PVALCPER+(L'PVALCPER-1)                                       
         LR    R1,RF                                                            
         CLI   0(RF),C'-'                                                       
         BE    *+10                                                             
         BCT   RF,*-8                                                           
         DC    H'0'                BAD OUTPUT FROM PERVAL                       
         SR    R1,RF               SHOW -DDMMMYY                                
                                                                                
VALADAX  L     RE,FVADDR           DISPLAY THE PERIOD INTERPRETED               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,RE),0(RF)                                             
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A CASH AMOUNT                                   *         
* NTRY - R1=A(PARAMETER LIST) AS FOLLOWS:-                            *         
*        P1=A(INPUT FIELD HEADER)                                     *         
*        P2=A(6 BYTE PACKED OUTPUT VALUE)                             *         
* EXIT - CC=LOW IF FIELD NOT INPUT                                    *         
*        CC=EQUAL IF FIELD INPUT AND CORRECT                          *         
*        CC=HIGH IF FIELD INPUT AND INVALID (WITH FVMSGNO SET)        *         
***********************************************************************         
                                                                                
VALAMT   LR    R2,R1               R2=A(PARAMETER LIST)                         
         L     R1,4(R2)                                                         
         ZAP   0(6,R1),PZERO       CLEAR OUTPUT VALUE                           
         ICM   R1,15,0(R2)                                                      
         BZ    VALAMT2                                                          
         GOTO1 AFLDVAL             VALIDATE THE INPUT                           
         BNE   ROUVX                                                            
VALAMT2  MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         XR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         GOTO1 VCASHVAL,PARM,FVIFLD,(R0)                                        
         CLI   0(R1),0                                                          
         BNE   ROUVH                                                            
         L     RF,4(R1)                                                         
         CVD   RF,DUB                                                           
         OC    DUB(2),DUB          ENSURE WILL PACK INTO 6 BYTES                
         BNZ   ROUVH                                                            
         L     R1,4(R2)                                                         
         ZAP   0(6,R1),DUB         RETURN PACKED NUMBER TO CALLER               
         BAS   RE,CLRFLD                                                        
         LA    R1,1(R1)            AFTER CLRFLD R1=FIELD X'LENGTH               
         LR    R0,R1                                                            
         LA    R2,L'FVIHDR(RF)                                                  
         CURED DUB,((R0),(R2)),2,ALIGN=LEFT,FLOAT=-                             
         B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH MONTH                                                *         
***********************************************************************         
                                                                                
VALBMO   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    BATMON,BATMON                                                    
         XC    BATMONP,BATMONP                                                  
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         LA    R2,WORK                                                          
         USING BMONVALD,R2                                                      
         GOTO1 VBMONVAL,DMCB,(0,FVIHDR),(XACTBTYP,ACOM),               X        
               (AGYLANG,BMONVALD),(COMPANY,0)                                   
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    VALBMO2                                                          
         MVC   FVMSGNO,BMOMSG                                                   
         TM    BMOERR,BMOELOKQ+BMOERNGQ  TEST LOCKED/OUT OF RANGE               
         BZ    ROUVH                                                            
VALBMO2  MVC   BATMONP,BMOMOSP                                                  
         MVC   BATMON,BMOMOSC                                                   
         MVI   BMOMOSP+L'BMOMOSP,X'01'  GRAFT DAY TO END OF PWOS YM             
         BAS   RE,CLRFLD                                                        
         LA    RF,L'FVIHDR(RF)                                                  
         GOTO1 VDATCON,DMCB,(1,BMOMOSP),(9,(RF))                                
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BNE   ROUVH                                                            
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH NAME                                                           
***********************************************************************         
                                                                                
VALBNA   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    BATNAM,BATNAM                                                    
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         MVC   BATNAM,FVIFLD                                                    
VALBNAX  B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE BANK ACCOUNT                                               *         
***********************************************************************         
                                                                                
VALBNK   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    ACCOUNT,ACCOUNT                                                  
         XC    ACCINDS,ACCINDS                                                  
         MVC   FVUNLD,BANKUL                                                    
         MVI   FVPREF,C'*'                                                      
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING ACTRECD,R2          R2=A(ACCOUNT KEY)                            
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'BANKUL),BANKUL                                         
         LA    R1,ACTKACT                                                       
         LA    RE,FVIFLD                                                        
         IC    RF,FVXLEN                                                        
         CLI   FVIFLD,C'*'         TEST NON-STANDARD BANK U/L                   
         BNE   VALBNK2                                                          
         LA    R1,ACTKUNT          TAKE U/L FROM SCREEN, TOO                    
         LA    RE,1(RE)            BUMP BEYOND ASTERISK                         
         BCTR  RF,0                DROP ONE FROM EXECUTE LENGTH                 
                                                                                
VALBNK2  EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)                                                    
         MVI   GETIND,GETIABLQ     ACCOUNT BALANCE ELEMENT REQUIRED             
         TM    XACTINDS,ACTIPOST   TEST ACTION POSTS                            
         BZ    VALBNK4                                                          
*&&US                                                                           
         CLI   XACTION,ACTVOID     SKIP LOCK TEST FOR BANK/VOID                 
         BE    VALBNK4                                                          
*&&                                                                             
         OI    GETIND,GETINLOK     GIVE ERROR IF ACCOUNT IS LOCKED              
VALBNK4  GOTO1 AGETACC,0                                                        
         BNE   ROUVH                                                            
         MVC   ACCOUNT,ACTKCULA                                                 
         GOTO1 APROFILE            EXTRACT TYPE/ACTION PROFILE                  
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH REFERENCE                                            *         
***********************************************************************         
                                                                                
VALBRF   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    BATREF,BATREF                                                    
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         XR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
         CLI   0(R1),C'A'                                                       
         BL    ROUVH                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         MVC   BATREF,FVIFLD                                                    
VALBRFX  B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE CHEQUE DATE                                                *         
***********************************************************************         
                                                                                
VALCDT   XC    CHQDATB,CHQDATB                                                  
         XC    CHQDATE,CHQDATE                                                  
         XC    CHQDATP,CHQDATP                                                  
         GOTO1 AVALDAT             VALIDATE AND DISPLAY A SINGLE DATE           
         BNE   ROUVX                                                            
         LA    R2,WORK             PERVAL OUTPUT BLOCK FROM VALDAT              
         USING PERVALD,R2                                                       
         MVC   CHQDATB,PVALCSTA    BINARY COMPRESSED START DATE                 
         MVC   CHQDATE,PVALESTA    EBCDIC START DATE                            
         MVC   CHQDATP,PVALPSTA    PACKED START DATE                            
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CHEQUE NUMBER                                              *         
***********************************************************************         
                                                                                
VALCHQ   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         MVC   CHQNUM,SPACES                                                    
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
*&&US                                                                           
         TM    FVIIND,FVINUM           VALID NUMERIC?                           
         BO    VALCH10                                                          
         SR    R0,R0                   MUST VALIDATE FURTHER                    
         IC    R0,FVILEN               R0=FIELD LENGTH                          
         LA    RF,FVIFLD               RF=A(FIELD)                              
VALCH02  CLI   0(RF),C'A'              A-Z, 0-9 ARE ALL VALID                   
         BL    VALCH04                                                          
         CLI   0(RF),C'9'                                                       
         BH    VALCH04                                                          
         LA    RF,1(RF)                                                         
         BCT   R0,VALCH02                                                       
         B     VALCH10                 CHECK NUMBER IS VALID                    
                                                                                
VALCH04  MVC   FVMSGNO,=AL2(AE$CHNNV)  CHECK NUMBER IS NOT VALID                
         B     ROUVH                                                            
*&&                                                                             
VALCH10  IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CHQNUM(0),FVIFLD                                                 
         B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE CHEQUE TYPE                                                *         
***********************************************************************         
                                                                                
VALCHT   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         LA    R1,L'AC@DRS-1                                                    
         LA    RE,AC@DRS                                                        
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RE)     DEBITS                                       
         BE    VALCHT2                                                          
         LA    R1,L'AC@CRS-1                                                    
         LA    RE,AC@CRS                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RE)     CREDITS                                      
         BE    VALCHT2                                                          
         LA    R1,L'AC@BOTH-1                                                   
         LA    RE,AC@BOTH                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RE)     BOTH                                         
         BE    VALCHT2                                                          
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     ROUVH                                                            
VALCHT2  ST    R1,FULL                                                          
         BAS   RE,CLRFLD           RF=A(FLDHDR) AFTER CLRFLD                    
         L     R1,FULL             R1=EXECUTE L'TEXT                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,RF),0(RE)                                             
         B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRODUCTION CLIENT                                          *         
***********************************************************************         
                                                                                
VALCLI   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         MVC   FVUNLD,PRODUL                                                    
         MVI   FVPREF,C'*'                                                      
         MVI   FVMAMI,X'11'                                                     
         XC    SRCHACC,SRCHACC                                                  
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         CLC   FVILEN,PRODALEN     CHECK L'CLIENT CODE                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         B     ROUVH                                                            
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING ACTRECD,R2          R2=A(ACCOUNT KEY)                            
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),FVIFLD                                                
         MVI   GETIND,0                                                         
         GOTO1 AGETACC,0                                                        
         BNE   ROUVH                                                            
         MVC   ACCOUNT,ACTKCULA    BUILD C/U/L/CLI                              
VALCLIX  B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CONTRA ACCOUNT IN BANK UNIT/LEDGER (CC)                    *         
***********************************************************************         
                                                                                
VALCOB   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         MVC   CONBNK,SPACES                                                    
         XC    CONTIND,CONTIND                                                  
         USING ACTRECD,R2          R2=A(CONTRA BANK A/C)                        
         LA    R2,CONBNK                                                        
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'BANKUL),BANKUL                                         
         MVI   CONBNKXL,L'COMPANY+L'BANKUL-1                                    
         MVC   FVUNLD,BANKUL                                                    
         MVI   FVPREF,C'*'                                                      
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
                                                                                
         IC    RF,FVXLEN                                                        
         CLI   FVIFLD,C'*'         TEST NON-STANDARD BANK A/C                   
         BNE   VALCOB02                                                         
         BCTR  RF,0                DROP ASTERISK                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKUNT(0),FVIFLD+1                                              
         LA    RF,L'ACTKCPY(RF)    ADD COMPANY                                  
         STC   RF,CONBNKXL                                                      
         B     VALCOB04                                                         
                                                                                
VALCOB02 EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),FVIFLD                                                
         LA    RF,ACTKACT-ACTRECD(RF)  ADD C/U/L                                
         STC   RF,CONBNKXL                                                      
                                                                                
VALCOB04 MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         MVC   ACTKCULA,CONBNK                                                  
         MVI   GETIND,0                                                         
         GOTO1 AGETACC,0           DON'T CARE IF NOT FOUND/VALID                
         BE    VALCOB06                                                         
         XC    RECNAME,RECNAME     CLEAR RECNAME, IF NOT FOUND/VALID            
         B     VALCOBX                                                          
VALCOB06 DS    0H                                                               
*&&UK                                                                           
         PUSH  USING                                                            
PRESET   USING CURTABD,PRSTCURT                                                 
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING ASTELD,R1                                                        
         XR    R0,R0                                                            
VALCOB08 AR    R1,R0                    SEARCH FOR AN ASTEL                     
         CLI   ASTEL,0                                                          
         BE    VALCOBX                                                          
         IC    R0,ASTLN                                                         
         CLI   ASTEL,ASTELQ                                                     
         BNE   VALCOB08                                                         
                                                                                
         CLI   PRESET.CURTCUR,ASTCANY   TEST COMPATABLE CURRENCY                
         BE    *+14                                                             
         CLC   PRESET.CURTCUR,SPACES                                            
         BH    *+10                                                             
         MVC   PRESET.CURTCUR,ASTCUR                                            
         CLC   PRESET.CURTCUR,ASTCUR                                            
         BNE   ROUVH                                                            
         POP   USING                                                            
*&&                                                                             
VALCOBX  B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CONTRA ACCOUNT                                             *         
***********************************************************************         
                                                                                
VALCON   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    CONTRA,CONTRA                                                    
         MVI   CONTRAXL,0                                                       
         XC    CONTIND,CONTIND                                                  
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         MVC   CONTRA,SPACES                                                    
         IC    RF,FVXLEN                                                        
         CLI   FVIFLD,C'*'         TEST NON-READ CONTRA                         
         BNE   VALCON2                                                          
*&&US                                                                           
         CLI   FVXLEN,12           CAN ENTER 13 CHARS-* +12 FOR ACCNT           
         BNH   *+14                FVXLEN HAS LENGTH-1                          
         MVC   FVMSGNO,=AL2(EAACCINV)                                           
         B     ROUVH                                                            
*&&                                                                             
         BCTR  RF,0                DROP ASTERISK                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTRA+(ACTKACT-ACTRECD)(0),FVIFLD+1                             
         LA    RF,ACTKACT-ACTRECD(RF)  ADD C/U/L                                
         STC   RF,CONTRAXL                                                      
         XC    RECNAME,RECNAME     CLEAR RECNAME                                
         B     VALCONX                                                          
                                                                                
VALCON2  MVC   CONTRA(L'COMPANY),COMPANY                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTRA+L'COMPANY(0),FVIFLD                                       
         LA    RF,L'ACTKCPY(RF)                                                 
         STC   RF,CONTRAXL                                                      
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING ACTRECD,R2          R2=A(ACCOUNT KEY)                            
         MVC   ACTKCULA,CONTRA                                                  
         MVI   GETIND,0                                                         
         GOTO1 AGETACC,0           DON'T CARE IF NOT FOUND/VALID                
         BE    *+14                                                             
         XC    RECNAME,RECNAME     CLEAR RECNAME, IF NOT FOUND/VALID            
         B     VALCONX                                                          
*&&US                                                                           
         NI    CONTIND,255-ACCISVLS  CLEAR SUPPLIER INDICATOR BITS              
         LA    RF,VENDLDGS         US/CANADA VENDORS                            
         LA    R0,VENDLDGN                                                      
         CLC   ACTKLDG,0(RF)                                                    
         BE    VALCON4                                                          
         LA    RF,L'VENDLDGS(RF)                                                
         BCT   R0,*-14                                                          
         B     VALCONX                                                          
                                                                                
VALCON4  OC    CONTIND,1(RF)       SET INDICATOR                                
*&&                                                                             
VALCONX  B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CURRENCY                                                   *         
***********************************************************************         
                                                                                
*&&UK                                                                           
         PUSH  USING                                                            
         USING FILTD,FILTVAL                                                    
VALCUR   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         GOTO1 AFLDVAL                                                          
         BH    VALCURH                                                          
         LA    R2,WORKD                                                         
         AH    R2,=Y(SCANOUT-WORKD)                                             
         USING SCANOUTD,R2         R2=A(SCANNER OUTPUT BLOCK)                   
         LR    R0,R2               CLEAR SCANNER OUTPUT BLOCK                   
         LA    R1,SCANLTAB                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 VSCANNER,DMCB,(L'SCANTXT2,FVADDR),('SCANMAXN',(R2)),    >        
               C',=  '                                                          
         CLI   4(R1),0                                                          
         BE    VALCURH                                                          
         MVI   FVINDX,1                                                         
         L     R0,ACURRTAB         CLEAR CURRENCY TABLE                         
         LA    R1,CURRTABN*L'CURRTAB                                            
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   FILT1,FILTFC+FILTAC  FILTER ALL NOT IN CURRENCY TABLE            
                                                                                
PRESET   USING CURTABD,PRSTCURT                                                 
         XR    R0,R0                                                            
         CLC   PRESET.CURTCUR,SPACES  INSERT PRESET INTO TABLE                  
         BNH   VALCUR02                                                         
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BE    VALCUR02                                                         
         GOTO1 ABLDCURT,DMCB,(X'00',PRESET.CURTCUR)                             
         BH    VALCURH             BAD PRESET CURRENCY                          
                                                                                
VALCUR02 CLI   SCANLEN1,0          TEST END OF SCAN TABLE                       
         BE    VALCURV                                                          
         CLI   SCANLEN2,0          TEST NO '=' SIGN                             
         BNE   VALCURH                                                          
         CLI   SCANLEN1,L'CURTCUR  TEST 3 CHAR CODE                             
         BNE   VALCUR08                                                         
         CLC   SCANTXT1(L'CURTCUR),SPACES                                       
         BNH   VALCUR06                                                         
                                                                                
         CLC   PRESET.CURTCUR,SPACES  TEST COMPATABLE WITH PRESET               
         BNH   VALCUR04                                                         
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BE    VALCUR04                                                         
         CLC   PRESET.CURTCUR,SCANTXT1                                          
         BNE   VALCURL                                                          
                                                                                
VALCUR04 GOTO1 ABLDCURT,DMCB,(X'00',SCANTXT1)                                   
         BCTR  R0,0                                                             
                                                                                
VALCUR06 LA    R2,SCANOUTL(R2)     TAKE NEXT FIELD                              
         B     VALCUR02                                                         
                                                                                
VALCUR08 XR    RE,RE               ALLOW ALL SPACES AS A CODE                   
         IC    RE,SCANLEN1                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    VALCUR06                                                         
         CLC   SCANTXT1(0),SPACES                                               
                                                                                
         LTR   RE,RE               TEST CODE IS SINGLE '*'                      
         BNZ   VALCURH                                                          
         CLI   SCANTXT1,C'*'                                                    
         BNE   VALCURH                                                          
         CLC   PRESET.CURTCUR,SPACES                                            
         BNH   *+12                                                             
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BNE   VALCURL                                                          
         MVI   PRESET.CURTCUR,ASTCANY                                           
         LTR   R0,R0                                                            
         BNZ   VALCURH                                                          
         NI    FILT1,FF-FILTFC-FILTAC  DON'T FILTER ANY CURRENCIES              
                                                                                
VALCURV  LTR   R0,R0               TEST TABLE ENTRIES AND NON-AGENCY            
         BNZ   *+8                                                              
         NI    FILT1,FF-FILTAC     DON'T FILTER AGENCY CURRENCIES               
         GOTO1 ABLDCURR            VALIDATE ISO CODES FOUND                     
         BNE   VALCURH                                                          
         LTR   R0,R0                                                            
         BNZ   VALCURX                                                          
         CLI   SCANTXT1,C'*'                                                    
         BNE   VALCURL                                                          
         DROP  R2                                                               
VALCURX  B     ROUVE               RETURN CODE EQUAL                            
VALCURH  MVC   FVMSGNO,=AL2(EAINVCUR)                                           
         B     ROUVH               RETURN CODE HIGH                             
VALCURL  B     ROUVL               RETURN CODE LOW - CHANGE CURRENCY            
         POP   USING                                                            
*&&                                                                             
*&&US                                                                           
VALCUR   B     ROUVE                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SINGLE DATE                                                *         
***********************************************************************         
                                                                                
VALDAT   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX               CC LOW OR HIGH                               
         MVC   WORK(1),AGYLANG     SET LANGUAGE                                 
         OI    WORK,X'60'          SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(WORK,WORK)                         
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   ROUVH                                                            
         BAS   RE,CLRFLD                                                        
         LA    RF,L'FVIHDR(RF)                                                  
         LA    R2,WORK+(PVALPSTA-PERVALD)                                       
         GOTO1 VDATCON,DMCB,(1,(R2)),(17,(RF))                                  
         B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE INCLUDE MARKED ITEMS TRANSACTIONS (DEFAULT=EXCLUDE MARKED) *         
***********************************************************************         
                                                                                
VALICL   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         MVI   ICLMARK,ICLMNO                                                   
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         LA    R2,L'AC@NO-1        SET EXECUTE LENGTH                           
         LA    RE,AC@NO            ADDRESS TEXT                                 
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),AC@NO     NO                                           
         BE    VALICLX                                                          
         MVI   ICLMARK,ICLMYES                                                  
         LA    R2,L'AC@YES-1       SET EXECUTE LENGTH                           
         LA    RE,AC@YES           ADDRESS TEXT                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),AC@YES    YES                                          
         BE    VALICLX                                                          
         MVI   ICLMARK,ICLMONLY                                                 
         LA    R2,L'AC@ONLY-1      SET EXECUTE LENGTH                           
         LA    RE,AC@ONLY          ADDRESS TEXT                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),AC@ONLY   ONLY                                         
         BE    VALICLX                                                          
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     ROUVH                                                            
VALICLX  L     R1,FVADDR           R1=A(FIELD HEADER)                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FVIFLD-FVIHDR(0,R1),FVIFLD-FVIHDR(R1)                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD-FVIHDR(0,R1),0(RE)                                        
         B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE INCLUDE RECEIPTS (DEFAULT=EXCLUDE RECEIPTS)                *         
***********************************************************************         
                                                                                
VALIRC   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         GOTO1 AFLDVAL                                                          
         BH    ROUVX               ERROR                                        
         BE    *+12                                                             
         OI    ICLMARK,ICLCRO      NO INPUT - CREDITS (PAYMENTS) ONLY           
         B     ROUVX                                                            
         LA    R2,L'AC@NO-1        SET EXECUTE LENGTH                           
         LA    RE,AC@NO            ADDRESS TEXT                                 
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),AC@NO     NO                                           
         BNE   *+12                                                             
         OI    ICLMARK,ICLCRO      NO - CREDITS (PAYMENTS) ONLY                 
         B     VALIRCX                                                          
         LA    R2,L'AC@YES-1       SET EXECUTE LENGTH                           
         LA    RE,AC@YES           ADDRESS TEXT                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),AC@YES    YES                                          
         BNE   *+12                                                             
         OI    ICLMARK,ICLDRCR                                                  
         B     VALIRCX                                                          
         LA    R2,L'AC@ONLY-1      SET EXECUTE LENGTH                           
         LA    RE,AC@ONLY          ADDRESS TEXT                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),AC@ONLY   ONLY                                         
         BNE   *+12                                                             
         OI    ICLMARK,ICLDRO                                                   
         B     VALIRCX                                                          
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     ROUVH                                                            
VALIRCX  L     R1,FVADDR           R1=A(FIELD HEADER)                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FVIFLD-FVIHDR(0,R1),FVIFLD-FVIHDR(R1)                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD-FVIHDR(0,R1),0(RE)                                        
         B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH ITEM COUNT                                           *         
***********************************************************************         
                                                                                
VALITE   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    BATITEM,BATITEM                                                  
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         TM    FVIIND,FVINUM       TEST VALID NUMERIC                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     ROUVH                                                            
         IC    R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FVIFLD(0)                                                    
         CVB   R1,DUB                                                           
         STCM  R1,3,BATITEM                                                     
         B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRODUCTION JOB                                             *         
* GETIND PRESET TO GETICCBQ OR 0 BY OVERLAY                           *         
***********************************************************************         
                                                                                
VALJOB   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         MVC   FVUNLD,PRODUL                                                    
         MVI   FVPREF,C'*'                                                      
         MVI   FVMAMI,X'33'                                                     
         MVC   SRCHACC,ACCOUNT+(ACTKACT-ACTKCULA)                               
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         XR    R0,R0                                                            
         XR    RF,RF                                                            
         IC    R0,PRODCLEN         R0=L'CLIENT+L'PRODUCT+L'JOB                  
         IC    RF,PRODBLEN         RF=L'CLIENT+L'PRODUCT                        
         SR    R0,RF               R0=L'JOB                                     
         CLM   R0,1,FVILEN         CHECK MAX L'JOB NOT EXCEEDED                 
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         B     ROUVH                                                            
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING ACTRECD,R2          R2=A(ACCOUNT KEY)                            
         MVC   ACTKCULA,ACCOUNT    TAKE C/U/L/CLI/PRO                           
         LA    RF,ACTKACT(RF)      RF=A(JOB IN KEY)                             
         IC    R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),FVIFLD                                                   
         OI    GETIND,GETIABLQ     ACCOUNT BALANCE ELEMENT REQUIRED             
         GOTO1 AGETACC,0                                                        
         BNE   ROUVH                                                            
         MVC   ACCOUNT,ACTKCULA    BUILD C/U/L/CLI/PRO/JOB                      
         GOTO1 APROFILE            EXTRACT TYPE/ACTION PROFILE                  
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE UNIT & LEDGER                                              *         
***********************************************************************         
                                                                                
VALLDG   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    LEDGER,LEDGER       UNIT & LEDGER FIELD                          
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING LDGRECD,R2          R2=A(ACCOUNT KEY)                            
         MVC   LDGKCPY,COMPANY                                                  
         MVI   LDGKUNT,C'S'        UNIT DEFAULTS TO S                           
         LA    R1,LDGKLDG          TAKE LEDGER ONLY FROM SCREEN                 
         XR    RF,RF                                                            
         ICM   RF,1,FVXLEN         TEST RF=0, ONE BYTE INPUT                    
         BZ    *+8                 YES                                          
         LA    R1,LDGKUNT          NO - TAKE UNIT & LEDGER FROM SCREEN          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),FVIFLD                                                   
         GOTO1 AGETLDG,0                                                        
         BNE   ROUVH                                                            
         MVC   LEDGER,LDGKUNT      EXTRACT UNIT & LEDGER                        
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q  RE-READ LEDGER RECORD              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOBUFF                                                       
         LA    R2,LDGRFST                                                       
         XR    RF,RF                                                            
                                                                                
         USING NAMELD,R2                                                        
VALLDG2  CLI   NAMEL,0             TEST E-O-R                                   
         BE    VALLDGX                                                          
         CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BE    VALLDG4                                                          
         IC    RF,NAMLN                                                         
         AR    R2,RF                                                            
         B     VALLDG2                                                          
                                                                                
VALLDG4  MVC   RECNAME,SPACES                                                   
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         BM    VALLDGX                                                          
         EX    RF,*+8                                                           
         B     VALLDGX                                                          
         MVC   RECNAME(0),NAMEREC  EXTRACT RECORD NAME                          
                                                                                
VALLDGX  B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE MOS RANGE FILTER                                           *         
***********************************************************************         
                                                                                
VALMOS   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    MOSSTA,MOSSTA                                                    
         MVI   MOSEND,X'FF'                                                     
         MVC   MOSEND+1(L'MOSEND-1),MOSEND                                      
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(AGYLANG,WORK)                      
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         TM    4(R1),X'03'         START AND/OR END INVALID?                    
         BNZ   ROUVH                                                            
         BAS   RE,CLRFLD                                                        
         LA    R3,L'FVIHDR(RF)                                                  
         LA    R2,WORK                                                          
         USING PERVALD,R2                                                       
         TM    PVALASSM,STARTASS   START ENTIRELY ASSUMED?                      
         BO    VALMOS2             YES - JUST TAKE PERVAL END DATE              
         MVC   MOSSTA,PVALPSTA     MOS RANGE START FOR FILTERING                
         GOTO1 VDATCON,DMCB,(1,PVALPSTA),(9,(R3))                               
         LA    R3,7(R3)                                                         
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         LA    R3,1(R3)                                                         
VALMOS2  MVI   0(R3),C'-'                                                       
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RF,FVIFLD(RF)                                                    
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    ROUVE               DON'T SET END DATE                           
         GOTO1 VDATCON,DMCB,(1,PVALPEND),(9,1(R3))                              
         MVC   MOSEND,PVALPEND     MOS RANGE END FOR FILTERING                  
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE MARK FIELD                                      *         
***********************************************************************         
                                                                                
VALMRK   MVI   BYTE,0                                                           
         ST    R1,FVADDR                                                        
         TM    FVATRB-FVIHDR(R1),FVAPROT  TEST WE PROTECTED TRANSACTION         
         BO    ROUVL               INDICATE NO INPUT                            
         LA    RF,L'FVIHDR(R1)                                                  
         CLI   0(RF),C'*'          TEST USER PROTECTED TRANSACTION              
         BNE   *+16                                                             
         MVI   BYTE,TSARCHGQ       INDICATE MARK FIELD HAS CHANGED              
         MVI   TSARCHA,C'*'        RESET THE 'PROTECT ME' CHARACTER             
         B     ROUVE               OVERLAY WILL CALL BLDLIN AT ONCE             
                                                                                
         CLI   OPTPAG,0            TEST PAGE MARK/UNMARK                        
         BE    *+14                                                             
         MVC   0(1,RF),OPTPAG      OPTPAG(1)=AC@YES/AC@NO                       
         B     VALMRK02                                                         
                                                                                
         CLI   0(RF),C' '                                                       
         BH    VALMRK02                                                         
         MVI   0(RF),C' '          SET FUNNY TO SPACE                           
         B     ROUVL               INDICATE NO INPUT                            
                                                                                
VALMRK02 MVI   BYTE,TSARMKQ        INDICATE USER IS MARKING                     
         CLC   0(1,RF),AC@YES                                                   
         BE    ROUVE                                                            
         MVI   BYTE,0              INDICATE USER IS UNMARKING                   
         CLC   0(1,RF),AC@NO                                                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     ROUVH                                                            
         MVI   0(RF),C' '          CLEAR "N"                                    
VALMRKX  B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE OFFICE CODE                                                *         
***********************************************************************         
                                                                                
VALOFF   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    OFFICE,OFFICE                                                    
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         XC    RECNAME,RECNAME     CLEAR RECORD NAME                            
         MVC   OFFICEXL,FVXLEN     PRESET EXECUTE L'OFFICE                      
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BNZ   VALOFF02                                                         
         CLI   FVILEN,1            NO - CHECK ONE CHARACTER                     
         BE    VALOFF06                                                         
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         B     ROUVH                                                            
                                                                                
VALOFF02 CLI   FVILEN,L'OFFKOFF                                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFSHRT)                                           
         B     ROUVH                                                            
         LA    R2,KEY                                                           
         USING OFFRECD,R2          R2=A(OFFICE RECORD KEY)                      
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
         MVC   OFFKOFF,FVIFLD                                                   
         MVC   FVXTRA(L'OFFKOFF),OFFKOFF                                        
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BE    VALOFF04                                                         
         MVC   FVMSGNO,=AL2(EGRECNOF)                                           
         B     ROUVH                                                            
VALOFF04 TM    OFFKSTAT,OFFSLIST        IS OFFICE AN OFFICE LIST ?              
         BNO   VALOFF05                                                         
         MVC   FVMSGNO,=AL2(AE$OLNO)                                            
         B     ROUVH                                                            
VALOFF05 MVC   FVXTRA(L'OFFKOFF),SPACES                                         
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AACCELS                                                          
                                                                                
VALOFF06 DS    0H                                                               
*&&US*&& TM    BATCHSEC,CPYBSOFF   TEST OFFICE SECURITY OVERRIDE                
*&&US*&& BZ    VALOFF08                                                         
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVI   OFFAACT,OFFAVAL     VALIDATE INPUT OFFICE                        
         MVC   OFFAOFFC,FVIFLD     SET INPUT OFFICE                             
         GOTO1 VOFFAL                                                           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(EASECLOC)                                           
         B     ROUVH               SECURITY LOCKOUT                             
VALOFF08 MVC   OFFICE,FVIFLD                                                    
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD FILTERS                                             *         
* NOTE - L'PERIOD FIELD MUST EQUAL L'PVALCPER                         *         
***********************************************************************         
                                                                                
VALPER   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    PERSTA,PERSTA                                                    
         MVI   PEREND,X'FF'                                                     
         MVC   PEREND+1(L'PEREND-1),PEREND                                      
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(AGYLANG,WORK)                      
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         TM    4(R1),X'03'         START AND/OR END INVALID?                    
         BNZ   ROUVH                                                            
         BAS   RE,CLRFLD           CLEAR PERIOD FIELD                           
         LA    R2,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R2                                                       
         TM    PVALASSM,STARTASS   START ENTIRELY ASSUMED?                      
         BO    VALPER4             YES - JUST TAKE PERVAL END DATE              
         MVC   PERSTA,PVALPSTA     TRANSACTION START DATE FOR FILTERING         
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RF,FVIFLD(RF)                                                    
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VALPER2             DON'T SET END DATE                           
         MVC   PEREND,PVALPEND     TRANSACTION END DATE FOR FILTERING           
         LA    RF,PVALCPER                                                      
         LA    R1,L'PVALCPER-1                                                  
         B     VALPERX             SHOW DDMMMYY-DDMMMYY                         
                                                                                
VALPER2  LA    RF,PVALCPER         TAKE PERVAL START DATE                       
         LA    R1,L'PVALCPER-1(RF)                                              
         CLI   0(R1),C'-'                                                       
         BE    *+10                                                             
         BCT   R1,*-8                                                           
         DC    H'0'                BAD OUTPUT FROM PERVAL                       
         SR    R1,RF                                                            
         B     VALPERX             SHOW DDMMMYY-                                
                                                                                
VALPER4  MVC   PEREND,PVALPEND     TRANSACTION END DATE FOR FILTERING           
         LA    RF,PVALCPER+(L'PVALCPER-1)                                       
         LR    R1,RF                                                            
         CLI   0(RF),C'-'                                                       
         BE    *+10                                                             
         BCT   RF,*-8                                                           
         DC    H'0'                BAD OUTPUT FROM PERVAL                       
         SR    R1,RF               SHOW -DDMMMYY                                
                                                                                
VALPERX  L     RE,FVADDR           DISPLAY THE PERIOD INTERPRETED               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,RE),0(RF)                                             
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRODUCTION PRODUCT                                         *         
***********************************************************************         
                                                                                
VALPRO   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         MVC   FVUNLD,PRODUL                                                    
         MVI   FVPREF,C'*'                                                      
         MVI   FVMAMI,X'22'                                                     
         MVC   SRCHACC,ACCOUNT+(ACTKACT-ACTKCULA)                               
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         XR    R0,R0                                                            
         XR    RF,RF                                                            
         IC    R0,PRODBLEN         R0=L'CLIENT+L'PRODUCT                        
         IC    RF,PRODALEN         RF=L'CLIENT                                  
         SR    R0,RF               R0=L'PRODUCT                                 
         CLM   R0,1,FVILEN         CHECK L'PRODUCT CODE                         
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         B     ROUVH                                                            
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING ACTRECD,R2          R2=A(ACCOUNT KEY)                            
         MVC   ACTKCULA,ACCOUNT    TAKE C/U/L/CLI                               
         LA    RF,ACTKACT(RF)      RF=A(PRODUCT IN KEY)                         
         IC    R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),FVIFLD                                                   
         MVI   GETIND,0                                                         
         GOTO1 AGETACC,0                                                        
         BNE   ROUVH                                                            
         MVC   ACCOUNT,ACTKCULA    BUILD C/U/L/CLI/PRO                          
VALPROX  B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE REFERENCE NUMBER RANGE                                     *         
***********************************************************************         
                                                                                
VALREF   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    REFVALS(REFVALSL),REFVALS                                        
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         XR    R1,R1                                                            
         IC    R1,FVXLEN           R1=EXECUTE L'FIELD                           
         LA    R2,FVIFLD(R1)       R2=A(LAST CHARACTER)                         
         CLI   FVIFLD,C'-'         TEST -END                                    
         BE    *+12                                                             
         CLI   0(R2),C'-'          TEST START-                                  
         BNE   VALREF04                                                         
                                                                                
         CLI   FVXLEN,L'REFSTA     REFERENCE MUST BE <=6 CHARS                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         B     ROUVH                                                            
         CLI   FVILEN,2            REFERENCE MUST BE >=1 CHAR                   
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFSHRT)                                           
         B     ROUVH                                                            
         BCTR  R1,0                DROP THE HYPHEN                              
         CLI   FVIFLD,C'-'         TEST -END                                    
         BE    VALREF02                                                         
         STC   R1,REFSTAXL         SET START REFERENCE                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REFSTA(0),FVIFLD                                                 
         MVI   REFEND,C'9'         SET MAXIMUM END REF                          
         MVC   REFEND+1(L'REFEND-1),REFEND                                      
         MVI   REFENDXL,L'REFEND-1 SET EXECUTE LENGTH                           
         B     VALREF10            AND THAT WILL DO                             
                                                                                
VALREF02 STC   R1,REFENDXL         SET END REFERENCE                            
         EX    R1,*+8                                                           
         B     VALREF10                                                         
         MVC   REFEND(0),FVIFLD+1                                               
                                                                                
VALREF04 LTR   R1,R1               TEST MORE THAN ONE CHARACTER                 
         BZ    VALREF06                                                         
         CLI   0(R2),C'-'          SEARCH FOR A HYPHEN                          
         BE    VALREF08                                                         
         BCTR  R2,0                                                             
         BCT   R1,*-10                                                          
         CLI   FVILEN,L'REFSTA     REFERENCE MUST BE <=6 CHARS                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         B     ROUVH                                                            
         IC    R1,FVXLEN                                                        
VALREF06 STC   R1,REFSTAXL         SET START REFERENCE                          
         EX    R1,*+8                                                           
         B     VALREF10                                                         
         MVC   REFSTA(0),FVIFLD                                                 
                                                                                
VALREF08 XR    RE,RE               EXTRACT BOTH SIDES                           
         IC    RE,FVXLEN                                                        
         SR    RE,R1                                                            
         CLM   R1,1,=AL1(L'REFSTA)                                              
         BH    *+12                                                             
         CLM   RE,1,=AL1(L'REFEND)                                              
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFLONG)                                           
         B     ROUVH                                                            
         BCTR  R1,0                                                             
         STC   R1,REFSTAXL                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REFSTA(0),FVIFLD                                                 
         BCTR  RE,0                                                             
         STC   RE,REFENDXL                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   REFEND(0),1(R2)                                                  
                                                                                
VALREF10 BAS   RE,CLRFLD           CLEAR AND REBUILD REFERENCE FIELD            
         LA    RE,L'FVIHDR(RF)     RE=A(REFERENCE FIELD)                        
         XR    RF,RF                                                            
         OC    REFSTA,REFSTA       HAVE WE A START NUMBER?                      
         BZ    VALREF12            MUST BE AN END                               
         IC    RF,REFSTAXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),REFSTA                                                   
         OC    REFEND,REFEND       HAVE WE AN END BILL NUMBER?                  
         BZ    VALREFX                                                          
         LA    RE,1(RF,RE)                                                      
VALREF12 MVI   0(RE),C'-'                                                       
         IC    RF,REFENDXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),REFEND                                                   
         CLC   REFSTA,REFEND       ENSURE START DOESN'T EXCEED END              
         BNH   VALREFX                                                          
         MVC   FVMSGNO,=AL2(EARNGINV)                                           
         B     ROUVH                                                            
                                                                                
VALREFX  B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SOURCE A/C (CPJEL)                                         *         
***********************************************************************         
                                                                                
VALSRC   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    SRCACC,SRCACC                                                    
         MVI   SRCACCXL,0                                                       
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         MVC   SRCACC,SPACES                                                    
         IC    R1,FVXLEN           L'FIELD-1                                    
         LA    RF,FVIFLD                                                        
         CLI   0(RF),C'*'          TEST NON-READ SOURCE ACCOUNT                 
         BNE   *+10                                                             
         LA    RF,1(RF)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRCACC(0),0(RF)                                                  
         STC   R1,SRCACCXL                                                      
         CLI   FVIFLD,C'*'         TEST NON-READ SOURCE ACCOUNT                 
         BE    VALSRC2                                                          
         MVC   KEY,SPACES          ATTEMPT TO READ FOR NAME                     
         LA    R2,KEY                                                           
         USING ACTRECD,R2          R2=A(ACCOUNT KEY)                            
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'SRCACC),SRCACC                                         
         MVI   GETIND,0                                                         
         GOTO1 AGETACC,0                                                        
         BE    VALSRCX                                                          
VALSRC2  XC    RECNAME,RECNAME     CLEAR RECNAME, IF NOT FOUND/VALID            
*                                  BUT STILL GIVE GOOD EXIT                     
VALSRCX  B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUPPLIER                                                   *         
* NTRY - LEDGER CONTAINS VALID UNIT/LEDGER CODE (FROM VALLDG)         *         
***********************************************************************         
                                                                                
VALSUP   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    ACCOUNT,ACCOUNT                                                  
         XC    ACCINDS,ACCINDS                                                  
         MVC   FVUNLD,LEDGER                                                    
         MVI   FVPREF,C'*'                                                      
         MVI   FVMAMI,0                                                         
         XC    SRCHACC,SRCHACC                                                  
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING ACTRECD,R2          R2=A(ACCOUNT KEY)                            
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'LEDGER),LEDGER  LEDGER ALREADY EXTRACTED               
         IC    RF,FVXLEN                                                        
         CHI   RF,11               COMPARE TO FULL ACCOUNT LENGTH               
         BNH   *+8                                                              
         LHI   RF,11               FORCE TO MAXIMUM LENGTH OF ACCOUNT           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),FVIFLD                                                
         MVI   GETIND,GETIABLQ+GETIRTXQ  ABLEL REQ'D + SEEK RATETAXQ EL         
         TM    XACTINDS,ACTIPOST   TEST ACTION POSTS                            
         BZ    *+8                                                              
         OI    GETIND,GETINLOK     GIVE ERROR IF ACCOUNT IS LOCKED              
         GOTO1 AGETACC,0                                                        
         BNE   ROUVH                                                            
         MVC   ACCOUNT,ACTKCULA                                                 
         XC    FACTOR,FACTOR       CLEAR FACTORING ACCOUNT                      
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING SPAELD,R1                                                        
         XR    R0,R0                                                            
VALSUP02 CLI   SPAEL,0                                                          
         BE    VALSUP06                                                         
         CLI   SPAEL,SPAELQ                                                     
         BNE   *+12                                                             
         CLI   SPATYPE,SPATFACC                                                 
         BE    VALSUP04                                                         
         IC    R0,SPALN                                                         
         AR    R1,R0                                                            
         B     VALSUP02                                                         
                                                                                
VALSUP04 MVC   FACTOR,SPAAULA                                                   
         DROP  R1                                                               
                                                                                
VALSUP06 DS    0H                                                               
*&&UK                                                                           
         PUSH  USING                                                            
         TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    VALSUP10                CURRENCY CODE PROBLEM                    
PRESET   USING CURTABD,PRSTCURT                                                 
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING ASTELD,R1                                                        
         XR    R0,R0                                                            
VALSUP08 AR    R1,R0               SCAN FOR ASTEL                               
         CLI   ASTEL,0                                                          
         BE    VALSUP10                                                         
         IC    R0,ASTLN                                                         
         CLI   ASTEL,ASTELQ                                                     
         BNE   VALSUP08                                                         
                                                                                
         CLI   PRESET.CURTCUR,ASTCANY  TEST COMPATABLE CURRENCY                 
         BE    *+14                                                             
         CLC   PRESET.CURTCUR,SPACES                                            
         BH    *+10                                                             
         MVC   PRESET.CURTCUR,ASTCUR                                            
         CLC   PRESET.CURTCUR,ASTCUR                                            
         BNE   ROUVH                                                            
         CLC   PRESET.CURTCUR,SPACES                                            
         BNH   VALSUP10                                                         
         CLC   PRESET.CURTCUR,COMPCURT+(CURTCUR-CURTABD)                        
         BE    VALSUP10                                                         
         TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BNZ   *+14                    CURRENCY CODE PROBLEM                    
         MVC   FVMSGNO,=AL2(AE$SUPCC)                                           
         B     ROUVH                                                            
*                                                                               
         MVI   FULL,ACTCURR                                                     
         GOTO1 VSECRET,DMCB,('SECPRACT',ASECBLK),(XTYPE,FULL)                   
         BE    VALSUP10                                                         
                                                                                
         MVC   FVMSGNO,=AL2(AE$SECUR)  CURRENCY SECURITY PROBLEM                
         MVI   FVXTRA,C'*'                                                      
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BE    ROUVH                                                            
         MVC   FVXTRA(L'CURTCUR),PRESET.CURTCUR                                 
         B     ROUVH                                                            
         POP   USING                                                            
*&&                                                                             
VALSUP10 GOTO1 APROFILE            EXTRACT TYPE/ACTION PROFILE                  
*&&US                                                                           
         LA    RF,VENDLDGS         US/CANADA VENDORS                            
         LA    R0,VENDLDGN                                                      
         CLC   ACTKLDG,0(RF)                                                    
         BE    VALSUP12                                                         
         LA    RF,L'VENDLDGS(RF)                                                
         BCT   R0,*-14                                                          
         B     VALSUP14                                                         
                                                                                
VALSUP12 MVC   ACCIND1,1(RF)       SET INDICATOR BYTE ONE                       
         TM    ACCIND1,ACCISPOT+ACCIPRNT  TEST SPOT/PRINT                       
         BZ    VALSUP14                                                         
         CLI   FVILEN,5            REPS ARE 5 CHARACTERS OR LESS                
         BH    *+12                                                             
         OI    ACCIND1,ACCIREPC    SET REP VENDOR ACCOUNT                       
         B     VALSUP14                                                         
         TM    ACCIND1,ACCIPRNT    TEST PRINT VENDOR                            
         BNO   VALSUP14                                                         
         CLI   ACTKACT+9,C'*'      TEST OFFICE OVERRIDE ACCOUNT                 
         BE    VALSUP14                                                         
         OI    ACCIND1,ACCIPUBN    SET SPECIAL PUBLICATION NUMBER               
*&&                                                                             
VALSUP14 B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE WORKCODE                                                   *         
***********************************************************************         
                                                                                
VALWRK   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    WORKCODE,WORKCODE   VALIDATE WORKCODE                            
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         MVC   KEY,SPACES                                                       
         LA    R2,KEY                                                           
         USING WCORECD,R2          R2=A(WORK CODE RECORD KEY)                   
         MVC   FVMSGNO,=AL2(EGIFSHRT)                                           
         CLI   FVILEN,L'WCOKWRK                                                 
         BNE   ROUVH                                                            
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,COMPANY                                                  
         MVC   WCOKUNT(L'PRODUL),PRODUL                                         
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WCOKWRK(0),FVIFLD                                                
         MVC   FVXTRA(L'PRODUL+L'WCOKWRK),WCOKUNT                               
         MVC   FVMSGNO,=AL2(EGRECNOF)                                           
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BNE   ROUVH                                                            
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AACCELS                                                          
         MVC   WORKCODE,FVIFLD                                                  
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ZOOM IN MARK FIELD                              *         
*                                                                     *         
* NTRY R1=A(FIELD HEADER)                                             *         
***********************************************************************         
                                                                                
VALZMRK  DS    0H                                                               
*&&DO                                                                           
         L     R2,VXTRAINF                                                      
         USING XTRAINFD,R2                                                      
         TM    XIFLAG1,XITSTADV    ARE WE CONNECTED TO TST SYSTEM               
         BNO   ROUVL               THIS FEATURE HAS BEEN DISABLED FOR           
*                                  LIVE AND CSC AS PER PSHA                     
         TM    XIFLAG2,XICSCADV    ARE WE CONNECTED TO CSC SYSTEM               
         BO    ROUVL               DISABLED FOR CSC ALSO.                       
*&&                                                                             
*                                                                               
         CLC   L'FVIHDR(L'DISLMARK,R1),AC@ZOOM                                  
         BNE   ROUVL               SET CC LOW IF NOT INPUT                      
         ST    R1,FVADDR                                                        
         LA    RE,MRKOLAYH                                                      
         SR    R1,RE                                                            
         STH   R1,SZOOMRK          DISPLACEMENT OF ZOOM MARK                    
         GOTO1 VSECRET,DMCB,('SECPRACT',ASECBLK),(XTYPE,=AL1(ACTZOOM))          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(EASECLOC)                                           
         B     ROUVH               SET CC HIGH IF ERROR                         
         TM    XACTIND2,ACTI2ZOO   TEST THAT ZOOM IS AVAILABLE                  
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     ROUVH               SET CC HIGH IF ERROR                         
VALZMRKX B     ROUVE               SET CC EQUAL IF ZOOM INPUT & VALID           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT STATUS LINE.                                      *         
* NTRY   -   0(R1) (STATUS_BYTE,A(TABLE))                             *         
*            4(R1) (PRINT_CONTROL,A(FIELD_HDR))                       *         
* PRINT_CONTROL - X'80' CLEAR FIELD FIRST                             *         
*               - X'40' REMOVE LAST STATUS ITEM IF DELETABLE          *         
***********************************************************************         
                                                                                
STATOUT  TM    4(R1),X'80'         TEST CLEAR STATUS LINE                       
         BZ    STAT002                                                          
         L     R3,4(R1)                                                         
         XR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R3)                                             
         SH    RF,=Y(L'FVIHDR+1)                                                
         BM    STAT002                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R3),L'FVIHDR(R3)                                      
                                                                                
STAT002  TM    4(R1),X'40'         TEST REMOVE LAST ITEM                        
         BZ    STAT004                                                          
         TM    STAIND1,STA1LID     TEST LAST ITEM DELETABLE                     
         BZ    STAT004                                                          
         NI    STAIND1,FF-STA1LID                                               
         L     R2,4(R1)                                                         
         XR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R2)                                             
         AR    R2,RF                                                            
         SH    RF,=Y(L'FVIHDR)                                                  
         LR    RE,RF                                                            
         BCTR  R2,0                LOOP TO FIND LAST COMMA                      
         CLI   0(R2),C','                                                       
         BE    *+10                                                             
         BCT   RF,*-10                                                          
         BCTR  RE,0                                                             
         SR    RE,RF               DELETE LAST ITEM                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)                                                    
                                                                                
STAT004  L     R3,0(R1)            STATUS TABLE                                 
         USING STATABD,R3                                                       
STAT006  IC    RF,STATEST          BRANCH STATUS SET/RESET                      
         LA    RE,STABO+STABZ                                                   
         NR    RF,RE                                                            
         IC    RE,STABIT                                                        
         EX    RE,*+12             TM     STATUS_BYTE,STABIT                    
         EX    RF,*+12             BO/Z   STAT008                               
         B     STAT100                                                          
         TM    0(R1),0             EXECUTED TEST INSTRUCTION                    
         NOP   STAT008             EXECUTED BRANCH INSTRUCTION                  
                                                                                
STAT008  XR    R2,R2                                                            
         TM    STATEST,STAPROD     TEST PRODUCTION                              
         BZ    STAT012                                                          
         L     RF,AIOBUFF                                                       
         LA    RE,TRNRFST-TRNRECD(RF)                                           
         USING CPYELD,RE                                                        
STAT010  CLI   CPYEL,0                                                          
         BE    STAT100                                                          
         CLI   CPYEL,CPYELQ                                                     
         BNE   *+14                                                             
         CLC   CPYPROD,TRNKUNT-TRNRECD(RF)                                      
         BE    *+14                                                             
         IC    R2,CPYLN                                                         
         AR    RE,R2                                                            
         B     STAT010                                                          
         DROP  RE                                                               
         OI    STAIND1,STA1PROD                                                 
                                                                                
STAT012  TM    STATEST,STAVEN       TEST VENDER                                 
         BZ    STAT016                                                          
         LA    RF,VENDLIST                                                      
         L     RE,AIOBUFF                                                       
STAT014  CLI   0(RF),EOT                                                        
         BE    STAT100                                                          
         CLC   TRNKUNT-TRNRECD(UNTLDGL,RE),0(RF)                                
         BE    *+12                                                             
         LA    RF,UNTLDGL(RF)                                                   
         B     STAT014                                                          
         OI    STAIND1,STA1VEN                                                  
                                                                                
STAT016  TM    STATEST,STABANK      TEST BANK                                   
         BZ    STAT020                                                          
         LA    RF,BANKLIST                                                      
         L     RE,AIOBUFF                                                       
STAT018  CLI   0(RF),EOT                                                        
         BE    STAT100                                                          
         CLC   TRNKUNT-TRNRECD(UNTLDGL,RE),0(RF)                                
         BE    *+12                                                             
         LA    RF,UNTLDGL(RF)                                                   
         B     STAT018                                                          
         OI    STAIND1,STA1BANK                                                 
                                                                                
STAT020  TM    STATEST,STACTRD     TEST CONTRA`D                                
         BZ    STAT024                                                          
         L     RE,AIOBUFF                                                       
         LA    RE,TRNRFST-TRNRECD(RE)                                           
         USING TRSELD,RE                                                        
         IC    RF,STATEST2                                                      
         LA    R2,STACON+STANCON                                                
         NR    RF,R2                                                            
STAT022  CLI   TRSEL,0                                                          
         BE    STAT100                                                          
         CLI   TRSEL,TRSELQ                                                     
         BNE   *+16                                                             
         TM    TRSSTAT,TRSSOFFS                                                 
         EX    RF,*+8              BO/Z   STAT024                               
         B     *+8                                                              
         NOP   STAT024                                                          
         IC    R2,TRSLN                                                         
         AR    RE,R2                                                            
         B     STAT022                                                          
         DROP  RE                                                               
                                                                                
STAT024  TM    STATEST,STAIR       TEST INVOICE REGISTER IN USE                 
         BZ    STAT028                                                          
         L     RE,AIOBUFF                                                       
         LA    RE,TRNRFST-TRNRECD(RE)                                           
         USING CPYELD,RE                                                        
STAT026  CLI   CPYEL,0                                                          
         BE    STAT100                                                          
         CLI   CPYEL,CPYELQ                                                     
         BNE   *+12                                                             
         TM    CPYSTAT4,CPYSIREG                                                
         BE    STAT028                                                          
         IC    R2,CPYLN                                                         
         AR    RE,R2                                                            
         B     STAT026                                                          
         DROP  RE                                                               
                                                                                
STAT028  TM    STATEST2,STAACTN     TEST ACTION TEST                            
         BZ    STAT030                                                          
         IC    R2,STATEST2                                                      
         LA    RE,X'0F'                                                         
         NR    R2,RE                                                            
         CLM   R2,1,XACTION         TEST ACTION                                 
         BNE   STAT100                                                          
         B     STAT050              THIS TEST NULLIFIES REST                    
                                                                                
STAT030  TM    STATEST2,STANACTN    TEST NOT ACTION TEST                        
         BZ    STAT032                                                          
         IC    R2,STATEST2                                                      
         LA    RE,X'0F'                                                         
         NR    R2,RE                                                            
         CLM   R2,1,XACTION         TEST ACTION                                 
         BE    STAT100                                                          
         B     STAT050              THIS TEST NULLIFIES REST                    
                                                                                
STAT032  TM    STATEST2,STANPRD+STANVEN+STANBNK                                 
         BZ    STAT034                                                          
         IC    R2,STATEST2                                                      
         LA    RE,STANPRD+STANVEN+STANBNK                                       
         NR    R2,RE                                                            
         EX    R2,*+12                                                          
         BZ    STAT034                                                          
         B     STAT100                                                          
         TM    STAIND1,0                                                        
                                                                                
STAT034  DS    0H                                                               
                                                                                
STAT050  L     R2,4(R1)            STATUS OUTPUT BUILD AREA                     
         XR    RF,RF                                                            
         IC    RF,FVTLEN-FVIHDR(R2)                                             
         AR    R2,RF                                                            
         SH    RF,=Y(L'FVIHDR)                                                  
         LR    R4,RF               MAX TEXT LENGTH                              
                                                                                
         BCTR  R2,0                LOOP TO FIND STATUS LINE END                 
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-10                                                          
         SR    R4,RF               REMAINING LENGTH                             
                                                                                
         IC    RF,STATXTL          ROOM NEEDED FOR TEXT                         
         MVC   LAREADDR,STATXT     EXTRACT TEXT                                 
         BCTR  RF,0                                                             
*MN                                                                             
*MN      EX    R0,LARE                                                          
         EX    0,LARE                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
*MN                                                                             
         LA    RE,WORK(RF)                                                      
         CLI   0(RE),C' '          FIND NEW STATUS WORD REAL LENGTH             
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         S     R4,=F'1'            SUBTRACT ONE BECAUSE MACHINE LEN             
         SR    R4,RF               TEST ROOM FOR TEXT                           
         BNP   STAT100                                                          
                                                                                
         CLI   0(R2),C' '          TEST NEED FOR COMMA                          
         BNH   STAT052                                                          
         S     R4,=F'1'            TEST ROOM FOR COMMA                          
         BNP   STAT100                                                          
         MVI   1(R2),C','          INSERT COMMA                                 
         LA    R2,2(R2)                                                         
                                                                                
STAT052  DS    0H                                                               
*MN                                                                             
*MN      EX    R0,LARE             INSERT TEXT                                  
         EX    0,LARE             INSERT TEXT                                   
         EX    RF,*+8                                                           
         B     STAT054                                                          
         MVC   0(0,R2),0(RE)                                                    
*MN                                                                             
                                                                                
STAT054  NI    STAIND1,FF-STA1LID                                               
         TM    STATEST,STASD       TEST STATUS DELETABLE                        
         BZ    *+8                                                              
         OI    STAIND1,STA1LID     LAST ITEM IS DELETABLE                       
                                                                                
STAT100  LA    R3,STALEN(R3)       TRY NEXT STATUS                              
         CLI   0(R3),EOT                                                        
         BNE   STAT006                                                          
STATOUTX B     ROUVE                                                            
         EJECT                                                                  
***********************************************************************         
* ZOOM FACILITY ROUTINE                                               *         
* NTRY - 0(R1) TSAR RECORD  (FIRST TIME THROUGH)                      *         
***********************************************************************         
                                                                                
ZOOMFAC  TM    TWAMODE3,TWAM3ZOO   TEST FOR SCREEN LOADED                       
         BO    ZOOM040                                                          
         MVC   SZOORNUM,0(R1)                                                   
         GOTO1 ATSARGET,SZOORNUM   LOAD TSAR RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    SZFLAGS,255-(SZFMRK+SZFSWP+SZFNAR+SZFDDT+SZFERP)                 
         TM    TSARINDS,TSARMKQ    TEST TRANSACTION MARKED                      
         BZ    *+8                                                              
         OI    SZFLAGS,SZFMRK                                                   
         MVC   SZDADDR,TSARDADR    SAVE DISK ADDRESS                            
         MVC   SZSTATS,TSARRSTA    SAVE DISK STATUS                             
         MVC   SZTWASC,TWASCROV    SAVE CURRENT OVERLAY SCREEN                  
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(2,0),TWAD                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AOVRSCR,ZFSCR1      ZOOM FACILITY SCREEN                         
         OI    TWAMODE3,TWAM3ZOO   SET SCREEN LOADED                            
         MVC   IODA,SZDADDR        SET DISK ADDRESS                             
         LA    R1,IOGET+IOACCMST+IO1Q  GETRUP BUILDS KEY ON OLD FILE            
         TM    SZSTATS,TRNSARCH    TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*&&US*&& XC    ZOOBRTT,ZOOBRTT     ALWAYS CLEAR EXCHANGE RATE TEXT              
*&&UK                                                                           
         OC    ATLX,ATLX                                                        
         BNZ   *+14                                                             
         XC    ZOOBRTT,ZOOBRTT                                                  
         B     ZOOM010                                                          
         GOTO1 AEDTRAT,DMCB,ATLXRATE,(L'ZOOBRAT,ZOOBRAT)                        
*&&                                                                             
                                                                                
ZOOM010  L     R2,AIOBUFF                                                       
         USING TRNRECD,R2          FILL SCREEN WITH TRANS REC. DETAILS          
         MVC   ZOOACCX(L'TRNKACT),TRNKACT                                       
         MVC   ZOOCTRX(L'TRNKULC),TRNKULC                                       
         MVC   ZOOREFX(L'TRNKREF),TRNKREF                                       
                                                                                
*&&UK                                                                           
         CLI   ZOOREFX+3,C'*'                                                   
         BNE   ZOOM014             SKIP IF NOT MODIFIED TO XXX*NN               
         LA    RE,TRNRFST                                                       
         USING FFTELD,RE                                                        
         XR    R1,R1                                                            
                                                                                
ZOOM012  IC    R1,FFTLN                                                         
         AR    RE,R1                                                            
         CLI   FFTEL,0                                                          
         BE    ZOOM014                                                          
         CLI   FFTEL,FFTELQ                                                     
         BNE   ZOOM012                                                          
         CLI   FFTTYPE,FFTTKREF                                                 
         BNE   ZOOM012                                                          
         MVC   ZOOREFX(L'TRNKREF),FFTDATA  SAVE ORIGINAL REF IF PRESENT         
         DROP  RE                                                               
*&&                                                                             
                                                                                
ZOOM014  DS    0H                                                               
         GOTO1 VDATCON,DMCB,(1,TRNKDATE),(17,ZOODATX)                           
         MVC   DUB,TRNRSMOS                                                     
         MVI   DUB+2,1             SET TO FIRST DAY OF MONTH                    
         GOTO1 VDATCON,DMCB,(1,DUB),(9,ZOOMOAX)                                 
         TM    TRNRSTA2,TRNSUSED   TEST FOR DUE DATE                            
         BO    ZOOM016                                                          
         GOTO1 VDATCON,DMCB,(2,TRNRSDUE),(17,ZOODDT1)                           
         B     ZOOM018                                                          
                                                                                
ZOOM016  OI    ZOODDT1H+(FVATRB-FVIHDR),FVAPROT                                 
         OI    ZOODDT1H+(FVOIND-FVIHDR),FVOXMT                                  
         OI    ZOOERPDH+(FVATRB-FVIHDR),FVAPROT                                 
                                                                                
ZOOM018  XC    STAIND1,STAIND1                                                  
         GOTO1 ASTATOUT,DMCB,(TRNRSTAT,STATAB1),(X'80',ZOOSTTXH)                
         GOTO1 ASTATOUT,DMCB,(TRNRSTA2,STATAB2),ZOOSTTXH                        
         DROP  R2                                                               
         GOTO1 ASETELAD,AIOBUFF    SET UP ELEMENT ADDRESSES                     
         USING TRNELD,R2           FILL SCREEN WITH TRANS ELE. DETAILS          
         ICM   R2,15,ATRNEL                                                     
         BZ    ZOOM038                                                          
         MVC   ZOOOFFX(L'TRNOFFC),TRNOFFC                                       
                                                                                
*&&UK                                                                           
ZOOM020  CURED TRNAMNT,(L'ZOOAMTX,ZOOAMTX),COMPCURT,ALIGN=LEFT,        >        
               ZERO=NOBLANK,MINUS=YES                                           
         MVC   ZOOAMCX,COMPCURT+(CURTCUR-CURTABD)                               
*&&                                                                             
                                                                                
*&&US                                                                           
ZOOM020  CURED TRNAMNT,(L'ZOOAMTX,ZOOAMTX),2,ALIGN=LEFT,               >        
               ZERO=NOBLANK,MINUS=YES                                           
*&&                                                                             
         GOTO1 ASTATOUT,DMCB,(TRNSTAT,STATAB3),ZOOSTTXH                         
         XR    RE,RE                                                            
         IC    RE,TRNLN            EXTRACT NARRATIVE                            
         LA    RF,TRNLN1Q+1                                                     
         SR    RE,RF                                                            
         BM    ZOOM022                                                          
         LA    RF,L'ZOONRT1-1                                                   
         LR    R1,RE                                                            
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ZOONRT1(0),TRNNARR                                               
         CR    R1,RF                                                            
         BNH   ZOOM022                                                          
         SHI   R1,L'ZOONRT1        R1=NARRATIVE LEFT TO PUT OUT                 
         LA    RF,L'ZOONRT2-1      RF=LENGTH AVAILABLE                          
         CR    R1,RF                                                            
         BNH   *+6                                                              
         LR    R1,RF                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ZOONRT2(0),TRNNARR+L'ZOONRT1                                     
ZOOM022  DS    0H                                                               
                                                                                
*&&US*&& XC    ZOOIERT,ZOOIERT      ALWAYS CLEAR EXCHANGE RATE TEXT             
*&&UK                                                                           
         USING AFCELD,R2                                                        
         ICM   R2,15,AAFCEL                                                     
         BNZ   *+14                                                             
         XC    ZOOIERX,ZOOIERX     NO AFCEL - CLEAR EXCHANGE RATE TEXT          
         B     ZOOM028                                                          
         OC    OLDAFCEL,OLDAFCEL   TEST AFCEL CONVERTED TO EUROS                
         BZ    ZOOM024                                                          
         LA    R2,OLDAFCEL         SHOW ORIGINAL DETAILS HERE                   
W        USING CURTABD,WORK                                                     
         MVC   W.CURTCUR,AFCCURR   BUILD TEMPORARY CURTAB ENTRY                 
         GOTO1 VBLDCUR,DMCB,W.CURTCUR,W.CURTABD,ACOM                            
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,W.CURTABD                                                     
         DROP  W                                                                
         B     *+8                                                              
                                                                                
ZOOM024  L     RF,ACURRTAB                                                      
         USING CURTABD,RF                                                       
         CLC   AFCCURR,CURTCUR                                                  
         BE    ZOOM026                                                          
         LA    RF,L'CURRTAB(RF)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                CURRENCY NOT FOUND IN TABLE                  
                                                                                
ZOOM026  MVC   ZOOCACX,AFCCURR                                                  
         CURED AFCAMNT,(L'ZOOCAMX,ZOOCAMX),CURTABD,ALIGN=LEFT,         >        
               ZERO=NOBLANK,MINUS=YES                                           
         GOTO1 AEDTRAT,DMCB,AFCXRATE,(L'ZOOIERX,ZOOIERX)                        
         DROP  R2,RF                                                            
*&&                                                                             
                                                                                
         USING TRSELD,R2                                                        
ZOOM028  ICM   R2,15,ATRSEL        TRANSACTION STATUS ELEMENT                   
         BZ    ZOOM030                                                          
         GOTO1 ASTATOUT,DMCB,(TRSSTAT,STATAB4),ZOOSTTXH                         
         GOTO1 ASTATOUT,DMCB,(TRSSTAT2,STATAB5),ZOOSTTXH                        
         DROP  R2                                                               
                                                                                
ZOOM030  LA    R3,ANOTELS          FILL SCREEN WITH NOTE DETAILS                
         LA    R4,ZOODAT1H                                                      
         USING ZOODAT1H,R4                                                      
         LA    R0,ANOTELN                                                       
                                                                                
         USING NOTELD,R2                                                        
ZOOM032  ICM   R2,15,0(R3)         GET ADDRESS OF NOTE ELEMENT                  
         BZ    ZOOM036                                                          
         GOTO1 VDATCON,DMCB,(2,NOTDATE),(17,ZOODAT1)                            
         MVC   ZOOREF1,NOTREF                                                   
         XR    RE,RE                                                            
         IC    RE,NOTLN                                                         
         SH    RE,=Y(NOTLN1Q+1)                                                 
         BNP   ZOOM034                                                          
         CH    RE,=Y(L'ZOOMEM1)                                                 
         BNH   *+8                                                              
         LH    RE,=Y(L'ZOOMEM1-1)                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ZOOMEM1(0),NOTNOTE                                               
         DROP  R2                                                               
                                                                                
ZOOM034  LA    R4,ZOODAT2H         BUMP TO NEXT SCREEN LINE                     
         LA    R3,L'ANOTELS(R3)    BUMP TO NEXT NOTE ELEMENT                    
         BCT   R0,ZOOM032                                                       
         DROP  R4                                                               
                                                                                
         USING GDAELD,R2                                                        
ZOOM036  ICM   R2,15,AGDAERPD      GENERAL DATE - EARLIEST PAYMENT DATE         
         BZ    ZOOM038                                                          
         GOTO1 VDATCON,DMCB,(1,GDADATE),(17,ZOOERPD)                            
         OI    ZOOERPDH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
ZOOM038  GOTO1 VSECRET,DMCB,('SECPFLDP',ASECBLK),=AL1(FLDNARR)                  
         BE    *+12                                                             
         OI    ZOONRT1H+(FVATRB-FVIHDR),FVAPROT                                 
         OI    ZOONRT2H+(FVATRB-FVIHDR),FVAPROT                                 
         GOTO1 VSECRET,DMCB,('SECPFLDP',ASECBLK),=AL1(FLDDUEDT)                 
         BE    *+8                                                              
         OI    ZOODDT1H+(FVATRB-FVIHDR),FVAPROT                                 
         GOTO1 VSECRET,DMCB,('SECPFLDP',ASECBLK),=AL1(FLDERPD)                  
         BE    *+8                                                              
         OI    ZOOERPDH+(FVATRB-FVIHDR),FVAPROT                                 
         MVC   FVMSGNO,=AL2(IAENTCHA)                                           
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         B     ZOOM142                                                          
                                                                                
ZOOM040  L     RF,AINP             VALIDATE PFK ACTIONS                         
         XR    RE,RE                                                            
         ICM   RE,1,TIOBAID-TIOBD(RF)                                           
         BZ    ZOOM050                                                          
         MVI   TIOBAID-TIOBD(RF),0 CLEAR PFKEY SETTING                          
         CH    RE,=Y(PFK03)        QUIT PFKEY?                                  
         BE    ZOOM090                                                          
         CH    RE,=Y(PFK02)        (UN)MARK PFKEY?                              
         BNE   *+8                                                              
         XI    SZFLAGS,SZFMRK      TOGGLE TRANSACTION MARKED                    
         MVC   FVMSGNO,=AL2(AI$MSWEC)                                           
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         B     ZOOM142                                                          
                                                                                
ZOOM050  LA    R2,ZOODDT1H         VALIDATE DUE DATE                            
         CLI   ZOODDT1H+(FVILEN-FVIHDR),0                                       
         BE    ZOOM062                                                          
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         GOTO1 AVALDAT,ZOODDT1H                                                 
         BNE   ZOOM150                                                          
         LA    R2,WORK             PERVAL OUTPUT BLOCK FROM VALDAT              
         MVC   HALF,PVALCSTA-PERVALD(R2) COMPRESSED DATE                        
         GOTO1 VDATCON,DMCB,(2,HALF),(17,ZOODDT1)                               
                                                                                
ZOOM062  LA    R2,ZOOERPDH         VALIDATE EARLIEST PAYMENT DATE               
         XC    DUB2,DUB2                                                        
         TM    ZOOERPDH+(FVATRB-FVIHDR),FVAPROT                                 
         BO    ZOOM070                                                          
         CLI   ZOOERPDH+(FVILEN-FVIHDR),0                                       
         BE    ZOOM070                                                          
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         GOTO1 AVALDAT,ZOOERPDH                                                 
         BNE   ZOOM150                                                          
         LA    R2,WORK             PERVAL OUTPUT BLOCK FROM VALDAT              
         USING PERVALD,R2                                                       
         MVC   DUB2(L'PVALPSTA),PVALPSTA                                        
         GOTO1 VDATCON,DMCB,(1,PVALPSTA),(17,ZOOERPD)                           
                                                                                
ZOOM070  LA    R4,ZOODAT1H         VALIDATE NOTE DETAILS                        
         USING ZOODAT1H,R4                                                      
         LA    R3,TEMP                                                          
         USING NOTELD,R3                                                        
         LA    R0,ANOTELN          # TRANSACTION NOTES                          
         XC    TEMP,TEMP                                                        
                                                                                
ZOOM072  CLI   ZOODAT1H+(FVILEN-FVIHDR),0                                       
         BE    ZOOM074                                                          
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         GOTO1 AVALDAT,ZOODAT1H                                                 
         BNE   ZOOM150                                                          
         LA    R2,WORK             PERVAL OUTPUT BLOCK FROM VALDAT              
         MVC   NOTDATE,PVALCSTA-PERVALD(R2) COMPRESSED DATE                     
                                                                                
ZOOM074  CLI   ZOOREF1H+(FVILEN-FVIHDR),0                                       
         BE    ZOOM076                                                          
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         GOTO1 AFLDVAL,ZOOREF1H                                                 
         BNE   ZOOM150                                                          
         MVC   NOTREF,FVIFLD                                                    
         MVC   ZOOREF1,FVIFLD                                                   
                                                                                
ZOOM076  XR    R2,R2                                                            
         CLI   ZOOMEM1H+(FVILEN-FVIHDR),0                                       
         BE    ZOOM078                                                          
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         GOTO1 AFLDVAL,ZOOMEM1H                                                 
         BNE   ZOOM150                                                          
         ICM   R2,1,FVILEN                                                      
         BZ    ZOOM078                                                          
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   NOTNOTE,FVIFLD                                                   
         MVC   ZOOMEM1,FVIFLD                                                   
         LA    R2,1(R2)                                                         
                                                                                
ZOOM078  OC    NOTREF,NOTREF       TEST REF OR MEMO                             
         BNZ   *+10                                                             
         LTR   R2,R2                                                            
         BZ    ZOOM080                                                          
         OC    NOTDATE,NOTDATE     TEST DATE PRESENT                            
         BNZ   *+10                                                             
         MVC   NOTDATE,TODAYC      DEFAULT TO TODAY                             
         GOTO1 VDATCON,DMCB,(2,NOTDATE),(17,ZOODAT1)                            
         LA    R2,NOTLN1Q(R2)                                                   
         STC   R2,NOTLN                                                         
         MVI   NOTEL,NOTELQ        NOTE ELEMENT CREATED                         
         AR    R3,R2               BUMP TO CREATE NEXT NOTE ELEMENT             
                                                                                
ZOOM080  LA    R4,ZOODAT2H         BUMP TO NEXT SCREEN LINE                     
         BCT   R0,ZOOM072                                                       
         DROP  R4                                                               
         DROP  R3                                                               
                                                                                
         LA    R1,ZOONRT1H         TEST CHANGED FIELDS                          
         XR    RE,RE                                                            
         LA    RF,ZOOTAB                                                        
         TM    FVIIND-FVIHDR(R1),FVITHIS                                        
         BO    ZOOM100             SOMETHING CHANGED                            
         ICM   RE,1,0(R1)                                                       
         BZ    *+8                                                              
         BXLE  R1,RE,*-16                                                       
                                                                                
ZOOM090  NI    TWAMODE3,255-TWAM3ZOO                                            
         MVC   BYTE,SZFLAGS                                                     
         MVC   HALF,SZOORNUM                                                    
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y((SAVEAREA+SAVEAREL)-MRKOLAYH)                            
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,(2,0),ATIA,,(RF)                    
         LA    R0,MRKOLAYH                                                      
         LH    R1,=Y((SAVEAREA+SAVEAREL)-MRKOLAYH)                              
         L     RE,ATIA                                                          
         MVC   MRKMSG,MRKMSG-TWAD(RE)                                           
         LA    RE,MRKOLAYH-TWAD(RE)                                             
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTO1 AXMTSCR                                                          
         MVC   SZFLAGS,BYTE                                                     
         MVC   SZOORNUM,HALF                                                    
         MVC   TWASCROV,SZTWASC                                                 
         MVI   ANYMARK,1             SET SOMETHING MARKED THIS TIME             
         LA    RF,MRKOLAYH                                                      
         AH    RF,SZOOMRK                                                       
         MVC   L'FVIHDR(1,RF),AC@NO  REPLACE ZOOM MARK                          
         TM    SZFLAGS,SZFMRK        TEST TRANSACTION MARKED                    
         BZ    *+10                                                             
         MVC   L'FVIHDR(1,RF),AC@YES                                            
ZOOM092  B     ROUVL                                                            
                                                                                
ZOOM100  MVC   FVMSGNO,=AL2(IAENTCHA)                                           
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         GOTO1 ATSARGET,SZOORNUM   RE-READ TRANSACTION TSAR RECORD              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODA,SZDADDR        SET DISK ADDRESS                             
         LA    R1,IOGETRUP+IOACCMST+IO1Q                                        
         TM    SZSTATS,TRNSARCH    TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF    SET UP ELEMENT ADDRESSES                     
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('NOTELQ',AIOBUFF),0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         LA    R0,ANOTELN          LOOP FOR EACH NOTE ELEMENT                   
         LA    R2,TEMP                                                          
         USING NOTELD,R2                                                        
         XR    R3,R3                                                            
                                                                                
ZOOM102  CLI   NOTEL,NOTELQ                                                     
         BNE   ZOOM104                                                          
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AIOBUFF,(R2)                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         IC    R3,NOTLN                                                         
         AR    R2,R3                                                            
         BCT   R0,ZOOM102                                                       
         DROP  R2                                                               
                                                                                
ZOOM104  TM    ZOONRT1H+(FVIIND-FVIHDR),FVITHIS                                 
         BNZ   ZOOM105                                                          
         TM    ZOONRT2H+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    ZOOM122                                                          
                                                                                
ZOOM105  GOTO1 ASETELAD,AIOBUFF    SET UP ELEMENT ADDRESSES                     
         XR    R2,R2               CLEAR R2                                     
         XR    R3,R3               CLEAR R3                                     
         XR    RE,RE               CLEAR RE                                     
         XR    RF,RF               CLEAR RF                                     
         OI    SZFLAGS,SZFNAR      SET NARRATIVE ALTERED                        
                                                                                
         LA    R3,TRNLN1Q                                                       
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         MVC   TEMP,TRNEL          COPY EXISTING TRNEL                          
         LA    RF,ZOONRT1+L'ZOONRT1-1 RF=A(LAST CHARACTER IN NARRATIVE)         
         LA    RE,L'ZOONRT1        RE=LENGTH OF FIRST NARRATIVE FIELD           
                                                                                
ZOOM106  CLI   0(RF),C' '          FIND LAST CHARACTER                          
         BH    ZOOM108                                                          
         SHI   RF,1                                                             
         BCT   RE,ZOOM106                                                       
         MVI   TEMP+TRNLN1Q,C' '   NO NARRATIVE, MOVE IN A BLANK                
         B     ZOOM109                                                          
                                                                                
ZOOM108  BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+TRNLN1Q(0),ZOONRT1                                          
                                                                                
ZOOM109  AHI   RE,1                                                             
         AR    R3,RE               R3=NEW LENGTH OF TRNEL                       
         CLC   ZOONRT2,SPACES      HAVE WE GOT ANYTHING IN THE NEXT FLD         
         BNH   ZOOM114             NO                                           
         LA    R1,TEMP+TRNLN1Q                                                  
         AR    R1,RE                                                            
         MVI   0(R1),C' '          SEPARATE WITH A SPACE                        
         AHI   R1,1                                                             
         AHI   R3,1                                                             
         LA    RF,ZOONRT2+L'ZOONRT2-1                                           
         LA    RE,L'ZOONRT2-1      SS                                           
                                                                                
ZOOM110  CLI   0(RF),C' '                                                       
         BH    ZOOM112                                                          
         SHI   RF,1                                                             
         BCT   RE,ZOOM110                                                       
                                                                                
ZOOM112  EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),ZOONRT2                                                  
         AHI   RE,1                                                             
         AR    R3,RE                                                            
                                                                                
ZOOM114  LA    R2,TEMP                                                          
         STC   R3,TRNLN                                                         
         CLI   TRNEL,TRNELQ                                                     
         BNE   ZOOM122                                                          
         L     RF,AIOBUFF                                                       
         L     RE,AIOBUFFA                                                      
         MVC   0(TRNRFST-TRNRECD,RE),0(RF)                                      
                                                                                
NEW      USING TRNRECD,RE                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   NEW.TRNRFST(0),TRNELD                                            
         LA    R3,NEW.TRNRFST+1(R3)                                             
         ICM   R2,15,ATRNEL                                                     
                                                                                
ZOOM116  CLI   TRNEL,0                                                          
         BE    ZOOM120                                                          
         SR    R1,R1                                                            
         IC    R1,TRNLN                                                         
         CLI   TRNEL,TRNELQ                                                     
         BE    ZOOM118                                                          
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)                                                    
         AHI   R1,1                                                             
         AR    R3,R1                                                            
                                                                                
ZOOM118  AR    R2,R1                                                            
         B     ZOOM116                                                          
                                                                                
ZOOM120  MVI   0(R3),0                                                          
         AHI   R3,1                ADD 1 FOR EOR                                
         SR    R3,RE                                                            
         STCM  R3,3,NEW.TRNRLEN                                                 
         LR    RF,R3                                                            
                                                                                
         L     R2,AIOBUFF          CLEAR AIOBUFF FIRST                          
         LA    R3,L'IOBUFF                                                      
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
                                                                                
         LR    R3,RF               MOVE FROM NEW BACK TO AIOBUFF                
         L     R2,AIOBUFF                                                       
         MVCL  R2,RE                                                            
         DROP  NEW                                                              
                                                                                
ZOOM122  GOTO1 ASETELAD,AIOBUFF    SET UP ELEMENT ADDRESSES                     
         TM    ZOODDT1H+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    ZOOM126             TEST DUE DATE CHANGED                        
         L     R2,AIOBUFF                                                       
         USING TRNRECD,R2                                                       
         TM    TRNRSTA2,TRNSUSED   TEST NOT USED DATE                           
         BO    ZOOM126                                                          
         MVC   TRNRSDUE,HALF       UPDATE DUE DATE IN TRANS RECORD              
         OI    SZFLAGS,SZFDDT      SET DUE DATE ALTERED                         
         USING DUEELD,R2                                                        
         ICM   R2,15,ADUEEL                                                     
         BNZ   ZOOM124                                                          
         LA    R2,TEMP                                                          
         MVI   DUEEL,DUEELQ                                                     
         MVI   DUELN,DUELNQ                                                     
                                                                                
ZOOM124  MVC   DUEDATE,HALF        UPDATE DUE DATE ELEMENT                      
         MVC   TSARFDUE,HALF                                                    
         MVC   TSARFDU2,HALF                                                    
         ICM   R2,15,ADUEEL                                                     
         BNZ   ZOOM126                                                          
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AIOBUFF,TEMP                           
         CLI   12(R1),0                                                         
         BE    ZOOM126                                                          
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R2                                                               
                                                                                
ZOOM126  TM    ZOOERPDH+(FVIIND-FVIHDR),FVITHIS                                 
         BZ    ZOOM134             TEST EARLIEST PAYMENT DATE CHANGED           
         L     R2,AIOBUFF                                                       
         USING TRNRECD,R2                                                       
         TM    TRNRSTA2,TRNSUSED   TEST NOT USED                                
         BO    ZOOM134                                                          
         OI    TSARIND3,TSARZEPD   ZOOM ALTERED EARLIEST PAYMENT DATE           
         USING GDAELD,R2                                                        
         ICM   R2,15,AGDAERPD      IF THERE IS ALREADY AN ELEMENT               
         BZ    ZOOM130                                                          
         OC    DUB2,DUB2           TEST USER REMOVING DATE                      
         BNZ   ZOOM128                                                          
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),('GDAELQ',AIOBUFF),0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         B     ZOOM132                                                          
ZOOM128  MVC   GDADATE,DUB2        UPDATE IT                                    
         B     ZOOM132                                                          
                                                                                
ZOOM130  LA    R2,ELEMT            OTHERWISE - BUILD A NEW ONE                  
         XC    GDAELD(GDALNQ),GDAELD                                            
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATERPD                                                 
         MVC   GDADATE,DUB2        SET EARLIEST PAYMENT DATE                    
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AIOBUFF,GDAELD                         
         CLI   12(R1),0                                                         
         BE    ZOOM132                                                          
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R2                                                               
                                                                                
ZOOM132  OI    SZFLAGS,SZFERP      SET EARLIEST PAYMENT DATE ALTERED            
                                                                                
ZOOM134  GOTO1 ASETELAD,AIOBUFF    SET UP ELEMENT ADDRESSES                     
         L     R2,AIOBUFF                                                       
         USING TRNRECD,R2                                                       
         ICM   R1,15,AAFCEL        TEST RECORD CARRIED AFCEL                    
         BZ    ZOOM136                                                          
         OC    OLDAFCEL,OLDAFCEL   TEST AFCEL CONVERTED TO EUROS                
         BZ    ZOOM136                                                          
         XC    OLDAFCEL,0(R1)      RESTORE ORIGINAL AFCEL                       
         XC    0(L'OLDAFCEL,R1),OLDAFCEL                                        
         XC    OLDAFCEL,0(R1)      WHEN WRITING BACK RECORD                     
         OI    SZFLAGS,SZFSWP                                                   
                                                                                
ZOOM136  TM    SZFLAGS,SZFERP+SZFNAR+SZFDDT  HAVE WE ALTERED ANYTHING           
         BZ    ZOOM138             NO                                           
         USING TRSELD,R1                                                        
         ICM   R1,15,ATRSEL        TEST RECORD CARRIED TRSEL                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   TRSMARK,TRSZOOMQ    SET CREDITOR ZOOM SCREEN                     
                                                                                
ZOOM138  LA    R1,IOPUT+IOACCMST+IO1Q                                           
         TM    SZSTATS,TRNSARCH    TEST RECORD ON ARCHIVE                       
         BNO   *+12                                                             
         NI    TRNRSTAT,255-TRNSARCH  CLEAR ACCARCH INDICATOR                   
         LA    R1,IOADFR+IOACCMST+IO1Q  PROMOTE RECORD TO ACCMST                
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   KEY(L'TRNKEY),TRNKEY  EXTRACT TRANSACTION KEY                    
         LA    R1,IORDUP+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   KEY+(TRNKSTA-TRNRECD)(L'TRNKSTA),TRNRSTA                         
         DROP  R2                                                               
         TM    SZSTATS,TRNSARCH                                                 
         BNO   *+14                                                             
         L     R2,AIOSAVE          SET NEW DISK ADDRESS                         
         MVC   KEY+(TRNKDA-TRNRECD)(L'TRNKDA),0(R2)                             
         LA    R1,IOWRITE+IOACCDIR+IO1Q                                         
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
ZOOM140  L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
ZOOM142  LA    R1,ZOONRT1H                                                      
         TM    ZOONRT1H+(FVATRB-FVIHDR),FVAPROT                                 
         BZ    ZOOM150                                                          
         LA    R1,ZOODDT1H                                                      
         TM    ZOODDT1H+(FVATRB-FVIHDR),FVAPROT                                 
         BZ    ZOOM150                                                          
         LA    R1,ZOOERPDH                                                      
         TM    ZOOERPDH+(FVATRB-FVIHDR),FVAPROT                                 
         BZ    ZOOM150                                                          
         LA    R0,ANOTELN          SET R1 TO A(FIRST EMPTY LINE)                
         LA    R0,ANOTELN          SET R1 TO A(FIRST EMPTY LINE)                
         LA    R1,ZOODAT1H                                                      
         USING ZOODAT1H,R1                                                      
         CLI   ZOODAT1H+FVILEN-FVIHDR,0                                         
         BE    ZOOM150                                                          
         LA    R1,ZOODAT2H                                                      
         BCT   R0,*-12                                                          
         DROP  R1                                                               
                                                                                
ZOOM150  ST    R1,FVADDR                                                        
         GOTO1 ASTATOUT,DMCB,(SZFLAGS,STATAB6),(X'40',ZOOSTTXH)                 
         LA    R1,ZOOSTTXH                                                      
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         MVC   FULL2,ADISPFKS                                                   
         LA    R1,ZOOPFKH                                                       
         ST    R1,ADISPFKS                                                      
         GOTO1 ABLDPFK             BUILD PFKEY DISPLAY LINE                     
         MVC   ADISPFKS,FULL2                                                   
         TM    SZFLAGS,SZFSWP      TEST AFCELS SWAPPED                          
         BZ    ZOOMFACX                                                         
         NI    SZFLAGS,FF-(SZFSWP) RESET AFCELS SWAPPED                         
         L     R1,AAFCEL           NOW RESTORE EURO AFCEL FOR DISPLAY           
         XC    OLDAFCEL,0(R1)                                                   
         XC    0(L'OLDAFCEL,R1),OLDAFCEL                                        
         XC    OLDAFCEL,0(R1)      AND KEEP ORIGINAL IN OLDAFCEL                
                                                                                
ZOOMFACX B     ROUVE                                                            
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* VALIDATE FOR SPECIAL PUBLICATION NUMBER AND CALL PUBVAL IF FOUND    *         
*                                                                     *         
* NTRY - R1=A(TRANSACTION KEY)                                        *         
***********************************************************************         
                                                                                
VALPUB   LR    R2,R1                                                            
         USING TRNRECD,R2          R2=A(TRANSACTION KEY)                        
         TM    ACCIND1,ACCIPUBN    TEST SPECIAL PUBLICATION NUMBER              
         BZ    VALPUBX             NO - EXIT                                    
         MVC   KEYSAVE(L'TRNKEY),TRNKEY                                         
         MVC   TRNKEY(L'KEY),SPACES                                             
         MVC   TRNKCULA,ACCOUNT                                                 
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BE    VALPUBX                                                          
         MVC   TRNKEY,KEYSAVE                                                   
         LA    R1,ACCOUNT+(ACTKACT-ACTRECD)                                     
         LA    RF,L'ACTKACT-1(R1)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R1               RF=L'SUPPLIER ACCOUNT-1                      
         BM    VALPUBX                                                          
         MVC   WORK(L'TRNKEY),TRNKEY                                            
         MVC   WORK+4(20),SPACES                                                
         GOTO1 VPUBVAL,DMCB,((RF),ACCOUNT+4),(1,WORK+4)                         
         CLI   0(R1),X'FF'         TEST FOR ERROR                               
         BE    VALPUBX                                                          
         MVC   TRNKEY,WORK         SET MASSAGED TRANSACTION KEY                 
VALPUBX  B     ROUVE                                                            
         DROP  R2                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* TEST CURRENCY IS EURO OR A EURO MEMBER                              *         
*                                                                     *         
* NTRY - R1=A(3 BYTE CURRENCY CODE)                                   *         
* EXIT - CC EQU  - CURRENCY IS A EURO MEMBER                          *         
*        CC LOW  - CURRENCY IS THE EURO                               *         
*        CC HIGH - CURRENCY IS NOT A EURO MEMBER/EURO                 *         
***********************************************************************         
                                                                                
TSTEUR   CLC   EURO,0(R1)          TEST CURRENCY IS THE EURO                    
         BE    ROUVL                                         - CC LOW           
         LA    RF,EURMEM           EURO MEMBER CURRENCIES                       
         LA    R0,EURMEMN                                                       
         CLC   0(EURMEML,RF),0(R1) TEST CURRENCY IS A EURO MEMBER               
         BE    ROUVX                                         - CC EQU           
         LA    RF,EURMEML(RF)                                                   
         BCT   R0,*-14                                                          
         B     ROUVH               CURRENCY NOT EURO/MEMBER  - CC HIGH          
         EJECT                                                                  
***********************************************************************         
* VALIDATE USED (CONTRA'D) DATE FILTERS                               *         
* NOTE - L'PERIOD FIELD MUST EQUAL L'PVALCPER                         *         
***********************************************************************         
                                                                                
VALUDA   OI    FVOIND-FVIHDR(R1),FVOXMT  R1=A(FIELD HEADER)                     
         XC    UDASTA,UDASTA                                                    
         MVI   UDAEND,X'FF'                                                     
         MVC   UDAEND+1(L'UDAEND-1),UDAEND                                      
         GOTO1 AFLDVAL                                                          
         BNE   ROUVX                                                            
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(AGYLANG,WORK)                      
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         TM    4(R1),X'03'         START AND/OR END INVALID?                    
         BNZ   ROUVH                                                            
         BAS   RE,CLRFLD           CLEAR PERIOD FIELD                           
         LA    R2,WORK             REFINE PERVAL DATE INTERPRETATION            
         USING PERVALD,R2                                                       
         TM    PVALASSM,STARTASS   START ENTIRELY ASSUMED?                      
         BO    VALUDA4             YES - JUST TAKE PERVAL END DATE              
         MVC   UDASTA,PVALCSTA     USED START DATE FOR FILTERING                
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RF,FVIFLD(RF)                                                    
         CLI   0(RF),C'-'          TEST LAST CHAR INPUT WAS '-'                 
         BE    VALUDA2             DON'T SET END DATE                           
         MVC   UDAEND,PVALCEND     USED END DATE FOR FILTERING                  
         LA    RF,PVALCPER                                                      
         LA    R1,L'PVALCPER-1                                                  
         B     VALUDAX             SHOW DDMMMYY-DDMMMYY                         
                                                                                
VALUDA2  LA    RF,PVALCPER         TAKE PERVAL START DATE                       
         LA    R1,L'PVALCPER-1(RF)                                              
         CLI   0(R1),C'-'                                                       
         BE    *+10                                                             
         BCT   R1,*-8                                                           
         DC    H'0'                BAD OUTPUT FROM PERVAL                       
         SR    R1,RF                                                            
         B     VALUDAX             SHOW DDMMMYY-                                
                                                                                
VALUDA4  MVC   UDAEND,PVALCEND     USED END DATE FOR FILTERING                  
         LA    RF,PVALCPER+(L'PVALCPER-1)                                       
         LR    R1,RF                                                            
         CLI   0(RF),C'-'                                                       
         BE    *+10                                                             
         BCT   RF,*-8                                                           
         DC    H'0'                BAD OUTPUT FROM PERVAL                       
         SR    R1,RF               SHOW -DDMMMYY                                
                                                                                
VALUDAX  L     RE,FVADDR           DISPLAY THE PERIOD INTERPRETED               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   L'FVIHDR(0,RE),0(RF)                                             
         B     ROUVE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROOT VALIDATION FACILITIES                                          *         
***********************************************************************         
                                                                                
***********************************************************************         
* CLEAR AND TRANSMIT AN INPUT FIELD ADDRESSED BY FVADDR               *         
***********************************************************************         
                                                                                
CLRFLD   L     RF,FVADDR                                                        
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
         XR    R1,R1                                                            
         IC    R1,FVTLEN-FVIHDR(RF)                                             
         SH    R1,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(RF),FVAXTND                                        
         BZ    *+8                                                              
         SH    R1,=Y(L'FVIHDR)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,RF),L'FVIHDR(RF)                                      
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* VENDOR LEDGERS LIST                                                 *         
***********************************************************************         
VENDLIST DS    0X                                                               
*&&UK*&& DC    C'SF',C'ST',C'SV',C'SX'                                          
*&&US*&& DC    C'SP',C'SQ',C'SS',C'ST',C'SU',C'SV',C'SW',C'SX',C'SY'            
         DC    AL1(EOT)                                                         
                                                                                
BANKLIST DS    0X                                                               
         DC    C'SC'                                                            
UNTLDGL  EQU   *-BANKLIST                                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* STATUS TABLES                                                       *         
***********************************************************************         
STATAB1  DS    0H                  TRANS. RECORD, TRNKSTAT BYTE                 
         DC    AL1(TRNSREVS,STABO),S(LC@RVRSL)                                  
         DC    AL1(L'LC@RVRSL,0)                                                
         DC    AL1(EOT)                                                         
         CNOP  2,4                                                              
                                                                                
STATAB2  DS    0H                  TRANS, RECORD, TRNKSTA2 BYTE                 
         DC    AL1(TRNSPEEL,STABO),S(LC@PELED)                                  
         DC    AL1(L'LC@PELED,0)                                                
         DC    AL1(TRNSGLUP,STABO),S(LC@UPDGL)                                  
         DC    AL1(L'LC@UPDGL,0)                                                
         DC    AL1(TRNSUSED,STABO+STAPROD),S(LC@BLD)                            
         DC    AL1(L'LC@BLD,0)                                                  
         DC    AL1(TRNSUSED,STABO+STAVEN+STACTRD),S(LC@PAID)                    
         DC    AL1(L'LC@PAID,STACON)                                            
         DC    AL1(TRNSUSED,STABO+STAVEN+STACTRD),S(LC@CTRD)                    
         DC    AL1(L'LC@CTRD,STANCON+STANACTN+ACTOFFS)                          
         DC    AL1(TRNSUSED,STABO),S(LC@USED)                                   
         DC    AL1(L'LC@USED,STANVEN+STANPRD)                                   
         DC    AL1(EOT)                                                         
         CNOP  2,4                                                              
                                                                                
STATAB3  DS    0H                  TRANS. ELEMENT, TRNSTAT BYTE                 
         DC    AL1(TRNSDR,STABO),S(LC@DR)                                       
         DC    AL1(L'LC@DR,0)                                                   
         DC    AL1(TRNSDR,STABZ),S(LC@CR)                                       
         DC    AL1(L'LC@CR,0)                                                   
         DC    AL1(TRNSURG,STABO),S(LC@URG)                                     
         DC    AL1(L'LC@URG,0)                                                  
         DC    AL1(TRNSREV,STABO),S(LC@RVRSL)                                   
         DC    AL1(L'LC@RVRSL,STANACTN+ACTREVS)                                 
         DC    AL1(TRNSAUTH,STABO+STAIR),S(LC@AUTHD)                            
         DC    AL1(L'LC@AUTHD,STANACTN+ACTAUTH)                                 
         DC    AL1(TRNSAPPR,STABO+STAVEN),S(LC@SELED)                           
         DC    AL1(L'LC@SELED,STANACTN+ACTSELC)                                 
         DC    AL1(TRNSNOCM,STABO),S(LC@NCOM)                                   
         DC    AL1(L'LC@NCOM,0)                                                 
         DC    AL1(TRNSHOLD,STABO+STAVEN),S(LC@HELD)                            
         DC    AL1(L'LC@HELD,STANACTN+ACTHOLD)                                  
         DC    AL1(TRNSBREC,STABO+STABANK),S(LC@RCND)                           
         DC    AL1(L'LC@RCND,STANACTN+ACTRECN)                                  
         DC    AL1(EOT)                                                         
         CNOP  2,4                                                              
                                                                                
STATAB4  DS    0H                  TRANS. STATUS ELEMENT, TRSSTAT BYTE          
         DC    AL1(TRSSOFFS,STABO),S(LC@OFFST)                                  
         DC    AL1(L'LC@OFFST,STANACTN+ACTOFFS)                                 
         DC    AL1(TRSSVOID,STABO),S(LC@VOID)                                   
         DC    AL1(L'LC@VOID,STANACTN+ACTVOID)                                  
         DC    AL1(EOT)                                                         
         CNOP  2,4                                                              
                                                                                
STATAB5  DS    0H                  TRANS. STATUS ELEMENT, TRSSTAT2 BYTE         
         DC    AL1(TRSSACRL,STABO),S(LC@ACRL)                                   
         DC    AL1(L'LC@ACRL,0)                                                 
         DC    AL1(TRSSACRV,STABO),S(LC@ACRRV)                                  
         DC    AL1(L'LC@ACRRV,0)                                                
         DC    AL1(EOT)                                                         
         CNOP  2,4                                                              
                                                                                
STATAB6  DS    0H                  ZOOM FACILITY, SZFLAGS BYTE                  
         DC    AL1(SZFMRK,STABO+STASD),S(LC@AUTHD)                              
         DC    AL1(L'LC@AUTHD,STAACTN+ACTAUTH)                                  
                                                                                
         DC    AL1(SZFMRK,STABO+STASD),S(LC@SELED)                              
         DC    AL1(L'LC@SELED,STAACTN+ACTSELC)                                  
                                                                                
         DC    AL1(SZFMRK,STABO+STASD),S(LC@HELD)                               
         DC    AL1(L'LC@HELD,STAACTN+ACTHOLD)                                   
                                                                                
         DC    AL1(SZFMRK,STABO+STASD),S(LC@DSC)                                
         DC    AL1(L'LC@DSC,STAACTN+ACTDISC)                                    
                                                                                
         DC    AL1(SZFMRK,STABO+STASD),S(LC@PAID)                               
         DC    AL1(L'LC@PAID,STAACTN+ACTMANU)                                   
                                                                                
         DC    AL1(SZFMRK,STABO+STASD),S(LC@MARKD)                              
         DC    AL1(L'LC@MARKD,STAACTN+ACTCHEQ)                                  
                                                                                
*&&UK*&& DC    AL1(SZFMRK,STABO+STASD),S(LC@CTRD)                               
*&&US*&& DC    AL1(SZFMRK,STABO+STASD),S(LC@OFFST)                              
*&&UK*&& DC    AL1(L'LC@CTRD,STAACTN+ACTOFFS)                                   
*&&US*&& DC    AL1(L'LC@OFFST,STAACTN+ACTOFFS)                                  
         DC    AL1(EOT)                                                         
         CNOP  2,4                                                              
                                                                                
VENDLDGS DS    0XL2                                                             
         DC    CL1'S',AL1(ACCISPOT)                                             
         DC    CL1'T',AL1(ACCISPOT)                                             
         DC    CL1'U',AL1(ACCISPOT+ACCINETW)                                    
         DC    CL1'P',AL1(ACCIPRNT)                                             
         DC    CL1'Q',AL1(ACCIPRNT)                                             
         DC    CL1'V',AL1(ACCIPROD)                                             
         DC    CL1'Y',AL1(ACCIPROD)                                             
         DC    CL1'R',AL1(ACCIPROD)  TREAT SR AS GENUINE PRODUCTION             
VENDLDGN EQU   (*-VENDLDGS)/L'VENDLDGS                                          
                                                                                
EURO     DC    CL3'EUR'            EURO CURRENCY CODE                           
                                                                                
EURMEM   DC    C'IEP'              EURO MEMBER CURRENCIES                       
EURMEML  EQU   *-EURMEM                                                         
         DC    C'PTE'                                                           
         DC    C'FIM'                                                           
         DC    C'ESP'                                                           
         DC    C'ITL'                                                           
         DC    C'NLG'                                                           
         DC    C'DEM'                                                           
         DC    C'BEF'                                                           
         DC    C'LUF'                                                           
         DC    C'ATS'                                                           
         DC    C'FRF'                                                           
EURMEMN  EQU   (*-EURMEM)/EURMEML                                               
         LTORG                                                                  
         DROP  RB,RA,R9,R8,R5                                                   
         EJECT                                                                  
       ++INCLUDE ACMRKWRK                                                       
       ++INCLUDE ACMRKCCD          CREDITOR/CHEQUE DSECT REQUIRED               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* ACGOBLOCK                                                                     
         PRINT OFF                                                              
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
* FAXTRAINF                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'103ACMRK42   06/12/18'                                      
         END                                                                    
