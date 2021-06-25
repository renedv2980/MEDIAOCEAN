*          DATA SET ACMRK09    AT LEVEL 053 AS OF 03/28/14                      
*PHASE T61609A                                                                  
ACMRK09  TITLE 'BANK - RECONCILE'                                               
* JFOX 046 YEAR 2000 FIX                                                        
ACMRK09  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MRK9**,RA                                                    
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         L     R8,AOVERWRK                                                      
         USING OVRWRKD,R8                                                       
         USING SAVED,R7                                                         
         USING TWAD,R6                                                          
                                                                                
*                                  SET INPUT SCREEN DISPS FOR ROOT              
         LH    R1,=Y(INPHEDH-TWAD)                                              
         STH   R1,DISPHED          DISPLACEMENT OF INPUT HEADLINE               
         AR    R1,R6                                                            
         ST    R1,ADISHEAD         A(INPUT HEADLINE)                            
         LH    R1,=Y(INPHD2H-TWAD)                                              
         STH   R1,DISPHED2         DISPLACEMENT OF INPUT 2ND HEADLINE           
         AR    R1,R6                                                            
         ST    R1,ADISHEA2         A(INPUT 2ND HEADLINE)                        
         LH    R1,=Y(INPDETH-TWAD)                                              
         STH   R1,DISPDET          DISPLACEMENT OF 1ST DETAIL LINE              
         AR    R1,R6                                                            
         ST    R1,ADISDET1         A(1ST DETAIL LINE)                           
         LH    R1,=Y(INPTOTH-TWAD)                                              
         STH   R1,DISPTOT          DISPLACEMENT OF TOTALS LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISTOTS         A(TOTALS LINE)                               
         LH    R1,=Y(INPPFKH-TWAD)                                              
         STH   R1,DISPPFK          DISPLACEMENT OF PF KEY LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISPFKS         A(PF KEY LINE)                               
                                                                                
                                                                                
         CLI   BYTE,ACTIPRVL                                                    
         BE    PREVAL              ROUTINE TO PRE-VALIDATE HEADER               
         TM    TWAMODE2,TWAM2NXA                                                
         BO    NXTACC              ROUTINE TO GET NEXT BANK ACCOUNT             
         CLI   XACTION,ACTUPDT                                                  
         BE    UPDATE              UPDATE                                       
         CLI   XACTION,ACTDRFT                                                  
         BE    UPDATE              DRAFT (UPDATE WITHOUT UPDATE)                
         CLI   XACTION,ACTQUIT                                                  
         BE    QUIT                QUIT                                         
         CLI   XACTION,ACTRECN                                                  
         BE    *+6                 RECONCILE                                    
         DC    H'0'                                                             
         CLI   TWASCROV,BRSCR1                                                  
         BE    VALHED              RECONCILE - VALIDATE HEADER                  
         CLI   TWASCROV,BRSCR2                                                  
         BE    VALINP              RECONCILE - VALIDATE INPUT                   
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* PRE-VALIDATE HEADER                                                 *         
***********************************************************************         
                                                                                
PREVAL   CLI   PROFBSDT,C'Y'       TEST STATEMENT DATE COMPULSORY               
         BNE   *+12                                                             
         OI    RECSDTTH+(FVATRB-FVIHDR),FVAHIGH                                 
         OI    RECSDTTH+(FVOIND-FVIHDR),FVOXMT                                  
*                                  CONTROL TOTAL CODE SUSPENDED                 
*        CLI   PROFBTIC,1          TEST BATCH TOTAL/ITEM COUNT REQ'D            
*        BE    PREVAL04            PRESENT, BUT NOT REQUIRED                    
*        CLI   PROFBTIC,2          TEST REQUIRED                                
*        BE    PREVAL02                                                         
*        OI    RECITEH+(FVATRB-FVIHDR),FVAPROT  PROTECT ITEM COUNT              
*        OI    RECTOTH+(FVATRB-FVIHDR),FVAPROT  PROTECT BATCH TOTAL             
*        OI    RECITEH+(FVOIND-FVIHDR),FVOXMT   XMIT ITEM COUNT                 
*        OI    RECTOTH+(FVOIND-FVIHDR),FVOXMT   XMIT BATCH TOTAL                
*        XC    RECITET,RECITET     CLEAR ITEM COUNT TEXT FIELD                  
*        XC    RECTOTT,RECTOTT     CLEAR BATCH TOTAL TEXT FIELD                 
*        B     *+12                                                             
                                                                                
*REVAL02 OI    RECITETH+(FVATRB-FVIHDR),FVAHIGH  HIGHLIGHT ITEM COUNT           
*        OI    RECTOTTH+(FVATRB-FVIHDR),FVAHIGH  HIGHLIGHT BATCH TOTAL          
*        OI    RECITETH+(FVOIND-FVIHDR),FVOXMT                                  
*        OI    RECTOTTH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
*                                  DEFAULT MOA CODE SUSPENDED                   
*REVAL04 CLI   RECMOAH+(FVILEN-FVIHDR),0                                        
*        BNE   PREVAL08            LEAVE MOA RANGE INTACT, IF PRESENT           
*        MVC   TEMP(L'TODAYB),TODAYB  TAKE TODAY IN BINARY                      
*        SR    R1,R1                                                            
*        IC    R1,TEMP+1                                                        
*        SH    R1,=H'1'            GO BACK ONE MONTH                            
*        BNP   *+12                CROSSED YEAR BOUNDARY                        
*        STC   R1,TEMP+1           SET START MONTH                              
*        B     PREVAL06            DISPLAY DATE                                 
*        AH    R1,=H'12'           SET RELEVANT MONTH IN PREVIOUS YEAR          
*        STC   R1,TEMP+1                                                        
*        ICM   R1,1,TEMP                                                        
*        BNZ   *+12                TEST CENTURY BOUNDARY                        
*        LA    R1,99               SET LAST YEAR OF PREVIOUS CENTURY            
*        B     *+6                                                              
*        BCTR  R1,0                GO BACK A YEAR                               
*        STC   R1,TEMP                                                          
*REVAL06 LA    R2,RECMOA                                                        
*        GOTO1 VDATCON,DMCB,(3,TEMP),(9,(R2))                                   
*        LA    R2,7(R2)                                                         
*        CLI   0(R2),C' '                                                       
*        BH    *+8                                                              
*        BCT   R2,*-8                                                           
*        MVI   1(R2),C'-'                                                       
*        GOTO1 (RF),(R1),(3,TODAYB),(9,2(R2))                                   
                                                                                
PREVAL08 LA    R1,LACCOUNT                                                      
         USING ACTRECD,R1                                                       
         CLC   ACTKCULA,SPACES     TEST LAST TYPE/ACTION ACCOUNT                
         BNH   PREVAL12                                                         
         CLI   LTYPE,TYPBNK        TEST BANK ACTION LAST                        
         BNE   PREVAL10                                                         
         XC    RECBNK,RECBNK                                                    
         OI    RECBNKH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   RECBNK(L'ACTKACT),ACTKACT                                        
         CLC   ACTKUNT(L'BANKUL),BANKUL                                         
         BE    PREVAL12                                                         
         MVI   RECBNK,C'*'         OVERRIDE BANK UNIT/LEDGER                    
         MVC   RECBNK+1(L'ACTKULA),ACTKULA                                      
         B     PREVAL12                                                         
                                                                                
PREVAL10 DS    0H                                                               
*&&UK                                                                           
         CLI   LTYPE,TYPCRD        TEST CREDITOR ACTION LAST                    
         BNE   PREVAL12                                                         
         XC    RECCON,RECCON                                                    
         OI    RECCONH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   RECCON(L'ACTKULA),ACTKULA                                        
         B     PREVAL12                                                         
*&&                                                                             
PREVAL12 XC    LACCOUNT,LACCOUNT   CLEAR LAST TYPE/ACTION ACCOUNT               
                                                                                
         TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    PREVAL14                                                         
         MVI   FULL,ACTCURR                                                     
         GOTO1 VSECRET,DMCB,('SECPRACT',ASECBLK),('TYPBNK',FULL)                
         BE    PREVAL16                                                         
                                                                                
PREVAL14 DS    0H                                                               
*&&UK                                                                           
         XC    RECCURT,RECCURT                                                  
         OI    RECCURTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    RECCUR,RECCUR                                                    
         OI    RECCURH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECCURH+(FVOIND-FVIHDR),FVOXMT                                   
*&&                                                                             
PREVAL16 DS    0H                                                               
                                                                                
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SET NEXT ACCOUNT IN RELEVANT SCREEN FIELD                           *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
NXTACC   CLI   RECBNKH+(FVILEN-FVIHDR),0  TEST INPUT TO BANK ACCOUNT            
         BNE   *+16                                                             
         XC    TWASKEY,TWASKEY     YES - CLEAR KEY SAVED IN TWA                 
         XC    ACCOUNT,ACCOUNT     AND MUST CLEAR BANK ACCOUNT                  
         OC    ACCOUNT,ACCOUNT     TEST FIRST TIME FOR TWAM2NXA                 
         BNZ   NXTACC2                                                          
         LA    R2,ACCOUNT          YES - REBUILD ACCOUNT                        
         MVC   ACCOUNT,SPACES                                                   
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'BANKUL),BANKUL                                         
         OC    TWASKEY,TWASKEY     TEST ACCOUNT SAVED IN TWA                    
         BZ    NXTACC4                                                          
         MVC   ACTKUNT(L'ACTKCULA-1),TWASKEY  RESTORE ACCOUNT                   
NXTACC2  LA    R2,KEY                                                           
         MVC   ACTKEY,SPACES       BUILD KEY FOR IO ROUTINE                     
         MVC   ACTKCULA,ACCOUNT                                                 
         B     NXTACC8                                                          
                                                                                
NXTACC4  LA    R2,KEY                                                           
         MVC   ACTKCULA,ACCOUNT                                                 
NXTACC6  SR    RF,RF               BUMP KEY FOR NEXT ACCOUNT                    
         IC    RF,ACTKACT+L'ACTKACT-1                                           
         LA    RF,1(RF)                                                         
         STC   RF,ACTKACT+L'ACTKACT-1                                           
         GOTO1 AIOEXEC,IOHI+IOACCDIR+IO1Q                                       
         CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BNE   NXTACC12            END OF LEDGER                                
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AACCELS                                                          
         OC    RECABLEL,RECABLEL   TEST LOW LEVEL ACCOUNT                       
         BZ    NXTACC6             NO - TRY AGAIN                               
         MVC   ACCOUNT,ACTKCULA                                                 
         B     NXTACC10                                                         
                                                                                
NXTACC8  SR    RF,RF               BUMP KEY FOR NEXT BANK ACCOUNT               
         IC    RF,ACTKCULA+L'ACTKCULA-1                                         
         LA    RF,1(RF)                                                         
         STC   RF,ACTKCULA+L'ACTKCULA-1                                         
         MVI   GETIND,GETIABLQ                                                  
         GOTO1 AGETACC,0                                                        
         BNE   NXTACC10            NOT FOUND/NOT VALID                          
         CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BE    NXTACC14            UNIT/LEDGER IS STILL OK                      
         B     NXTACC12            UNIT/LEDGER HAS CHANGED - FINISH             
                                                                                
NXTACC10 CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BNE   NXTACC12            PAST BANK LEDGER                             
         MVI   GETIND,GETIABLQ     RE-/READ ACCOUNT                             
         GOTO1 AGETACC,0                                                        
         BE    NXTACC14            VALID THIS TIME                              
         B     NXTACC8             STILL NO GOOD - TRY NEXT                     
                                                                                
NXTACC12 MVC   FVMSGNO,=AL2(EANOACCS)                                           
         MVC   FVXTRA,SPACES                                                    
         LA    R1,RECBNKH                                                       
         ST    R1,FVADDR                                                        
         B     NXTACCX                                                          
                                                                                
NXTACC14 MVC   ACCOUNT,ACTKCULA                                                 
         GOTO1 VACSRCHC,DMCB,RECBNKH,TWAD,BANKUL,                      X        
               (X'C0',BNKNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
*&&UK                                                                           
         GOTO1 VALBCUR             VALIDATE BANK CURRENCY & SECURITY            
         BH    EXIT                                                             
*&&                                                                             
         MVC   RECBNK(L'ACTKACT),ACTKACT   MOVE OUT BANK ACCOUNT CODE           
         CLC   BANKUL,ACTKUNT      TEST THIS IS THE ACTUAL BANK U/L             
         BE    NXTACC16                                                         
         MVI   RECBNK,C'*'         BUILD NON-STANDARD 'BANK A/C'                
         MVC   RECBNK+1(L'ACTKCULA-1),ACTKUNT  MOVE OUT U/L/ACCOUNT             
                                                                                
NXTACC16 OI    RECBNKH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
         MVC   FVMSGNO,=AL2(IAEPAP1N)      PROCESS A/C OR PF1 NEXT              
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,RECBNKH                                                       
         ST    R1,FVADDR                                                        
                                                                                
NXTACCX  NI    TWAMODE2,255-TWAM2NXA                                            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER SCREEN FIELDS                                       *         
***********************************************************************         
                                                                                
VALHED   XC    FACTOR,FACTOR       NO SUPPLIER TO VALIDATE                      
***********************************************************************         
* VALIDATE BANK ACCOUNT                                               *         
***********************************************************************         
                                                                                
VALBNK   TM    RECBNKH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBNKX                                                          
*&&UK                                                                           
         XC    BANKCUR,BANKCUR                                                  
         NI    SAFCIND1,FF-SAFCI1BC                                             
*&&                                                                             
         CLI   RECBNK,C'*'         TEST NON-STANDARD BANK U/L                   
         BNE   VALBNK6                                                          
         LA    R1,LDGLIST                                                       
VALBNK2  CLI   0(R1),EOT                                                        
         BNE   VALBNK4                                                          
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         LA    R1,RECBNKH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                EXIT WITH ERROR SET                          
VALBNK4  CLC   0(L'LDGLIST,R1),RECBNK+1                                         
         BE    VALBNK6                                                          
         LA    R1,L'LDGLIST(R1)                                                 
         B     VALBNK2                                                          
                                                                                
VALBNK6  MVI   FVMINL,1                                                         
         GOTO1 AVALBNK,RECBNKH                                                  
         BH    EXIT                                                             
         GOTO1 VACSRCHC,DMCB,RECBNKH,TWAD,BANKUL,                      X        
               (X'C0',BNKNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         MVC   ACCNAME,RECNAME     SAVE NAME FOR LATER                          
         ZAP   ACCBAL,RECBAL       SAVE BALANCE FOR LATER                       
         ICM   R1,15,RECABLEL      SET RECONCILED BALANCE BBF                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ZAP   RCNBAL,ABLFRWD-ABLELD(,R1)                                       
         MVC   TWASKEY,ACCOUNT+(ACTKUNT-ACTKEY)                                 
*&&UK                                                                           
         XC    PRSTCURT,PRSTCURT                                                
         TM    RECBNKH+(FVIIND-FVIHDR),FVIVAL                                   
         BO    VALBNKX                                                          
         GOTO1 VALBCUR             VALIDATE BANK CURRENCY & SECURITY            
         BH    EXIT                                                             
*&&                                                                             
VALBNKX  OI    RECBNKH+(FVIIND-FVIHDR),FVIVAL                                   
                                                                                
***********************************************************************         
* VALIDATE BATCH ITEM COUNT                                           *         
***********************************************************************         
                                                                                
*ALITE   TM    RECITEH+(FVATRB-FVIHDR),FVAPROT                                  
*        BO    VALITEX                                                          
*        CLI   PROFBTIC,2          TEST BATCH TOTAL/ITEM COUNT REQ'D            
*        BNE   *+8                                                              
*        MVI   FVMINL,1            SET REQUIRED                                 
*        GOTO1 AVALITE,RECITEH                                                  
*        BH    EXIT                INPUT NOT VALID                              
*ALITEX  DS    0H                                                               
*        EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH TOTAL                                                *         
***********************************************************************         
                                                                                
*ALTOT   XC    BATCTOT,BATCTOT                                                  
*        TM    RECTOTH+(FVATRB-FVIHDR),FVAPROT                                  
*        BO    VALTOTX                                                          
*        OI    RECTOTH+(FVOIND-FVIHDR),FVOXMT                                   
*        CLI   PROFBTIC,2          TEST BATCH TOTAL/ITEM COUNT REQ'D            
*        BNE   *+8                                                              
*        MVI   FVMINL,1            SET REQUIRED                                 
*        GOTO1 AVALAMT,DMCB,RECTOTH,BATCTOT                                     
*        BH    EXIT                INPUT NOT VALID                              
*        BE    VALTOTX                                                          
*        XC    BATCTOT,BATCTOT     NO INPUT - CLEAR ZERO AMOUNT                 
*ALTOTX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE OFFICE                                                     *         
***********************************************************************         
                                                                                
VALOFF   TM    RECOFFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALOFFX                                                          
         XC    RECOFFN,RECOFFN                                                  
         OI    RECOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALOFF,RECOFFH                                                  
         BL    VALOFFX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   RECOFFN,RECNAME                                                  
         OI    RECOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    RCNBAL,RCNBAL       NO RECONCILED BALANCE                        
VALOFFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE SOURCE                                                     *         
***********************************************************************         
                                                                                
VALCON   TM    RECCONH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCONX                                                          
         XC    RECCONN,RECCONN                                                  
         OI    RECCONNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALCON,RECCONH                                                  
         BH    EXIT                                                             
         BL    VALCONX             NOT REQUIRED, NOT INPUT                      
         MVC   RECCONN,RECNAME                                                  
         OI    RECCONNH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    RCNBAL,RCNBAL       NO RECONCILED BALANCE                        
VALCONX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CHEQUE NUMBER RANGE (REFERENCE NUMBER)                     *         
***********************************************************************         
                                                                                
VALREF   TM    RECREFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALREFX                                                          
         GOTO1 AVALREF,RECREFH                                                  
         BH    EXIT                                                             
         BL    VALREFX                                                          
         XC    RCNBAL,RCNBAL       NO RECONCILED BALANCE                        
VALREFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CHEQUE DATE RANGE (PERIOD)                                 *         
***********************************************************************         
                                                                                
VALPER   TM    RECPERH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALPERX                                                          
         GOTO1 AVALPER,RECPERH                                                  
         BH    EXIT                                                             
         BL    VALPERX                                                          
         XC    RCNBAL,RCNBAL       NO RECONCILED BALANCE                        
VALPERX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTIVITY DATE RANGE                                        *         
***********************************************************************         
                                                                                
VALADA   TM    RECADAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALADAX                                                          
         GOTO1 AVALADA,RECADAH                                                  
         BH    EXIT                                                             
         BL    VALADAX                                                          
         XC    RCNBAL,RCNBAL       NO RECONCILED BALANCE                        
VALADAX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CHEQUE MOS RANGE                                           *         
***********************************************************************         
                                                                                
VALMOS   TM    RECMOAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMOSX                                                          
         GOTO1 AVALMOS,RECMOAH                                                  
         BH    EXIT                                                             
         BL    VALMOSX                                                          
         XC    RCNBAL,RCNBAL       NO RECONCILED BALANCE                        
VALMOSX  DS    0H                                                               
                                                                                
                                                                                
*&&UK                                                                           
***********************************************************************         
* VALIDATE CURRENCY FILTER                                            *         
***********************************************************************         
                                                                                
VALCUR   TM    RECCURH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCURX                                                          
         GOTO1 AVALCUR,RECCURH     PUT VALID CURRENCIES INTO TABLE              
         BH    EXIT                                                             
         TM    RECCURH+(FVIIND-FVIHDR),FVIVAL                                   
         BO    VALCURX                                                          
         OI    RECCURH+(FVATRB-FVIHDR),FVAHIGH                                  
         PUSH  USING               TEST UNACCEPTABLE CURRENCY FILTER            
PRESET   USING CURTABD,PRSTCURT                                                 
         CLC   PRESET.CURTCUR,RECCUR                                            
         BE    VALCURX                                                          
         CLC   RECCUR+L'CURTCUR(L'RECCUR-L'CURTCUR),SPACES                      
         BNH   VALCURX                                                          
         MVC   RECCUR,SPACES       UNACCEPTABLE CURRENCY FILTER                 
         OI    RECCURH+(FVOIND-FVIHDR),FVOXMT                                   
         MVI   RECCUR,C'*'                                                      
         MVI   RECCURH+(FVILEN-FVIHDR),1                                        
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BE    *+14                                                             
         MVC   RECCUR(L'CURTCUR),PRESET.CURTCUR                                 
         MVI   RECCURH+(FVILEN-FVIHDR),L'CURTCUR                                
         MVC   FVMSGNO,=AL2(AI$CURFC)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
VALCURX  OI    RECCURH+(FVIIND-FVIHDR),FVIVAL                                   
         CLC   PRESET.CURTCUR,COMPCURT+(CURTCUR-CURTABD)                        
         BE    *+14                                                             
         CLC   PRESET.CURTCUR,SPACES                                            
         BH    *+10                                                             
         MVC   PRSTCURT,COMPCURT                                                
         POP   USING                                                            
*&&                                                                             
***********************************************************************         
* VALIDATE INCLUDE RECONCILED                                         *         
***********************************************************************         
                                                                                
VALREC   TM    RECRECH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALRECX                                                          
         GOTO1 AVALICL,RECRECH                                                  
         BH    EXIT                                                             
VALRECX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE INCLUDE RECEIPTS                                           *         
***********************************************************************         
                                                                                
VALIRC   TM    RECRCPH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALIRCX                                                          
         GOTO1 AVALIRC,RECRCPH                                                  
         BH    EXIT                                                             
VALIRCX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE STATEMENT DATE                                             *         
***********************************************************************         
                                                                                
VALSDT   TM    RECSDTH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSDTX                                                          
         XC    STMNTDT,STMNTDT     CLEAR STATEMENT DATE                         
         CLI   PROFBSDT,C'Y'       TEST STATEMENT DATE COMPULSORY               
         BNE   *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AVALDAT,RECSDTH                                                  
         BH    EXIT                                                             
         BL    VALSDTX                                                          
         CLC   TODAYC,WORK+(PVALCSTA-PERVALD)   CHECK NOT IN FUTURE             
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(EAFUSTDT)                                           
         B     EXIT                                                             
         MVC   STMNTDT,WORK+(PVALCSTA-PERVALD)  SET STATEMENT DATE              
VALSDTX  DS    0H                                                               
                                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
* READ AND FILTER TRANSACTIONS.  PUT QUALIFYING TRANSACTIONS TO TSAR  *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
READTRN  OI    DISIND,DISIOFLO     PRESET OVERFLOW (WHICH IS ALLOWED)           
                                                                                
         LA    R1,TOTALS           CLEAR TOTALS ACCUMULATORS                    
         LA    R0,TOTALSN                                                       
         ZAP   0(L'TOTALS,R1),PZERO                                             
         LA    R1,L'TOTALS(R1)                                                  
         BCT   R0,*-10                                                          
                                                                                
         LA    R2,KEY              BUILD START KEY                              
         GOTO1 SETKEY,SETALL                                                    
         MVI   TSARLEN+1,TSARBRL   SET TSAR RECORD LENGTH                       
         LA    R1,IOHIGH+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
*&&UK                                                                           
         NI    SAFCIND1,FF-SAFCIGBP                                             
         XC    FORECURT,FORECURT   SET UP CURRENCY FIRST TIME THROUGH           
*&&                                                                             
         B     READ04                                                           
                                                                                
READ02   LA    R2,KEY                                                           
         LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
*EAD04   TM    TRNKSTAT,TRNSDELT+TRNSDRFT+TRNSREVS                              
READ04   TM    TRNKSTAT,TRNSDELT+TRNSDRFT                                       
         BNZ   READ02                                                           
         TM    TRNKSTA2,TRNSPEEL+TRNSUSED                                       
         BNZ   READ02                                                           
         CLC   TRNKCULA,ACCOUNT                                                 
         BNE   READTRNX                                                         
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READ06              NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    READ06              YES - OFFICE NOT IN KEY                      
         OC    OFFICE,OFFICE                                                    
         BZ    READ06                                                           
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         BNE   READTRNX                                                         
         CLC   TRNKOFF(0),OFFICE                                                
READ06   OC    CONTRA,CONTRA                                                    
         BZ    READ08                                                           
         IC    RF,CONTRAXL                                                      
         EX    RF,*+8                                                           
         BE    READ08                                                           
         CLC   TRNKCULC(0),CONTRA                                               
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READTRNX            NO - GONE TOO FAR                            
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    READTRNX            YES - GONE TOO FAR                           
         OC    OFFICE,OFFICE       TEST FIXED OFFICE                            
         BNZ   READTRNX            YES - GONE TOO FAR                           
         GOTO1 SETKEY,SETCON+NXTOFF                                             
         B     READ02                                                           
READ08   CLC   TRNKDATE,PERSTA                                                  
         BNL   READ10                                                           
         GOTO1 SETKEY,SETSDT                                                    
         B     READ02                                                           
READ10   CLC   TRNKDATE,PEREND                                                  
         BNH   READ14                                                           
         TM    CONTIND,CONTILOQ    TEST LOW-LEVEL CONTRA FILTER                 
         BNZ   READ12              YES - DON'T SET NEXT CONTRA                  
         GOTO1 SETKEY,SETSDT+NXTCON                                             
         B     READ02                                                           
READ12   TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READTRNX            NO - GONE TOO FAR                            
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    READTRNX            YES - GONE TOO FAR                           
         OC    OFFICE,OFFICE       TEST FIXED OFFICE                            
         BNZ   READTRNX            YES - GONE TOO FAR                           
         GOTO1 SETKEY,SETSDT+NXTOFF                                             
         B     READ02                                                           
READ14   OC    REFSTA,REFSTA                                                    
         BZ    READ16                                                           
         IC    RF,REFSTAXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKREF(0),REFSTA                                                
         BE    READ16                                                           
         BL    *+14                                                             
         OC    REFEND,REFEND                                                    
         BNZ   READ18                                                           
         GOTO1 SETKEY,SETREF                                                    
         B     READ02                                                           
READ16   OC    REFEND,REFEND                                                    
         BZ    READ20                                                           
READ18   IC    RF,REFENDXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKREF(0),REFEND                                                
         BNH   READ20                                                           
         GOTO1 SETKEY,SETREF+NXTSDT                                             
         B     READ02                                                           
READ20   CLC   TRNKSMOS,MOSSTA                                                  
         BL    READ02                                                           
         CLC   TRNKSMOS,MOSEND                                                  
         BH    READ02                                                           
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         TM    TRNKSTAT,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         DROP  R2                                                               
                                                                                
         GOTO1 AGENFILT,AIOBUFF    FILTERS                                      
         BNE   READ02                                                           
         BAS   RE,RFILTER          OVERLAY SPECIFIC FILTERING                   
         BNE   READ02                                                           
                                                                                
         L     R1,AIOSAVE          R1=A(SAVED DIRECTORY VALUES)                 
         MVC   TSARDADR,0(R1)      EXTRACT DATA RECORD DISK ADDRESS             
                                                                                
         USING TRNRECD,R1          R1=A(DATA RECORD KEY)                        
         L     R1,AIOBUFF          EXTRACT TRNKEY VALUES                        
         MVC   TSARCON,TRNKCULC                                                 
         MVC   TSARDAT,TRNKDATE                                                 
         MVC   TSARREF,TRNKREF                                                  
         MVC   TSARSBR,TRNKSBR                                                  
         MVC   TSARMOS,TRNRSMOS                                                 
         MVI   TSARRSTA,0                                                       
         TM    TRNRSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         OI    TSARRSTA,TRNSARCH   SET RECORD IS ON ARCHIVE                     
                                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        EXTRACT TRNEL VALUES                         
         TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BZ    *+10                NO - DON'T SET TSAROFF                       
         MVC   TSAROFF,TRNOFFC                                                  
         MVC   TSARBAT,TRNBTCH                                                  
         MVC   TSARBTY,TRNTYPE                                                  
         MVC   TSARSTA,TRNSTAT                                                  
         XC    TSARVAR,TSARVAR     CLEAR VARIABLE KEY BYTE                      
         ZAP   TSARAMNT,TRNAMNT                                                 
         ZAP   DUB,PZERO                                                        
         TM    TRNSTAT,TRNSBREC                                                 
         BNO   *+14                                                             
         OI    TSARINDS,TSARMKQ+TSARINMQ                                        
         ZAP   DUB,TRNAMNT                                                      
         TM    TRNSTAT,TRNSDR      TEST DEBIT TRANSACTION                       
         BO    *+14                                                             
         MP    DUB,PONENEG         CREDIT REDUCES BALANCE                       
         B     *+12                                                             
         OI    TSARINDS,TSARDRQ                                                 
         MVI   TSARVAR,TRNSDR      SET VARIABLE KEY BYTE FOR SORT SEQ.          
                                                                                
         OC    RCNBAL,RCNBAL       TEST RECONCILED BALANCE IN USE               
         BZ    *+10                                                             
         AP    RCNBAL,DUB          ADJUST RECONCILED BALANCE                    
                                                                                
         USING TRSELD,R2           EXTRACT TRSEL VALUES                         
         ICM   R2,15,ATRSEL                                                     
         MVC   TSARADAT,TRSDATE                                                 
         MVC   TSARSSTA,TRSSTAT                                                 
         MVC   TSARBSDT,TRSBSTDT                                                
*MN                                                                             
         USING GDAELD,R2                                                        
         ICM   R2,15,AGDAERP2                                                   
         BZ    READ20H                                                          
         OC    GDADATE,GDADATE                                                  
         BZ    READ20C                                                          
         GOTO1 VDATCON,DMCB,(1,GDADATE),(0,TSARRCDT)                            
READ20C  OC    GDADATE2,GDADATE2                                                
         BZ    READ20H                                                          
         GOTO1 VDATCON,DMCB,(1,GDADATE2),(0,TSARCLDT)                           
*MN                                                                             
                                                                                
         USING OTHELD,R2           EXTRACT OTHEL VALUES                         
READ20H  ICM   R2,15,AOTHEL                                                     
         BZ    *+10                                                             
         MVC   TSARFOTH,OTHNUM                                                  
                                                                                
         OC    ANOTELS,ANOTELS     TEST MEMO ITEMS ATTACHED                     
         BZ    *+8                                                              
         OI    TSARIND2,TSARMEMO                                                
                                                                                
*&&UK*&& GOTO1 AVAL1FC             VALIDATE ONE FOREIGN CURRENCY                
                                                                                
         GOTO1 ATSARADD            PUT RECORD TO TSAR                           
         BNE   READ22                                                           
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         LA    RF,TOTCRS           RF=A(TOTAL CREDITS)                          
         TM    TRNSTAT,TRNSDR      TEST DEBIT TRANSACTION                       
         BNO   *+8                                                              
         LA    RF,TOTDRS           RF=A(TOTAL DEBITS)                           
         AP    0(L'TOTALS,RF),TRNAMNT                                           
*&&UK                                                                           
         USING AFCELD,R1                                                        
         ICM   R1,15,AAFCEL                                                     
         BZ    *+10                                                             
         AP    CURTOTS-TOTALS(L'TOTALS,RF),AFCAMNT                              
*&&                                                                             
         TM    TRNSTAT,TRNSBREC                                                 
         BNO   READ21                                                           
         LA    RF,L'TOTALS(RF)     RF=A(TOTAL MARKED CRS/DRS)                   
         AP    0(L'TOTALS,RF),TRNAMNT                                           
*&&UK                                                                           
         LTR   R1,R1                                                            
         BZ    *+10                                                             
         AP    CURTOTS-TOTALS(L'TOTALS,RF),AFCAMNT                              
         DROP  R1                                                               
*&&                                                                             
READ21   MVI   ANYADD,1            SET TRANSACTION(S) ADDED                     
         B     READ02              READ SEQUENTIAL                              
         DROP  R2                                                               
                                                                                
READ22   TM    DISIND,DISIOFLO     TEST ERROR WAS OVERFLOW                      
         BNO   EXIT                NO - EXIT WITH ROOT ERROR SET                
         B     DISPTRN             QUIT READ LEAVING OVERFLOW SET               
                                                                                
READTRNX NI    DISIND,255-DISIOFLO  CLEAR OVERFLOW (DID NOT OCCUR)              
         CLI   ANYADD,1            TEST ANYTHING ADDED                          
         BE    DISPTRN                                                          
         LA    R1,RECBNKH          NO - SET CURSOR TO SUPPLIER FIELD            
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IANOTRAN)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
DISPTRN  GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(1,0),MRKOLAYH               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&UK                                                                           
         GOTO1 ABLDCURR            BUILD CURRECNY ENTRIES                       
         BNH   *+6                                                              
         DC    H'0'                BAD CURRECNY IN TABLE                        
         GOTO1 ASETFORE            SET UP SINGLE FOREIGN CURRECNY               
*&&                                                                             
         OI    TWAMODE,TWAMHDRS    INDICATE WE HAVE A HEADER SAVED              
         GOTO1 AOVRSCR,BRSCR2      OVERLAY ACTION INPUT SCREEN                  
         OI    MRKTYPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    MRKACTH+(FVATRB-FVIHDR),FVAPROT                                  
                                                                                
         GOTO1 ABLDBAL,INPACCH     BUILD ACCOUNT CODE/NAME/BALANCE LINE         
                                                                                
         OI    DISIND,DISIRST      SET START FROM BEGINNING                     
         GOTO1 ADISPLAY                                                         
         TM    DISIND,DISIOFLO                                                  
         BNO   *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNWRN)                                           
         GOTO1 ABLDTOT,INPTOTH                                                  
                                                                                
DISPTRNX B     EXIT                                                             
         EJECT                                                                  
         USING DISLINED,R2                                                      
VALINP   CLI   OPTALL,0            TEST GLOBAL MARK/UNMARK                      
         BE    VALINP2                                                          
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BO    VALINP14            YES - CALL DISPLAY                           
         BAS   RE,MRKALL           MARK ALL TRANSACTIONS                        
         OI    TWAMODE2,TWAM2SKP   SET SKIP VALIDATION                          
         OI    DISIND,DISIRST      SET TO RESTART DISPLAY                       
         B     VALINP14                                                         
                                                                                
VALINP2  MVI   ANYMARK,0                                                        
         LA    R3,DISLIST          R3=A(LIST OF TSAR RECDS ON DISPLAY)          
         L     R2,ADISDET1         R2=A(1ST DETAIL LINE)                        
         SR    R0,R0                                                            
         ICM   R0,3,DISLCNT        NUMBER OF DISPLAY LINES                      
         BZ    VALINP14            NO RECORDS TO DISPLAY                        
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BZ    VALINP4                                                          
         NI    TWAMODE2,255-TWAM2SKP  RESET SKIP VALIDATION                     
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BNE   VALINP14            YES - CALL DISPLAY                           
                                                                                
*MN VALINP4  GOTO1 AVALZMRK,DISLHDR2   ZOOM INPUT                               
VALINP4  GOTO1 AVALZMRK,D2SLHDR2   ZOOM INPUT                                   
         BL    VALINP5             NO ZOOM - TRY OTHER MARKS                    
         BH    VALINPX             ZOOM INVALID - EXIT WITH ERROR SET           
         GOTO1 AZOOMFAC,(R3)       PREPARE ZOOM SCREEN AND EXIT TO USER         
         B     EXIT                                                             
                                                                                
*MN VALINP5  GOTO1 AVALMRK,DISLHDR2                                             
VALINP5  GOTO1 AVALMRK,D2SLHDR2                                                 
         BH    VALINPX             EXIT WITH ERROR SET                          
         BL    VALINP12            NO INPUT - NEXT SCREEN LINE                  
                                                                                
         MVC   HALF,0(R3)                                                       
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   BYTE,TSARCHGQ       TEST ONLY WANT TO REDISPLAY                  
         BE    VALINP10                                                         
         CLI   BYTE,TSARMKQ        TEST IF USER IS MARKING                      
         BNE   VALINP6                                                          
         TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BO    VALINP12                                                         
         OI    TSARINDS,TSARMKQ                                                 
*MN                                                                             
         CLI   D2SLDATE-1,X'00'                                                 
         BE    VALINP5B                                                         
         CLI   D2SLDATE-1,C' '                                                  
         BE    VALINP5B                                                         
VALINP5A MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     VALINPX                                                          
                                                                                
VALINP5B CLC   D2SLDATE,SPACES                                                  
         BE    VALINP5C                                                         
         OC    D2SLDATE,D2SLDATE                                                
         BZ    VALINP5C                                                         
         GOTO1 VDATVAL,DMCB,(0,D2SLDATE),(0,WORK)                               
         CLC   WORK(6),=CL6'000000'                                             
         BE    VALINP5A                                                         
         MVC   TSARCLDT,WORK                                                    
VALINP5C GOTO1 VDATCON,DMCB,(5,0),(0,TSARRCDT)                                  
*MN                                                                             
                                                                                
         LA    RF,TOTMCR                                                        
         TM    TSARINDS,TSARDRQ    TEST DEBIT                                   
         BNO   *+8                                                              
         LA    RF,TOTMDR                                                        
         AP    0(L'TOTALS,RF),TSARAMNT  ADD TO MARKED CRS/DRS                   
*&&UK*&& AP    CURTOTS-TOTALS(L'TOTALS,RF),TSARAFCA  ADD TO MARKED(AFC)         
         LA    RF,L'TOTALS(RF)     RF=A(REPORT DR/CR UN/MARKED TOTALS)          
         TM    TSARINDS,TSARINMQ   TEST RECORD MARKED ON FILE                   
*&&US*&& BO    *+14                                                             
*&&UK*&& BO    *+20                                                             
         AP    0(L'TOTALS,RF),TSARAMNT         ADD TO MARKED                    
*&&UK*&& AP    CURTOTS-TOTALS(L'TOTALS,RF),TSARAFCA   (AFC)                     
         B     VALINP8                                                          
         SP    L'TOTALS(L'TOTALS,RF),TSARAMNT  SUBTRACT FROM UNMARKED           
*&&UK*&& SP    (CURTOTS-TOTALS)+L'TOTALS(L'TOTALS,RF),TSARAFCA                  
         B     VALINP8                                                          
                                                                                
VALINP6  TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BZ    VALINP12                                                         
         NI    TSARINDS,255-TSARMKQ                                             
*MN                                                                             
         XC    TSARRCDT,TSARRCDT                                                
         XC    TSARCLDT,TSARCLDT                                                
*MN                                                                             
         LA    RF,TOTMCR                                                        
         TM    TSARINDS,TSARDRQ    TEST DEBIT                                   
         BNO   *+8                                                              
         LA    RF,TOTMDR                                                        
         SP    0(L'TOTALS,RF),TSARAMNT  SUBTRACT FROM MARKED CRS/DRS            
*&&UK*&& SP    CURTOTS-TOTALS(L'TOTALS,RF),TSARAFCA   (AFC)                     
         LA    RF,L'TOTALS(RF)     RF=A(REPORT DR/CR UN/MARKED TOTALS)          
         TM    TSARINDS,TSARINMQ   TEST RECORD MARKED ON FILE                   
*&&US*&& BO    *+14                                                             
*&&UK*&& BO    *+20                                                             
         SP    0(L'TOTALS,RF),TSARAMNT         SUBTRACT FROM MARKED             
*&&UK*&& SP    CURTOTS-TOTALS(L'TOTALS,RF),TSARAFCA                             
         B     VALINP8                                                          
         AP    L'TOTALS(L'TOTALS,RF),TSARAMNT  ADD TO UNMARKED                  
*&&UK*&& AP    (CURTOTS-TOTALS)+L'TOTALS(L'TOTALS,RF),TSARAFCA (AFC)            
         B     VALINP8                                                          
                                                                                
VALINP8  L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ANYMARK,1                                                        
         DROP  RF                                                               
*MN VALINP10 GOTO1 ABLDLIN,DISLHDR1    REBUILD LHS, XMIT, (UN)HIGHLIGHT         
VALINP10 GOTO1 ABLDLIN,D2SLHDR1    REBUILD LHS, XMIT, (UN)HIGHLIGHT             
         CLC   TSARCLDT,SPACES                                                  
         BE    VALINP12                                                         
         OC    TSARCLDT,TSARCLDT                                                
         BZ    VALINP12                                                         
         GOTO1 VDATCON,DMCB,(0,TSARCLDT),(5,D2SLDATE)                           
         OI    D2SLHDR2+(FVOIND-FVIHDR),FVOXMT                                  
*MN                                                                             
                                                                                
VALINP12 LA    R2,DISLINEL(R2)     R2=A(NEXT INPUT LINE)                        
         LA    R3,L'DISLIST(R3)    R3=A(NEXT TSAR RECORD NUMBER)                
         BCT   R0,VALINP4                                                       
                                                                                
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   *+16                                                             
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BE    *+8                                                              
         OI    TWAMODE2,TWAM2SKP   SET SKIP VALIDATION                          
         TM    DISIND,DISINCOL     TEST NEW COLUMN DISPLAY                      
         BNZ   VALINP14            DISPLAY NEW COLUMNS (NO SCROLLING)           
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   VALINP14                                                         
         OI    DISIND,DISIFFLT     FORCE FILTERING OF DISLIST NEXT TIME         
         LA    R1,MRKSCRH          SET CURSOR TO SCROLL FIELD                   
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAMKTEPA)                                           
         TM    DISIND,DISIEOF+DISIBOF                                           
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IAMKTNOM)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     VALINPX                                                          
                                                                                
VALINP14 GOTO1 ADISPLAY                                                         
                                                                                
VALINPX  GOTO1 ABLDTOT,INPTOTH                                                  
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   *+8                                                              
         OI    TWAMODE2,TWAM2CHG   SET CHANGES BIT IN TWA                       
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTIONS                                                 *         
***********************************************************************         
                                                                                
         USING TSARD,RF                                                         
         USING REPD,R3                                                          
UPDATE   CLI   BYTE,ACTICHUP       TEST CHECKING UPDATE IS OK                   
         BNE   UPD02                                                            
                                                                                
         B     EXIT                BATCH CONTROL TOTALS SUSPENDED               
                                                                                
         OC    BATCTOT,BATCTOT     TEST BATCH TOTAL CONTROL PRESENT             
         BZ    UPDCH4                                                           
         MVC   FVMSGNO,=AL2(EABATTOT)                                           
         ZAP   DUB,TOTMDR          TAKE RECONCILED DEBITS                       
         AP    DUB,TOTMCR          ADD RECONCILED CREDITS                       
         CP    BATCTOT,DUB         TEST EQUAL                                   
         BNE   EXIT                                                             
UPDCH4   OC    BATITEM,BATITEM     TEST BATCH ITEM CONTROL PRESENT              
         BNZ   UPDCH6                                                           
         CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
UPDCH6   ICM   R1,3,OVRNUM                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,OVRNUM                                                      
         GOTO1 ATSARGET,OVRNUM                                                  
         BNE   UPDCH8                                                           
         TM    TSARINDS,TSARINMQ   TEST ORIGINALLY RECONCILED                   
         BO    UPDCH6                                                           
         TM    TSARINDS,TSARMKQ    TEST NEWLY RECONCILED                        
         BZ    UPDCH6                                                           
         ICM   R1,3,ITEMS          INCREASE ITEMS MARKED                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,ITEMS                                                       
         B     UPDCH6                                                           
                                                                                
UPDCH8   L     R1,ATSARBLK                                                      
         TM    TSERRS-TSARD(R1),TSEEOF  TEST E-O-F                              
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(EABATITE)                                           
         CLC   ITEMS,BATITEM                                                    
         B     EXIT                EXIT WITH CC EQU/NEQ                         
                                                                                
UPD02    LA    R1,MRKSCRH                                                       
         TM    TWAMODE2,TWAM2CHG   TEST ANY CHANGES                             
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(EANOTHIN)  NOTHING DONE YET                         
         B     UPDATEX2                                                         
                                                                                
         OC    PRTSUB,PRTSUB                                                    
         BZ    UPD04                                                            
         L     R3,AREPWRK          R3=A(REPORT W/S)                             
         GOTO1 APRTINI             INITIALISE AND PRINT FRONT PAGE              
*MN      MVC   REPH5+L'DISLLINE+1(L'LC@RCND),LC@RCND                            
*MN      LA    R1,REPH5+L'DISLLINE+1+L'LC@RCND-1                                
         MVC   REPH5+L'D2SLLINE+1(L'LC@RCND),LC@RCND                            
         LA    R1,REPH5+L'D2SLLINE+1+L'LC@RCND-1                                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'?'                                                       
UPD04    LA    R1,INPMRKH                                                       
         ST    R1,FVADDR                                                        
         LA    R1,1                                                             
         STCM  R1,3,TEMP                                                        
                                                                                
UPD06    GOTO1 ATSARGET,TEMP                                                    
         BE    UPD08                                                            
         L     RF,ATSARBLK                                                      
         TM    TSERRS,TSEEOF                                                    
         BO    UPD24                                                            
         DC    H'0'                                                             
                                                                                
UPD08    TM    TSARINDS,TSARMKQ    TEST IF USER IS MARKING                      
         BZ    UPD10                                                            
         TM    TSARSTA,TRNSBREC    TEST ALREADY RECONCILED                      
         BO    UPD22                                                            
         MVI   NEWSTA,TRNSBREC                                                  
         B     UPD12                                                            
                                                                                
UPD10    TM    TSARSTA,TRNSBREC    TEST ALREADY RECONCILED                      
         BZ    UPD22                                                            
         MVI   NEWSTA,0                                                         
                                                                                
UPD12    OC    PRTSUB,PRTSUB       PRINT REPORT IF REQUIRED                     
         BZ    UPD14               MUST BE LIVE IF NO REPORT                    
         LA    R1,REPP1-1                                                       
         ICM   R1,8,=C'R'                                                       
         GOTO1 ABLDLIN             BUILD PRINT LINE USING REPDISP               
*MN      MVC   REPP1+L'DISLLINE+1(L'LC4YES),LC4YES                              
         MVC   REPP1+L'D2SLLINE+1(L'LC4YES),LC4YES                              
         TM    TSARINDS,TSARMKQ                                                 
         BO    *+10                                                             
*MN      MVC   REPP1+L'DISLLINE+1(L'LC4NO),LC4NO                                
         MVC   REPP1+L'D2SLLINE+1(L'LC4NO),LC4NO                                
         GOTO1 VREPORT,REPD        PRINT IT                                     
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    UPD22               YES - GET NEXT TSAR RECORD                   
                                                                                
UPD14    MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGETRUP+IOACCMST+IO1Q                                        
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF                                                 
         USING TRSELD,R2                                                        
         ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRSSTAT,TSARSSTA    TEST SOMEONE AMENDING ELSEWHERE              
         BNE   UPD16                                                            
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRNSTAT,TSARSTA     TEST SOMEONE AMENDING ELSEWHERE              
         BE    UPD18                                                            
UPD16    NI    TWAMODE2,255-TWAM2CHG  RESET CHANGES MADE BIT                    
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         MVC   FVMSGNO,=AL2(IAUPDCUT)  UPDATE CUT SHORT                         
         LA    R1,MRKACTH                                                       
         OC    PRTSUB,PRTSUB                                                    
         BZ    UPDATEX                                                          
         LA    R1,PARM                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,=AL2(RARAEUCS)                                           
         MVI   GTMTYP,GTMREP       REPORT MESSAGE                               
         MVI   GTMAXL,L'REPP1                                                   
         OI    GT1INDS,GT1NOREF+GT1OWRK                                         
         LA    R0,REPP1                                                         
         STCM  R0,7,GTAOUT                                                      
         GOTO1 VGETTXT                                                          
         GOTO1 VREPORT,REPD                                                     
         GOTO1 APRTCLO                                                          
         MVC   FVMSGNO,=AL2(IAUPCUTR)  UPDATE CUT SHORT, REPORT SPOOLED         
         LA    R1,MRKACTH                                                       
         B     UPDATEX                                                          
         DROP  R1                                                               
                                                                                
UPD18    NI    TRNSTAT,255-TRNSBREC     UNRECONCILE                             
         CLI   NEWSTA,0                 TEST UNRECONCILING                      
         BE    *+8                                                              
         OI    TRNSTAT,TRNSBREC         RECONCILE                               
*MN                                                                             
         USING GDAELD,R2                                                        
UPD19    ICM   R2,15,AGDAERP2                                                   
         BNZ   UPD19B                                                           
*        CLC   TSARCLDT,SPACES          IS THERE A CLEARED DATE                 
*        BNH   UPD19E                   NO EXIT DON'T ADD GDAEL                 
         XC    ELEMT,ELEMT                                                      
         LA    R2,ELEMT                                                         
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALN2Q                                                    
         MVI   GDATYPE,GDATRECN                                                 
         GOTO1 VDATCON,DMCB,(5,0),(1,GDADATE)      RECONCILED DATE              
*                                                                               
         CLC   TSARCLDT,SPACES                ANYTHING IN CLEARED DATE          
         BNH   UPD19A                                                           
         GOTO1 VDATCON,DMCB,(0,TSARCLDT),(1,GDADATE2)                           
*                                                                               
UPD19A   DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'P',=CL8'ACCMST  '),AIOBUFF,GDAELD                 
         B     UPD19E                                                           
*MN                                                                             
                                                                                
UPD19B   XC    GDADATE,GDADATE                                                  
         XC    GDADATE2,GDADATE2                                                
         TM    TSARINDS,TSARMKQ                                                 
         BZ    UPD19E                                                           
         GOTO1 VDATCON,DMCB,(5,0),(1,GDADATE)                                   
         CLC   TSARCLDT,SPACES                                                  
         BNH   UPD19E                                                           
         GOTO1 VDATCON,DMCB,(0,TSARCLDT),(1,GDADATE2)                           
         DROP  R2                                                               
                                                                                
UPD19E   ICM   R2,15,ATRSEL                                                     
         USING TRSELD,R2                                                        
         CLI   TRSLN,TRSLNQ             TEST SHORT ELEMENT                      
         BNL   UPD20                                                            
         GOTO1 AEXTRSL                  EXTEND IT                               
         GOTO1 ASETELAD,AIOBUFF         REFRESH ELEMENT ADDRESSES               
         ICM   R2,15,ATRSEL                                                     
UPD20    MVI   TRSMARK,TRSMBRQ          SET MARKER TYPE/ACTION                  
         MVC   TRSBSTDT,STMNTDT         SET STATEMENT DATE OR 0                 
         CLI   NEWSTA,0                                                         
         BNE   *+14                                                             
         OI    TRSMARK,TRSMUMQ          SET ACTION IS NEGATIVE                  
         XC    TRSBSTDT,TRSBSTDT        CLEAR STATEMENT DATE                    
         L     R2,AIOBUFF               R2=A(DATA RECORD)                       
         USING TRNRECD,R2                                                       
         LA    R1,IOPUT+IOACCMST+IO1Q   PUT BACK TO ACCMST                      
         TM    TRNRSTAT,TRNSARCH        TEST TRANSACTION ON ACCARC              
         BNO   *+12                                                             
         NI    TRNRSTAT,255-TRNSARCH    CLEAR ACCARC INDICATOR                  
         LA    R1,IOADFR+IOACCMST+IO1Q  RE-ADD ACCARC RECORD TO ACCMST          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   KEY(L'TRNKEY),TRNKEY     EXTRACT TRANSACTION KEY                 
         TM    IOCTCOMM,IOADFR          TEST RECORD RE-ADDED TO ACCMST          
         BNO   UPD22                                                            
         LA    R1,IORDUP+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+(TRNKSTA-TRNRECD)(L'TRNRSTA),TRNRSTA                         
         L     R2,AIOSAVE               R2=A(SAVED DATA RECORD VALUES)          
         MVC   KEY+(TRNKDA-TRNRECD)(L'TRNKDA),0(R2)                             
         LA    R1,IOWRITE+IOACCDIR+IO1Q                                         
         GOTO1 AIOEXEC                                                          
         BE    UPD22                                                            
         DC    H'0'                                                             
                                                                                
UPD22    ICM   R1,3,TEMP                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,TEMP                                                        
         B     UPD06                                                            
                                                                                
UPD24    OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    UPD26               NO - MUST BE LIVE UPDATE                     
         GOTO1 APRTCLO             CLOSE REPORT, BUILD SPOOL-ID MESSAGE         
         LA    R1,MRKSCRH                                                       
         CLI   XACTION,ACTDRFT     TEST REPORT WAS DRAFT                        
         BE    UPDATEX             PRTCLO HAS SET MESSAGE                       
                                                                                
UPD26    LA    R1,MRKACTH                                                       
         NI    TWAMODE2,255-TWAM2CHG  RESET CHANGES MADE BIT                    
         MVC   FVMSGNO,=AL2(IATRNUPS)                                           
         OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNUPR)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
                                                                                
UPDATEX  XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
UPDATEX2 ST    R1,FVADDR           STORE FIELD ADDRESS                          
         B     EXIT                                                             
         DROP  R2,R3,RF                                                         
         EJECT                                                                  
***********************************************************************         
* QUIT                                                                *         
***********************************************************************         
                                                                                
QUIT     XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         LA    R1,MRKACTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAHDRECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         NI    TWAMODE2,255-TWAM2CHG                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OVERLAY ROUTINES                                                    *         
***********************************************************************         
                                                                                
***********************************************************************         
* ROUTINE TO RECONCILE/UNRECONCILE ALL TRANSACTIONS                   *         
***********************************************************************         
                                                                                
MRKALL   NTR1  ,                                                                
         LA    R1,1                                                             
MRKALL2  STH   R1,HALF                                                          
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TSARINDS,TSARDISQ   TEST DISPLAY ONLY TRANSACTION                
         BO    MRKALL8                                                          
         GOTO1 AFILTER             FILTER TRANSACTION                           
         BNE   MRKALL8             NOT REQUIRED - GET NEXT                      
         CLC   OPTALL,AC@YES       TEST IF USER IS MARKING                      
         BNE   MRKALL4                                                          
         TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BO    MRKALL8                                                          
         OI    TSARINDS,TSARMKQ                                                 
         LA    RF,TOTMCR                                                        
         TM    TSARINDS,TSARDRQ    TEST DEBIT                                   
         BNO   *+8                                                              
         LA    RF,TOTMDR                                                        
         AP    0(L'TOTALS,RF),TSARAMNT  ADD TO MARKED CRS/DRS                   
*&&UK*&& AP    CURTOTS-TOTALS(L'TOTALS,RF),TSARAFCA                             
         LA    RF,L'TOTALS(RF)     RF=A(REPORT DR/CR UN/MARKED TOTALS)          
         TM    TSARINDS,TSARINMQ   TEST RECORD MARKED ON FILE                   
*&&US*&& BO    *+14                                                             
*&&UK*&& BO    *+20                                                             
         AP    0(L'TOTALS,RF),TSARAMNT         ADD TO MARKED                    
*&&UK*&& AP    CURTOTS-TOTALS(L'TOTALS,RF),TSARAFCA                             
         B     MRKALL6                                                          
         SP    L'TOTALS(L'TOTALS,RF),TSARAMNT  SUBTRACT FROM UNMARKED           
*&&UK*&& SP    (CURTOTS-TOTALS)+L'TOTALS(L'TOTALS,RF),TSARAFCA (AFC)            
         B     MRKALL6                                                          
                                                                                
MRKALL4  TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BZ    MRKALL8                                                          
         NI    TSARINDS,255-TSARMKQ                                             
         LA    RF,TOTMCR                                                        
         TM    TSARINDS,TSARDRQ    TEST DEBIT                                   
         BNO   *+8                                                              
         LA    RF,TOTMDR                                                        
         SP    0(L'TOTALS,RF),TSARAMNT  SUBTRACT FROM MARKED CRS/DRS            
*&&UK*&& SP    CURTOTS-TOTALS(L'TOTALS,RF),TSARAFCA (AFC)                       
         LA    RF,L'TOTALS(RF)     RF=A(REPORT DR/CR UN/MARKED TOTALS)          
         TM    TSARINDS,TSARINMQ   TEST RECORD MARKED ON FILE                   
*&&US*&& BO    *+14                                                             
*&&UK*&& BO    *+20                                                             
         SP    0(L'TOTALS,RF),TSARAMNT         SUBTRACT FROM MARKED             
*&&UK*&& SP    CURTOTS-TOTALS(L'TOTALS,RF),TSARAFCA (AFC)                       
         B     MRKALL6                                                          
         AP    L'TOTALS(L'TOTALS,RF),TSARAMNT  ADD TO UNMARKED                  
*&&UK*&& AP    (CURTOTS-TOTALS)+L'TOTALS(L'TOTALS,RF),TSARAFCA                  
         B     MRKALL6                                                          
                                                                                
MRKALL6  L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TWAMODE2,TWAM2CHG                                                
         B     MRKALL8                                                          
         DROP  RF                                                               
                                                                                
MRKALL8  LH    R1,HALF                                                          
         LA    R1,1(R1)                                                         
         CH    R1,DISMAX                                                        
         BNH   MRKALL2                                                          
                                                                                
MRKALLX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OVERLAY SPECIFIC FILTERING                                          *         
***********************************************************************         
                                                                                
RFILTER  NTR1  ,                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         BZ    RFILTNE                                                          
         GOTO1 ATCLOSE             TEST TRANS IS CLOSED (USING OFFAL)           
         BNE   RFILTNE                                                          
         TM    ICLMARK,ICLMYES     INCLUDE RECONCILED                           
         BNZ   RFILT02                                                          
         LA    RF,X'10'            BO IF EXCLUDING RECONCILED                   
         TM    ICLMARK,ICLMNO                                                   
         BNZ   *+8                                                              
         LA    RF,X'80'            BZ IF RECONCILED ONLY                        
         TM    TRNSTAT,TRNSBREC                                                 
         EX    RF,*+4                                                           
         NOP   RFILTNE             BO OR BZ                                     
                                                                                
RFILT02  OC    OFFICE,OFFICE       TEST OFFICE FILTER SET                       
         BZ    RFILT04                                                          
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         BNE   RFILTNE             WRONG OFFICE                                 
         CLC   TRNOFFC(0),OFFICE                                                
                                                                                
RFILT04  TM    ICLMARK,ICLDRCR     INCLUDE DEBITS & CREDITS?                    
*MN      BNZ   RFILT06                                                          
         BNZ   RFILT05                                                          
         LA    RF,X'10'            BO IF EXCLUDING DEBITS                       
         TM    ICLMARK,ICLDRO      TEST DEBITS (RECEIPTS) ONLY                  
         BNO   *+8                                                              
         LA    RF,X'80'            BZ IF DEBITS (RECEIPTS) ONLY                 
         TM    TRNSTAT,TRNSDR      TEST FOR DEBIT                               
         EX    RF,*+4                                                           
         NOP   RFILTNE             BO OR BZ                                     
                                                                                
*MN                                                                             
         USING GDAELD,R2                                                        
RFILT05  ICM   R2,15,AGDAERP2                                                   
         BZ    RFILT06                                                          
         OC    OPTEREC,OPTEREC                                                  
         BZ    RFILT05A                                                         
         CLC   GDADATE,OPTSREC                                                  
         BL    RFILTNE                                                          
         CLC   GDADATE,OPTEREC                                                  
         BH    RFILTNE                                                          
                                                                                
RFILT05A OC    OPTECLR,OPTECLR                                                  
         BZ    RFILT06                                                          
         CLC   GDADATE2,OPTSCLR                                                 
         BL    RFILTNE                                                          
         CLC   GDADATE2,OPTECLR                                                 
         BH    RFILTNE                                                          
*MN                                                                             
                                                                                
         USING TRSELD,R2                                                        
RFILT06  ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
         CLC   TRSDATE,ADASTA      TEST ACTIVITY DATE                           
         BL    RFILTNE                                                          
         CLC   TRSDATE,ADAEND                                                   
         BH    RFILTNE                                                          
                                                                                
RFILTEQ  CR    RB,RB                                                            
         B     EXIT                                                             
                                                                                
RFILTNE  OC    RCNBAL,RCNBAL       TEST RECONCILED BALANCE IN USE               
         BZ    RFILTNE2                                                         
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         BZ    RFILTNE2                                                         
         TM    TRNSTAT,TRNSBREC    TEST FAILED TRANSACTION RECONCILED           
         BZ    RFILTNE2                                                         
         ZAP   DUB,TRNAMNT                                                      
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+10                                                             
         MP    DUB,PONENEG                                                      
         AP    RCNBAL,DUB          ADJUST RECONCILED BALANCE                    
RFILTNE2 LTR   RB,RB                                                            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A KEY FOR TRANSACTION READING                      *         
*                                                                     *         
* NTRY - R1=KEY BUILD MASK (SEE SETXXX EQUATES)                       *         
*        R2=A(TRANSACTION KEY)                                        *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
SETKEY   NTR1  ,                                                                
         STC   R1,WORK                                                          
                                                                                
         TM    WORK,SETACC         SET ACCOUNT                                  
         BZ    SETKEY0                                                          
         MVC   TRNKEY,SPACES                                                    
         OC    ACCOUNT,ACCOUNT                                                  
         BNZ   *+6                                                              
         DC    H'0'                BANK ACCOUNT MISSING                         
         MVC   TRNKCULA,ACCOUNT                                                 
                                                                                
SETKEY0  TM    WORK,SETOFF         SET OFFICE                                   
         BZ    SETKEY1                                                          
         MVC   TRNKOFF,SPACES                                                   
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    SETKEY1             NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    SETKEY1             YES - OFFICE NOT IN KEY                      
         OC    OFFICE,OFFICE                                                    
         BZ    SETKEY1                                                          
         IC    RE,OFFICEXL                                                      
         EX    RE,*+4                                                           
         MVC   TRNKOFF(0),OFFICE                                                
                                                                                
SETKEY1  TM    WORK,SETCON         SET CONTRA                                   
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,SPACES                                                  
         LA    RF,TRNKCULC         POINT TO COMPANY IN CONTRA A/C               
         CLC   BANKUL,TRNKUNT      TEST ACCOUNT IS IN BANK U/L                  
         BE    *+8                 YES - SET TO SKIP '   ***VOID***'            
         LA    RF,L'TRNKCULC-1(RF) ELSE SET FOR 1ST CONTRA A/C > SPACES         
         MVI   0(RF),X'41'         SET FIRST/LAST BYTE OF TRNKCULC              
         OC    CONTRA,CONTRA                                                    
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,CONTRA                                                  
                                                                                
SETKEY2  TM    WORK,SETSDT         SET TRANSACTION DATE                         
         BZ    SETKEY3                                                          
         MVC   TRNKDATE,PERSTA                                                  
                                                                                
SETKEY3  TM    WORK,SETREF         SET REFERENCE (BILL NUMBER)                  
         BZ    SETKEY4                                                          
         MVC   TRNKREF,SPACES                                                   
         MVI   TRNKREF+L'TRNKREF-1,X'41'                                        
         OC    REFSTA,REFSTA                                                    
         BZ    SETKEY4                                                          
         MVC   TRNKREF,REFSTA                                                   
                                                                                
SETKEY4  TM    WORK,NXTOFF         BUMP OFFICE (NEW OFFICES ONLY)               
         BZ    SETKEY5                                                          
         IC    RE,TRNKOFF+(L'TRNKOFF-1)                                         
         LA    RE,1(RE)                                                         
         STC   RE,TRNKOFF+(L'TRNKOFF-1)                                         
                                                                                
SETKEY5  TM    WORK,NXTCON         BUMP CONTRA                                  
         BZ    SETKEY6                                                          
         IC    RE,TRNKCACT+(L'TRNKCACT-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKCACT+(L'TRNKCACT-1)                                       
                                                                                
SETKEY6  TM    WORK,NXTSDT         BUMP DATE                                    
         BZ    SETKEY7                                                          
         IC    RE,TRNKDATE+(L'TRNKDATE-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKDATE+(L'TRNKDATE-1)                                       
                                                                                
SETKEY7  DS    0H                                                               
                                                                                
SETKEYX  MVI   TRNKSBR,0           ALWAYS CLEAR SUB-REFERENCE                   
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* VALIDATE BANK CURRNCY CODE AND SECURITY                             *         
***********************************************************************         
                                                                                
VALBCUR  NTR1  ,                                                                
         XC    PRSTCURT,PRSTCURT                                                
         MVC   BANKCUR,RECCURR     EXTRACT ACCOUNT CURRENCY                     
         OC    BANKCUR,BANKCUR     TEST BANK ACCOUNT CURRENCY KNOWN             
         BNZ   *+10                                                             
         MVC   BANKCUR,COMPCURT+(CURTCUR-CURTABD)                               
         CLC   BANKCUR,COMPCURT+(CURTCUR-CURTABD)                               
         BE    VALBC02                                                          
                                                                                
         CLI   BANKCUR,ASTCANY     TEST CURRENCY ACCEPTABLE                     
         BE    *+14                                                             
         CLC   BANKCUR,SPACES                                                   
         BNH   VALBC06                                                          
*        TM    COMPSTA6,CPYSFMCR+CPYSFOCR       @@TEMP                          
*        BZ    VALBC04                                                          
         OI    SAFCIND1,SAFCI1BC   SET BANK ACCOUNT IN CURRENCY                 
         MVI   FULL,ACTCURR                                                     
         GOTO1 VSECRET,DMCB,('SECPRACT',ASECBLK),(XTYPE,FULL)                   
         BNE   VALBC04                                                          
                                                                                
VALBC02  MVC   RECCUR,BANKCUR     GIVE CURRENCY SYMBOL                          
         MVI   RECCURH+(FVILEN-FVIHDR),L'BANKCUR                                
         NI    RECCURH+(FVATRB-FVIHDR),FF-FVAHIGH                               
         PUSH  USING                                                            
PRESET   USING CURTABD,PRSTCURT                                                 
         MVC   PRESET.CURTCUR,BANKCUR                                           
         POP   USING                                                            
         B     VALBCURX                                                         
                                                                                
VALBC04  MVC   FVMSGNO,=AL2(AE$SECUB)                                           
         MVI   FVXTRA,C'*'                                                      
         CLI   BANKCUR,ASTCANY                                                  
         BE    VALBCURH                                                         
         MVC   FVXTRA(L'CURTCUR),BANKCUR                                        
         B     VALBCURH                                                         
                                                                                
VALBC06  TM    RECCURH+(FVATRB-FVIHDR),FVAHIGH                                  
         BNZ   VALBCURX                                                         
         XC    RECCUR,RECCUR                                                    
         OI    RECCURH+(FVATRB-FVIHDR),FVAHIGH                                  
                                                                                
VALBCURX OI    RECCURH+(FVOIND-FVIHDR),FVOXMT                                   
         MVI   DUB,0                                                            
         B     *+8                                                              
VALBCURH MVI   DUB,1                                                            
         CLI   DUB,0                                                            
         XIT1  ,                                                                
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* GENERAL EXIT                                                        *         
***********************************************************************         
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
LDGLIST  DS    0CL(L'LEDGER)       VALID LEDGERS FOR 'BANK A/C'                 
         DC    C'SB'               IN CASE THEY PUT *ULACCOUNT                  
         DC    C'SC'               ALLOW THIS, THOUGH IT'S THE DEFAULT          
         EJECT                                                                  
OVRWRKD  DSECT                                                                  
NEWSTA   DS    XL1                 NEW TRANSACTION STATUS                       
OVRNUM   DS    XL2                                                              
ITEMS    DS    XL2                                                              
                                                                                
                                                                                
                                                                                
BNKNDSP  EQU   19                                                               
* ACMRKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMRKWRK                                                       
         PRINT ON                                                               
SAVED    DSECT                                                                  
         ORG   SOVRWRK                                                          
STMNTDT  DS    XL2                                                              
         ORG   TOTALS                                                           
TOTDRS   DS    PL8                                                              
TOTMDR   DS    PL8                                                              
REPMAD   DS    PL8                                                              
REPUMD   DS    PL8                                                              
TOTCRS   DS    PL8                                                              
TOTMCR   DS    PL8                                                              
REPMAC   DS    PL8                                                              
REPUMC   DS    PL8                                                              
*&&UK                                                                           
         ORG   CURTOTS                                                          
CURDRS   DS    PL8                                                              
CURMDR   DS    PL8                                                              
RCPMAD   DS    PL8                                                              
RCPUMD   DS    PL8                                                              
CURCRS   DS    PL8                                                              
CURMCR   DS    PL8                                                              
RCPMAC   DS    PL8                                                              
RCPUMC   DS    PL8                                                              
*&&                                                                             
TWAD     DSECT                                                                  
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKF9D                                                       
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKE9D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053ACMRK09   03/28/14'                                      
         END                                                                    
