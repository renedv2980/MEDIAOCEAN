*          DATA SET ACLFM36    AT LEVEL 033 AS OF 05/01/02                      
*PHASE T60336A,+0                                                               
*INCLUDE ACDELEL                                                                
         TITLE 'MAINTAIN ACCOUNT CONVERSIONS'                                   
T60336   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**LFM36*,R9,RR=R5,CLEAR=YES                            
         LR    R8,RC                                                            
         USING LWSD,R8                                                          
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         ST    R5,PRELOC                                                        
         ST    RB,SAVRB                                                         
                                                                                
         USING COMFACSD,R5                                                      
         L     R5,COMFACS                                                       
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        IS THIS FIRST TIME IN                                    *             
*        BUILD KEY FOR COMPANY/UNIT/LEDGER/ACCOUNT                *             
*        - DISPLAY NAMES OF ABOVE                                 *             
*-----------------------------------------------------------------*             
         MVI   ERROR,X'FF'                                                      
         CLI   MODE,BUILDKEY       IS THIS A NEW RECORD?                        
         BNE   DSP10               NO                                           
         MVC   KEY,SPACES          ZERO KEY                                     
         MVC   KEY(1),COMPANY      ADD COMPANY TO KEY                           
         LA    R2,LOGUNITH         R2 = UNIT HEADER                             
         GOTO1 ANY                 LENGTH RETURNED IN R1                        
         MVC   KEY+1(1),LOGUNIT                                                 
         TM    4(R2),X'20'         WAS PREVALID BIT SET?                        
         BO    BLD10               YES                                          
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         NI    LOGLEDGH+4,X'FF'-X'20'   UNVALIDATE TO FORCE REVALIDATE          
         NI    LOGSACCH+4,X'FF'-X'20'   UNVALIDATE FOR DISPLAY MODE             
         FOUT  LOGUNAMH,SPACES,36  SET TRANSBIT FOR UNIT NAME                   
         FOUT  LOGLNAMH,SPACES,36  SET TRANSBIT FOR LEDGER NAME                 
         MVI   KEY+1,C' '          READ COMPANY RECORD AND SAVE                 
         GOTO1 READ                OFF COMPANY ELEMENT                          
         BAS   RE,SAVEM                                                         
         USING CPYELD,R4                                                        
         LA    R4,SVCOMPEL                                                      
         CLI   CPYLN,CPYLN3Q       IF LOW NOT SET UP TO CONVERT                 
         BL    CONERR                                                           
         CLI   CPYTCMP,0           IF ZERO NOT SET UP TO CONVERT                
         BNH   CONERR                                                           
         MVC   CONVCMP,CPYTCMP     SAVE CONVERSION ID                           
         MVC   KEY+1(1),LOGUNIT    ADD UNIT TO KEY                              
         MVC   FRUNIT,LOGUNIT      SAVE UNIT OFF                                
         GOTO1 READ                READ UNIT RECORD                             
         GOTO1 NAMOUT              DISPLAY UNIT NAME                            
                                                                                
         MVC   MYKEY,SPACES        VALIDATE THE SAME UNIT EXISTS ON             
         MVC   MYKEY(1),CONVCMP    TEST ID                                      
         MVC   MYKEY+1(1),LOGUNIT                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYKEY,IO                     
         CLI   8(R1),0                                                          
         BNE   NOTONTST            IS MISSING PUT OUT ERROR MSG                 
                                                                                
         OI    4(R2),X'20'         SET PREVALID BIT                             
         MVI   ANYKEY,C'Y'         SET KEY CHANGE                               
                                                                                
BLD10    LA    R2,LOGLEDGH         R2 = LEDGER HEADER                           
         GOTO1 ANY                 LENGTH RETURNED IN R1                        
         MVC   KEY+2(1),LOGLEDG    ADD LEDGER TO KEY                            
         MVC   FRLEDG,LOGLEDG      SAVE OF LEDGER                               
         TM    4(R2),X'20'         WAS PREVALID BIT SET                         
         BO    BLD25               YES                                          
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         NI    LOGSACCH+4,X'FF'-X'20'    UNVALIDATE FOR DISPLAY MODE            
         FOUT  LOGLNAMH,SPACES,36  SET TRANSBIT FOR LED NAME                    
         GOTO1 READ                READ LEDGER RECORD                           
         GOTO1 NAMOUT              DISPLAY LEDGER NAME                          
                                                                                
         MVC   MYKEY,SPACES        VALIDATE THE SAME LEDGER EXISTS ON           
         MVC   MYKEY(1),CONVCMP    TEST ID                                      
         MVC   MYKEY+1(1),LOGUNIT                                               
         MVC   MYKEY+2(1),LOGLEDG                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYKEY,IO                     
         CLI   8(R1),0             IF NOT FOUND PUT OUT ERROR MSG               
         BNE   NOTONTST                                                         
                                                                                
         OI    4(R2),X'20'         SET PREVALID BIT FOR LEDGER                  
         MVI   ANYKEY,C'Y'                                                      
                                                                                
         LA    R4,IO               IF LEDGER DOES EXIST ON TEST ID              
         AH    R4,DATADISP         SAVE HEIRARCHY ELEMENT FOR LATER             
BLD15    CLI   0(R4),0             VALIDATION                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),ACLELQ                                                     
         BE    BLD18                                                            
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     BLD15                                                            
                                                                                
         USING ACLELD,R4                                                        
BLD18    MVC   TSIDHEIR,0(R4)      SAVE HEIRARCHY ELEMENT OFF THIS              
         MVI   MINLEN,0            UNIT AND LEDGER ON TEST ID - AND             
         LA    R5,ACLVALS          CALCULATE THE MINIMUM LENGTH                 
BLD19    CLI   0(R5),12            ACCOUNT INPUT MUST BE IF LEDGER IS           
         BE    BLD20               DEFAULT                                      
         MVC   MINLEN,0(R5)                                                     
         LA    R5,16(R5)                                                        
         B     BLD19                                                            
BLD20    ZIC   R5,MINLEN                                                        
         LA    R5,1(R5)                                                         
         STC   R5,MINLEN                                                        
                                                                                
BLD25    LA    R2,LOGUNITH         SET CURSOR POSITION                          
BLD99    B     XXIT                                                             
                                                                                
                                                                                
NOTONTST NI    LOGSACCH+4,X'FF'-X'20'  UNVALIDATE SO I WILL REDISPLAY           
         MVC   LOGHEAD,SPACES          CLEAR                                    
         MVI   ERROR,X'FE'             SET SPECIAL MESSAGE                      
         MVC   LOGHEAD(32),=C'UNIT/LEDGER NOT ON CONVERSION ID'                 
         OI    LOGHEADH+6,X'80'        TRANSMIT                                 
         B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        DISPLAY MODE                                                           
*-----------------------------------------------------------------*             
DSP10    CLI   MODE,DSPLYREC       ARE WE IN DISPLAY MODE?                      
         BNE   CHA10               NO                                           
         LA    R2,LOGTULH          R2 = ACCOUNT HEADER                          
         CLI   LOGACT,C'A'                                                      
         BE    *+12                                                             
         BAS   RE,PRTECT           PROTECT ALL FIELDS                           
         NI    LOGSACCH+4,X'FF'-X'20'                                           
         LA    R2,LOGSACCH         R2 = ACCOUNT HEADER                          
                                                                                
         CLI   PFKEY,0             IF NO PFKEY CHECK TO SEE IF STARTING         
         BE    DSP50               ACCOUNT HAS CHANGED                          
         CLI   PFKEY,5             PF5=DISPLAY UP                               
         BNE   DSP30                                                            
         NI    DISSW,X'FF'-DISDONE                                              
         CLI   SCRNUM,1            IF DISPLAYING FIRST KEY IN TABLE             
         BE    XXIT                CAN'T GO BACK ANY FURTHER                    
         LA    R5,NKEYTAB          ELSE FIND THIS ENTRY IN TABLE                
         LA    R4,KEYBLOCK                                                      
DSP12    CLC   SCRNUM,0(R4)        MATCH ON SCREEN NUMBER                       
         BE    DSP15                                                            
         LA    R4,LKEYTAB(R4)      CHECK NEXT TABLE ENTRY                       
         BCT   R5,DSP12                                                         
         DC    H'0'                HAS TO EXIST IN TABLE                        
DSP15    LA    R5,LKEYTAB          BUMP BACK ONE IN TABLE ENTRY AND             
         SR    R4,R5               GET THIS KEY TO BEGIN DISPLAY WITH           
         MVC   MYKEY(1),COMPANY    SET KEY FOR NEW DISPLAY                      
         MVC   MYKEY+1(14),1(R4)                                                
         MVI   MYKEY+15,X'FF'                                                   
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         BAS   RE,DSPSCRN          DISPLAY NEW SCREEN                           
         ZIC   R4,SCRNUM           RESET CURRENT SCREEN DISPLAY NUMBER          
         BCTR  R4,0                                                             
         STC   R4,SCRNUM           STORE FOR NEXT PASS                          
         B     XXIT                                                             
                                                                                
DSP30    CLI   PFKEY,6             DISPLAY DOWN ONE SCREEN                      
         BNE   DSP40                                                            
         TM    DISSW,DISDONE       IF I'VE HIT END OF UNIT/LEDGER               
         BO    DSPXIT              CAN'T GO ANY FURTHER                         
         CLI   SCRNUM,10           IF 10 IS LAST POSITION IN TABLE              
         BL    DSP35               SO NEED TO SHIFT KEYS BACK ONE SLOT          
         BAS   RE,RETAB            IN TABLE                                     
         LA    R4,KEYLAST          ADDRESS FOR TABLE                            
         B     DSP38                                                            
                                                                                
DSP35    LA    R4,KEYBLOCK         ELSE BUMP TO NEXT SLOT IN TABLE              
         LA    R5,NKEYTAB                                                       
DSP36    CLC   SCRNUM,0(R4)        FIND MATCH ON SCREEN NUMBER                  
         BE    DSP37                                                            
         LA    R4,LKEYTAB(R4)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R5,DSP36            AND CHECK AGAIN                              
         DC    H'0'                                                             
DSP37    LA    R4,LKEYTAB(R4)      BUMP TO NEXT SCREEN ENTRY IN TABLE           
         CLC   1(14,R4),SPACES     HAVE I ALREADY SET A KEY HERE                
         BH    DSP39                                                            
DSP38    MVC   1(14,R4),SVMYKEY+1  MOVE IN LAST KEY READ                        
DSP39    MVC   MYKEY,SVMYKEY       SET KEY TO START READING AT                  
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         BAS   RE,DSPSCRN          DISPLAY SCREEN                               
         CLI   SCRNUM,10           IF SCREEN NUMBER IS ALREADY MAX              
         BE    XXIT                DO NOT BUMP                                  
         ZIC   R4,SCRNUM           ELSE BUMP SCREEN NUMBER AND SAVE             
         LA    R4,1(R4)            FOR USE ON NEXT PASS                         
         STC   R4,SCRNUM                                                        
         B     XXIT                                                             
                                                                                
DSP40    DS    0H                                                               
         CLI   PFKEY,7                                                          
         BNE   XXIT                                                             
         CLI   LOGACT,C'A'                                                      
         BNE   XXIT                                                             
         B     CHA100                                                           
                                                                                
DSP50    TM    LOGSACCH+4,X'20'    VALIDATED PREVIOUSLY?                        
         BO    XXIT                                                             
         MVC   MYKEY,KEY           MOVE IN COMP/UNIT/LEDGER                     
         MVC   MYKEY+3(12),LOGSACC   AND ACCOUNT FROM SCREEN IN ANY             
         OC    MYKEY+1(L'MYKEY-1),SPACES                                        
         BAS   RE,CLRTAB           CLEAR KEY TABLE                              
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         MVI   SCRNUM,1            INITIALIZE SCREEN NUMBER                     
         LA    R4,KEYBLOCK                                                      
         MVC   1(14,R4),MYKEY+1    SET KEY IN KEY TABLE                         
         BAS   RE,DSPSCRN          DISPLAY SCREEN                               
         OI    LOGSACCH+4,X'20'    VALIDATED PREVIOUSLY                         
DSPXIT   B     XXIT                                                             
                                                                                
CONERR   MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(28),=C'AGENCY NOT SET UP TO CONVERT'                     
         OI    LOGHEADH+6,X'80'                                                 
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGUNITH                                                      
         B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        RECORD CHANGE MODE                                                     
*-----------------------------------------------------------------*             
CHA10    DS    0H                                                               
         BAS   RE,UNPRTECT                                                      
                                                                                
CHA11    CLI   PFKEY,0             IF NO PFKEY CHECK TO SEE IF STARTING         
         BE    CHA100              ACCOUNT HAS CHANGED                          
         CLI   PFKEY,5             PF5=DISPLAY UP                               
         BNE   CHA30                                                            
         BAS   RE,ANYCHA                                                        
         TM    DISSW,DISCHA                                                     
         BO    CHAERR                                                           
         NI    DISSW,X'FF'-DISDONE                                              
         CLI   SCRNUM,1            LAST ADDRESS IN KEY TABLE PROCESSED          
         BE    CHAXIT                                                           
         LA    R5,NKEYTAB                                                       
         LA    R4,KEYBLOCK                                                      
CHA14    CLC   SCRNUM,0(R4)                                                     
         BE    CHA15                                                            
         LA    R4,LKEYTAB(R4)                                                   
         BCT   R5,CHA14                                                         
         DC    H'0'                                                             
CHA15    LA    R5,LKEYTAB          BUMP BACK ONE IN TABLE AND REDISPLAY         
         SR    R4,R5                                                            
         MVC   MYKEY(1),COMPANY    SET KEY FOR NEW DISPLAY                      
         MVC   MYKEY+1(14),1(R4)                                                
         MVI   MYKEY+15,X'FF'                                                   
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         BAS   RE,DSPSCRN          DISPLAY NEW SCREEN                           
         ZIC   R4,SCRNUM                                                        
         BCTR  R4,0                                                             
         STC   R4,SCRNUM                                                        
         B     CHAXIT                                                           
                                                                                
CHA30    CLI   PFKEY,6             DISPLAY DOWN ONE SCREEN                      
         BNE   CHA100                                                           
         BAS   RE,ANYCHA                                                        
         TM    DISSW,DISCHA                                                     
         BO    CHAERR                                                           
         TM    DISSW,DISDONE                                                    
         BO    CHAXIT                                                           
         CLI   SCRNUM,10           IF 10 IS LAST POSITION IN TABLE              
         BL    CHA35               SO NEED TO SHIFT KEYS BACK ONE SLOT          
         BAS   RE,RETAB            IN TABLE                                     
         LA    R4,KEYLAST          ADDRESS FOR TABLE                            
         B     CHA38                                                            
                                                                                
CHA35    LA    R4,KEYBLOCK         ELSE BUMP TO NEXT SLOT IN TABLE              
         LA    R5,NKEYTAB                                                       
CHA36    CLC   SCRNUM,0(R4)                                                     
         BE    CHA37                                                            
         LA    R4,LKEYTAB(R4)                                                   
         BCT   R5,CHA36                                                         
         DC    H'0'                                                             
CHA37    LA    R4,LKEYTAB(R4)                                                   
         CLC   1(14,R4),SPACES                                                  
         BH    CHA39                                                            
CHA38    MVC   1(14,R4),SVMYKEY+1  MOVE IN LAST KEY READ AND START              
CHA39    MVC   MYKEY,SVMYKEY       ADDRESS IN KEY TABLE                         
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         BAS   RE,DSPSCRN          DISPLAY SCREEN                               
         CLI   SCRNUM,10                                                        
         BE    CHAXIT                                                           
         ZIC   R4,SCRNUM                                                        
         LA    R4,1(R4)                                                         
         STC   R4,SCRNUM                                                        
         B     CHAXIT                                                           
                                                                                
CHA100   CLI   PFKEY,7             IF ACTION IS AMEND AND PFKEY=7               
         BNE   CHA110              UPDATE THIS SCREEN                           
         BAS   RE,UPDSCRN                                                       
         NI    DISSW,X'FF'-DISCHA                                               
         B     XXIT                                                             
                                                                                
         USING SCRND,R3                                                         
CHA110   LA    R3,LOGFACCH                                                      
         LA    R5,SCRNTRY                                                       
CHA120   TM    SCRULH+4,X'80'      IF THEY MAKE ANY SCREEN CHANGES              
         BZ    *+8                 KEEP TRACK OF IT - WILL NOT LET              
         OI    DISSW,DISCHA        THEM MOVE DISPLAY UNLESS THEY                
         TM    SCRNACCH+4,X'80'    UPDATE OR DECIDE NOT TO UPDATE               
         BZ    *+8                                                              
         OI    DISSW,DISCHA                                                     
         TM    SCRNAMEH+4,X'80'                                                 
         BZ    *+8                                                              
         OI    DISSW,DISCHA                                                     
         LA    R3,SCRLLEN(R3)                                                   
         BCT   R5,CHA120                                                        
                                                                                
CHAXIT   MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(10),=C'ENTER NEXT'                                       
         OI    LOGHEADH+6,X'80'                                                 
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGTULH                                                       
         B     XXIT                                                             
                                                                                
CHAERR   DS    0H                                                               
         TM    DISSW,DISWARN1                                                   
         BZ    CHAER10                                                          
         NI    DISSW,X'FF'-DISCHA                                               
         NI    DISSW,X'FF'-DISWARN1                                             
         B     CHA11                                                            
CHAER10  MVC   LOGHEAD,SPACES                                                   
         MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(38),=C'SCREEN HAS CHANGED - TO UPDATE HIT PF7'           
         LA    R2,LOGUNITH                                                      
         OI    LOGHEADH+6,X'80'                                                 
         OI    DISSW,DISWARN1                                                   
         B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        DISPLAY SCREEN ENTRY - MYKEY MUST BE SET TO ACCOUNT                    
*        YOU WANT SCREEN DISPLAY TO BEGIN AT                                    
*-----------------------------------------------------------------*             
DSPSCRN  NTR1                                                                   
         LA    R4,SCRNTRY          NUMBER OF LINES PER SCREEN                   
         LA    R3,LOGFACCH         START OF DISPLAY                             
         ST    R3,CURSCRN          CURRENT SCREEN ADDRESS                       
DSCR10   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',MYKEY,IO                     
         CLI   8(R1),0             READ FROM KEY SENT                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   MYKEY(3),IO         HAS COMP/UNIT/LEDGER CHANGED                 
         BE    DSCR13              IF YES EXIT                                  
         OI    DISSW,DISDONE                                                    
         B     DSCRXIT                                                          
                                                                                
DSCR13   LA    R3,IO                                                            
         AH    R3,DATADISP                                                      
DSCR15   CLI   0(R3),0             IF END OF RECORD MOVE HIGH VALUES            
         BE    DSCR20              AFTER ACCOUNT AND READHI AGAIN               
         CLI   0(R3),ABLELQ        IF THERE IS NO BALANCE ELEMENT I'M           
         BE    DSCR40              NOT ON A LOW LEVEL ACCOUNT                   
         ZIC   R5,1(R3)                                                         
         AR    R3,R5                                                            
         B     DSCR15                                                           
DSCR20   MVC   MYKEY,IO                                                         
         MVI   MYKEY+15,X'FF'                                                   
         B     DSCR10                                                           
                                                                                
DSCR40   L     R3,CURSCRN          SET R3=CURRENT LINE ON SCREEN                
         BAS   RE,DLINE            DISPLAY THIS LINE OF INFORMATION             
         LA    R3,SCRLLEN(R3)      BUMP TO NEXT LINE ON SCREEN                  
         ST    R3,CURSCRN          SAVE THIS ADDRESS                            
         MVC   MYKEY,IO            MOVE HIGH VALUES AFTER ACCOUNT IN            
         MVI   MYKEY+15,X'FF'      KEY AND DO READ FOR NEXT LOW LEVEL           
         MVC   SVMYKEY,MYKEY       ACCOUNT - SAVE THIS KEY -                    
         BCT   R4,DSCR10           PROCESS NEXT                                 
                                                                                
DSCRXIT  B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        DISPLAY SCREEN ENTRY                                                   
*-----------------------------------------------------------------*             
DLINE    NTR1                                                                   
         USING SCRND,R3            DSECT FOR ONE LINE OF SCREEN ENTRY           
         L     R3,CURSCRN          CURRENT DISPLAY ADDRESS                      
         USING ACTRECD,R4                                                       
         LA    R4,IO               ACCOUNT RECORD READ                          
         MVC   SCRACC,ACTKACT      MOVE EXISTING ACCOUNT TO SCREEN              
         OI    SCRACCH+6,X'80'     TRANSMIT                                     
         AH    R4,DATADISP                                                      
                                                                                
DLINE15  CLI   0(R4),0             IF END OF RECORD NO ELEMENT                  
         BE    DLINXIT             SO NOTHING LEFT TO DISPLAY                   
         CLI   0(R4),NAMELQ                                                     
         BNE   DLINE16                                                          
         USING NAMELD,R4                                                        
         MVC   SCRNAME,SPACES                                                   
         ZIC   R5,1(R4)            SET NAME ON SCREEN TO A DEFAULT              
         SH    R5,=H'3'            OF OLD ACCOUNT NAME                          
         EX    R5,*+8                                                           
         B     DLINE18                                                          
         MVC   SCRNAME(0),NAMEREC                                               
         DROP  R4                                                               
         USING FFTELD,R4           DOES 'DB' EL ALREADY EXIST                   
DLINE16  CLI   0(R4),FFTELQ                                                     
         BNE   DLINE18                                                          
         CLI   FFTTYPE,FFTTCONV    IS IT CONVERSION TYPE                        
         BE    DLINE20                                                          
DLINE18  ZIC   R5,1(R4)            IF NOT GET NEXT ELEMENT                      
         AR    R4,R5                                                            
         B     DLINE15                                                          
                                                                                
DLINE20  DS    0H                                                               
         CLC   FROMUL,FFTDATA      IF UL ON THIS EL IS SAME AS THE ONE          
         BE    DLINE21             I'M LISTING FROM DO NOT DISPLAY              
         MVC   SCRUL,FFTDATA                                                    
         OI    SCRULH+6,X'80'                                                   
DLINE21  MVC   SCRNACC,FFTDATA+2   MOVE THIS CONVERSION ACC TO SCREEN           
         OI    SCRNACCH+6,X'80'    TRANSMIT                                     
         MVC   MYKEY2,SPACES       READ THIS CONVERSION ACCOUNT                 
         MVC   MYKEY2+1(14),FFTDATA                                             
         OC    MYKEY2,SPACES                                                    
         MVC   MYKEY2(1),CONVCMP                                                
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=C'ACCOUNT',MYKEY2,IO2                 
         CLI   8(R1),0                                                          
         BNE   TACCERR                                                          
                                                                                
         USING ACTRECD,R4                                                       
         LA    R4,IO2              FIND NAME ELEMENT                            
         AH    R4,DATADISP                                                      
DLINE25  CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),NAMELQ                                                     
         BE    DLINE30                                                          
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     DLINE25                                                          
                                                                                
         USING NAMELD,R4                                                        
DLINE30  DS    0H                                                               
         L     R3,CURSCRN          DISPLAY NEW NAME                             
         MVC   SCRNAME,SPACES                                                   
         ZIC   R5,1(R4)                                                         
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   SCRNAME(0),NAMEREC                                               
         OI    SCRNAMEH+6,X'80'                                                 
                                                                                
DLINXIT  B     XXIT                                                             
                                                                                
TACCERR  MVC   LOGHEAD,SPACES                                                   
         MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(29),=C'ACCOUNT NOT SET UP ON TEST ID'                    
         OI    LOGHEADH+6,X'80'                                                 
         LA    R2,SCRNACCH                                                      
         B     XXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        UPDATE THIS SCREEN                                                     
*-----------------------------------------------------------------*             
UPDSCRN  NTR1                                                                   
         MVI   ERROR,X'FF'       INITIALIZE ERROR TO NO ERROR                   
         USING SCRND,R3                                                         
UPD001   LA    R3,LOGFACCH       START AT FIRST ENTRY ON SCREEN                 
         LA    R6,SCRNTRY        NUMBER OF ENTRIES PER SCREEN                   
UPD002   LA    R2,SCRULH                                                        
         CLI   SCRULH+5,0        LOOP THROUGH SCREEN AND MAKE SURE              
         BE    UPD003            NO INPUT FIELDS ARE MISSING                    
         LA    R2,SCRNACCH                                                      
         CLI   SCRNACCH+5,0                                                     
         BE    UPD006                                                           
UPD003   LA    R3,SCRLLEN(R3)                                                   
         BCT   R6,UPD002                                                        
         B     UPD007                                                           
UPD006   MVI   ERROR,MISSING                                                    
         B     XXIT                                                             
                                                                                
UPD007   LA    R3,LOGFACCH       START AT FIRST ENTRY ON SCREEN                 
         LA    R6,SCRNTRY        NUMBER OF ENTRIES PER SCREEN                   
         MVC   SVACCREM,SPACES                                                  
         MVC   SVACCLNK,SPACES                                                  
UPD008   MVC   MYKEY2,SPACES            CLEAR KEY                               
         MVC   MYKEY2(1),COMPANY        BUILD EXISTING ACCOUNT KEY              
         MVC   MYKEY2+1(1),LOGUNIT      FROM SCREEN INFO AND READ               
         MVC   MYKEY2+2(1),LOGLEDG                                              
         MVC   MYKEY2+3(12),SCRACC                                              
         OC    MYKEY2+3(12),SPACES                                              
         OI    DMINBTS,X'80'                                                    
         MVC   COMMAND,=C'DMREAD  '                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',MYKEY2,IO2            
         CLI   8(R1),0           IF NOT FOUND THERE IS A PROBLEM                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
UPD010   CLI   0(R4),0                                                          
         BE    UPD020                                                           
         CLI   0(R4),NAMELQ      FIND NAME ELEMENT AND SAVE IT OFF              
         BNE   UPD013            FOR POSSIBLE USE LATER WHEN ADDING             
         MVC   SVNAMEL,SPACES    NEW ACCOUNT ON TEST ID                         
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     UPD015                                                           
         MVC   SVNAMEL(0),0(R4)                                                 
                                                                                
         USING FFTELD,R4            DOES 'DB' ELEMENT EXIST ALREADY             
UPD013   CLI   0(R4),FFTELQ                                                     
         BNE   UPD015                                                           
         CLI   FFTTYPE,FFTTCONV     IS TYPE OF ELEMENT CONVERSION               
         BE    UPD018                                                           
UPD015   ZIC   R5,1(R4)             GET THE NEXT ELEMENT                        
         AR    R4,R5                                                            
         B     UPD010                                                           
                                                                                
UPD018   DS    0H                                                               
         MVC   SVACCREM,FFTDATA                                                 
         CLC   SCRNACC,SPACES          IF ELEMENT EXISTS ON ORIGINAL            
         BH    UPD035                  ACCOUNT, BUT FIELD ON SCREEN IS          
         MVI   FFTEL,X'FF'             BLANK - DELETE ELEMENT AND               
         GOTO1 REMANEL,DMCB,(X'FF',0)  UPDATE ACCOUNT                           
         SR    R5,R5                                                            
         LA    RF,IO2                                                           
         ICM   R5,3,42(RF)                                                      
         SH    R5,=H'19'                                                        
         STCM  R5,3,42(RF)                                                      
         DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=CL8'DMWRT',=CL8'ACCOUNT',IO2,IO2                   
         CLI   8(R1),0                                                          
         BE    UPD300                                                           
         DC    H'0'                                                             
                                                                                
UPD020   DS    0H                                                               
         CLC   SCRNACC,SPACES       NO ELEMENT FOUND AND NO ACCOUNT             
         BNH   UPD300               ENTERED ON SCREEN - THEN THERE IS           
         XC    ELEMENT,ELEMENT      NOTHING TO UPDATE - ELSE                    
         USING FFTELD,R4            DOES 'DB' ELEMENT EXIST ALREADY             
         LA    R4,ELEMENT           BUILD NEW ELEMENT SHELL AND ADD             
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,19                                                         
         MVI   FFTTYPE,FFTTCONV                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,14                                                       
         GOTO1 ADDANEL              ADD SHELL ELEMENT                           
         LA    R4,IO2               AND GET ADDRESSABILITY TO IT FOR            
         AH    R4,DATADISP          FURTHER PROCESSING                          
UPD022   CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),FFTELQ                                                     
         BNE   UPD025                                                           
         CLI   FFTTYPE,FFTTCONV                                                 
         BE    UPD035                                                           
UPD025   ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     UPD022                                                           
                                                                                
UPD035   DS    0H                                                               
         MVC   THISMIN,MINLEN         SET DEFAULT TO HEADER UNIT/LEDG           
         MVC   FFTDATA(1),LOGUNIT     MOVE IN ORIGINAL UNIT AND                 
         MVC   FFTDATA+1(1),LOGLEDG   LEDGER                                    
         CLC   SCRUL,SPACES           IF SOMETHING IN THIS FIELD                
         BNH   UPD040                 THEN I'M CHANGING UNIT/LEDGER             
         CLC   SCRUL,FROMUL           IF IT'S THE SAME AS ORIGINAL NO           
         BE    UPD040                 NEED TO VALIDATE (DONE IN BLDKEY)         
         CLI   SCRULH+5,2             MUST BE 2 CHARACTERS                      
         BE    UPD037                                                           
         MVI   ERROR,INVALID                                                    
         LA    R2,SCRULH                                                        
         B     XXIT                                                             
UPD037   MVC   MYKEY,SPACES           READ TO MAKE SURE U/L EXISTS ON           
         MVC   MYKEY(1),CONVCMP       TEST ID                                   
         MVC   MYKEY+1(2),SCRUL                                                 
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYKEY,IO                     
         LA    R2,SCRULH                                                        
         CLI   8(R1),0                                                          
         BNE   ULERR                                                            
         BAS   RE,CHKMIN              GET MIN LEN ON THIS LEDGER                
                                                                                
         MVC   FFTDATA(2),SCRUL       MOVE U/L AND ACCOUNT INTO                 
UPD040   MVC   FFTDATA+2(12),SCRNACC  NEW ELEMENT                               
         OC    FFTDATA(14),SPACES     FILL TRAILING 0'S WITH SPACES             
         CLI   SCRNACC,C'"'                                                     
         BNE   UPD042                                                           
         MVC   FFTDATA(14),SVLGACC                                              
         OC    FFTDATA(14),SPACES     FILL TRAILING 0'S WITH SPACES             
         CLC   SVLGACC,SPACES                                                   
         BH    UPD060                                                           
         LA    R2,SCRNACCH                                                      
         MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(19),=C'NO PREVIOUS ACCOUNT'                              
         B     XXIT                                                             
                                                                                
UPD042   CLC   SCRNACCH+5(1),THISMIN  CHECK TO MAKE SURE ACCOUNT LENGTH         
         BNL   UPD060                 HAS MET MINIMUM REQUIREMENTS              
         OI    CACCSTA,HILVL          SET HIGH LEVEL ACCOUNT INDIC              
         LA    R5,FROMUL                                                        
         CLC   SCRUL,SPACES                                                     
         BNH   *+8                                                              
         LA    R5,SCRUL                                                         
         CLC   0(2,R5),=C'13'                                                   
         BE    UPD065                                                           
         CLC   0(2,R5),=C'14'                                                   
         BE    UPD065                                                           
         CLC   0(2,R5),=C'15'                                                   
         BE    UPD065                                                           
         CLC   0(2,R5),=C'16'                                                   
         BE    UPD065                                                           
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(21),=C'ACCOUNT NOT LOW LEVEL'                            
         MVI   ERROR,X'FE'                                                      
         LA    R2,SCRNACCH                                                      
         B     XXIT                                                             
                                                                                
UPD060   DS    0H                     UPDATE ACCOUNT RECORD                     
         MVC   MYKEY,SPACES           READ TO MAKE SURE U/L EXISTS ON           
         MVC   MYKEY(1),CONVCMP       TEST ID                                   
         ZIC   R5,THISMIN                                                       
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   MYKEY+1(0),FFTDATA                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYKEY,IO                     
         CLI   8(R1),0                                                          
         BE    UPD065                                                           
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(24),=C'HIGHER LEVELS NOT SET UP'                         
         MVI   ERROR,X'FE'                                                      
         LA    R2,SCRNACCH                                                      
         B     XXIT                                                             
                                                                                
UPD065   MVC   SVACCLNK,FFTDATA                                                 
         MVC   SVLGACC,FFTDATA                                                  
         GOTO1 DATAMGR,DMCB,=CL8'DMWRT',=CL8'ACCOUNT',IO2,IO2                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*        NOW UPDATE RECORD ON TEST ID                                           
                                                                                
         MVC   MYKEY2,SPACES          BUILD CONVERSION ACCOUNT KEY              
         MVC   MYKEY2(1),CONVCMP                                                
         MVC   MYKEY2+1(2),FROMUL                                               
         CLC   SCRUL,SPACES                                                     
         BNH   UPD120                                                           
         MVC   MYKEY2+1(2),SCRUL                                                
UPD120   MVC   MYKEY2+3(12),SCRNACC   READ FOR EXISTENCE OF CONVERSION          
         CLI   SCRNACC,C'"'           ACCOUNT                                   
         BNE   *+10                                                             
         MVC   MYKEY2+3(13),SVLGACC                                             
         OC    MYKEY2+3(12),SPACES                                              
         OI    DMINBTS,X'88'                                                    
         MVC   COMMAND,=C'DMREAD  '                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',MYKEY2,IO2            
         CLI   8(R1),0                                                          
         BE    UPD170                                                           
         TM    8(R1),X'02'                                                      
         BO    UPD160                                                           
                                                                                
         USING ACTRECD,R4                                                       
         LA    R4,IO2                 IF ACCOUNT NOT FOUND - BUILD              
         XC    0(255,R4),0(R4)        NEW ACCOUNT                               
         MVC   0(L'MYKEY2,R4),MYKEY2  I'VE ALREADY BUILT KEY                    
         MVC   42(2,R4),DATADISP                                                
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         USING NAMELD,R4              CHECK THAT NAME WAS INPUT ON              
         LA    R4,ELEMENT             SCREEN                                    
         MVI   NAMEL,NAMELQ                                                     
         CLC   SCRNAME,SPACES                                                   
         BH    UPD130                                                           
         ZIC   R5,SVNAMEL+1                                                     
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     UPD135                                                           
         MVC   ELEMENT(0),SVNAMEL                                               
                                                                                
UPD130   ZIC   R5,SCRNAMEH+5          BUILD NEW NAME ELEMENT AND                
         LA    R5,2(R5)               ADD TO RECORD                             
         STC   R5,NAMLN                                                         
         ZIC   R5,SCRNAMEH+5                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   NAMEREC(0),SCRNAME                                               
UPD135   GOTO1 ADDANEL                                                          
                                                                                
         XC    ELEMENT,ELEMENT        BUILD NEW STATUS ELEMENT AND              
         USING RSTELD,R4              ADD TO RECORD                             
         LA    R4,ELEMENT                                                       
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN3Q                                                    
         MVI   RSTFILT4,C' '                                                    
         GOTO1 DATCON,DMCB,(5,0),(1,RSTBDATE)                                   
         MVC   RSTTDATE,RSTBDATE                                                
         MVI   RSTFILT3,C' '                                                    
         MVI   RSTFILT1,C' '                                                    
         MVI   RSTFILT2,C' '                                                    
         MVI   RSTCOSTG,C' '                                                    
         MVI   RSTFILT5,C' '                                                    
         GOTO1 ADDANEL                                                          
                                                                                
         TM    CACCSTA,HILVL          IF THIS IS A HIGH LEVEL ACCOUNT           
         BO    UPD140                 DON'T ADD BALANCE ELEMENT                 
         XC    ELEMENT,ELEMENT        BUILD NEW BALANCE ELEMENT AND             
         USING ABLELD,R4              ADD TO RECORD                             
         LA    R4,ELEMENT                                                       
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN2Q                                                    
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         ZAP   ABLURG,=P'0'                                                     
         GOTO1 ADDANEL                                                          
UPD140   DS    0H                     ADD COMPLETED RECORD                      
         NI    CACCSTA,X'FF'-HILVL    RESET STATUS FOR NEXT PASS                
         GOTO1 DATAMGR,DMCB,=CL8'DMADD',=CL8'ACCOUNT',IO2,IO2                   
         CLI   8(R1),0                                                          
         BE    UPD300                                                           
         DC    H'0'                                                             
                                                                                
UPD160   LA    R4,IO2                 RECORD WAS MARKED DELETED SO              
         NI    44(R4),X'FF'-X'80'     UNDELETE                                  
                                                                                
UPD170   DS    0H                                                               
         LA    R4,IO2                 IF THE RECORD ALREADY EXISTED             
         USING NAMELD,R4              UPDATE THE NAME ELEMENT                   
         AH    R4,DATADISP                                                      
UPD175   CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),NAMELQ           FIND EXISTING NAME ELEMENT                
         BE    UPD180                                                           
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     UPD175                                                           
                                                                                
UPD180   DS    0H                                                               
         CLC   SCRNAME,SPACES         MISSING NAME HERE MEANS 'DO               
         BNH   UPD300                 NOT UPDATE'                               
         ZIC   R5,1(R4)               COMPARE NAME ON SCREEN TO NAME            
         SH    R5,=H'2'               ON RECORD TO SEE IF IT HAS                
         STC   R5,BYTE                CHANGED                                   
         CLC   BYTE,SCRNAMEH+5                                                  
         BNE   UPD185                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   NAMEREC(0),SCRNAME                                               
         BE    UPD300                                                           
                                                                                
UPD185   MVI   NAMEL,X'FF'            DELETE EXISTING NAME ELEMENT              
         GOTO1 REMANEL,DMCB,(X'FF',0)                                           
         XC    ELEMENT,ELEMENT        BUILD NEW ONE FROM SCREEN                 
         LA    R4,ELEMENT                                                       
         MVI   NAMEL,NAMELQ                                                     
         ZIC   R5,SCRNAMEH+5                                                    
         LA    R5,2(R5)                                                         
         STC   R5,NAMLN                                                         
         ZIC   R5,SCRNAMEH+5                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   NAMEREC(0),SCRNAME                                               
         GOTO1 ADDANEL                ADD BACK TO EXISTING RECORD               
**T                                                                             
         LA    R4,IO2                 REMANEL DOESN'T UPDATE RECORD             
         LA    R4,49(R4)              LENGTH - SO I'M DOING IT HERE             
         LA    R5,49                                                            
UPD190   CLI   0(R4),0                                                          
         BE    UPD192                                                           
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         AR    R5,R0                                                            
         B     UPD190                                                           
UPD192   LA    R5,1(R5)               END OF RECORD MARKER                      
         LA    RF,IO2                                                           
         STCM  R5,3,42(RF)                                                      
**T                                                                             
         GOTO1 DATAMGR,DMCB,=CL8'DMWRT',=CL8'ACCOUNT',IO2,IO2                   
         CLI   8(R1),0                UPDATE RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
UPD300   DS    0H                                                               
*        CLC   SVACCREM,SVACCLNK        IF DELETE AND ADD ARE EQUAL             
*        BE    UPD390                   THEN THEY NEGATE EACH OTHER             
*                                                                               
*        CLC   SVACCREM,SPACES          IF NOTHING DELETED ONLY UPDATE          
*        BE    UPD320                   ACCOUNT ADDED                           
*        MVC   MYKEY2,SPACES            CLEAR KEY                               
*        MVC   MYKEY2(1),CONVCMP        CONVERSION COMPANY                      
*        MVC   MYKEY2+1(14),SVACCREM    ACCOUNT REMOVED                         
*        OI    DMINBTS,X'80'                                                    
*        MVC   COMMAND,=C'DMREAD  '                                             
*        GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',MYKEY2,IO2            
*        CLI   8(R1),0           IF NOT FOUND THERE IS A PROBLEM                
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        LA    R4,IO2                                                           
*        AH    R4,DATADISP                                                      
*PD305   CLI   0(R4),0                                                          
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*        CLI   0(R4),ABLELQ                                                     
*        BE    UPD310                                                           
*        ZIC   R5,1(R4)                                                         
*        AR    R4,R5                                                            
*        B     UPD305                                                           
*        USING ABLELD,R4                                                        
*PD310   SP    ABLURG,=P'1'                                                     
*        GOTO1 DATAMGR,DMCB,=CL8'DMWRT',=CL8'ACCOUNT',IO2,IO2                   
*        CLI   8(R1),0                                                          
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
*PD320   CLC   SVACCLNK,SPACES          IF NOTHING DELETED ONLY UPDATE          
*        BE    UPD390                   ACCOUNT ADDED                           
*        MVC   MYKEY2,SPACES            CLEAR KEY                               
*        MVC   MYKEY2(1),CONVCMP        CONVERSION COMPANY                      
*        MVC   MYKEY2+1(14),SVACCLNK    ACCOUNT LINKED                          
*        OI    DMINBTS,X'80'                                                    
*        MVC   COMMAND,=C'DMREAD  '                                             
*        GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',MYKEY2,IO2            
*        CLI   8(R1),0           IF NOT FOUND THERE IS A PROBLEM                
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        LA    R4,IO2                                                           
*        AH    R4,DATADISP                                                      
*PD325   CLI   0(R4),0                                                          
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*        CLI   0(R4),ABLELQ                                                     
*        BE    UPD330                                                           
*        ZIC   R5,1(R4)                                                         
*        AR    R4,R5                                                            
*        B     UPD325                                                           
*        USING ABLELD,R4                                                        
*PD330   AP    ABLURG,=P'1'                                                     
*        GOTO1 DATAMGR,DMCB,=CL8'DMWRT',=CL8'ACCOUNT',IO2,IO2                   
*        CLI   8(R1),0                                                          
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
UPD390   LA    R3,SCRLLEN(R3)         BUMP TO NEXT ENTRY ON SCREEN              
         BCT   R6,UPD008                                                        
                                                                                
         MVC   SVLGACC,SPACES                                                   
         MVC   LOGHEAD,SPACES                                                   
         MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(14),=C'SCREEN UPDATED'                                   
         LA    R2,LOGUNITH                                                      
         B     XXIT                                                             
                                                                                
ULERR    MVC   LOGHEAD,SPACES                                                   
         MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(23),=C'UNIT/LEDGER NOT ON FILE'                          
         B     XXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        CHECK MINIMUM ACCOUNT LENGTH INPUT                                     
*-----------------------------------------------------------------*             
CHKMIN   NTR1                                                                   
         LA    R4,IO               IF LEDGER DOES EXIST ON TEST ID              
         AH    R4,DATADISP         SAVE HEIRARCHY ELEMENT FOR LATER             
CHK05    CLI   0(R4),0             VALIDATION                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),ACLELQ                                                     
         BE    CHK10                                                            
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     CHK05                                                            
                                                                                
         USING ACLELD,R4                                                        
CHK10    MVC   TEMPHEIR,0(R4)                                                   
         MVI   MINTEMP,0                                                        
         LA    R5,ACLVALS                                                       
CHK15    CLI   0(R5),12                                                         
         BE    CHK20                                                            
         MVC   MINTEMP,0(R5)                                                    
         LA    R5,16(R5)                                                        
         B     CHK15                                                            
CHK20    ZIC   R5,MINTEMP                                                       
         LA    R5,1(R5)                                                         
         STC   R5,MINTEMP                                                       
         MVC   THISMIN,MINTEMP                                                  
         B     XXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        ANY CHANGES TO THIS SCREEN                                             
*-----------------------------------------------------------------*             
ANYCHA   NTR1                                                                   
         USING SCRND,R3                                                         
         LA    R3,LOGFACCH                                                      
         LA    R5,SCRNTRY                                                       
ANYC20   TM    SCRULH+4,X'80'                                                   
         BZ    *+8                                                              
         OI    DISSW,DISCHA                                                     
         TM    SCRNACCH+4,X'80'                                                 
         BZ    *+8                                                              
         OI    DISSW,DISCHA                                                     
         TM    SCRNAMEH+4,X'80'                                                 
         BZ    *+8                                                              
         OI    DISSW,DISCHA                                                     
         LA    R3,SCRLLEN(R3)                                                   
         BCT   R5,ANYC20                                                        
                                                                                
ANYCXIT  B     XXIT                                                             
*-----------------------------------------------------------------*             
*        CLEAR THIS SCREEN                                                      
*-----------------------------------------------------------------*             
CLRSCRN  NTR1                                                                   
         USING SCRND,R3                                                         
         LA    R3,LOGFACCH                                                      
         LA    R5,SCRNTRY                                                       
CSCR10   MVC   SCRACC,SPACES                                                    
         OI    SCRACCH+6,X'80'                                                  
         MVC   SCRUL,SPACES                                                     
         OI    SCRULH+6,X'80'                                                   
         MVC   SCRNACC,SPACES                                                   
         OI    SCRNACCH+6,X'80'                                                 
         MVC   SCRNAME,SPACES                                                   
         OI    SCRNAMEH+6,X'80'                                                 
         LA    R3,SCRLLEN(R3)                                                   
         BCT   R5,CSCR10                                                        
         B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        PROTECT SCREEN                                                         
*-----------------------------------------------------------------*             
PRTECT   NTR1                                                                   
         USING SCRND,R3                                                         
         LA    R3,LOGFACCH                                                      
         LA    R5,SCRNTRY                                                       
PCT10    OI    SCRULH+1,X'20'                                                   
         OI    SCRULH+6,X'80'                                                   
         OI    SCRNACCH+1,X'20'                                                 
         OI    SCRNACCH+6,X'80'                                                 
         OI    SCRNAMEH+1,X'20'                                                 
         OI    SCRNAMEH+6,X'80'                                                 
         LA    R3,SCRLLEN(R3)                                                   
         BCT   R5,PCT10                                                         
         B     XXIT                                                             
                                                                                
*-----------------------------------------------------------------*             
*        UNPROTECT SCREEN                                                       
*-----------------------------------------------------------------*             
UNPRTECT NTR1                                                                   
         USING SCRND,R3                                                         
         LA    R3,LOGFACCH                                                      
         LA    R5,SCRNTRY                                                       
UPCT10   NI    SCRULH+1,X'FF'-X'20'                                             
         OI    SCRULH+6,X'80'                                                   
         NI    SCRNACCH+1,X'FF'-X'20'                                           
         OI    SCRNACCH+6,X'80'                                                 
         NI    SCRNAMEH+1,X'FF'-X'20'                                           
         OI    SCRNAMEH+6,X'80'                                                 
         LA    R3,SCRLLEN(R3)                                                   
         BCT   R5,UPCT10                                                        
         B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        TABLE FULL - SHIFT KEYS BACK AND CLEAR LAST KEY ENTRY                  
*-----------------------------------------------------------------*             
RETAB    NTR1                                                                   
         LA    R4,KEYBLOCK                                                      
         LA    R3,NKEYTAB-1                                                     
RT10     MVC   1(14,R4),16(R4)                                                  
         LA    R4,LKEYTAB(R4)                                                   
         BCT   R3,RT10                                                          
         XC    KEYLAST+1(14),KEYLAST+1                                          
         B     XXIT                                                             
                                                                                
*-----------------------------------------------------------------*             
*        CLEAR KEY TABLE ENTRIES                                                
*-----------------------------------------------------------------*             
CLRTAB   NTR1                                                                   
         LA    R4,KEYBLOCK         IF A NEW ACCOUNT WAS ENTERED CLEAR           
         LA    R3,NKEYTAB                                                       
         LA    R5,1                                                             
CLR10    DS    0H                                                               
         STC   R5,0(R4)                                                         
         XC    1(14,R4),1(R4)                                                   
         LA    R4,LKEYTAB(R4)                                                   
         LA    R5,1(R5)                                                         
         BCT   R3,CLR10                                                         
         B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        ROUTINE TO SAVE HEIRARCHY DETAILS AND COMPANY ELEMENT                  
*-----------------------------------------------------------------*             
SAVEM    NTR1                                                                   
         LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
SAVEH2   CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),CPYELQ                                                     
         BE    SAVEH6                                                           
         ZIC   R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     SAVEH2                                                           
SAVEH6   ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   SVCOMPEL(0),0(R4)                                                
         B     XXIT                                                             
                                                                                
XXIT     XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        LITERAL POOL                                                           
*-----------------------------------------------------------------*             
                                                                                
LKEYTAB  EQU   15                  LENGTH OF ONE ENTRY IN KEY TABLE             
NKEYTAB  EQU   10                  NUMBER OF ENTRIES KEPT IN TABLE              
                                                                                
SCRNTRY  EQU   14                  LINES DISPLAYED PER SCREEN                   
         LTORG                                                                  
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        DSECT FOR LOCAL W/S                                                    
*-----------------------------------------------------------------*             
LWSD     DSECT                                                                  
PRELOC   DS    F                                                                
SAVRB    DS    F                   ENTRY POINT                                  
SAVR2    DS    F                   ENTRY POINT                                  
ELCODE   DS    CL1                                                              
MYKEY    DS    CL42                                                             
MYKEY2   DS    CL42                                                             
SVCOMPEL DS    CL94                                                             
SVACCREM DS    CL14                                                             
SVACCLNK DS    CL14                                                             
LWSX     DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMCED                                                       
KEYBLOCK DC    AL1(1),CL14' '                                                   
         DC    AL1(2),CL14' '                                                   
         DC    AL1(3),CL14' '                                                   
         DC    AL1(4),CL14' '                                                   
         DC    AL1(5),CL14' '                                                   
         DC    AL1(6),CL14' '                                                   
         DC    AL1(7),CL14' '                                                   
         DC    AL1(8),CL14' '                                                   
         DC    AL1(9),CL14' '                                                   
KEYLAST  DC    AL1(10),CL14' '                                                  
                                                                                
CONVCMP  DS    CL1                                                              
FROMUL   DS    0CL2                                                             
FRUNIT   DS    CL1                                                              
FRLEDG   DS    CL1                                                              
                                                                                
DISSW    DC    XL1'00'                                                          
DISDONE  EQU   X'80'                 LEDGER DISPLAY IS COMPLETE                 
DISWARN1 EQU   X'40'                                                            
DISWARN2 EQU   X'20'                                                            
DISCHA   EQU   X'10'                                                            
CACCSTA  DC    XL1'00'               STATUS FOR NEW ACCOUNT                     
HILVL    EQU   X'80'                 ACCOUNT IS HIGH LEVEL                      
                                                                                
SCRNUM   DC    XL1'00'                                                          
CURSCRN  DS    F                                                                
                                                                                
SVMYKEY  DS    CL42                                                             
SVNAMEL  DS    CL38                                                             
BYTE     DS    CL1                                                              
TSIDHEIR DS    CL66                                                             
MINLEN   DS    CL1                                                              
TEMPHEIR DS    CL66                                                             
MINTEMP  DS    CL1                                                              
THISMIN  DS    CL1                                                              
SVLGACC  DS    CL14                                                             
         EJECT                                                                  
                                                                                
SCRND    DSECT                                                                  
SCRACCH  DS    CL8                                                              
SCRACC   DS    CL12                                                             
SCRULH   DS    CL8                                                              
SCRUL    DS    CL2                                                              
SCRNACCH DS    CL8                                                              
SCRNACC  DS    CL12                                                             
SCRNAMEH DS    CL8                                                              
SCRNAME  DS    CL36                                                             
SCRLLEN  EQU   *-SCRND                                                          
                                                                                
                                                                                
       ++INCLUDE ACLFMWORK                                                      
         EJECT                                                                  
         PRINT OFF                                                              
*        ACGENBOTH                                                              
       ++INCLUDE ACGENBOTH                                                      
*        ACGENFILE                                                              
       ++INCLUDE ACGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE ACLFMEQU                                                       
*        ACLFMEQU                                                               
       ++INCLUDE DDFLDIND                                                       
*        DDFLDIND                                                               
       ++INCLUDE DDCOMFACS                                                      
*        DDCOMFACS                                                              
       ++INCLUDE ACVATICAND                                                     
*        ACVATICAND                                                             
       ++INCLUDE DDFLDHDR                                                       
*        DDFLDHDR                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033ACLFM36   05/01/02'                                      
         END                                                                    
