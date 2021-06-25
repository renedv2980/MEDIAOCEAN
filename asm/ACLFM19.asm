*          DATA SET ACLFM19    AT LEVEL 160 AS OF 05/01/02                      
*PHASE T60319A,*                                                                
*INCLUDE SCINKEY                                                                
         TITLE 'T60319 - MULTI-LEDGER CLI/PRO/JOB LFM.'                         
T60319   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,**LFM19*,RR=RE                                               
         USING T60319+4096,R9,R5                                                
         LA    R9,T60319+2048                                                   
         LA    R9,2048(R9)                                                      
         LA    R5,2048(R9)                                                      
         LA    R5,2048(R5)                                                      
         L     RA,0(R1)            RA = A(TWA)                                  
         L     RC,4(R1)            RC = A(WORKING STORAGE).                     
         USING T603FFD,RA                                                       
         USING LOGWORKD,RC                                                      
         ST    RE,WRELO                                                         
         MVI   DMOUTBTS,0          APPLIC HANDLES I/O ERRORS.                   
         NI    LOGACTH+6,X'FF'-X'40'  TURN OFF CURSOR                           
         OI    LOGACTH+6,X'80'                                                  
         NI    LOGSJACH+6,X'FF'-X'40'                                           
         OI    LOGSJACH+6,X'80'                                                 
*                                                                               
         CLI   COMPANY,X'7E'       SPECIAL CODE FOR FCB                         
         BE    MODE0                                                            
         CLI   COMPANY,X'79'                                                    
         BNE   MODE1                                                            
*                                                                               
MODE0    CLC   LOGREC(2),=C'MJ'                                                 
         BNE   MODE1                                                            
         MVI   ERROR,INVALID                                                    
         LA    R2,LOGACTH                                                       
         CLI   LOGACT,C'N'                                                      
         BE    ERREXIT                                                          
         MVI   ERROR,X'FF'                                                      
*                                                                               
MODE1    LA    RE,WKSTRT           CLEAR MY WORKING STORAGE.                    
         LA    RF,WKNDLNQ                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     RF,=V(SCINKEY)      SET UP AND RELOCATE XTRNALS.                 
         A     RF,WRELO                                                         
         ST    RF,SCINKEY                                                       
         L     RF,COMFACS          GET COMFACS ROUTINES.                        
         USING COMFACSD,RF                                                      
         MVC   SCANNER,CSCANNER                                                 
         MVC   HELLO,CHELLO                                                     
         MVC   GETFACT,CGETFACT                                                 
         DROP  RF                                                               
         L     RF,FAPARM                                                        
         MVC   ATIA,12(RF)         SAVE A(TIA)                                  
         L     RE,12(RF)           SET UP TO XC TIA.                            
         L     RF,=A(TIALNQ)                                                    
         SR    R1,R1                                                            
         MVCL  RE,R0               XC IT.                                       
         LA    RE,IO2              CLEAR PRODUCTION RECORD AREA.                
         LA    RF,IOLENQ                                                        
         MVCL  RE,R0                                                            
         B     SETSCRN                                                          
         SPACE 1                                                                
*                                                                               
OKEXIT   CR    RB,RB                                                            
         MVI   ERROR,X'FF'         RESET ERROR INDICATOR.                       
         B     EXIT                                                             
*                                                                               
ERREXIT  LTR   R8,R8                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XMOD1                                                                  
*                                                                               
EXIT2    L     R2,FADR                                                          
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
         USING TYPTBLD,R8                                                       
         USING FLDTBLD,R7                                                       
SETSCRN  LA    R8,TYPTBL                                                        
         LA    R7,FLDTBL                                                        
SSCR020  CLI   TYPLVL,X'FF'        FIND TYPE TABLE ENTRY FOR REQUESTED          
         BNE   *+6                 RECORD TYPE.                                 
         DC    H'0'                TABLE SEEMS DAMAGED.                         
         CLC   LOGREC(2),TYPREC                                                 
         BE    SSCR040             FOUND IT.                                    
         LA    R8,TYPTBLNQ(R8)                                                  
         B     SSCR020                                                          
*                                                                               
SSCR040  CLC   TYPESAVE,TYPLVL                                                  
         BE    MODESET             SAME RECORD TYPE, SCREEN IS OK.              
         MVC   LASTKEY,SPACES                                                   
         GOTO1 CALLOV,DMCB,(TYPSCR,LOGLOADH),0 LOAD THE SUB-SCREEN.             
         CLI   DMCB+8,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                BAD LOAD.                                    
SSCR050  MVC   TYPESAVE,TYPLVL     SAVE RECORD TYPE CHARACTERISTICS.            
         ZICM  R2,TYPENDSC,2                                                    
         LA    R2,T603FFD(R2)      R2 = A(TAB FIELD FOR THIS SCREEN).           
         TWAXC LOGSJNMH,(R2)       CLEAR THE SCREEN                             
         LA    R1,LOGHEADH                                                      
         SR    RE,RE                                                            
         LR    RF,R2               SET UP TO TRANSMIT ALL FIELDS.               
         OI    6(R1),X'80'                                                      
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,*-8                                                        
*                                                                               
*              TAILOR SCREEN TO NEEDS OF RECORD TYPE.                           
         XR    R1,R1                                                            
SSCR060  CLI   FLDNMBR,X'FF'                                                    
         BE    SSCR100             NO MORE FIELDS.                              
         ZICM  RE,FLDADR,2         FIELD MUST BE PROTECTED.                     
         LA    RE,T603FFD(RE)      RE = A(FIELD HEADER).                        
         IC    R1,TYPESTAT                                                      
         EX    R1,FSTAT1TM          TM   FLDSTAT1,0                             
         BNZ   SSCR080             FIELD NOT PRESENT FOR RECORD TYPE.           
         TM    FLDSTAT1,LABELQ                                                  
         BNZ   SSCR070             LABELS ARE ALWAYS PROTECTED.                 
*                                                                               
         NI    1(RE),X'DF'         TURN OFF PROTECT BIT.                        
         IC    R1,TYPEPROT                                                      
         EX    R1,FSTAT2TM          TM   FLDSTAT2,0                             
         BZ    SSCR070             NO MODIFICATION NEEDED.                      
         TM    1(RE),X'20'                                                      
         BNZ   *+12                FIELD ALREADY PROTECTED.                     
         OI    1(RE),X'20'         PROTECT                                      
SSCR070  IC    R1,TYPEXC                                                        
         EX    R1,FSTAT2TM          TM   FLDSTAT2,0                             
         BNZ   SSCR075             FIELD IS TO BE BLANKED                       
         TM    FLDSTAT1,LABELQ     FIELD MUST SHOW.                             
         BZ    SSCR080             PREVIOUSLY BLANKED LABELS                    
         ZICM  RF,FLDDIS,3         MUST BE RESTORED.                            
         BZ    SSCR080             NO A(LABEL CHARACTERS) FOR THIS FLD.         
         A     RF,WRELO            RELOCATE.                                    
         IC    R1,0(RE)            R1 = L'FIELD + HEADER.                       
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     SSCR080                                                          
         MVC   8(0,RE),0(RF)                                                    
*                                                                               
SSCR075  IC    R1,0(RE)            FIELD MUST BE XC'D.                          
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,RE),8(RE)                                                    
         OI    1(RE),X'20'                                                      
SSCR080  LA    R7,FLDTBLNQ(R7)                                                  
         B     SSCR060                                                          
*                                                                               
FSTAT1TM TM    FLDSTAT1,0                                                       
FSTAT2TM TM    FLDSTAT2,0                                                       
*                                                                               
SSCR100  OI    LOGSJACH+6,X'C0'    CURSOR TO PRODUCTION ACCOUNT CODE.           
*&&US                                                                           
         LA    RE,LOGHEADH         RETRANSMIT WHOLE SCREEN AFTER                
         SR    R0,R0               CHANGING ATTRIBUTES.                         
SSCR110  CLI   0(RE),0                                                          
         BE    SSCR120             FOUND END OF TWA.                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         B     SSCR110                                                          
SSCR120  MVC   1(2,RE),=X'0101'                                                 
*&&                                                                             
         B     OKEXIT                                                           
         DROP  R7,R8                                                            
         EJECT                                                                  
MODESET  CLC   LASTCOMP,COMPANY                                                 
         BE    MOD030                                                           
         MVC   LASTKEY,SPACES                                                   
         MVC   LASTCOMP,COMPANY                                                 
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),LASTCOMP                                                  
         GOTO1 READ                READ NEW COMPANY RECORD.                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R8,IO+ACRECORD-ACKEYD R8 = A(1ST ELEMENT IN COMP RECORD)         
         USING ACCOMPD,R8                                                       
MOD010   CLI   ACMPEL,0                                                         
         BNE   *+6                                                              
         DC    H'0'                FAWLTY COMPANY RECORD.                       
         CLI   ACMPEL,X'10'                                                     
         BE    MOD020                                                           
         ZIC   R1,ACMPLEN                                                       
         AR    R8,R1                                                            
         B     MOD010                                                           
*                                                                               
MOD020   MVC   PRODLEDG,ACMPJOB    SAVE PRODUCTION AND RECEIVABLE               
         MVC   RECVLEDG,ACMPRECV   UNIT/LEDGERS.                                
         MVC   COMPSTAT,ACMPSTAT   SAVE COMPANY STATUS BYTE.                    
MOD030   CLC   LOGSJAC,LASTKEY+3                                                
         BE    MOD040                                                           
         CLI   ACTION,C'N'                                                      
         BE    MOD034              DON'T CLEAR NEW INPUT.                       
         ZICM  R2,TYPENDSC,2       SET UP TO CLEAR LAST SCREEN.                 
         LA    R2,T603FFD(R2)      R2 = A(TAB FIELD FOR THIS SCREEN).           
         TWAXC LOGSJNMH,(R2)       ENSURE A CLEAN SCREEN IS SEEN. KEEN.         
MOD034   MVC   LASTKEY(1),COMPANY                                               
         MVC   LASTKEY+1(2),=C'SJ'                                              
         MVC   LASTKEY+3(12),LOGSJAC                                            
         MVI   MODE,DSPLYREC                                                    
         CLI   LOGACT,C'N'         ALL ACTIONS BUT 'NEW' DISPLAY FIRST.         
         BE    MOD040                                                           
         CLI   LOGACT,C'C'         AND 'CLOSE'                                  
         BNE   FIELDING                                                         
MOD036   MVI   MODE,CLOSEJOB                                                    
         B     CLOSEIT                                                          
*                                                                               
MOD040   CLI   LOGACT,C'C'                                                      
         BE    MOD036                                                           
         MVI   MODE,BUILDREC                                                    
         CLI   LOGACT,C'E'                                                      
         BE    MOD050                                                           
         CLI   LOGACT,C'I'                                                      
         BNE   FIELDING                                                         
MOD050   MVI   MODE,DSPLYREC                                                    
         ZICM  R2,TYPENDSC,2       SET UP TO CLEAR LAST SCREEN.                 
         LA    R2,T603FFD(R2)      R2 = A(TAB FIELD FOR THIS SCREEN).           
         TWAXC LOGSJNMH,(R2)       ENSURE A CLEAN SCREEN IS SEEN. KEEN.         
         B     FIELDING                                                         
         EJECT                                                                  
         USING FLDTBLD,R7                                                       
FIELDING LA    R7,FLDTBL                                                        
         XR    R1,R1                                                            
*                                                                               
FLD020   CLI   FLDNMBR,X'FF'                                                    
         BE    FLD140              END OF TABLE.                                
         TM    FLDSTAT1,LABELQ                                                  
         BNZ   FLD122              IT'S A LABEL FIELD.                          
         IC    R1,TYPESTAT                                                      
         EX    R1,FSTAT1TM         TM    FLDSTAT1,0                             
         BNZ   FLD122              FIELD NOT PRESENT FOR RECORD TYPE.           
         IC    R1,TYPEPROT                                                      
         EX    R1,FSTAT2TM         TM    FLDSTAT2,0                             
         BNZ   FLD122              FIELD IS NOT SHOWN FOR RECORD TYPE.          
         ZICM  RE,FLDADR,2                                                      
         LA    RE,T603FFD(RE)      RE = A(FLDHDR)                               
         ST    RE,FADR                                                          
         OI    6(RE),X'80'                                                      
         MVI   ERROR,INVALID       DEFAULT ERROR MESSAGE.                       
         CLI   MODE,DSPLYREC                                                    
         BNE   FLD060                                                           
         ZICM  RF,FLDDIS,3                                                      
         BZ    FLD122                                                           
         B     FLD100                                                           
*                                                                               
FLD060   IC    R1,TYPEMAND                                                      
         EX    R1,FSTAT1TM         TM    FLDSTAT1,0                             
         BZ    FLD080                                                           
         L     R2,FADR                                                          
         GOTO1 ANY                                                              
FLD080   ZICM  RF,FLDVAL,3                                                      
         BZ    FLD122                                                           
         B     FLD100                                                           
*                                                                               
FLD100   LA    RE,FLD120           SET RETURN ADDRESS.                          
         A     RF,WRELO            RELOCATE.                                    
         NTR1                                                                   
         BR    RF                                                               
*                                                                               
FLD120   BNE   EXIT2                                                            
FLD122   LA    R7,FLDTBLNQ(R7)                                                  
         B     FLD020                                                           
*                                                                               
FLD140   CLI   MODE,DSPLYREC                                                    
         BNE   FLD160                                                           
         ZICM  RF,TYPEDISP,3                                                    
         BZ    SSCR100                                                          
         B     FLD180                                                           
FLD160   ZICM  RF,TYPEVAL,3                                                     
         BZ    SSCR100                                                          
FLD180   LA    RE,FLD200                                                        
         A     RF,WRELO                                                         
         NTR1                                                                   
         BR    RF                                                               
FLD200   BNE   EXIT2                                                            
         LA    R2,LOGSJACH                                                      
         B     EXIT2                                                            
         DROP  R7                                                               
         EJECT                                                                  
*              DISPLAY RECEIVABLE ACCOUNT DATA.                                 
*                                                                               
         USING ACPROFD,R8                                                       
DISSRAC  ZICM  R8,ADACPROF,4                                                    
         BZ    OKEXIT                                                           
         CLI   ACPRRECV,C' '                                                    
         BE    OKEXIT              NO RECEIVABLE ACCOUNT.                       
         CLI   ACPRRECV,0                                                       
         BE    OKEXIT                                                           
         MVI   LOGSRACH+5,12       FORCE LENGTH FOR VALACC                      
         MVC   THISLEDG,ACPRRECV+1 PASS RECEIVABLE LEDGER.                      
         MVC   LOGSRAC,ACPRRECV+3  DISPLAY RECEIVABLE ACC'T CODE.               
         BAS   RE,VALACC           VALIDATE AND DISPLAY NAME AND FILTS.         
         BAS   RE,ZEROSPC          ZEROIZE TRAILING SPACES.                     
         B     EXIT                                                             
         SPACE 1                                                                
*              DISPLAY COSTING ACCOUNT DATA.                                    
*                                                                               
DIS1CAC  ZICM  R8,ADACPROF,4                                                    
         BZ    OKEXIT                                                           
         CLI   ACPRCOST,C' '                                                    
         BE    OKEXIT                                                           
         CLI   ACPRCOST,0                                                       
         BE    OKEXIT                                                           
         MVI   LOG1CACH+5,12                                                    
         MVC   THISLEDG,ACPRCOST+1 PASS COSTING LEDGER.                         
         MVC   LOG1CAC,ACPRCOST+3                                               
         BAS   RE,VALACC                                                        
         BAS   RE,ZEROSPC                                                       
         B     EXIT                                                             
         SPACE 1                                                                
*              DISPLAY SALES ANALYSIS ACCOUNT DATA.                             
*                                                                               
         USING ACSAND,R8                                                        
DISSAAC  ZICM  R8,ADACSAN,4                                                     
         BZ    OKEXIT                                                           
         CLI   ACSACODE,C' '                                                    
         BE    OKEXIT                                                           
         MVI   LOGSAACH+5,12                                                    
         MVC   THISLEDG,ACSACODE+1 PASS SALES ANALYSIS LEDGER.                  
         MVC   LOGSAAC,ACSACODE+3                                               
         BAS   RE,VALACC                                                        
         BAS   RE,ZEROSPC                                                       
         B     EXIT                                                             
         SPACE 1                                                                
*              DISPLAY SALES ANALYSIS OFFICE/UNIT FOR ANALYSIS.                 
*                                                                               
         USING ACPROFD,R8                                                       
DISSAOF  ZICM  R8,ADSAPROF,4                                                    
         BZ    OKEXIT                                                           
         MVC   LOGSAAN,ACPROFFC    DISPLAY THE OFFICE/UNIT FOR ANALYSIS         
         B     OKEXIT                                                           
         SPACE 1                                                                
*              DISPLAY BILLING TYPE                                             
         USING BILTTBLD,RE                                                      
         USING ACPROFD,R8                                                       
DISBTYP  ZICM  R8,ADACPROF,4                                                    
         BZ    OKEXIT                                                           
         LA    RE,BILTTBL                                                       
DSBT010  CLI   BILTCOD,X'FF'                                                    
         BNE   *+6                                                              
         DC    H'0'                RECORD BILLING TYPE IS RUBBISH               
         CLC   ACPRBILL,BILTCOD                                                 
         BE    DSBT020             FOUND TABLE ENTRY FOR TYPE                   
         LA    RE,BILTTBLQ(RE)                                                  
         B     DSBT010                                                          
*                                                                               
DSBT020  TM    BILTTYP,MOVER                                                    
         BZ    DSTB040             NOT A SIMPLE NARRATIVE.                      
         MVC   LOGTYPE,BILTNAR     MOVE IN NARRATIVE                            
         B     OKEXIT              AND RETURN.                                  
*                                                                               
DSTB040  ZICM  RF,BILTDRTN,3                                                    
         A     RF,WRELO                                                         
         BR    RF                                                               
*                                                                               
DSTBFEE  MVC   LOGTYPE(3),=C'FEE'                                               
         BAS   RE,BLAMEDIT                                                      
         MVC   LOGTYPE+3(9),EDWORK                                              
         B     OKEXIT                                                           
*                                                                               
DSTBEST  BAS   RE,BLAMEDIT                                                      
         MVC   LOGTYPE,EDWORK                                                   
         LA    R1,LOGTYPE                                                       
         AR    R1,R0               ADD LENGTH OF EDITED PERCENT                 
         SH    R1,=H'3'                                                         
         CLC   0(3,R1),=C'.00'                                                  
         BNE   *+10                                                             
         MVC   0(3,R1),SPACES                                                   
         LA    R1,LOGTYPE+11       FIND END OF SIGNIFICANT DATA                 
         SR    R3,R3                                                            
PR9A     CLI   0(R1),C' '                                                       
         BNE   PR9B                                                             
         AH    R3,=H'1'                                                         
         BCT   R1,PR9A                                                          
PR9B     CH    R3,=H'9'                                                         
         BL    *+8                                                              
         LA    R3,9                                                             
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     OKEXIT                                                           
         MVC   1(0,R1),=C'%ESTIMATE'                                            
*                                                                               
DSTBSPC  BAS   RE,BLAMEDIT                                                      
         MVC   LOGTYPE,EDWORK                                                   
         B     OKEXIT                                                           
*                                                                               
BLAMEDIT EDIT  (4,ACPRBLAM),(12,EDWORK),2,ALIGN=LEFT,FLOAT=-                    
         BR    RE                                                               
*                                                                               
         SPACE 1                                                                
         EJECT                                                                  
*              DISPLAY NON-BILLABLE WORK CODES.                                 
*                                                                               
         USING ACPROFD,R8                                                       
DISNONB  ZICM  R8,ADACPROF,4                                                    
         BZ    OKEXIT                                                           
         CLI   ACPRUNBL,0                                                       
         BE    OKEXIT                                                           
         LA    RE,ACPRUNBL                                                      
         LA    RF,LOGNONB                                                       
         LA    R0,6                                                             
*                                                                               
DNNB010  CLC   0(2,RE),SPACES                                                   
         BE    OKEXIT                                                           
         MVC   0(2,RF),0(RE)                                                    
         BCT   R0,*+8                                                           
         B     OKEXIT                                                           
         CLC   2(2,RE),SPACES                                                   
         BE    OKEXIT                                                           
         MVI   2(RF),C','                                                       
         LA    RE,2(RE)                                                         
         LA    RF,3(RF)                                                         
         B     DNNB010                                                          
         SPACE 1                                                                
*              DISPLAY PROFILES                                                 
*                                                                               
         USING SCINBLKD,RE                                                      
         USING XPRFTABD,RF                                                      
DISXPROF DS    0H                                                               
         MVI   BLKCNT,0            INITIALIZE BLOCK COUNT.                      
         LA    RE,SCINBLK                                                       
         LA    RF,XPRFTAB                                                       
         ICM   R1,15,ADACJOB       GET JOB ELEMENT                              
         BZ    DXPR010                                                          
         USING ACJOBD,R1                                                        
         TM    ACJBSTAT,X'80'      TEST FOR UNAPPROVED ESTIMATE                 
         BZ    DXPR010                                                          
         MVC   SCINITEM,SPACES                                                  
         MVC   SCINITEM(9),=C'EST=UNAPP'                                        
         MVI   BLKCNT,1                                                         
         LA    RE,SCINBLNQ(RE)     NEXT BLOCK ITEM                              
         DROP  R1                                                               
*                                                                               
DXPR010  ICM   R8,15,ADACXPRF      GET A(EXTRA PROFILE ELEMENT)                 
         BNZ   DXPR020                                                          
         CLI   BLKCNT,0            TEST ANYTHING TO OUTPUT                      
         BE    OKEXIT              NO                                           
         B     DXPR160             YES                                          
*                                                                               
DXPR020  CLI   XPRFKWRD,X'FF'                                                   
         BE    DXPR160             END OF XTRA PROFILE TABLE.                   
         CLI   XPRFKWRD,X'FE'      DO NOT TEST MORE XTRA PROFILES IF            
         BNE   DXPR025             THE XPROF ELEMENT LENGTH IS 16.              
         CLI   1(R8),16                                                         
         BE    DXPR160                                                          
         LA    RF,1(RF)            IT ISN'T, SKIP THE X'FE'                     
DXPR025  MVC   SCINITEM,SPACES                                                  
         ZICM  R7,XPRFAFLD,2                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R7,0(R7,R8)         R7 = A(FIELD IN XTRA PROF LMNT).             
         TM    XPRFSTAT,NODSP      TEST NO DISPLAY                              
         BO    DXPR140             YES-SKIP ENTRY                               
         TM    XPRFSTAT,BITS                                                    
         BO    DXPR080             FIELD IS BIT SIGNIFICANT                     
         ZIC   R1,XPRFLFLD         R1 = L'FIELD - 1.                            
         TM    XPRFSTAT,PACKED                                                  
         BO    DXPR060             FIELD IS PACKED DECIMAL.                     
         EX    R1,XPRFCLC          CLC   0(0,R7),XPRFCMPV                       
         BE    DXPR140             DEFAULT IN RECORD, NO DISPLAY.               
         B     DXPR120                                                          
XPRFCLC  CLC   0(0,R7),XPRFDEFV                                                 
*                                                                               
DXPR060  LR    R0,R1               DUPLICATE 'EX' LENGTH FOR PACKED             
         SLL   R0,4                COMPARE                                      
         OR    R1,R0                                                            
         EX    R1,XPRFCP           CP    0(0,R7),XPRFCMPV(0)                    
         BNE   DXPR120             DISPLAY IF REC VALUE ISN'T DEFAULT.          
         B     DXPR140             NO DISPLAY, TRY NEXT FIELD/VALUE.            
XPRFCP   CP    0(0,R7),XPRFDEFV(0)                                              
*                                                                               
DXPR080  ZIC   R1,XPRFDEFV+1                                                    
         EX    R1,XPRFTM           TM    0(R7),0                                
         BO    DXPR120                                                          
         B     DXPR140             NO DISPLAY, TRY NEXT FIELD/VALUE.            
XPRFTM   TM    0(R7),0                                                          
*                                                                               
DXPR120  TM    XPRFSTAT,DSPRECV    DISPLAY RECORD VALUE                         
         BNO   DXPR121                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCINDISP(0),0(R7)   VALUE FROM RECORD                            
         B     DXPR122                                                          
         SPACE 1                                                                
DXPR121  MVC   SCINDISP(6),XPRFDSPV                                             
DXPR122  TM    XPRFSTAT,FLDVALQ                                                 
         BZ    DXPR123             FIELD HAS NO SPECIAL DISPLAY.                
         ZICM  R1,XPRFDRTN,3       GET A(SPECIAL DISPLAY ROUTINE).              
         BZ    DXPR123             DISPLAY IS NORMAL                            
         A     R1,WRELO            RELOCATE THE ADDRESS.                        
         BR    R1                                                               
*                                                                               
DXPR2DP  ZIC   R1,XPRFLFLD                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         ZAP   EDWORK(6),0(0,R7)                                                
         EDIT  (P6,EDWORK),(7,SCINDISP),2,ALIGN=LEFT                            
         B     DXPR123                                                          
*                                                                               
DXPRODP  UNPK  SCINDISP(2),0(2,R7)                                              
         OC    SCINDISP(2),=C'00'                                               
         B     DXPR123                                                          
*                                                                               
*              DISPLAY PAY=NET OR PAY=SNET                                      
*                                                                               
DXPPAY   MVC   SCINDISP(3),=C'NET'                                              
         CLI   0(R7),C'Y'                                                       
         BE    DXPR123                                                          
         MVC   SCINDISP(4),=C'SNET'                                             
         CLI   0(R7),C'S'                                                       
         BE    DXPR123                                                          
         MVC   SCINDISP,SPACES                                                  
         B     DXPR140             NO DISPLAY                                   
*                                                                               
DXPR123  MVC   SCINKWRD(9),XPRFKWRD                                             
         LA    R1,SCINITEM                                                      
         CLI   0(R1),C' '          FIND END OF KEYWORD.                         
         BE    *+12                FOUND IT.                                    
         LA    R1,1(R1)                                                         
         B     *-12                NOT YET.                                     
         MVI   0(R1),C'='          APPEND AN '=' SIGN.                          
         LA    R2,1(R1)            R2 = A(1ST POSTN FOR DISP VALUE).            
         LA    R1,SCINDISP+9       R1 = A(LAST BYTE OF DISP SPACE).             
DXPR125  CLI   0(R1),C' '          FIND END OF DISP VALUE.                      
         BE    *+16                NOT YET.                                     
         CLI   0(R1),C'='                                                       
         BNE   *+12                                                             
         B     DXPR130             VALUE IS BLANKS, DISPLAY ANYWAY.             
         BCT   R1,DXPR125                                                       
*                                                                               
         LA    R0,SCINDISP         FIND SIG L'DISPLAY VALUE.                    
         SR    R1,R0               R1 = THIS LENGTH.                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SCINDISP    APPEND DISP VALUE TO '=' SIGN.               
         LA    R2,1(R1,R2)         R2 = A(1ST BYTE BEYOND APPENDED VALU         
         LA    R0,10               FIND L'DISP SPACE TO BE CLEARED.             
         SR    R0,R1               R0 = THIS LENGTH                             
         BM    DXPR130             L'DISP VALUE = L'DISP SPACE.                 
         LR    R1,R0                                                            
         EX    R1,*+8              CLEAR THE SPACE                              
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
*                                                                               
DXPR130  ZIC   R1,BLKCNT           BUMP THE SCINKEY ITEM COUNT.                 
         LA    R1,1(R1)                                                         
         STC   R1,BLKCNT                                                        
         LA    RE,SCINBLNQ(RE)                                                  
DXPR140  LA    RF,XPRFTBLQ(RF)                                                  
         B     DXPR020                                                          
*                                                                               
DXPR160  ZICM  R0,BLKCNT                                                        
         BZ    OKEXIT              NOTHING TO DISPLAY.                          
         GOTO1 SCINKEY,DMCB,(2,LOGPRFH),(20,SCINBLK),(R0)                       
         OI    LOGPR2H+6,X'81'                                                  
         B     OKEXIT                                                           
         SPACE 1                                                                
         EJECT                                                                  
*              DISPLAY ADDRESS LINES.                                           
*                                                                               
         USING ACADDD,R8                                                        
DISADDR  ZICM  R8,ADACADD,4                                                     
         BZ    OKEXIT              NO ADDRESS                                   
         ZIC   R0,ACADLNES         R0 = N'ADDRESS LINES.                        
         LA    R8,ACADADD          R8 = A(1ST ADDRESS LINE IN ELEMENT).         
         L     RF,FADR             RF = A(1ST ADDRESS LINE ON SCREEN).          
*                                                                               
DADR020  MVC   8(26,RF),0(R8)      MOVE IN THIS LINE.                           
         OI    6(RF),X'81'         TRANSMIT IT.                                 
         BCT   R0,*+8              CARRY ON FOR ALL LINES.                      
         B     OKEXIT              NO MORE LINES.                               
DADR040  ZIC   R1,0(RF)            R1 = L'THIS SCREEN FIELD + HDR.              
         AR    RF,R1               BUMP TO NEXT FIELD.                          
         TM    1(RF),X'20'                                                      
         BO    DADR040             SKIP PROTECTED FIELDS.                       
         LA    R8,26(R8)           R8 = A(NEXT ELEMENT ADDRESS LINE).           
         B     DADR020                                                          
         SPACE 1                                                                
*              DISPLAY ADDITIONAL MATERIAL TO PRINT ON BILLS.                   
*                                                                               
         USING ACPROFD,R8                                                       
DISPRNT  ZICM  R8,ADACPROF,4                                                    
         BZ    OKEXIT                                                           
         MVC   LOGPRNT,ACPRBLPR    MOVE IN THE MATERIAL.                        
         B     OKEXIT                                                           
         SPACE 1                                                                
*              DISPLAY OTHER INFORMATION NARRATIVE.                             
*                                                                               
DISINF   ZICM  R8,ADACPROF,4                                                    
         BZ    OKEXIT                                                           
         CLI   ACPRLEN,105                                                      
         BE    OKEXIT              NO NARRATIVE FOR ACCOUNT.                    
         MVC   LOGINF1,ACPRNARR    MOVE IN 1ST LINE.                            
         CLI   ACPRLEN,155                                                      
         BE    OKEXIT              ONLY ONE NARRATIVE LINE.                     
         MVC   LOGINF2,ACPRNARR+50 MOVE IN 2ND LINE.                            
         OI    LOGINF2H+6,X'81'                                                 
         CLI   ACPRLEN,205                                                      
         BE    OKEXIT              NO THIRD LINE.                               
         MVC   LOGINF3,ACPRNARR+100                                             
         OI    LOGINF3H+6,X'81'                                                 
         B     OKEXIT                                                           
         SPACE 1                                                                
*              DISPLAY BILLING GROUP                                            
*                                                                               
DISBGRP  ZICM  R8,ADACPROF,4                                                    
         BZ    OKEXIT                                                           
         MVC   LOGGRUP,ACPRGRUP                                                 
         B     OKEXIT                                                           
         SPACE 1                                                                
*              DISPLAY BILL NUMBERING SCHEME                                    
*                                                                               
         USING ACNUMD,R8                                                        
DISBNUM  ZICM  R8,ADACNUM,4                                                     
         BZ    OKEXIT                                                           
         LA    RF,LOGNUMB          RF = A(START OF SCREEN FIELD).               
*                                                                               
DNMB020  LA    R1,LOGNUMB                                                       
         CR    RF,R1                                                            
         BE    *+12                FIRST ELEMENT, NO COMMA NEEDED.              
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
*                                                                               
         CLI   ACNUMLEN,14                                                      
         BE    DNMB040             SHORT ELEMENT, NO MEDIA CODE.                
         MVC   0(1,RF),ACNUMTYP    MOVE IN MEDIA CODE.                          
         MVI   1(RF),C'='                                                       
         LA    RF,2(RF)                                                         
*                                                                               
DNMB040  MVC   0(6,RF),ACNUMAFT    MOVE IN THE BILL NUMBER.                     
         LA    RF,6(RF)                                                         
DNMB050  ZIC   R1,ACNUMLEN                                                      
         AR    R8,R1                                                            
         CLI   ACNUMEL,0                                                        
         BE    OKEXIT              NO MORE NUMBERS.                             
         CLI   ACNUMEL,X'21'                                                    
         BE    DNMB020             ANOTHER NUMBER.                              
         B     DNMB050                                                          
         SPACE 2                                                                
*        D I S P L A Y   J O B   C L O S I N G   D A T E                        
*                                                                               
         USING ACJOBD,R8                                                        
DISCLOSD ZICM  R8,ADACJOB,4                                                     
         BZ    OKEXIT                                                           
         OI    LOGCLOSH+6,X'80'    TRANSMIT---OUTPUT INDICATOR                  
         MVC   LOGCLOS,SPACES      BLANK-OUT OPENED/CLOSEDINPUT AREA            
         LA    R2,LOGCLOS-1        NEEDED BY DATCON                             
         LA    R6,ACJBCLOS         ASSUMES NO OPENED DATE                       
         CLI   ACJBLEN,ACJBLNQ2    Q, EL-26 TOO SMALL FOR OPENED DATE           
         BL    DCL200               Y, DISPLAY ONLY CLOSED DATE                 
         NC    ACJBOPND,ACJBOPND   Q, AN OPENED DATE                            
         BZ    DCL200               N, DISPLAY ONLY CLOSED DATE                 
         LA    R6,ACJBOPND          Y, POINTS TO OPENED DATE FIELD              
DCL200   GOTO1 DATCON,DMCB,(1,0(R6)),(8,1(R2))                                  
         LA    R1,ACJBCLOS         POINTS TO CLOSED DATE FIELD                  
         CR    R1,R6               Q, CLOSED DATE PROCESSED                     
         BE    OKEXIT               Y, EXIT---DONE                              
         LR    R6,R1               TO PROCESS CLOSED DATE                       
         LA    R2,6(R2)            OUTPUT OPENED DATE AT LEAST 6-BYTES          
DCL300   LA    R2,1(R2)            LOOKING FOR NEXT BLANK                       
         CLI   0(R2),C' '          Q, A BLANK                                   
         BNE   DCL300               N                                           
         MVI   0(R2),C','           Y, MOVE COMA                                
         B     DCL200                                                           
         USING SCINBLKD,RE                                                      
         SPACE 2                                                                
*        D I S P L A Y   J O B   C O M M E N T S                                
*                                                                               
         USING ACOMMD,R8                                                        
DISCOM   ZICM  R8,ADACCOMM,4                                                    
         BZ    OKEXIT                                                           
         LA    RF,LOGCOM1H                                                      
         LA    R0,3                                                             
         MVI   DUB,C'N'            N = NORMAL COMMENTS IN THIS LINE             
         MVI   DUB+1,1             NOTHING YET IN LINE.                         
         LA    R7,8(RF)            R7 = A(1ST FREE BYTE IN LYN).                
*                                                                               
DCOM020  CLI   ACOMEL,0                                                         
         BE    OKEXIT                                                           
         CLI   ACOMEL,X'3E'                                                     
         BE    DCOM040             FOUND A COMMENT ELEMENT.                     
DCOM030  ZIC   R1,ACOMLEN                                                       
         AR    R8,R1                                                            
         B     DCOM020                                                          
*                                                                               
DCOM040  CLI   ACOMTYPE,C'M'                                                    
         BE    *+12                                                             
         CLI   ACOMTYPE,0                                                       
         BNE   DCOM080                                                          
         CLI   DUB,C'N'            OLD STYLE COMMENTS                           
         BE    DCOM060             LINE IS FRESH.                               
         BAS   RE,BUMPER           GET NEXT LINE.                               
         MVI   DUB,C'N'                                                         
DCOM060  ZIC   R1,ACOMLEN                                                       
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),ACOMMENT    DISPLAY THE COMMENT.                         
         BAS   RE,BUMPER           NEW LINE.                                    
         B     DCOM030                                                          
*                                                                               
DCOM080  LA    RE,ACOMMENT         ESTABLISH L'COMMENT NUMBER LESS              
         LA    R1,6                LEADING SPACES.                              
DCOM084  CLI   0(RE),C' '                                                       
         BNE   DCOM086                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,DCOM084                                                       
         B     DCOM030                                                          
*                                                                               
DCOM086  MVI   DUB,C'Y'            MARK AS NEW STYLE COMMENT LINE.              
         STC   R1,DUB+2            R1 = L'COMMENT NO. AS ABOVE.                 
         ST    RE,DUB+4            RE = A(1ST SIGNIFICANT BYTE OF NO.)          
         AH    R1,=H'5'            R1 = TOTAL L'DISPLAYED ITEM.                 
         AR    R1,R7               R1 = A(LAST BYTE + 1 OF DSPLYD ITEM)         
         LR    RE,RF               RE = A(HEADER)                               
         SR    R1,RE                                                            
         CH    R1,=H'58'                                                        
         BNH   *+8                 SUFFICIENT ROOM ON THIS LINE.                
         BAS   RE,BUMPER           NO ROOM, BUMP TO NEXT LYN.                   
*                                                                               
         CLI   DUB+1,1                                                          
         BE    *+12                1ST ITEM ON LINE, NO COMMA                   
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
         MVI   DUB+1,2             NO LONGER VIRGIN LINE.                       
         MVC   0(4,R7),=C'EST='    SET TYPE OF COMMENT.                         
         TM    ACOMTYPE,X'40'                                                   
         BO    *+10                                                             
         MVC   0(3,R7),=C'BIL'                                                  
         TM    ACOMTYPE,X'C0'                                                   
         BM    *+10                                                             
         MVC   0(3,R7),=C'B+E'                                                  
         L     RE,DUB+4                                                         
         ZIC   R1,DUB+2                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R7),0(RE)       DISPLAY NUMBER.                              
         LA    R7,5(R1,R7)                                                      
         B     DCOM030                                                          
*                                                                               
BUMPER   OI    6(RF),X'80'                                                      
         IC    R1,0(RF)            DISPLAY THE LINE.                            
         AR    RF,R1               BUMP TO NEXT                                 
         LA    R7,8(RF)                                                         
         MVI   DUB+1,1                                                          
         BCT   R0,*+8                                                           
         B     OKEXIT              END IF NO MORE LINES.                        
         BR    RE                                                               
         EJECT                                                                  
         USING XPRFTABD,RF                                                      
VALXPROF XC    ELEMENT(24),ELEMENT INITIALIZE NEW ELEMENT.                      
         MVC   ELEMENT(2),=X'3C18'                                              
         BAS   RE,ELREMOVE                                                      
         LA    RF,XPRFTAB                                                       
         LA    R8,ELEMENT                                                       
         MVI   USERIP,C'N'                                                      
*                                  SET ELEMENT TO DEFAULT VALUES.               
VXPR020  CLI   XPRFKWRD,X'FF'                                                   
         BE    VXPR120             END OF FULL LENGTH ELEMENT.                  
         CLI   XPRFKWRD,X'FE'      SKIP END-OF-SHORT-ELEMENT MARKER.            
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         ZICM  R7,XPRFAFLD,2                                                    
         LA    R7,0(R7,R8)         R7 = A(FIELD IN ELEMENT)                     
         ZIC   R1,XPRFLFLD                                                      
         TM    XPRFSTAT,BITS                                                    
         BO    VXPR080             FIELD IS BIT SIGNIFICANT.                    
         TM    XPRFSTAT,PACKED                                                  
         BO    VXPR060             FIELD IS PACKED.                             
         EX    R1,*+8              MOVE IN EBCDIC DEFAULT.                      
         B     VXPR100                                                          
         MVC   0(0,R7),XPRFDEFV                                                 
*                                                                               
VXPR060  LR    R0,R1                                                            
         SLL   R0,4                                                             
         OR    R1,R0                                                            
         EX    R1,*+8              ZAP IN PACKED DEFAULT.                       
         B     VXPR100                                                          
         ZAP   0(0,R7),XPRFDEFV(0)                                              
*                                                                               
VXPR080  ZIC   R1,XPRFDEFV                                                      
         EX    R1,*+8              OR ON BIT DEFAULT.                           
         B     VXPR100                                                          
         OI    0(R7),0                                                          
*                                                                               
VXPR100  LA    RF,XPRFTBLQ(RF)                                                  
         B     VXPR020             NEXT FIELD/VALUE.                            
*                                                                               
VXPR120  B     VXPR122                                                          
*        CLC   LOGREC(2),=C'MJ'    FOR JOBS, CHECK IF CLIENT FORCES             
*        BNE   VXPR122             UNAPPROVED ESTIMATE ON ADDING                
*        CLI   LOGACT,C'N'         A JOB                                        
*        BNE   VXPR122                                                          
*        TM    CLXPST1,X'02'                                                    
*        BZ    VXPR122             IT DOESN'T                                   
*        OI    (ACXPST1-ACXPROFD)(R8),X'04'                                     
*                                                                               
VXPR122  MVI   BLKCNT,0            INITIALIZE ENTRY COUNT.                      
         CLI   LOGPRFH+5,0                                                      
         BNE   VXPR122A            I/P IN FIRST FIELD.                          
         CLI   LOGPR2H+5,0                                                      
         BNE   VXPR140                                                          
         CLC   LOGREC(2),=C'MJ'    NEW JOB MAY BE FORCING EST=UNAPP             
         BNE   OKEXIT                                                           
         CLI   LOGACT,C'N'                                                      
         BNE   OKEXIT                                                           
         CLI   GONEWJUN,C'Y'       TEST IF FORCING UNAPPROVED                   
         BE    VXPR290             YES-DISPLAY IT ON SCREEN                     
         B     OKEXIT                                                           
*                                                                               
         B     VXPR140             I/P IN 2ND FIELD ONLY.                       
VXPR122A GOTO1 SCANNER,DMCB,LOGPRFH,SCINBLK                                     
         ZICM  R0,DMCB+4           R0 = N'ENTRIES IN 1ST FIELD.                 
         BNZ   *+12                                                             
VXPR124  MVI   ERROR,INVALID       BAD I/P.                                     
         B     ERREXIT                                                          
         STC   R0,BLKCNT           SAVE N'ENTRIES IN 1ST FIELD.                 
*                                                                               
VXPR140  CLI   LOGPR2H+5,0                                                      
         BE    VXPR170             NO I/P IN 2ND FIELD.                         
         ZIC   R0,BLKCNT                                                        
         MH    R0,=H'32'                                                        
         LA    R1,SCINBLK                                                       
         AR    R0,R1               R0 = A(NEXT FREE POSITION IN BLOCK).         
         GOTO1 SCANNER,DMCB,LOGPR2H,(R0)                                        
         ZICM  R0,DMCB+4                                                        
         BNZ   VXPR160                                                          
         LA    RF,LOGPR2H         BAD I/P IN 2ND FIELD, PASS A(FLDHDR)          
         ST    RF,FADR             FOR CURSOR POSITION.                         
         B     VXPR124                                                          
*                                                                               
VXPR160  ZIC   RF,BLKCNT           ADD N'ENTRIES IN 2ND FIELD TO                
         AR    RF,R0               N'FROM 1ST FIELD.                            
         STC   RF,BLKCNT                                                        
*                                                                               
VXPR170  LA    RE,SCINBLK          RE = A(SCANNER TABLE OF ENTRIES)             
VXPR172  LA    RF,XPRFTAB          RF = A(XTRA PROFILE VALUES).                 
         MVI   ERROR,INVALID                                                    
VXPR174  CLI   XPRFKWRD,X'FF'                                                   
         BE    ERREXIT             END OF TABLE.                                
         CLI   XPRFKWRD,X'FE'                                                   
         BNE   *+8                                                              
         LA    RF,1(RF)            SKIP END OF SHORT LMNT MARKER.               
         ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,VXPRKCLC         CLC   12(0,RE),XPRFKWRD                      
         BNE   VXPR260             NO MATCH ON XPROF KEYWORD                    
         ZICM  R7,XPRFAFLD,2                                                    
         LA    R7,0(R7,R8)         R7 = A(FIELD IN XPROF LMNT).                 
         TM    XPRFSTAT,FLDVALQ                                                 
         BZ    VXPR180             NO SPECIAL VALIDATION ROUTINE.               
         ZICM  R1,XPRFVRTN,3                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         A     R1,WRELO            R1 = PROPER A(VALIDATION ROUTINE)            
         BR    R1                                                               
*                                                                               
VXPR180  ZIC   R1,1(RE)                                                         
         BCTR  R1,0                R1 = L'I/P VALUE - 1.                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,RE),XPRFDSPV                                                
         BE    VXPR200                                                          
         MVI   ERROR,INVALID                                                    
         B     VXPR260                                                          
*                                                                               
VXPR200  TM    XPRFSTAT,BITS                                                    
         BO    VXPR240             BIT SIGNIFICANT DATA.                        
         ZIC   R1,XPRFLFLD         R1 = L'XPROF ELMNT FIELD - 1                 
         TM    XPRFSTAT,PACKED                                                  
         BO    VXPR220             PACKED DATA.                                 
         EX    R1,*+8              EBCDIC DATA.                                 
         B     VXPR280                                                          
         MVC   0(0,R7),XPRFOTHV    MOVE IN THE VALUE.                           
*                                                                               
VXPR220  LR    R0,R1               REPLICATE LNTH MASK FOR ZAP.                 
         SLL   R0,4                                                             
         OR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     VXPR280                                                          
         ZAP   0(0,R7),XPRFOTHV(0)                                              
*                                                                               
VXPR240  ZIC   R1,XPRFDEFV+1       R1 = BIT VALUE TO SET.                       
         EX    R1,*+8                                                           
         B     VXPR280                                                          
         OI    0(R7),0                                                          
*                                                                               
VXPR260  LA    RF,XPRFTBLQ(RF)     TRY NEXT KEYWORD.                            
         B     VXPR174                                                          
*                                                                               
VXPR280  LA    RE,32(RE)           NEXT I/P ITEM.                               
         ZIC   R0,BLKCNT                                                        
         BCT   R0,VXPR300                                                       
         BAS   RE,ELADDIN                                                       
         BNE   ERREXIT                                                          
VXPR290  CLC   LOGREC(2),=C'MJ'    IF MJOB AND EST=UNAPP                        
         BNE   OKEXIT                                                           
         CLI   LOGACT,C'N'         TEST ADDING JOB                              
         BNE   OKEXIT                                                           
         CLI   USERIP,C'Y'         WAS IT INPUT BY USER                         
         BE    OKEXIT              YES                                          
         CLI   GONEWJUN,C'Y'       TEST NEW JOBS ARE UNAPPROVED                 
         BNE   OKEXIT                                                           
         SPACE 1                                                                
         LA    RE,LOGPR2H          NO, SO DISPLAY IT BEFORE EXITTING            
         ZIC   R0,0(RE)            FIELD LENGTH                                 
         ZIC   R1,5(RE)            WHAT'S ALREADY IN FIELD                      
         LA    RE,8(RE)            START OF UNPROTECTED FIELD                   
         CH    R1,=H'0'            ANYTHING ELSE ON LINE                        
         BE    VXPR295             NO                                           
         AR    RE,R1               FIRST AVAILABLE SLOT IN FIELD                
         AH    R1,=H'18'           HOW MUCH IS NEEDED (HDR+EST=...)             
         CR    R1,R0               DO WE HAVE ENOUGH ROOM                       
         BH    OKEXIT              NO                                           
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
VXPR295  MVC   0(9,RE),=C'EST=UNAPP'                                            
         OI    LOGPR2H+6,X'80'     TRANSMIT FIELD                               
         B     OKEXIT                                                           
VXPR300  STC   R0,BLKCNT                                                        
         ZIC   R0,FNDX             BUMP FIELD INDEX NUMBER.                     
         AH    R1,=H'1'                                                         
         STC   R0,FNDX                                                          
         B     VXPR172                                                          
         SPACE 1                                                                
DUEVAL   TM    3(RE),X'80'         TEST INDICATOR FOR VALID NUMERIC.            
         BZ    ERREXIT             FAILED.                                      
         CLI   1(RE),2                                                          
         BH    ERREXIT             I/P IS TOO LONG.                             
         ZICM  R0,8(RE),4          R0 = BINARY VALUE OF I/P.                    
DUVL040  CVD   R0,DUB                                                           
         ZIC   R1,XPRFLFLD         R1 = L'FIELD IN ELEMENT - 1.                 
         SLL   R1,4                POSITION LNTH MASK FOR ZAP.                  
         EX    R1,*+8                                                           
         B     VXPR280                                                          
         ZAP   0(0,R7),DUB                                                      
*                                                                               
CSHVAL   ZIC   R0,1(RE)            R0 = L'INPUT ITEM.                           
         STM   RE,RF,SAVEREF                                                    
         GOTO1 CASHVAL,DMCB,22(RE),(R0)                                         
         CLI   DMCB,X'FF'                                                       
         BE    ERREXIT             BAD CASH I/P.                                
         LM    RE,RF,SAVEREF                                                    
         ZICM  R0,DMCB+4,4         R0 = BINARY VALUE OF I/P                     
         B     DUVL040                                                          
*                                                                               
VESTUNAP CLC   LOGREC(2),=C'MJ'                                                 
         BNE   ERREXIT                                                          
         MVI   USERIP,C'Y'         USER INPUT EST=UNAPP                         
         B     VUNAPP20                                                         
VJOBUNAP CLC   LOGREC(2),=C'MC'                                                 
         BNE   ERREXIT                                                          
VUNAPP20 ZIC   R1,1(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,RE),XPRFDSPV                                                
         BNE   ERREXIT                                                          
         CLI   USERIP,C'Y'         TEST FOR EST=UNAPP                           
         BE    *+10                                                             
         OC    0(1,R7),XPRFDEFV+1                                               
         B     VXPR280                                                          
*                                                                               
*              PAY=NET OR SNET                                                  
*                                                                               
VPAY     CLC   LOGREC(2),=C'MC'                                                 
         BNE   ERREXIT                                                          
         CLI   1(RE),3                                                          
         BNE   VPAY4                                                            
         CLC   22(3,RE),=C'NET'                                                 
         BNE   ERREXIT                                                          
         MVI   0(R7),C'Y'                                                       
         B     VXPR280                                                          
VPAY4    CLI   1(RE),4                                                          
         BNE   ERREXIT                                                          
         CLC   22(4,RE),=C'SNET'                                                
         BNE   ERREXIT                                                          
         MVI   0(R7),C'S'                                                       
         B     VXPR280                                                          
*                                                                               
*              %EST=T OR E                                                      
*                                                                               
VESTBILL CLI   1(RE),1                                                          
         BNE   ERREXIT                                                          
         CLI   22(RE),C'E'                                                      
         BE    *+12                                                             
         CLI   22(RE),C'T'                                                      
         BNE   ERREXIT                                                          
         MVC   0(1,R7),22(RE)                                                   
         B     VXPR280                                                          
*                                                                               
*              FILT2=?                                                          
*                                                                               
VFILT2   CLI   1(RE),1                                                          
         BNE   ERREXIT                                                          
         MVC   0(1,R7),22(RE)                                                   
         B     VXPR280                                                          
*                                                                               
VXPRKCLC CLC   12(0,RE),XPRFKWRD   COMPARE KEYWORD.                             
VXPRBC   BC    0,VXPR200                                                        
         DROP  RF                                                               
         SPACE 1                                                                
*              VALIDATE BILLING TYPE.                                           
*                                                                               
         USING ACPROFD,R8                                                       
         USING BILTTBLD,RE                                                      
VALBTYP  LA    RE,IO2                                                           
         ST    RE,ARECAREA         POINT ARECAREA TO PRODUCTION RECORD.         
         ZICM  R8,ADACPROF,4                                                    
         ZICM  R1,LOGTYPEH+5                                                    
         BNZ   VLBT010                                                          
         MVI   ACPRBILL,0                                                       
         XC    ACPRBLAM,ACPRBLAM                                                
         CLI   TYPELVL,1           FOR CLIENT RECORD, BILLING TYPE              
         BNE   OKEXIT              DEFAULTS TO PROGRESSIVE.                     
         MVI   ACPRBILL,C'P'                                                    
         B     OKEXIT                                                           
VLBT010  BCTR  R1,0                                                             
         LA    RE,BILTTBL                                                       
         MVI   ERROR,INVALID                                                    
*                                                                               
VLBT020  CLI   BILTCOD,X'FF'                                                    
         BE    VLBT060                                                          
         TM    BILTTYP,MOVER                                                    
         BZ    VLBT040                                                          
         EX    R1,VLBTCLC          CLC   BILTNAR(0),LOGTYPE                     
         BNE   VLBT040                                                          
         MVC   ACPRBILL,BILTCOD                                                 
         MVC   LOGTYPE,BILTNAR     REDISPLAY THE KEYWORD.                       
         B     OKEXIT                                                           
VLBT040  LA    RE,BILTTBLQ(RE)                                                  
         B     VLBT020                                                          
*                                                                               
VLBT060  LA    R2,LOGTYPE                                                       
         CLC   LOGTYPE(3),=C'FEE'                                               
         BE    VLBT080             TYPE IS FEE                                  
         LA    RE,LOGTYPE(R1)      RE = A(LAST CHAR I/P).                       
         TM    0(RE),X'F0'         IF THIS IS NUMERIC, TYPE IS SPECIAL,         
         BNO   VLBT100             ELSE IT'S ESTIMATE.                          
         MVI   ACPRBILL,C'S'                                                    
         LA    R0,1(R1)                                                         
         B     VLBT140                                                          
*                                                                               
VLBT080  MVI   ACPRBILL,C'F'       F = FEE.                                     
         LR    R0,R1                                                            
         SH    R0,=H'2'            R0 = L'NUMBER                                
         LA    R2,3(R2)            R2 = START OF NUMBER.                        
         B     VLBT140                                                          
*                                                                               
VLBT100  LA    RE,LOGTYPE                                                       
         ZIC   R1,LOGTYPEH+5                                                    
         LR    R0,R1                                                            
         SR    R3,R3                                                            
VLBT101  CLI   0(RE),C'%'        LOOK FOR PERCENT SIGN IN NUMBER                
         BE    VLBT120                                                          
         LA    R3,1(R3)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,VLBT101                                                       
         B     ERREXIT           UNKNOWN TYPE                                   
*                                                                               
VLBT120  LTR   R3,R3             PERCENT SIGN WAS FIRST                         
         BZ    ERREXIT                                                          
         LR    R0,R3             LENGTH UP TO PERCENT FOR CASHVAL               
         SR    R1,R3             GIVES LENGTH FOR EX                            
         BNP   ERREXIT                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=C'%ESTIMATE'                                            
         BNE   ERREXIT             NOTHING ELSE IS ACCEPTABLE.                  
VLBT130  MVI   ACPRBILL,C'E'                                                    
*                                                                               
VLBT140  GOTO1 CASHVAL,DMCB,(R2),(R0)                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERREXIT                                                          
         MVC   ACPRBLAM,DMCB+4                                                  
         CLI   ACPRBILL,C'E'                                                    
         BNE   OKEXIT                                                           
         L     R1,DMCB+4                                                        
         C     R1,=F'10000'  PERCENT EST. CAN'T BE MORE THAN                    
         BH    ERREXIT             100.00 PERCENT                               
         C     R1,=F'-10000'  OR LESS THAN -100.00                              
         BL    ERREXIT                                                          
         B     OKEXIT                                                           
VLBTCLC  CLC   BILTNAR(0),LOGTYPE                                               
         DROP  RE                                                               
         SPACE 1                                                                
*              VALIDATE NON-BILLABLE WORK CODES.                                
*                                                                               
VALNONB  ZICM  R8,ADACPROF,4                                                    
         MVC   ACPRUNBL,SPACES                                                  
         MVC   ACPRUNBL,CLPRUNBL                                                
         ZICM  R0,LOGNONBH+5                                                    
         BZ    OKEXIT                                                           
         LA    RF,ACPRUNBL                                                      
         LA    RE,LOGNONB                                                       
         MVI   ERROR,INVALID                                                    
*                                                                               
VLNB020  MVC   0(1,RF),0(RE)       MOVE IN 1ST CHAR OF CODE.                    
         BCT   R0,*+8                                                           
         B     OKEXIT              NO MORE I/P.                                 
         CLI   1(RE),C','                                                       
         BE    VLNB040             L'THIS CODE = 1.                             
         MVC   1(1,RF),1(RE)       MOVE IN 2ND CHAR OF I/P.                     
         BCT   R0,*+8                                                           
         B     OKEXIT                                                           
         CLI   2(RE),C','                                                       
         BNE   ERREXIT             INVALID FORMAT.                              
         LA    RE,3(RE)                                                         
VLNB030  LA    RF,2(RF)                                                         
         BCT   R0,VLNB020                                                       
         B     OKEXIT                                                           
*                                                                               
VLNB040  LA    RE,2(RE)                                                         
         B     VLNB030                                                          
         SPACE 1                                                                
*              VALIDATE BILLING GROUP.                                          
*                                                                               
VALGRUP  ZICM  R8,ADACPROF,4                                                    
         MVC   ACPRGRUP,SPACES                                                  
         ZICM  R1,LOGGRUPH+5                                                    
         BZ    OKEXIT                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   ACPRGRUP(0),LOGGRUP                                              
         SPACE 1                                                                
*              VALIDATE PRINT ON BILL INFORMATION.                              
*                                                                               
VALPRNT  ZICM  R8,ADACPROF,4                                                    
         MVC   ACPRBLPR,SPACES                                                  
         ZICM  R1,LOGPRNTH+5                                                    
         BZ    OKEXIT                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   ACPRBLPR(0),LOGPRNT                                              
         SPACE 1                                                                
*              VALIDATE OTHER INFORMATION                                       
*                                                                               
VALINF   ZICM  R8,ADACPROF,4                                                    
         MVI   ELEMENT+105,C' '    CLEAR NARRATIVE PORTION OF NEW LMNT.         
         MVC   ELEMENT+106(149),ELEMENT+105                                     
         MVC   ELEMENT(105),ACPREL PRESERVE START OF OLD ELEMENT.               
         LA    R8,ELEMENT                                                       
         BAS   RE,ELREMOVE                                                      
         LA    RF,ACPRNARR         RF = A(START OF NARRATIVE).                  
         LA    RE,LOGINF1H         RE = A(1ST INFO HEADER).                     
         LA    R0,3                R0 = N'INFO LINES POSSIBLE.                  
*                                                                               
VALI040  ZICM  R1,5(RE)            R1 = L'THIS LINE.                            
         BZ    VALI060                                                          
         BCTR  R1,0                                                             
         EX    R1,VALIMVC          MVC   0(0,R8),8(RE)                          
         LA    RF,50(RF)           RF = A(NEXT LYN POST'N IN EL).               
         IC    R1,0(RE)            R1 = L'HDR + L'FLD.                          
         AR    RE,R1               RE = A(NEXT HDR)                             
         BCT   R0,VALI040                                                       
*                                                                               
VALI060  LA    RE,3                DERIVE N'LINES I/P.                          
         SR    RE,R0               TOTAL L'INPUT IS N'LINES * 50.               
         MH    RE,=H'50'                                                        
         AH    RE,=H'105'          TOTAL L'ELEMENT = L'INPUT + 105.             
         STC   RE,ACPRLEN                                                       
         BAS   RE,ELADDIN          ADD THE ELEMENT.                             
         BNE   ERREXIT                                                          
         B     OKEXIT                                                           
VALIMVC  MVC   0(0,RF),8(RE)                                                    
         SPACE 1                                                                
*              VALIDATE BILL NUMBERING SCHEME                                   
*                                                                               
         USING ACNUMD,R8                                                        
VALBNUM  MVI   ELEMENT,X'21'                                                    
         BAS   RE,ELREMOVE                                                      
         CLI   LOGNUMBH+5,0                                                     
         BE    OKEXIT                                                           
         MVI   ERROR,INVALID       REFRESH ERROR MESSAGE                        
         GOTO1 SCANNER,DMCB,LOGNUMBH,(10,BLOCK)                                 
         ZICM  R0,DMCB+4                                                        
         BZ    ERREXIT                                                          
         LA    RF,BLOCK                                                         
         LA    R8,ELEMENT                                                       
         MVI   FNDX,1                                                           
VBNM040  MVI   ACNUMLEN,14                                                      
         MVI   ACNUMBEF,C'0'                                                    
         MVC   ACNUMBEF+1(5),ACNUMBEF                                           
         CLI   0(RF),1                                                          
         BNE   VBNM060                                                          
         MVI   ACNUMLEN,15                                                      
         MVC   ACNUMTYP,12(RF)                                                  
         CLI   1(RF),6                                                          
         BNE   VBNM080                                                          
         TM    3(RF),X'80'                                                      
         BZ    ERREXIT                                                          
         MVC   ACNUMBEF,22(RF)                                                  
         B     VBNM084                                                          
*                                                                               
VBNM060  CLI   0(RF),6                                                          
         BNE   ERREXIT                                                          
         TM    2(RF),X'80'                                                      
         BZ    ERREXIT                                                          
         MVC   ACNUMBEF,12(RF)                                                  
         B     VBNM084                                                          
*                                                                               
VBNM080  CLI   1(RF),0                                                          
         BNE   ERREXIT                                                          
VBNM084  MVC   ACNUMAFT,ACNUMBEF                                                
         BAS   RE,ELADDIN                                                       
         BNE   ERREXIT                                                          
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    RF,32(RF)                                                        
         BCT   R0,VBNM040                                                       
         MVI   FNDX,0                                                           
         B     OKEXIT                                                           
         DROP  R8                                                               
         SPACE 1                                                                
*              VALIDATE ADDRESS INPUT                                           
*                                                                               
         USING ACADDD,R8                                                        
VALADDR  MVI   ELEMENT,X'22'                                                    
         BAS   RE,ELREMOVE                                                      
         CLI   LOGADD1H+5,0                                                     
         BE    OKEXIT                                                           
         MVI   ELEMENT+1,C' '                                                   
         MVC   ELEMENT+2(253),ELEMENT+1                                         
         LA    R8,ELEMENT                                                       
         LA    RF,ACADADD                                                       
         LA    RE,LOGADD1H                                                      
         LA    R0,4                                                             
*                                                                               
VLAD020  ZICM  R1,5(RE)                                                         
         BZ    VLAD040                                                          
         BCTR  R1,0                                                             
         EX    R1,VLADMVC                                                       
         LA    RF,26(RF)                                                        
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         TM    1(RE),X'20'                                                      
         BO    *-10                SKIP PROTECTED FIELDS.                       
         BCT   R0,VLAD020                                                       
*                                                                               
VLAD040  LA    RE,4                                                             
         SR    RE,R0                                                            
         STC   RE,ACADLNES                                                      
         MH    RE,=H'26'                                                        
         AH    RE,=H'3'                                                         
         STC   RE,ACADLEN                                                       
         BAS   RE,ELADDIN                                                       
         BNE   ERREXIT                                                          
         B     OKEXIT                                                           
VLADMVC  MVC   0(0,RF),8(RE)                                                    
         SPACE 2                                                                
*        V A L I D A T E   C L O S I N G   D A T E                              
*                                                                               
         USING ACJOBD,R8                                                        
VALCLOS  ICM   R8,15,ADACJOB       Q, A JOB EL-26                               
         BZ    VCL50                NO                                          
         NI    ACJBSTAT,X'FF'-X'80' TURN OFF EST=UNAPP BIT                      
         CLI   USERIP,C'Y'         TEST USER I/P EST=UNAPP                      
         BNE   *+8                                                              
         OI    ACJBSTAT,X'80'       YES                                         
         B     VCL200               Y, GO AMEND ELEMENT                         
         SPACE 1                                                                
VCL50    L     RE,ARECAREA          N, CHECK IF NEW RECORD FLAG ON              
         TM    0(RE),X'80'         Q, A NEW JOB RECORD                          
         BNZ   VCL100               Y, ADD NEW JOB EL-26                        
         DC    H'0'                 N, BLOW-UP                                  
         SPACE 1                                                                
VCL100   LA    R8,ELEMENT          POINTS TO ELEMENT AREA                       
         XC    ELEMENT,ELEMENT     CLEARS ELEMENT AREA                          
         MVI   ACJBEL,X'26'        MOVE ELEMENT ID-26                           
         GOTO1 GETFACT,DMCB,0      GET FACILITIES INFO                          
         L     RE,DMCB             POINTS TO FACILITIES INFO BLOCK              
         USING FACTSD,RE                                                        
         MVC   DUB,FADATE          MOVE CURRENT DATE---YMD(BINARY)              
         DROP  RE                                                               
         GOTO1 DATCON,DMCB,(4,DUB),(1,ACJBSTRT)                                 
         CLI   USERIP,C'Y'         TEST USER INPUT EST=UNAPP                    
         BE    *+12                YES                                          
         CLI   GONEWJUN,C'Y'       TEST NEW JOBS UNAPPROVED                     
         BNE   *+8                                                              
         OI    ACJBSTAT,X'80'                                                   
         SPACE 1                                                                
VCL200   MVI   ERROR,MISSING       ASSUMES MISSING ERROR                        
         LA    R2,LOGCLOSH         POINTS TO FIELD HDR                          
         CLI   5(R2),0             Q, ZERO INPUT LENGTH---ERROR                 
         BE    ERREXIT              Y, EXIT---ERROR                             
         XC    WORK(3),WORK        WILL CONTAIN OPENED DATE OR ZEROES           
         MVI   ERROR,DATERR        ASSUMES DATE ERROR---BAD DATE                
         SPACE 1                                                                
VCL300   GOTO1 DATVAL,DMCB,8(R2),DUB  DUB CONTAINS VALID-DATE YYMMDD            
         OC    DMCB,DMCB           Q, VALID DATE--DMCB LEN-OF VAL-DATE          
         BZ    ERREXIT              N, ERROR---EXIT                             
         A     R2,DMCB              Y, R2 MAY NOW POINT TO COMA                 
         GOTO1 DATCON,DMCB,(0,DUB),(1,ACJBCLOS)                                 
         CLI   8(R2),C','          Q, A COMA---2ND DATE---CLOSED DATE           
         BNE   VCL400               N, ONLY ONE DATE---CLOSED DATE              
         LA    R2,1(R2)             Y, POINT PAST COMA                          
         MVC   WORK(3),ACJBCLOS    SAVES OPENED DATE IN WORK                    
         B     VCL300              PROCESS 2ND DATE---CLOSED DATE               
VCL400   CLC   WORK(3),ACJBCLOS    Q, OPENED LOWER THAN CLOSED DATE             
         BNL   ERREXIT              N, EXIT---ERROR                             
         CLI   ACJBLEN,0           Q, ZERO LENGTH---NEW JOB ELEMENT             
         BE    VCL800               Y, ADD NEW ELEMENT                          
         OC    WORK(3),WORK        Q, AN OPENED DATE                            
         BZ    VCL500               N, CLOSED DATE ONLY                         
         CLI   ACJBLEN,ACJBLNQ2    Q, LONG ENOUGH ELEMENT                       
         BL    VCL600               N, LONGER EL NEEDED FOR OPENED DATE         
         MVC   ACJBOPND,WORK        Y, MOVE OPENED DATE                         
         B     OKEXIT              EXIT---NO EXPANSION NEEDED                   
         SPACE 1                                                                
VCL500   CLI   ACJBLEN,ACJBLNQ2    Q, ROOM FOR OPENED DATE                      
         BL    OKEXIT               N, THATS OK---EXIT                          
         MVC   ACJBOPND,WORK       SET OPENED DATE TO HEX ZEROES                
         B     OKEXIT              EXIT---NO EXPANSION NEEDED                   
         SPACE 1                                                                
VCL600   XC    ELEMENT,ELEMENT     RESETS NEW EL OUTPUT AREA                    
         MVC   VCL700+1(1),ACJBLEN TO MOVE OLD SHORTER EL                       
VCL700   MVC   ELEMENT(0),ACJOBD   MOVES OLD ELEMENT TO EL WORK AREA            
         BAS   RE,ELREMOVE         REMOVES OLD JOB ELEMENT-26                   
         LA    R8,ELEMENT          NEEDED FOR NEXT FEW INSTRUCTIONS             
VCL800   MVI   ACJBLEN,ACJBLNQ3    MAKE EXPANDED EL MAX LENGTH--JDILLON         
         MVC   ACJBOPND,WORK       MOVE OPENED DATE TO LONGER EL-26             
         BAS   RE,ELADDIN          ADD LARGER SIZE ELEMENT                      
         BNE   ERREXIT                                                          
         B     OKEXIT                                                           
         DROP  R8                                                               
         EJECT                                                                  
*        V A L I D A T E   J O B   C O M M E N T S                              
*                                                                               
         USING ACOMMD,R8                                                        
         USING LOGCOM1H,R7                                                      
VALCOM   XC    ELEMENT,ELEMENT     FOR 1ST LINE, REMOVE COMMENT                 
         MVI   ELEMENT,X'3E'       ELEMENTS.                                    
         BAS   RE,ELREMOVE                                                      
*                                                                               
VALCOM2  LA    R8,ELEMENT                                                       
         L     R7,FADR                                                          
         CLI   LOGCOM1H+5,0                                                     
         BE    OKEXIT                                                           
         MVI   ACOMTYPE,0                                                       
         GOTO1 SCANNER,DMCB,LOGCOM1H,BLOCK,0                                    
         ZICM  R0,DMCB+4                                                        
         BZ    VCOM020                                                          
         CLI   BLOCK+1,0                                                        
         BNE   VCOM040                                                          
*                                                                               
VCOM020  ZIC   R1,LOGCOM1H+5       SIMPLE COMMENT LINE.                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACOMMENT(0),LOGCOM1                                              
         LA    R1,5(R1)                                                         
         STC   R1,ACOMLEN          SET ELEMENT LENGTH.                          
         BAS   RE,ELADDIN                                                       
         BNE   ERREXIT                                                          
         ZIC   R1,ACOMSEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ACOMSEQ                                                       
         B     OKEXIT                                                           
         DROP  R7                                                               
*                                                                               
VCOM040  LA    R7,BLOCK            R7 = A(OUTPUT FROM SCANNER)                  
         XC    KEY,KEY             CLEAR KEY FOR COMMENT RECORDS.               
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),COMPANY                                                 
VCOM050  MVI   ACOMLEN,10                                                       
         MVI   ACOMTYPE,X'44'                                                   
         CLC   12(3,R7),=C'EST='                                                
         BE    VCOM060                                                          
         MVI   ACOMTYPE,X'84'                                                   
         CLC   12(3,R7),=C'BIL'                                                 
         BE    VCOM060                                                          
         CLC   12(3,R7),=C'B+E'                                                 
         BNE   VCOMERR                                                          
         OI    ACOMTYPE,X'40'                                                   
*                                                                               
VCOM060  MVC   ACOMMENT,SPACES                                                  
         ZICM  R1,1(R7)            R1 = L'NUMBER INPUT                          
         BZ    VCOMERR                                                          
         CLI   1(R7),6             MUST BE 1-6 BYTES LONG.                      
         BH    VCOMERR                                                          
         LA    RF,6                STORE IN ELEMENT AS 6 BYTES LONG             
         SR    RF,R1               WITH LEADING SPACES.                         
         LA    RF,ACOMMENT(RF)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),22(R7)                                                   
         MVC   KEY+2(6),ACOMMENT                                                
         GOTO1 READ                READ FOR COMMENT RECORD.                     
         CLI   DMCB+8,0                                                         
         BNE   VCOMERR                                                          
         BAS   RE,ELADDIN          OK.                                          
         BNE   ERREXIT                                                          
         ZIC   R1,ACOMSEQ          BUMP SEQ NUMBER FOR NEXT ELEMENT.            
         LA    R1,1(R1)                                                         
         STC   R1,ACOMSEQ                                                       
         LA    R7,32(R7)           BUMP TO NEXT SCANNER ENTRY.                  
         BCT   R0,VCOM050                                                       
         B     OKEXIT                                                           
*                                                                               
VCOMERR  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         DROP  R8                                                               
         EJECT                                                                  
VALSJAC  MVC   THISLEDG,PRODLEDG                                                
         BAS   RE,VALACC           READ LEDGER, VALIDATE ACCOUNT, ETC.          
         BNE   ERREXIT             BAD INPUT.                                   
         MVC   SJLVLLNS,LEDGLVLS   SAVE SJ HIERARCHY.                           
         ZIC   RE,TYPELVL          ENSURE I/P KEY LENGHT IS IN RANGE            
         LA    RE,SJLVLLNS-1(RE)   FOR RECORD TYPE                              
         CLC   LOGSJACH+5(1),0(RE)                                              
         MVI   ERROR,ACTOOLNG                                                   
         BH    ERREXIT             I/P IS TOO LONG.                             
         CLI   TYPELVL,1                                                        
         BE    VSJ010              LEVEL 1 ACCT CAN'T BE TOO SHORT.             
         BCTR  RE,0                                                             
         CLC   LOGSJACH+5(1),0(RE)                                              
         MVI   ERROR,ACTOOSHT                                                   
         BNH   ERREXIT             I/P IS TOO SHORT.                            
*&&US                                                                           
         OC    LOGSJAC,SPACES                                                   
         CLC   LOGSJAC+6(L'LOGSJAC-6),SPACES                                    
         BE    VSJ010                                                           
         CLC   LOGSJAC+3(3),SPACES                                              
         BNE   VSJ010                                                           
         MVI   ERROR,NOHIGHER      THEY FORGOT THE PRODUCT LEVEL                
         B     ERREXIT                                                          
*&&                                                                             
VSJ010   MVC   KEY,SPACES                                                       
         MVC   KEY+1(2),PRODLEDG                                                
         SR    RE,RE               GET THE LEVEL WE ARE AT                      
         IC    RE,TYPELVL                                                       
         BCTR  RE,0                BACK UP 1                                    
         IC    RE,SJLVLLNS-1(RE)   GET LENGTH OF PREVIOUS FIELD                 
         LA    RE,LOGSJAC(RE)                                                   
         MVI   ERROR,INVALID                                                    
         CLI   0(RE),C' '                                                       
         BE    ERREXIT                                                          
*                                                                               
         MVI   ERROR,14                                                         
         CLI   TYPELVL,1           IS THIS THE CLIENT LEVEL ?                   
         BNE   VSJ020              NO                                           
         CLC   0(3,RE),=C'ALL'     YES, IS IT "ALL" ?                           
         BE    ERREXIT             YES                                          
*                                                                               
VSJ020   BAS   RE,GETACC           READ FOR SJ ACCOUNT                          
         BE    VSJ040              RECORD FOUND                                 
         CLI   DMCB+8,X'10'                                                     
         BE    VSJ060              RECORD NOT FOUND                             
         DC    H'0'                                                             
*                                                                               
VSJ040   MVI   ERROR,RECONFLE                                                   
         CLI   LOGACT,C'N'                                                      
         BE    ERREXIT             KEY EXISTS ALREADY.                          
         B     VSJ120                                                           
*                                                                               
VSJ060   MVI   ERROR,RECNTFND                                                   
         CLI   LOGACT,C'N'                                                      
         BNE   ERREXIT                                                          
         CLI   TYPELVL,3           FIRST CHARACTER OF JOB CODE MUST             
         BNE   VSJ080              BE A VALID MEDIA CODE.                       
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         ZIC   RE,SJLVLLNS+1       RE = L'KEY AT PRODUCT LEVEL.                 
         LA    RE,LOGSJAC(RE)      R) = A(1ST BYTE OF JOB CODE INPUT).          
         MVC   KEY+2(1),0(RE)                                                   
         GOTO1 READ                READ THE LITTLE MEDIUM.                      
         CLI   DMCB+8,0                                                         
         BE    VSJ080                                                           
         MVI   ERROR,RECNTFND                                                   
         B     ERREXIT             NOT THERE OR END OF WORLD.                   
VSJ080   BAS   RE,RECBILD          BUILD BASIC RECORD.                          
*                                                                               
VSJ120   BAS   RE,NAMER                                                         
         BAS   RE,STATEL                                                        
         BNE   ERREXIT                                                          
         CLC   LOGREC(2),=C'MJ'    TEST FOR JOB RECORD                          
         BNE   VSJ150                                                           
         CLI   LOGACT,C'N'         TEST ADDING JOB                              
         BNE   VSJ130              NO                                           
         BAS   RE,RDOPT            READ THE OPTIONS                             
         CLI   GOPROD,C'N'         TEST NO PRODUCTION JOBS ALLOWED              
         BNE   VSJ150                                                           
         MVI   ERROR,NOTVLREC                                                   
         LA    R2,LOGACTH                                                       
         B     ERREXIT                                                          
*                                                                               
VSJ130   CLI   LOGACT,C'C'         ARE WE CLOSING THE JOB ?                     
         BE    *+12                YES, READ OPTIONS                            
         CLI   MODE,BUILDREC                                                    
         BNE   *+8                                                              
         BAS   RE,RDOPT            READ JOB OPTIONS                             
*                                                                               
VSJ150   B     OKEXIT                                                           
         SPACE 1                                                                
*              DISPLAY OFFICE/UNIT FOR ANALYSIS FOR PRODUCTION RECORD.          
*                                                                               
         USING ACPROFD,R8                                                       
DISSJOF  ZICM  R8,ADACPROF,4                                                    
         BZ    OKEXIT                                                           
         MVC   LOGSJCO,ACPROFFC                                                 
         B     OKEXIT                                                           
         SPACE 1                                                                
*              VALIDATE OFFICE/UNIT FOR ANALYSIS FOR PRODUCTION RECORD.         
*                                                                               
         USING ACPROFD,R8                                                       
VALSJOF  LA    RE,IO2                                                           
         ST    RE,ARECAREA                                                      
         ZICM  R8,ADACPROF,4                                                    
         BNZ   *+12                                                             
         BAS   RE,ADDPROF                                                       
         B     VALSJOF                                                          
*                                                                               
         CLI   TYPELVL,3                                                        
         BE    OKEXIT                                                           
         CLI   LOGSJCOH+5,0                                                     
         BNE   VSJO020                                                          
         MVI   ACPRUNIT,0                                                       
         MVC   ACPROFFC,SPACES                                                  
         CLI   TYPELVL,1                                                        
         BNE   OKEXIT              NOT REQUIRED IF NOT CLIENT.                  
         TM    COMPSTAT,X'20'                                                   
         BZ    OKEXIT              NOT REQUIRED FOR NON-OFFICE COMPANY.         
         MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
VSJO020  TM    COMPSTAT,X'20'                                                   
         BZ    VSJO040             NO VALIDATION FOR NON-OFFICE COMP.           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'     DEPARTMENT U/L.                              
         MVC   KEY+3(1),LOGSJCO                                                 
         GOTO1 READ                READ FOR OFFICE.                             
         CLI   DMCB+8,0                                                         
         BE    VSJO040             FOUND THE OFFICE.                            
         MVI   ERROR,INVALID       OFFICE NOT FOUND.                            
         B     ERREXIT                                                          
*                                                                               
VSJO040  MVC   ACPRUNIT,LOGSJCO    GOOD INPUT.                                  
         MVC   ACPROFFC(L'ACPRUNIT),ACPRUNIT                                    
         OC    ACPROFFC,SPACES                                                  
         B     OKEXIT                                                           
         DROP  R8                                                               
         EJECT                                                                  
VALSRAC  CLI   LOGSRACH+5,0                                                     
         BNE   VSR020                                                           
         USING ACPROFD,R8                                                       
         ZICM  R8,ADACPROF,4                                                    
         BZ    OKEXIT                                                           
         XC    ACPRRECV,ACPRRECV                                                
         XC    LOGSRNM,LOGSRNM                                                  
         MVI   LOGSRF1,0                                                        
         MVI   LOGSRF2,0                                                        
         MVI   LOGSRAN,0                                                        
         MVI   LOGSRSC,0                                                        
         B     OKEXIT                                                           
         DROP  R8                                                               
*                                                                               
VSR020   MVC   THISLEDG,RECVLEDG                                                
         BAS   RE,VALACC                                                        
         BNE   ERREXIT                                                          
*&&UK                                                                           
         LA    RE,VSR040           SR ADDRESS REFLECTS SJ ADDRESS.              
         LA    RF,VALADDR                                                       
SRADDR   NTR1                                                                   
         BR    RF                                                               
*&&                                                                             
*                                                                               
         USING ACPROFD,R8                                                       
VSR040   ZICM  R8,ADACPROF,4       R8 = A(PROFILE ELEMENT, X'24').              
         BNZ   VSR060                                                           
         LA    RF,IO2                                                           
         ST    RF,ARECAREA                                                      
         BAS   RE,ADDPROF                                                       
         B     VSR040                                                           
*                                                                               
VSR060   ZIC   R1,LOGSRACH+5                                                    
         BCTR  R1,0                                                             
         CLI   TYPELVL,1                                                        
         BE    VSR100              CLIENT LEVEL, NO CHECK NECESSARY             
         CLI   TYPELVL,3                                                        
         BE    VSR080              PRODUCT LEVEL SHOULDN'T CLI LEVEL.           
VSR064   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LOGSRAC(0),CLRECV                                                
*&&UK*&& BE    VSRERR                                                           
         B     VSR100                                                           
*                                                                               
VSR080   OC    PRRECV,PRRECV       JOB LEVEL, IF NO RECV AT PROD LEVEL          
         BZ    VSR064              THEN IT SHOULDN'T DUPE CLIENT LEVEL          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LOGSRAC(0),PRRECV                                                
*&&UK*&& BE    VSRERR                                                           
         B     VSR100                                                           
*                                                                               
VSR100   MVC   ACPRRECV,SPACES                                                  
         MVC   ACPRRECV(1),COMPANY                                              
         MVC   ACPRRECV+1(2),THISLEDG                                           
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   ACPRRECV+3(0),LOGSRAC MOVE IN RECV+3BL ACCOUNT CODE.             
         DROP  R8                                                               
*                                                                               
VSRERR   MVI   ERROR,RECVCLPR                                                   
         B     ERREXIT                                                          
*                                                                               
VAL1CAC  CLI   LOG1CACH+5,0                                                     
         BNE   V1C020                                                           
         USING ACPROFD,R8                                                       
         ZICM  R8,ADACPROF,4                                                    
         BZ    OKEXIT                                                           
         XC    ACPRCOST,ACPRCOST                                                
         XC    LOG1CNM,LOG1CNM                                                  
         MVI   LOG1CF1,0                                                        
         MVI   LOG1CF2,0                                                        
         MVI   LOG1CAN,0                                                        
         MVI   LOG1CSC,0                                                        
         B     OKEXIT                                                           
         DROP  R8                                                               
*                                                                               
V1C020   MVC   THISLEDG,=C'1C'                                                  
         BAS   RE,VALACC                                                        
         BNE   ERREXIT                                                          
         MVC   THISLEDG,=C'29'                                                  
         BAS   RE,VALACC                                                        
         BNE   ERREXIT                                                          
*                                                                               
         USING ACPROFD,R8                                                       
         ZICM  R8,ADACPROF,4                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ACPRCOST,SPACES                                                  
         MVC   ACPRCOST(1),COMPANY                                              
         MVC   ACPRCOST+1(2),=C'1C'                                             
         ZIC   R1,LOG1CACH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   ACPRCOST+3(0),LOG1CAC MOVE IN COST+3ING ACCOUNT CODE.            
         DROP  R8                                                               
*                                                                               
VALSAAC  LA    R8,IO2                                                           
         ST    R8,ARECAREA         POINT TO PRODUCT RECORD.                     
         MVI   ELEMENT,X'3D'                                                    
         BAS   RE,ELREMOVE         REMOVE XISTING SALES ANAL. ELEMENT.          
         CLI   LOGSAACH+5,0                                                     
         BNE   VSA020                                                           
         XC    LOGSANM,LOGSANM                                                  
         MVI   LOGSAF1,0                                                        
         MVI   LOGSAF2,0                                                        
         MVI   LOGSAAN,0                                                        
         MVI   LOGSASC,0                                                        
         B     OKEXIT                                                           
*                                                                               
VSA020   MVI   ERROR,DUPINPUT      SALES ANALYSIS PRODUCT SHOUL NOT             
         OC    LOGSAAC,SPACES                                                   
         CLC   LOGSAAC,LOGSJAC     EQUAL MAJOR PRODUCT CODE.                    
         BE    ERREXIT                                                          
         MVC   THISLEDG,PRODLEDG                                                
         BAS   RE,VALACC                                                        
         BNE   ERREXIT             BAD INPUT.                                   
         LA    RE,IO2                                                           
         ST    RE,ARECAREA         POINT TO PRODUCT RECORD.                     
*                                                                               
         USING ACSAND,R8           BUILD NEW ANALYSIS ELEMENT.                  
         LA    R8,ELEMENT                                                       
         MVI   ACSAEL,X'3D'                                                     
         MVI   ACSALN,X'35'                                                     
         MVC   ACSACODE(1),COMPANY                                              
         MVC   ACSACODE+1(2),PRODLEDG                                           
         MVC   ACSACODE+3(12),LOGSAAC    ACCOUNT NAME.                          
         MVC   ACSANAME,LOGSANM    ACCOUNT NAME.                                
         OC    ACSACODE+1(50),SPACES                                            
         BAS   RE,ELADDIN          ADD THE NEW ANALYSIS ELEMENT.                
         BNE   ERREXIT                                                          
         B     OKEXIT                                                           
         SPACE 1                                                                
*              VALIDATE SALES ANALYSIS OFFICE/UNIT FOR ANALYSIS.                
*                                                                               
         USING ACPROFD,R8                                                       
VALSAOF  ZICM  R8,ADSAPROF,4                                                    
         BNZ   VSAO020                                                          
         LA    RE,SALEREC-TIAD                                                  
         A     RE,ATIA                                                          
         ST    RE,ARECAREA                                                      
         BAS   RE,ADDPROF                                                       
         B     VALSAOF                                                          
*                                                                               
VSAO020  CLI   LOGSAANH+5,0                                                     
         BE    OKEXIT                                                           
         TM    COMPSTAT,X'20'                                                   
         BZ    VSAO060             NOT OFFICE COMPANY, NO VALIDATION.           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'     DEPARTMENT UNIT/LEDGER.                      
         MVC   KEY+3(1),LOGSAAN                                                 
         GOTO1 READ                READ FOR OFFICE.                             
         CLI   DMCB+8,0                                                         
         BE    VSAO060             FOUND THE OFFICE.                            
         MVI   ERROR,INVALID       OFFICE NOT FOUND                             
         B     ERREXIT                                                          
*                                                                               
VSAO060  MVC   ACPRUNIT,LOGSAAN                                                 
         MVC   ACPROFFC(L'ACPRUNIT),ACPRUNIT                                    
         OC    ACPROFFC,SPACES                                                  
         B     OKEXIT                                                           
         DROP  R8                                                               
         SPACE 1                                                                
*              VALIDATE THE ACCOUNT CODE, LEVEL ETC.                            
*                                                                               
VALACC   NTR1                                                                   
         BAS   RE,GETLEDG                                                       
         BNE   EXIT                LEDGER RECORD MISSING.                       
         L     RF,FADR                                                          
         LA    R0,1                                                             
         LA    RE,LOGSJACH         DON'T CHECK ACC'T HIERARCHY IF               
         CR    RE,RF               THIS IS THE CLI/PRO/JOB RECORD.              
         BNE   VAC010              IT ISN'T.                                    
         XC    CLRECV(24),CLRECV                                                
         MVC   CLPRUNBL,SPACES                                                  
         MVI   CLPRUNIT,C' '                                                    
         MVI   CLXPST1,0                                                        
         ZIC   R0,TYPELVL          CHECK FOR PRESENCE ON HIGHER LEVEL           
         B     VAC060              ACCOUNTS.                                    
VAC010   LA    RE,LEDGLVLS                                                      
*                                                                               
VAC020   CLC   5(1,RF),0(RE)       FIND HIERARCHY LEVEL FOR ACCT CODE.          
         BNH   VAC040                                                           
         LA    RE,1(RE)                                                         
         AH    R0,=H'1'                                                         
         CH    R0,=H'4'                                                         
         BNH   VAC020                                                           
         MVI   ERROR,ACTOOLNG                                                   
         B     ERREXIT                                                          
*                                                                               
VAC040   CH    R0,=H'4'            MUST BE A LOW LEVEL ACCOUNT.                 
         BE    VAC060                                                           
         CLC   THISLEDG,PRODLEDG                                                
         BNE   VAC050              SALES ANALYSIS ACCOUNT MUST BE               
         CH    R0,=H'2'            AT PRODUCT LEVEL.                            
         BE    VAC060                                                           
         B     *+12                                                             
VAC050   CLI   1(RE),0                                                          
         BE    VAC060                                                           
         MVI   ERROR,WRNGLVAC      IT ISN'T.                                    
         B     ERREXIT                                                          
*                                                                               
VAC060   LA    RE,LEDGLVLS         MAKE SURE HIGHER LEVEL ACCOUNTS XIST         
VAC062   BCT   R0,*+8                                                           
         B     VAC100              NO HIGHER LEVELS.                            
         MVC   KEY+1(L'KEY-1),SPACES                                            
         MVC   KEY+1(2),THISLEDG                                                
         LA    RE,LEDGLVLS         FIND LENGTH OF THIS LEVEL.                   
         AR    RE,R0                                                            
         BCTR  RE,0                                                             
         ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         L     RF,FADR                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(RF)      RF = A(ACCT CODE FIELD HEADER).              
         GOTO1 READ                                                             
         CLI   DMCB+8,0                                                         
         BE    VAC080              FOUND THIS LEVEL OK.                         
         CLI   DMCB+8,X'10'                                                     
         BE    *+6                 THIS LEVEL IS MISSING.                       
         DC    H'0'                BAD I/O ERROR.                               
         MVI   ERROR,NOHIGHER      THIS ONE DOESNT                              
         B     ERREXIT                                                          
*                                                                               
VAC080   LA    RE,LOGSJACH         FOR THE PRODUCTION RECORD,                   
         C     RE,FADR             SAVE THE OFFICE/UNIT FOR ANAL FROM           
         BNE   VAC062              THE LOWEST HIER LVL ACCT IN WHICH            
         LA    R8,IO+ACRECORD-ACKEYD IT IS PRESENT.                             
         USING ACPROFD,R8                                                       
VAC084   CLI   ACPREL,0                                                         
         BE    VAC062              NO PROFILE ELEMENT.                          
         CLI   ACPREL,ACPRELQ                                                   
         BE    VAC086              FOUND IT                                     
         CLI   ACPREL,ACXPELQ                                                   
         BE    VAC090              EXTRA PROFILE ELEMENT                        
VAC085   ZIC   R1,ACPRLEN                                                       
         AR    R8,R1                                                            
         B     VAC084                                                           
*                                                                               
VAC086   CLI   CLPRUNIT,C' '                                                    
         BH    *+10                                                             
         MVC   CLPRUNIT,ACPROFFC                                                
         CLC   CLPRUNBL,SPACES                                                  
         BNE   VAC088                                                           
         MVC   CLPRUNBL,ACPRUNBL                                                
*                                                                               
VAC088   ZIC   RE,TYPELVL          SAVE THE RECEIVABLE AC CODE FOR              
         SR    RE,R0               LEVEL                                        
         BCTR  RE,0                                                             
         MH    RE,=H'12'                                                        
         LA    RF,CLRECV(RE)                                                    
         MVC   0(12,RF),ACPRRECV+3                                              
         B     VAC085                                                           
*                                                                               
         USING ACXPROFD,R8                                                      
VAC090   MVC   CLXPST1,ACXPST1     SAVE CLIENT'S EXTRA PROFILE STATUS           
         B     VAC085                                                           
*                                                                               
VAC100   LA    RE,LOGSJACH         CLI/PRO/JOB RECS DO THIER OWN                
         C     RE,FADR             GETACC, STATEL, ETC.                         
         BE    OKEXIT                                                           
         MVC   KEY+3(L'KEY-3),SPACES                                            
         BAS   RE,GETACC                                                        
         BNE   VAC140                                                           
         CLC   THISLEDG,PRODLEDG                                                
         BE    VAC150                                                           
         L     R8,ARECAREA         CHECK FOR BALANCE ELEMENT.                   
         LA    R8,1+ACRECORD-ACKEYD(R8)                                         
VAC120   CLI   0(R8),0                                                          
         BE    VAC130              NONE THERE.                                  
         CLI   0(R8),X'32'                                                      
         BE    VAC150                                                           
         ZIC   R1,1(R8)                                                         
         AR    R8,R1                                                            
         B     VAC120                                                           
*                                                                               
VAC130   MVI   ERROR,18            ACCOUNT INVALID FOR POSTING.                 
         B     ERREXIT                                                          
*                                                                               
VAC140   BAS   RE,RECBILD                                                       
VAC150   BAS   RE,NAMER                                                         
         BAS   RE,STATEL                                                        
         BNE   ERREXIT                                                          
         B     OKEXIT                                                           
         DROP  R8                                                               
         EJECT                                                                  
*              READ ACCOUNT SAVING A(NECESSARY ELEMENTS)                        
*                                                                               
         USING LDGTABD,RE                                                       
GETACC   NTR1                                                                   
         LA    RE,LDGTAB           RE = A(LEDGER RECORD TABLE).                 
         LA    R0,T603FFD                                                       
         L     R1,FADR                                                          
         SR    R1,R0               R1 = DISP OF THIS FIELD INTO TWA.            
*                                                                               
GAC020   CLI   LDGLEDG,X'FF'                                                    
         BE    OKEXIT                                                           
         CLI   THISLEDG,C'S'       FOR UNIT S ACCOUNTS, MATCH ON                
         BNE   GAC024              SCREEN POSITION.                             
         CLM   R1,3,LDGAHED                                                     
         BE    GAC040              FOUND THIS FIELDS TABLE ENTRY.               
         B     GAC030              NO MATCH.                                    
GAC024   CLC   LDGLEDG,THISLEDG    COSTING ACCOUNTS MATCH ON U/L.               
         BE    GAC040                                                           
GAC030   LA    RE,LDGTBLNQ(RE)                                                  
         B     GAC020                                                           
*                                                                               
GAC040   ZICM  R2,LDGAREC,2                                                     
         BNZ   *+12                                                             
         LA    R2,IO2              CLI/PRO/JOB RECORD INTO IO2.                 
         B     *+8                                                              
         A     R2,ATIA             OTHERS INTO SLOT IN TIA.                     
         ST    R2,ARECAREA         SAVE A(RECORD AREA).                         
         LA    R2,1(R2)            BUMP PAST INDICATOR BYTE.                    
         MVC   0(42,R2),SPACES                                                  
         MVC   KEY(1),COMPANY                                                   
         MVC   0(L'KEY,R2),KEY                                                  
         L     RF,FADR   RF = A(THIS RECORD'S FIELD HEADER).                    
         ZICM  R1,5(RF)                                                         
         BZ    OKEXIT    NO INPUT HERE.                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R2),8(RF)       MOVE IN KEY VALUE LESS CO/UNIT/LEDG.         
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMREAD'),=C'ACCOUNT',(R2),(R2),  *        
               DMWORK                                                           
         TM    DMCB+8,X'ED'                                                     
         BZ    *+6       READ IS OK.                                            
         DC    H'0'      BAD DISK ERROR.                                        
         TM    DMCB+8,X'10'                                                     
         BO    ERREXIT                                                          
         NI    (ACSTATUS-ACKEYD)(R2),X'7F'                                      
*                                                                               
         BCTR  R2,0                R2 = A(THIS RECORDS INDICATOR BYTE).         
         OI    0(R2),X'40'                                                      
*                                                                               
         BAS   RE,SETELAD          SET ELEMENT ADDRESSES.                       
         B     OKEXIT                                                           
         DROP  RE                                                               
         SPACE 1                                                                
*              READ LEDGER RECORD SAVING HIERARCHY                              
*                                                                               
GETLEDG  NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),THISLEDG   SET THE LEDGER.                              
         GOTO1 READ                READ THE LEDGER RECORD.                      
         CLI   DMCB+8,0                                                         
         BE    GLDG020             GOOD READ.                                   
*&&UK*&& MVI   ERROR,110           LEDGER NOT SET UP.                           
*&&US*&& MVI   ERROR,9                                                          
         CLI   DMCB+8,X'10'                                                     
         BE    ERREXIT                                                          
         DC    H'0'                DIE ON ALL ERRORS BUT NOT FOUND.             
*                                                                               
GLDG020  LA    RE,LEDGLVLS         RE = A(LNTHS FOR LEDGERS).                   
         LA    RF,IO               RF = A(LEDGER RECORD).                       
         USING ACKEYD,RF                                                        
         LA    RF,ACRECORD         RF = A(1ST ELEMENT IN RECORD).               
         USING ACHEIRD,RF                                                       
*                                                                               
GLDG040  CLI   ACHREL,0                                                         
         BNE   *+6                                                              
         DC    H'0'                NO HIERARCHY ELEMENT.                        
         CLI   ACHREL,X'16'                                                     
         BE    GLDG060             FOUND THE HIERARCHY ELEMENT.                 
         ZIC   R0,ACHRLEN                                                       
         AR    RF,R0                                                            
         B     GLDG040             TRY THE NEXT ELEMENT.                        
*                                                                               
GLDG060  LA    RF,ACHRLEVA         RF = A(1ST LEVEL INFO).                      
         LA    R0,4                TAKE FOUR LEVELS OF INFO.                    
GLDG062  MVC   0(1,RE),0(RF)       MOVE IN THIS LEVELS LENGTH.                  
         LA    RF,16(RF)           NEXT LEVEL.                                  
         LA    RE,1(RE)                                                         
         BCT   R0,GLDG062                                                       
         B     OKEXIT                                                           
         DROP  RF                                                               
         SPACE 1                                                                
*              BUILD A BLANK PROFILE ELEMENT                                    
*                                                                               
         USING ACPROFD,R8                                                       
ADDPROF  NTR1                                                                   
         LA    R8,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACPREL,ACPRELQ                                                   
         MVI   ACPRLEN,ACPRNARR-ACPROFD                                         
         CLI   TYPELVL,1           AT CLIENT LEVEL GIVE DEFAULT VALUES          
         BNE   *+8                 TO BILLING TYPE                              
         MVI   ACPRBILL,C'P'                                                    
         MVC   ACPROFFC,SPACES                                                  
         MVC   ACPRUNBL,SPACES                                                  
         BAS   RE,ELADDIN                                                       
         BNE   ERREXIT                                                          
         L     R8,ARECAREA                                                      
         LA    R8,1+ACRECORD-ACKEYD(R8) R8 = A(RECORD).                         
ADPR040  CLI   0(R8),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEMENT DID NOT GET ADDED.                   
         CLI   0(R8),X'24'                                                      
         BE    ADPR080                                                          
         ZIC   R1,1(R8)                                                         
         AR    R8,R1                                                            
         B     ADPR040                                                          
*                                                                               
ADPR080  LA    RE,IO2                                                           
         C     RE,ARECAREA                                                      
         BNE   *+12                                                             
         ST    R8,ADACPROF         PROFILE IS FOR CLI/PRO/JOB.                  
         B     OKEXIT                                                           
         ST    R8,ADSAPROF                                                      
         B     OKEXIT                                                           
         DROP  R8                                                               
         EJECT                                                                  
*                                                                               
*              FIX 30 ELEMENT                                                   
*                                                                               
FIX30EL  NTR1                                                                   
         L     R6,ARECAREA                                                      
         LA    R6,1(R6)                                                         
         AH    R6,DATADISP                                                      
*                                                                               
FIX10    CLI   0(R6),0                                                          
         BE    FIXX                NO STATUS ELEMENT PRESENT.                   
         CLI   0(R6),X'30'                                                      
         BE    FIX20               FOUND THE STATUS ELEMENT.                    
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     FIX10                                                            
*                                                                               
         USING ACSTATD,R6                                                       
FIX20    DS    0H                                                               
         CLI   ACSTLEN,ACSTLNQ2    IF RECORD HAS OLD ELEMENT THEN               
         BNL   FIXX                DELETE OLD ELEMENT AND READD ELEM            
         XC    ELEMENT,ELEMENT                                                  
         ZIC   R1,ACSTLEN                                                       
         SH    R1,=H'1'                                                         
         EXMVC R1,ELEMENT,ACSTATD                                               
         BAS   RE,ELREMOVE                                                      
         LA    R6,ELEMENT                                                       
         MVI   ACSTLEN,ACSTLNQ2                                                 
         BAS   RE,ELADDIN                                                       
FIXX     B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*              BUILD OR DISPLAY A STATUS ELEMENT.                               
*                                                                               
         USING ACSTATD,R8                                                       
         USING LOGSJACH,R2                                                      
STATEL   NTR1                                                                   
         CLI   MODE,DSPLYREC                                                    
         BE    *+8                                                              
         BAS   RE,FIX30EL          FIX LENGTH OF X'30' ELEMENT                  
*                                                                               
         L     R8,ARECAREA                                                      
         LA    R8,1+ACRECORD-ACKEYD(R8) R8 = A(1ST ELEMENT FOR RECORD).         
*                                                                               
         L     R2,FADR                                                          
STEL020  CLI   ACSTEL,0                                                         
         BE    STEL060             NO STATUS ELEMENT PRESENT.                   
         CLI   ACSTEL,X'30'                                                     
         BE    STEL040             FOUND THE STATUS ELEMENT.                    
         ZIC   R1,ACSTLEN                                                       
         AR    R8,R1                                                            
         B     STEL020                                                          
*                                                                               
STEL040  CLI   MODE,DSPLYREC                                                    
         BE    *+12                                                             
         CLI   MODE,CLOSEJOB                                                    
         BNE   STEL080             AMEND THE STATUS ELEMENT.                    
         MVC   LOGSJF1,ACSTFILT                                                 
         MVC   LOGSJF2,ACSTFILT+1                                               
         MVC   LOGSJSC,ACSTSUB                                                  
         CLI   ACSTLEN,ACSTLNQ2    FILTER 5 ON NEW ELEMENTS ONLY                
         BL    *+10                                                             
         MVC   LOGSJF5,ACSTFLT5                                                 
         CLC   THISLEDG,PRODLEDG                                                
         BE    *+14                ACSTANAL NOT RELEVANT TO PRODUCTION.         
         MVC   LOGSJCO,ACSTANAL                                                 
         B     OKEXIT                                                           
         CLI   TYPELVL,3           ST=C,L ONLY FOR JOBS                         
         BNE   OKEXIT                                                           
         XC    LOGSTAT,LOGSTAT                                                  
         LA    R1,LOGSTAT                                                       
         TM    ACSTSTAT,X'60'                                                   
         BZ    OKEXIT                                                           
         MVC   0(3,R1),=C'ST='                                                  
         LA    R1,3(R1)                                                         
         TM    ACSTSTAT,X'40'                                                   
         BZ    STEL046                                                          
         MVC   0(2,R1),=C'C,'                                                   
         TM    ACSTSTAT,X'20'                                                   
         BO    STEL045                                                          
         MVI   1(R1),C' '                                                       
         B     OKEXIT                                                           
STEL045  LA    R1,2(R1)                                                         
STEL046  MVI   0(R1),C'L'                                                       
         B     OKEXIT                                                           
*                                                                               
STEL060  CLI   MODE,DSPLYREC                                                    
         BE    OKEXIT              NO STATUS TO BE DISPLAYED                    
         LA    R8,ELEMENT          NO STATUS EL PRESENT, BUILD ONE.             
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACSTANAL,C' '                                                    
         MVI   ACSTSUB,C' '                                                     
         MVC   ACSTFILT,SPACES                                                  
         MVI   ACSTFLT5,C' '                                                    
*                                                                               
STEL080  LA    RE,STATTAB          FOR ACCOUNTS OTHER THAN CLI/PRO/JOB,         
         LA    RF,LOGSJF1H         EXISTING FILTERS REMAIN IF THERE             
STEL082  CLI   0(RE),X'FF'         WAS NO SCREEN INPUT. FOR CLI/PRO/JOB         
         BE    STEL088             THE SCREEN INPUT IS THE TRUTH.               
         ZICM  R1,0(RE),2                                                       
         LA    R1,0(R1,R8)         R1 = A(FILTER IN STATUS ELEMENT).            
         CLI   5(RF),0                                                          
         BE    STEL084             NO INPUT.                                    
         CLI   8(RF),C'.'          PERIOD IS INVALID                            
         BE    STEL150                                                          
STEL0822 MVC   0(1,R1),8(RF)       MOVE SCREEN VALUE TO ELEMENT.                
         B     STEL086                                                          
*                                                                               
STEL084  LA    R0,IO2                                                           
         C     R0,ARECAREA                                                      
         BNE   *+12                NOT THE CLI/PRO/JOB.                         
         MVI   0(R1),C' '                                                       
         B     STEL086                                                          
         MVC   8(1,RF),0(R1)       MOVE FILTER TO SCREEN.                       
*                                                                               
STEL086  ZIC   R1,0(RF)                                                         
         AR    RF,R1               RF = A(NEXT FLDHDR)                          
         LA    RE,2(RE)            RE = A(NEXT FILTER TABLE ENTRY).             
         B     STEL082                                                          
*                                                                               
STEL088  CLC   THISLEDG,PRODLEDG   ACSTANAL IS NOT USED IN PRODUCTION           
         BNE   STEL100             LEDGER.                                      
         MVI   ACSTANAL,C' '                                                    
*                                                                               
STEL100  CLI   ACSTEL,0                                                         
         BNE   OKEXIT              STATUS EL ALREADY EXISTS                     
         MVC   ACSTEL(2),=X'301D'  NEW ELEMENT, COMPLETE CONSTRUCTION.          
         MVI   ACSTCOST,C' '                                                    
         GOTO1 GETFACT,DMCB,0                                                   
         L     R7,DMCB             R7 = A(GETFACT RETURN BLOCK).                
         USING FACTSD,R7                                                        
         MVC   DUB(L'FADATE),FADATE SAVE THE DATE                               
         GOTO1 DATCON,DMCB,(4,DUB),(1,ACSTLAST)                                 
         MVC   ACSTBFDT,ACSTLAST   INIT BAL B/FWD DATE.                         
         DROP  R7                                                               
         BAS   RE,ELADDIN                                                       
         BNE   ERREXIT                                                          
         B     OKEXIT                                                           
*                                                                               
STEL150  ST    RF,FADR                                                          
         MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         DROP  R2,R8                                                            
         SPACE 1                                                                
*              DISPLAY THE ACCOUNT NAME.                                        
*                                                                               
         USING ACNAMED,R8                                                       
         USING LOGSJACH,RE                                                      
NAMER    NTR1                                                                   
         L     R8,ARECAREA         R8 = A(RECORD AREA).                         
         LA    R8,1+ACRECORD-ACKEYD(R8) R8 = A(1ST ELEMENT).                    
         L     RE,FADR             RE = A(ACCOUNT FIELD HEADER).                
NAMD020  CLI   ACNMEL,0                                                         
         BE    NAMD080             ACCOUNT WITH NO NAME.                        
         ZIC   R1,ACNMLEN          R1 = L'NAME ELEMENT.                         
         CLI   ACNMEL,X'20'                                                     
         BE    NAMD040             GOT THE ELEMENT.                             
         AR    R8,R1                                                            
         B     NAMD020                                                          
*                                                                               
NAMD040  CLI   MODE,DSPLYREC                                                    
         BE    NAMD045                                                          
         CLI   MODE,CLOSEJOB                                                    
         BNE   NAMD060                                                          
NAMD045  SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   LOGSJNM(0),ACNMNAME                                              
*                                                                               
NAMD060  CLI   LOGSJNMH+5,0        IF NO NAME INPUT, DISPLAY EXISTING           
         BE    NAMD045             NAME IN RECORD.                              
         XC    ELEMENT,ELEMENT     ELSE, INPUT RULES.                           
         MVI   ELEMENT,X'20'                                                    
         BAS   RE,ELREMOVE                                                      
         B     NAMD084                                                          
*                                                                               
NAMD080  CLI   MODE,DSPLYREC                                                    
         BE    OKEXIT              FINISHED IF DISPLAYING.                      
         CLI   MODE,CLOSEJOB                                                    
         BE    OKEXIT              OR CLOSING                                   
NAMD084  L     RE,FADR                                                          
         ZICM  R1,LOGSJNMH+5       R1 = L'NAME INPUT.                           
         BZ    NAMD100             NO NAME INPUT.                               
         BCTR  R1,0                                                             
         LA    R8,ELEMENT          SET UP TO BUILD NEW ELEMENT.                 
         MVI   ACNMEL,X'20'                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACNMNAME(0),LOGSJNM                                              
         AH    R1,=H'3'            L'ELEMENT = L'NAME + 3.                      
         STC   R1,ACNMLEN                                                       
         BAS   RE,ELADDIN          ADD THE NEW ELEMENT.                         
         BNE   ERREXIT                                                          
         B     OKEXIT                                                           
*                                                                               
         DROP  RE                                                               
         USING LOGSRACH,RE                                                      
NAMD100  LA    RF,LOGSJACH         IF IT'S CLI/PRO/JOB, EXIT NOW                
         CR    RE,RF               TO ALLOW MIPF TO BE POSTED                   
         BE    OKEXIT              BY FIELDING.                                 
         ZIC   R1,LOGSJNMH+5       IF THERE'S BEEN NO NAME INPUT TO             
         STC   R1,LOGSRNMH+5       THIS ACCOUNT, AND IT HAD NO NAME             
         OI    LOGSRNMH+6,X'81'    ELEMENT, USE THE NAME INPUT TO               
         BCTR  R1,0                THE CLIENT/PRODUCT/JOB.                      
         EX    R1,*+8                                                           
         B     NAMD084                                                          
         MVC   LOGSRNM(0),LOGSJNM                                               
         DROP  R8                                                               
         SPACE 1                                                                
*              BUILD A BLANK ACCOUNT RECORD IN THIS RECORD AREA.                
*                                                                               
         USING ACKEYD,RE                                                        
RECBILD  NTR1                                                                   
         L     RE,ARECAREA                                                      
         OI    0(RE),X'80'         MARK RECORD INIC FOR ADD.                    
         LA    RE,1(RE)            POINT RE AT RECORD.                          
         LA    RF,IOLENQ           RF = L'RECORD AREA.                          
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR RECORD AREA.                           
*                                                                               
         USING LOGSJACH,RF                                                      
         L     RE,ARECAREA                                                      
         LA    RE,1(RE)            RE = A(RECORD AREA).                         
         L     RF,FADR             RF = A(THIS ACC'T FIELD HEADER).             
         ZIC   R1,LOGSJACH+5       R1 = L'ACCOUNT CODE INPUT.                   
         BCTR  R1,0                                                             
         MVC   ACKEYACC(42),SPACES INIT KEY.                                    
         MVC   ACLENGTH,=H'50'                                                  
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),THISLEDG                                           
         EX    R1,*+8                                                           
         B     RECB020                                                          
         MVC   ACKEYACC+3(0),LOGSJAC                                            
         DROP  RE,RF                                                            
*                                                                               
RECB020  LA    RE,IO2                                                           
         C     RE,ARECAREA                                                      
         BNE   RECB024                                                          
         CLI   TYPELVL,3                                                        
         BNE   OKEXIT                                                           
         B     RECB030             JOB NEEDS BALANCE & PEEL ELEMENTS.           
RECB024  L     RE,ARECAREA                                                      
         CLC   2(2,RE),PRODLEDG    SALES ANALYSIS ACCOUNT DOES NOT.             
         BE    OKEXIT                                                           
*                                                                               
RECB030  XC    ELEMENT(26),ELEMENT                                              
         MVI   ELEMENT,ACBLELQ                                                  
         MVI   ELEMENT+1,ACBLLNQ                                                
         LA    R8,ELEMENT                                                       
         USING ACBALD,R8                                                        
         ZAP   ACBLFRWD,=P'0'                                                   
         ZAP   ACBLDR,=P'0'                                                     
         ZAP   ACBLCR,=P'0'                                                     
         ZAP   ACBLURG,=P'0'                                                    
         BAS   RE,ELADDIN                                                       
         BNE   ERREXIT                                                          
         XC    ELEMENT(20),ELEMENT                                              
         MVC   ELEMENT(2),=X'3314'                                              
         USING ACPEELD,R8                                                       
         ZAP   ACPEDR,=P'0'                                                     
         ZAP   ACPECR,=P'0'                                                     
         BAS   RE,ELADDIN                                                       
         BNE   ERREXIT                                                          
         B     OKEXIT                                                           
         DROP  R8                                                               
         SPACE 1                                                                
*              ADD AN ELEMENT (SOFTLY, SOFTLY).                                 
*                                                                               
ELADDIN  NTR1                                                                   
         L     R8,ARECAREA         R8 = A(RECORD INDICATOR BYTE).               
         LA    R8,1(R8)            R8 = A(RECORD)                               
         GOTO1 HELLO,DMCB,(C'P',=C'ACCOUNT'),(ELEMENT,(R8)),ELEMENT,0           
         CLI   DMCB+12,0                                                        
         BE    ELADDOK             GOOD ADD, SET ADDRESS AND EXIT               
         MVI   ERROR,66            ERROR,GET READY FOR RECORD TOO LONG          
         CLI   DMCB+12,5                                                        
         BE    ERREXIT                                                          
         DC    H'0'                BAD ELEMENT ADD.                             
ELADDOK  BAS   RE,SETELAD          SET ELEMENT ADDRESSES.                       
         B     OKEXIT                                                           
         SPACE 1                                                                
*              REMOVE AN ELEMENT.                                               
*                                                                               
ELREMOVE NTR1                                                                   
         L     R8,ARECAREA                                                      
         LA    R8,1(R8)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT'),(ELEMENT,(R8)),0,0                 
         CLI   DMCB+12,0                                                        
         BE    ELR020              GOOD REMOVAL.                                
         CLI   DMCB+12,X'06'                                                    
         BE    ELR020              ELEMENT NOT FOUND IS OK.                     
         DC    H'0'                NOWT ELSE IS.                                
ELR020   BAS   RE,SETELAD          SET ELEMENT ADDRESSES.                       
         B     OKEXIT                                                           
         SPACE                                                                  
*              SET ADDRESSES OF REQUIRED ELEMENTS FOR THIS RECORD.              
*                                                                               
         USING LDGTABD,R8                                                       
SETELAD  NTR1                                                                   
         LA    R8,LDGTAB           R8 = A(LEDGER INFO TABLE).                   
         LA    RE,IO2                                                           
         C     RE,ARECAREA         IF RECORD IS CLI/PRO/JOB,                    
         BE    SAD040              USE 1ST LINE OF LDGTAB.                      
         LA    R8,LDGTBLNQ(R8)     START TABLE SEARCH AT LINE 2.                
*                                                                               
SAD020   CLI   LDGLEDG,X'FF'                                                    
         BE    OKEXIT              NOT AN INTERESTING RECORD.                   
         CLC   LDGLEDG,2(RE)       MATCH TABLE VS. RECORD U/L.                  
         BE    SAD040              GOT IT.                                      
         LA    R8,LDGTBLNQ(R8)     NEXT TABLE LINE.                             
         B     SAD020                                                           
*                                                                               
         USING ELTABD,RF                                                        
SAD040   ZICM  RF,LDGAELT,3        RF = A(ELEMENT TABLE FOR THIS REC).          
         A     RF,WRELO                                                         
         LA    RE,1+ACRECORD-ACKEYD(RE)                                         
         LR    R8,RF               SAVE A(ELEMENT TABLE IN R8)                  
*                                                                               
SAD060   CLI   ELTCODE,X'FF'       CLEAR ANY EXISTING ADDRESSES.                
         BE    SAD080              NO MORE ADDRESSES.                           
         ZICM  R1,ELTELADR,2                                                    
         LA    R1,LOGWORKD(R1)     R1 = A(A(THIS ELEMENT)).                     
         XC    0(4,R1),0(R1)       CLEAR A(THIS ELEMENT).                       
         LA    RF,ELTBLNQ(RF)                                                   
         B     SAD060              NEXT ELEMENTS ADDRESS.                       
*                                                                               
SAD080   LR    RF,R8               RF BACK TO START OF ELTABLE.                 
SAD100   CLI   0(RE),0                                                          
         BE    OKEXIT              END OF RECORD.                               
         LR    RF,R8                                                            
SAD110   CLI   ELTCODE,X'FF'                                                    
         BE    SAD140              END OF ELEMENT TABLE.                        
         CLC   ELTCODE,0(RE)                                                    
         BNE   SAD120              NO MATCH.                                    
         ZICM  R1,ELTELADR,2                                                    
         LA    R1,LOGWORKD(R1)     R1 = A(A(ELEMENT))                           
         OC    0(4,R1),0(R1)                                                    
         BNZ   SAD140              ONLY SAVE A(1ST ELEMENT).                    
         ST    RE,0(R1)            SAVE A(ELEMENT).                             
         B     SAD140                                                           
SAD120   LA    RF,ELTBLNQ(RF)      NEXT TABLE ENTRY.                            
         B     SAD110                                                           
SAD140   ZIC   R1,1(RE)            NEXT ELEMENT.                                
         AR    RE,R1                                                            
         B     SAD100                                                           
         DROP  R8,RF                                                            
         SPACE                                                                  
*              MAKE TRAILING SPACES IN SCREEN FIELD BINARY ZEROES.              
*                                                                               
ZEROSPC  NTR1                                                                   
         L     RE,FADR             RE = A (SCREEN HEADER)                       
         ZIC   RF,0(RE)                                                         
         SH    RF,=H'8'            RF = L'SCREEN FIELD - L'HEADER.              
         LA    RE,8(RE)            RE = A(FIELD DATA).                          
*                                                                               
ZSP020   LR    R1,RF               R1 = REMAINING L'DATA.                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),SPACES                                                   
         BE    ZSP040              FOUND TRAILING SPACES.                       
         LA    RE,1(RE)            NOT YET. RE = A(NEXT INPUT CHAR.             
         BCT   RF,ZSP020           DECREMENT REMAINING LEN, TRY AGAIN.          
         B     OKEXIT              NO TRAILING SPACES.                          
*                                                                               
ZSP040   EX    R1,*+8              XC THE TRAILING SPACES.                      
         B     OKEXIT                                                           
         XC    0(0,RE),0(RE)                                                    
         EJECT                                                                  
         USING LDGTABD,R8                                                       
VALEND   LA    R8,LDGTAB                                                        
VEND020  CLI   LDGLEDG,X'FF'                                                    
         BE    VEND100                                                          
         ZICM  R7,LDGAREC,2                                                     
         BNZ   *+12                                                             
         LA    R7,IO2              R7 = A(CLI/PRO/JOB RECORD).                  
         B     *+8                                                              
         A     R7,ATIA             R7 = A(OTHER RECORD).                        
         ST    R7,ARECAREA                                                      
         CLI   0(R7),0                                                          
         BE    VEND080             NO RECORD PRESENT.                           
         MVC   COMMAND,=CL8'DMWRT'                                              
         TM    0(R7),X'80'                                                      
         BZ    VEND060             NOT A NEW RECORD, SIMPLE WRITE.              
         MVC   COMMAND,=CL8'DMADD' NEW RECORD NEEDS ADDING.                     
VEND060  LA    R7,1(R7)            BUMP PAST RECORD INDICATOR.                  
         LR    RF,R7                                                            
         BAS   RE,VALLEN                                                        
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R7),(R7),DMWORK                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DEATH NEEDED FOR RECOVERY.                   
VEND080  LA    R8,LDGTBLNQ(R8)                                                  
         B     VEND020             NEXT RECORD.                                 
*                                                                               
VEND100  CLI   TYPELVL,3           FOR JOBS ONLY, ADD A REQUEST FOR JOB         
         BNE   OKEXIT              LABELS IF COMPANY REQUIRES IT AND A          
         TM    COMPSTAT,X'02'      ANY LABEL DATA HAS CHANGED.                  
         BO    OKEXIT              COMPANY DOESN'T REQUIRE LABELS.              
         CLI   LOGACT,C'N'         IS THIS A NEW RECORD ?                       
         BE    VEND120             YES, ALWAYS DO LABELS                        
         TM    LOGCLOSH+4,X'80'                                                 
         BO    VEND120             DATA HAS CHANGED.                            
         TM    LOGSJNMH+4,X'80'                                                 
         BO    VEND120                                                          
         TM    LOGSJCOH+4,X'80'                                                 
         BO    VEND120                                                          
         TM    LOGTYPEH+4,X'80'                                                 
         BO    VEND120                                                          
         TM    LOGNONBH+4,X'80'                                                 
         BZ    OKEXIT              NO LABEL DATA HAS CHANGED.                   
*                                                                               
VEND120  XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+10,12                                                    
         MVC   ELEMENT+26(80),SPACES                                            
         MVC   ELEMENT+26(2),=C'12'                                             
         MVC   ELEMENT+28(1),COMPANY                                            
         MVC   ELEMENT+29(1),CLPRUNIT                                           
         ZICM  R8,ADACPROF,4                                                    
         USING ACPROFD,R8                                                       
         CLI   ACPROFFC,C' '                                                    
         BNH   *+10                                                             
         MVC   ELEMENT+29(1),ACPROFFC                                           
         DROP  R8                                                               
         LA    RF,IO2                                                           
         MVC   ELEMENT+35(15),1(RF)                                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'ACCREQS',ELEMENT,ELEMENT,0             
         CLI   DMCB+8,0                                                         
         BE    OKEXIT              REQUEST ADDED SUCCESSFULLY.                  
         DC    H'0'                BAD ADD                                      
         EJECT                                                                  
*              CHANGE STATUS OF JOB TO CLOSE                                    
         SPACE 2                                                                
CLOSEIT  DS    0H                                                               
         LA    R2,LOGSJACH                                                      
         ST    R2,FADR                                                          
         GOTO1 ANY                 JOB MUST BE THERE                            
         LA    RF,VALSJAC          VALIDATE IT                                  
         LA    RE,CLOSE02          SET RETURN ADDRESS                           
         NTR1                                                                   
         BR    RF                                                               
CLOSE02  BNE   CLOSERR                                                          
         LA    R4,IO2                                                           
         LA    R4,1(R4)            BUMP PAST INDICATOR BYTE                     
         AH    R4,DATADISP                                                      
         SR    R8,R8                                                            
         SPACE 1                                                                
CLOSE04  CLI   0(R4),X'30'         STATUS ELEMENT                               
         BE    CLOSE08                                                          
         CLI   0(R4),X'32'         BALANCE ELEMENT                              
         BE    CLOSE10                                                          
         CLI   0(R4),ASTELQ        ACCOUNT STATUS ELEMENT                       
         BE    CLOSE07                                                          
         CLI   0(R4),X'26'         JOB ELEMENT                                  
         BE    CLOSE12                                                          
         CLI   0(R4),0                                                          
         BE    CLOSE14                                                          
         SPACE 1                                                                
CLOSE06  IC    R8,1(R4)                                                         
         AR    R4,R8                                                            
         B     CLOSE04                                                          
         SPACE 1                                                                
CLOSE07  DS    0H                                                               
         USING ASTELD,R4                                                        
         OC    ASTDRAFT,ASTDRAFT                                                
         BZ    CLOSE06                                                          
         MVI   ERROR,CLODRFT       CANNOT CLOSE - DRAFT ITEMS ON FILE           
         B     CLOSERR                                                          
         SPACE 1                                                                
CLOSE08  DS    0H                                                               
         USING ACSTATD,R4                                                       
         OI    ACSTSTAT,X'40'      CLOSED                                       
         B     CLOSE06                                                          
         SPACE 1                                                                
CLOSE10  DS    0H                                                               
         USING ACBALD,R4                                                        
         CP    ACBLDR,ACBLCR       DEBITS MUST EQUAL CREDITS                    
         BE    CLOSE06                                                          
         MVI   ERROR,NOTVLREC                                                   
         B     CLOSERR                                                          
         SPACE 1                                                                
         USING ACJOBD,R4                                                        
CLOSE12  DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,ACJBCLOS)                                   
         B     CLOSE06                                                          
         SPACE 1                                                                
CLOSE14  DS    0H                  CONTINUE CHECKING                            
         LA    R4,IO2                                                           
         MVC   KEY,1(R4)                                                        
         LA    R4,IO                                                            
         USING ACKEYACC,R4                                                      
         GOTO1 HIGH                                                             
CLOSE16  GOTO1 SEQ                                                              
         CLC   ACKEYACC,KEYSAVE    STILL SAME KEY ?                             
         BNE   CLOSE26             NO, ALL DONE                                 
         CLI   ACRECORD,X'44'                                                   
         BNE   CLOSE16                                                          
         CLC   ACKEYWRK,=C'99'                                                  
         BE    CLOSE16                                                          
         CLC   ACKEYWRK,=C'**'                                                  
         BNE   CLOSE18                                                          
         MVI   ERROR,NOTVLREC                                                   
         B     CLOSERR                                                          
         SPACE 1                                                                
         USING TRANSD,R6                                                        
CLOSE18  LA    R6,ACRECORD                                                      
         MVI   ERROR,HOLDIT                                                     
         TM    TRNSSTAT,X'04'      IS RECORD ON HOLD ?                          
         BO    ERREXIT             YES, CAN'T CLOSE                             
*                                                                               
         CLI   GOBILTYP,C'C'       IS THIS CLIENT BILLING ?                     
         BE    CLOSE20             YES, USE DIFFERENT LOGIC                     
         OC    ACDTUSED,ACDTUSED     NO, IS IT BILLED ?                         
         BNZ   CLOSE16               YES                                        
*                                                                               
         TM    TRNSSTAT,X'20'        NO, IS IT A REVERSAL ?                     
         BO    CLOSE16               YES, SKIP IT                               
*                                                                               
         MVI   ERROR,OPENSKS                                                    
         CLC   ACKEYCON+1(2),=C'SK'  NO, IS THE CONTRA SK                       
         BE    ERREXIT               YES, CAN'T CLOSE                           
         MVI   ELCODE,X'4C'          NO, LOOK AT SUBSIDIARY POSTING             
         BAS   RE,GETELIO                                                       
         BNE   CLOSE16                                                          
         USING TRSDESCD,R6                                                      
         CLC   TRSDACCS(2),=C'SK'                                               
         BE    ERREXIT                                                          
         B     CLOSE16                                                          
*                                                                               
         USING TRBDETD,R6                                                       
CLOSE20  MVI   ERROR,UNBILLED                                                   
         LR    R8,R6                                                            
         ZAP   AMOUNT,=P'0'                                                     
         MVI   ELCODE,TRBDELQ                                                   
*                                                                               
CLOSE22  BAS   RE,NEXTEL                                                        
         BNE   CLOSE24                                                          
         CLC   TRBDNO,SPACES       IS THIS BILLED ?                             
         BNH   CLOSE22             NO, GET NEXT                                 
         ICM   RF,15,TRBDAMNT      YES, GET AMOUNT                              
         CVD   RF,DUB                                                           
         AP    AMOUNT,DUB                                                       
         B     CLOSE22                                                          
*                                                                               
CLOSE24  CP    AMOUNT,TRNSAMNT-TRANSD(L'TRNSAMNT,R8)                            
         BNE   ERREXIT                                                          
         B     CLOSE16                                                          
*                                                                               
CLOSE26  BAS   RE,STATEL           DISPLAY NEW STATUS ELEMENT                   
         BNE   ERREXIT                                                          
         OI    LOGSTATH+6,X'80'                                                 
         LA    RF,DISCLOSD                                                      
         LA    RE,CLOSE28                                                       
         NTR1                                                                   
         BR    RF                  AND CLOSE DATE                               
CLOSE28  LA    R4,IO2                                                           
         LA    R4,1(R4)            BUMP PAST INDICATOR BYTE                     
         LR    RF,R4                                                            
         BAS   RE,VALLEN                                                        
         GOTO1 DATAMGR,DMCB,=CL8'DMWRT',=C'ACCOUNT',(R4),(R4),DMWORK            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                BAD WRITE BY DATAMGR                         
         TM    COMPSTAT,X'02'      IF COMPANY REQUIRES LABELS                   
         BE    VEND120             ADD A REQUEST                                
         B     OKEXIT                                                           
         SPACE 2                                                                
CLOSERR  DS    0H                  PUT OUT ERROR MESSAGE AND GET OUT            
         LA    R2,LOGSJACH                                                      
         OI    6(R2),X'40'         CURSOR TO JOB FIELD                          
         OI    6(R2),X'80'         TRANSMIT IT                                  
         CLI   ERROR,CLODRFT                                                    
         BE    CLOSER1                                                          
         LA    R4,NOTVLMSG         ERROR IS EITHER NOT VALID FOR CLOSE          
         CLI   ERROR,NOTVLREC                                                   
         BE    *+8                                                              
         LA    R4,NOTFDMSG         OR IS NOT AN ACCOUNT                         
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(37),0(R4)                                                
         OI    LOGHEADH+6,X'80'    TRANSMIT ERROR                               
         L     RD,BASERD           SET TO GET STRAIGHT OUT                      
CLOSER1  XIT1                                                                   
         SPACE 2                                                                
GETELIO  LA    R6,IO                                                            
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
VALLEN   CLC   42(2,RF),=X'03EC'   MAKE SURE RECORD IS NOT TOO BIG              
         BNHR  RE                                                               
         MVI   ERROR,66                                                         
         B     ERREXIT                                                          
         EJECT                                                                  
* SUB-ROUTINE TO READ JOB OPTIONS THROUGH GETOPT                                
*                                                                               
RDOPT    NTR1                                                                   
         GOTO1 CALLOV,DMCB,0,X'D9000A84'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GETOPT,0(R1)                                                     
         SPACE 1                                                                
RDOPT2   LA    RE,GOBLOCK          CLEAR GOBLOCK                                
         LA    RF,L'GOBLOCK                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         SPACE 1                                                                
RDOPT4   MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL(1),COMPANY                                              
         MVC   GOSELCUL+1(2),PRODLEDG  SET UNIT/LEDGER                          
*                                                                               
         ZIC   R1,SJLVLLNS         GET CLIENT LEVEL                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELCLI(0),LOGSJAC                                              
         OC    GOSELCLI,SPACES     SPACE PAD CLIENT                             
*                                                                               
         LA    RE,LOGSJAC+1(R1)                                                 
         LA    RF,1(R1)            RF=L'CLIENT                                  
         ZIC   R1,SJLVLLNS+1       L'CLIENT+L'PRODUCT                           
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELPRO(0),0(RE)   EXTRACT PRODUCT CODE                         
         OC    GOSELPRO,SPACES     SPACE PAD PRODUCT                            
*                                                                               
         LA    RE,1(R1,RE)         RE=A(JOB CODE)                               
         LA    RF,1(R1)            RF=L'PRODUCT                                 
         ZIC   R1,SJLVLLNS+2       GET L'PRODUCT+L'JOB                          
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELJOB(0),0(RE)                                                
         OC    GOSELJOB,SPACES     SPACE PAD PRODUCT                            
*                                                                               
RDOPT6   GOTO1 GETOPT,DMCB,GOBLOCK                                              
         B     OKEXIT                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              TABLE OF VALUES AND MASKS FOR RECORD TYPES.                      
*                                                                               
TYPTBL   DC    CL8'MCLIENT ',X'01E580088040',AL3(0,VALEND)                      
         DC    AL2(LOGCTABH-T603FFD)                                            
         DC    CL8'MPRODUCT',X'02E540042010',AL3(0,VALEND)                      
         DC    AL2(LOGCTABH-T603FFD)                                            
         DC    CL8'MJOB    ',X'03E420020804',AL3(0,VALEND)                      
         DC    AL2(LOGJTABH-T603FFD)                                            
*                                                                               
*              TABLE OF SCREEN FIELD CHARACTERISTICS AND ROUTINES.              
*                                                                               
FLDTBL   DC    AL1(1,ALLMAND,0),AL2(LOGSJACH-T603FFD)                           
         DC    AL3(VALSJAC,VALSJAC)                                             
         DC    AL1(2,ALLMAND,0),AL2(LOGSJNMH-T603FFD),AL3(0,0)                  
         DC    AL1(3,0,0),AL2(LOGSJF1H-T603FFD),AL3(0,0)                        
         DC    AL1(4,0,0),AL2(LOGSJF2H-T603FFD),AL3(0,0)                        
         DC    AL1(5,0,JOBXC),AL2(LOGSJCOH-T603FFD)                             
         DC    AL3(DISSJOF,VALSJOF)                                             
         DC    AL1(6,0,0),AL2(LOGSJSCH-T603FFD),AL3(0,0)                        
         DC    AL1(7,CLIMAND,0),AL2(LOGSRACH-T603FFD)                           
         DC    AL3(DISSRAC,VALSRAC)                                             
         DC    AL1(8,0,0),AL2(LOGSRNMH-T603FFD)                                 
         DC    AL3(0,0)                                                         
         DC    AL1(9,0,0),AL2(LOGSRF1H-T603FFD),AL3(0,0)                        
         DC    AL1(10,0,0),AL2(LOGSRF2H-T603FFD),AL3(0,0)                       
         DC    AL1(11,0,0),AL2(LOGSRANH-T603FFD),AL3(0,0)                       
         DC    AL1(12,0,0),AL2(LOGSRSCH-T603FFD),AL3(0,0)                       
         DC    AL1(13,0,0),AL2(LOGSRF5H-T603FFD),AL3(0,0)                       
         DC    AL1(14,CLIMAND,0),AL2(LOG1CACH-T603FFD)                          
         DC    AL3(DIS1CAC,VAL1CAC)                                             
         DC    AL1(15,0,0),AL2(LOG1CNMH-T603FFD),AL3(0,0)                       
         DC    AL1(16,0,0),AL2(LOG1CF1H-T603FFD),AL3(0,0)                       
         DC    AL1(17,0,0),AL2(LOG1CF2H-T603FFD),AL3(0,0)                       
         DC    AL1(18,0,0),AL2(LOG1CANH-T603FFD),AL3(0,0)                       
         DC    AL1(19,0,0),AL2(LOG1CSCH-T603FFD),AL3(0,0)                       
         DC    AL1(20,0,0),AL2(LOG1CF5H-T603FFD),AL3(0,0)                       
*&&UK                                                                           
         DC    AL1(21,LABELQ,ALLXC),AL2(LOGSALH-T603FFD),AL3(SALLBL,0)          
         DC    AL1(22,0,ALLPROT),AL2(LOGSAACH-T603FFD),AL3(0,0)                 
         DC    AL1(23,0,ALLPROT),AL2(LOGSANMH-T603FFD),AL3(0,0)                 
         DC    AL1(24,0,ALLPROT),AL2(LOGSAF1H-T603FFD),AL3(0,0)                 
         DC    AL1(25,0,ALLPROT),AL2(LOGSAF2H-T603FFD),AL3(0,0)                 
         DC    AL1(26,0,ALLPROT),AL2(LOGSAANH-T603FFD),AL3(0,0)                 
         DC    AL1(27,0,ALLPROT),AL2(LOGSASCH-T603FFD),AL3(0,0)                 
         DC    AL1(28,0,ALLPROT),AL2(LOGSAF5H-T603FFD),AL3(0,0)                 
*&&                                                                             
*&&US                                                                           
         DC    AL1(21,LABELQ,CLIXC+JOBXC),AL2(LOGSALH-T603FFD)                  
         DC    AL3(SALLBL,0)                                                    
         DC    AL1(22,0,CLIPROT+JOBPROT),AL2(LOGSAACH-T603FFD)                  
         DC    AL3(DISSAAC,VALSAAC)                                             
         DC    AL1(23,0,CLIPROT+JOBPROT),AL2(LOGSANMH-T603FFD),AL3(0,0)         
         DC    AL1(24,0,CLIPROT+JOBPROT),AL2(LOGSAF1H-T603FFD),AL3(0,0)         
         DC    AL1(25,0,CLIPROT+JOBPROT),AL2(LOGSAF2H-T603FFD),AL3(0,0)         
         DC    AL1(26,0,CLIPROT+JOBPROT),AL2(LOGSAANH-T603FFD)                  
         DC    AL3(DISSAOF,VALSAOF)                                             
         DC    AL1(27,0,CLIPROT+JOBPROT),AL2(LOGSASCH-T603FFD),AL3(0,0)         
         DC    AL1(28,0,CLIPROT+JOBPROT),AL2(LOGSAF5H-T603FFD),AL3(0,0)         
*&&                                                                             
         DC    AL1(29,0,0),AL2(LOGTYPEH-T603FFD),AL3(DISBTYP,VALBTYP)           
         DC    AL1(30,0,0),AL2(LOGNONBH-T603FFD),AL3(DISNONB,VALNONB)           
         DC    AL1(31,0,0),AL2(LOGPRFH-T603FFD),AL3(DISXPROF,VALXPROF)          
         DC    AL1(32,0,0),AL2(LOGPRNTH-T603FFD),AL3(DISPRNT,VALPRNT)           
         DC    AL1(33,0,0),AL2(LOGINF1H-T603FFD),AL3(DISINF,VALINF)             
         DC    AL1(34,JOBMISS+LABELQ,PRODXC),AL2(LOGGRPLH-T603FFD)              
         DC    AL3(GRPLBL,0)                                                    
         DC    AL1(35,JOBMISS,PRODXC),AL2(LOGGRUPH-T603FFD)                     
         DC    AL3(DISBGRP,VALGRUP)                                             
         DC    AL1(36,JOBMISS,0),AL2(LOGADD1H-T603FFD)                          
         DC    AL3(DISADDR,VALADDR)                                             
         DC    AL1(37,JOBMISS+LABELQ,PRODXC),AL2(LOGNUMLH-T603FFD)              
         DC    AL3(NUMLBL,0)                                                    
         DC    AL1(38,JOBMISS,PRODXC),AL2(LOGNUMBH-T603FFD)                     
         DC    AL3(DISBNUM,VALBNUM)                                             
         DC    AL1(39,CLIMISS+PROMISS,0),AL2(LOGCLOSH-T603FFD)                  
         DC    AL3(DISCLOSD,VALCLOS)                                            
         DC    AL1(41,CLIMISS+PROMISS,JOBXC)                                    
         DC    AL2(LOGSTATH-T603FFD),AL3(0,0)                                   
         DC    AL1(40,CLIMISS+PROMISS,0),AL2(LOGCOM1H-T603FFD)                  
         DC    AL3(DISCOM,VALCOM)                                               
         DC    AL1(35,CLIMISS+PROMISS,0),AL2(LOGCOM2H-T603FFD)                  
         DC    AL3(0,VALCOM2)                                                   
         DC    AL1(36,CLIMISS+PROMISS,0),AL2(LOGCOM3H-T603FFD)                  
         DC    AL3(0,VALCOM2)                                                   
         DC    AL1(255)                                                         
*                                                                               
*              TABLE OF RECORD AREAS, ELEMENT TABLES FOR EACH LEDGER.           
*                                                                               
LDGTAB   DC    C'SJ',AL2(0,LOGSJACH-T603FFD),AL3(SJELTAB)                       
         DC    C'SR',AL2(RECVREC-TIAD,LOGSRACH-T603FFD),AL3(SRELTAB)            
         DC    C'1C',AL2(CST1REC-TIAD,LOG1CACH-T603FFD),AL3(C1ELTAB)            
         DC    C'29',AL2(CST2REC-TIAD,LOG1CACH-T603FFD),AL3(C2ELTAB)            
         DC    C'SJ',AL2(SALEREC-TIAD,LOGSAACH-T603FFD),AL3(SAELTAB)            
         DC    X'FF'                                                            
*                                                                               
*              TABLE OF ELEMENTS TO SAVE FOR SR RECORD.                         
*                                                                               
SRELTAB  DC    X'24',AL2(ADSRPROF-LOGWORKD)                                     
         DC    X'FF'                                                            
*                                                                               
*              TABLE OF ELEMENTS TO SAVE FOR COSTING (1C & 29) RECORDS.         
*                                                                               
C1ELTAB  DC    X'24',AL2(ADCSPROF-LOGWORKD)                                     
         DC    X'FF'                                                            
C2ELTAB  DC    X'30',AL2(ADC2STAT-LOGWORKD)                                     
         DC    X'FF'                                                            
*                                                                               
*              TABLE OF ELEMENTS TO SAVE FOR SJ SALES ANALYSES RECORD.          
*                                                                               
SAELTAB  DC    X'24',AL2(ADSAPROF-LOGWORKD)                                     
         DC    X'FF'                                                            
*                                                                               
*              TABLE OF ELEMENTS TO BE SAVED FOR PRODUCTION RECORD.             
*                                                                               
SJELTAB  DS    0H                                                               
         DC    X'21',AL2(ADACNUM-LOGWORKD)                                      
         DC    X'22',AL2(ADACADD-LOGWORKD)                                      
         DC    X'24',AL2(ADACPROF-LOGWORKD)                                     
         DC    X'26',AL2(ADACJOB-LOGWORKD)                                      
         DC    X'30',AL2(ADACSTAT-LOGWORKD)                                     
         DC    X'3C',AL2(ADACXPRF-LOGWORKD)                                     
         DC    X'3D',AL2(ADACSAN-LOGWORKD)                                      
         DC    X'3E',AL2(ADACCOMM-LOGWORKD)                                     
         DC    AL1(255)                                                         
*                                                                               
XPRFTAB  DC    CL9'DUE',AL2(ACXPDUE-ACXPROFD),AL1(L'ACXPDUE-1)                  
         DC    AL1(PACKED+FLDVALQ),PL2'10',CL2' ',CL6' ',AL3(DUEVAL)            
         DC    AL3(DXPRODP)                                                     
*                                                                               
         DC    CL9'OVER',AL2(ACXPOVER-ACXPROFD),AL1(L'ACXPOVER-1)               
         DC    AL1(PACKED+FLDVALQ),PL3'10000',CL1' ',CL6' ',AL3(CSHVAL)         
         DC    AL3(DXPR2DP)                                                     
*                                                                               
         DC    CL9'LOW ',AL2(ACXPLOW-ACXPROFD),AL1(L'ACXPLOW-1)                 
         DC    AL1(PACKED+FLDVALQ),PL4'5000',CL6' ',AL3(CSHVAL)                 
         DC    AL3(DXPR2DP)                                                     
*                                                                               
         DC    CL9'SUMMARY',AL2(ACXPSUM-ACXPROFD),AL1(L'ACXPSUM-1)              
         DC    AL1(0),CL4'Y',CL6'NO',CL3'N'                                     
         DC    AL3(0)                                                           
*                                                                               
         DC    CL9'PAY',AL2(ACXPNET-ACXPROFD),AL1(L'ACXPNET-1)                  
         DC    AL1(FLDVALQ),CL4'N',CL6' ',AL3(VPAY)                             
         DC    AL3(DXPPAY)                                                      
*                                                                               
         DC    CL9'DETAIL',AL2(ACXPDET-ACXPROFD),AL1(L'ACXPDET-1)               
         DC    AL1(0),CL4'Y',CL6'NO',CL3'N'                                     
         DC    AL3(0)                                                           
*                                                                               
         DC    CL9'ESTDETAIL',AL2(ACXPEDET-ACXPROFD),AL1(L'ACXPEDET-1)          
         DC    AL1(0),CL4'N',CL6'YES',CL3'Y'                                    
         DC    AL3(0)                                                           
*                                                                               
         DC    CL9'DISC',AL2(ACXPCD-ACXPROFD),AL1(L'ACXPCD-1)                   
         DC    AL1(EQUAL),CL4' ',CL6'NO',CL3'N'                                 
         DC    AL3(0)                                                           
*                                                                               
         DC    AL1(254)            END OF DATA FOR SHORT ELEMENT.               
*                                                                               
         DC    CL9'ESTIMATE',AL2(ACXPEST-ACXPROFD),AL1(L'ACXPEST-1)             
         DC    AL1(EQUAL),CL4' ',CL6'NO',CL3'N'                                 
         DC    AL3(0)                                                           
*                                                                               
         DC    CL9'ETA',AL2(ACXPST1-ACXPROFD),AL1(0)                            
         DC    AL1(BITS+EQUAL),X'00800000',CL6'NO',AL3(0)                       
         DC    AL3(0)                                                           
*                                                                               
         DC    CL9'ECOMM',AL2(ACXPST1-ACXPROFD),AL1(0)                          
         DC    AL1(BITS+EQUAL),X'00400000',CL6'NO',AL3(0)                       
         DC    AL3(0)                                                           
*                                                                               
         DC    CL9'TRANSFER',AL2(ACXPST1-ACXPROFD),AL1(0)                       
         DC    AL1(BITS+EQUAL),X'00200000',CL6'NO',AL3(0)                       
         DC    AL3(0)                                                           
*                                                                               
         DC    CL9'PRODPAK',AL2(ACXPST1-ACXPROFD),AL1(0)                        
         DC    AL1(BITS+EQUAL),X'00100000',CL6'NO',AL3(0)                       
         DC    AL3(0)                                                           
*                                                                               
         DC    CL9'POST',AL2(ACXPST1-ACXPROFD),AL1(0)                           
         DC    AL1(BITS),X'00080000',CL6'PLUSCD',AL3(0)                         
         DC    AL3(0)                                                           
*                                                                               
         DC    CL9'EST',AL2(ACXPST1-ACXPROFD),AL1(0)                            
         DC    AL1(NODSP+BITS+FLDVALQ),X'00040000',CL6'UNAPP'                   
         DC    AL3(VESTUNAP),AL3(0)                                             
*                                                                               
         DC    CL9'JOBS',AL2(ACXPST1-ACXPROFD),AL1(0)                           
         DC    AL1(BITS+FLDVALQ),X'00020000',CL6'UNAPP',AL3(VJOBUNAP)           
         DC    AL3(0)                                                           
*                                                                               
         DC    CL9'FILT2',AL2(ACXPREP-ACXPROFD),AL1(0)                          
         DC    AL1(DSPRECV+FLDVALQ),AL4(0),CL6' ',AL3(VFILT2)                   
         DC    AL3(0)                                                           
*                                                                               
         DC    CL9'%EST',AL2(ACXPBILL-ACXPROFD),AL1(0)                          
         DC    AL1(DSPRECV+FLDVALQ),AL4(0),CL6' ',AL3(VESTBILL)                 
         DC    AL3(0)                                                           
*                                                                               
         DC    AL1(255)                                                         
*                                                                               
BILTTBL  DC    AL1(0),AL1(0),AL3(OKEXIT),XL9'00'                                
         DC    C'P',AL1(MOVER),CL12'PROGRESSIVE'                                
         DC    C'U',AL1(MOVER),CL12'UNBILLABLE'                                 
         DC    C'1',AL1(MOVER),CL12'1 LINE'                                     
         DC    C'T',AL1(MOVER),CL12'TOTAL'                                      
         DC    C'C',AL1(MOVER),CL12'CLIENT'                                     
         DC    C'S',AL1(0),AL3(DSTBSPC),XL9'00'                                 
         DC    C'E',AL1(0),AL3(DSTBEST),XL9'00'                                 
         DC    C'F',AL1(0),AL3(DSTBFEE),XL9'00'                                 
         DC    AL1(255)                                                         
*                                                                               
STATTAB  DC    AL2(ACSTFILT-ACSTATD)                                            
         DC    AL2(ACSTFILT+1-ACSTATD)                                          
         DC    AL2(ACSTANAL-ACSTATD)                                            
         DC    AL2(ACSTSUB-ACSTATD)                                             
         DC    AL2(ACSTFLT5-ACSTATD)                                            
         DC    AL1(255)                                                         
*                                                                               
SALLBL   DC    C'SALES'                                                         
GRPLBL   DC    C'BILLING GROUP'                                                 
NUMLBL   DC    C'NUMBER SCHEME'                                                 
         SPACE 2                                                                
NOTVLMSG DC    C'ERROR - ACCOUNT NOT VALID FOR CLOSING'                         
NOTFDMSG DC    C'ERROR - ACCOUNT NOT FOUND            '                         
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
*                                                                               
WRELO    DS    A                   RELOCATION FACTOR.                           
WKSTRT   DS    0C                                                               
SAVEREF  DS    2F                                                               
SCANNER  DS    A                   A(SCANNER).                                  
SCINKEY  DS    A                   A(SCINKEY)                                   
GETFACT  DS    A                   A(GETFACT).                                  
HELLO    DS    A                   A(HELLO).                                    
GETOPT   DS    V                   V(GETOPT)                                    
ELADDRS  DS    0A                                                               
ADACNUM  DS    A                   A(NUMBER ELEMENT), X'21'.                    
ADACADD  DS    A                   A(ADDRESS ELEMENT), X'22'.                   
ADACPROF DS    A                   A(PROFILE ELEMENT), X'24'                    
ADACJOB  DS    A                   A(JOB ELEMENT), X'26'.                       
ADACSTAT DS    A                   A(STATUS ELEMENT), X'30'                     
ADACXPRF DS    A                   A(EXTRA PROFILE ELEMENT), X'3C'.             
ADACSAN  DS    A                   A(SALES ANALYSIS ELEMENT), X'3D'.            
ADACCOMM DS    A                   A(COMMENT ELEMENT), X'3E'.                   
ADSRPROF DS    A                   A(SR PROFILE ELEMENT), X'24'.                
ADCSPROF DS    A                   A(1C/29 PROFILE ELEMENT), X'24'.             
ADSAPROF DS    A                   A(SJ SALES PROFILE ELEMENT), X'24'.          
ADC2STAT DS    A                   A(29 STATUS ELEMENT), X'30'.                 
ELADDLNQ EQU   *-ELADDRS                                                        
ATIA     DS    A                   A(TIA)                                       
FADR     DS    A                   A(THIS FIELD HEADER).                        
ARECAREA DS    A                   A(RECORD BEING VALIDATED).                   
*                                                                               
CLXPST1  DS    CL1                 CLIENT'S EXTRA PROFILE STATUS BYTE           
CLPRUNIT DS    CL1                 SAVED OFFICE/UNIT FROM CLI OR PRO.           
CLPRUNBL DS    CL12                SAVED UNBILLABLE CODES FROM CLI/PRO.         
CLRECV   DS    CL12                SAVED CLIENT LEVEL REC'VABLE ACC.            
PRRECV   DS    CL12                SAVED PRODUCT LEVEL REC'VABLE ACC.           
THISLEDG DS    CL2                                                              
LEDGLVLS DS    CL4                 L'EACH OF THIS LEDGERS LEVELS.               
SJLVLLNS DS    CL4                 L'EACH OF SJ'S LEVELS.                       
EDWORK   DS    CL20                                                             
*                                                                               
BLKCNT   DS    C                   COUNT OF SCINKEY ITEMS.                      
SCINBLK  DS    20CL20                                                           
*                                                                               
AMOUNT   DS    PL8                                                              
ELCODE   DS    C                                                                
WKNDLNQ  EQU   *-WKSTRT                                                         
         SPACE 1                                                                
*              DSECT TO COVER FIELD TABLE FLDTBL).                              
*                                                                               
FLDTBLD  DSECT                                                                  
FLDNMBR  DS    AL1       FIELD NUMBER.                                          
FLDSTAT1 DS    AL1       FIELD STATUS - X'80' = NOT PRESENT FOR CLIENT.         
*                                      X'40' = NOT PRESENT FOR PRODUCT.         
*                                      X'20' = NOT PRESENT FOR JOB.             
*                                      X'10' = LABEL FIELD.                     
*                                      X'08' = MANDATORY FOR CLIENT.            
*                                      X'04' = MANDATORY FOR PRODUCT.           
*                                      X'02' = MANDATORY FOR JOB.               
FLDSTAT2 DS    AL1       MORE STATUS  - X'80' = PROTECTED FOR CLIENT.           
*                                       X'40' = XC FOR CLIENT.                  
*                                       X'20' = PROTECTED FOR PRODUCT.          
*                                       X'10' = XC FOR PRODUCT.                 
*                                       X'08' = PROTECTED FOR JOB.              
*                                       X'04' = XC FOR JOB.                     
FLDADR   DS    AL2       A(FIELD HEADER) AS FLDHDRH-T603FFD.                    
FLDDIS   DS    AL3       A(DISPLAY ROUTINE).                                    
FLDVAL   DS    AL3       A(VALIDATION ROUTINE).                                 
FLDTBLNQ EQU   *-FLDTBLD                                                        
         SPACE 1                                                                
*              DSECT TO COVER RECORD TYPE TABLE.                                
*                                                                               
TYPTBLD  DSECT                                                                  
TYPREC   DS    CL8       KEYWORD FOR RECORD TYPE.                               
TYPLVL   DS    AL1       ACCOUNT LEVEL FOR TYPE (1,2,3,4).                      
TYPSCR   DS    AL1       SUB-SCREEN NUMBER FOR TYPE.                            
TYPSTAT  DS    AL1       'FIELD NOT PRESENT' BIT FOR TYPE.                      
TYPMAND  DS    AL1       'FIELD MANDATORY' BIT FOR TYPE.                        
TYPPROT  DS    AL1       'FIELD PROTECTED' (DISPLAY ONLY) BIT FOR TYPE.         
TYPXC    DS    AL1       'FIELD NOT USED' BIT FOR TYPE.                         
TYPDISP  DS    AL3       A(POST DISPLAY ROUTINE FOR TYPE).                      
TYPVAL   DS    AL3       A(POST VALIDATION ROUTINE FOR TYPE).                   
TYPNDSC  DS    AL2       DISPLACEMENT OF SUB-SCREEN TAB FIELD.                  
TYPTBLNQ EQU   *-TYPTBLD                                                        
         SPACE 1                                                                
*              DSECT TO COVER EXTRA PROFILE DATA ITEM TABLE.                    
*                                                                               
XPRFTABD DSECT                                                                  
XPRFKWRD DS    CL9       KEYWORD FOR DATA ITEM.                                 
XPRFAFLD DS    AL2       DISPLACEMENT OF ITEM'S FIELD IN ACXPROFD.              
XPRFLFLD DS    AL1       L'ACXPROFD FIELD.                                      
XPRFSTAT DS    AL1       ITEM STATUS -  X'01' = IF XPRFCMPV = VALUE IN          
*                                              RECORD, DISPLAY ITEM.            
*                                              IF BIT IS OFF, DISPLAY           
*                                              ON NOT EQUAL.                    
*                                       X'02' = DISPLAY FROM RECORD.            
*                                       X'04' = ITEM IS PACKED DECIMAL.         
*                                       X'08' = BIT SIGNIFICANT DATE.           
*                                       X'10' = NO DISPLAY FOR KEYWORD          
*                                       BOTH BITS OFF = EBCDIC DATA.            
XPRFDEFV DS    CL4       COMPARISON VALUE FOR FIELD.                            
XPRFDSPV DS    CL6       DISPLAY VALUE IF NOT FROM RECORD.                      
XPRFVRTN DS    AL3       A(SPECIAL VALIDATION ROUTINE).                         
XPRFDRTN DS    AL3                 A(SPECIAL DISPLAY ROUTINE).                  
         ORG   XPRFVRTN                                                         
XPRFOTHV DS    CL3                 NON-DEFAULT VALUE.                           
         ORG                                                                    
XPRFTBLQ EQU   *-XPRFTABD                                                       
         SPACE 1                                                                
*              DSECT TO COVER TABLE OF SJ ELEMENTS TO BE POSTED.                
*                                                                               
ELTABD   DSECT                                                                  
ELTCODE  DS    C                   ELEMENT CODE                                 
ELTELADR DS    AL2                 DISPLACEMENT OF FIELD FOR A(ELEMENT)         
ELTBLNQ  EQU   *-ELTABD                                                         
         SPACE 1                                                                
*              DSECT TO COVER RECORD INFORMATION TABLE.                         
*                                                                               
LDGTABD  DSECT                                                                  
LDGLEDG  DS    CL2                 UNIT/LEDGER FOR RECORD.                      
LDGAREC  DS    AL2                 DISP OF RECORD SLOT IN TIA.                  
LDGAHED  DS    AL2                 A(ACCOUNT CODE FIELD HEADER).                
LDGAELT  DS    AL3                 A(ELEMENT TABLE).                            
LDGTBLNQ EQU   *-LDGTABD                                                        
         SPACE 1                                                                
*        DSECT TO COVER TIA.                                                    
*                                                                               
TIAD     DSECT                                                                  
         DS    C                   THIS GIVES ALL NON-ZERO DSPLCMNTS.           
RECVREC  DS    1001C                                                            
CST1REC  DS    1001C                                                            
CST2REC  DS    1001C                                                            
SALEREC  DS    1001C                                                            
TIALNQ   EQU   *-TIAD                                                           
         SPACE 1                                                                
*              DSECT TO COVER BILLING TYPE DATA TABLE.                          
*                                                                               
BILTTBLD DSECT                                                                  
BILTCOD  DS    CL1       VALUES OF ACPRBILL.                                    
BILTTYP  DS    AL1       'MOVER' = KEYWORD DISPLAY, X'00' = SPECIAL.            
BILTNAR  DS    CL12      KEYWORD VALUE FOR 'MOVER' TYPES.                       
         ORG   BILTNAR                                                          
BILTDRTN DS    AL3       A(DISPLAY ROUTINE) FOR 'SPECIAL' TYPES.                
         ORG                                                                    
BILTTBLQ EQU   *-BILTTBLD                                                       
         SPACE 1                                                                
*              DSECT TO COVER SCINKEY BLOCK.                                    
*                                                                               
SCINBLKD DSECT                                                                  
SCINITEM DS    0CL20               THW WHOLE ITEM                               
SCINKWRD DS    CL10                THE KEYWORD TO BE DISPLAYED.                 
SCINDISP DS    CL10                THE SPACE FOR THE DISPLAY VALUE.             
SCINBLNQ EQU   *-SCINBLKD                                                       
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFME6D                                                       
         ORG   LOGLOADH                                                         
       ++INCLUDE ACLFME5D                                                       
         ORG   LOGLOADH                                                         
       ++INCLUDE ACLFME4D                                                       
         ORG   LOGWORK                                                          
LASTKEY  DS    CL15                                                             
         ORG   LASTKEY                                                          
LASTCOMP DS    C                                                                
         ORG                                                                    
TYPESAVE DS    0CL14               AREA FOR RECORD TYPE CHARACTERISTICS         
TYPELVL  DS    AL1                 ACCOUNT LEVEL FOR TYPE.                      
TYPESCR  DS    AL1                 SUB-SCREEN NUMBER FOR RECORD TYPE.           
TYPESTAT DS    AL1                 FIELD STATUS BIT FOR RECORD TYPE.            
TYPEMAND DS    AL1                 BIT FOR TYPE FOR MANDATORY FIELD.            
TYPEPROT DS    AL1                 BIT FOR TYPE FOR DISPLAY ONLY FIELD.         
TYPEXC   DS    AL1                 BIT FOR TYPE FOR NO SHOW FIELD.              
TYPEDISP DS    AL3                 A(RTN FOR POST DISPLAY PROCESSING).          
TYPEVAL  DS    AL3                 A(RTN FOR POST VALIDATION PROCSSING)         
TYPENDSC DS    AL2                 DISP OF TAB FIELD FOR THIS SCREEN.           
*                                                                               
PRODLEDG DS    CL2                 PRODUCTION UNIT/LEDGER.                      
RECVLEDG DS    CL2                 RECEIVABLE UNIT/LEDGER.                      
COMPSTAT DS    XL1                 COMPANY ELEMENT STATUS BYTE.                 
USERIP   DS    CL1                 USER INPUT BYTE                              
       ++INCLUDE ACGOBLOCK                                                      
         EJECT                                                                  
CLIMAND  EQU   X'08'                                                            
PROMAND  EQU   X'04'                                                            
JOBMAND  EQU   X'02'                                                            
ALLMAND  EQU   CLIMAND+PROMAND+JOBMAND                                          
*                                                                               
CLIMISS  EQU   X'80'                                                            
PROMISS  EQU   X'40'                                                            
JOBMISS  EQU   X'20'                                                            
LABELQ   EQU   X'10'                                                            
CLIPROT  EQU   X'80'                                                            
CLIXC    EQU   X'40'                                                            
PROPROT  EQU   X'20'                                                            
PRODXC   EQU   X'10'                                                            
JOBPROT  EQU   X'08'                                                            
JOBXC    EQU   X'04'                                                            
ALLXC    EQU   CLIXC+PRODXC+JOBXC                                               
ALLPROT  EQU   CLIPROT+PROPROT+JOBPROT                                          
MOVER    EQU   X'01'                                                            
*                                                                               
EQUAL    EQU   X'01'                                                            
FLDVALQ  EQU   X'02'                                                            
PACKED   EQU   X'04'                                                            
BITS     EQU   X'08'                                                            
NODSP    EQU   X'10'                                                            
DSPRECV  EQU   X'80'               DISPLAY RECORD VALUE                         
DUPINPUT EQU   35                                                               
RECNTFND EQU   53                                                               
ACTOOSHT EQU   112                                                              
WRNGLVAC EQU   103                                                              
*                                                                               
       ++INCLUDE ACLFMEQU                                                       
*              INCLUDED HERE ARE - ACGENBOTH                                    
*                                  DNNTWA                                       
*                                  DDFLDIND                                     
*                                  DDCOMFACS                                    
*                                  FAFACTS                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE FATWA                                                          
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'160ACLFM19   05/01/02'                                      
         END                                                                    
