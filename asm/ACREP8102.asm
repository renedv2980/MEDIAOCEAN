*          DATA SET ACREP8102  AT LEVEL 030 AS OF 05/22/03                      
*PHASE AC8102A                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'CONSOLIDATED TRIAL BALANCE'                                     
AC8102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC8102,R9       R9 IS SECOND BASE REG                        
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC8102D,RC                                                       
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              RUNFRST                                                          
*-------------------------------------------------------------------*           
         CLI   MODE,RUNFRST                                                     
         BNE   RQF10                                                            
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY2)                                
         GOTO1 (RF),(R1),(4,RCDATE),(1,TODAY3)                                  
         RELOC (R5)                                                             
         ST    R5,RELO                                                          
         LA    RE,RELOTAB                                                       
         LA    RF,ATYPES                                                        
RNF10    L     R1,0(RE)            V-TYPE                                       
         A     R1,RELO             ADD RELO FACTOR                              
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RNF10                                                            
                                                                                
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
                                                                                
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              REQFRST                                                          
*-------------------------------------------------------------------*           
RQF10    CLI   MODE,REQFRST                                                     
         BNE   UNF10                                                            
                                                                                
         MVI   RCSUBPRG,0                                                       
                                                                                
         USING ACMD,R3                                                          
         L     R3,AMONACC                                                       
         MVI   ACMOCBF,C'N'          SET OFF/CONTRA/BF TO DEFAULT               
         MVI   FCGENBUK,C'O'         SET GENBUK TO OFFICE DEFAULT               
         MVI   FCRDHIST,C'Y'         SET READ HISTORY TO DEFAULT                
*                                                                               
         MVC   SVACMSTR,ACMMSTR      SAVE MOA START AND END INCASE I            
         MVC   SVACMEND,ACMMEND      CHANGE IT BEFORE PROCESSING                
         MVC   SVAOCBF,ACMOCBF       SWITCH TO SEND OF/CON BBF RECS             
         MVC   SVPACC,SPACES                                                    
         MVC   SVPSBAC,SPACES                                                   
                                                                                
         CLI   PROGPROF+2,C'Y'       PROFILE TO SUPPRESS INACTIVES              
         BNE   *+8                   IF YES SET OPTION ON - CAN BE              
         MVI   QOPT1,C'S'            EITHER                                     
                                                                                
         LA    R2,RACCUMS            INITIALIZE ALL ACCUMS                      
         LA    R3,RACMNUM                                                       
         ZAP   0(RACMLEN,R2),=P'0'                                              
         LA    R2,RACMLEN(R2)                                                   
         BCT   R3,*-10                                                          
                                                                                
         MVI   ROPTST,0              SET ONE SWITCH FOR ALL POSSIBLE            
         MVI   ACTVSW,0              CLEAR ACTIVITY SWITCH                      
         CLI   QSEQ,C' '             COMBINATIONS                               
         BE    RQF30                                                            
         MVI   ROPTST,ROPTDET        DETAIL SEQUENCE (CONTRA NOT VALID          
         CLI   QSEQ,QSEQDET          HERE)                                      
         BE    RQF30                                                            
         CLI   QOPT6,C'C'                                                       
         BE    RQF24                                                            
         MVI   ROPTST,ROPTACC        ACCOUNT SEQUENCE - NO CONTRA               
         CLI   QSEQ,QSEQACC                                                     
         BE    RQF30                                                            
         MVI   ROPTST,ROPTOFF        OFFICE SEQUENCE - NO CONTRA                
         CLI   QSEQ,QSEQOFF                                                     
         BE    RQF30                                                            
         DC    H'0'                                                             
RQF24    MVI   ROPTST,ROPTACON       ACCOUNT SEQUENCE - CONTRA DETAIL           
         OI    ROPTST,ROPTCONT                                                  
         CLI   QSEQ,QSEQACC                                                     
         BE    RQF30                                                            
         MVI   ROPTST,ROPTOCON       OFFICE SEQUENCE - CONTRA DETAIL            
         OI    ROPTST,ROPTCONT                                                  
         CLI   QSEQ,QSEQOFF                                                     
         BE    RQF30                                                            
         DC    H'0'                                                             
                                                                                
RQF30    DS    0H                                                               
         TM    ROPTST,ROPTACON       IF ACCOUNT SEQUENCE - NO CONTRA            
         BNO   *+8                   DON'T GENERATE ANY BUCKETS (WAS            
         MVI   FCGENBUK,C'N'         TURNED ON IN 01 PHASE)                     
                                                                                
         TM    ROPTST,ROPTOCON       IF OFF/CON SEQ GENEREATE OFFICE            
         BNO   *+8                   CONTRA BUCKETS AND PASS ME                 
         MVI   FCGENBUK,FCGENCON     MODE=PROCCBUK                              
                                                                                
         TM    ROPTST,ROPTACON       IF ACC/CON SEQ CHECK REQUEST FOR           
         BNO   RQF34                 SINGLE OFFICE OR OFFICE LIST               
         USING ACMD,R3                                                          
         L     R3,AMONACC                                                       
         CLC   QOFFICE,SPACES        CHECK OFFICE FIELD                         
         BE    RQF34                                                            
         MVI   ACMOCBF,C'Y'          SET SWITCH TO PASS STRAIGHT                
         MVI   SVAOCBF,C'Y'          ACC/OFF/CON BBF RECORDS                    
         MVI   FCRDHIST,C'N'         DON'T READ REGULAR HISTORY RECS            
         XC    ACMMSTR,ACMMSTR       CLEAR START DATE - I'M GOING TO            
         DROP  R3                    NEED TRANSACTIONS THROUGH END DATE         
                                                                                
RQF34    DS    0H                                                               
         CLI   QOPT7,C'Y'            DOWNLOADING                                
         BE    RQF60                                                            
         CLI   QOPT5,C'Y'                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,2            DON'T SHOW OPENING BALANCES                
         GOTO1 PROLLER,DMCB,0,(8,ACCUMS),8,5                                    
                                                                                
         MVC   PAGE,=H'1'                                                       
         MVC   HEADMON,SPACES        CLEAR HEADING LINE                         
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         CLI   ACMMEND,X'FF'                                                    
         BE    RQF40                                                            
         CLI   SVACMSTR,0                                                       
         BE    RQF43                                                            
         CLC   SVACMSTR,ACMMEND      MONTHS EQUAL = 1 MONTH ONLY                
         BE    RQF45                                                            
                                                                                
         MVC   HEADMON(25),=C'POSTINGS MMM/YY TO MMM/YY'                        
         MVC   HEADMON+9(L'ACMCMSTR),ACMCMSTR     STARTING MONTH                
         MVC   HEADMON+19(L'ACMCMEND),ACMCMEND        ENDING MONTH              
         B     RQF48                                                            
                                                                                
RQF40    MVC   HEADMON(8),=C'MONTH OF'        CURRENT MONTH ONLY                
         GOTO1 DATCON,DMCB,(4,RCDATE),(6,HEADMON+9)                             
         B     RQF48                                                            
                                                                                
RQF43    MVC   HEADMON(13),=C'POSTINGS THRU'  1ST MONTH THRU                    
         MVC   HEADMON+14(L'ACMCMEND),ACMCMEND    MONTH SPECIFIED               
         B     RQF48                                                            
                                                                                
RQF45    MVC   HEADMON(20),=C'MMM/YY POSTINGS ONLY' 1 MONTH ONLY                
         MVC   HEADMON(L'ACMCMSTR),ACMCMSTR                                     
                                                                                
         DROP  R3                                                               
RQF48    LA    R2,REQGEN                                                        
         USING LEVGEND,R2                                                       
         XC    LEVGEN,LEVGEN                                                    
         MVI   LEVLET,C'R'                                                      
         MVI   LEVNUM,1                                                         
         MVI   LEVACMNO,8                                                       
         MVC   LEVKEY(L'LEVKEY+L'LEVDESC+L'LEVNAME),SPACES                      
         MVC   LEVNAME(7),=C'REQUEST'                                           
         B     CTXIT                                                            
                                                                                
RQF60    MVI   RCSUBPRG,3                                                       
         MVI   DLSTAT,0                                                         
         ZAP   BCASHA,=P'0'                                                     
         ZAP   BCASHB,=P'0'                                                     
         ZAP   BCASHC,=P'0'                                                     
         ZAP   DLDUB,=P'0'                                                      
         LA    R5,DLBUFF           INITIALISE DLFLD                             
         USING DLCBD,R5                                                         
         MVI   DLCBACT,DLCBSOR                                                  
         LA    RE,DLPLINE                                                       
         ST    RE,DLCBAPL                                                       
         LA    RE,DLPRINT                                                       
         ST    RE,DLCBAPR                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DOWNLOAD,(R5)                                                    
         MVC   DLPLINE,SPACES                                                   
         B     CTXIT                                                            
         DROP  R5                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              UNITFRST                                                         
*-------------------------------------------------------------------*           
UNF10    CLI   MODE,UNITFRST                                                    
         BNE   LDF10                                                            
         CLI   RCSUBPRG,3                                                       
         BE    CTXIT               DOWN LOADING                                 
         LA    R2,UNITGEN                                                       
         USING LEVGEND,R2                                                       
                                                                                
         XC    LEVGEN,LEVGEN                                                    
         MVI   LEVLET,C'U'                                                      
         MVI   LEVNUM,1                                                         
         MVI   LEVACMNO,7                                                       
         MVC   LEVKEY(L'LEVKEY+L'LEVDESC+L'LEVNAME),SPACES                      
                                                                                
         L     R4,ADUNTNAM                                                      
         USING NAMELD,R4                                                        
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EXMVC R1,LEVNAME,NAMEREC                                               
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              LEDGFRST                                                         
*-------------------------------------------------------------------*           
LDF10    CLI   MODE,LEDGFRST                                                    
         BNE   OFF10                                                            
         CLI   RCSUBPRG,3                                                       
         BE    CTXIT               DOWN LOADING                                 
                                                                                
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFF,(X'80',1)                           
                                                                                
         XC    LEVACONT(4*L'LEVACONT),LEVACONT                                  
         XC    LEVALOW(8),LEVALOW                                               
         XC    LOWACTV(8),LOWACTV                                               
                                                                                
         MVI   SPACESW,0           FORCE AT LEAST ONE TOTAL LINE.               
         MVI   ACTIVE,C'N'                                                      
         MVC   LASTFIGS,SPACES                                                  
         MVI   LASTSPAC,2          SO I DON'T SPACE AT BEGINNING                
         MVI   FORCEHED,C'Y'                                                    
                                                                                
         LA    R2,LEDGEN                                                        
         USING LEVGEND,R2                                                       
         XC    LEVGEN,LEVGEN                                                    
         MVI   LEVLET,C'L'                                                      
         MVI   LEVNUM,1                                                         
         MVI   LEVACMNO,6                                                       
         MVC   LEVKEY(L'LEVKEY+L'LEVDESC+L'LEVNAME),SPACES                      
                                                                                
         L     R4,ADLDGNAM                                                      
         USING NAMELD,R4                                                        
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EXMVC R1,LEVNAME,NAMEREC                                               
                                                                                
         BAS   RE,STRUCTUR                                                      
                                                                                
**T                                                                             
         CLI   QOPT2,C' '                                                       
         BE    CTXIT                                                            
         CLI   QOPT6,C'C'                                                       
         BNE   CTXIT                                                            
         CLI   QSEQ,C'A'                                                        
         BE    LDF20                                                            
         CLI   QSEQ,C'O'                                                        
         BNE   CTXIT                                                            
LDF20    CLC   LOWLEV,REQLOW                                                    
         BE    CTXIT                                                            
         MVI   ROPTST,ROPTACC                                                   
         CLI   QSEQ,C'A'                                                        
         BE    CTXIT                                                            
         MVI   ROPTST,ROPTOFF                                                   
**T                                                                             
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              OFFIRST                                                          
*-------------------------------------------------------------------*           
OFF10    CLI   MODE,OFFIRST                                                     
         BNE   OFA10                                                            
                                                                                
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BZ    CTXIT                                                            
                                                                                
         ZAP   MYOBAL,=P'0'        CLEAR OFFICE BUCKETS                         
         ZAP   OBCASH,=P'0'                                                     
         ZAP   ODCASH,=P'0'                                                     
         ZAP   OCCASH,=P'0'                                                     
                                                                                
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         MVC   OFFNAME,ACMOFNAM                                                 
         L     R2,ACMAOFA                                                       
         USING OFARECD,R2                                                       
         MVC   OFFCODE,OFAKOFF     EXTRACT OFFICE CODE                          
                                                                                
         MVI   FORCEHED,C'Y'       BREAK A PAGE FOR NEW OFFICE                  
                                                                                
         B     CTXIT                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              OFFICE ACCOUNT FIRST                                             
*-------------------------------------------------------------------*           
OFA10    CLI   MODE,OFACFRST                                                    
         BNE   LVAF10                                                           
                                                                                
         TM    ROPTST,ROPTDET                                                   
         BNO   CTXIT                                                            
                                                                                
         ZAP   MYOBAL,=P'0'        CLEAR OFFICE BUCKETS                         
         ZAP   ODCASH,=P'0'                                                     
         ZAP   OCCASH,=P'0'                                                     
                                                                                
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         MVC   OFFNAME,ACMOFNAM                                                 
         L     R2,ACMAOFA                                                       
         USING OFARECD,R2                                                       
         MVC   OFFCODE,OFAKOFF     EXTRACT OFFICE CODE                          
                                                                                
         B     CTXIT                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              LEVAFRST ETC.                                                    
*-------------------------------------------------------------------*           
LVAF10   CLI   MODE,LEVAFRST                                                    
         BNE   LVBF10                                                           
         CLI   RCSUBPRG,3          DOWN LOADING                                 
         BE    CTXIT                                                            
         LA    R2,LEVAGEN                                                       
         L     R4,ADLVANAM                                                      
         L     R6,ADHEIRA                                                       
         BAS   RE,LEVUP                                                         
         MVI   FIRSTLOW,C'Y'                                                    
         XC    LEVALOW(8),LEVALOW                                               
         B     CTXIT                                                            
                                                                                
LVBF10   CLI   MODE,LEVBFRST                                                    
         BNE   LVCF10                                                           
         CLI   RCSUBPRG,3          DOWNLOADING?                                 
         BE    CTXIT                                                            
         LA    R2,LEVBGEN                                                       
         L     R4,ADLVBNAM                                                      
         L     R6,ADHEIRB                                                       
         BAS   RE,LEVUP                                                         
         XC    LEVBLOW(6),LEVBLOW                                               
         B     CTXIT                                                            
                                                                                
LVCF10   CLI   MODE,LEVCFRST                                                    
         BNE   PACC10                                                           
         CLI   RCSUBPRG,3          DOWNLOADING?                                 
         BE    CTXIT                                                            
         LA    R2,LEVCGEN                                                       
         L     R4,ADLVCNAM                                                      
         L     R6,ADHEIRC                                                       
         BAS   RE,LEVUP                                                         
         XC    LEVCLOW(4),LEVCLOW                                               
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              PROCACC                                                          
*-------------------------------------------------------------------*           
PACC10   CLI   MODE,PROCACC                                                     
         BNE   PSB10                                                            
         CLI   RCSUBPRG,3                                                       
         BE    PACC40              SKIP LEVEL SETTING IF DOWN LOADING           
         XC    LEVDLOW,LEVDLOW                                                  
                                                                                
         MVI   ACTVSW,0            CLEAR ACTIVITY SWITCH                        
         MVI   ACTIVE,C'Y'         SWITCH DENOTES PRESENCE OF LOW LEVEL         
         LA    R2,LEVAGEN                                                       
         USING LEVGEND,R2                                                       
         LA    R3,4                                                             
PACC20   CLC   LEVLET,LOWLEV                                                    
         BE    PACC25                                                           
         LA    R2,L'LEVGEN(R2)                                                  
         BCT   R3,PACC20                                                        
         DC    H'0',C'HORRIBLE PROBLEM'                                         
                                                                                
PACC25   L     R4,ADACCNAM                                                      
         L     R6,ADACC                                                         
         BAS   RE,LEVUP                                                         
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BNZ   PACC40                                                           
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   PACC40                                                           
         CLI   QOPT2,C' '                                                       
         BNE   PACC40                                                           
         TM    ROPTST,ROPTDET                                                   
         BNO   *+16                                                             
         CLI   OFFCACTV,C'Y'       ANYTHING PRINTED FOR PRIOR ACCOUNT?          
         BNE   *+8                                                              
         BAS   RE,CTREPORT         SKIP A LINE                                  
         MVI   OFFCACTV,C'N'       NO ACTIVITY FOR OFFICE YET                   
         LA    R4,P                FOR NEW FILE-SHOW ACCT NAME FIRST            
         USING PLINED,R4                                                        
         MVC   PLKEY,LEVKEY                                                     
         MVC   PLDESC,LEVNAME                                                   
                                                                                
         TM    ROPTST,ROPTDET      SHOW OFFICE DETAIL?                          
         BNO   PACC40                                                           
         MVC   SAVEP,P             YES -- SAVE PRINT LINE                       
                                                                                
PACC40   DS    0H                                                               
         TM    ROPTST,ROPTCONT     SHOWING CONTRA DETAIL                        
         BZ    PACC60                                                           
         LA    R4,SVPACC           SAVE PRINT LINE UNTIL I KNOW I HAVE          
         USING PLINED,R4           ACTIVITY                                     
         MVC   PLKEY,LEVKEY                                                     
         MVC   PLDESC,LEVNAME                                                   
                                                                                
         ZAP   OBCASH,=P'0'        CLEAR CONTRA ACCUMULATORS                    
         ZAP   ODCASH,=P'0'                                                     
         ZAP   OCCASH,=P'0'                                                     
         ZAP   MYOBAL,=P'0'                                                     
                                                                                
PACC60   L     R4,ADACCBAL         DO I HAVE RECORD                             
         LTR   R4,R4                                                            
         BZ    CTXIT                                                            
         L     R6,ADACC                                                         
         AH    R6,DATADISP                                                      
         SR    R1,R1                                                            
PACC65   CLI   0(R6),0                                                          
         BE    PACC80                                                           
         CLI   0(R6),APOELQ        LOOK FOR '33' (PEEL) ELEMENT                 
         BE    PACC70                                                           
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         LTR   R1,R1                                                            
         BNZ   PACC65                                                           
         DC    H'0',C'ZERO LENGTH ELEMENT'                                      
                                                                                
         USING APOELD,R6                                                        
         USING ABLELD,R4                                                        
PACC70   CLC   TODAY3,APOPLDT      FIX UP BALANCE ELEMENT IF WE PEELED          
         BNE   PACC80              TODAY                                        
         SP    ABLFRWD,APODR                                                    
         AP    ABLFRWD,APOCR                                                    
         AP    ABLDR,APODR                                                      
         AP    ABLCR,APOCR                                                      
                                                                                
PACC80   EQU   *                                                                
         ZAP   MYBAL,=P'0'                                                      
         ZAP   DCASH,=P'0'                                                      
         ZAP   CCASH,=P'0'                                                      
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              PROCSBAC                                                         
*-------------------------------------------------------------------*           
PSB10    CLI   MODE,PROCSBAC                                                    
         BNE   PTRN10                                                           
                                                                                
         CLI   SVAOCBF,C'Y'        ONLY NEED THIS MODE IF DOING                 
         BNE   CTXIT               ACC/CON WITH OFFICE/OFFICE LIST              
                                                                                
         USING CACRECD,R2                                                       
         L     R2,ADSUBAC          ONLY WANT TO LOOK AT ACC/OFF/CON             
         CLC   CACKOFF,SPACES      RECORDS                                      
         BNH   CTXIT                                                            
                                                                                
         CLC   SVPACC,SPACES       PRINT ACCOUNT IF I HAVE NOT ALREADY          
         BE    PSB20                                                            
         MVC   P,SVPACC                                                         
         GOTO1 CTREPORT                                                         
         MVC   SVPACC,SPACES                                                    
                                                                                
         USING PLINED,R4                                                        
PSB20    LA    R4,P                IF NO ACTIVITY ON LAST SUBACC REC I          
         CLC   PLKEY,SPACES        WOULD NOT GET MODE=SBACLAST, WOULD           
         BNH   PSB30               NOT HAVE PRINTED IT THERE SO I NEED          
         CLC   PLKEY+1(14),CACKULC     TO PRINT IT HERE BEFORE                  
         BE    PSB40               PROCESSING THIS RECORD                       
         ZAP   DUB,CBCASH          IF CONTRA ACCOUNT IS THE SAME                
         AP    DUB,CDCASH          KEEP ACCUMULATING                            
         SP    DUB,CCCASH                                                       
         ZAP   MYCBAL,DUB                                                       
         LA    R3,CBCASH                                                        
         BAS   RE,EDITOR                                                        
         GOTO1 CTREPORT                                                         
                                                                                
PSB30    ZAP   CBCASH,=P'0'        RESET ACCUMULATORS FOR THIS SUBACC           
         ZAP   CDCASH,=P'0'                                                     
         ZAP   CCCASH,=P'0'                                                     
         ZAP   MYCBAL,=P'0'                                                     
                                                                                
         LA    R4,P                SET UP PRINT LINE                            
         MVC   PLKEY,SPACES                                                     
         MVC   PLKEY+1(14),CACKULC                                              
                                                                                
         USING CACELD,R2                                                        
PSB40    AH    R2,DATADISP         SET UP CONTRA ACCOUNT AND NAME               
         CLI   0(R2),CACELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PLDESC,SPACES                                                    
         CLI   CACLN,17                                                         
         BH    *+14                                                             
         MVC   PLDESC(14),CACCNTU                                               
         B     PSB55                                                            
         ZIC   R1,CACLN                                                         
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLDESC(0),CACNAME                                                
         B     PSB55                                                            
                                                                                
PSB50    CLI   0(R2),0                                                          
         BE    PSB99                                                            
         CLI   0(R2),PBKELQ        PROCESS PRIOR BUCKET ELEMENT                 
         BE    PSB60                                                            
         CLI   0(R2),BUKELQ        PROCESS MONTHLY BUCKET ELEMENTS              
         BE    PSB70                                                            
PSB55    ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PSB50                                                            
                                                                                
         USING PBKELD,R2                                                        
PSB60    DS    0H                                                               
         AP    CBCASH,PBKDR        ADD DEBITS AND CREDITS TO CONTRA             
         SP    CBCASH,PBKCR        ACCUMULATORS                                 
         AP    OBCASH,PBKDR        ADD TO THE OFFICE BUCKETS                    
         SP    OBCASH,PBKCR                                                     
         B     PSB55                                                            
                                                                                
         USING BUKELD,R2                                                        
PSB70    L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         CLC   BUKMOS,SVACMEND                                                  
         BH    PSB55                                                            
         OC    SVACMSTR,SVACMSTR                                                
         BZ    PSB80                                                            
         CLC   BUKMOS,SVACMSTR                                                  
         BNL   PSB80                                                            
         AP    CBCASH,BUKDR        IF DATE IS PRIOR TO START DATE               
         SP    CBCASH,BUKCR        ADD TO BBF ACCUM                             
         AP    OBCASH,BUKDR        ADD TO THE OFFICE BUCKETS                    
         SP    OBCASH,BUKCR                                                     
         B     PSB55                                                            
                                                                                
PSB80    AP    CDCASH,BUKDR        ELSE IF IT IS CONSIDERED CURRENT             
         AP    CCCASH,BUKCR        ADD TO DEBIT AND CREDIT BUCKETS              
         AP    ODCASH,BUKDR                                                     
         AP    OCCASH,BUKCR                                                     
         B     PSB55                                                            
                                                                                
PSB99    DS    0H                                                               
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              PROCTRNS                                                         
*-------------------------------------------------------------------*           
PTRN10   CLI   MODE,PROCTRNS                                                    
         BNE   SBACL10                                                          
                                                                                
         CLI   SVAOCBF,C'Y'        IF DOING LIMITED ACC/CON REQ NEED            
         BE    PTRN20              TRANSACTIONS                                 
         TM    ROPTST,ROPTACON     DON'T INCLUDE TRANS IF READING               
         BO    CTXIT               HISTORY RECORDS OR                           
         TM    ROPTST,ROPTOCON     GENERATING OFFICE/CON HISTORIES              
         BO    CTXIT                                                            
                                                                                
PTRN20   L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         CLI   TRNEL,TRNELQ                                                     
         BNE   CTXIT                                                            
         LR    R6,R4                                                            
         SH    R6,DATADISP                                                      
         USING TRNRECD,R6                                                       
         OC    TRNRECD+ACCOPEEL(2),TRNRECD+ACCOPEEL                             
         BZ    PTRN40                                                           
         CLC   TRNRECD+ACCOPEEL(2),TODAY2   IGNORE IF PEELED BEFORE             
         BL    PTRN99                       TODAY                               
                                                                                
PTRN40   DS    0H                                                               
         TM    TRNSTAT,TRNSDR         IS DEBIT SWITCH ON                        
         BO    PTRN60                                                           
         CLI   SVAOCBF,C'Y'           IF DOING LIMITED ACC/CON RUN              
         BNE   PTRN45                 ACCUMULATE IN DIFFERENT PLACE             
         CLI   SVACMSTR,0             WAS START DATE REQUESTED                  
         BE    PTRN45                                                           
         CLC   TRNDATE(2),SVACMSTR    COMPARE START TO TRAN DATE                
         BNL   PTRN45                                                           
         SP    OBCASH,TRNAMNT         ACCUM IN BBF                              
         SP    CBCASH,TRNAMNT                                                   
         B     PTRN99                                                           
PTRN45   AP    CCASH,TRNAMNT          ELSE IN CREDIT ACCUMS                     
         AP    OCCASH,TRNAMNT                                                   
         AP    CCCASH,TRNAMNT                                                   
         B     PTRN99                                                           
                                                                                
PTRN60   DS    0H                                                               
         CLI   SVAOCBF,C'Y'           IF DOING LIMITED ACC/CON RUN              
         BNE   PTRN65                 ACCUMULATE IN DIFFERENT PLACE             
         CLI   SVACMSTR,0             WAS START DATE REQUESTED                  
         BE    PTRN65                                                           
         CLC   TRNDATE(2),SVACMSTR    COMPARE START TO TRAN DATE                
         BNL   PTRN65                                                           
         AP    OBCASH,TRNAMNT         ACCUM IN BBF                              
         AP    CBCASH,TRNAMNT                                                   
         B     PTRN99                                                           
PTRN65   AP    DCASH,TRNAMNT          ELSE IN DEBIT ACCUMS                      
         AP    ODCASH,TRNAMNT                                                   
         AP    CDCASH,TRNAMNT                                                   
         B     PTRN99                                                           
                                                                                
PTRN99   DS    0H                                                               
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        SUBACC LAST                                                            
*-------------------------------------------------------------------*           
SBACL10  CLI   MODE,SBACLAST                                                    
         BNE   PCBK10                                                           
                                                                                
         TM    ROPTST,ROPTACON      ONLY PROCESS THIS MODE ON                   
         BNO   CTXIT                ACC/CON REQUESTS                            
                                                                                
SBACL20  DS    0H                                                               
**MN     CLI   SVAOCBF,C'Y'         IF DOING LIMITED ACC/CON I HAVE             
**MN     BE    SBACL80              NAMES AND ACCUMS ALREADY                    
                                                                                
         USING PLINED,R4                                                        
         LA    R4,SVPSBAC                                                       
         USING CACRECD,R2                                                       
         L     R2,ADSUBAC           GET CONTRA ACCOUNT CODE                     
         SH    R2,DATADISP                                                      
         MVC   PLKEY,SPACES                                                     
         MVC   PLKEY+1(14),CACKULC                                              
                                                                                
         USING CACELD,R2                                                        
         AH    R2,DATADISP          GET CONTRA ACCOUNT NAME                     
         MVC   PLDESC,SPACES                                                    
         CLI   CACLN,17                                                         
         BH    *+14                                                             
         MVC   PLDESC(14),CACCNTU                                               
         B     SBACL25                                                          
         ZIC   R1,CACLN                                                         
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLDESC(0),CACNAME                                                
                                                                                
SBACL25  DS    0H                                                               
         CLI   SVAOCBF,C'Y'         IF DOING LIMITED ACC/CON I HAVE             
         BE    SBACL80              NAMES AND ACCUMS ALREADY                    
                                                                                
SBACL30  CLI   0(R2),0              IF END OF REC PRINT TOTALS                  
         BE    SBACL80                                                          
         CLI   0(R2),PBKELQ         IS IT PRIOR MONTHS BUCKET                   
         BE    SBACL50                                                          
         CLI   0(R2),BUKELQ         IS IT MONTHLY BUCKET                        
         BE    SBACL60                                                          
SBACL40  ZIC   R0,1(R2)             GET NEXT ELEMENT                            
         AR    R2,R0                                                            
         B     SBACL30                                                          
                                                                                
         USING PBKELD,R2                                                        
SBACL50  CLI   ACMATYP,ACMATPL      IS IT P&L ?                                 
         BE    SBACL40              YES, DON'T ADD TO BBF                       
         AP    CBCASH,PBKDR         ACCUMULATE PRIOR DOLLARS IN                 
         SP    CBCASH,PBKCR         BALANCE BROUGHT FORWARD                     
         B     SBACL40                                                          
                                                                                
         USING BUKELD,R2                                                        
*&&DO                                                                           
SBACL60  CLC   BUKMOS,SVACMEND      IF BUCKET MM/YY HIGHER THAN                 
         BH    SBACL40              END MONTH - DON'T USE                       
         OC    SVACMSTR,SVACMSTR    IF NO START EVERYTHING IS                   
         BZ    SBACL65              CURRENT                                     
         CLC   BUKMOS,SVACMSTR      IS BUCKET MM/YY LOWER THAN                  
         BNL   SBACL65              START                                       
         AP    CBCASH,BUKDR         ACCUM IN BBF                                
         SP    CBCASH,BUKCR                                                     
         B     SBACL40              GET NEXT ELEMENT                            
                                                                                
*&&                                                                             
SBACL60  L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         CLC   BUKMOS,ACMMEND       IF BUCKET MM/YY HIGHER THAN END             
         BH    SBACL40              MONTH IN REQUEST - DON'T USE                
         OC    ACMMSTR,ACMMSTR      IF NO START EVERYTHING IS CURRENT           
         BZ    SBACL65                                                          
         CLC   BUKMOS,ACMMSTR       IF BUCKET MM/YY LOWER THAN START            
         BNL   SBACL65              ACCUM IN BBF                                
         CLI   ACMATYP,ACMATPL      IS IT P&L ?                                 
         BNE   SBACL62              NO, ADD TO BBF                              
         CLC   BUKMOS,ACMFDTE       BEFORE FISCAL START ?                       
         BL    SBACL40              YES, SKIP                                   
SBACL62  AP    CBCASH,BUKDR         ADD TO BBF                                  
         SP    CBCASH,BUKCR                                                     
         B     SBACL40              GET NEXT ELEMENT                            
                                                                                
SBACL65  AP    CDCASH,BUKDR         ELSE ACCUM IN CURRENT ACCUMS                
         AP    CCCASH,BUKCR                                                     
         B     SBACL40              GET NEXT ELEMENT                            
                                                                                
SBACL80  ZAP   DUB,CBCASH           TOTAL AND PRINT ALL INFO                    
         AP    DUB,CDCASH           FOR THIS ACC/CON                            
         SP    DUB,CCCASH                                                       
         ZAP   MYCBAL,DUB                                                       
                                                                                
         CLI   QOPT1,C'S'                                                       
         BNE   SBACL82                                                          
         CLC   CBCASH(32),=4PL8'0'                                              
         BE    SBACL88                                                          
         USING PLINED,R4                                                        
SBACL82  CLC   SVPACC,SPACES        ACCOUNT YET DO IT NOW                       
         BE    SBACL85                                                          
         MVC   P,SVPACC                                                         
         GOTO1 CTREPORT                                                         
         MVC   SVPACC,SPACES                                                    
                                                                                
SBACL85  MVC   P,SVPSBAC                                                        
         LA    R4,P                                                             
         LA    R3,CBCASH            PRINT CON/NAME/TOTALS                       
         BAS   RE,EDITOR                                                        
         GOTO1 CTREPORT                                                         
         MVC   SVPSBAC,SPACES                                                   
         OI    ACTVSW,ACTOCON                                                   
                                                                                
SBACL88  CLI   SVAOCBF,C'Y'         ALREADY TOTALED UP IN PROCTRNS              
         BE    SBACL90              WHEN USING MONACC OFFICE FILTER             
         AP    OBCASH,CBCASH        ADD TO THE OFFICE BUCKETS                   
         AP    ODCASH,CDCASH                                                    
         AP    OCCASH,CCCASH                                                    
         AP    MYOBAL,MYCBAL                                                    
                                                                                
SBACL90  ZAP   CBCASH,=P'0'         CLEAR CONTRA ACCUMULATORS                   
         ZAP   CDCASH,=P'0'                                                     
         ZAP   CCCASH,=P'0'                                                     
         ZAP   MYCBAL,=P'0'                                                     
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCCBUK - PROCESS CONTRA BUCKETS GENERATED BY MONACC                  
*-------------------------------------------------------------------*           
PCBK10   CLI   MODE,PROCCBUK                                                    
         BNE   ACCL10                                                           
                                                                                
         TM    ROPTST,ROPTOCON      ONLY USE THESE RECORDS WHEN DOING           
         BNO   CTXIT                OFF/CON                                     
                                                                                
         L     R2,ADSUBAC                                                       
         USING PLINED,R4                                                        
         USING CACRECD,R2           GET CONTRA ACCOUNT CODE                     
PCBK15   LA    R4,SVPSBAC                                                       
         SH    R2,DATADISP                                                      
         MVC   PLKEY,SPACES                                                     
         MVC   PLKEY+1(14),CACKULC                                              
                                                                                
         USING CACELD,R2                                                        
         AH    R2,DATADISP          GET CONTRA ACCOUNT NAME                     
         MVC   PLDESC,SPACES                                                    
         CLI   CACLN,17                                                         
         BH    *+14                                                             
         MVC   PLDESC(14),CACCNTU                                               
         B     PCBK20                                                           
         ZIC   R1,CACLN                                                         
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLDESC(0),CACNAME                                                
                                                                                
PCBK20   L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         L     R2,ACMABUK           ADDRESS OF BUCKET RECORD                    
         AH    R2,DATADISP                                                      
                                                                                
PCBK30   CLI   0(R2),0              IF END OF RECORD PRINT TOTALS               
         BE    PCBK80                                                           
         CLI   0(R2),PBKELQ         PRIOR BUCKETS ELEMENT                       
         BE    PCBK40                                                           
         CLI   0(R2),BUKELQ         MONTHLY BUCKET ELEMENT                      
         BE    PCBK50                                                           
PCBK35   ZIC   R0,1(R2)             GET NEXT ELEMENT                            
         AR    R2,R0                                                            
         B     PCBK30                                                           
                                                                                
         USING PBKELD,R2                                                        
PCBK40   CLI   ACMATYP,ACMATPL      IS IT P&L ?                                 
         BE    PCBK35               YES, DON'T ADD TO BBF                       
         AP    CBCASH,PBKDR         ACCUMULATE PRIOR DOLLARS IN                 
         SP    CBCASH,PBKCR         BALANCE BROUGHT FORWARD                     
         B     PCBK35                                                           
                                                                                
         USING BUKELD,R2                                                        
PCBK50   L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         CLC   BUKMOS,ACMMEND       IF BUCKET MM/YY HIGHER THAN END             
         BH    PCBK35               MONTH IN REQUEST - DON'T USE                
         OC    ACMMSTR,ACMMSTR      IF NO START EVERYTHING IS CURRENT           
         BZ    PCBK70                                                           
         CLC   BUKMOS,ACMMSTR       IF BUCKET MM/YY LOWER THAN START            
         BNL   PCBK70               ACCUM IN BBF                                
         CLI   ACMATYP,ACMATPL      IS IT P&L ?                                 
         BNE   PCBK60               NO, ADD TO BBF                              
         CLC   BUKMOS,ACMFDTE       BEFORE FISCAL START ?                       
         BL    PCBK35               YES, SKIP                                   
PCBK60   AP    CBCASH,BUKDR         ADD TO BBF                                  
         SP    CBCASH,BUKCR                                                     
         B     PCBK35               GET NEXT ELEMENT                            
                                                                                
PCBK70   AP    CDCASH,BUKDR         ELSE ACCUM IN CURRENT ACCUMS                
         AP    CCCASH,BUKCR                                                     
         B     PCBK35               GET NEXT ELEMENT                            
                                                                                
PCBK80   ZAP   DUB,CBCASH           TOTAL ALL INFO FOR THIS                     
         AP    DUB,CDCASH           OFFICE/CON                                  
         SP    DUB,CCCASH                                                       
         ZAP   MYCBAL,DUB                                                       
                                                                                
         AP    OBCASH,CBCASH        ADD TO THE OFFICE BUCKETS                   
         AP    ODCASH,CDCASH                                                    
         AP    OCCASH,CCCASH                                                    
         AP    MYOBAL,MYCBAL                                                    
                                                                                
         AP    CBCASHC,CBCASH       ADD TO CONTRA ACCUMS BY OFFICE              
         AP    CDCASHC,CDCASH                                                   
         AP    CCCASHC,CCCASH                                                   
         AP    MYCBALC,MYCBAL                                                   
                                                                                
         USING PLINED,R4                                                        
         CLI   QOPT1,C'S'                                                       
         BNE   PCBK82                                                           
         CLC   CBCASH(32),=4PL8'0'                                              
         BE    PCBK90                                                           
PCBK82   LA    R4,SVPACC                                                        
         CLC   SVPACC,SPACES        DO IT NOW                                   
         BE    PCBK85                                                           
         MVC   P,SVPACC                                                         
         GOTO1 CTREPORT                                                         
         MVC   SVPACC,SPACES        CLEAR SAVE AREA                             
                                                                                
PCBK85   MVC   P,SVPSBAC            PRINT LINE FOR THIS OFF/CON                 
         LA    R4,P                                                             
         LA    R3,CBCASH                                                        
         BAS   RE,EDITOR                                                        
         GOTO1 CTREPORT                                                         
         MVC   SVPSBAC,SPACES       CLEAR SAVE AREA                             
         OI    ACTVSW,ACTOCON       ACTIVITY ON THIS SUB ACCOUNT                
                                                                                
PCBK90   ZAP   CBCASH,=P'0'         CLEAR ACCUMULATORS FOR NEXT PASS            
         ZAP   CDCASH,=P'0'                                                     
         ZAP   CCCASH,=P'0'                                                     
         ZAP   MYCBAL,=P'0'                                                     
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              ACCLAST                                                          
*-------------------------------------------------------------------*           
ACCL10   CLI   MODE,ACCLAST                                                     
         BNE   OFFL10                                                           
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BNZ   CTXIT                                                            
                                                                                
ACCL15   L     R3,AMONACC          ** CAN GET HERE FROM MODE OFACLAST *         
         USING ACMD,R3                                                          
         ZAP   BCASH,=P'0'                                                      
                                                                                
         TM    ROPTST,ROPTCONT                                                  
         BZ    ACCL20                                                           
         MVC   BCASH(4*8),OBCASH   USE OFFICE TOTALS                            
                                                                                
         CLI   SVAOCBF,C'Y'        COULD HAVE AN ENTRY LEFT TO PRINT            
         BNE   ACCL25              FROM PROCSBAC IF I HAD NO ACTIVITY           
         USING PLINED,R4           AND DID NOT GET SBACLAST                     
         LA    R4,P                                                             
         CLC   PLKEY,SPACES                                                     
         BNH   ACCL25                                                           
         LA    R3,CBCASH                                                        
         BAS   RE,EDITOR                                                        
         GOTO1 CTREPORT                                                         
         ZAP   CBCASH,=P'0'                                                     
         ZAP   CDCASH,=P'0'                                                     
         ZAP   CCCASH,=P'0'                                                     
         ZAP   MYCBAL,=P'0'                                                     
         B     ACCL25                                                           
                                                                                
ACCL20   LA    RF,ACMABAL                                                       
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BZ    *+8                                                              
         LA    RF,ACMOBAL                                                       
         CLI   QOPT5,C'Y'                                                       
         BE    *+10                DON'T SHOW OPENING BALANCE                   
         ZAP   BCASH,0(L'ACMABAL,RF)                                            
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BZ    *+10                                                             
         AP    OBCASH,BCASH                                                     
ACCL25   ZAP   DUB,BCASH                                                        
         AP    DUB,DCASH                                                        
         SP    DUB,CCASH                                                        
         CLI   RCSUBPRG,3                                                       
         BNE   ACCL30                                                           
         MVI   DLEVSW,4                                                         
         BAS   RE,CTDOWN                                                        
         B     CTXIT                                                            
                                                                                
ACCL30   CLI   QOPT3,C' '                                                       
         BE    ACCL37                                                           
         CLI   QOPT3,C'C'                                                       
         BNE   ACCL35                                                           
         CP    DUB,=P'0'           FOR CR - BAL MUST BE LT ZERO                 
         BNL   ACCL50                                                           
         B     ACCL37                                                           
ACCL35   CP    DUB,=P'0'           FOR DR - BAL MUST BE GT ZERO                 
         BNH   ACCL50                                                           
ACCL37   GOTO1 PROLLER,DMCB,1,ACCUMS,1                                          
         L     R3,DMCB                                                          
         MVC   0(24,R3),BCASH                                                   
         ZAP   DUB,BCASH                                                        
         AP    DUB,DCASH                                                        
         SP    DUB,CCASH                                                        
         ZAP   24(8,R3),DUB                                                     
*-------------------------------------------------------------------*           
* THIS WAS THE CODE WHEN PROGPROF+2 READ                                        
* 'SUPPRESS NIL BALANCE ACCOUNTS'                                               
* NOW IT SHOULD READ                                                            
* 'SUPRESS INACTIVE ACCOUNTS'                                                   
* AND LOGIC IS THE SAME AS QOPT1 = S                                            
*                                                                               
* HOWEVER UK PROTEST HAS LEAD TO THE ORIGINAL MEANING                           
* BEING RESTORED (6/85)                                                         
*-------------------------------------------------------------------*           
         GOTO1 PROLLER,DMCB,6,ACCUMS                                            
                                                                                
ACCL50   LA    R2,LEVAGEN                                                       
         LA    R3,4                FOUR LEVELS                                  
         USING LEVGEND,R2                                                       
ACCL55   CLC   LEVLET,LOWLEV                                                    
         BE    ACCL70                                                           
         LA    R2,L'LEVGEN(R2)                                                  
         BCT   R3,ACCL55                                                        
         DC    H'0',C'HORRIBLE ERROR'                                           
                                                                                
ACCL70   BAS   RE,LEVLAST                                                       
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              OFFLAST                                                          
*-------------------------------------------------------------------*           
OFFL10   CLI   MODE,OFFLAST                                                     
         BNE   OFAL10                                                           
                                                                                
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BZ    CTXIT                                                            
         TM    ROPTST,ROPTCONT                                                  
         BO    OFFL50                                                           
                                                                                
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
                                                                                
         ZAP   DUB,OBCASH                                                       
         AP    DUB,ODCASH                                                       
         SP    DUB,OCCASH                                                       
         ZAP   MYOBAL,DUB                                                       
                                                                                
         MVC   BUFFOFFC,OFFCODE    OFFICE CODE                                  
         MVC   BUFFOFFN,OFFNAME    OFFICE NAME                                  
         MVC   BUFFACCS,OBCASH     SAVE OFFICE ACCMULATORS                      
         MVI   BUFFLEVL,0          LOWEST LEVEL OFFICE TOTALS                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
         MVI   BUFFLEVL,X'FE'      OFFICE TOTALS FOR WHOLE LEDGER               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
         MVI   BUFFLEVL,X'FF'      LEDGER-WIDE TOTAL ACROSS OFFICES             
         XC    BUFFOFFC,BUFFOFFC                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
                                                                                
         CLI   QOPT1,C'S'          SUPPRESS INACTIVE ACCOUNTS?                  
         BNE   *+14                                                             
         CLC   OBCASH(32),=4PL8'0' YES                                          
         BE    OFFL30                                                           
                                                                                
         CLI   QOPT2,C' '          SUPPRESS DETAILS?                            
         BNE   OFFL30              YES                                          
                                                                                
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVC   PLSPACES,SPACES     CLEAR POSSIBLE GARBAGE                       
         MVC   PLTOT,=C'TOTALS FOR'                                             
         MVC   PLTOT+11(L'OFFNAME),OFFNAME                                      
         LA    R3,OBCASH                                                        
         BAS   RE,EDITOR                                                        
         BAS   RE,CTREPORT                                                      
                                                                                
OFFL30   MVC   OFFNAME,SPACES                                                   
         MVC   OFFCODE,SPACES                                                   
         ZAP   OBCASH,=P'0'                                                     
         B     CTXIT                                                            
                                                                                
OFFL50   MVC   BUFFOFFC,OFFCODE    OFFICE CODE                                  
         MVC   BUFFOFFN,OFFNAME    OFFICE NAME                                  
         MVC   BUFFACCS,CBCASHC    SAVE OFFICE ACCMULATORS                      
         MVI   BUFFLEVL,0          LOWEST LEVEL OFFICE TOTALS                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
         MVI   BUFFLEVL,X'FE'      OFFICE TOTALS FOR WHOLE LEDGER               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
         MVI   BUFFLEVL,X'FF'      LEDGER-WIDE TOTAL ACROSS OFFICES             
         XC    BUFFOFFC,BUFFOFFC                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
                                                                                
         CLI   QOPT1,C'S'          SUPPRESS INACTIVE ACCOUNTS?                  
         BNE   *+14                                                             
         CLC   CBCASHC(32),=4PL8'0' YES                                         
         BE    OFFL80                                                           
                                                                                
         CLI   QOPT2,C' '          SUPPRESS DETAILS?                            
         BNE   OFFL80              YES                                          
                                                                                
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVC   PLSPACES,SPACES     CLEAR POSSIBLE GARBAGE                       
         MVC   PLTOT,=C'TOTALS FOR'                                             
         MVC   PLTOT+11(L'OFFNAME),OFFNAME                                      
         LA    R3,CBCASHC                                                       
         BAS   RE,EDITOR                                                        
         BAS   RE,CTREPORT                                                      
                                                                                
OFFL80   MVC   OFFNAME,SPACES                                                   
         MVC   OFFCODE,SPACES                                                   
         ZAP   OBCASH,=P'0'                                                     
         ZAP   CBCASHC,=P'0'                                                    
         ZAP   CDCASHC,=P'0'                                                    
         ZAP   CCCASHC,=P'0'                                                    
         ZAP   MYCBALC,=P'0'                                                    
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              OFFICE ACCOUNT LAST                                              
*-------------------------------------------------------------------*           
OFAL10   CLI   MODE,OFACLAST                                                    
         BNE   LVAL10                                                           
                                                                                
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BNZ   ACCL15                                                           
                                                                                
         TM    ROPTST,ROPTDET                                                   
         BZ    CTXIT                                                            
                                                                                
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
                                                                                
         ZAP   OBCASH,=P'0'                                                     
         CLI   QOPT5,C'Y'                                                       
         BE    *+10                                                             
         ZAP   OBCASH,ACMOBAL                                                   
                                                                                
         ZAP   DUB,OBCASH                                                       
         AP    DUB,ODCASH                                                       
         SP    DUB,OCCASH                                                       
         ZAP   MYOBAL,DUB                                                       
                                                                                
         MVC   BUFFOFFC,OFFCODE    OFFICE CODE                                  
         MVC   BUFFOFFN,OFFNAME    OFFICE NAME                                  
         MVC   BUFFACCS,OBCASH     SAVE OFFICE ACCMULATORS                      
         MVI   BUFFLEVL,0          LOWEST LEVEL OFFICE TOTALS                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
         MVI   BUFFLEVL,X'FE'      OFFICE TOTALS FOR WHOLE LEDGER               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
         MVI   BUFFLEVL,X'FF'      LEDGER-WIDE TOTAL ACROSS OFFICES             
         XC    BUFFOFFC,BUFFOFFC                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
                                                                                
         CLI   QOPT1,C'S'          SUPPRESS INACTIVE ACCOUNTS?                  
         BNE   *+14                                                             
         CLC   OBCASH(32),=4PL8'0' YES                                          
         BE    CTXIT                                                            
                                                                                
         CLI   QOPT2,C' '          SUPPRESS DETAILS?                            
         BNE   CTXIT               YES                                          
         MVI   OFFCACTV,C'Y'       WE'VE PRINTED FOR AN OFFICE                  
         CLC   SAVEP,SPACES                                                     
         BE    *+20                                                             
         MVC   P,SAVEP                                                          
         BAS   RE,CTREPORT                                                      
         MVC   SAVEP,SPACES                                                     
                                                                                
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         LA    R3,OBCASH                                                        
         BAS   RE,EDITOR                                                        
         MVC   PLOFFCD,OFFCODE     OFFICE CODE AND NAME                         
         LA    R0,L'OFFNAME                                                     
         GOTO1 CHOPPER,DMCB,((R0),OFFNAME),(20,PLOFFNM),(C'P',2)                
         BAS   RE,CTREPORT                                                      
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              LEVALAST ETC.                                                    
*-------------------------------------------------------------------*           
LVAL10   LA    R2,LEVAGEN                                                       
         CLI   MODE,LEVALAST                                                    
         BNE   LVBL10                                                           
         CLI   RCSUBPRG,3                                                       
         BNE   LALL10                                                           
         MVI   DLEVSW,1                                                         
         B     LALL10                                                           
LVBL10   LA    R2,LEVBGEN                                                       
         CLI   MODE,LEVBLAST                                                    
         BNE   LVCL10                                                           
         CLI   RCSUBPRG,3                                                       
         BNE   LALL10                                                           
         MVI   DLEVSW,2                                                         
         B     LALL10                                                           
LVCL10   LA    R2,LEVCGEN                                                       
         CLI   MODE,LEVCLAST                                                    
         BNE   LDGL10                                                           
         CLI   RCSUBPRG,3                                                       
         BNE   LALL10                                                           
         MVI   DLEVSW,3                                                         
         B     LALL10                                                           
                                                                                
LALL10   CLI   RCSUBPRG,3                                                       
         BNE   *+12                                                             
         BAS   RE,CTDOWN                                                        
         B     CTXIT                                                            
         BAS   RE,LEVLAST                                                       
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              LEDGLAST ETC.                                                    
*-------------------------------------------------------------------*           
LDGL10   LA    R2,LEDGEN                                                        
         CLI   MODE,LEDGLAST                                                    
         BE    LL20                                                             
         LA    R2,UNITGEN                                                       
         CLI   MODE,UNITLAST                                                    
         BE    LL20                                                             
         LA    R2,REQGEN                                                        
         CLI   MODE,REQLAST                                                     
         BNE   CTXIT                                                            
                                                                                
         MVI   SVAOCBF,C'N'                                                     
                                                                                
         CLI   RCSUBPRG,3                                                       
         BNE   LL20                                                             
         LA    R5,DLBUFF                                                        
         USING DLCBD,R5                                                         
         MVI   DLCBACT,DLCBEOR     END OF REPORT                                
         GOTO1 DOWNLOAD,(R5)                                                    
         B     LL20                                                             
         DROP  R5                                                               
                                                                                
LL20     CLI   RCSUBPRG,3                                                       
         BE    CTXIT                                                            
         BAS   RE,LEVLAST                                                       
                                                                                
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   CTXIT                                                            
         CLI   MODE,LEDGLAST                                                    
         BNE   CTXIT                                                            
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BNZ   *+12                                                             
         TM    ROPTST,ROPTDET                                                   
         BZ    CTXIT                                                            
                                                                                
         MVI   RCSUBPRG,4          YES -- DO OFFICE RECAP                       
         CLI   QOPT5,C'Y'                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,5          DON'T SHOW OPENING BALANCES                  
         MVI   FORCEHED,C'Y'                                                    
         XC    BUFFREC(BUFFRECQ),BUFFREC                                        
         MVI   BUFFLEVL,X'FE'      GET OFFICE TOTALS FOR THE LEDGER             
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFFREC,1                            
LL30     TM    DMCB+8,X'90'        ANYTHING TO PRINT?                           
         BNZ   CTXIT               NO                                           
         CLI   BUFFLEVL,X'FE'      STILL LOOKING AT OFFICE TOTALS?              
         BNE   LL40                                                             
                                                                                
         MVI   SPACING,2                                                        
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVC   PLKEY(2),BUFFOFFC   OFFICE CODE                                  
         MVC   PLDESC,BUFFOFFN     OFFICE NAME                                  
         LA    R3,BUFFACCS         ACCUMULATORS                                 
         BAS   RE,EDITOR                                                        
         BAS   RE,CTREPORT                                                      
         DROP  R4                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFFREC,1                             
         B     LL30                                                             
                                                                                
LL40     CLI   BUFFLEVL,X'FF'      LEDGER TOTAL RECORD?                         
         BNE   CTXIT               BUFFALO LEDGER TOTAL RECORD VANISHED         
                                                                                
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVC   PLSPACES,SPACES     CLEAR POSSIBLE GARBAGE                       
         MVC   PLTOT,=C'TOTALS FOR'                                             
         L     R3,ADLDGNAM                                                      
         USING NAMELD,R3                                                        
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLTOT+11(0),NAMEREC                                              
         DROP  R3                                                               
         LA    R3,BUFFACCS         ACCUMULATORS                                 
         BAS   RE,EDITOR                                                        
         BAS   RE,CTREPORT                                                      
         DROP  R4                                                               
                                                                                
CTXIT    XMOD1 1                                                                
XIT      XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              SUBROUTINES                                                      
*-------------------------------------------------------------------*           
*-------------------------------------------------------------------*           
*              STRUCTUR SETS UP A TABLE OF CONTROL INFORMATION USING            
*              THE LEDGER STRUCTURE AND THE PROFILE OPTIONS                     
*-------------------------------------------------------------------*           
STRUCTUR NTR1                                                                   
         LA    R6,LEVAGEN                                                       
         USING LEVGEND,R6                                                       
         LA    R1,4                                                             
         XC    LEVGEN,LEVGEN                                                    
         LA    R6,L'LEVGEN(R6)                                                  
         BCT   R1,*-10                                                          
         LA    R6,LEVAGEN                                                       
                                                                                
         XC    REQLOW,REQLOW                                                    
                                                                                
         L     R4,ADLDGHIR                                                      
         LA    R4,ACLVLEN-ACLELD(R4)                                            
         LA    R3,4                FOUR POSSIBLE LEVELS                         
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         ZIC   R5,=C'A'                                                         
         LA    RE,5                LEVEL A =ROW 5 OF ACCUMULATORS               
         IC    RF,QOPT2            NO OF LEVELS REQUIRED                        
         SLL   RF,28                                                            
         SRL   RF,28                                                            
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LA    RF,4                                                             
                                                                                
         CLI   PROGPROF,C'Y'       LEDGER TOTALS ONLY ?                         
         BNE   STRU20              NO                                           
         CLI   PROGPROF+1,C'Y'     SUPPRESS IF PROGPROF+1='Y'                   
         BNE   STRU10                                                           
         CLI   QLEDGER,C' '        AND REQUEST IS ONLY FOR A SINGLE             
         BNE   STRU20              LEDGER                                       
                                                                                
STRU10   SR    RF,RF               SET FOR LEDGER TOTALS ONLY                   
         MVI   REQLOW,C'L'                                                      
                                                                                
STRU20   EQU   *                   LOOP BUILDS TABLE                            
         IC    R2,0(R4)                                                         
         LTR   R2,R2                                                            
         BZ    STRU30              RUN OUT OF LEVELS                            
         STC   R1,LEVKDISP                                                      
         STC   R5,LEVLET                                                        
         STC   RF,LEVNUM                                                        
         SR    R2,R1                                                            
         STC   R2,LEVKLEN                                                       
         AR    R1,R2                                                            
         STC   RE,LEVACMNO                                                      
         MVC   LEVDESC,ACLVDESC-ACLVLEN(R4)                                     
         MVC   LEVNAME,SPACES                                                   
         MVC   LEVKEY,SPACES                                                    
                                                                                
         BCTR  RE,0                                                             
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         BCT   RF,*+8                                                           
         STC   R5,REQLOW                                                        
                                                                                
         LA    R5,1(R5)                                                         
         LA    R4,L'ACLVALS(R4)                                                 
         LA    R6,L'LEVGEN(R6)                                                  
         BCT   R3,STRU20                                                        
                                                                                
STRU30   BCTR  R5,0                                                             
         STC   R5,LOWLEV                                                        
         OC    REQLOW,REQLOW                                                    
         BNZ   *+10                                                             
         MVC   REQLOW,LOWLEV                                                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              LEVUP MAINTAINS THE VARIABLE FIELDS IN LEVAGEN ETC.              
*-------------------------------------------------------------------*           
LEVUP    NTR1                                                                   
         USING LEVGEND,R2                                                       
         USING NAMELD,R4                                                        
         USING ACTRECD,R6                                                       
                                                                                
         CLI   LEVNUM,0                                                         
         BE    XIT                                                              
         MVC   LEVNAME,SPACES                                                   
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EXMVC R1,LEVNAME,NAMEREC                                               
                                                                                
         LA    R5,LEVKEY                                                        
         MVC   LEVKEY,SPACES                                                    
         CLC   LEVLET,LOWLEV                                                    
         BE    LEV10                                                            
         BAS   RE,REKEY                                                         
         B     LEV30                                                            
LEV10    LA    R2,LEVAGEN                                                       
                                                                                
LEV20    BAS   RE,REKEY                                                         
         CLC   LEVLET,LOWLEV                                                    
         BE    LEV30                                                            
         ZIC   R1,LEVKLEN                                                       
         LA    R5,1(R1,R5)                                                      
         LA    R2,L'LEVGEN(R2)                                                  
         B     LEV20                                                            
                                                                                
LEV30    B     XIT                                                              
                                                                                
REKEY    ZIC   R1,LEVKDISP                                                      
         ZIC   R3,LEVKLEN                                                       
         BCTR  R3,0                                                             
         LA    R1,3(R1,R6)                                                      
         EXMVC R3,0(R5),0(R1)                                                   
         BR    RE                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              LEVLAST HANDLES PRINTING OF REPORT & RESETING OF                 
*              ACCUMULATORS                                                     
*-------------------------------------------------------------------*           
LEVLAST  NTR1                                                                   
         MVI   ACTV,C'N'                                                        
         USING LEVGEND,R2                                                       
         CLI   LEVNUM,0                                                         
         BE    LVLXIT                                                           
                                                                                
         ZIC   R3,LEVACMNO                                                      
                                                                                
         GOTO1 PROLLER,DMCB,1,ACCUMS,(R3)                                       
         L     R3,DMCB                                                          
                                                                                
*        RULES FOR THE PRINTING OF LINES                                        
                                                                                
         CLI   LEVLET,C'D'         HIGH LEVELS                                  
         BNH   LVL10                                                            
         CLI   LEVLET,C'R'                                                      
         BE    *+14                SKIP TEST IF REQLAST                         
         CLC   0(32,R3),=4PL8'0'                                                
         BE    LVLXIT                                                           
         CLC   LEVLET,REQLOW                                                    
         BE    LVL20                                                            
         CLC   LASTFIGS,0(R3)                                                   
         BE    LVLXIT                                                           
         MVC   LASTFIGS,0(R3)                                                   
         B     LVL20                                                            
                                                                                
LVL10    CLI   ACTIVE,C'Y'         HAVE WE HAD A LOW LEVEL ACCOUNT              
         BNE   LVLXIT                                                           
                                                                                
         CLC   LEVLET,LOWLEV       LOW LEVEL                                    
         BNE   LVL12                                                            
*                                                                               
LVL10A   CLI   QOPT3,C' '          CREDIT/DEBIT BALANCE OPTION                  
         BE    LVL11                                                            
         CLI   QOPT3,C'C'                                                       
         BNE   LVL10C                                                           
         CP    24(8,R3),=P'0'      FOR CREDIT OPTION                            
         BL    LVL11               IF BALANCE LT ZERO - OK                      
         B     LVLXIT                                                           
LVL10C   CP    24(8,R3),=P'0'      FOR DEBIT OPTION                             
         BNH   LVLXIT              IF BALANCE GT ZERO - OK                      
                                                                                
LVL11    CLC   LEVLET,LOWLEV       IF NOT LOW LEVEL CHECK FOR                   
         BNE   LVL11A              SUPPRESSION OF INACTIVES                     
         TM    ROPTST,ROPTCONT     DON'T DO THESE CHECKS IF NOT DOING           
         BZ    LVL11A              CONTRA DETAIL                                
         TM    ACTVSW,ACTOCON                                                   
         BZ    LVL11G                                                           
         B     LVL17               HAVE ACTIVITY SHOWN THAT TOTALS ZERO         
                                                                                
LVL11A   CLI   QOPT1,C'S'                                                       
         BNE   LVL17                                                            
LVL11G   CLC   0(32,R3),=4PL8'0'                                                
         BE    LVLXIT                                                           
         B     LVL17                                                            
                                                                                
LVL12    CLC   LEVLET,REQLOW       MIDDLE LEVELS                                
         BNE   LVL15                                                            
         CLC   0(32,R3),=4PL8'0'                                                
         BE    LVLXIT                                                           
         B     LVL17                                                            
                                                                                
LVL15    CLI   LEVLET,C'A'                                                      
         BL    LVL20                                                            
         CLI   LEVLET,C'D'                                                      
         BH    LVL16                                                            
         ZIC   RF,LEVLET                                                        
         BCTR  RF,0                                                             
         SLL   RF,28                                                            
         SRL   RF,26                                                            
         EX    R0,EXLVL(RF)                                                     
         LH    RF,0(R1)                                                         
         AH    RF,=H'1'                                                         
         STH   RF,0(R1)                                                         
                                                                                
LVL16    ZIC   RF,LEVLET           CHECK LEVEL COUNT                            
         CLI   LEVLET,C'C'                                                      
         BH    LVL20                                                            
         SLL   RF,28                                                            
         SRL   RF,26                                                            
         EX    R0,EXLVL(RF)                                                     
         CLC   0(2,R1),=H'1'       IF MORE THAN ONE AT THIS LEV-1               
         BH    LVL20               THEN PRINT A TOTAL LINE                      
         BL    LVL16A                                                           
                                                                                
         SRL   RF,1                IF EXACTLY ONE LINE AT THE                   
         LA    R5,LOWACTV(RF)      LEVEL BENEATH THIS AND THERE WERE            
         LA    RF,LEVALOW(RF)      MORE LOW LEV ACCOUNTS IN THIS LEVEL          
         SH    RF,=H'2'            THAN IN THAT LINE THEN DO A                  
         CLC   0(2,RF),0(R5)       TOTAL LINE                                   
         BH    LVL20                                                            
         B     LVL16B                                                           
                                                                                
LVL16A   SRL   RF,1                                                             
         LA    RF,LEVALOW(RF)                                                   
         SH    RF,=H'2'            IF NONE AT ALL AT THIS LEVEL-1               
         CLC   0(2,RF),=H'1'       BUT MORE THAN ONE LOW LEVEL LINE             
         BH    LVL20               THEN PRINT TOTAL LINE                        
                                                                                
LVL16B   SH    R1,=H'2'            IF 1 TOTAL AT THIS LEVEL-1                   
         LH    RF,0(R1)            THEN TAKE ONE FROM THIS LEV                  
         BCTR  RF,0                                                             
         STH   RF,0(R1)                                                         
         CLI   SPACESW,C' '        ONLY 1 LINE FOR LEVEL(S).                    
         BE    LVLXIT              LINE SKIPPED ALREADY, NO MORE.               
         CLI   LASTSPAC,1          IF LAST LINE WAS NOT SKIPPED                 
         BNE   LVLXIT              AND WAS NOT A TOTAL LINE,                    
         MVC   P+1(109),SPACES     SKIP A LINE HERE.                            
         CLI   FORCEHED,C'Y'                                                    
         BE    LVLXIT              DON'T SKIP IF WE'RE AT TOP OF PAGE           
         BAS   RE,CTREPORT                                                      
         B     LVLXIT                                                           
                                                                                
LVL17    ZIC   RF,LEVLET           UPDATE LEVEL COUNT                           
         BCTR  RF,0                                                             
         SLL   RF,28                                                            
         SRL   RF,26                                                            
         EX    R0,EXLVL(RF)                                                     
         LH    RF,0(R1)                                                         
         AH    RF,=H'1'                                                         
         STH   RF,0(R1)                                                         
                                                                                
*        NOW WE PRINT THE LINE                                                  
                                                                                
LVL20    LA    R4,P                                                             
         MVI   ACTV,C'Y'                                                        
         USING PLINED,R4                                                        
         CLC   LEVLET,LOWLEV                                                    
         BNE   LVL40                                                            
         LA    R5,4                                                             
         LA    R1,LEVALOW                                                       
LVL22    LH    RF,0(R1)            BUMP COUNT OF LOW LEV. ACCOUNTS              
         LA    RF,1(RF)            WITHIN EACH HIERARCHICAL LEVEL               
         STH   RF,0(R1)                                                         
         LA    R1,2(R1)                                                         
         BCT   R5,LVL22                                                         
                                                                                
         CLI   FIRSTLOW,C'Y'                                                    
         BNE   LVL25                                                            
         MVI   FIRSTLOW,C'N'                                                    
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   *+12                NOT ON NEW FILE/NEW OFFICES                  
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BZ    LVL25                                                            
         CLI   LASTSPAC,1                                                       
         BNE   LVL25                                                            
         BAS   RE,CTREPORT                                                      
                                                                                
LVL25    MVC   PLKEY,LEVKEY                                                     
         MVC   PLDESC,LEVNAME                                                   
         MVI   SPACING,1                                                        
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BNZ   LVL60                                                            
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   LVL60               NOT ON NEW FILE/NEW OFFICES                  
         TM    ROPTST,ROPTDET                                                   
         BZ    LVL60                                                            
         MVI   OFFCACTV,C'Y'       WE'VE PRINTED FOR AN OFFICE                  
         CLC   SAVEP,SPACES                                                     
         BE    *+14                                                             
         MVC   P,SAVEP                                                          
         BAS   RE,CTREPORT                                                      
         MVC   PLKEY,SPACES        CLEAR OUT KEY FIELD                          
         MVC   PLDESC,SPACES       AND NAME                                     
         MVC   PLDESC(7),=C'*TOTAL*'                                            
         B     LVL60                                                            
                                                                                
LVL40    CLI   LASTSPAC,1                                                       
         BNE   LVL45                                                            
         CLI   SPACESW,C' '        IF WE'VE JUST SKIPPED A LINE,                
         BE    LVL45               DON'T SKIP ANOTHER ONE.                      
         CLI   OFFCACTV,C'Y'                                                    
         BNE   LVL45                                                            
         BAS   RE,CTREPORT         NEED A BLANK LINE                            
LVL45    MVI   SPACING,2                                                        
                                                                                
         MVC   PLSPACES,SPACES     CLEAR POSSIBLE GARBAGE                       
         MVC   PLTOT,=C'TOTALS FOR'                                             
         CLI   LEVLET,C'D'                                                      
         BNH   LVL50                                                            
                                                                                
         MVC   PLDESC,LEVNAME      REQUEST,UNIT,LEDGER TOTALS                   
         B     LVL60                                                            
                                                                                
LVL50    MVI   MYWORK,C' '                                                      
         MVC   MYWORK+1(L'MYWORK-1),MYWORK                                      
         LA    R1,MYWORK                                                        
         MVC   0(L'LEVDESC,R1),LEVDESC                                          
         LA    R1,L'LEVDESC+1(R1)                                               
         MVC   0(L'LEVNAME,R1),LEVNAME                                          
         LA    R1,L'LEVNAME+1(R1)                                               
         MVI   0(R1),C'('                                                       
         LA    R1,1(R1)                                                         
         MVC   0(L'LEVKEY,R1),LEVKEY                                            
         LA    R5,L'MYWORK                                                      
         GOTO1 ADSQUASH,DMCB,MYWORK,(R5)                                        
         L     R5,DMCB+4                                                        
         LA    R5,MYWORK(R5)                                                    
         MVI   0(R5),C')'                                                       
                                                                                
         ST    R3,MYFULL           SAVE A(ACCUMS)                               
         L     R0,DMCB+4                                                        
         AH    R0,=H'1'                                                         
         STC   R0,MYFULL           SAVE L'STRING                                
         L     RE,AMONACC                                                       
         TM    ACMINDS-ACMD(RE),ACMIEMUD+ACMINEWO                               
         BNO   LVL58                                                            
         CLI   QOPT2,C' '          SUPPRESS DETAILS?                            
         BE    LVL58                                                            
         CLC   LEVLET,REQLOW                                                    
         BNE   LVL58                                                            
                                                                                
         LA    R5,L'PLDESC+L'PLTOT+1                                            
         GOTO1 CHOPPER,DMCB,((R0),MYWORK),((R5),PLTOT),(C'P',2)                 
         BAS   RE,CTREPORT         PRINT CATEGORY NAME                          
                                                                                
         XC    BUFFREC(BUFFRECQ),BUFFREC                                        
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFFREC,1                            
LVL55    TM    DMCB+8,X'90'        ANYTHING TO PRINT?                           
         BNZ   LVL57               NO                                           
         CLI   BUFFLEVL,0          HAVE WE HIT THE LEDGER TOTALS YET?           
         BNE   LVL57                                                            
                                                                                
         MVI   SPACING,1                                                        
         MVC   PLOFFCD,BUFFOFFC    OFFICE CODE                                  
         MVC   PLOFFNM,BUFFOFFN    OFFICE NAME                                  
         LA    R3,BUFFACCS         ACCUMULATORS                                 
         BAS   RE,EDITOR                                                        
         BAS   RE,CTREPORT                                                      
         MP    BUFFOB,=P'-1'       CLEAR ALL TOTALS BY ADDING INVERSE           
         MP    BUFFOD,=P'-1'                                                    
         MP    BUFFOC,=P'-1'                                                    
         MP    BUFFMY,=P'-1'                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC,1                             
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFFREC,1                             
         B     LVL55                                                            
                                                                                
LVL57    MVC   PLSPACES,SPACES     CLEAR POSSIBLE GARBAGE                       
         MVC   PLTOT,=C'TOTALS FOR'                                             
LVL58    L     R0,MYFULL                                                        
         LR    R3,R0               RESTORE A(ACCUMS)                            
         SRL   R0,24               RESTORE LENGTH                               
         LA    R5,L'PLDESC                                                      
         GOTO1 CHOPPER,DMCB,((R0),MYWORK),((R5),PLDESC),(C'P',2)                
                                                                                
LVL60    MVC   LASTSPAC,SPACING                                                 
                                                                                
         BAS   RE,EDITOR                                                        
                                                                                
         TM    ROPTST,ROPTCONT                                                  
         BZ    LVL61                                                            
         USING PLINED,R4                                                        
LVL60A   LA    R4,P                                                             
         MVC   PLKEY,SPACES                                                     
         MVC   PLTOT,=C'TOTALS FOR'                                             
         BAS   RE,CTREPORT                                                      
                                                                                
LVL61    BAS   RE,CTREPORT                                                      
                                                                                
LVLXIT   ZIC   R3,LEVACMNO         CLEAR 1ST N ROWS OF ACCUMULATORS             
LVLX10   GOTO1 PROLLER,DMCB,2,ACCUMS,(R3)                                       
         BCT   R3,LVLX10                                                        
                                                                                
         CLI   LEVLET,C'D'                                                      
         BH    LVLX20                                                           
         CLI   ACTV,C'Y'                                                        
         BNE   LVLX15                                                           
         ZIC   RF,LEVLET                                                        
         SLL   RF,28                                                            
         SRL   RF,27                                                            
         SH    RF,=H'2'                                                         
         LA    R1,LEVALOW(RF)      SAVE THE NUMBER OF LOW LEVEL ACCS.           
         LA    RF,LOWACTV(RF)      IN THIS TOTAL LINE                           
         MVC   0(2,RF),0(R1)                                                    
         CLC   LEVLET,LOWLEV                                                    
         BE    LVLX20                                                           
LVLX15   ZIC   RF,LEVLET                                                        
         SLL   RF,28                                                            
         SRL   RF,26                                                            
         EX    R0,EXLVL(RF)                                                     
         XC    0(L'LEVACONT,R1),0(R1)                                           
                                                                                
LVLX20   CLI   LEVLET,C'A'                                                      
         BNE   *+8                                                              
         MVI   ACTIVE,C'N'                                                      
                                                                                
         B     XIT                                                              
                                                                                
EXLVL    LA    R1,LEVACONT                                                      
         LA    R1,LEVBCONT                                                      
         LA    R1,LEVCCONT                                                      
         LA    R1,LEVDCONT                                                      
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              EDITOR EDITS FIGURES ONTO PLINE                                  
*-------------------------------------------------------------------*           
EDITOR   NTR1                                                                   
         USING PLINED,R4                                                        
         LA    RE,PLFIGS                                                        
         LA    RF,4                                                             
                                                                                
ED10     MVC   12(3,RE),=C'NIL'                                                 
         CP    0(8,R3),=P'0'       R3 PTS TO LINE OF ACCUMLATORS                
         BE    ED20                                                             
         MVC   12(3,RE),SPACES                                                  
         MVC   WORK(L'EDMASK),EDMASK                                            
         ED    WORK(L'EDMASK),1(R3)                                             
         MVC   0(16,RE),WORK+1                                                  
                                                                                
         CH    RF,=H'2'                                                         
         BNE   ED15                                                             
         CLC   14(2,RE),=C'CR'                                                  
         BNE   ED20                                                             
         MVC   14(2,RE),=C'+ '                                                  
         B     ED20                                                             
ED15     CH    RF,=H'3'                                                         
         BNE   ED20                                                             
         CLC   14(2,RE),=C'CR'                                                  
         BNE   ED20                                                             
         MVC   14(2,RE),=C'- '                                                  
                                                                                
ED20     LA    RE,L'PLFIGS(RE)                                                  
         LA    R3,8(R3)                                                         
         BCT   RF,ED10                                                          
                                                                                
         B     XIT                                                              
                                                                                
EDMASK   DC   X'4020202020202020202020214B2020C3D9'                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              CTREPORT                                                         
*-------------------------------------------------------------------*           
CTREPORT NTR1                                                                   
         CLI   RCSUBPRG,3          TEST DOWNLOADING                             
         BNE   CTREP3                                                           
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         B     XIT                                                              
                                                                                
CTREP3   MVC   SPACESW,P+17                                                     
         CLI   RCSUBPRG,4          NOT DURING OFFICE RECAP                      
         BE    CTREP10                                                          
         CLI   RCSUBPRG,5                                                       
         BE    CTREP10                                                          
         TM    ROPTST,ROPTDET                                                   
         BZ    *+14                                                             
         MVC   HEAD4+47(18),=C'WITH OFFICE DETAIL'                              
         B     CTREP10                                                          
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BZ    CTREP10                                                          
         MVC   HEAD4+51(9),=C'BY OFFICE'                                        
                                                                                
CTREP10  MVC   HEAD5+85(L'HEADMON),HEADMON                                      
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BZ    *+16                                                             
         MVC   HEAD6+85(2),OFFCODE                                              
         MVC   HEAD6+88(21),OFFNAME  ONLY ROOM FOR 21 CHARACTERS                
                                                                                
         CLI   QOPT5,C'Y'                                                       
         BNE   CTREPX                                                           
                                                                                
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         CLC   PLFIGS+12(3),=C'NIL'                                             
         BNE   CTREPX                DON'T PRINT NIL IF                         
         MVC   PLFIGS+12(3),SPACES   SUPPRESSING OPENING BALANCE                
CTREPX   GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PRINT  ROUTINE FOR DOWNLOADING                                         
*-------------------------------------------------------------------*           
DLPRINT  NTR1                                                                   
         MVC   P,DLPLINE                                                        
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVI   SPACING,1                                                        
         MVI   FORCEHED,C'N'                                                    
         GOTO1 ACREPORT                                                         
         MVC   DLPLINE,SPACES                                                   
         MVI   LINE,1                                                           
         XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PRODUCE REPORT IN DOWN-LOADABLE FORMAT                                 
*-------------------------------------------------------------------*           
CTDOWN   NTR1                                                                   
         CLI   DLEVSW,1                                                         
         BNE   CTD10                                                            
         ZAP   DLDUB,BCASHA                                                     
         B     CTD40                                                            
CTD10    CLI   DLEVSW,2                                                         
         BNE   CTD20                                                            
         ZAP   DLDUB,BCASHB                                                     
         B     CTD40                                                            
CTD20    CLI   DLEVSW,3                                                         
         BNE   CTD30                                                            
         ZAP   DLDUB,BCASHC                                                     
         B     CTD40                                                            
CTD30    ZAP   DLDUB,DUB                                                        
CTD40    CLI   PROGPROF+2,C'Y'                                                  
         BNE   CTD50                                                            
         CLI   DLEVSW,4            DON'T TEST FOR INACTIVE ACCOUNTS             
         BNE   CTD50               UNLESS WE'RE AT ACC LEVEL                    
         CLC   BCASH(24),=3PL8'0'  IN US SUPPRESS INACTIVE ACCOUNTS             
         BE    CTXIT                                                            
CTD50    CLI   QOPT3,C' '                                                       
         BE    CTD60                                                            
         CLI   QOPT3,C'C'          CREDIT BAL IS LESS THAN ZERO                 
         BNE   CTD55                                                            
         CP    DLDUB,=P'0'                                                      
         BNL   CTXIT                                                            
CTD55    CP    DLDUB,=P'0'         DEBIT BAL MORE THAN ZERO                     
         BNH   CTXIT                                                            
CTD60    CLI   DLEVSW,4            DON'T TEST FOR INACTIVE ACCOUNTS             
         BNE   CTD70               UNLESS WE'RE AT ACC LEVEL                    
         CLI   QOPT1,C'S'          SUPPRESS INACTIVE ACCOUNTS?                  
         BNE   CTD70                                                            
         CLC   BCASH(24),=3PL8'0'                                               
         BE    CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
CTD70    LA    R5,DLBUFF                                                        
         USING DLCBD,R5                                                         
         CLI   DLSTAT,0            FIRST TIME THROUGH                           
         BNE   CTD80                                                            
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BZ    CTD73                                                            
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(6),=C'OFFICE'                                            
         GOTO1 DOWNLOAD,(R5)                                                    
                                                                                
CTD73    MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(12),=C'ACCOUNT CODE'                                     
         GOTO1 DOWNLOAD,(R5)                                                    
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(12),=C'ACCOUNT NAME'                                     
         GOTO1 DOWNLOAD,(R5)                                                    
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(7),=C'BALANCE'                                           
         GOTO1 DOWNLOAD,(R5)                                                    
         MVI   DLCBACT,DLCBEOL                                                  
         GOTO1 DOWNLOAD,(R5)                                                    
         MVI   DLSTAT,1                                                         
                                                                                
CTD80    CLI   DLEVSW,4            IF IT'S NOT ACC LEVEL                        
         BNE   CTD90               DON'T ADD IN HIGHER LEV TOTALS               
         ZAP   BCASH,DUB           ACCOUNT BALANCE                              
         AP    BCASHC,DUB          LEV C BALANCE                                
         AP    BCASHB,DUB          LEV B BALANCE                                
         AP    BCASHA,DUB          LEV A BALANCE                                
CTD90    CLI   QOPT2,C' '          DON'T TEST FOR ACC LEVEL IF                  
         BNE   CTD100              WE'VE GOT LEVEL OPTION SET                   
         CLI   PROGPROF+4,C'N'     PROFILE TO DOWNLOAD ONLY ACC                 
         BE    CTD100              LEVEL IF QOPT2=C' '                          
         CLI   DLEVSW,4            ARE WE AT ACC LEVEL?                         
         BE    CTD430              YES - PRINT ACCOUNT BALANCE                  
         CLI   DLEVSW,3                                                         
         BE    CTD350                                                           
         CLI   DLEVSW,2                                                         
         BE    CTD250                                                           
         B     CTD150                                                           
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
CTD100   CLI   DLEVSW,1            LEVEL A?                                     
         BNE   CTD200                                                           
         L     R6,ADHEIRA          A(LEVEL A CODE)                              
         L     R7,ADLVANAM         A(LEVEL A NAME)                              
         BAS   RE,CTD500                                                        
CTD150   ZAP   BCASHA,=P'0'                                                     
         B     CTXIT                                                            
                                                                                
CTD200   CLI   DLEVSW,2            LEVEL B?                                     
         BNE   CTD300                                                           
         CLI   QOPT2,C' '          IF NO QOPT2 (AND PROGPROF+4=N)               
         BE    *+12                                                             
         CLI   QOPT2,C'1'          OR QOPT2>1, SHOW THIS BALANCE                
         BNH   CTXIT                                                            
         L     R6,ADHEIRB          A(LEVEL B CODE)                              
         L     R7,ADLVBNAM         A(LEVEL B NAME)                              
         BAS   RE,CTD500                                                        
CTD250   ZAP   BCASHB,=P'0'                                                     
         B     CTXIT                                                            
                                                                                
CTD300   CLI   DLEVSW,3            LEVEL C?                                     
         BNE   CTD400                                                           
         CLI   QOPT2,C' '          IF NO QOPT2 (AND PROGPROF+4=N)               
         BE    *+12                                                             
         CLI   QOPT2,C'2'          OR QOPT2>2, SHOW THIS BALANCE                
         BNH   CTXIT                                                            
         L     R6,ADHEIRC          A(LEVEL C CODE)                              
         L     R7,ADLVCNAM         A(LEVEL C NAME)                              
         BAS   RE,CTD500                                                        
CTD350   ZAP   BCASHC,=P'0'                                                     
         B     CTXIT                                                            
                                                                                
CTD400   CLI   QOPT2,C' '          IF NO QOPT2 (AND PROGPROF+4=N)               
         BE    CTD430              SHOW THIS BALANCE                            
         L     R4,ADLDGHIR                                                      
         USING ACLELD,R4                                                        
         LA    R6,ACLVALS                                                       
         CLI   0(R6),0                                                          
*        CLI   ACHRLEVB,0          ACCLAST EQUIVALENT TO LEVALAST?              
         BE    CTD430              THEN SHOW THIS BALANCE                       
         LA    R6,L'ACLVALS(R6)                                                 
*        CLI   ACHRLEVC,0          ACCLAST EQUIVALENT TO LEVBLAST?              
         BNE   CTD415                                                           
         CLI   QOPT2,C'1'          AND QOPT2>1?                                 
         BH    CTD430              THEN SHOW THIS BALANCE                       
         B     CTXIT                                                            
CTD415   DS    0H                                                               
         LA    R6,L'ACLVALS(R6)                                                 
*        CLI   ACHRLEVD,0          ACCLAST EQUIVALENT TO LEVCLAST?              
         BNE   CTXIT                                                            
         CLI   QOPT2,C'2'          AND QOPT2>2?                                 
         BH    CTD430              THEN SHOW THIS BALANCE                       
         B     CTXIT                                                            
                                                                                
CTD430   L     R6,ADACC            A(ACCOUNT CODE)                              
         L     R7,ADACCNAM         A(ACCOUNT NAME)                              
         BAS   RE,CTD500                                                        
         B     CTXIT                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CODE, NAME AND BALANCE INTO PRINT LINE                                 
*-------------------------------------------------------------------*           
CTD500   ST    RE,SAVEREG                                                       
         TM    ROPTST,ROPTOFF+ROPTOCON                                          
         BZ    CTD503                                                           
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(2),OFFCODE  OFFICE CODE                                  
         GOTO1 DOWNLOAD,(R5)                                                    
                                                                                
CTD503   MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(14),1(R6)   CODE                                         
         GOTO1 DOWNLOAD,(R5)                                                    
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         ZIC   RE,1(R7)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8              NAME                                         
         B     *+10                                                             
         MVC   DLCBFLD(0),NAMEREC-NAMELD(R7)                                    
         GOTO1 DOWNLOAD,(R5)                                                    
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBNUM                                                  
         CURED (P8,DLDUB),(16,DLCBFLD),2,MINUS=YES                              
         GOTO1 DOWNLOAD,(R5)                                                    
         MVI   DLCBACT,DLCBEOL     E-O-LINE                                     
         GOTO1 DOWNLOAD,(R5)                                                    
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              LTORG                                                            
*-------------------------------------------------------------------*           
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              CONSTANTS                                                        
*-------------------------------------------------------------------*           
RELOTAB  DS    0F                                                               
         DC    V(DLFLD)                                                         
         DC    A(BUFFALOC)                                                      
         DC    X'FF'                                                            
                                                                                
         BUFF  LINES=600,ROWS=1,COMMENT=36,COLUMNS=4,FLAVOR=PACKED,    +        
               KEYLIST=(3,A)                                                    
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONTRA ACCOUNT BUFFER                                                  
*-------------------------------------------------------------------*           
MXCON    EQU   1000                                                             
         DS    0D                                                               
         DC    CL8'**CONTAB*'                                                   
CCOUNT   DC    F'0'                NUMBER IN TABLE                              
CMAX     DC    AL4(MXCON)          MAX. IN TABLE                                
CACCT    DC    CL14' '             ACCOUNT                                      
CONTAB   DS    (MXCON*CONLNQ)C     TABLE                                        
                                                                                
CONTABD  DSECT                                                                  
CONKEY   DS    0CL14                                                            
CONCON   DS    CL14                CONTRA ACCOUNT                               
CONBK    DS    0XL(3*8)                                                         
CONBAL   DS    PL8                 BALANCE                                      
CONDR    DS    PL8                 DEBITS                                       
CONCR    DS    PL8                 CREDITS                                      
CONBKN   EQU   (*-CONBK)/8                                                      
CONLNQ   EQU   *-CONTABD                                                        
*                                                                               
*-------------------------------------------------------------------*           
*        DSECTS                                                                 
*-------------------------------------------------------------------*           
AC8102D  DSECT                                                                  
ATYPES   EQU   *                                                                
DOWNLOAD DS    A                                                                
ABUFF    DS    A                                                                
RELO     DS    F                                                                
MYFULL   DS    F                                                                
SAVEREG  DS    F                                                                
LEVACONT DS    H                                                                
LEVBCONT DS    H                                                                
LEVCCONT DS    H                                                                
LEVDCONT DS    H                                                                
                                                                                
HEADMON  DS    CL30                                                             
TODAY2   DS    CL2                 COMPRESSED                                   
TODAY3   DS    CL3                 BINARY                                       
                                                                                
ROPTST   DS    CL1                 RUN OPTIONS                                  
ROPTDET  EQU   X'80'               DETAIL SEQ                                   
ROPTACC  EQU   X'40'               ACCOUNT SEQ                                  
ROPTOFF  EQU   X'20'               OFFICE SEQ                                   
ROPTACON EQU   X'10'               ACCOUNT SEQ/CONTRA DETAIL                    
ROPTOCON EQU   X'08'               OFFICE SEQ/CONTRA DETAIL                     
ROPTCONT EQU   X'04'               ANY CONTRA SEQUENCE                          
                                                                                
ACTVSW   DS    CL1                                                              
ACTACON  EQU   X'80'                                                            
ACTOCON  EQU   X'40'                                                            
                                                                                
SVACMSTR DS    CL(L'ACMMSTR)       SAVE START DATE FROM REQUEST CARD            
SVACMEND DS    CL(L'ACMMEND)       SAVE END DATE FROM REQEUST CARD              
SVAOCBF  DS    CL(L'ACMOCBF)       SAVE OFF/CON BUCKET SWITCH                   
                                                                                
RACCUMS  DS    0C                  ACCUMULATORS                                 
BCASH    DS    PL8                 ACCOUNT BALANCE                              
RACMLEN  EQU   *-RACCUMS                                                        
DCASH    DS    PL8                 DEBIT                                        
CCASH    DS    PL8                 CREDIT                                       
MYBAL    DS    PL8                 MANUAL BALANCE                               
                                                                                
OBCASH   DS    PL8                 OFFICE BALANCE                               
ODCASH   DS    PL8                 OFFICE DEBITS                                
OCCASH   DS    PL8                 OFFICE CREDITS                               
MYOBAL   DS    PL8                 OFFICE MANUAL BALANCE                        
                                                                                
CBCASH   DS    PL8                 CONTRA BALANCE                               
CDCASH   DS    PL8                 CONTRA DEBITS                                
CCCASH   DS    PL8                 CONTRA CREDITS                               
MYCBAL   DS    PL8                 CONTRA MANUAL BALANCE                        
                                                                                
CBCASHC  DS    PL8                 CUMULATIVE CONTRA BALANCE                    
CDCASHC  DS    PL8                 CUMULATIVE CONTRA DEBITS                     
CCCASHC  DS    PL8                 CUMULATIVE CONTRA CREDITS                    
MYCBALC  DS    PL8                 CUMULATIVE CONTRA MANUAL BALANCE             
RACMNUM  EQU   (*-RACCUMS)/RACMLEN                                              
                                                                                
OFFCODE  DS    CL2                 OFFICE CODE                                  
OFFNAME  DS    CL36                OFFICE NAME                                  
                                                                                
REQGEN   DS    CL71                )                                            
UNITGEN  DS    CL71                )                                            
LEDGEN   DS    CL71                )                                            
LEVAGEN  DS    CL71                )-LEVGEND RECORDS                            
LEVBGEN  DS    CL71                )                                            
LEVCGEN  DS    CL71                )                                            
LEVDGEN  DS    CL71                )                                            
                                                                                
LASTSPAC DS    CL1                 LAST 'SPACING' USED                          
LOWLEV   DS    CL1                 LETTER OF LOWEST LEVEL THIS LEDGER           
REQLOW   DS    CL1                 LETTER OF LOWEST REQUESTED LEVEL             
FIRSTLOW DS    CL1                                                              
ACTIVE   DS    CL1                                                              
SPACESW  DS    CL1                                                              
                                                                                
LASTFIGS DS    0CL32               SAVE LAST FIGURES PRINTED                    
         DS    4PL8                                                             
ACCUMS   DS    CL8,(8*5)PL8        ACCUMULATOR SPACE FOR PROLLER                
                                                                                
MYWORK   DS    CL200                                                            
LEVALOW  DS    H                   COUNT OF LOW LEVEL ACCOUNTS                  
LEVBLOW  DS    H                   WITHIN EACH LEVEL                            
LEVCLOW  DS    H                                                                
LEVDLOW  DS    H                                                                
LOWACTV  DS    4H                  # OF LOW LEV ACCS IN LAST TOTAL              
ACTV     DS    C                                                                
OFFCACTV DS    C                   'Y' IF OFFICE HAD SOME ACTIVITY              
                                                                                
SAVEP    DS    0CL132              DETAIL USED TO USE THIS                      
SVPACC   DS    CL132               ACC/CON USES THIS                            
SVPSBAC  DS    CL132               ACC/CON USES THIS                            
                                                                                
DLPLINE  DS    CL132               DOWN-LOAD PRINT-LINE                         
DLBUFF   DS    CL80                BUFFER FOR DLFLD                             
DLSTAT   DS    CL1                 STATUS..0 IS FIRST TIME                      
DLEVSW   DS    CL1                 LEVEL FOR DOWNLOAD                           
BCASHA   DS    PL8                 DOWNLOAD LEVA BALANCE                        
BCASHB   DS    PL8                 DOWNLOAD LEVB BALANCE                        
BCASHC   DS    PL8                 DOWNLOAD LEVC BALANCE                        
DLDUB    DS    PL8                 DOWNLOAD ANY LEV BALANCE                     
                                                                                
BUFFREC  DS    0D                  BUFFALO RECORD                               
BUFFLEVL DS    X                   SORT LEVEL                                   
*                                   X'00' = LOW LEVEL OFFICE TOTALS             
*                                   X'FE' = OFFICE TOTALS FOR LEDGER            
*                                   X'FF' = LEDGER TOTALS ACROSS OFFICE         
BUFFOFFC DS    CL2                 OFFICE CODE                                  
BUFFOFFN DS    CL36                OFFICE NAME                                  
BUFFACCS DS    0XL32               ACCUMULATORS                                 
BUFFOB   DS    PL8                 OFFICE BALANCE                               
BUFFOD   DS    PL8                 OFFICE DEBITS                                
BUFFOC   DS    PL8                 OFFICE CREDITS                               
BUFFMY   DS    PL8                 OFFICE MANUAL BALANCE                        
BUFFRECQ EQU   *-BUFFREC                                                        
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              PLINED,DSECT FOR PRINT LINE                                      
*-------------------------------------------------------------------*           
PLINED   DSECT                                                                  
PLINE    DS    0CL110                                                           
         DS    CL1                                                              
PLKEY    DS    CL15                KEY                                          
         DS    CL1                                                              
PLDESC   DS    CL36                DESCRIPTION                                  
         ORG   PLDESC                                                           
         DS    CL1                                                              
PLOFFCD  DS    CL2                 OFFICE CODE                                  
         DS    CL1                                                              
PLOFFNM  DS    CL36                OFFICE NAME                                  
         ORG                                                                    
         ORG   PLDESC+28                                                        
PLFIGS   DS    4CL16                                                            
                                                                                
         ORG   PLINE                                                            
         DS    CL1                                                              
PLSPACES DS    CL5                 SPACES WHEN 'TOTALS FOR' FOLLOWS             
PLTOT    DS    CL10                USUALLY SET TO 'TOTALS FOR'                  
                                                                                
*-------------------------------------------------------------------*           
*        LEVGEND,DSECT FOR LEVAGEN ETC. CONTROLS SUBROUTINES SUCH               
*        AS LEVUP & LEVLAST.                                                    
*-------------------------------------------------------------------*           
                                                                                
LEVGEND  DSECT                                                                  
LEVGEN   DS    0CL71                                                            
LEVLET   DS    CL1                 'A','B',ETC.                                 
LEVNUM   DS    CL1                 IF ZERO SHOWS LEVEL NOT WANTED               
LEVACMNO DS    CL1                 ROW NO FOR PROLLER TABLE                     
LEVKDISP DS    CL1                 DISPLACEMENT THIS LEVELS PART OF KEY         
LEVKLEN  DS    CL1                 LENGTH OF THIS LEVELS PART OF KEY            
LEVKEY   DS    CL15                              CHANGED BY LEVUP               
LEVDESC  DS    CL15                'CLIENT','PRODUCT',ETC.                      
LEVNAME  DS    CL36                'RUMBLES LTD' CHANGED BY LEVUP               
         EJECT                                                                  
                                                                                
                                                                                
*        INCLUDE ACMASTD                                                        
*        INCLUDE ACGENFILE                                                      
*        INCLUDE ACGENMODES                                                     
*        INCLUDE ACREPWORKD                                                     
*        INCLUDE DDDLCB                                                         
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACREP8102 05/22/03'                                      
         END                                                                    
