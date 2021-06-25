*          DATA SET ACINQ01    AT LEVEL 012 AS OF 05/01/02                      
*PHASE T60601A,*,NOAUTO                                                         
         TITLE 'ACCOUNT ENQUIRY MK2 - DDS STATEMENT - T60601'                   
T60601   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR TYPE 'DS' IN ACCOUNT ENQUIRY PROGRAM                  
*              THIS TYPE IS IDENTICAL TO JOB STATEMENT EXCEPT THAT IT           
*              IS NOT RESTRICTED TO THE PRODUCTION LEDGER                       
*              UNLIKE STATEMENT (ST) IT DOES NOT USE THE BALANCE                
*              ELEMENT FOR ACCOUNT TOTALS                                       
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60601)                                               
         DC    A(FILTABLE-T60601)                                               
         DC    A(KNTRYPNT-T60601)                                               
         DC    A(FNTRYPNT-T60601)                                               
         DC    A(DNTRYPNT-T60601)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS+1)                                              
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS+2)                                              
         DC    AL2(EDITACC-GWS)                                                 
*                                                                               
         DC    CL10'THE REST'                                                   
         DC    C' '                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(15)                                                          
         DC    AL1(27)                                                          
         DC    AL2(SPACES-GWS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    X'00'               END OF FILTER TABLE                          
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
*              HANDLE CONTRA-ACCOUNT FILTER                                     
*              (CALLED BY FILTER ROUTINE IN ROOT)                               
         SPACE 1                                                                
FNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ1**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60601,RB                                                        
         USING T606TWA,RA                                                       
         LA    R2,WORK             SET UP DUMMY COMPARANDS                      
         LA    R5,INFCAC                                                        
         BCTR  R3,0                R3 = LENGTH MINUS 1                          
         EX    R3,VCONTMV1         EQUATE COMPARANDS                            
         LA    R4,1                R4 = DISPLACEMENT INTO CA/C                  
         L     R6,AIO                                                           
         USING ACKEYD,R6                                                        
         CLC   ACKEYCON(3),SPACES                                               
         BNE   *+8                                                              
         LA    R4,3                                                             
         LA    R7,ACKEYCON(R4)                                                  
         EX    R3,VCONTCLC                                                      
         BL    VCONTNO                                                          
         SPACE 1                                                                
         CLC   SAVECACN,SPACES     SAVE NAME IF AVAILABLE                       
         BNE   VCONTX                                                           
         LA    R9,L'ACKEYCON                                                    
         SR    R9,R4                                                            
         BCTR  R9,0                                                             
         LA    R7,ACKEYCON(R4)                                                  
         EX    R9,VCONTCLC         DOES THE CA/C READ MATCH THE FILTER          
         BNE   VCONTX              IN FULL                                      
         CLI   ACRECORD,X'43'      IF SO IS THIS A CA/C HEADER                  
         BNE   VCONTX                                                           
         LA    R7,ACRECORD         IF SO SAVE THE NAME                          
         USING TRSUBHD,R7                                                       
         IC    R4,TRSBLEN                                                       
         SH    R4,=H'18'                                                        
         EX    R4,VCONTMV4                                                      
         B     VCONTX                                                           
         SPACE 1                                                                
VCONTNO  MVI   WORK,0              UNEQUATE COMPARANDS                          
VCONTX   LA    R3,1(R3)                                                         
         XIT1  REGS=(R2,R5)                                                     
         SPACE 1                                                                
VCONTMV1 MVC   WORK(0),INFCAC                                                   
VCONTMV4 MVC   SAVECACN(0),TRSBNAME                                             
VCONTCLC CLC   0(0,R7),INFCAC                                                   
         EJECT                                                                  
*              MAIN PROCESS FOR A RECORD                                        
         SPACE 1                                                                
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ1**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60601,RB                                                        
         USING T606TWA,RA                                                       
         L     R8,ALOCAL                                                        
         USING LOCALD,R8                                                        
         L     R7,ATIA             R7 = A(TABLE ENTRY)                          
         MVC   TABWIDTH(8),0(R7)   TABLE ENTRY WIDTH & MAX NO ENTRIES           
         LA    R7,8(R7)                                                         
         USING TABLED,R7                                                        
         SPACE 1                                                                
         CLI   LASTKMK,0           CONTINUATION OR PREVIOUS SCREEN              
         BNE   TDISP                                                            
         SPACE 1                                                                
TFIRST   CLI   VIRGIN,C'2'         FIRST REC - INITIALISE TABLE & COUNT         
         BE    TUPDATE                                                          
         XC    TABLE(2*TABLEN),TABLE                                            
         MVI   TABLE,X'FF'                                                      
         MVC   RECNUM,=F'1'                                                     
         XC    COUNT,COUNT                                                      
         ZAP   DEBIT,=P'0'                                                      
         ZAP   CREDIT,=P'0'                                                     
         MVI   VIRGIN,C'2'                                                      
         MVC   OTHERDR(12),DEBIT                                                
         ST    R7,ATABLE                                                        
         MVI   TABTYPE,0           SET INITIAL TABLE WIDTH = TYPE 0             
         MVC   TABWIDTH(8),TABPROFS                                             
         L     R9,AIO                                                           
         USING ACKEYD,R9                                                        
         MVC   SAVEKEY,ACKEYD      SAVE ACCOUNT KEY                             
         ICM   R9,15,ABAL                                                       
         BZ    TNEXT                                                            
         USING ACBALD,R9           PUT BALANCE FORWARD ENTRY IN TABLE           
         MVI   TABLE,X'01'         = B/F                                        
         ZAP   TABDR,ACBLFRWD                                                   
         ZAP   TABCR,=P'9999'      NONZERO TO PREVENT COMPRESSION               
         MVI   TABLE+TABLEN,X'FF'                                               
         MVI   RECNUM+3,2                                                       
         B     TNEXT                                                            
         EJECT                                                                  
*              UPDATE TABLE OF CONTRA CODE/DEBITS/CREDITS/NAME                  
         SPACE 1                                                                
TUPDATE  L     R9,AIO                                                           
         USING ACKEYD,R9                                                        
         CLI   ACKEYACC,RUNLAST    NO MORE RECORDS TO READ                      
         BNE   T1                                                               
         CLI   TABLE,X'FF'         ANYTHING TO DISPLAY                          
         BNE   T01                                                              
         CP    OTHERDR,=P'0'                                                    
         BNE   T02                                                              
         CP    OTHERCR,=P'0'                                                    
         BNE   T02                                                              
         B     TXIT                                                             
T01      XC    CONCODE,CONCODE     IF SO ADD OTHERS VALUES TO FINAL             
         MVI   CONCODE,X'FF'       TABLE ENTRY                                  
         BAS   RE,FINDIT                                                        
T02      MVC   TABDR(12),OTHERDR                                                
         L     R7,ATABLE                                                        
         B     TDISP                                                            
         SPACE 1                                                                
T1       L     R9,AIO                                                           
         USING ACKEYD,R9                                                        
         OC    ACDTPEEL,ACDTPEEL   SKIP PEELED                                  
         BNZ   TNEXT                                                            
         MVI   DMCB,0              ELEMENT FILTERING                            
         L     RF,AFILTER                                                       
         BASR  RE,RF                                                            
         BNZ   T3                                                               
T2       ICM   R9,15,ATRN          ADD FILTERED OUT VALUES TO OTHER             
         BZ    TNEXT                                                            
         USING TRANSD,R9                                                        
         LA    R5,OTHERDR                                                       
         TM    TRNSSTAT,X'80'                                                   
         BO    *+8                                                              
         LA    R5,6(R5)                                                         
         AP    0(6,R5),TRNSAMNT                                                 
         B     TNEXT                                                            
         SPACE 1                                                                
T3       DS    0H                  ADD NAME OR VALUE TO TABLE                   
         USING ACKEYD,R9                                                        
         MVC   CONCODE,ACKEYCON                                                 
         MVC   CONNAME,SPACES                                                   
         ICM   R9,15,ASUB                                                       
         BZ    T31                                                              
         CLI   TABTYPE,0           DONT ADD NAME IF TABLE COMPRESSED            
         BNE   TNEXT                                                            
         USING TRSUBHD,R9                                                       
         ZIC   R3,TRSBLEN                                                       
         SH    R3,=H'18'                                                        
         BM    T31                                                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   CONNAME(0),TRSBNAME                                              
         SPACE 1                                                                
T31      BAS   RE,FINDIT                                                        
         BZ    T2                  NOT FOUND AND NO ROOM TO ADD IT              
         ICM   R9,15,ATRN                                                       
         USING TRANSD,R9                                                        
         BZ    TNEXT                                                            
         LA    R5,TABDR                                                         
         TM    TRNSSTAT,X'80'                                                   
         BO    *+8                                                              
         LA    R5,6(R5)                                                         
         AP    0(6,R5),TRNSAMNT                                                 
         B     TNEXT                                                            
         EJECT                                                                  
*              SET UP DISPLAY FROM TABLE                                        
         SPACE 1                                                                
TDISP    MVI   VIRGIN,C'H'                                                      
         CLI   LINE+1,3                                                         
         BE    T32                                                              
         LA    R6,INFDAT2H                                                      
         GOTO1 EDITACNM            SET UP ACCOUNT & CONTRA NAMES                
         MVC   INFDAT3,HEADING     AND HEADING                                  
         OI    INFDAT3H+6,X'80'                                                 
T32      MVI   LINE+1,4                                                         
         MVI   LASTKMK,0                                                        
         LH    R5,COUNT            R5 = COUNT OF TABLE ENTRIES DISPLAYD         
         LA    R6,INFDAT5H         R6 = A(LINE HEADER)                          
         USING LINED,R6                                                         
         ZAP   DEBIT,=P'0'                                                      
         ZAP   CREDIT,=P'0'                                                     
         MVC   OTHERDR(12),DEBIT                                                
         CLC   INFKEY(4),=C'PREV'  ADJUST COUNT IN R5 FOR PREVIOUS              
         BNE   T4                                                               
         SR    R2,R2                                                            
         LR    R3,R5                                                            
         D     R2,=F'13'                                                        
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         LA    R2,13                                                            
         LA    R2,13(R2)                                                        
         SR    R5,R2                                                            
         B     T4                                                               
HEADING  DC    CL39'CONTRA-ACCOUNT'                                             
         DC    CL39'                   DEBITS      CREDITS'                     
         SPACE 1                                                                
T4       CLI   TABLE,X'01'         DISPLAY B/F TOTAL FROM X'01' ENTRY           
         BNE   T5                                                               
         MVC   INFDAT4+15(15),=C'BALANCE FORWARD'                               
         LA    RF,INFDAT4+31                                                    
         EDIT  (P6,TABDR),(13,0(RF)),2,MINUS=YES                                
         OI    INFDAT4H+6,X'80'                                                 
         AP    DEBIT,TABDR                                                      
         A     R7,TABWIDTH                                                      
         SPACE 1                                                                
T5       LTR   R5,R5               IF THIS IS A CONTINUATION SCREEN             
         BZ    T6                  ADD VALUES UP TO START POINT INTO            
         LA    R3,1                OTHERS                                       
         LR    R4,R3                                                            
T52      AP    OTHERDR,TABDR                                                    
         AP    OTHERCR,TABCR                                                    
         CP    TABDR,=P'0'         DONT BUMP COUNT FOR ZERO ENTRIES AS          
         BNE   T54                 THESE AREN'T DISPLAYED                       
         CP    TABCR,=P'0'                                                      
         BNE   T54                                                              
         BCTR  R3,0                                                             
T54      A     R7,TABWIDTH                                                      
         BXLE  R3,R4,T52                                                        
         SPACE 1                                                                
T6       CLI   TABLE,X'FF'         DISPLAY A LINE FOR A CONTRA                  
         BE    TOTALS                                                           
         CLC   LINE,=H'17'                                                      
         BE    TFULL                                                            
         CP    TABDR,=P'0'         ZERO ENTRIES NOT DISPLAYED                   
         BNE   *+14                                                             
         CP    TABCR,=P'0'                                                      
         BE    T7                                                               
         LA    R3,13                                                            
         LA    R4,TABCON+1                                                      
         CLI   0(R4),C' '                                                       
         BH    *+12                                                             
         LA    R4,1(R4)                                                         
         BCT   R3,*-12                                                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LINEDATA(0),0(R4)                                                
         BAS   RE,GETNAME                                                       
         MVC   LINEDATA+15(36),WORK                                             
         LA    R4,TABDR                                                         
         BAS   RE,DISPVALS                                                      
         AP    DEBIT,TABDR                                                      
         AP    CREDIT,TABCR                                                     
         LA    R5,1(R5)            BUMP COUNT                                   
         LH    R1,LINE                  SCREEN LINE                             
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         LA    R6,LINELEN(R6)                                                   
T7       A     R7,TABWIDTH              TABLE                                   
         B     T6                                                               
         EJECT                                                                  
*              SET UP OTHERS LINE AND PRESENT BALANCE/TOTAL LINE                
         SPACE 1                                                                
TFULL    OI    LASTKMK,X'80'       CONTINUATION BIT                             
         SPACE 1                                                                
TOTALS   STH   R5,COUNT            DISPLAY OTHERS AND PRESENT BALANCE           
         C     R5,=F'13'                                                        
         BNH   *+8                                                              
         OI    LASTKMK,X'40'       PREVIOUS SCREEN MARKER                       
T9       AP    OTHERDR,TABDR                                                    
         AP    OTHERCR,TABCR                                                    
         CLI   TABLE,X'FF'                                                      
         BE    *+12                                                             
         A     R7,TABWIDTH                                                      
         B     T9                                                               
         CP    OTHERDR,=P'0'                                                    
         BNE   *+14                                                             
         CP    OTHERCR,=P'0'                                                    
         BE    T95                                                              
         AP    DEBIT,OTHERDR       ADD TO GRAND TOTALS                          
         AP    CREDIT,OTHERCR                                                   
         MVC   LINEDATA+15(6),=C'OTHERS'                                        
         LA    R4,OTHERDR                                                       
         BAS   RE,DISPVALS                                                      
         LA    R6,LINELEN(R6)                                                   
T95      MVC   LINEDATA+46(5),=C'TOTAL'                                         
         LA    R4,DEBIT                                                         
         BAS   RE,DISPVALS                                                      
         SP    DEBIT,CREDIT                                                     
         MVC   LINEDATA+15(15),=C'PRESENT BALANCE'                              
         LA    RF,LINEDATA+31                                                   
         EDIT  (P6,DEBIT),(13,0(RF)),2,MINUS=YES                                
         CLI   LASTKMK,0           COULD THERE BE A 'PREV' OR 'NEXT'            
         BE    TXIT                ENQUIRY                                      
         GOTO1 AWRITIA             IF SO SAVE TABLE                             
         MVI   LINE+1,3            AND HEADLINES                                
         TM    LASTKMK,X'80'                                                    
         BZ    TXIT                                                             
         LNR   RF,RF               CC = NEG FOR SCREEN FULL                     
         B     TXIT                                                             
TNEXT    L     R7,ATIA                                                          
         MVC   0(8,R7),TABWIDTH                                                 
         LTR   RB,RB               CC = POS FOR NEXT RECORD PLEASE              
TXIT     XIT1                      CC = EQU FOR END                             
         EJECT                                                                  
*              GET A CONTRA-ACCOUNT NAME INTO WORK FOR DISPLAY                  
*              ON ENTRY R7   = A(TABLE ENTRY) COVERED BY TABLED                 
*              ON EXIT  WORK = NAME(36) OR SPACES                               
         SPACE 1                                                                
GETNAME  NTR1                      GET NAME FROM TABLE ENTRY IF PRESENT         
         MVC   WORK(36),SPACES                                                  
         L     R1,TABWIDTH                                                      
         SH    R1,=H'28'                                                        
         BM    GETN2                                                            
         EX    R1,*+8                                                           
         B     GETNX                                                            
         MVC   WORK(0),TABNAME                                                  
         SPACE 1                                                                
GETN2    LA    R4,KEYB                                                          
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES       IF NO NAME IN TABLE AND LEDGER IS            
         L     R9,AIOB             PRODUCTION, READ ACCOUNT RECORD              
         CLC   PRODUNIT(2),SAVEKEY+1                                            
         BNE   GETN3                                                            
         MVC   ACTKCULA,TABCON                                                  
         GOTO1 AREADB                                                           
         BZ    GETNX                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNZ   GETNX                                                            
         USING ACNAMED,R9                                                       
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         BM    GETNX                                                            
         EX    R3,*+8                                                           
         B     GETNX                                                            
         MVC   WORK(0),ACNMNAME                                                 
         SPACE 1                                                                
         USING CHDRECD,R4                                                       
GETN3    MVC   CHDKCULA,SAVEKEY    OTHERWISE READ SUB-A/C HEADER FOR            
         MVC   CHDKCULC,TABCON     NAME                                         
         GOTO1 AREADB                                                           
         BZ    GETNX                                                            
         MVI   ELCODE,X'43'                                                     
         BAS   RE,GETEL                                                         
         BNZ   GETNX                                                            
         USING TRSUBHD,R9                                                       
         ZIC   R3,TRSBLEN                                                       
         SH    R3,=H'18'                                                        
         BM    GETNX                                                            
         EX    R3,*+8                                                           
         B     GETNX                                                            
         MVC   WORK(0),TRSBNAME                                                 
         SPACE 1                                                                
GETNX    XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*              DISPLAY A LINE OF VALUES                                         
*              ON ENTRY R4 = A(DEBIT/CREDIT PAIR OF PL6 VALUE FIELDS)           
*                       R6 = A(LINE HEADER) COVERED BY DSECT LINED              
         SPACE 1                                                                
DISPVALS OI    LINEHDR+6,X'80'                                                  
         LA    RF,LINEDATA-2       RF=NOTIONAL COLUMN ZERO ON SCREEN            
         EDIT  (P6,0(R4)),(13,54(RF)),2,MINUS=YES                               
         EDIT  (P6,6(R4)),(13,67(RF)),2,MINUS=YES                               
         BR    RE                                                               
         EJECT                                                                  
*              FIND A TABLE ENTRY OR ADD ONE WITH ZERO VALUES                   
*              ON ENTRY CONCODE(15) = CONTRA-ACCOUNT KEY FOR ENTRY              
*              ON EXIT  R7          = A(TABLE ENTRY) COVERED BY TABLED          
*                       CC          = EQU IF TABLE IS FULL                      
*                                                                               
FINDIT   NTR1                                                                   
FIND2    LM    R4,R6,RECNUM                                                     
         GOTO1 VBINSRCH,DMCB,(1,CONCODE),ATABLE,(R4),(R5),(0,15),(R6)           
         MVC   RECNUM,DMCB+8                                                    
         ICM   R7,15,DMCB                                                       
         BNZ   FINDX                                                            
         SPACE 1                                                                
         ZIC   R1,TABTYPE          TABLE IS FULL SO COMPRESS IT BY              
         LA    R1,1(R1)            INCREMENTING THE TABLE TYPE (PRE-SET         
         LR    R0,R1               TO ZERO), USING THIS TO INDEX INTO           
         SLL   R1,3                A SET OF ALTERNATIVE TABLE PROFILES          
         LA    R1,TABPROFS(R1)     CONSISTING OF REDUCED TABLE ENTRY            
         CLI   0(R1),X'FF'         WIDTHS AND INCREASED MAX. NOS., AND          
         BE    FINDX               COMPRESSING THE TABLE ACCORDINGLY.           
         STC   R0,TABTYPE                                                       
         L     R7,ATABLE                                                        
         L     R5,RECNUM                                                        
         M     R4,TABWIDTH                                                      
         AR    R5,R7                                                            
         BCTR  R5,0                                                             
         L     R4,TABWIDTH                                                      
         MVC   TABWIDTH(8),0(R1)                                                
         LR    R1,R7                                                            
         L     R2,TABWIDTH                                                      
         LR    R6,R2                                                            
         BCTR  R6,0                                                             
         L     R3,RECNUM                                                        
FIND3    CLI   TABLE,X'FF'                                                      
         BE    FIND4                                                            
         CP    TABDR,=P'0'         ELIMINATE ZERO ENTRIES TO SAVE SPACE         
         BNE   FIND4                                                            
         CP    TABCR,=P'0'                                                      
         BNE   FIND4                                                            
         BCTR  R3,0                                                             
         B     *+10                                                             
FIND4    EX    R6,MOVNTRY                                                       
         AR    R1,R2                                                            
         BXLE  R7,R4,FIND3                                                      
         ST    R3,RECNUM                                                        
         B     FIND2                                                            
MOVNTRY  MVC   0(0,R1),0(R7)                                                    
         SPACE 1                                                                
FINDX    XIT1  REGS=(R7)                                                        
         SPACE 1                                                                
TABPROFS DC    F'63',F'36'         INITIAL PROFILE - FULL CONTRA NAME           
         DC    F'27',F'85'         MINIMUM PROFILE - NO   CONTRA NAME           
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R9,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              DSECT TO COVER LOCAL WORKING STORAGE                             
LOCALD   DSECT                                                                  
RECNUM   DS    F         X         NUMBER OF TABLE ENTRIES SO FAR               
TABWIDTH DS    F         X         CURRENT SIZE OF AN ENTRY (26 TO 62)          
TABMAX   DS    F         X         CURRENT MAX NUMBER OF ENTRIES                
ATABLE   DS    A         X         ADDRESS OF TABLE                             
OTHERDR  DS    PL6       P         TOTAL OF OTHER DEBITS                        
OTHERCR  DS    PL6       P         TOTAL OF OTHER CREDITS                       
TABTYPE  DS    C         X         TABLE TYPE - INDEX INTO TABPROFS             
CONCODE  DS    CL15      C         CONTRA-ACCOUNT CODE = KEY                    
DEBIT    DS    PL6       P         *USED FOR ADDITION OF NEW ENTRIES            
CREDIT   DS    PL6       P         *AND GRAND TOTALLING                         
CONNAME  DS    CL36      C         CONTRA-ACCOUNT NAME                          
         SPACE 1                                                                
*              DSECT TO COVER A TABLE ENTRY IN TIA                              
TABLED   DSECT                                                                  
TABLE    DS    0C                                                               
TABCON   DS    CL15      C         CONTRA-ACCOUNT CODE = KEY                    
TABDR    DS    PL6       P         DEBITS FOR C/A                               
TABCR    DS    PL6       P         CREDITS FOR C/A                              
TABNAME  DS    CL36      C         CONTRA-ACCOUNT NAME (0 TO 36 BYTES)          
TABLEN   EQU   *-TABLED            INITIAL ENTRY SIZE                           
         EJECT                                                                  
*                                                                               
*              NESTED INCLUDE FOR ACINQDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
SAVEKEY  DS    CL42      C         SAVED ACCOUNT KEY                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACINQ01   05/01/02'                                      
         END                                                                    
