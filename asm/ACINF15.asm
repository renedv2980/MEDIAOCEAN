*          DATA SET ACINF15    AT LEVEL 010 AS OF 05/01/02                      
*PHASE T6050FA,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE LI'                         
T6050F   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE 'LI' IN ACCOUNTS INFO PROGRAM             
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T6050F)                                               
         DC    A(FILTABLE-T6050F)                                               
         DC    A(PREHEADS-T6050F)                                               
         DC    A(PRETABLE-T6050F)                                               
         DC    A(HEADINGS-T6050F)                                               
         DC    A(DATTABLE-T6050F)                                               
         DC    A(KNTRYPNT-T6050F)                                               
         DC    A(FNTRYPNT-T6050F)                                               
         DC    A(DNTRYPNT-T6050F)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'RECORDTYPE'                                                 
         DC    C' '                                                             
         DC    C' '                                                             
         DC    AL1(ACLKCEQU)                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LIST CODE'                                                  
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(2)                                                           
         DC    AL1(5)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'ENDDATE'                                                    
         DC    CL2'EN'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(0)              FORCE VALIDATION BY FNTRYPNT                 
         DC    AL2(0)                                                           
         DC    AL1(9)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'TYPE'                                                       
         DC    CL2'TY'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ALST-GWS)                                                    
         DC    AL2(ACLITYPE-ACLISTD)                                            
         DC    AL1(6)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
         EJECT                                                                  
*              PRE-HEADING LINE - TO CONTAIN CONSTANT ELEMENTS OF               
*                                 EXPRESSIONS IN FORM X=Y,A=B                   
*                                                                               
*              CL39      C                      SCREEN COLS  2-40               
*              CL39      C                      SCREEN COLS 41-79               
*                                                                               
PREHEADS DC    CL39'                                       '                    
         DC    CL39'                                       '                    
*                                                                               
*              PRE-HEADING DATA TABLE -  9 BYTE ENTRIES                         
*                                                                               
*              CONTENTS AS FOR DISPLAY DATA TABLE BELOW                         
*                                                                               
PRETABLE DC    X'FF'                                                            
*                                                                               
*              SCREEN HEADINGS - 2 LINES                                        
*                                                                               
*              CL39      C         FIRST LINE - SCREEN COLS  2-40               
*              CL39      C         2ND   LINE -                                 
*              CL39      C         FIRST LINE - SCREEN COLS 41-79               
*              CL39      C         2ND   LINE                                   
*                                                                               
HEADINGS DC    CL39'LIST CODE AND NAME       TYPE   EXPIRY '                    
         DC    CL39'------------------       ----    DATE  '                    
         DC    CL39'   LIST CONTENTS                       '                    
         DC    CL39'   -------------                       '                    
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       LIST CODE                                    
         DC    AL2(ACLKLIST-ACLKEY)                                             
         DC    AL1(L'ACLKLIST)                                                  
         DC    AL2(0)                                                           
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ANAM-GWS)       LIST NAME                                    
         DC    AL2(0)                                                           
DATNAMLN DC    AL1(18)                                                          
         DC    X'FF00'                                                          
DATNAMST DC    AL1(8)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ALST-GWS)       TYPE                                         
         DC    AL2(ACLITYPE-ACLISTD)                                            
DATTYPLN DC    AL1(6)                                                           
         DC    X'FF00'                                                          
DATTYPST DC    AL1(27)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ALST-GWS)       EXPIRY DATE                                  
         DC    AL2(ACLIDATE-ACLISTD)                                            
DATDATLN DC    AL1(9)                                                           
         DC    X'FF00'                                                          
DATDATST DC    AL1(34)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ALSD-GWS)       LIST CONTENTS                                
         DC    AL2(0)                                                           
DATCONLN DC    AL1(36)                                                          
         DC    X'FF00'                                                          
DATCONST DC    AL1(44)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
FNTRYPNT DS    0D                  HANDLE FILTER VALIDATION AND MASSAGE         
         NMOD1 0,**AISF**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6050F,RB                                                        
         USING FTBD,R6                                                          
         OC    FTBDISP,FTBDISP                                                  
         BZ    FVALDAT             DATE VALIDATION                              
         CLC   FTBDISP,=AL2(ACLIDATE-ACLISTD)                                   
         BE    FDATE               DATE MASSAGE                                 
         CLC   FTBDISP,=AL2(ACLITYPE-ACLISTD)                                   
         BE    FTYPE               TYPE MASSAGE                                 
         DC    H'0'                                                             
*                                                                               
FVALDAT  LA    RF,ALST             BUILD FILTAB ENTRY FOR DATE                  
         ST    RF,FTBELMNT                                                      
         MVC   FTBDISP,=AL2(ACLIDATE-ACLISTD)                                   
         MVI   FTBLEN,L'ACLIDATE                                                
         MVI   FTBMARK,C'C'                                                     
         LA    RF,FNTRYPNT                                                      
         ST    RF,FTBSR                                                         
         MVC   FTBVAL(L'ACLIDATE),=X'FFFFFF'                                    
         L     R2,4(R1)            (ADDRESS OF INPUT VALUE)                     
         ZIC   RF,WORK+1           (LENGTH OF INPUT VALUE)                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),=C'PERMANENT'                                            
         BE    FVAL3                                                            
         GOTO1 VDATVAL,DMCB,(R2),WORK                                           
         OC    DMCB,DMCB                                                        
         BNZ   FVAL2                                                            
         MVI   DMCB,1              ERROR                                        
         MVI   ERROR,DATERR                                                     
         B     EXIT                                                             
FVAL2    GOTO1 VDATCON,(R1),WORK,(1,FTBVAL)                                     
FVAL3    LA    R6,FTBTBLEN(R6)     BUMP FILTAB                                  
         B     EXIT6                                                            
*                                                                               
FDATE    CLC   0(3,R2),FTBVAL      MASSAGE DATE FILTER                          
         BNL   EXIT                                                             
         MVC   WORK(3),FTBVAL      IF LESS EQUATE COMPARANDS                    
         LA    R2,WORK                                                          
         B     EXIT2               RETURN R2                                    
*                                                                               
FTYPE    BAS   RE,GETTYPE          MASSAGE TYPE FILTER                          
         B     EXIT2               RETURN R2                                    
         DROP  R6                                                               
         EJECT                                                                  
DNTRYPNT DS    0D                  HANDLE DISPLAY                               
         NMOD1 0,**AISF**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6050F,RB                                                        
         USING T605TWA,RA                                                       
         USING DATTABD,R5                                                       
         MVC   WORK(80),SPACES                                                  
         CLC   DATSTART,DATNAMST                                                
         BE    DTNAM               LIST NAME                                    
         CLC   DATSTART,DATTYPST                                                
         BE    DTTYP               TYPE                                         
         CLC   DATSTART,DATDATST                                                
         BE    DTDAT               DATE                                         
         CLC   DATSTART,DATCONST                                                
         BE    DTDATA              LIST CONTENTS                                
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
DTNAM    DS    0H                  CHOP NAME ONTO 2 LINES                       
         GOTO1 VCALLOV,DMCB,0,X'C1000A02',0                                     
         L     RF,0(R1)                                                         
         ZIC   R0,1(R2)                                                         
         SH    R0,=H'2'                                                         
         BM    DTNAM2                                                           
         GOTO1 (RF),(R1),((R0),2(R2)),(DATNAMLN,WORK),2                         
         ICM   R4,15,8(R1)                                                      
         BZ    *+6                                                              
         BCTR  R4,0                RETURN NUM OF EXTRA LINES IN R4              
DTNAM2   LA    R2,WORK                                                          
         B     EXIT234                                                          
*                                                                               
DTTYP    BAS   RE,GETTYPE          DISPLAY TYPE                                 
         B     EXIT2                                                            
*                                                                               
DTDAT    CLC   0(3,R2),=X'FFFFFF'  NEW WAY FOR YEAR 2000                        
         BNE   DTDAT2              OR                                           
         MVC   WORK(9),=C'PERMANENT'                                            
DTDAT1   LA    R2,WORK                                                          
         B     EXIT234                                                          
DTDAT2   MVC   WORK(9),SPACES                                                   
         GOTO1 VDATCON,DMCB,(1,0(R2)),(8,WORK)                                  
         B     DTDAT1                                                           
         EJECT                                                                  
*              HANDLE DISPLAY OF LIST CONTENTS                                  
*                                                                               
DTDATA   OC    LINE,LINE           CLEAR SAVED DATA IF NEW SEQUENCE             
         BNZ   DTD1                                                             
         CLC   INFKEY(4),=C'NEXT'                                               
         BE    DTD1                                                             
         MVI   LSTELNO,0                                                        
         MVI   SUBDISP,0                                                        
DTD1     L     R5,ADRIOB           R5 = LINE POINTER IN OUTPUT BLOCK            
         ZIC   R6,DATCONLN                                                      
         AR    R6,R5               R6 = NEXT LINE POINTER                       
         SR    R4,R4               R4 = COUNT OF EXTRA LINES                    
         MVC   HALF,LINE           HALF = MY OWN LINE NUMBER                    
         MVC   0(80,R5),SPACES                                                  
         ICM   R7,15,ALSD          R7 = A(LIST DATA EL)                         
         BZ    DTDX                NONE                                         
         USING ACLDATAD,R7                                                      
         SR    RF,RF                                                            
         ICM   RF,1,LSTELNO        ARE WE STARTING AT FIRST                     
         BZ    DTD4                YES                                          
         SR    R0,R0               NO BUMP TO THE NTH                           
DTD2     IC    R0,ACLDLEN                                                       
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BE    DTDX                THE NTH ISNT THERE                           
         CLI   ACLDEL,ACLDELEQ                                                  
         BNE   DTD2                                                             
         BCT   RF,DTD2                                                          
*                                                                               
DTD4     DS    0H                  SET UP SUBEL CONTROL                         
         ZIC   R2,ACLDITLN         R2 = SUBEL SIZE                              
         LA    R1,ACLDUL           R1 = SUBEL POINTER                           
         L     RE,ALST                                                          
         CLI   ACLITYPE-ACLISTD(RE),C'L'                                        
         BE    *+8                                                              
         LA    R1,ACLDACCS                                                      
         CLI   SUBDISP,0           ARE WE STARTING AT BEGINNING                 
         BE    *+12                YES                                          
         ZIC   R1,SUBDISP          IF NOT ADD SAVED DISP                        
         AR    R1,R7                                                            
         ZIC   R3,ACLDLEN                                                       
         AR    R3,R7                                                            
         BCTR  R3,0                R3 = A(LAST BYTE OF ELEMENT)                 
*                                                                               
DTD6     L     RE,ADRIOB           NEW LINE CONTROL                             
         CR    RE,R5                                                            
         BE    DTD10               NOT FIRST TIME FOR RECORD                    
         CLC   HALF,=H'20'                                                      
         BNE   DTD8                                                             
         SR    R1,R7               SCREEN FULL                                  
         STC   R1,SUBDISP          SAVE DISP INTO ELEMENT                       
         L     RF,ADRIO                                                         
         LA    RF,ACLKLIST-ACLKEY+L'ACLKLIST-1(RF)                              
         ZIC   RE,0(RF)                                                         
         BCTR  RE,0                                                             
         STC   RE,0(RF)            REDUCE KEY TO FORCE READ OF SAME             
         B     DTDX                RECORD NEXT TIME                             
*                                                                               
DTD8     LA    R4,1(R4)            WE HAVE ANOTHER LINE                         
         LH    RF,HALF             BUMP MY LINE COUNT                           
         LA    RF,1(RF)                                                         
         STH   RF,HALF                                                          
         LR    R5,R6               RESET THIS LINE POINTER                      
         ZIC   R6,DATCONLN                                                      
         AR    R6,R5               AND THE NEXT LINE POINTER                    
         MVC   0(80,R5),SPACES     CLEAR NEW LINE                               
*                                                                               
DTD10    LA    RF,0(R1,R2)         HANDLE SUBEL ITEM                            
         BCTR  RF,0                GET ACTUAL ITEM LENGTH INTO RE               
         LR    RE,R2                                                            
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         LA    RF,0(R5,RE)         SEE IF IT FITS INTO THIS LINE                
         CR    RF,R6                                                            
         BH    DTD6                IT DOESNT                                    
         BCTR  RE,0                IT DOES SO MOVE IT IN                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R1)                                                    
         LA    R5,1(RE,R5)         BUMP LINE POINTER                            
         CR    R5,R6               ADD A COMMA TOO IF THERE'S ROOM              
         BNL   *+12                                                             
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)            AND BUMP POINTER AGAIN                       
         BXLE  R1,R2,DTD10         BUMP TO NEXT SUBEL                           
*                                                                               
DTD12    LA    R7,1(R3)            AT END OF EL RESET R7 TO NEXT ONE            
         MVI   SUBDISP,0           CLEAR SUBEL DISPLACEMENT                     
         CLI   ACLDEL,ACLDELEQ     ANY MORE LIST DATA                           
         BNE   DTD14               NO                                           
         ZIC   RF,LSTELNO          IF SO BUMP ELEMENT SEQUENCE                  
         LA    RF,1(RF)                                                         
         STC   RF,LSTELNO                                                       
         B     DTD4                AND GO  BACK TO SET SUBEL CONTROL            
*                                                                               
DTD14    MVI   LSTELNO,0           AT END OF RECORD CLEAR EL SEQUENCE           
         BCTR  R5,0                CLEAR FINAL COMMA IF ANY                     
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         B     DTDX                                                             
*                                                                               
DTDX     L     R2,ADRIOB           RETURN A(BLOCK)/WIDTH/NUM OF XTRA            
         ZIC   R3,DATCONLN         LINES                                        
         B     EXIT234                                                          
         DROP  R7                                                               
         EJECT                                                                  
*              ROUTINE TO GET EXPANDED (6 CHAR) TYPE                            
*              ON ENTRY R2 = A(ACLITYPE)                                        
*              ON EXIT  R2 = A(6-CHAR TYPE NAME)                                
*                                                                               
GETTYPE  NTR1                                                                   
         MVC   WORK(6),SPACES      TYPE NAME IS RETURNED IN WORK                
         LA    RF,TYPTAB                                                        
GT2      CLI   0(RF),0             FIND NAME FOR LETTER                         
         BE    GT4                                                              
         CLC   0(1,R2),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,L'TYPTAB(RF)                                                  
         B     GT2                                                              
         MVC   WORK(6),1(RF)                                                    
         B     GTX                                                              
*                                                                               
GT4      CLI   0(R2),C'A'          IF WE DIDNT FIND A NAME                      
         BE    *+14                IT SHOULD BE A=ACCOUNT                       
         MVC   WORK(6),=C'UNKNWN'                                               
         B     GTX                                                              
         MVC   WORK(5),=C'ACCNT'   IN WHICH CASE WE WANT THE                    
         ICM   R5,15,ALSD          NAME FOR ACCOUNTS AT THIS LEVEL              
         BZ    GTX                 FOR THIS LEDGER                              
         USING ACLDATAD,R5                                                      
         MVC   KEYB,SPACES                                                      
         MVC   KEYB(1),MYCO                                                     
         MVC   KEYB+1(2),ACLDAUNT                                               
         CLC   KEYB(3),SAVELEDG    DO WE HAVE THIS LEDGER                       
         BE    GT20                YES                                          
         MVC   WORK+6(L'KEY),KEY   SAVE W/S OVERWRITTEN BY GETUNLE              
         MVC   WORK+6+L'KEY(AELTBLEN),AELTAB                                    
         LA    R7,SAVELEDG                                                      
         MVC   KEY,KEYB                                                         
         GOTO1 AGETUNLE                                                         
         MVC   KEY,WORK+6          RESTORE SAVED W/S                            
         MVC   AELTAB(AELTBLEN),WORK+6+L'KEY                                    
         L     RE,ADRIO                                                         
         ST    RE,AKEY                                                          
         BZ    GTX                 COULDNT FIND LEDGER                          
         MVC   SAVELEDG(3),KEYB    SAVE KEY TO SAVE I/O NEXT TIME               
*                                                                               
GT20     ZIC   RF,ACLDALVL         NOW INDEX INTO HIERARCHY EL                  
         BCTR  RF,0                FOR ACCOUNT NAME FOR LEVEL                   
         MH    RF,=H'16'                                                        
         LA    RF,SAVEHIER+3(RF)                                                
         MVC   WORK(6),0(RF)       AND MOVE INTO WORK                           
*                                                                               
GTX      LA    R2,WORK             RETURN A(WORK)                               
         B     EXIT2                                                            
*                                                                               
TYPTAB   DS    0CL7                TABLE OF TYPELETTERS/NAMES                   
         DC    CL7'LLEDGER'                                                     
         DC    CL7'MMEDIA'                                                      
         DC    CL7'WWORKCD'                                                     
         DC    X'00'                                                            
*                                                                               
EXIT     XIT1                      SIMPLE EXIT                                  
*                                                                               
EXIT2    MVI   DMCB,0              SET FOR NO ERROR                             
         XIT1  REGS=(R2)           RETURN ADDRESS                               
*                                                                               
EXIT234  MVI   DMCB,0              SET FOR NO ERROR                             
         XIT1  REGS=(R2,R4)        RETURN ADDRESS,LENGTH,EXTRA LINES            
*                                                                               
EXIT6    MVI   DMCB,0              SET FOR NO ERROR                             
         XIT1  REGS=(R6)           RETURN ADDRESS OF NEXT FILTAB NTRY           
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
T605TWA  DSECT                                                                  
         ORG   RULCOUNT                                                         
LSTELNO  DS    X                   LIST DATA EL SEQUENCE NO (0 = 1ST)           
SUBDISP  DS    X                   DISP INTO LIST DATA EL OF NEXT SUBEL         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACINF15   05/01/02'                                      
         END                                                                    
