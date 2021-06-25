*          DATA SET ACINF13    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T6050DA,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE RL'                         
T60513   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE  RL   IN ACCOUNTS INFO PROGRAM            
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60513)                                               
         DC    A(FILTABLE-T60513)                                               
         DC    A(PREHEADS-T60513)                                               
         DC    A(PRETABLE-T60513)                                               
         DC    A(HEADINGS-T60513)                                               
         DC    A(DATTABLE-T60513)                                               
         DC    A(KNTRYPNT-T60513)                                               
         DC    A(FNTRYPNT-T60513)                                               
         DC    A(DNTRYPNT-T60513)                                               
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
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(PRODUNIT-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(PRODLEDG-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'40'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'LEVEL'                                                      
         DC    CL2'LE'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ALEVEL-GWS)                                                  
         DC    AL2(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEVEL'                                                      
         DC    CL2'LE'                                                          
         DC    CL8'1-2'                                                         
         DC    X'04'                                                            
         DC    AL2(ALEVEL-GWS)                                                  
         DC    AL2(3)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'MEDIA'                                                      
         DC    CL2'ME'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ARUL-GWS)                                                    
         DC    AL2(ACRLMED-ACRULED)                                             
         DC    AL1(L'ACRLMED)                                                   
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'MEDIA'                                                      
         DC    CL2'ME'                                                          
         DC    CL8'ALL'                                                         
         DC    AL1(0)                                                           
         DC    AL2(ARUL-GWS)                                                    
         DC    AL2(ACRLMED-ACRULED)                                             
         DC    AL1(L'ACRLMED)                                                   
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'WORKCODE'                                                   
         DC    CL2'WO'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ARUL-GWS)                                                    
         DC    AL2(ACRLWORK-ACRULED)                                            
         DC    AL1(L'ACRLWORK)                                                  
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'WORKCODE'                                                   
         DC    CL2'WO'                                                          
         DC    CL8'ALL'                                                         
         DC    AL1(0)                                                           
         DC    AL2(ARUL-GWS)                                                    
         DC    AL2(ACRLWORK-ACRULED)                                            
         DC    AL1(L'ACRLWORK)                                                  
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
PREHEADS DC    CL39'UNIT=                                / '                    
         DC    CL39'LEDGER=                                '                    
*                                                                               
*              PRE-HEADING DATA TABLE -  9 BYTE ENTRIES                         
*                                                                               
*              CONTENTS AS FOR DISPLAY DATA TABLE BELOW                         
*                                                                               
PRETABLE DC    AL2(AKEY-GWS)       UNIT CODE                                    
         DC    AL2(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL1(7)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AUNITNAM-GWS)   UNIT NAME                                    
         DC    AL2(0)                                                           
         DC    AL1(29)                                                          
         DC    AL2(0)                                                           
         DC    AL1(9)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AKEY-GWS)       LEDGER CODE                                  
         DC    AL2(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL1(48)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ALEDGNAM-GWS)   LEDGER NAME                                  
         DC    AL2(0)                                                           
         DC    AL1(30)                                                          
         DC    AL2(0)                                                           
         DC    AL1(50)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF PRE-HEADING DATA TABLE                
*                                                                               
*              SCREEN HEADINGS - 2 LINES                                        
*                                                                               
*              CL39      C         FIRST LINE - SCREEN COLS  2-40               
*              CL39      C         2ND   LINE -                                 
*              CL39      C         FIRST LINE - SCREEN COLS 41-79               
*              CL39      C         2ND   LINE                                   
*                                                                               
HEADINGS DC    CL39'ACCOUNT CODE ------------ACCOUNT NAME--'                    
         DC    CL39' '                                                          
*&&UK*&& DC    CL39'----------  WORKCODE  MEDIA  COMMN  TAX'                    
*&&US*&& DC    CL39'----------  WORKCODE  MEDIA  COMMN'                         
         DC    CL39' '                                                          
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD (DUMMY)              
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       ACCOUNT CODE                                 
         DC    AL2(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(0)                                                           
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ANAM-GWS)       ACCOUNT NAME                                 
         DC    AL2(ACNMNAME-ACNAMED)                                            
         DC    AL1(L'ACNMNAME)                                                  
         DC    AL2(EDITNAME-GWS)                                                
         DC    AL1(15)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AKEY-GWS)       RULE DETAILS - DUMMY ENTRY                   
         DC    AL2(0)                                                           
         DC    AL1(27)                                                          
         DC    X'FF00'             HANDLED BY OVERLAY INCLUDING EXTRA           
         DC    AL1(53)             FILTERING                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
*              SPECIAL DISPLAY DATA TABLE FOR RULES                             
*              NB EDIT S/R DISPLACEMENT IS FROM START OF OVERLAY                
         SPACE 1                                                                
RULFIELD DS    0C                                                               
*                                                                               
*                                                                               
         DC    AL2(ARUL-GWS)       MEDIA CODE                                   
         DC    AL2(ACRLMED-ACRULED)                                             
         DC    AL1(L'ACRLMED)                                                   
         DC    AL2(EDITMEWC-T60513)                                             
         DC    AL1(63)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ARUL-GWS)       WORK CODE                                    
         DC    AL2(ACRLWORK-ACRULED)                                            
         DC    AL1(L'ACRLWORK)                                                  
         DC    AL2(EDITMEWC-T60513)                                             
         DC    AL1(53)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ARUL-GWS)       COMMISSION RATE                              
         DC    AL2(ACRLCOMM-ACRULED)                                            
         DC    AL1(L'ACRLCOMM)                                                  
         DC    AL2(EDITCOMM-T60513)                                             
         DC    AL1(70)                                                          
         DC    AL1(0)                                                           
*                                                                               
*&&UK                                                                           
         DC    AL2(ARUL-GWS)       TAX RATE                                     
         DC    AL2(ACRLTAX-ACRULED)                                             
         DC    AL1(L'ACRLTAX)                                                   
         DC    AL2(EDITTAX-T60513)                                              
         DC    AL1(77)                                                          
         DC    AL1(0)                                                           
*&&                                                                             
*                                                                               
         DC    X'FF'                                                            
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
FNTRYPNT DS    0D                  FILTERS ON RULES ELEMENT FIELDS              
         NMOD1 0,**RLST**          ENSURE THAT THEY PASS THE RECORD-            
         L     RC,0(R1)            -LEVEL TESTS SO THAT THEY CAN BE             
         USING GWS,RC              HANDLED AT SUB-RECORD LEVEL LATER            
         L     RB,APHASE                                                        
         USING T60513,RB                                                        
         LA    R5,WORK+1                                                        
         MVI   WORK+1,1                                                         
         LA    R2,WORK                                                          
         MVI   WORK,0                                                           
         USING FTBD,R6                                                          
         CLI   FTBSIGN,C'N'        NEGATIVE                                     
         BE    *+8                                                              
         MVI   WORK,1                                                           
         LA    R3,1                                                             
FXIT     XIT1  REGS=(R2,R5)                                                     
         DROP  R6                                                               
         EJECT                                                                  
DNTRYPNT DS    0D                                                               
         NMOD1 0,**RLST**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60513,RB                                                        
         USING T605TWA,RA                                                       
         LA    R2,BINTAB           R2 = A(DISPLAY FIELD)                        
         LA    R3,RLFLDLEN         R3 = WIDTH OF DISPLAY FIELD                  
         SR    R4,R4               R4 = LENGTH MINUS 1 (INITIALLY '-1')         
         BCTR  R4,0                                                             
         LR    R5,R2               R5 = DISPLAY FIELD POINTER                   
         LA    R6,1                R6 = BXLE REGISTER FOR PAGE CONTROL          
         LA    R7,20               R7 = DITTO                                   
         SH    R7,LINE                                                          
         ICM   R9,15,ARUL          R9 = RULE ELEMENT POINTER                    
         USING ACRULED,R9                                                       
         SPACE 1                                                                
         BZ    DEND1               NO RULES                                     
         CLC   INFKEY(4),=C'NEXT'  IF WE HAVE DISPLAYED SOME RULES FOR          
         BE    *+10                THIS RECORD ALREADY, BUMP TO THE             
         XC    RULCOUNT,RULCOUNT   FIRST UNDISPLAYED ELEMENT                    
         LH    RF,RULCOUNT                                                      
         LTR   RF,RF                                                            
         BZ    D20                                                              
         LA    RE,1                                                             
         LR    R1,RE                                                            
         ZIC   R8,ACRLLEN                                                       
D10      AR    R9,R8                                                            
         CLI   0(R9),X'42'         CHECK IN CASE FILTERS HAVE BEEN              
         BE    D15                 CHANGED IN MID-ENQUIRY                       
         XC    RULCOUNT,RULCOUNT                                                
         L     R9,ARUL                                                          
         B     D20                                                              
D15      BXLE  R1,RE,D10                                                        
         SPACE 1                                                                
D20      ST    R9,ARUL                                                          
         BAS   RE,FILTER           ELEMENT-LEVEL FILTER                         
         BZ    D30                                                              
         BXLE  R4,R6,*+10          WE HAVE AN ELEMENT TO DISPLAY SO             
         BCTR  R4,0                BUMP THE LINE COUNT                          
         B     DEND2               NO MORE ROOM                                 
         MVC   0(RLFLDLEN,R5),SPACES                                            
         BAS   RE,SETLINE                                                       
         LA    R5,RLFLDLEN(R5)                                                  
         SPACE 1                                                                
D30      LH    R1,RULCOUNT         BUMP TO NEXT RULES ELEMENT                   
         LA    R1,1(R1)                                                         
         STH   R1,RULCOUNT                                                      
         ZIC   R8,ACRLLEN                                                       
         AR    R9,R8                                                            
         CLI   0(R9),X'42'                                                      
         BE    D20                                                              
         SPACE 1                                                                
DEND1    XC    RULCOUNT,RULCOUNT   NO MORE RULES                                
         B     DXIT                                                             
         SPACE 1                                                                
DEND2    L     R1,ADRIO                                                         
         LA    R1,14(R1)           NO MORE ROOM ON PAGE                         
         CLI   LEVFILT,0           SO LOWER THE KEY IN IO TO FOOL THE           
         BE    DEND3               ROOT TO READ THIS RECORD AGAIN               
         CLC   LEVFILT,LEVEL+1                                                  
         BH    DEND3                                                            
         ZIC   RF,LEVFILT                                                       
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         ZIC   R1,SAVEHIER+2(RF)                                                
         L     RE,ADRIO                                                         
         LA    RE,2(RE)                                                         
         AR    R1,RE                                                            
DEND3    ZIC   RE,0(R1)            BY SUBTRACTING 1 FROM LAST SIGNFCNT          
         BCTR  RE,0                BYTE OF KEY                                  
         STC   RE,0(R1)                                                         
         SPACE 1                                                                
DXIT     MVI   DMCB,0              SET FOR NO ERROR                             
         XIT1  REGS=(R2,R4)        RETURN ADDRESS,LENGTH,NO. EXTRA LNES         
         EJECT                                                                  
*              APPLY FILTERS VIA FILTER TABLE - CC=ZERO IF NOT SATISF'D         
*                                                                               
FILTER   NTR1                                                                   
         LA    R3,FILTAB                                                        
         USING FTBD,R3                                                          
         LA    R8,ARUL                                                          
F1       CLI   0(R3),X'FF'                                                      
         BNE   *+10                                                             
         LTR   RB,RB                                                            
         B     FEXIT                                                            
         C     R8,FTBELMNT                                                      
         BNE   F5                                                               
         LR    R4,R9                                                            
         MVC   HALF,FTBDISP                                                     
         AH    R4,HALF             R4 = FIELD ADDRESS                           
*                                                                               
         CLI   0(R4),0             X'00' = ALL FOR MEDIA AND WORK CODE          
         BNE   F1A                                                              
         CLI   FTBMARK,C'M'        IF FILTER IS NOT ALL, CRITERION MET          
         BNE   F5                                                               
         CLI   FTBSIGN,C'P'                                                     
         BE    F5                  IF POSITIVE, CRITERION MUST BE MET           
         B     F6                                                               
*                                                                               
F1A      ZIC   R5,FTBLEN           R5 = FIELD LENGTH                            
         LA    R6,FTBVAL           R6 = A(MASK OR COMPARE VALUE)                
         MVC   FULL,BRANCH                                                      
         SR    R1,R1                                                            
         CLI   FTBMARK,C'M'                                                     
         BE    F3                                                               
         CLI   FTBSIGN,C'N'                                                     
         BE    *+8                                                              
         XI    FULL+1,X'F0'        CHANGE TO BNE                                
F2       BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R6)                                                    
         EX    R1,FULL             BRANCH IF EQUAL/UNEQUAL                      
         B     F5                                                               
         SPACE 1                                                                
F3       CLI   FTBSIGN,C'P'                                                     
         BE    *+8                                                              
         XI    FULL+1,X'F0'                                                     
F4       ZIC   R7,0(R6)                                                         
         EX    R7,*+8                                                           
         B     *+8                                                              
         TM    0(R4),0                                                          
         EX    R1,FULL             BRANCH IF NONZERO/ZERO                       
F5       LA    R3,FTBTBLEN(R3)                                                  
         B     F1                                                               
BRANCH   BE    F6                  EXECUTED BRANCH                              
*                                   COMPARE/NEGATIVE - FILTER OUT               
*                                   TEST(TM)POSITIVE - FILTER OUT               
F6       SR    R1,R1                                                            
FEXIT    XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*              SET UP A RULES ELEMENT DISPLAY                                   
*              R5 = A(DISPLAY FIELD)                                            
*              R9 = A(RULES ELEMENT)                                            
         SPACE 1                                                                
SETLINE  NTR1                                                                   
         LA    R4,RULFIELD                                                      
         USING DATTABD,R4                                                       
SL1      CLI   0(R4),X'FF'                                                      
         BE    SL4                                                              
         LR    R2,R9                                                            
         MVC   HALF,DATDISP                                                     
         AH    R2,HALF                                                          
         ZIC   R3,DATLEN                                                        
         BCTR  R3,0                                                             
         OC    DATEDIT,DATEDIT                                                  
         BE    SL2                                                              
         MVC   HALF,DATEDIT                                                     
         LH    RF,HALF             GET DISPLACEMENT                             
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
SL2      ZIC   R7,DATSTART                                                      
         AR    R7,R5                                                            
         SH    R7,=H'53'                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R2)                                                    
SL3      LA    R4,DATTBLEN(R4)                                                  
         B     SL1                                                              
*                                                                               
SL4      XIT1                                                                   
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
*              EDIT MEDIA OR WORKCODE FOR DISPLAY                               
*                                                                               
EDITMEWC NTR1                                                                   
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R2)                                                    
         CLI   0(R2),0                                                          
         BNE   EM1                                                              
         MVC   WORK(3),=C'ALL'                                                  
         LA    R3,2                                                             
EM1      LA    R2,WORK                                                          
         XIT1  REGS=(R2,R3)        R2 = ADDRESS, R3 = L-1                       
*                                                                               
*              EDIT COMMISSION RATE FOR DISPLAY                                 
*                                                                               
EDITCOMM NTR1                                                                   
         MVC   WORK+20(7),SPACES                                                
         CLI   ACRLCOMM,X'FF'                                                   
         BE    ECX                                                              
         CP    ACRLCOMM,=P'0'                                                   
         BNE   EC1                                                              
         MVC   WORK+20(4),=C'ZERO'                                              
         B     ECX                                                              
EC1      CLI   ACRLLEN,X'0B'                                                    
         BL    EC2                                                              
         TM    ACRLSTAT,X'80'      4DP                                          
         BO    EC4                                                              
EC2      EDIT  (P4,ACRLCOMM),(6,WORK+20),2                                      
         B     ECX                                                              
EC4      EDIT  (P4,ACRLCOMM),(7,WORK+20),4,DROP=1                               
ECX      LA    R2,WORK+20                                                       
         LA    R3,6                                                             
         XIT1  REGS=(R2,R3)        R2 = ADDRESS, R3 = L-1                       
*                                                                               
*              EDIT TAX RATE FOR DISPLAY -UK ONLY                               
*                                                                               
EDITTAX  NTR1                                                                   
         LA    R2,WORK                                                          
         MVI   WORK,C' '                                                        
         CLI   ACRLTAX,X'FF'                                                    
         BE    *+10                                                             
         MVC   WORK(1),ACRLTAX                                                  
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
RLFLDLEN EQU   27                  RULE DISPLAY FIELD WIDTH                     
         LTORG                                                                  
         EJECT                                                                  
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACINF13   05/01/02'                                      
         END                                                                    
