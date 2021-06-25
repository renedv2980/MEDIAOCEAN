*          DATA SET ACINF14    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T6050EA,*,NOAUTO                                                         
*INCLUDE CHOPPER                                                                
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE DI'                         
T6050E   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE 'DI' IN ACCOUNTS INFO PROGRAM             
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T6050E)                                               
         DC    A(FILTABLE-T6050E)                                               
         DC    A(PREHEADS-T6050E)                                               
         DC    A(PRETABLE-T6050E)                                               
         DC    A(HEADINGS-T6050E)                                               
         DC    A(DATTABLE-T6050E)                                               
         DC    A(KNTRYPNT-T6050E)                                               
         DC    A(FNTRYPNT-T6050E)                                               
         DC    A(DNTRYPNT-T6050E)                                               
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
         DC    X'FF00'             CHECK FOR UNIT 3                             
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS+1)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS+2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'DELETED'                                                    
         DC    CL2'DE'                                                          
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(ACSTATUS-ACKEYD)                                             
         DC    AL1(L'ACSTATUS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FILTER1'                                                    
         DC    CL2'F1'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FILTER2'                                                    
         DC    CL2'F2'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'FILTER3'                                                    
         DC    CL2'F3'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACOMFILT-GWS)                                                
         DC    AL2(2)                                                           
         DC    AL1(L'ACSTANAL)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEVEL'                                                      
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
         DC    CL10'SCHEME'                                                     
         DC    CL2'SC'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ADIS-GWS)                                                    
         DC    AL2(ACDICODE-ACDISTD)                                            
         DC    AL1(L'ACDICODE)                                                  
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
         DC    CL39'                                       '                    
         DC    CL39'----------   SCHEME CODE          UNITS'                    
         DC    CL39'                                       '                    
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
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
         DC    AL2(AKEY-GWS)       DUMMY FOR DISTRIB CODE/UNITS                 
         DC    AL2(0)              TO FORCE ENTRY TO OVERLAY                    
         DC    AL1(22)                                                          
         DC    X'FF00'                                                          
         DC    AL1(58)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
         EJECT                                                                  
KNTRYPNT DS    0D                  CHECK THAT UNIT KEY IS 3                     
         NMOD1 0,**AISE**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6050E,RB                                                        
         USING T605TWA,RA                                                       
         XC    RULCOUNT,RULCOUNT   CLEAR COUNT OF X'62' ELS DISPLAYED           
         MVI   OPTIONS,0           AND SCHEME CODE FILTER FLAG                  
         CLI   0(R2),C'3'                                                       
         BE    KXIT                                                             
         MVI   ERROR,UNITNVAL                                                   
         MVI   DMCB,1                                                           
KXIT     XIT1                                                                   
         EJECT                                                                  
FNTRYPNT DS    0D                  HANDLE SCHEME CODE FILTER                    
         NMOD1 0,**AISE**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6050E,RB                                                        
         USING FTBD,R6                                                          
         L     R9,ADIS                                                          
         USING ACDISTD,R9                                                       
         SR    R0,R0                                                            
         BCTR  R3,0                LENGTH OF FILTER VALUE                       
         SPACE 1                                                                
F05      LA    R2,ACDICODE         HANDLE AN ELEMENT                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(R5)                                                    
         BE    F20                                                              
         SPACE 1                                                                
F10      IC    R0,ACDILEN          BUMP TO NEXT                                 
         AR    R9,R0                                                            
         CLI   0(R9),0                                                          
         BE    FXIT                                                             
         CLI   0(R9),X'62'                                                      
         BE    F05                                                              
         B     F10                                                              
         SPACE 1                                                                
F20      CLI   FTBSIGN,C'P'        SET FLAG IF POS FILTER SATISFIED             
         BNE   FXIT                                                             
         MVI   OPTIONS,SCHEMFLT                                                 
FXIT     XIT1 REGS=(R2)                                                         
         DROP  R6                                                               
         EJECT                                                                  
DNTRYPNT DS    0D                  HANDLE DISTRIBUTION SCHEME DISPLAYS          
         NMOD1 0,**AISE**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6050E,RB                                                        
         USING T605TWA,RA                                                       
         LA    R2,BINTAB           R2 = A(DISPLAY FIELD)                        
         LA    R3,SCHEMLEN         R3 = WIDTH OF DISPLAY FIELD                  
         SR    R4,R4               R4 = LENGTH MINUS 1 (INITIALLY '-1')         
         BCTR  R4,0                                                             
         LR    R5,R2               R5 = DISPLAY FIELD POINTER                   
         LA    R6,1                R6 = BXLE REGISTER FOR PAGE CONTROL          
         LA    R7,20               R7 = DITTO                                   
         SH    R7,LINE                                                          
         ICM   R9,15,ADIS          R9 = DISTRIB ELEMENT POINTER                 
         USING ACDISTD,R9                                                       
         SPACE 1                                                                
         BZ    DEND1               NO DISTRIB ELS                               
         CLC   INFKEY(4),=C'NEXT'  IF WE HAVE DISPLAYED SOME ELS FOR            
         BE    *+10                THIS RECORD ALREADY, BUMP TO THE             
         XC    RULCOUNT,RULCOUNT   FIRST UNDISPLAYED ELEMENT                    
         LH    RF,RULCOUNT                                                      
         LTR   RF,RF                                                            
         BZ    D20                                                              
         LA    RE,1                                                             
         LR    R1,RE                                                            
         ZIC   R8,ACDILEN                                                       
D10      AR    R9,R8                                                            
         CLI   0(R9),X'62'         CHECK IN CASE FILTERS HAVE BEEN              
         BE    D15                 CHANGED IN MID-ENQUIRY                       
         XC    RULCOUNT,RULCOUNT                                                
         L     R9,ADIS                                                          
         B     D20                                                              
D15      BXLE  R1,RE,D10                                                        
         SPACE 1                                                                
D20      CLI   OPTIONS,SCHEMFLT    IF SCHEME FILTER ONLY SHOW FILTERED          
         BNE   D25                                                              
         BAS   RE,FILTER           ELEMENT-LEVEL FILTER                         
         BZ    D30                                                              
D25      BXLE  R4,R6,*+10          WE HAVE AN ELEMENT TO DISPLAY SO             
         BCTR  R4,0                BUMP THE LINE COUNT                          
         B     DEND2               NO MORE ROOM                                 
         MVC   0(SCHEMLEN,R5),SPACES                                            
         MVC   0(2,R5),ACDICODE                                                 
         EDIT  ACDIVAL,(12,10(R5)),2                                            
         LA    R5,SCHEMLEN(R5)                                                  
         SPACE 1                                                                
D30      LH    R1,RULCOUNT         BUMP TO NEXT RULES ELEMENT                   
         LA    R1,1(R1)                                                         
         STH   R1,RULCOUNT                                                      
D32      ZIC   R8,ACDILEN                                                       
         AR    R9,R8                                                            
         CLI   0(R9),X'62'                                                      
         BE    D20                                                              
         CLI   0(R9),0                                                          
         BNE   D32                                                              
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
*              WHERE POSITIVE SCHEME CODE FILTER(S) SELECT FOR DISPLAY          
*                                                                               
FILTER   NTR1                                                                   
         LA    R3,FILTAB                                                        
         USING FTBD,R3                                                          
         LA    R8,ADIS                                                          
         SPACE 1                                                                
F1       CLI   0(R3),X'FF'         LOOK FOR POS SCHEME FILTER                   
         BE    FEXIT               SET CC=EQU FOR NOT REQUIRED                  
         C     R8,FTBELMNT                                                      
         BNE   F5                                                               
         CLI   FTBSIGN,C'P'        POSITIVE                                     
         BNE   F5                                                               
         ZIC   R1,FTBLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACDICODE(0),FTBVAL                                               
         BNE   F5                                                               
         LTR   RB,RB               SET CC=NEQ FOR REQUIRED                      
         B     FEXIT                                                            
         SPACE 1                                                                
F5       LA    R3,FTBTBLEN(R3)                                                  
         B     F1                                                               
FEXIT    XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
SCHEMFLT EQU   1                   POSITIVE SCHEME CODE FILTER IN USE           
SCHEMLEN EQU   22                  WIDTH OF ELEMENT DISPLAY                     
         LTORG                                                                  
*                                                                               
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACINF14   05/01/02'                                      
         END                                                                    
