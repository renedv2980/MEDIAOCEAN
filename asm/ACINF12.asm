*          DATA SET ACINF12    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T6050CA,*,NOAUTO                                                         
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE NA'                         
T6050C   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE NARRAT'N IN ACCOUNTS INFO PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T6050C)                                               
         DC    A(FILTABLE-T6050C)                                               
         DC    A(PREHEADS-T6050C)                                               
         DC    A(PRETABLE-T6050C)                                               
         DC    A(HEADINGS-T6050C)                                               
         DC    A(DATTABLE-T6050C)                                               
         DC    A(KNTRYPNT-T6050C)                                               
         DC    A(FNTRYPNT-T6050C)                                               
         DC    A(DNTRYPNT-T6050C)                                               
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
         DC    X'41'                                                            
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
FILTABLE DC    CL10'COMMENT'                                                    
         DC    CL2'CO'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ACMM-GWS)                                                    
         DC    AL2(ACOMMENT-ACOMMD)                                             
         DC    AL1(6)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'MEDIA'                                                      
         DC    CL2'ME'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(0)                                                           
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'BILL'                                                        
         DC    X'80'                                                            
         DC    AL2(ACMM-GWS)                                                    
         DC    AL2(ACOMTYPE-ACOMMD)                                             
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'STATUS'                                                     
         DC    CL2'ST'                                                          
         DC    CL8'EST'                                                         
         DC    X'40'                                                            
         DC    AL2(ACMM-GWS)                                                    
         DC    AL2(ACOMTYPE-ACOMMD)                                             
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'DELETED'                                                    
         DC    CL2'DE'                                                          
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(ACSTATUS-ACKEYD)                                             
         DC    AL1(L'ACSTATUS)                                                  
         DC    AL2(0)                                                           
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
         DC    CL39'---------- COMMENT COMMENT'                                 
         DC    CL39'           CODE    STATUS'                                  
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
         DC    AL2(AKEY-GWS)       COMMENT NUMBER AND STATUS (DUMMY)            
         DC    AL2(0)                                                           
         DC    AL1(28)                                                          
         DC    X'FF00'             HANDLED BY OVERLAY - INCLUDING EXTRA         
         DC    AL1(52)                                  FILTERING               
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
FNTRYPNT DS    0D                  GET JOB MEDIA CODE FROM KEY (FILTER)         
         NMOD1 4,**NARF**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6050C,RB                                                        
         USING T605TWA,RA                                                       
         SPACE 1                                                                
         C     R2,AKEY             IS IT A MEDIA FILTER                         
         BNE   F2                                                               
         LA    R5,SAVEHIER                                                      
         USING ACHEIRD,R5                                                       
         ZIC   R4,ACHRLEVB                                                      
         L     RE,ADRIO                                                         
         LA    RE,3(RE)                                                         
         AR    R4,RE                                                            
         MVC   WORK(1),0(R4)                                                    
         LA    R2,WORK                                                          
         XIT1  REGS=(R2)                                                        
         DROP  R5                                                               
         SPACE 1                                                                
F2       LA    R5,WORK+1           IT MUST BE A COMMENT NUMBER/STATUS           
         MVI   WORK+1,1            FILTER SO MAKE SURE IT PASSES THE            
         LA    R2,WORK             TEST SO THAT IT CAN BE HANDLED AT            
         MVI   WORK,0              SUB-RECORD LEVEL LATER (QV DNTRYPNT)         
         USING FTBD,R6                                                          
         CLI   FTBSIGN,C'N'                                                     
         BE    *+8                                                              
         MVI   WORK,1                                                           
         LA    R3,1                                                             
         XIT1  REGS=(R2,R5)                                                     
         DROP  R6                                                               
         EJECT                                                                  
DNTRYPNT DS    0D                                                               
         NMOD1 4,**NARD**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6050C,RB                                                        
         L     R9,ACMM                                                          
         USING ACOMMD,R9                                                        
         LA    R4,1                R4 = NUMBER OF ADDITIONAL LINES              
         LNR   R4,R4                    NEEDED - NEGATIVE=SKIP RECORD           
         LA    R3,CMFLDLEN         R3 = DISPLAY FIELD WIDTH PER ELEMENT         
         LA    R2,BINTAB           R2 = A(DISPLAY FIELD)                        
         LR    R5,R2               R5 = DISPLAY FIELD POINTER                   
         SPACE 1                                                                
         LTR   R9,R9               ANY COMMENTS AT ALL                          
         BZ    DXIT                NO                                           
         B     D2                                                               
         SPACE 1                                                                
D1       ZIC   R7,ACOMLEN          BUMP TO NEXT COMMENT ELEMENT                 
         AR    R9,R7                                                            
         CLI   ACOMEL,X'3E'                                                     
         BNE   DXIT                                                             
         SPACE 1                                                                
D2       CLI   ACOMTYPE,0          TYPE MUST BE NONZERO                         
         BE    D1                                                               
         BAS   RE,FILTER                                                        
         BZ    D1                                                               
         MVC   0(CMFLDLEN,R5),SPACES                                            
         LA    R7,ACOMMENT         MOVE IN LEFT-JUSTIFIED COMMENT NO            
         LA    R8,5                                                             
D3       CLI   0(R7),C' '                                                       
         BH    D4                                                               
         LA    R7,1(R7)                                                         
         BCT   R8,D3                                                            
D4       EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R7)                                                    
         SPACE 1                                                                
         LA    R7,8(R5)            AND STATUS                                   
         TM    ACOMTYPE,X'80'                                                   
         BNO   D5                                                               
         MVC   0(4,R7),=C'BILL'                                                 
         LA    R7,5(R7)                                                         
         TM    ACOMTYPE,X'40'                                                   
         BNO   D8                                                               
         MVC   0(5,R7),=C'&& EST'                                               
         LA    R7,6(R7)                                                         
         B     D8                                                               
D5       TM    ACOMTYPE,X'40'                                                   
         BNO   D8                                                               
         MVC   0(3,R7),=C'EST'                                                  
         LA    R7,4(R7)                                                         
         SPACE 1                                                                
D8       AH    R4,=H'1'            INCREMENT COUNT OF EXTRA LINES               
         LA    R5,CMFLDLEN(R5)     BUMP DISPLAY FIELD POINTER                   
         B     D1                                                               
         SPACE 1                                                                
DXIT     MVI   DMCB,0              SET FOR NO ERROR                             
         XIT1  REGS=(R2,R4)        RETURN ADDRESS,LENGTH,NO. EXTRA LNES         
         EJECT                                                                  
*              APPLY FILTERS VIA FILTER TABLE - CC=ZERO IF NOT SATISF'D         
         SPACE 1                                                                
FILTER   NTR1                                                                   
         LA    R3,FILTAB                                                        
         USING FTBD,R3                                                          
         LA    R8,ACMM                                                          
         SPACE 1                                                                
F1       CLI   0(R3),X'FF'                                                      
         BNE   *+10                                                             
         LTR   RB,RB                                                            
         B     FEXIT                                                            
         C     R8,FTBELMNT         IS IT A COMMENT ELEMENT FILTER               
         BNE   F5                  NO - BUMP TO NEXT                            
         LR    R4,R9               R9 = A(COMMENT ELEMENT)                      
         MVC   HALF,FTBDISP                                                     
         AH    R4,HALF             R4 = FIELD ADDRESS                           
         SPACE 1                                                                
         ZIC   R5,FTBLEN           R5 = FIELD LENGTH                            
         LA    R6,FTBVAL           R6 = A(MASK OR COMPARE VALUE)                
         MVC   FULL,BRANCH                                                      
         SR    R1,R1                                                            
         CLI   FTBMARK,C'M'                                                     
         BE    F3                                                               
         CLI   FTBSIGN,C'N'                                                     
         BE    *+8                                                              
         XI    FULL+1,X'F0'        CHANGE TO BNE                                
         BCTR  R5,0                                                             
         MVC   WORK(6),SPACES      RIGHT JUSTIFY COMMENT NO.FILTER VAL          
         LA    R7,WORK+5                                                        
         SR    R7,R5                                                            
         EX    R5,FMOVE                                                         
         CLC   0(6,R4),WORK                                                     
         EX    R1,FULL             BRANCH IF EQUAL/UNEQUAL                      
         B     F5                                                               
FMOVE    MVC   0(0,R7),0(R6)                                                    
         SPACE 1                                                                
F3       CLI   FTBSIGN,C'P'        MASK                                         
         BE    *+8                                                              
         XI    FULL+1,X'F0'                                                     
         ZIC   R7,0(R6)                                                         
         EX    R7,*+8                                                           
         B     *+8                                                              
         TM    0(R4),0                                                          
         EX    R1,FULL             BRANCH IF NONZERO/ZEO                        
         SPACE 1                                                                
F5       LA    R3,FTBTBLEN(R3)                                                  
         B     F1                                                               
BRANCH   BE    F6                  EXECUTED BRANCH                              
*                                   COMPARE/NEGATIVE - FILTER OUT               
*                                   TEST(TM)POSITIVE - FILTER OUT               
F6       SR    R1,R1                                                            
FEXIT    XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
CMFLDLEN EQU   28                  WIDTH OF DISPLAY FIELD PER COMMENT           
         LTORG                                                                  
*                                                                               
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACINF12   05/01/02'                                      
         END                                                                    
