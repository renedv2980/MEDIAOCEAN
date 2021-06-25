*          DATA SET ACINF10    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T6050AA,*,NOAUTO                                                         
*INCLUDE BINSRCH                                                                
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE RU'                         
T60510   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE  RULES   IN ACCOUNTS INFO PROGRAM         
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60510)                                               
         DC    A(FILTABLE-T60510)                                               
         DC    A(PREHEADS-T60510)                                               
         DC    A(PRETABLE-T60510)                                               
         DC    A(HEADINGS-T60510)                                               
         DC    A(DATTABLE-T60510)                                               
         DC    A(KNTRYPNT-T60510)                                               
         DC    A(FNTRYPNT-T60510)                                               
         DC    A(DNTRYPNT-T60510)                                               
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
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'O'                                                             
         DC    C' '                                                             
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
FILTABLE DC    CL10'MEDIA'                                                      
         DC    CL2'ME'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ARUL-GWS)                                                    
         DC    AL2(MYRLMED-MYRULED)                                             
         DC    AL1(L'MYRLMED)                                                   
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'MEDIA'                                                      
         DC    CL2'ME'                                                          
         DC    CL8'ALL'                                                         
         DC    AL1(0)                                                           
         DC    AL2(ARUL-GWS)                                                    
         DC    AL2(MYRLMED-MYRULED)                                             
         DC    AL1(L'MYRLMED)                                                   
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'WORKCODE'                                                   
         DC    CL2'WO'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(ARUL-GWS)                                                    
         DC    AL2(MYRLWORK-MYRULED)                                            
         DC    AL1(L'MYRLWORK)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'WORKCODE'                                                   
         DC    CL2'WO'                                                          
         DC    CL8'ALL'                                                         
         DC    AL1(0)                                                           
         DC    AL2(ARUL-GWS)                                                    
         DC    AL2(MYRLWORK-MYRULED)                                            
         DC    AL1(L'MYRLWORK)                                                  
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
PREHEADS DC    CL39'CODE AND NAME='                                             
         DC    CL39' '                                                          
*                                                                               
*              PRE-HEADING DATA TABLE -  9 BYTE ENTRIES                         
*                                                                               
*              CONTENTS AS FOR DISPLAY DATA TABLE BELOW                         
*                                                                               
PRETABLE DC    AL2(AKEY-GWS)       COMPANY CODE                                 
         DC    AL2(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(EDITCOMP-GWS)                                                
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(AKEY-GWS)       ACCOUNT CODE IF ANY                          
         DC    AL2(1)                                                           
         DC    AL1(14)                                                          
         DC    AL2(0)                                                           
         DC    AL1(19)                                                          
         DC    AL1(0)                                                           
*                                                                               
         DC    AL2(ANAM-GWS)       COMPANY OR ACCOUNT NAME                      
         DC    AL2(ACNMNAME-ACNAMED)                                            
         DC    AL1(L'ACNMNAME)                                                  
         DC    AL2(EDITNAME-GWS)                                                
         DC    AL1(35)                                                          
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
*&&UK                                                                           
HEADINGS DC    CL39'SOURCE MED W/C COMM  TAX   SOURCE MED W'                    
         DC    CL39'------------------------   ------------'                    
         DC    CL39'/C COMM  TAX   SOURCE MED W/C COMM  TAX'                    
         DC    CL39'------------   ------------------------'                    
*&&                                                                             
*&&US                                                                           
HEADINGS DC    CL39'SOURCE MED W/C COMM        SOURCE MED W'                    
         DC    CL39'-------------------        ------------'                    
         DC    CL39'/C COMM        SOURCE MED W/C COMM'                         
         DC    CL39'-------        -------------------'                         
*&&                                                                             
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD (DUMMY)              
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       DUMMY DATA TABLE TO FORCE ENTRY TO           
         DC    AL2(0)               OVERLAY                                     
         DC    AL1(78)                                                          
         DC    X'FF00'                                                          
         DC    AL1(27)                                                          
         DC    AL1(53)                                                          
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
*              SPECIAL DISPLAY DATA TABLE FOR RULES                             
*              COVERED BY DSECT RDATTABD WHICH DIFFERS FROM DATTABD             
*              1. EDIT S/R DISPLACEMENT IS FROM START OF OVERLAY                
*              2. ADDITIONAL BYTE FOR START POSITION ON SCREEN FOR 3-UP         
         SPACE 1                                                                
RULFIELD DS    0C                                                               
*                                                                               
         DC    AL2(ARUL-GWS)       SOURCE (AGY/CLI/PRO/JOB)                     
         DC    AL2(ACRLLEN-ACRULED)                                             
         DC    AL1(1)                                                           
         DC    AL2(EDITSRCE-T60510)                                             
         DC    AL1(2)                                                           
         DC    AL1(29)                                                          
         DC    AL1(56)                                                          
*                                                                               
         DC    AL2(ARUL-GWS)       MEDIA CODE                                   
         DC    AL2(ACRLMED-ACRULED)                                             
         DC    AL1(L'ACRLMED)                                                   
         DC    AL2(EDITMEWC-T60510)                                             
         DC    AL1(9)                                                           
         DC    AL1(36)                                                          
         DC    AL1(63)                                                          
*                                                                               
         DC    AL2(ARUL-GWS)       WORK CODE                                    
         DC    AL2(ACRLWORK-ACRULED)                                            
         DC    AL1(L'ACRLWORK)                                                  
         DC    AL2(EDITMEWC-T60510)                                             
         DC    AL1(13)                                                          
         DC    AL1(40)                                                          
         DC    AL1(67)                                                          
*                                                                               
         DC    AL2(ARUL-GWS)       COMMISSION RATE                              
         DC    AL2(ACRLCOMM-ACRULED)                                            
         DC    AL1(L'ACRLCOMM)                                                  
         DC    AL2(EDITCOMM-T60510)                                             
         DC    AL1(17)                                                          
         DC    AL1(44)                                                          
         DC    AL1(71)                                                          
*                                                                               
*&&UK                                                                           
         DC    AL2(ARUL-GWS)       TAX RATE                                     
         DC    AL2(ACRLTAX-ACRULED)                                             
         DC    AL1(L'ACRLTAX)                                                   
         DC    AL2(EDITTAX-T60510)                                              
         DC    AL1(24)                                                          
         DC    AL1(51)                                                          
         DC    AL1(78)                                                          
*&&                                                                             
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
KNTRYPNT DS    0D                  ROUTINE TO SORT OUT COMPANY/ACCOUNT          
         NMOD1 0,**RULK**          KEY - CALLED FROM LEDGER KEY ENTRY           
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60510,RB                                                        
         CLI   SCANBLCK,C' '       IF USER HAS NOT INPUT A KEY                  
         BNE   RXIT                IT IS A COMPANY-LEVEL ENQUIRY                
         MVI   KEY+1,C' '          SO CLEAR THE UNIT CODE IN THE KEY            
         LA    R2,SPACES           AND ENSURE THE LEDGER CODE IS SPACE          
         B     RXIT                                                             
         EJECT                                                                  
FNTRYPNT DS    0D                  ROUTINE TO SAVE RULES ELEMENTS FROM          
         NMOD1 4,**RULF**          HIGHER LEVEL ACCOUNT RECORDS IN A            
         LR    R9,RC               CORE TABLE                                   
         USING LOCALD,R9                                                        
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60510,RB                                                        
         L     R5,ADRIOB                                                        
         BAS   RE,SAVRULES                                                      
         B     RXIT                                                             
         EJECT                                                                  
DNTRYPNT DS    0D                                                               
         NMOD1 300,**RULD**                                                     
         LR    R9,RC                                                            
         USING LOCALD,R9                                                        
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60510,RB                                                        
         USING T605TWA,RA                                                       
         USING LINED,R6                                                         
         XC    LOCALD(LOCALEN),LOCALD                                           
         LA    R6,LINELEN(R6)                                                   
         ST    R6,AFRSTLIN         A(1ST LINE DATA HEADER)                      
         LH    R1,LINE                                                          
         BCTR  R1,0                                                             
         STH   R1,LINE                                                          
         CLC   INFKEY(4),=C'NEXT'                                               
         BE    RU010                                                            
         XC    RULCOUNT,RULCOUNT                                                
         B     RU010                                                            
         EJECT                                                                  
*              IF NOT AT COMPANY LEVEL READ COMPANY RECORD AND SAVE ITS         
*              RULES                                                            
         SPACE 3                                                                
RU010    CLI   LEVEL+1,0                                                        
         BE    RU030                                                            
         MVC   KEYB,SPACES                                                      
         MVC   KEYB(1),MYCO                                                     
         GOTO1 AREAD                                                            
         MVI   ERROR,NOTVLCDE                                                   
         BZ    RERRX                                                            
         MVC   HALF,LEVEL                                                       
         XC    LEVEL,LEVEL         SET LEVEL FOR COMPANY                        
         L     R5,ADRIOB                                                        
         BAS   RE,SAVRULES                                                      
         MVC   LEVEL,HALF                                                       
         SPACE 3                                                                
*              IF AT JOB LEVEL BUILD A FILTAB ENTRY FOR THE JOB MEDIA           
         SPACE 1                                                                
RU020    CLI   LEVEL+1,3                                                        
         BNE   RU030                                                            
         LA    R4,FILTAB                                                        
         USING FTBD,R4                                                          
         CLI   0(R4),X'FF'         FIND END OF FILTER TABLE                     
         BE    *+12                                                             
         LA    R4,FTBTBLEN(R4)                                                  
         B     *-12                                                             
         LA    R3,ARUL             EXTEND TABLE WITH NEW ENTRY FOR              
         ST    R3,FTBELMNT         MEDIA=JOBMEDIA                               
         LA    R3,MYRLMED-MYRULED                                               
         STH   R3,FTBDISP                                                       
         MVI   FTBLEN,1                                                         
         MVI   FTBMARK,C'C'        COMPARE                                      
         LA    R5,SAVEHIER                                                      
         USING ACHEIRD,R5                                                       
         ZIC   R7,ACHRLEVB                                                      
         L     RE,ADRIO                                                         
         LA    RE,3(RE)                                                         
         AR    R7,RE                                                            
         MVC   FTBVAL(1),0(R7)     JOB MEDIA                                    
         MVI   FTBSIGN,C'P'        POSITIVE                                     
         XC    FTBSR,FTBSR                                                      
         MVI   FTBD+FTBTBLEN,X'FF' NEW TERMINATOR                               
         DROP  R5                                                               
         EJECT                                                                  
*              SAVE RULES AND VALUES OF POSITIVE MEDIA OR W/C FILTERS           
         SPACE                                                                  
RU030    L     R5,ADRIO            SAVE RULES                                   
         BAS   RE,SAVRULES                                                      
         MVI   FILTMED,X'FF'       DEFAULT VALUES OF POSITIVE FILTERS           
         MVI   FILTWORK,X'FF'                                                   
         LA    R4,FILTAB                                                        
         LA    R2,MYRLMED-MYRULED  R2 = FTBDISP FOR MEDIA FILTER                
         LA    R3,MYRLWORK-MYRULED R3 = DO      FOR W/C                         
         SPACE 1                                                                
RU031    CLI   0(R4),X'FF'                                                      
         BE    RU040                                                            
         CLI   FTBSIGN,C'P'        POSITIVE                                     
         BNE   RU036                                                            
         CH    R2,FTBDISP                                                       
         BNE   RU033                                                            
         MVC   FILTMED,FTBVAL                                                   
         B     RU036                                                            
RU033    CH    R3,FTBDISP                                                       
         BNE   RU036                                                            
         MVC   FILTWORK,FTBVAL                                                  
RU036    LA    R4,FTBTBLEN(R4)                                                  
         B     RU031                                                            
         DROP  R4                                                               
         EJECT                                                                  
*              READ TABLE OF MODIFIED RULES ELEMENTS IN MEDIA/WORKCODE/         
*              REVERSE LEVEL SEQUENCE.                                          
*              CREATE A TABLE OF RE-MODIFIED RULES ELEMENTS IN LOCAL WS         
*              SELECTED FROM THE FIRST BY THE FOLLOWING CRITERIA:               
*              1 MUST SATISFY THE FILTERS IF ANY                                
*              2 MUST NOT HAVE MEDIA/WORKCODE FOR WHICH A LOWER-LEVEL           
*                RULE EXISTS, INCLUDING A LOWER LEVEL RULE OF WIDER             
*                SCOPE IE'ALL/ALL'AT JOB LEVEL OVERRIDES'A/01'AT CLIENT         
*                LEVEL                                                          
         SPACE 3                                                                
RU040    LA    R2,BINTAB                                                        
         USING MYRULED,R2                                                       
         LA    R3,LOCTAB                                                        
         USING ACRULED,R3                                                       
         CLI   0(R2),0                                                          
         BE    RU080                                                            
         LR    R4,R3               R4 = POINTER TO NEXT LOCTAB SLOT             
         SPACE 1                                                                
RU041    ST    R2,ARUL             FILTER                                       
         BAS   RE,FILTER                                                        
         BZ    RU050                NOT REQUIRED                                
         LA    R3,LOCTAB                                                        
         SPACE 1                                                                
RU043    CLI   0(R3),0                                                          
         BE    RU048                                                            
         CLC   ACRLMED,MYRLMED     HAVE WE ALREADY FOUND A RULE FOR             
         BL    RU045               THIS MEDIA/ALL WORKCODE                      
         BH    RU048                                                            
         CLI   ACRLWORK,0                                                       
         BE    RU046                                                            
RU045    CLI   ACRLMED,0           OR FOR ALL MEDIA/THIS WORKCODE               
         BNE   RU047                                                            
         CLC   ACRLWORK,MYRLWORK                                                
         BNE   RU047                                                            
RU046    CLC   ACRLLEN,MYRLLEV     AT A LOWER LEVEL                             
         BL    RU050               IF SO WE DONT WANT THIS ONE                  
         CLC   MYRLMED,FILTMED     AT SAME/HIGHER LEVEL                         
         BNE   RU046A                                                           
         CLC   ACRLWORK,MYRLWORK   WITH FILTER ON MEDIA & MATCHING W/C          
         BE    RU046B                                                           
         B     RU047                                                            
RU046A   CLC   MYRLWORK,FILTWORK   OR FILTER ON W/C & MATCHING MEDIA            
         BNE   RU047                                                            
         CLC   ACRLMED,MYRLMED                                                  
         BNE   RU047                                                            
RU046B   MVI   ACRLEL,C'N'         HIGHER LEVEL NOT REQUIRED                    
         B     RU048                                                            
RU047    LA    R3,LOCTABL(R3)                                                   
         B     RU043                                                            
         SPACE 1                                                                
RU048    LR    R3,R4               RESTORE POINTER TO END OF LOCTAB             
         MVC   0(LOCTABL,R3),0(R2) MOVE AN ENTRY INTO LOCTAB                    
         MVC   WORK(4),ACRLLEN     REFORMAT IT (LEVEL TO ACRLLEN)               
         MVC   ACRLLEN,WORK+3                                                   
         MVC   ACRLMED,WORK                                                     
         MVC   ACRLWORK,WORK+1                                                  
         LA    R3,LOCTABL(R3)      BUMP POINTER                                 
         MVI   0(R3),0                                                          
         LR    R4,R3                                                            
         SPACE 1                                                                
RU050    MVC   LASTMEWC(3),MYRLMED BUMP TO NEXT MEDIA/WORKCODE                  
RU051    LA    R2,LOCTABL(R2)                                                   
         CLI   0(R2),0                                                          
         BE    RU060                                                            
         CLC   LASTMEWC(3),MYRLMED                                              
         BE    RU051                                                            
         B     RU041                                                            
         EJECT                                                                  
*              CHECK THROUGH TABLE IN LOCAL WS FROM LOWEST LEVEL/MOST           
*              SPECIFIC RULE BACK TO HIGHEST LEVEL/MOST GENERAL.                
*              MARK EACH RULE AS REQUIRED ('R' IN ACRLEL).                      
*              THEN CHECK WHETHER IT COMPLETELY SATISFIES THE ENQUIRY,          
*              TAKING POSITIVE FILTERS INTO ACCOUNT.                            
*              IF IT DOES, TERMINATE THE SEARCH                                 
         SPACE 3                                                                
RU060    LA    R5,LOCTAB           IF R3 (LOCTAB ENTRY POINTER) POINTS          
         LR    R3,R4                                                            
         CR    R3,R5               TO START OF LOCTAB                           
         BE    RU080               NO RULES TO DISPLAY                          
         SPACE 1                                                                
         BCTR  R5,0                R3/4/5= BXH REGISTERS FOR BACKWARDS          
         LA    R4,LOCTABL                  SEARCH OF LOCTAB                     
         LNR   R4,R4                                                            
         STM   R3,R5,DMCB          SAVE                                         
         ZIC   R2,REVLEV           R2/0/1= BXLE REGISTERS FOR LEVEL             
         LA    R0,1                        CONTROL LOOP                         
         LA    R1,3                                                             
         SPACE 1                                                                
RU061    LM    R3,R5,DMCB          RESET BXH REGISTER                           
RU062    BXH   R3,R4,RU063                                                      
         BXLE  R2,R0,RU061         UPDATE LEVEL NUMBER                          
         B     RU070                                                            
         SPACE 1                                                                
RU063    STC   R2,WORK             IS ENTRY FOR REQUIRED LEVEL                  
         CLC   ACRLLEN,WORK                                                     
         BNE   RU062               NO - BUMP UP TO NEXT                         
         CLI   ACRLEL,C'N'         NOT REQUIRED                                 
         BE    RU062                                                            
         MVI   ACRLEL,C'R'         YES- MARK ENTRY AS REQUIRED                  
         SPACE 1                                                                
         CLC   ACRLMED(3),FILTMED  DOES ENTRY SATISFY FILTERS COMPLTELY         
         BE    RU070                                                            
         OC    ACRLMED(2),ACRLMED  OR IS IT ALL/ALL                             
         BZ    RU070                                                            
         MVI   WORK,0                                                           
         MVC   WORK+1(2),FILTWORK                                               
         CLC   WORK(3),ACRLMED     OR ALL/FILTER WORKCODE                       
         BE    RU070                                                            
         MVC   WORK(1),FILTMED                                                  
         MVI   WORK+1,0                                                         
         CLC   WORK(2),ACRLMED     OR FILTER MEDIA/ALL                          
         BE    RU070               STOP SEARCH                                  
         B     RU062               OTHERWISE BUMP UP TO NEXT                    
         EJECT                                                                  
*              FIND NEXT RULES ELEMENT TO DISPLAY AND SET UP DISPLAY            
         SPACE 3                                                                
RU070    LA    R3,LOCTAB                                                        
         SR    R4,R4                                                            
RU071    CLI   0(R3),0                                                          
         BE    RU080                                                            
         CLI   0(R3),C'R'          REQUIRED                                     
         BNE   RU076               BUMP                                         
         CLI   SWITCH,0            LOOKING FOR FIRST TO DISPLAY THISTME         
         BNE   RU073                                                            
         CH    R4,RULCOUNT                                                      
         BE    RU072                                                            
         LA    R4,1(R4)                                                         
         B     RU076                                                            
RU072    MVI   SWITCH,1                                                         
RU073    CLC   LINE,=H'20'                                                      
         BNE   RU074                                                            
         CLI   TWOUP,2             3-UP ALL USED                                
         BE    RU082                                                            
         ZIC   R1,TWOUP                                                         
         LA    R1,1(R1)                                                         
         STC   R1,TWOUP                                                         
         MVC   LINE,FRSTLINE                                                    
RU074    CLC   LINE,FRSTLINE                                                    
         BNE   RU075                                                            
         LH    R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         L     R6,AFRSTLIN                                                      
RU075    OI    LINEHDR+6,X'80'                                                  
         BAS   RE,SETLINE                                                       
         LH    R1,RULCOUNT                                                      
         LA    R1,1(R1)                                                         
         STH   R1,RULCOUNT                                                      
RU076    LA    R3,LOCTABL(R3)                                                   
         B     RU071                                                            
*                                                                               
*              COME HERE IF NO (OR NO MORE) RULES ELEMENTS TO DISPLAY           
*                                                                               
RU080    OC    RULCOUNT,RULCOUNT                                                
         BNZ   RU081                                                            
         OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA(19),=C'NO RULES TO DISPLAY'                             
RU081    L     RE,ADRIO                                                         
         MVI   41(RE),X'FF'        CHANGE KEY                                   
         XC    RULCOUNT,RULCOUNT                                                
*                                                                               
*              COME HERE FOR ALL RETURNS TO ROOT                                
*                                                                               
RU082    LA    R2,SPACES           RETURN DUMMY PRINT FIELD & LENGTH            
         LA    R3,1                                                             
         MVI   DMCB,0                                                           
         B     *+8                                                              
RERRX    MVI   DMCB,1                                                           
RXIT     XIT1  REGS=(R2,R3)                                                     
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
*              APPLY FILTERS VIA FILTER TABLE - CC=ZERO IF NOT SATISF'D         
*                                                                               
FILTER   NTR1                                                                   
         LA    R3,FILTAB                                                        
         USING FTBD,R3                                                          
F1       CLI   0(R3),X'FF'                                                      
         BNE   *+10                                                             
         LTR   RB,RB                                                            
         B     FEXIT                                                            
         MVC   FULL,FTBELMNT                                                    
         L     R4,FULL                                                          
         L     R4,0(R4)                                                         
         LTR   R4,R4                                                            
         BZ    F6                                                               
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
*              SET UP A DISPLAY PART-LINE FROM A RULES ELEMENT                  
*              R3 = A(RULES ELEMENT IN LOCTAB)                                  
*               R6 = A(LINE ON SCREEN)                                          
         SPACE 1                                                                
SETLINE  NTR1                                                                   
         LA    R4,RULFIELD                                                      
         USING RDATTABD,R4                                                      
         LR    R5,R3                                                            
         USING ACRULED,R5                                                       
         SPACE 1                                                                
SL1      CLI   0(R4),X'FF'                                                      
         BE    SL4                                                              
         LR    R2,R5                                                            
         MVC   HALF,RDATDISP                                                    
         AH    R2,HALF             R2 = FIELD ADDRESS                           
         SR    R3,R3                                                            
         IC    R3,RDATLEN                                                       
         BCTR  R3,0                R3 = FIELD LENGTH MINUS 1                    
         OC    RDATEDIT,RDATEDIT                                                
         BE    SL2                 IS THERE AN EDIT S/R FOR THIS FIELD          
         MVC   HALF,RDATEDIT                                                    
         LH    RF,HALF             GET DISPL FROM START OF OVERLAY              
         AR    RF,RB                                                            
         BASR  RE,RF               IF SO PASS IT R2 AND R3 TO EDIT AND          
SL2      LA    R7,LINEHDR+7         RETURN ADDRESS IN R2 AND L-1 IN R3          
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         IC    R1,TWOUP            4-UP (0,1,2 OR 3)                            
         IC    RF,RDATSTRT(R1)                                                  
         AR    R7,RF               R7 = START POSITION IN DATA LINE             
         BCTR  R7,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R2)       MOVE FIELD INTO LINE                         
SL3      LA    R4,RDATBLEN(R4)                                                  
         B     SL1                                                              
*                                                                               
SL4      LH    R1,LINE             UPDATE LINE COUNT AND SCREEN POINTER         
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         LA    R6,LINELEN(R6)                                                   
         XIT1  REGS=(R6)                                                        
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
*              EDIT RULE SOURCE FOR DISPLAY                                     
*                                                                               
EDITSRCE NTR1                                                                   
         ZIC   R4,0(R2)                                                         
         MH    R4,=H'3'                                                         
         LA    R2,SRCETAB(R4)                                                   
         LA    R3,2                                                             
         XIT1  REGS=(R2,R3)        R2 = ADDRESS, R3 = L-1                       
SRCETAB  DC    C'JOBPROCLIAGY'                                                  
*                                                                               
*              EDIT MEDIA OR WORK CODE FOR DISPLAY                              
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
EC1      TM    ACRLSTAT,X'80'                                                   
         BO    EC4                 4DP                                          
         EDIT  (P4,ACRLCOMM),(6,WORK+20),2                                      
         LA    R3,7                                                             
         B     ECX                                                              
EC4      EDIT  (P4,ACRLCOMM),(7,WORK+20),4,DROP=1                               
         LA    R3,6                                                             
ECX      LA    R2,WORK+20                                                       
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
*              SAVE MODIFIED RULES IN BINTAB IN GLOBAL WS                       
*              THE FORMAT OF THE RULES ELEMENT IS MODIFIED TO                   
*              ELEMENT CODE/MEDIA/WORKCODE/REVERSE LEVEL (REVLEV) ETC.          
*              THE MODIFIED ELEMENT IS COVERED BY DSECT MYRULED QV.             
*              ELEMENTS ARE SAVED IN MEDIA/WORKCODE/REVLEV SEQUENCE             
*              REVLEV = 0(JOB)/1(PRO)/2(CLI)/3(AGY)                             
*              ON ENTRY LEVEL    = HALFWORD CONTAINING LEVEL NUMBER             
*                       BINCOUNT = COUNT OF ELEMENTS SAVED SO FAR               
*                       R5       = A(RECORD)                                    
         SPACE 1                                                                
SAVRULES NTR1                                                                   
         LA    R1,3                CALCULATE AND STORE REVERSE LEVEL            
         SH    R1,LEVEL                                                         
         STC   R1,REVLEV                                                        
         LA    R5,49(R5)                                                        
         SR    R6,R6                                                            
         L     R7,BINCOUNT                                                      
SR1      IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         CLI   0(R5),0                                                          
         BE    SREND                                                            
         CLI   0(R5),X'42'                                                      
         BNE   SR1                                                              
         C     R7,MAXRECS                                                       
         BNL   SREND                                                            
         USING ACRULED,R5                                                       
         MVC   WORK(11),ACRLEL                                                  
         CLI   ACRLLEN,X'0B'                                                    
         BNL   *+8                                                              
         MVI   WORK+10,0                                                        
         MVC   WORK+1(3),ACRLMED                                                
         MVC   WORK+4(1),REVLEV                                                 
         L     R3,MAXRECS                                                       
         GOTO1 =V(BINSRCH),DMCB,(1,WORK),BINTAB,(R7),11,(1,4),         X        
               (R3),RR=RB                                                       
         L     R7,DMCB+8                                                        
         B     SR1                                                              
SREND    LA    R4,BINTAB                                                        
         ST    R7,BINCOUNT                                                      
         MH    R7,=H'11'                                                        
         AR    R4,R7                                                            
         MVI   0(R4),0                                                          
         XIT1                                                                   
         SPACE 1                                                                
MAXRECS  DC    F'200'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                  DSECT TO COVER LOCAL WORKING STORAGE                         
         SPACE 1                                                                
LOCALD   DSECT                                                                  
AFRSTLIN DS    F         X         A(FIRST DATA LINE)                           
SWITCH   DS    C         X         0=LOOKING FOR NEXT RULE TO DISPLAY           
REVLEV   DS    C         X         REVERSE LEVEL NUMBER (0=JOB,3=AGY)           
FILTMED  DS    C         C         POSITIVE MEDIA FILTER VALUE OR X'FF'         
FILTWORK DS    CL2       C         DITTO WORKCODE                               
LASTMEWC DS    CL3       C         LAST MEDIA/WORKCODE                          
LOCTAB   DS    C         V         START OF TABLE OF RULES FOR DISPLAY          
LOCALEN  EQU   *-LOCALD                                                         
         SPACE 3                                                                
*                  DSECT TO COVER RULES DATA TABLE ENTRY                        
         SPACE 1                                                                
RDATTABD DSECT                                                                  
RDATELMT DS    CL2       X         SEE DATTABD IN ROOT LISTING FOR              
RDATDISP DS    CL2       X         FIELD DEFINITIONS EXCEPT FOR -               
RDATLEN  DS    CL1       X                                                      
RDATEDIT DS    CL2       X         DISPLACEMENT FROM START OF OVERLAY           
RDATSTRT DS    CL1       X                                                      
RDAT2UP  DS    CL1       X                                                      
RDAT3UP  DS    CL1       X         NEW FIELD                                    
RDATBLEN EQU   *-RDATTABD                                                       
         SPACE 3                                                                
*                  DSECT TO COVER MODIFIED RULES ELEMENT IN BINTAB              
         SPACE 1                                                                
MYRULED  DSECT                                                                  
MYRLEL   DS    CL1       X         ELEMENT CODE X'42'                           
MYRLMED  DS    CL1       V         MEDIA CODE (X'00' = ALL)                     
MYRLWORK DS    CL2       V         WORK CODE (X'00' = ALL)                      
MYRLLEV  DS    CL1       X         REVERSE LEVEL NUMBER (JOB=0,AGY=3)           
MYRLCOMM DS    PL4       P         COMMISSION RATE (2 DEC PLACES)               
MYRLTAX  DS    CL1       X         TAX RATE CODE                                
MYRLSTAT DS    CL1       X         STATUS                                       
LOCTABL  EQU   *-MYRULED                                                        
         SPACE 3                                                                
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACINF10   05/01/02'                                      
         END                                                                    
