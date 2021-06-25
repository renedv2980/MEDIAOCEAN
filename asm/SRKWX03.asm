*          DATA SET SRKWX03    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T14703A                                                                  
         TITLE '$KWX - OVERLAY FOR ALL REPORT HANDLING FUNCTIONS'               
T14703   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**T14703          R2/R3 = A(OUT/IN FLD HDR IN TWA/TIA)         
         L     RC,0(R1)                                                         
         USING WORKD,RC            RC    = A(GWS)                               
         USING T147FFD,RA          RA    = A(TWA)                               
         USING KWXACD,R7           R7    = A(KWX A/C REC - TYPE=REPORT)         
         SR    R0,R0                                                            
         SPACE 1                                                                
K00      CLI   TYPE,REPORT         IF TYPE=REPORT GO TO PRINT ROUTINE           
         BNE   K01                                                              
         BAS   RE,REPRINT                                                       
         B     KEYXIT2                                                          
         SPACE 1                                                                
K01      CLC   8(6,R3),=C'CHECK='  KEYWORD 'CHECK'                              
         BNE   K02                                                              
         BAS   RE,KEYCHECK                                                      
         B     KEYXIT2                                                          
         SPACE 1                                                                
K02      BAS   RE,KEYREPT          KEYWORD 'REPORT' OR 'KWX'                    
         B     KEYXIT2                                                          
         SPACE 1                                                                
KEYERR   SR    R0,R0               CC = EQU FOR ERROR                           
ANXIT1   XIT1                                                                   
         SPACE 1                                                                
KEYXIT   LTR   RB,RB               CC = POS FOR OK                              
KEYXIT2  XIT1  REGS=(R2,R3)                                                     
         EJECT                                                                  
*              ROUTINE TO HANDLE 'REPORT=SPP,9999,FORCE,PAGE=99-99,             
*              LINES=99-99,DISPLAY,NEWPAGE,SENDER' OR 'KWX=KKK,9999'            
*              ON ENTRY TYPE  = CHECK OR REPORT                                 
*                       SCOPY = Z IF WE'RE LOOKING FOR REPORT EXTRACTS          
*                               TO APPEND TO SENDER'S COPY (CC=EQU=NO)          
*              SEE KEYCALL FOR OTHER PARAMETERS                                 
         SPACE 1                                                                
KEYREPT  NTR1                                                                   
         CLI   TYPE,REPORT         IF WERE CHECKING AND THIS IS NOT THE         
         BE    KR1                 FIRST REPORT=, TRMINATE THE LAST ONE         
         TM    USAGE,REPTEQLS                                                   
         BNO   KR1                                                              
         OI    USAGE,TWORPRTS                                                   
         CLI   PASAVE,TRMINATE                                                  
         BE    KR1                                                              
         MVI   PLA,TRMINATE                                                     
         GOTO1 APSEQ                                                            
         SPACE 1                                                                
KR1      OI    USAGE,REPTEQLS      SCAN INPUT                                   
         XC    SCANBLCK(250),SCANBLCK                                           
         XC    SCANBLCK+250(70),SCANBLCK+250                                    
         GOTO1 ASCANNER,DMCB,(R3),(9,SCANBLCK)                                  
         MVI   FERN,INVALID                                                     
         CLI   DMCB+4,2            MUST BE AT LEAST SPP,9999                    
         BL    KEYERR                                                           
         SPACE 1                                                                
KR2      LA    R8,PLREP            SET UP INITIALISE CALL AND DEFAULT           
         USING PQPLD,R8            CONTROL VALUES                               
         XC    PLREP,PLREP                                                      
         MVI   QLEXTRA,X'FF'                                                    
         MVI   FNDX,1                                                           
         CLI   SCANBLCK+1,3                                                     
         BNE   KEYERR                                                           
         MVC   QLSUBID,SCANBLCK+22                                              
         MVI   FNDX,2                                                           
         TM    SCANBLCK+34,X'80'                                                
         BNO   KEYERR                                                           
         MVC   QLREPNO,SCANBLCK+L'SCANBLCK+6                                    
         MVC   QLSRCID,SAVTUSER                                                 
         SPACE 1                                                                
KR5      MVC   PLA,PLREP           INITIAL READ TO LOCATE                       
         MVC   PLA+2(6),=C'LOCATE'                                              
         GOTO1 APSEQ                                                            
         MVC   PLREP,PLA                                                        
         MVI   FERN,REPTNXST                                                    
         MVI   FNDX,0                                                           
         TM    DMCB+8,X'10'        TEST NOT FOUND                               
         BO    KEYERR                                                           
         MVI   FERN,INVALID                                                     
         CLI   DMCB+8,0                                                         
         BNE   KEYERR                                                           
         SPACE 1                                                                
KR6      MVI   REPSTAT,0           SET DEFAULT REPORT VALUES                    
         MVC   STARTP,=H'1'                                                     
         MVC   ENDP,=H'9999'                                                    
         MVC   STARTL(4),STARTP                                                 
         L     R6,APBUFFA                                                       
         USING PQRECD,R6                                                        
         MVC   SAVPAGES,PQPAGES    EXTRACT LINES AND PAGES                      
         MVC   SAVLPP,PQLPP                                                     
         MVC   SAVLINES,PQLINES+1                                               
         MVC   ENDP,PQPAGES                                                     
         DROP  R6                                                               
         SPACE 1                                                                
KR7      CLI   TYPE,REPORT         HANDLE KWX=KKK,9999                          
         BE    KR8                                                              
         CLC   8(4,R3),=C'KWX='                                                 
         BNE   KR8                                                              
         MVI   FERN,NOTAKWX                                                     
         CLI   QLCLASS,C'K'        KWX                                          
         BNE   KEYERR                                                           
         CLI   QLSUBID,C'S'        SENDER'S COPY                                
         BNE   KEYERR                                                           
         MVI   FERN,INVALID                                                     
KR7A     GOTO1 APSEQ                                                            
         BZ    KEYERR                                                           
         CLI   PLA,PR1SP2          KWX MESSAGE FOLLOWS THIS                     
         BNE   KR7A                                                             
         MVI   REPSTAT,KWXEQLS                                                  
         BAS   RE,REPDISP                                                       
         BZ    KEYERR                                                           
         B     KR17                                                             
         SPACE 1                                                                
KR8      LA    R5,SCANBLCK+64      HANDLE KEYWORDS IN INPUT STRING              
         MVI   FNDX,3                                                           
         SR    R6,R6                                                            
         SPACE 1                                                                
KR9      CLI   0(R5),0                                                          
         BE    KR11                                                             
         LA    R4,12(R5)                                                        
         LA    R1,REPTAB                                                        
         BAS   RE,KEYCALL                                                       
         BP    KR10                                                             
         BZ    KEYERR                                                           
         MVI   FERN,KEYWNXST                                                    
         B     KEYERR                                                           
KR10     IC    R6,FNDX                                                          
         LA    R6,1(R6)                                                         
         STC   R6,FNDX                                                          
         LA    R5,L'SCANBLCK(R5)                                                
         B     KR9                                                              
         SPACE 1                                                                
KR11     CLI   SCOPY,C'Z'          IS THIS THE SENDER'S COPY                    
         BNE   KR12                IF SO DO WE WANT THIS REPORT                 
         TM    REPSTAT,SENDER      APPENDED TO IT                               
         BNO   KEYERR              NO                                           
         SPACE 1                                                                
KR12     MVI   FNDX,0              CHECK FOR TOO MANY LINES                     
         LH    R1,SAVLINES                                                      
         LH    R7,ENDP                                                          
         SH    R7,STARTP                                                        
         LA    R7,1(R7)                                                         
         CH    R7,SAVPAGES                                                      
         BE    KR13                                                             
         ZIC   R0,SAVLPP                                                        
         MR    R6,R0                                                            
         LR    R1,R7                                                            
KR13     MH    R1,DESTNUM                                                       
         C     R1,MAXLINES                                                      
         BNH   KR14                                                             
         CLI   FORCE,C'Y'          OVERRIDE                                     
         BE    KR14                                                             
         MVI   FERN,XCEEDMAX                                                    
         B     KEYERR                                                           
         SPACE 1                                                                
KR14     TM    REPSTAT,LINEQLS     IF LINES SPECIFIED, MUST BE ONE PAGE         
         BNO   KR15                                                             
         CLC   STARTP,ENDP                                                      
         BE    KR15                                                             
         MVC   FNDX,SAVFNDX                                                     
         MVI   FERN,NOSNGLEP                                                    
         B     KEYERR                                                           
         SPACE 1                                                                
KR15     CLI   TYPE,REPORT         MOVE LINE TO OUTPUT FIELD AND BUMP           
         BE    KEYXIT              IN AND OUT                                   
         TM    REPSTAT,DISPLAY                                                  
         BNO   KR16                                                             
         BAS   RE,REPDISP          OR MOVE REQUESTED REPORT SUBSET TO           
         BZ    KEYERR              OUTPUT IF KEYWORD 'DISPLAY' PRESENT          
         B     KR17                                                             
KR16     BAS   RE,MOVEIT                                                        
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
KR17     IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     KEYXIT                                                           
         SPACE 1                                                                
REPTAB   DS    0CL12               KEYWORD TABLE                                
         DC    CL7'FORCE  ',AL1(1),AL4(SRFORCE)                                 
         DC    CL7'PAGE   ',AL1(1),AL4(SRPAGE)                                  
         DC    CL7'LINES  ',AL1(1),AL4(SRLINES)                                 
         DC    CL7'DISPLAY',AL1(1),AL4(SRDISP)                                  
         DC    CL7'NEWPAGE',AL1(1),AL4(SRNEWP)                                  
         DC    CL7'SENDER ',AL1(3),AL4(SRSENDER)                                
         DC    X'00'                                                            
         EJECT                                                                  
*              SUBROUTINES TO HANDLE KEYWORDS IN THE 'REPORT=' STRING           
*              ON ENTRY R5 = A(SCANBLCK ENTRY)                                  
*              ON EXIT  CC = EQU IF ERROR                                       
*                       CC = POS IF OK                                          
         SPACE 1                                                                
SRFORCE  MVI   FORCE,C'Y'                                                       
         LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 1                                                                
SRPAGE   NTR1                                                                   
         LA    R1,STARTP                                                        
         BAS   RE,STARTEND                                                      
         BZ    KEYERR                                                           
         MVI   FERN,NOVERMAX                                                    
         CLC   SAVPAGES,ENDP                                                    
         BL    KEYERR                                                           
         B     SROKXIT                                                          
         SPACE 1                                                                
SRLINES  NTR1                                                                   
         LA    R1,STARTL                                                        
         BAS   RE,STARTEND                                                      
         BZ    KEYERR                                                           
         MVI   FERN,NOVERMAX                                                    
         CLC   SAVLPP,ENDL+1                                                    
         BL    KEYERR                                                           
         OI    REPSTAT,LINEQLS                                                  
         MVC   SAVFNDX,FNDX                                                     
         B     SROKXIT                                                          
         SPACE 1                                                                
SRDISP   OI    REPSTAT,DISPLAY                                                  
         CLI   22(R5),C'R'         DISPLAY RHS 80 COLS                          
         BNE   *+8                                                              
         OI    REPSTAT,RHS                                                      
         LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 1                                                                
SRNEWP   OI    REPSTAT,NEWPAGE                                                  
         LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 1                                                                
SROKXIT  LTR   RB,RB                                                            
         B     ANXIT1                                                           
         SPACE 1                                                                
SRSENDER OI    USAGE,SENDER                                                     
         OI    REPSTAT,SENDER                                                   
         CLI   SCOPY,C'Z'                                                       
         BE    *+8                                                              
         LTR   RB,RB                                                            
         BR    RE                                                               
         OI    REPSTAT,NEWPAGE                                                  
         CLI   USAGE,REPTEQLS+SENDER    DONT APPEND WHOLE REPORT TO             
         BR    RE                       SENDER'S COPY                           
         EJECT                                                                  
*              ROUTINE TO VALIDATE 'PAGE=' OR 'LINE=' IN A 'REPORT='            
*              STRING                                                           
*              ON ENTRY R1 = A(2 HALFWORDS FOR OUTPUT BINARY START/END)         
*                       R5 = A(SCANBLCK ENTRY)                                  
*              ON EXIT  CC = EQU IF ERROR AND FERN IS SET                       
         SPACE 1                                                                
STARTEND NTR1                                                                   
         OI    USAGE,ANY           TO INDICATE REPORT SUBSET                    
         TM    3(R5),X'80'         SINGLE NUMBER                                
         BNO   SE2                                                              
         MVC   0(2,R1),10(R5)                                                   
         MVC   2(2,R1),10(R5)                                                   
         B     SROKXIT                                                          
         SPACE 1                                                                
SE2      LA    R3,22(R5)           OTHERWISE IT MUST BE 99-99 SO LOOK           
         LA    R2,1                FOR HYPHEN                                   
         LNR   R2,R2                                                            
         ZIC   R4,1(R5)                                                         
         AR    R4,R3                                                            
         BCTR  R4,0                                                             
         LR    R6,R4                                                            
         MVI   FERN,NONUMERC                                                    
         CLI   0(R4),C'-'                                                       
         BE    *+12                                                             
         BXH   R4,R2,*-8                                                        
         B     KEYERR              NO HYPHEN                                    
         SR    R4,R3                                                            
         BNP   KEYERR              NO PRECEDING NUMBER                          
         BCTR  R4,0                                                             
         BAS   RE,NUMVAL                                                        
         BZ    KEYERR              NOT NUMERIC                                  
         LA    R3,2(R4,R3)                                                      
         SR    R6,R3                                                            
         BM    KEYERR              NO FOLLOWING NUMBER                          
         LR    R4,R6                                                            
         LA    R1,2(R1)                                                         
         BAS RE,NUMVAL                                                          
         BZ    KEYERR              NOT NUMERIC                                  
         SH    R1,=H'2'                                                         
         MVI   FERN,STGTREND                                                    
         CLC   0(2,R1),2(R1)                                                    
         BH    KEYERR              START GREATER THAN END                       
         B     SROKXIT                                                          
         SPACE 1                                                                
NUMVAL   MVC   WORK(5),=C'00000'   R1=A(OUTPUT AREA),R3=A(EBCDIC INPUT)         
         EX    R4,MOVEZONE         R4=EXECUTE LENGTH                            
         CLC   WORK(5),=C'00000'                                                
         BE    *+8                                                              
         SR    RF,RF                                                            
         BR    RE                  NOT NUMERIC                                  
         EX    R4,PACKIT                                                        
         CVB   RF,DUB                                                           
         STH   RF,0(R1)                                                         
         LTR   RB,RB                                                            
         BR    RE                                                               
MOVEZONE MVZ   WORK(0),0(R3)                                                    
PACKIT   PACK  DUB,0(0,R3)                                                      
         EJECT                                                                  
*              ROUTINE TO HANDLE 'CHECK=YES/RHS'                                
*              SEE KEYCALL FOR PARAMETERS                                       
         SPACE 1                                                                
KEYCHECK NTR1                                                                   
         MVI   FERN,NOREPRE                                                     
         TM    USAGE,REPTEQLS                                                   
         BNO   KEYERR                                                           
         LA    R1,L'KWXDATA                                                     
         LR    R5,R2                                                            
         SR    R5,R1                                                            
         CLC   0(7,R5),=C'REPORT='  PRECEDING LINE MUST BE 'REPORT='            
         BNE   KEYERR                                                           
         MVI   FERN,INVALID                                                     
         CLI   6(R4),C'Y'          CHECK=YES OR LHS OR RHS                      
         BE    KC2                                                              
         CLI   6(R4),C'L'                                                       
         BE    KC2                                                              
         CLI   6(R4),C'R'                                                       
         BNE   KEYERR                                                           
         OI    REPSTAT,RHS                                                      
         SPACE 1                                                                
KC2      OI    USAGE,CHKEQLS       CURSOR MUST POINT TO FIRST CHECK             
         ST    R2,ACURSOR          DISPLAY LINE                                 
         OI    REPSTAT,CHKEQLS                                                  
         BAS   RE,REPDISP          DISPLAY AS MANY LINES AS FIT                 
         BZ    KEYERR                                                           
         L     R3,ATABHED          DONT LOOK FOR MORE INPUT                     
         B     KEYXIT                                                           
         EJECT                                                                  
*              ROUTINE TO DISPLAY A REPORT SUBSET ON THE SCREEN                 
*              (USED FOR 'KWX=', 'REPORT=...,DISPLAY' AND 'CHECK=')             
*              ON ENTRY R2      = A(1ST OUTPUT HEADER IN TWA)                   
*                       PLA     = PRINT LINE BUFFER - FOR 'KWX=' THIS           
*                                 CONTAINS 1ST LINE TO BE DISPLAYED -           
*                                 IN ALL CASES INITIAL READ IS COMPLETE         
*                       REPSTAT = CALL STATUS BYTE                              
*                                 KWXEQLS - 'KWX=' CALL                         
*                                 CHKEQLS - 'CHECK=' CALL                       
*                                 DISPLAY - 'REPORT=...,DISPLAY' CALL           
*                                 RHS     - RHS OF PRINT LINE REQUIRED          
*                       STARTP,ENDP,STARTL,ENDL SPECIFY PAGES/LINES             
*              ON EXIT  R2      = A(NEXT OUTPUT HEADER IN TWA)                  
*                       CC      = EQU IF ERROR                                  
         SPACE 1                                                                
REPDISP  NTR1                                                                   
         OI    USAGE,DISPLAY       (FORCES RETURN OF SCRN FOR CHECKING)         
         LA    R3,PLAH             FUDGE R3 SO WE CAN USE MOVEIT AS IF          
         MVI   PLAH,X'FF'          R3 ADDRESSED A FIELD HEADER                  
         SR    R6,R6               R6 = PAGE                                    
         LA    R5,1                R5 = LINE                                    
         TM    REPSTAT,KWXEQLS                                                  
         BNO   *+8                                                              
         LA    R6,1                                                             
         SR    R0,R0                                                            
         SPACE 1                                                                
RED2     MVI   FERN,INVALID        LOOP TO READ A PRINT LINE                    
         GOTO1 APSEQ                                                            
         BZ    KEYERR                                                           
         CLI   PLA,TRMINATE                                                     
         BE    REDX                                                             
         TM    REPSTAT,KWXEQLS                                                  
         BNO   *+12                                                             
         CLI   PLA,SP1             IF ITS A KWX WE STOP HERE                    
         BE    RED8                                                             
         CH    R6,STARTP           CHECK PAGE AND LINE IN RANGE                 
         BL    RED7                                                             
         CH    R6,ENDP                                                          
         BH    RED8                                                             
         CH    R5,STARTL                                                        
         BL    RED6                                                             
         CH    R5,ENDL                                                          
         BH    RED8                                                             
         TM    REPSTAT,KWXEQLS     IF ITS NOT A KWX SPACE LINES DONT            
         BO    *+14                COUNT                                        
         CLC   PLA+1(132),SPACES                                                
         BE    RED7                                                             
         MVI   FERN,NOROOM                                                      
         CLI   0(R2),9             IF WEVE REACHED THE TAB FIELD ITS AN         
         BNE   RED4                ERROR UNLESS ITS 'CHECK='                    
         TM    REPSTAT,CHKEQLS                                                  
         BO    RED8                                                             
         B     KEYERR                                                           
         SPACE 1                                                                
RED4     TM    REPSTAT,RHS         DISPLAY A LINE (LHS OR RHS)                  
         BNO   *+10                                                             
         MVC   PLA+1(78),PLA+55                                                 
         BAS   RE,MOVEIT                                                        
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         SPACE 1                                                                
RED6     CLC   PLA+1(132),SPACES   UPDATE PAGE AND LINE COUNTS                  
         BE    *+8                                                              
         LA    R5,1(R5)                                                         
RED7     CLI   PLA,HEADOF                                                       
         BL    RED2                                                             
         LA    R6,1(R6)                                                         
         LA    R5,1                                                             
         B     RED2                                                             
         SPACE 1                                                                
RED8     MVI   PLA,TRMINATE                                                     
         GOTO1 APSEQ                                                            
REDX     LTR   RB,RB                                                            
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              ROUTINE TO PRINT A REPORT OR A SUBSET OF ONE                     
         SPACE 1                                                                
REPRINT  NTR1                                                                   
         SR    R6,R6               R6 = PAGE                                    
         LA    R5,1                R5 = LINE                                    
         CLI   SCOPY,C'Z'                                                       
         BE    REP1                                                             
         TM    USAGE,TWORPRTS      INITIALISE IF NECESSARY                      
         BNO   REP2                                                             
REP1     LR    R3,R2                                                            
         BAS   RE,KEYREPT                                                       
         BNZ   REP4                                                             
         B     REPERR                                                           
REP2     CLI   FIRSTID,C'N'                                                     
         BNE   REP4                                                             
         MVC   PLA,PLREP                                                        
         SPACE 1                                                                
REP3     GOTO1 APSEQ                                                            
         BZ    REPERR                                                           
         SPACE 1                                                                
REP4     MVC   PLB,PLA             INITIALISE PRINT IF ITS JUST A               
         CLI   PLB,INITIAL         COMPLETE REPORT                              
         BNE   REP5                                                             
*        CLI   USAGE,REPTEQLS                                                   
*        BNE   *+14                                                             
*        MVC   PLB+(QLSRCID-PQPLD)(2),QLSRCID                                   
*        B     REP8                                                             
         TM    REPSTAT,NEWPAGE     OTHERWISE START WITH A THROW TO TOP          
         BNO   REP3                OF FORM IF REQUESTED                         
         TM    PBSAVE,HEADOF                                                    
         BO    REP3                UNLESS WEVE JUST DONE ONE                    
         MVC   PLB,SPACES                                                       
         MVI   PLB,HEADOF                                                       
         GOTO1 APRINT                                                           
         B     REP3                                                             
         SPACE 1                                                                
REP5     CLI   PLB,TRMINATE        TERMINATE IN THAT CASE TOO                   
         BNE   REP6                                                             
*        CLI   USAGE,REPTEQLS                                                   
*        BE    REP8                                                             
         B     REP14                                                            
         SPACE 1                                                                
REP6     CH    R6,STARTP           CHECK PAGE AND LINE IN RANGE                 
         BL    REP10                                                            
         CH    R6,ENDP                                                          
         BH    REP12                                                            
         CH    R5,STARTL                                                        
         BL    REP9                                                             
         CH    R5,ENDL                                                          
         BH    REP12                                                            
         SPACE 1                                                                
REP7     CLI   FIRSTID,C'Y'        FIRST TIME COUNT CHARS/LINES/PAGES           
         BNE   REP8                                                             
         GOTO1 AACCOUNT                                                         
         SPACE 1                                                                
REP8     GOTO1 APRINT              PRINT                                        
         CLI   PLA,TRMINATE                                                     
         BE    REPXIT                                                           
         SPACE 1                                                                
REP9     CLC   PLA+1(132),SPACES   BUMP LINE AND PAGE                           
         BE    REP10                                                            
         LA    R5,1(R5)                                                         
REP10    CLI   PLA,HEADOF                                                       
         BL    REP3                                                             
         LA    R6,1(R6)                                                         
         LA    R5,1                                                             
         B     REP3                                                             
         SPACE 1                                                                
REP12    MVI   PLA,TRMINATE        TERMINATE                                    
         GOTO1 APSEQ                                                            
         SPACE 1                                                                
REP14    TM    REPSTAT,NEWPAGE     THROW TO TOP OF FORM IF REQUESTED            
         BNO   REPXIT                                                           
         TM    PBSAVE,HEADOF                                                    
         BO    REPXIT                                                           
         MVC   PLB,SPACES                                                       
         MVI   PLB,HEADOF                                                       
         GOTO1 APRINT                                                           
         B     REPXIT                                                           
         SPACE 1                                                                
REPERR   CLI   PASAVE,TRMINATE                                                  
         BE    REPERR2                                                          
         MVI   PLA,TRMINATE                                                     
         GOTO1 APSEQ                                                            
REPERR2  SR    RC,RC                                                            
REPXIT   LTR   RC,RC                                                            
         B     ANXIT1                                                           
         EJECT                                                                  
*              ROUTINE TO MOVE A LINE FROM THE TIA TO THE TWA AND               
*              TRANSMIT IT IF NECESSARY, CHECKING FOR POSSIBLE UNEQUAL          
*              LINE LENGTHS                                                     
*              ON ENTRY R2 = A(OUTPUT FIELD HEADER IN TWA)                      
*                       R3 = A(INPUT FIELD HEADER IN TIA)                       
         SPACE 1                                                                
MOVEIT   ZIC   RF,0(R3)                                                         
         CLC   0(1,R3),0(R2)                                                    
         BNH   *+8                                                              
         IC    RF,0(R2)                                                         
         SH    RF,=H'9'                                                         
         EX    RF,MOMVC                                                         
         EX    RF,MOOC                                                          
         BZR   RE                                                               
         OI    6(R2),X'80'                                                      
         CLC   8(7,R2),=C'REPORT='                                              
         BER   RE                                                               
         OI    USAGE,ANY                                                        
         BR    RE                                                               
MOMVC    MVC   8(0,R2),8(R3)                                                    
MOOC     OC    8(0,R2),8(R2)                                                    
         SPACE 3                                                                
*              GENERAL ROUTINE TO CALL SUBROUTINES TO HANDLE KEYWORDS           
*              ON ENTRY R1 = A(TABLE TO DRIVE PROCESS) - SEE DSECT KWD          
*                       R2 = A(OUTPUT FIELD HEADER IN TWA)                      
*                       R3 = A(INPUT FIELD HEADER IN TIA)                       
*                       R4 = A(FIELD)                                           
*              ON EXIT  CC = EQU IF ERROR - FERN IS SET                         
*                       CC = POS IF OK    - R2/R3 BUMPED IF APPROPRIATE         
*                       CC = NEG IF NO MATCH FOUND                              
         SPACE 1                                                                
KEYCALL  NTR1                                                                   
         SR    RF,RF                                                            
         USING KWD,R1                                                           
KC1      CLI   0(R1),0                                                          
         BNE   *+10                                                             
         LNR   R1,R1                                                            
         B     KEYXIT2                                                          
         IC    RF,KWEXLEN                                                       
         EX    RF,KEYCLC                                                        
         BE    *+12                                                             
         LA    R1,KWDL(R1)                                                      
         B     KC1                                                              
         RELOC (RE)                                                             
         L     RF,KWASUBR                                                       
         AR    RF,RE                                                            
         BASR  RE,RF                                                            
         B     KEYXIT2                                                          
KEYCLC   CLC   KWORD(0),0(R4)                                                   
         DROP  R1                                                               
         EJECT                                                                  
MAXLINES DC    F'1000'                                                          
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
*SRKWXDSECT                                                                     
       ++INCLUDE SRKWXDSECT                                                     
         EJECT                                                                  
*SRKWXACD                                                                       
       ++INCLUDE SRKWXACD                                                       
         EJECT                                                                  
*SRKWXFFD                                                                       
       ++INCLUDE SRKWXFFD                                                       
         EJECT                                                                  
*DMPRTQL                                                                        
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
*DMPRTQD                                                                        
       ++INCLUDE DMPRTQD                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRKWX03   05/01/02'                                      
         END                                                                    
