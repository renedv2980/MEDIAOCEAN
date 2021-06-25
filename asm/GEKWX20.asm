*          DATA SET GEKWX20    AT LEVEL 007 AS OF 05/01/02                      
*PHASE TF2020A,+0                                                               
*INCLUDE GETBOOK                                                                
         TITLE '$KWX - OVERLAY FOR ALL KEYWORD HANDLING FUNCTIONS'              
         PRINT NOGEN               TYPE=CHECK - ADD, CHANGE                     
KWX20    CSECT                     OVERLAY IS CALLED BY THESE ACTIONS -         
         NMOD1 0,**KX20**          TYPE=REPORT- SEND,ADDSEND,PRINT              
         L     RC,0(R1)            OVERLAY RETURNS CC AS FOLLOWS -              
         USING GWS,RC              EQU        - ERROR                           
         USING TWAD,RA             NEG        - (CHECK ONLY) KEYWORDS           
         USING KWXACD,R7                        HAVE CAUSED TEXT TO             
         LA    R9,2048(RB)                      EXPAND                          
         LA    R9,2048(R9)         POS        - OTHER OK                        
         USING KWX20+4096,R9                                                    
         ST    RB,ABASE3                                                        
         ST    R9,A2NDBAS3                                                      
         LA    R1,PSEQ                                                          
         ST    R1,APSEQ                                                         
         SR    R0,R0                                                            
         SPACE 1                                                                
K00      CLI   TYPE,CHECK                                                       
         BE    T070                                                             
         CLI   TYPE,REPORT                                                      
         BE    REP0                                                             
         SPACE 1                                                                
KEYERR   SR    R0,R0               EXITS                                        
         B     ANXIT1              CC = EQU IF ERROR                            
KEYOKX   LTR   RB,RB                                                            
         B     ANXIT1              CC = POS IF OK                               
KEYMOREX LNR   RB,RB                                                            
ANXIT1   XIT1                      CC = NEG IF OK AND MESSAGE CHANGED           
         SPACE 1                                                                
KEYXIT   LTR   RB,RB               CC = POS + RETURN UPDATED R2/R3              
KEYXIT2  XIT1  REGS=(R2,R3)                                                     
         EJECT                                                                  
*              CHECK DEFAULT MESSAGE AREA FOR KEYWORDS, VALIDATE THEM           
*              AND GENERATE TEXT IF REQUIRED                                    
         SPACE 1                                                                
T070     LA    R2,KWXDATAH         MOVE TEXT LINES + HEADERS TO TIA             
         L     R3,ATIA                                                          
         MOVE  ((R3),2000),(R2)                                                 
         MVI   1(R3),0             2260 INPUT TRANSLATOR REQUIREMENT            
         SR    R4,R4                                                            
         SR    R6,R6                                                            
         SPACE 1                                                                
T071     CLI   5(R2),0             CLEAR THEM IN TWA                            
         BE    T072                                                             
         IC    R6,5(R2)                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         MVI   USAGE,ANY                                                        
T072     ZIC   R4,0(R2)                                                         
         AR    R2,R4                                                            
         CLI   0(R2),8+8           BUMP AGAIN IF REFNUM                         
         BH    T071                                                             
         CLI   0(R2),0                                                          
         BNE   T072                                                             
         ST    R2,ASCRLAST                                                      
         SR    R2,R4                                                            
         ST    R2,ACURSOR                                                       
         LA    R2,KWXDATAH                                                      
         CLI   USAGE,0                                                          
         MVI   USAGE,0                                                          
         MVI   ERRORMK,C'N'                                                     
         BNE   T075                                                             
         ST    R2,ACURSOR                                                       
         MVI   FERN,MISSING                                                     
         B     KEYERR                                                           
         SPACE 1                                                                
T075     CLI   ERRORMK,C'Y'        PROCESS A TEXT LINE                          
         BE    T076                IF IT STARTS WITH A KEYWORD CALL THE         
         LA    R1,KEYWTAB          RELEVANT SUBROUTINE, OTHERWISE JUST          
         LA    R4,8(R3)            MOVE IT BACK FROM THE TIA TO THE TWA         
         MVI   FNDX,0                                                           
         BAS   RE,KEYCALL                                                       
         BM    T076                NO KEYWORD                                   
         BNZ   *+8                 KEYWORD PRESENT - OK                         
         MVI   ERRORMK,C'Y'        KEYWORD PRESENT - ERROR                      
         CLI   PASAVE,TRMINATE     DO A TERMINATE PQ READ IF NEC                
         BE    T075A                                                            
         MVI   PLA,TRMINATE                                                     
         GOTO1 APSEQ                                                            
T075A    CLI   ERRORMK,C'Y'                                                     
         BNE   T077                                                             
         ST    R2,ACURSOR          ERROR - SO ALL SUBSEQUENT LINES ARE          
T076     BAS   RE,MOVEIT           MOVE BACK UNCHANGED                          
         SR    R0,R0                                                            
T076A    IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R2),0             BUMP AGAIN IF SHORT FLD (REFNO)              
         BE    T080                                                             
         CLI   0(R2),8+8                                                        
         BL    T076A                                                            
T077     CLI   0(R3),0             HAVE WE REACHED THE END                      
         BE    T080                                                             
         CLI   0(R2),0                                                          
         BNE   T075                                                             
         SPACE 1                                                                
T080     CLI   ERRORMK,C'Y'                                                     
         BE    KEYERR                                                           
         TM    USAGE,TEXTEQLS+CHKEQLS+DISPLAY                                   
         BNZ   T085                                                             
         LA    R2,KWXACTH                                                       
         ST    R2,ACURSOR                                                       
         B     KEYOKX                                                           
T085     MVC   KWXHEAD(L'INVITE),INVITE                                         
         B     KEYMOREX                                                         
         SPACE 1                                                                
INVITE   DC    CL45'MODIFIED INPUT DISPLAYED - PRESS ENTER TO ADD'              
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
         BR    RE                                                               
MOMVC    MVC   8(0,R2),8(R3)                                                    
MOOC     OC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
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
         L     RF,KWASUBR                                                       
         RELOC                                                                  
         AR    RF,RE                                                            
         BASR  RE,RF                                                            
         B     KEYXIT2                                                          
KEYCLC   CLC   KWORD(0),0(R4)                                                   
         DROP  R1                                                               
         SPACE 3                                                                
KEYWTAB  DS    0CL12                                                            
         DC    CL7'TEXT=',AL1(4),AL4(KEYTEXT)                                   
KEYWREP  DC    CL7'REPORT=',AL1(6),AL4(KEYREPT)                                 
         DC    CL7'CHECK=',AL1(5),AL4(KEYCHECK)                                 
KEYWSPAC DC    CL7'SPACE=',AL1(5),AL4(KEYSPACE)                                 
         DC    X'00'                                                            
         EJECT                                                                  
*              PROCESS A 'REPORT=' STRING AT PRINT STAGE                        
         SPACE 1                                                                
REP0     SR    R6,R6               R6 = PAGE                                    
         LA    R5,1                R5 = LINE                                    
REP1     LR    R2,R3               R3 = A(PRINT LINE HOLDING REPORT=)           
         BAS   RE,KEYREPT                                                       
         BNZ   REP4                                                             
         B     REPERR                                                           
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
         BE    KEYERR                                                           
         MVI   PLA,TRMINATE                                                     
         GOTO1 APSEQ                                                            
         B     KEYERR                                                           
REPXIT   B     KEYOKX                                                           
         EJECT                                                                  
*              ROUTINE TO HANDLE 'TEXT=AAAAAAAAAA,NOSHIFT,X=Y' ETC              
*              SEE KEYCALL FOR PARAMETERS                                       
         SPACE 1                                                                
KEYTEXT  NTR1                      SCAN INPUT                                   
         MVI   FERN,INVKWCHA       INVALID KEYWORD IF ACTION CHANGE             
         CLI   ACTION,ADM                                                       
         BNE   KEYERR                                                           
         XC    SCANBLCK(250),SCANBLCK                                           
         XC    SCANBLCK+250(70),SCANBLCK+250                                    
         MVC   CARD,SPACES         USE CARD AS FLDILEN U/S                      
         OC    CARD(72),8(R3)                                                   
         GOTO1 ASCANNER,DMCB,(C'C',CARD),(9,SCANBLCK)                           
         MVI   FERN,INVALID                                                     
         CLI   DMCB+4,0                                                         
         BE    KEYERR                                                           
         MVI   NOSHIFT,C'N'                                                     
         SPACE 1                                                                
KT1      MVI   FNDX,2              IF MORE THAN ONE ENTRY, 2ND AND              
         LA    R5,SCANBLCK+32      SUBSEQUENT ONES ARE KEYWORDS OR              
         SR    R6,R6               SUBSTITUTION REQUEST                         
KT3      OC    0(2,R5),0(R5)                                                    
         BZ    KT4                                                              
         LA    R1,TEXTAB                                                        
         LA    R4,12(R5)                                                        
         BAS   RE,KEYCALL                                                       
         MVI   FERN,INVALID                                                     
         BZ    KEYERR                                                           
         BM    KT3A                NO MATCHING KEYWORD                          
         MVC   2(2,R5),=C'**'      ENTRY+2(2) INDICATES NOT SUBSTITUTES         
         B     KT3B                                                             
KT3A     MVI   FERN,SUBSINVL                                                    
         CLI   1(R5),0                                                          
         BE    KEYERR                                                           
         MVI   2(R5),0             CLEAR BYTE FOR HIT MARKER                    
KT3B     IC    R6,FNDX                                                          
         LA    R6,1(R6)                                                         
         STC   R6,FNDX                                                          
         LA    R5,L'SCANBLCK(R5)                                                
         B     KT3                                                              
         SPACE 1                                                                
KT4      MVI   FNDX,0              GET TEXT FROM CTFILE AND SET IT UP           
         BAS   RE,GETTEXT          IN TWA                                       
         BZ    KEYERR                                                           
         SPACE 1                                                                
KT5      CLI   SCANBLCK+32,0       CHECK FOR NO SUBSTITUTION HITS               
         BE    KT8                                                              
         LA    R5,SCANBLCK+32                                                   
         MVI   FNDX,2                                                           
         MVI   FERN,NOHITS                                                      
KT6      CLI   0(R5),0                                                          
         BE    KT8                                                              
         CLI   2(R5),0                                                          
         BE    KEYERR                                                           
         IC    R6,FNDX                                                          
         LA    R6,1(R6)                                                         
         STC   R6,FNDX                                                          
         LA    R5,L'SCANBLCK(R5)                                                
         B     KT6                                                              
         SPACE 1                                                                
KT8      ZIC   R0,0(R3)            BUMP TIA POINTER                             
         AR    R3,R0                                                            
         CLI   0(R3),8+8           BUMP AGAIN IF REFNUM                         
         BH    *+12                                                             
         CLI   0(R3),0                                                          
         BNE   KT8                                                              
         B     KEYXIT                                                           
         SPACE 1                                                                
TEXTAB   DS    0CL12               KEYWORD TABLE                                
         DC    CL7'NOSHIFT',AL1(6),AL4(SRSHIFT)                                 
         DC    X'00'                                                            
         SPACE 3                                                                
*              SUBROUTINES TO HANDLE KEYWORDS IN THE 'TEXT=' STRING             
*              ON EXIT  CC = EQU IF ERROR                                       
*                       CC = POS IF OK                                          
         SPACE 1                                                                
SRSHIFT  MVI   NOSHIFT,C'Y'        SET INDICATOR THAT SUBSTITUTIONS ARE         
         LTR   RB,RB               TO BE PERFORMED WITHOUT SHIFTS               
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO GET TEXT FROM CTFILE, PERFORM SUBSTITUTIONS           
*              IF ANY AND SET IT UP IN THE TWA                                  
*              ON ENTRY SCANBLCK = SCANNED 'TEXT=A' INPUT                       
*                       R2       = A(1ST OUTPUT FIELD HEADER)                   
*              ON EXIT  CC=EQU   = ERROR                                        
*                       R2       = A(NEXT OUTPUT HEADER) IF NO ERROR            
*                       EACH SCANBLCK ENTRY HAS A SUBSTITUTION HIT              
*                       MARKER AT +2                                            
         SPACE 1                                                                
GETTEXT  NTR1                      BUILD KEY FOR GETBOOK CALL                   
         LA    R7,KEY                                                           
         USING CTCREC,R7                                                        
         XC    CTCKEY,CTCKEY                                                    
         MVI   CTCKTYP,C'C'                                                     
         MVC   CTCKUSER,SAVTKWID                                                
         MVI   CTCKSYS,C'T'                                                     
         MVC   CTCKID,SCANBLCK+22                                               
         OC    CTCKID,SPACES                                                    
         GOTO1 =V(GETBOOK),DMCB,KEY,CARD,ADATAMGR,RR=RB                         
         MVI   FERN,TEXTNFND                                                    
         CLI   DMCB+8,X'10'                                                     
         BE    KEYERR                                                           
         OI    USAGE,TEXTEQLS+ANY                                               
         B     GT2                                                              
         SPACE 1                                                                
GT1      BASR  RE,RF               RE-CALL GETBOOK                              
         CLI   DMCB+8,X'80'        END                                          
         BE    KEYXIT                                                           
GT2      MVI   FERN,INVALID                                                     
         CLI   DMCB+8,0                                                         
         BNE   KEYERR                                                           
         MVI   FERN,NOROOM                                                      
         CLI   0(R2),0             THIS IS END OF SCREEN SO WEVE RUN            
         BE    KEYERR              OUT OF SPACE                                 
         SPACE 1                                                                
GT4      MVC   CARD+72(8),SPACES   CLEAR SEQUENCE NUMBER                        
         BAS   RE,REPLACE          PERFORM SUBSTITUTIONS                        
         ZIC   R4,0(R2)                                                         
         SH    R4,=H'9'                                                         
         EX    R4,GTMVC                                                         
         OI    6(R2),X'80'                                                      
GT5      IC    R4,0(R2)                                                         
         AR    R2,R4                                                            
         CLI   0(R2),8+8           BUMP AGAIN IF REFNUM                         
         BH    GT1                                                              
         CLI   0(R2),0                                                          
         BNE   GT5                                                              
         B     GT1                                                              
GTMVC    MVC   8(0,R2),CARD                                                     
         DROP  R7                                                               
         EJECT                                                                  
*              ROUTINE TO PERFORM SUBSTITUTIONS ON A LINE OF TEXT               
*              ON ENTRY CARD     = LINE OF TEXT - 72 BYTES                      
*                       SCANBLCK = (FROM 2ND ENTRY ON) SUBSTITUTIONS            
*              ON EXIT  EACH SCANBLCK ENTRY CONTAINS A SUBSTITUTION HIT         
*                       MARKER AT +2                                            
         SPACE 1                                                                
REPLACE  CLI   SCANBLCK+32,0                                                    
         BER   RE                                                               
         NTR1                                                                   
         LA    R5,SCANBLCK+32                                                   
         LA    R2,1                                                             
         LA    R3,CARD+L'CARD-1                                                 
         SR    R4,R4                                                            
         SR    R7,R7                                                            
         SPACE 1                                                                
RPL1     CLI   0(R5),0             SEARCH FOR A MATCH                           
         BE    ANXIT1                                                           
         CLI   3(R5),C'*'          NOT A SUBSTITUTION REQUEST                   
         BE    RPL4                                                             
         LA    R1,CARD                                                          
         IC    R4,0(R5)                                                         
         BCTR  R4,0                                                             
RPL2     EX    R4,RPLCLC                                                        
         BE    RPL3                                                             
         BXLE  R1,R2,RPL2                                                       
         B     RPL4                                                             
RPLCLC   CLC   0(0,R1),12(R5)                                                   
         SPACE 1                                                                
RPL3     OI    2(R5),1             SET HIT MARKER AND SUBSTITUTE,               
         LA    R6,1(R1,R4)         SHIFTING REMAINDER OF LINE LEFT              
         MVC   CARD2,SPACES        OR RIGHT IF REQUIRED                         
         MVC   WORK(L'CARD),0(R6)                                               
         EX    R4,RPLSPACE                                                      
         IC    R7,1(R5)                                                         
         BCTR  R7,0                                                             
         EX    R7,RPLMVC                                                        
         LA    R1,1(R1,R7)                                                      
         CLI   NOSHIFT,C'Y'                                                     
         BE    RPL2                                                             
         MVC   0(L'CARD,R1),WORK                                                
         B     RPL2                                                             
RPLMVC   MVC   0(0,R1),22(R5)                                                   
RPLSPACE MVC   0(0,R1),SPACES                                                   
         SPACE 1                                                                
RPL4     LA    R5,L'SCANBLCK(R5)                                                
         B     RPL1                                                             
         EJECT                                                                  
*              ROUTINE TO HANDLE 'SPACE=1/2/3' FOR SINGLE/DOUBLE/TRIPLE         
*              LINE-SPACEING                                                    
*              SEE KEYCALL FOR PARAMETERS                                       
         SPACE 1                                                                
KEYSPACE NTR1                                                                   
         MVI   FERN,INVALID                                                     
         CLI   7(R4),C' '                                                       
         BH    KEYERR                                                           
         CLI   6(R4),C'1'                                                       
         BL    KEYERR                                                           
         CLI   6(R4),C'3'                                                       
         BH    KEYERR                                                           
         B     KEYMOREX                                                         
         EJECT                                                                  
*              ROUTINE TO HANDLE 'REPORT=SPP,9999,FORCE,PAGE=99-99,             
*              LINES=99-99,DISPLAY,NEWPAGE,SENDER' OR 'KWX=KKK,9999'            
*              ON ENTRY TYPE  = CHECK OR REPORT                                 
*                       SCOPY = Z IF WE'RE LOOKING FOR REPORT EXTRACTS          
*                               TO APPEND TO SENDER'S COPY (CC=EQU=NO)          
*              SEE KEYCALL FOR OTHER PARAMETERS                                 
*              BUT NB IF TYPE=REPORT R3 ADDRESSES A CARD-IMAGE INSTEAD          
*              OF A FIELD HEADER                                                
         SPACE 1                                                                
KEYREPT  NTR1                                                                   
         OI    USAGE,REPTEQLS                                                   
         XC    SCANBLCK(250),SCANBLCK                                           
         XC    SCANBLCK+250(70),SCANBLCK+250                                    
         MVC   CARD,SPACES                                                      
         OC    CARD(72),0(R3)                                                   
         CLI   TYPE,REPORT                                                      
         BE    KR1                                                              
         MVC   CARD,SPACES                                                      
         OC    CARD(72),8(R3)                                                   
KR1      LA    R5,CARD                                                          
         GOTO1 ASCANNER,DMCB,(C'C',(R5)),(9,SCANBLCK)                           
         MVI   FERN,INVALID                                                     
         CLI   DMCB+4,2            MUST BE AT LEAST SPP,9999                    
         BL    KEYERR                                                           
         SPACE 1                                                                
KR2      LA    R8,PLREP            SET UP INITIALISE CALL AND DEFAULT           
         USING PQPLD,R8            CONTROL VALUES                               
         XC    PLREP,PLREP                                                      
         MVI   PLCC,INITIAL                                                     
         MVI   FNDX,1                                                           
         CLI   SCANBLCK+1,3                                                     
         BNE   KEYERR                                                           
         MVC   QLSUBID,SCANBLCK+22                                              
         MVI   FNDX,2                                                           
         TM    SCANBLCK+34,X'80'                                                
         BNO   KEYERR                                                           
         MVC   QLREPNO,SCANBLCK+L'SCANBLCK+6                                    
         MVC   QLSRCID,TWAUSRID                                                 
         SPACE 1                                                                
KR5      MVC   PLA,PLREP           INITIAL READ TO LOCATE REPORT                
         MVC   PLA+2(6),=C'LOCATE'                                              
         GOTO1 APSEQ                                                            
         MVC   PLREP,PLA                                                        
         MVI   FERN,REPTNXST                                                    
         MVI   FNDX,0                                                           
         TM    DMCB+8,X'10'                                                     
         BO    KEYERR                                                           
         MVI   FERN,INVALID                                                     
         CLI   DMCB+8,0                                                         
         BNE   KEYERR                                                           
         SPACE 1                                                                
KR6      MVI   REPSTAT,0           SET DEFAULT REPORT VALUES                    
         MVC   STARTP,=H'1'                                                     
         MVC   ENDP,=H'9999'                                                    
         MVC   STARTL(4),STARTP                                                 
         L     R6,ABUFFER                                                       
         USING PQRECD,R6                                                        
         MVC   SAVPAGES,PQPAGES                                                 
         MVC   SAVLPP,PQLPP                                                     
         MVC   SAVLINES,PQLINES+1                                               
         MVC   ENDP,PQPAGES                                                     
         DROP  R6                                                               
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
         CLI   0(R2),8+8                                                        
         BH    KR17                                                             
         CLI   0(R2),0                                                          
         BE    KR17                                                             
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
KR17     IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),8+8                                                        
         BH    KEYXIT                                                           
         CLI   0(R3),0                                                          
         BE    KEYXIT                                                           
         IC    R0,0(R3)                                                         
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
SRDISP   MVI   FERN,INVKWCHA                                                    
         CLI   ACTION,CHM                                                       
         BNE   *+8                                                              
         SR    R0,R0                                                            
         BR    RE                                                               
         OI    REPSTAT,DISPLAY                                                  
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
         BAS   RE,NUMVAL                                                        
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
         MVI   FERN,INVKWCHA                                                    
         CLI   ACTION,ADM                                                       
         BNE   KEYERR                                                           
         LA    R1,L'KWXDATA+8+3    L OF MESSAGE LINE + CHUNK HDR+DATA           
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
         MVC   PLA,PLREP                                                        
         GOTO1 APSEQ               REINITIALISE                                 
         BAS   RE,REPDISP          DISPLAY AS MANY LINES AS FIT                 
         BZ    KEYERR                                                           
         L     R3,ASCRLAST         DONT LOOK FOR MORE INPUT                     
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
         SR    R0,R0                                                            
         SPACE 1                                                                
RED2     MVI   FERN,INVALID        LOOP TO READ A PRINT LINE                    
         GOTO1 APSEQ                                                            
         BZ    KEYERR                                                           
         CLI   PLA,TRMINATE                                                     
         BE    REDX                                                             
         CH    R6,STARTP           CHECK PAGE AND LINE IN RANGE                 
         BL    RED7                                                             
         CH    R6,ENDP                                                          
         BH    RED8                                                             
         CH    R5,STARTL                                                        
         BL    RED6                                                             
         CH    R5,ENDL                                                          
         BH    RED8                                                             
         CLC   PLA+1(132),SPACES                                                
         BE    RED7                                                             
         MVI   FERN,NOROOM                                                      
         CLI   0(R2),0             IF WEVE REACHED END OF SCREEN ITS AN         
         BNE   RED4                ERROR UNLESS ITS 'CHECK='                    
         TM    REPSTAT,CHKEQLS                                                  
         BO    RED8                                                             
         B     KEYERR                                                           
         SPACE 1                                                                
RED4     TM    REPSTAT,RHS         DISPLAY A LINE (LHS OR RHS)                  
         BNO   *+10                                                             
         MVC   PLA+1(78),PLA+55                                                 
         BAS   RE,MOVEIT                                                        
RED5     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),8+8           BUMP AGAIN IF REFNUM                         
         BH    *+12                                                             
         CLI   0(R2),0                                                          
         BNE   RED5                                                             
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
*              DATAMGR CALL TO READ PRINT QUEUE                                 
*              ON EXIT CC = EQU AFTER AN ERROR AND FERN = 0                     
         SPACE 1                                                                
PSEQ     NTR1  BASE=ABASE3                                                      
         L     R9,A2NDBAS3                                                      
         GOTO1 ACLOSE,PARAS,(SAVMODE,0) ENSURE BUFFER IS CLEAR/SAVED            
*                                                                               
PSEQ1    CLI   PLA,0               TEST IF CALL TO LOCATE REPORT                
         BNE   PSEQ2                                                            
         CLC   PLA+2(6),=C'LOCATE'                                              
         BNE   PSEQ2                                                            
         XC    PLB(40),PLB         FIND OUT WHICH PRTQ FILE                     
         MVC   PLB+0(2),PLA+QLSRCID-PQPLD                                       
         GOTO1 ADATAMGR,DMCB,(X'08',GFILE),PRTQUE,PLB,PLB,ABUFFER               
         MVC   PRTQID,PLB+32                                                    
         XC    NDXA,NDXA           CLEAR INDEX FOR LOCATE                       
         MVC   NDXA+0(2),PLA+QLSRCID-PQPLD                                      
         MVC   NDXA+2(3),PLA+QLSUBID-PQPLD                                      
         MVC   NDXA+5(2),PLA+QLREPNO-PQPLD                                      
         GOTO1 ADATAMGR,DMCB,(X'08',INDEX),PRTQID,NDXA,PLA,ABUFFER              
         CLI   DMCB+8,0                                                         
         BNE   PSEQ3A                                                           
         XC    PLA(4),PLA          READ REPORT HEADER INTO PLA                  
         MVI   PLA+4,C'L'                                                       
         GOTO1 ADATAMGR,DMCB,(X'00',RANDOM)                                     
         B     PSEQ3A                                                           
*                                                                               
PSEQ2    CLI   PLA,X'FF'           TEST IF CALL TO TERMINATE REPORT             
         BNE   PSEQ3                                                            
         MVC   PRTQID,=C'PRTQUE'                                                
         GOTO1 ADATAMGR,DMCB,(X'00',BUFF),PRTQID,NDXA,PLA,ABUFFER               
         MVI   PLA,X'FF'                                                        
         MVI   DMCB+8,X'80'        RETURN EOF FOR TERMINATE                     
         B     PSEQ3A                                                           
*                                                                               
PSEQ3    GOTO1 ADATAMGR,DMCB,(X'00',REAF),PRTQID,NDXA,PLA,ABUFFER               
         TM    DMCB+8,X'80'        TEST END OF REPORT                           
         BZ    PSEQ3A                                                           
         XC    PLA,PLA                                                          
         MVI   PLA,X'FF'           SET END OF REPORT IN PLA                     
         MVI   DMCB+8,X'80'        AND ENSURE EOF IS ONLY RETURN CODE           
PSEQ3A   MVC   PASAVE,PLA                                                       
         TM    DMCB+8,X'7F'        TEST FOR ERRORS (EXCEPT EOF)                 
         BZ    KEYOKX                                                           
*                                                                               
PSEQ4    MVI   PASAVE,X'FF'        TERMINATE FLUSH WRITE ON ERROR               
         CLI   PBSAVE,X'FF'                                                     
         BE    PSEQ5                                                            
         MVC   PLB,PLA                                                          
         GOTO1 APRINT                                                           
PSEQ5    MVI   FERN,0                                                           
         B     KEYERR                                                           
         EJECT                                                                  
MAXLINES DC    F'1000'                                                          
PRTQUE   DC    CL8'PRTQUE'                                                      
INDEX    DC    CL8'INDEX'                                                       
REAF     DC    CL8'READ'                                                        
RANDOM   DC    CL8'RANDOM'                                                      
BUFF     DC    CL8'BUFF'                                                        
GFILE    DC    CL8'GFILE'                                                       
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              NESTED INCLUDES                                                  
* SRKWXACD                                                                      
* DMPRTQL                                                                       
* DMPRTQD                                                                       
* CTGENFILE                                                                     
* GEKWXDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE SRKWXACD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DMPRTQD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE GEKWXDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007GEKWX20   05/01/02'                                      
         END                                                                    
