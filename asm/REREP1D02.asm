*          DATA SET REREP1D02  AT LEVEL 022 AS OF 05/01/02                      
*PHASE RE1D02C,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREP1D02 - BUDGET ALLOCATION RESETTER'                         
*********************************************************************           
*                                                                   *           
*        REREP1D02 --- REPPAK BUDGET ALLOCATION RESETTER            *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* JAN28/91 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
* FEB13/92 (BU ) --- TURN OFF RESET TO ZERO IF NO RESTART $ FOUND   *           
*                                                                   *           
* FEB27/92 (BU ) --- ALLOW OPTION TO RESET MONTHLY SPREAD $ ONLY.   *           
*                    ALLOW OPTION TO RESET CONTRACT TYPE BUDGET TO  *           
*                    ZERO AS WELL AS RESET MONTHLY SPREAD $.        *           
*                                                                   *           
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                   *           
* JAN14/98 (BU ) --- 4K CONTRACTS REGENALL1 REGENALL1A              *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*      R8  =  THIRD  BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
RE1D02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1D02,R7,R8,RR=RE                                           
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         BZ    MAIN30              ZERO IS GOOD RETURN                          
         B     MAINBAD             NON-ZERO IS ERROR                            
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     MODEEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
INITIAL  NTR1                                                                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
*                                                                               
*   PICK UP 2ND REQUEST CARD                                                    
*                                                                               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXRQNUM                                                       
         CLI   0(R2),2             NEED TWO CARDS                               
         BE    IN002                                                            
         MVC   P(L'CARDMISS),CARDMISS                                           
         B     BADEXIT                                                          
IN002    EQU   *                                                                
         L     R4,VXRQCARD         GET 2ND CARD                                 
         MVC   CARD2,80(R4)                                                     
         DROP  R5                                                               
*                                                                               
*   SET UP SPREAD CONTRACT TYPES                                                
*                                                                               
IN003    EQU   *                                                                
         LA    R2,SPREDCT          A(SPREAD PERCENTAGE ARRAY)                   
         LA    R3,5                NUMBER OF ENTRIES                            
         LA    R4,CARD2+12         A(1ST ENTRY, CARD2)                          
*                                                                               
*  CHECK CARD LAYOUT AS A FUNCTION OF REQUESTOR                                 
*                                                                               
IN004    EQU   *                                                                
         CLI   0(R4),C' '          CONTRACT TYPE ENTERED?                       
         BE    IN008               NO  - FINISHED                               
         MVC   0(1,R2),0(R4)       STORE CONTRACT TYPE                          
         LA    R2,2(R2)            NEXT A(CONTRACT TYPE)                        
         LA    R4,4(R4)            NEXT CARD ENTRY                              
         BCT   R3,IN004            PROCESS NEXT ENTRY                           
IN008    EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         CLI   QRGPROG,C'9'        BUDGET YEAR IN '90'S?                        
         BE    AH006               YES - IGNORE PRIOR TO 90'S                   
         MVC   HEAD1+48(2),=C'20'  NO  - MOVE IN NEXT CENTURY                   
         B     AH008                                                            
AH006    EQU   *                                                                
         MVC   HEAD1+48(2),=C'19'                                               
AH008    EQU   *                                                                
         MVC   HEAD1+50(2),QRGPROG LOAD BUDGET YEAR                             
         B     MODEEXIT                                                         
         EJECT                                                                  
*   RPTDONE:  LAST REQUEST MODE SETTING:                                        
*             RETRIEVE THE BUDGET RECORD                                        
*             FIND THE RESTART BUDGET ELEMENT                                   
*             FIND 1ST REQUEST CONTRACT TYPE IN RESTART ELEMENT                 
*             FOR 1ST CONTRACT TYPE:                                            
*                REESTABLISH THE ALLOCATION $ TO RESTART VALUE                  
*                ZERO OUT BUCKET AMOUNTS                                        
*             FOR ALL OTHER REQUEST CONTRACT TYPES:                             
*                ZERO OUT BUCKET AMOUNTS                                        
*                ZERO OUT ALLOCATION $                                          
*             REWRITE THE RECORD                                                
*                                                                               
RPTDONE  NTR1                                                                   
*                                                                               
         XC    KEY,KEY             ESTABLISH FIRST BUDGET KEY                   
         MVI   KEY,X'13'                                                        
         MVC   KEY+16(2),RCREPFL                                                
         MVC   KEY+18(2),QRGPROG   LOAD START YEAR                              
         OC    QSTATION,QSTATION   ANY STATION ENTERED?                         
         BZ    RD004                                                            
         CLC   QSTATION(5),SPACES  DITTO LAST COMMENT                           
         BE    RD004                                                            
         MVC   KEY+20(5),QSTATION  YES - LOAD INTO KEY                          
*                                                                               
RD004    GOTO1 HIGH                                                             
*                                                                               
RD008    EQU   *                                                                
         CLI   KEY,X'13'           SAME CODE?                                   
         BNE   RD019               NO  - DONE PROCESSING                        
         CLC   KEY+16(2),RCREPFL   SAME REP?                                    
         BNE   RD019               NO  - DONE PROCESSING                        
         CLC   KEY+18(2),QRGPROG   SAME YEAR?                                   
         BNE   RD019               NO  - DONE PROCESSING                        
         OC    QSTATION,QSTATION   ANY STATION ENTERED?                         
         BZ    RD009                                                            
         CLC   QSTATION(5),SPACES  DITTO LAST COMMENT                           
         BE    RD009                                                            
         CLC   KEY+20(5),QSTATION  SAME STATION?                                
         BNE   RD019               NO  - DONE PROCESSING                        
*                                                                               
RD009    EQU   *                                                                
         OC    QOFFICE,QOFFICE     ANY OFFICE ENTERED?                          
         BZ    RD010               NO                                           
         CLC   QOFFICE(2),SPACES   DITTO LAST COMMENT                           
         BE    RD010                                                            
         CLC   KEY+25(2),QOFFICE   SAME OFFICE?                                 
         BNE   RD012               NO  - SKIP THIS BUDGET RECORD                
RD010    EQU   *                                                                
         BAS   RE,GETBUD           RETRIEVE BUDGET RECORD                       
         BAS   RE,RD030            PROCESS THE BUDGET RECORD                    
RD012    EQU   *                                                                
         GOTO1 SEQ                 READ NEXT BUDGET RECORD                      
         B     RD008               PROCESS THIS READ                            
*                                                                               
RD019    B     MODEEXIT            JOB OVER - END IT                            
*                                                                               
RDDUMP   DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*   ROUTINE RESETS ALLOCATION DOLLARS IN THIS MANNER:                           
*     1. ALLOCATION DOLLARS ARE FOUND FOR FIRST BUDGET CONTRACT                 
*            TYPE ON REQUEST IN RESTART ELEMENT                                 
*     2. EACH ADDITIONAL CONTRACT TYPE OF THE REQUEST IS SET TO ZERO            
*                                                                               
DALLOC$  EQU   RBUD$TOT-RBUDELEM                                                
DCONTYP  EQU   RBUDTYPE-RBUDELE2                                                
DALLFLG  EQU   RBUDTAG-RBUDELEM                                                 
DBUDTOT  EQU   RBUDSTOT-RBUDELEM                                                
DBUDVAL  EQU   RBUDSTA-RBUDELEM                                                 
LDBUDVAL EQU   RBUD$TOT-RBUDSTA                                                 
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
RD030    NTR1                                                                   
         LA    R1,SPREDCT          A(1ST CONTRACT TYPE REQUESTED)               
         MVI   ELCODE,X'02'                                                     
         LA    R6,RBUDREC                                                       
         BAS   RE,GETEL                                                         
         BNE   RD038               NO ELEMENT FOR BASIC ALLOCATION              
*                                                                               
RD031    EQU   *                                                                
         CLC   DCONTYP(1,R6),0(R1) CONTRACT TYPE FOUND?                         
         BE    RD032               FOUND - USE ITS ALLOC $                      
         BAS   RE,NEXTEL           NOT FOUND - FIND NEXT                        
         BNE   RD038               NO ELEMENT FOR BASIC ALLOCATION              
         B     RD031                                                            
*                                                                               
RD032    EQU   *                                                                
         XC    ALLOC$,ALLOC$                                                    
         CLI   QOPTION1,C'Z'       ZERO MONTHLY $ ONLY/UPDATE?                  
         BE    RD033               YES                                          
         CLI   QOPTION1,C'$'       ZERO BUDGET ALLOC $       ?                  
         BE    RD032A              YES                                          
         BAS   RE,RD070            SEEK RESTART $ FOR 1ST TYPE                  
         OC    ALLOC$,ALLOC$       ANY VALUE FOUND?                             
         BZ    RD040               NO  - DON'T DO ANYTHING                      
*                                                                               
*   IF NO RESTART VALUE IS FOUND, DON'T CHANGE THE EXISTING VALUES              
*        FOR THE RECORD.  THIS RECORD MAY HAVE BEEN ENTERED AFTER               
*        THE RUN BEING RESET.                                                   
*                                                                               
RD032A   EQU   *                                                                
         MVC   DALLOC$(4,R6),ALLOC$  INSERT RESTART DOLLARS                     
RD033    EQU   *                                                                
         XC    DBUDVAL(LDBUDVAL,R6),DBUDVAL(R6)                                 
         MVI   DALLFLG(R6),C'D'                                                 
         MVC   P+02(5),RBUDKSTA                                                 
         MVC   P+12(2),RBUDKTEM                                                 
         MVC   P+22(1),DCONTYP(R6)                                              
         CLI   QOPTION1,C'Z'       ZERO MONTHLY $ ONLY/UPDATE?                  
         BE    RD033A              YES                                          
         CLI   QOPTION1,C'X'       ZERO MONTHLY $ ONLY/TEST  ?                  
         BNE   RD033B              YES                                          
RD033A   EQU   *                                                                
         MVC   ALLOC$,DALLOC$(R6)  LOAD BUCKET TOTAL ALLOC FOR PRINT            
RD033B   EQU   *                                                                
         EDIT  ALLOC$,(10,P+26),COMMAS=YES  PRINT ALLOCATION $                  
         MVC   P+42(28),=C'MONTHLY VALUES RESET TO ZERO'                        
         GOTO1 REPORT              PRINT LINE                                   
*                                                                               
*    ZERO OUT THE BUDGET FIELDS THROUGH STATION BUDGET TOTAL                    
*        AND SET ALLOCATION FLAG TO 'DEALLOCATED'                               
*                                                                               
*    NOW DO THE OTHER CONTRACT TYPES SPECIFIED ON THE REQUEST                   
*                                                                               
         LA    R5,4                CHECK NEXT FOUR CONTRACT TYPES               
*                                                                               
RD034    EQU   *                                                                
         CLI   QOPTION1,C'Z'       ZERO MONTHLY $ ONLY/UPDATE?                  
         BE    RD046               YES - ONLY PROCESS 1 CONTYPE                 
         LA    R1,2(R1)            NEXT CONTRACT TYPE                           
         CLI   0(R1),C' '          ANY ENTRY?                                   
         BE    RD046               NO  - REWRITE RECORD                         
         LA    R6,RBUDREC          SET BACK TO RECORD START                     
         BAS   RE,GETEL            GET FIRST ELEMENT                            
*                                                                               
RD035    EQU   *                                                                
         CLC   DCONTYP(1,R6),0(R1) CONTRACT TYPE FOUND?                         
         BE    RD036               FOUND                                        
         BAS   RE,NEXTEL           NOT FOUND - FIND NEXT                        
         BNE   RD037               NO ELEMENT FOR TYPE: SKIP IT                 
         B     RD035                                                            
*                                                                               
RD036    EQU   *                                                                
         XC    DBUDVAL(LDBUDVAL,R6),DBUDVAL(R6)                                 
         XC    DALLOC$(4,R6),DALLOC$(R6)                                        
         MVI   DALLFLG(R6),C'D'                                                 
         MVC   P+22(1),DCONTYP(R6)                                              
         MVC   P+42(28),=C'MONTHLY VALUES RESET TO ZERO'                        
         GOTO1 REPORT              PRINT LINE                                   
*                                                                               
*    ZERO OUT THE BUDGET FIELDS THROUGH ALLOCATION AMOUNT                       
*        AND SET ALLOCATION FLAG TO 'DEALLOCATED'                               
*                                                                               
RD037    EQU   *                                                                
         BCT   R5,RD034            GO BACK FOR NEXT CARD FIELD                  
         B     RD046               REWRITE RECORD ON COMPLETION                 
*                                                                               
RD038    EQU   *                                                                
         MVC   P+02(5),RBUDKSTA                                                 
         MVC   P+12(2),RBUDKTEM                                                 
         MVC   P+45(LNCTF),NCTF    INSERT MESSAGE                               
         MVC   P+63(1),0(R1)       INSERT CONTRACT TYPE                         
         GOTO1 REPORT              PRINT LINE                                   
         B     RD059               END OF ROUTINE                               
*                                                                               
NCTF     DC    C'NO CONTRACT TYPE " '                                           
         DC    C'" FOUND FOR THIS STATION/OFFICE: NO ACTION'                    
LNCTF    EQU   *-NCTF                                                           
         DS    0H                                                               
*                                                                               
RD039    EQU   *                                                                
         MVC   P+45(LNA$F),NA$F    INSERT MESSAGE                               
         GOTO1 REPORT              PRINT LINE                                   
         B     RD059               END OF ROUTINE                               
*                                                                               
NA$F     DC    C'NO ALLOCATION DOLLARS'                                         
         DC    C' FOUND FOR THIS STATION/OFFICE: NO SPREAD DONE'                
LNA$F    EQU   *-NA$F                                                           
         DS    0H                                                               
*                                                                               
RD040    EQU   *                                                                
         MVC   P+45(LNR$F),NR$F    INSERT MESSAGE                               
         GOTO1 REPORT              PRINT LINE                                   
         B     RD059               END OF ROUTINE                               
*                                                                               
NR$F     DC    C'NO RESTART DOLLARS'                                            
         DC    C' FOUND FOR THIS STATION/OFFICE: NO ACTION'                     
LNR$F    EQU   *-NR$F                                                           
         DS    0H                                                               
*                                                                               
RD046    EQU   *                                                                
         CLI   QOPTION1,C'T'       TEST RUN                                     
         BE    RD059               YES - NO REWRITE OF RECORD                   
         CLI   QOPTION2,C'T'       TEST RUN                                     
         BE    RD059               YES - NO REWRITE OF RECORD                   
         BAS   RE,PUTBUD                                                        
RD059    EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
*          DATA SET REREP1D02  AT LEVEL 165 AS OF 01/10/91                      
*                                                                               
*  LOOK FOR RESTART ELEMENT.  THEN FIND FIRST REQUESTED CONTRACT                
*    TYPE WITHIN ELEMENT, AND USE ALLOC $ FROM IT.                              
*                                                                               
FRSTRTYP EQU   RBUDRTYP-RBUDELE3                                                
LRTYPBUK EQU   5                                                                
*                                                                               
RD070    NTR1                                                                   
         MVI   ELCODE,X'03'        RETRIEVE RESTART ELT FROM REC                
         LA    R6,RBUDREC                                                       
         BAS   RE,GETEL            LOOK FOR X'03' ELT                           
         BNE   RD079               NOT FOUND - FINISHED                         
         LA    R6,FRSTRTYP(R6)     FOUND - SCAN FOR TYPE                        
         LA    R5,9                LOOP CONTROL                                 
RD071    EQU   *                                                                
         CLI   0(R6),X'00'         TYPE EMPTY (END)?                            
         BE    RD079               YES - FINISHED                               
         CLC   0(1,R6),SPREDCT     SAME AS 1ST CON TYPE?                        
         BE    RD074               YES - USE ALLOCATION DOLLARS                 
         LA    R6,LRTYPBUK(R6)     NO  - BUMP TO NEXT BUCKET                    
         BCT   R5,RD071            DO EACH                                      
         B     RD079               NOT FOUND - FINISHED                         
RD074    EQU   *                                                                
         MVC   ALLOC$(4),1(R6)     RETRIEVE ALLOCATION $                        
         B     RD079               EXIT                                         
RD079    EQU   *                                                                
         MVI   ELCODE,X'02'        RESET ELT CODE FOR GETEL                     
         B     MODEEXIT                                                         
         EJECT                                                                  
GETBUD   LA    RF,RBUDREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
PUTBUD   LA    RF,RBUDREC                                                       
         B     PUTFILE                                                          
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*  DATA MANAGER INTERFACE (CHECK ERRORS)                                        
*                                                                               
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION?               
         BZ    DM020                                                            
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         B     MODEEXIT                                                         
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    MODEEXIT                                                         
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*                                                                               
*   ROUTINE TO TRACE DATA MANAGER CALLS                                         
*                                                                               
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,MTRACDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         SPACE 3                                                                
PUTFILE  NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 PREC                                                             
         B     MODEEXIT                                                         
         SPACE 3                                                                
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
MODEEXIT EQU   *                                                                
         LTR   R0,R0                                                            
MEXIT    EQU   *                                                                
         XIT1                                                                   
*                                                                               
BADEXIT  EQU   *                                                                
         LA    R0,1                SET CC NOT = ZERO                            
         B     MEXIT                                                            
         EJECT                                                                  
*                                                                               
CARDMISS DC    C'SECOND REQUEST CARD MISSING'                                   
*                                                                               
*              WORK SPACE ETC                                                   
UNDRSCOR DS    XL20                 SET TO UNDERSCORE FOR PRINTING              
         SPACE 3                                                                
         SPACE 3                                                                
ASTART   DS    F                                                                
CONFLAG  DS    CL1                                                              
*                                                                               
SPREDCT  DC    XL10'40004000400040004000'   CON TYPES/PERCENTAGES               
PROCCTR  DC    PL4'0'              CONTRACTS PROCESSED CTR                      
SORTCTR  DC    PL4'0'              RECORDS RELEASED TO SORT                     
RETCTR   DC    PL4'0'              RECORDS RETURNED FROM SORT                   
BUDCTR   DC    PL4'0'              BUDGET SORT RECS OUTPUT                      
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
SWAPSTA  DS    CL5                 STATION/OFFICE SWAP AREA                     
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
ACTIVE   DS    CL1                                                              
ELCODE   DS    CL1                                                              
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
SORTCARD DC    CL80'SORT FIELDS=(1,11,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=104'                                   
WORKX    DS    CL150               ELEMENT BUILD AREA                           
WORKY    DS    CL50                ELEMENT BUILD AREA                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
*                                                                               
         DC    C'**CARD2**'                                                     
CARD2    DS    0CL80               2ND REQUEST CARD                             
CBUDFRM  DS    CL6                 'BUDGET-FROM' DATE                           
CBUDTO   DS    CL6                 'BUDGET-TO' DATE                             
CBUDCTYP DS    5CL4                CONTRACT TYPE/PERCENTAGE                     
         DS    CL48                                                             
CARD2L   EQU   *-CARD2                                                          
*                                                                               
         DS    0F                                                               
HUNDRED  DC    F'100'                                                           
HUND25   DC    H'125'                                                           
EIGHTY   DC    H'80'                                                            
FIFTY    DC    H'50'                                                            
ADJUST   DS    H                                                                
*                                                                               
         DS    0F                                                               
ALLOC$   DS    XL4                                                              
         DS    0D                                                               
CONTOTS  DS    0CL16                                                            
         DC    4F'0'                                                            
TOTYEAR  DS    XL4                 YEAR'S TOTAL DOLLARS                         
TMONTBL  DS    0CL48                                                            
         DC    12F'00'             BUCKET ACCUMULATOR                           
*  12 MONTHLY BUCKETS -                                                         
*                                                                               
*  NOT-JOINED ALLOCATION SPREADER ACCUMULATORS:  USED TO ADD UP                 
*     TOTALS BY OFFICE, AS WELL AS OVERALL TOTALS.  IF A STATION                
*     WAS NOT JOINED AT THE START OF THE BILLING CYCLE, THE OFFICE'S            
*     PERCENTAGE FOR THE MONTH OF THE TOTAL COMPANY BILLING WILL BE             
*     USED AS THE ALLOCATION SPREAD.                                            
*                                                                               
*  THIRTEEN 4-BYTE FIELDS WILL BE USED PER OFFICE.  A MAX OF 50 OFFICES         
*     IS PROVIDED FOR.                                                          
*        FIELD 1     =   BYTES 1-2  - OFFICE CODE                               
*                        BYTES 3-4  - NOT USED AT THIS TIME                     
*        FIELD 2-13  =   ACCUMULATORS MONTH 1 - 12                              
*                                                                               
*    FIRST ENTRY WILL BE COMPANY TOTALS                                         
*    NEXT ENTRIES WILL BE OFFICE TOTALS                                         
*                                                                               
ACCTBL   DS    50CL52                                                           
         ORG ACCTBL                                                             
ACCOFF   DS    CL2                 FIRST OFFICE IN TABLE                        
         DS    CL2                 UNUSED TWO BYTES                             
ACCMON1  DS    XL4                 FIRST MONTH'S DATA                           
         DS    XL44                NEXT 11 MONTH'S DATA                         
LACCOFF  EQU   *-ACCOFF                                                         
         ORG                                                                    
NJSTATS  DS    100CL5              100 5-BYTE ENTRIES                           
LNJSTATS EQU   *-NJSTATS                                                        
ENJSTATS DC    CL5'ZZZZZ'                                                       
ANJSTATS DS    F                   A(NEXT AVAILABLE ENTRY)                      
         ORG                                                                    
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022REREP1D02 05/01/02'                                      
         END                                                                    
