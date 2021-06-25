*          DATA SET REREPCLX   AT LEVEL 069 AS OF 05/01/02                      
*PHASE RECLOSEA,*                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE SORTER                                                                 
*ENTRY RECLOSE                                                                  
         TITLE 'REPPAK - CLOSEOUT'                                              
****************************************************************                
*  HISTORY OF CHANGES:                                         *                
*  AUG08/90 (BU ) --- MAKE PROCESS REP-SENSITIVE.              *                
*                                                              *                
*  AUG01/91 (BU ) --- IGNORE 'KEEP CONTRACT+AVAIL' FLAG TEST   *                
*                                                              *                
*                                                              *                
*  NOV18/92 (BU ) --- ADD 'COMMISSION' AND 'FORECAST' RECORDS  *                
*                     TO CLOSEOUT                              *                
*                                                              *                
*                                                              *                
****************************************************************                
*  PRINT OPTIONS:  CARD:  PRTOPTION=XXXXXXXX                   *                
*                 WHERE   XXXXXXXX  =                          *                
*                     1.  PRTDROPS: DISPLAY RECORDS DROPPED    *                
*                     2.  PRTREADS: PRINT FIRST 100 READ       *                
*                         PRTBOTH : BOTH 1 AND 2               *                
****************************************************************                
*  RECORD SELECTION OPTIONS:                                   *                
*                  CARD:  TYPEFROM=XX                          *                
*                         TYPETO=XX                            *                
*                 WHERE   XX   =   RECORD TYPE CODE            *                
*         EITHER, BOTH, OR NEITHER MAY BE SUBMITTED            *                
****************************************************************                
*  ADDITIONAL BASE REGISTERS:                                  *                
*      R4, R9                                                  *                
****************************************************************                
RECLOSE  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,RECLOSE,=V(REGSAVE),R4,R9                                      
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         USING RECD,R5                                                          
         SPACE 3                                                                
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     RF,=V(STXITER)                                                   
         ST    RF,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
         XC    WORK,WORK                                                        
         SPACE 3                                                                
         MVI   BADCARD,C'N'        SET 'NO CARD ERRORS'                         
         MVC   TITLE+10(17),=C'REP FILE CLOSEOUT'                               
         SPACE 2                                                                
         BAS   R8,TABINIT          INITIALIZE TABLE OF REP SELECTIONS           
         SPACE 1                                                                
NEXTCARD GOTO1 CARDS,DMCB,CARDIN,=C'RE00'                                       
         MVC   P+1(80),CARDIN                                                   
         GOTO1 PRINTER                                                          
         CLC   CARDIN(2),=C'/*'                                                 
         BNE   VALCARD                                                          
         CLI   BADCARD,C'Y'        ANY CARD ERRORS?                             
         BNE   PROCESS             NO  - OKAY TO CONTINUE                       
*                                                                               
         GOTO1 PRINTER                                                          
         MVC   P+1(43),=C'***    CARD INPUT ERRORS ENCOUNTERED    ***'          
         GOTO1 PRINTER                                                          
         MVC   P+1(43),=C'***  PLEASE FIX ERRORS AND RESUBMIT JOB ***'          
         GOTO1 PRINTER                                                          
         MVC   P+1(43),=C'***  DUMP CAUSED BY ERROR(S) IN CARDS   ***'          
         GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         DC    H'0'                BLOW IT UP                                   
         SPACE 3                                                                
VALCARD  EQU   *                                                                
*              VALIDATE INPUT CARDS                                             
*                                                                               
*  RULES FOR INPUT CARDS                                                        
*  1.    INPUT CARDS MAY BE ENTERED FOR 1 OR MORE REPS, PLUS A DEFAULT          
*        SET FOR 'ALL OTHERS'.                                                  
*  2.    EACH SET MUST BEGIN WITH CARD WITH 'REP=XX', WHERE XX IS THE           
*        REP CODE.  ABSENCE OF 'REP=' CARD WILL TERMINATE INPUT WITH            
*        ERROR.                                                                 
*  3.    FOR DEFAULT SET, XX MUST BE SET TO '**'.                               
*  4.    IF DEFAULT SET IS USED, A REP MAY BE EXCLUDED FROM PROCESSING          
*        BY ENTERING ONLY A 'REP=XX' CARD, WHERE XX IS THE REP TO BE            
*        EXCLUDED.                                                              
*  5.    DEFAULT SET MUST BE LAST SET INPUT.  IF OTHER 'REP=' CARDS             
*        ARE ENTERED AFTER THE DEFAULT SET, JOB WILL TERMINATE WITH             
*        AN ERROR.                                                              
*  6.    A SET OF INPUT CARDS WILL CONTROL A SPECIFIC REP.  IF DATA             
*        IS ENCOUNTERED FOR A REP CODE NOT IN THE TABLE, THE DEFAULT            
*        PARAMETERS WILL APPLY, IF ENTERED.  IF DEFAULT PARAMETERS              
*        HAVE NOT BEEN ENTERED, THE DATA WILL BE PASSED THROUGH.                
*  7.    RECORDS (WITH EXCEPTION OF CONTRACTS AND BUYS) MAY BE                  
*        DELETED DIRECTLY (WITHOUT NEEDING A LOAD=RELDPURG RUN) BY              
*        ENTERING A 'LOADTP=YES' CARD.  THIS CARD IS NOT REQUIRED,              
*        BUT VALID ENTRIES FOR IT ARE 'YES' AND 'NO '.                          
*                                                                               
*   NOTE: >>>> R7 CONTROLS THE TABLE INSERTION!!! <<<<                          
*              DON'T CHANGE IT IN THE VALIDATION                                
*                                                                               
*                                                                               
*                                                                               
         SPACE 1                                                                
         CLC   CARDIN(7),=C'LOADTP='                                            
         BE    VALNOLOD                                                         
         CLC   CARDIN(4),=C'REP='                                               
         BE    VALREP                                                           
         L     R7,THISREP          A(SET OF INPUT CARDS)                        
         OC    DREP(2,R7),DREP(R7) 'REP=' CARD MUST BE FIRST                    
         BZ    CARDERR1            NOT PRESENT=ERROR                            
         CLC   CARDIN(10),=C'CONTRACTS='                                        
         BE    VALCON                                                           
         CLC   CARDIN(7),=C'AVAILS='                                            
         BE    VALAVL                                                           
         CLC   CARDIN(10),=C'PROPOSALS='                                        
         BE    VALPRP                                                           
         CLC   CARDIN(10),=C'INVENTORY='                                        
         BE    VALINV                                                           
         CLC   CARDIN(07),=C'BUDGET='                                           
         BE    VALBUD                                                           
         CLC   CARDIN(04),=C'EOM='                                              
         BE    VALEOM                                                           
         CLC   CARDIN(11),=C'COMMISSION='                                       
         BE    VALCOMM                                                          
         CLC   CARDIN(09),=C'FORECAST='                                         
         BE    VALFORE                                                          
         CLC   CARDIN(10),=C'PRTOPTION='                                        
         BE    VALOPT                                                           
         CLC   CARDIN(09),=C'TYPEFROM='                                         
         BE    VALFROM                                                          
         CLC   CARDIN(07),=C'TYPETO='                                           
         BE    VALTO                                                            
         B     CARDERR                                                          
         SPACE 2                                                                
*                                                                               
*   IF 'LOADTP=NO', CLOSED-OUT RECORDS (EXCEPT CONTRACTS AND BUYS)              
*        ARE NOT - REPEAT, NOT - WRITTEN TO THE OUTPUT TAPE TO LATER            
*        BE DROPPED BY THE RELDPURG OPTION.  THEY ARE DIRECTLY DELETED.         
*                                                                               
VALNOLOD EQU   *                                                                
         CLC   CARDIN+7(3),=C'NO '                                              
         BE    NEXTCARD                                                         
         CLC   CARDIN+7(3),=C'YES'                                              
         BNE   CARDERR7                                                         
         MVI   LOADTP,C'Y'                                                      
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALREP   OC    NEXTREP,NEXTREP     ZERO=FIRST TIME                              
         BNZ   VR010                                                            
         LA    R7,REPNTRY+LREPNTRY SET A(TABLE) OF NEXTREP                      
         ST    R7,NEXTREP                                                       
         B     VR020                                                            
VR010    MVC   THISREP,NEXTREP     REPLACE CURRENT WITH NEXT                    
         L     R7,NEXTREP                                                       
         LA    R7,LREPNTRY(R7)     BUMP BY LENGTH                               
         ST    R7,NEXTREP                                                       
VR020    CLC   CARDIN+04(2),=C'  ' NO REP CODE ENTERED                          
         BE    CARDERR2                                                         
         CLC   CARDIN+04(2),=C'**' CHECK FOR DEFAULT REP                        
         BNE   VR022                                                            
         MVI   DEFREP,C'Y'         FLAG 'DEFAULT REP' ENTERED                   
         B     VR024                                                            
VR022    CLI   DEFREP,C'Y'         DEFAULT REP ALREADY ENTERED                  
         BE    CARDERR3            YES - DEFAULT MUST BE LAST SET               
VR024    CP    REPCTR(2),=P'50'    TABLE FULL? 50 MAX!                          
         BE    CARDERR5            YES - NOTIFY USER, STOP JOB                  
         AP    REPCTR(2),=P'1'     NO  - BUMP CTR                               
         L     R7,THISREP          LOAD A(REP IN TABLE)                         
         MVC   DREP(2,R7),CARDIN+4  SAVE INPUT - NO MORE VALIDATION             
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALCON   MVI   SORT,C'Y'                                                        
         LA    R3,CARDIN+10                                                     
         LA    R2,DCONYR(R7)                                                    
         BAS   R8,VALDATE                                                       
         SPACE 1                                                                
         OC    DAVLYR(3,R7),DAVLYR(R7)                                          
         BNZ   CARDERR4                                                         
         MVC   DAVLYR(3,R7),DCONYR(R7)                                          
         OC    DPRPYR(3,R7),DPRPYR(R7)                                          
         BNZ   CARDERR4                                                         
         MVC   DPRPYR(3,R7),DCONYR(R7)                                          
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALAVL   MVI   SORT,C'Y'                                                        
         LA    R3,CARDIN+7                                                      
         LA    R2,DAVLYR(R7)                                                    
         BAS   R8,VALDATE                                                       
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALPRP   MVI   SORT,C'Y'                                                        
         LA    R3,CARDIN+10                                                     
         LA    R2,DPRPYR(R7)                                                    
         BAS   R8,VALDATE                                                       
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALINV   OC    DINVYR(3,R7),DINVYR(R7)                                          
         BNZ   CARDERR                                                          
         GOTO1 DATVAL,DMCB,(0,CARDIN+10),WORK                                   
         CLI   DMCB+3,0                                                         
         BE    CARDERR                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,DINVYR(R7))                              
         GOTO1 DATCON,DMCB,(0,WORK),(3,DINVBY(R7))                              
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALBUD   OC    DBUDYR(2,R7),DBUDYR(R7)                                          
         BNZ   CARDERR                                                          
         MVC   DATECHK+4(2),CARDIN+7      SET UP DATE FOR CHECK                 
         GOTO1 DATVAL,DMCB,(0,DATECHK),WORK                                     
         CLI   DMCB+3,0                                                         
         BE    CARDERR6                                                         
         MVC   DBUDYR(2,R7),CARDIN+07     LOAD YEAR OF BUDGET                   
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALEOM   OC    DEOMYR(1,R7),DEOMYR(R7)                                          
         BNZ   CARDERR                                                          
         MVC   DATECHK+4(2),CARDIN+4      SET UP DATE FOR CHECK                 
         GOTO1 DATVAL,DMCB,(0,DATECHK),WORK                                     
         CLI   DMCB+3,0                                                         
         BE    CARDERR6                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+8)                                  
         MVC   DEOMYR(1,R7),WORK+8        LOAD YEAR OF EOM                      
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALCOMM  EQU   *                                                                
         OC    DCOMMYR(2,R7),DCOMMYR(R7)                                        
         BNZ   CARDERR                                                          
         GOTO1 DATVAL,DMCB,(2,CARDIN+11),WORK                                   
         CLI   DMCB+3,0                                                         
         BE    CARDERR                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+8)                                  
         MVC   DCOMMYR(2,R7),WORK+8    LOAD YM OF COMMISSION                    
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALFORE  EQU   *                                                                
         OC    DFOREYR(2,R7),DFOREYR(R7)                                        
         BNZ   CARDERR                                                          
         GOTO1 DATVAL,DMCB,(0,CARDIN+09),WORK                                   
         CLI   DMCB+3,0                                                         
         BE    CARDERR                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,WORK+8)                                  
         MVC   DFOREYR(2,R7),WORK+8    LOAD COMPRESSED FORECAST DATE            
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALOPT   EQU   *                                                                
         MVC   PRTOPT,CARDIN+10    MOVE OPTION TO STORAGE                       
         CLC   CARDIN+10(08),=C'PRTDROPS'                                       
         BE    NEXTCARD                                                         
         CLC   CARDIN+10(08),=C'PRTREADS'                                       
         BE    NEXTCARD                                                         
         CLC   CARDIN+10(08),=C'PRTBOTH '                                       
         BE    NEXTCARD                                                         
         B     CARDERR                                                          
         SPACE 2                                                                
VALFROM  EQU   *                                                                
         LA    R2,CODETABL         A(RECORD TYPE CODE TABLE)                    
VFRO0010 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    CARDERR                                                          
         CLC   CARDIN+9(2),0(R2)   CARD RECORD TYPE VS TABLE                    
         BE    VFRO0020            TYPE FOUND                                   
         LA    R2,L'CODETABL(R2)   NOT FOUND - BUMP TO NEXT ENTRY               
         B     VFRO0010            GO BACK FOR NEXT                             
VFRO0020 EQU   *                                                                
         MVC   TYPEFROM,2(R2)      MOVE CODE TO TYPE FROM                       
         CLC   TYPEFROM,TYPETO     CHECK FROM/TO SEQUENCE                       
         BNH   NEXTCARD                                                         
         MVC   P+1(34),=C'CARD TYPE FILTERS OUT OF SEQUENCE '                   
         B     CERROR                                                           
         SPACE 2                                                                
VALTO    EQU   *                                                                
         LA    R2,CODETABL         A(RECORD TYPE CODE TABLE)                    
VATO0010 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    CARDERR                                                          
         CLC   CARDIN+7(2),0(R2)   CARD RECORD TYPE VS TABLE                    
         BE    VATO0020            TYPE FOUND                                   
         LA    R2,L'CODETABL(R2)   NOT FOUND - BUMP TO NEXT ENTRY               
         B     VATO0010            GO BACK FOR NEXT                             
VATO0020 EQU   *                                                                
         MVC   TYPETO,2(R2)        MOVE CODE TO TYPE TO                         
         CLC   TYPEFROM,TYPETO     CHECK FROM/TO SEQUENCE                       
         BNH   NEXTCARD                                                         
         MVC   P+1(34),=C'CARD TYPE FILTERS OUT OF SEQUENCE '                   
         B     CERROR                                                           
         EJECT                                                                  
*                                                                               
*   CODETABL:  VALIDATE AND TRANSLATE TABLE FOR RECORD TYPES.                   
*        POSITIONS 1 - 2  =  EBCDIC RECORD TYPE ENTERED VIA CARD                
*        POSITION      3  =  BINARY RECORD TYPE FOR COMPARISON                  
*                                                                               
CODETABL DS    0CL3                                                             
         DC    CL2'01',XL1'01'                                                  
         DC    CL2'02',XL1'02'                                                  
         DC    CL2'03',XL1'03'                                                  
         DC    CL2'04',XL1'04'                                                  
         DC    CL2'05',XL1'05'                                                  
         DC    CL2'06',XL1'06'                                                  
         DC    CL2'07',XL1'07'                                                  
         DC    CL2'08',XL1'08'                                                  
         DC    CL2'09',XL1'09'                                                  
         DC    CL2'0A',XL1'0A'                                                  
         DC    CL2'0B',XL1'0B'                                                  
         DC    CL2'0C',XL1'0C'                                                  
         DC    CL2'0D',XL1'0D'                                                  
         DC    CL2'0F',XL1'0F'                                                  
         DC    CL2'12',XL1'12'                                                  
         DC    CL2'13',XL1'13'                                                  
         DC    CL2'14',XL1'14'                                                  
         DC    CL2'15',XL1'15'                                                  
         DC    CL2'16',XL1'16'                                                  
         DC    CL2'17',XL1'17'                                                  
         DC    CL2'18',XL1'18'                                                  
         DC    CL2'19',XL1'19'                                                  
         DC    CL2'1A',XL1'1A'                                                  
         DC    CL2'22',XL1'22'                                                  
         DC    CL2'23',XL1'23'                                                  
         DC    CL2'24',XL1'24'                                                  
         DC    CL2'25',XL1'25'                                                  
         DC    CL2'26',XL1'26'                                                  
         DC    CL2'27',XL1'27'                                                  
         DC    CL2'28',XL1'28'                                                  
         DC    CL2'29',XL1'29'                                                  
         DC    CL2'2A',XL1'2A'                                                  
         DC    CL2'2B',XL1'2B'                                                  
         DC    CL2'2D',XL1'2D'                                                  
         DC    CL2'2E',XL1'2E'                                                  
         DC    CL2'2F',XL1'2F'                                                  
         DC    CL2'30',XL1'30'                                                  
         DC    CL2'31',XL1'31'                                                  
         DC    CL2'32',XL1'32'                                                  
         DC    CL2'33',XL1'33'                                                  
         DC    CL2'34',XL1'34'                                                  
         DC    CL2'35',XL1'35'                                                  
         DC    XL1'00',XL1'01'     DELIMITER                                    
*                                                                               
         EJECT                                                                  
VALDATE  OC    0(3,R2),0(R2)                                                    
         BNZ   CARDERR             DUPLICATE                                    
         GOTO1 DATVAL,DMCB,(0,0(R3)),WORK                                       
         CLI   DMCB+3,0                                                         
         BE    CARDERR                                                          
         SPACE 2                                                                
         GOTO1 GETBROAD,DMCB,(0,WORK),WORK+6                                    
         CLC   WORK(6),WORK+12                                                  
         BNE   CARDERR8            NOT LAST DAY OF BROADCAST MONTH              
         SPACE 2                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(3,0(R2))                                   
         BR    R8                                                               
         SPACE 2                                                                
CARDERR  MVC   P+1(26),=C'INVALID OR DUPLICATE CARD:'                           
         B     CERROR                                                           
CARDERR1 MVC   P+1(20),=C'NO REP= FOR CARD SET'                                 
         B     CERROR                                                           
CARDERR2 MVC   P+1(25),=C'NO REP CODE IN REP= CARD:'                            
         B     CERROR                                                           
CARDERR3 MVC   P+1(25),=C'DEFAULT SET MUST BE LAST:'                            
         B     CERROR                                                           
CARDERR4 MVC   P+1(37),=C'CONTRACT= ENTERED: NO AVAIL= OR PROP='                
         B     CERROR                                                           
CARDERR5 MVC   P+1(37),=C'TOO MANY SETS OF INPUT CARDS: 50 MAX!'                
         B     CERROR                                                           
CARDERR6 MVC   P+1(32),=C'YEAR ON CARD INVALID AS ENTERED!'                     
         B     CERROR                                                           
CARDERR7 MVC   P+1(29),=C'LOADTP= MUST BE "YES" OR "NO"'                        
         B     CERROR                                                           
CARDERR8 MVC   P+1(31),=C'NOT LAST DAY OF BROADCAST MONTH'                      
         B     CERROR                                                           
CERROR   GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         MVC   P+1(20),CARDIN                                                   
         GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*  SET BASIC FLAGS.  PROPOGATE FIRST INITIALIZED TABLE ENTRY INTO               
*  50 OTHER ENTRIES.                                                            
*                                                                               
TABINIT  LA    R7,REPNTRY                                                       
         ST    R7,THISREP          SET 'THISREP', 'NEXTREP' STILL=0             
         LA    R2,MOREREPS         A(1ST UNINITIALIZED TABLE ENTRY)             
         LA    R3,50               TABLE ENTRIES                                
TAB010   MVC   0(LREPNTRY,R2),0(R7) PROPAGATE ENTRY                             
         LA    R2,LREPNTRY(R2)     BUMP TO NEXT ENTRY                           
         BCT   R3,TAB010           DO 50 ENTRIES                                
         BR    R8                  RETURN TO CALLING RTN                        
         EJECT                                                                  
*              BUILD FILE OF CONTRACT NUMBERS                                   
         SPACE 1                                                                
*                                                                               
*   R7 IS USED THROUGHOUT THE REMAINDER OF THE PROGRAM TO CONTROL               
*      REP CODE TABLE SEARCH AND COUNTER UPDATE CONTROL.                        
*      >>>>>> PLEASE DON'T STEP ON R7!!!! <<<<<<                                
*                                                                               
PROCESS  EQU   *                                                                
*        BAS   RE,PRNTCNTS         PRINT TABLE AFTER SETUP                      
         LA    R7,REPNTRY          LOAD A(1ST TABLE ENTRY)                      
         CLI   SORT,C'N'                                                        
         BE    PHASE2              ONE PASS IS ENOUGH                           
         MVI   PHASE,C'1'                                                       
         OPEN  (IN,(INPUT))                                                     
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         SPACE 1                                                                
GETR1    GET   IN,REC-4                                                         
         LH    RE,REC-4            SET EOR MARKER                               
         LA    RE,REC-4(RE)                                                     
         MVI   0(RE),0                                                          
         LA    R5,REC                                                           
         SPACE 1                                                                
GETCON1  CLI   RCONKTYP,X'0C'                                                   
         BL    GETR1                                                            
         BH    END                                                              
         XC    SORTREC,SORTREC  BUILD THE SORT RECORD                           
         MVC   SREP,RCONKREP                                                    
         MVO   DUB(5),RCONKCON(4)                                               
         OI    DUB+4,X'0F'                                                      
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB(5)                                                
         MVO   WORK(5),WORK+10(5)                                               
         PACK  SCON(1),WORK+3(1)                                                
         PACK  SCON+1(1),WORK+2(1)                                              
         PACK  SCON+2(1),WORK+1(1)                                              
         PACK  SCON+3(1),WORK(1)                                                
         MVI   KEEPSW,0                                                         
         LA    RE,RCONELEM                                                      
GETC1    CLI   0(RE),0             CYCLE ELEMENTS TO FIND                       
         BE    GETC3                                                            
         CLI   0(RE),X'14'         THE AVAIL (X'14') ELEMENT                    
         BE    GETC2                                                            
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     GETC1                                                            
         USING RCONAVEL,RE                                                      
GETC2    MVC   KEEPSW,RCONAVKP                                                  
         SPACE 1                                                                
*                                                                               
*  TABLE MUST BE SEARCHED FOR CONTRACT'S REP CODE, OR DEFAULT.                  
*  IF NOT FOUND, TESTS ARE SKIPPED.                                             
*                                                                               
GETC3    LA    R2,RCONKREP                                                      
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    GETR1               NOT FOUND - NO SORT RECORD                   
         OC    DCONYR(3,R7),DCONYR(R7)                                          
         BZ    GETC4               NOT DELETING CONTRACTS                       
         AP    CONINP(5),=P'1'     INCREMENT COUNTER                            
         SPACE 1                                                                
         MVI   SACT,C'K'                                                        
         CLC   RCONDATE+3(3),DCONYR(R7)                                         
         BH    GETC3A                                                           
         MVI   SACT,C'D'                                                        
GETC3A   MVI   STYP,X'0B'          DELETE BUYS                                  
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         SPACE 1                                                                
GETC4    OC    DAVLYR(3,R7),DAVLYR(R7)                                          
         BZ    GETC5               NOT DELETING AVAILS                          
         MVI   SACT,C'K'                                                        
         CLI   KEEPSW,C'Y'                                                      
         BE    GETC4A                                                           
         CLC   RCONDATE+3(3),DAVLYR(R7)                                         
         BH    GETC4A                                                           
         MVI   SACT,C'D'                                                        
GETC4A   MVI   STYP,X'14'                                                       
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         SPACE 1                                                                
GETC5    OC    DPRPYR(3,R7),DPRPYR(R7)                                          
         BZ    GETR1               NOT DELETING PROPOSALS                       
         MVI   SACT,C'K'                                                        
         CLC   RCONDATE+3(3),DPRPYR(R7)                                         
         BH    GETC5A                                                           
         MVI   SACT,C'D'                                                        
GETC5A   MVI   STYP,X'16'                                                       
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     GETR1                                                            
         EJECT                                                                  
*                                                                               
*  SCAN REP TABLE.  SET R7 TO A(REPCODE) IF CODE IS FOUND, OR IF                
*       DEFAULT ENTRY IS ENCOUNTERED (LAST ALLOWABLE ENTRY).  IF                
*       NO DEFAULT, SET 'REPFOUND' = NO!                                        
*                                                                               
*       R7  =  A(REPCODE IN TABLE ENTRY)                                        
*       R2  =  A(REPCODE IN TAPE RECORD)                                        
*                                                                               
FINDREP  MVI   REPFOUND,C'Y' PRESET FLAG                                        
         CLC   0(2,R7),0(R2)       CHECK CURRENT POSITION                       
         BER   R8                                                               
         LA    R7,REPNTRY          RESET TO TOP OF TABLE                        
FR010    CLC   0(2,R7),0(R2)       CHECK REP CODE                               
         BER   R8                  FOUND - RETURN                               
         CLC   0(2,R7),=C'**'      DEFAULT FOUND?                               
         BER   R8                  YES - RETURN                                 
         CLC   0(2,R7),=C'00'      END OF TABLE?                                
         BNE   FR016               NO                                           
         MVI   REPFOUND,C'N'       SET 'NO REP' FLAG                            
         BR    R8                  RETURN                                       
FR016    LA    R7,LREPNTRY(R7)     BUMP TABLE                                   
         B     FR010               GO BACK FOR NEXT ENTRY                       
         EJECT                                                                  
*              CREATE THE OUTPUT TAPE                                           
         SPACE 1                                                                
PHASE2   OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
         XC    SORTREC,SORTREC                                                  
         SPACE 1                                                                
GETR2    GET   IN,REC-4                                                         
         LH    RE,REC-4                                                         
         LA    RE,REC-4(RE)                                                     
         MVI   0(RE),0                                                          
         LA    R5,REC                                                           
         AP    RECIN,=P'1'                                                      
         SPACE 2                                                                
*                                                                               
*   TYPEFROM DEFAULT = 00                                                       
*   TYPETO   DEFAULT = FF                                                       
*   IF THIS OPTION NOT ENTERED VIA CARD, ALL TYPES ARE PROCESSED                
*                                                                               
         CLC   RBUYKTYP,TYPEFROM   CHECK FOR TYPE RANGE START                   
         BL    PUT                 TYPE NOT BEING PROCESSED THIS PASS           
         CLC   RBUYKTYP,TYPETO     CHECK FOR TYPE RANGE END                     
         BH    PUT                 TYPE NOT BEING PROCESSED THIS PASS           
*                                                                               
*                                                                               
         CLI   RBUYKTYP,X'0B'                                                   
         BE    BUYREC                                                           
         CLI   RCONKTYP,X'0C'                                                   
         BE    CONREC                                                           
         CLI   RINVKTYP,X'12'                                                   
         BE    INVREC                                                           
         CLI   RAVLKTYP,X'14'                                                   
         BE    AVLREC                                                           
         CLI   RPRPKTYP,X'16'                                                   
         BE    PRPREC                                                           
         CLI   RBUDKTYP,X'13'                                                   
         BE    BUDREC                                                           
         CLI   REOMKTYP,X'18'                                                   
         BE    EOMREC                                                           
         CLI   RCOMKTYP,X'29'                                                   
         BE    COMMREC                                                          
         CLI   RSBBKTYP,X'2D'                                                   
         BE    FOREREC                                                          
         B     PUT                                                              
         EJECT                                                                  
*              DELETE BUYS                                                      
         SPACE 1                                                                
BUYREC   EQU   *                                                                
         LA    R2,RBUYKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         OC    DCONYR(3,R7),DCONYR(R7)                                          
         BZ    PUT                 DO NOT DELETE BUY                            
         SPACE 1                                                                
         MVC   WORK(1),RBUYKTYP                                                 
         MVC   WORK+1(2),RBUYKREP                                               
         MVC   WORK+3(4),RBUYKCON                                               
         SPACE 1                                                                
BUYNXT   CLC   SORTREC(7),WORK                                                  
         BL    BUYSRT                                                           
         BH    DELETE              CONTRACT ALREADY GONE                        
         CLI   SACT,C'K'           KEEP THIS                                    
         BE    PUT                                                              
         B     DELETE                                                           
         SPACE 1                                                                
BUYSRT   BAS   R8,GETSORT                                                       
         B     BUYNXT                                                           
         EJECT                                                                  
*              DELETE CONTRACTS                                                 
         SPACE 1                                                                
CONREC   LA    R2,RCONKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         OC    DCONYR(3,R7),DCONYR(R7)                                          
         BZ    PUT            DO NOT DELETE CONTRACTS                           
         CLC   RCONDATE+3(3),DCONYR(R7)                                         
         BH    PUT                                                              
         B     DELETE              DON'T CHECK AVAIL ELEMENT                    
*                                  ANY LONGER - GET RID OF RECORD               
         SPACE 1                                                                
*        LA    RE,RCONELEM                                                      
*CONREC1  CLI   0(RE),0                                                         
*        BE    DELETE                                                           
*        CLI   0(RE),X'14'                                                      
*        BE    CONREC2                                                          
*        ZIC   RF,1(RE)                                                         
*        AR    RE,RF                                                            
*        B     CONREC1                                                          
*        USING RCONAVEL,RE                                                      
*CONREC2  CLI   RCONAVKP,C'Y'                                                   
*        BE    PUT                                                              
*        B     DELETE                                                           
*        DROP  RE                                                               
         EJECT                                                                  
*              DELETE INVENTORY                                                 
*                                                                               
INVREC   LA    R2,RINVKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         OC    DINVYR(2,R7),DINVYR(R7)                                          
         BZ    PUT                 DO NOT DELETE                                
         SPACE 1                                                                
         OC    RINVKSRC,RINVKSRC                                                
         BNZ   INVBK               NOT A HEADER                                 
         MVI   INVALL,0                                                         
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    PUT                 THIS HAS NO END DATE                         
         SPACE 1                                                                
         CLC   RINVPEFF+2(2),DINVYR(R7)                                         
         BH    PUT                                                              
         MVI   INVALL,X'FF'        DELETE ALL BOOKS AND TEXTS                   
         B     DELETE                                                           
         SPACE 1                                                                
INVBK    CLI   RINVKSRC,X'FF'                                                   
         BE    INVTXT              NOT A BOOK                                   
         CLI   RINVKSRC,C'M'       MARKET FACT TEXT                             
         BE    INVTXT                                                           
         CLI   RINVKSRC,C'S'       STATION FACT TEXT                            
         BE    INVTXT                                                           
         SPACE 1                                                                
         CLI   INVALL,X'FF'        IF I DELETED HEADER                          
         BE    DELETE              DELETE ALL BOOKS                             
         SPACE 1                                                                
         CLC   RINVKBK,DINVBY(R7)  BUT DELETE IT ANYWAY                         
         BH    PUT                 IF BOOK IS NOT HIGHER THAN                   
         B     DELETE              CLOSE DATE                                   
         SPACE 1                                                                
INVTXT   DS    0H                                                               
         CLC   RINVKSTA,=C'ZZZZZ'                                               
         BNE   INVTXT1                                                          
         MVI   INVALL,0             THIS IS GENERAL TEXT                        
         SPACE 1                                                                
INVTXT1  CLI   INVALL,X'FF'        IF I DELETED HEADER                          
         BE    DELETE                                                           
         LA    R2,RINVPEL          FIRST ELEMENT                                
INVTXT2  CLI   0(R2),2                                                          
         BE    INVTXT3                                                          
         CLI   0(R2),0             NO FILTER ON THIS                            
         BE    PUT                                                              
         ZIC   R6,1(R2)                                                         
         AR    R2,R6                                                            
         B     INVTXT2                                                          
         SPACE 1                                                                
         USING RINVFEL,R2                                                       
INVTXT3  OC    RINVFYR(2),RINVFYR                                               
         BZ    PUT                 NO DATE FILTER                               
         CLC   RINVFYR(2),DINVBY(R7)                                            
         BH    PUT                                                              
         B     DELETE                                                           
         EJECT                                                                  
*              DELETE AVAILS                                                    
         SPACE 1                                                                
AVLREC   LA    R2,RAVLKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         OC    DAVLYR(3,R7),DAVLYR(R7)                                          
         BZ    PUT                 DO NOT DELETE AVAILS                         
         SPACE 1                                                                
         MVC   WORK(1),RAVLKTYP                                                 
         MVC   WORK+1(2),RAVLKREP                                               
         MVC   WORK+3(4),RAVLKCON                                               
         SPACE 1                                                                
AVLNXT   CLC   SORTREC(7),WORK                                                  
         BL    AVLSRT                                                           
         BH    DELETE              CONTRACT ALREADY GONE                        
         CLI   SACT,C'K'                                                        
         BE    PUT                 KEEP IT                                      
         B     DELETE                                                           
         SPACE 1                                                                
AVLSRT   BAS   R8,GETSORT                                                       
         B     AVLNXT                                                           
         EJECT                                                                  
*              DELETE PROPOSALS                                                 
         SPACE 1                                                                
PRPREC   LA    R2,RPRPKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         OC    DPRPYR(3,R7),DPRPYR(R7)                                          
         BZ    PUT                 DO NOT DELETE PROPOSALS                      
         SPACE 1                                                                
         MVC   WORK(1),RPRPKTYP                                                 
         MVC   WORK+1(2),RPRPKREP                                               
         MVC   WORK+3(4),RPRPKCON                                               
         SPACE 1                                                                
PRPNXT   CLC   SORTREC(7),WORK                                                  
         BL    PRPSRT                                                           
         BH    DELETE              CONTRACT ALREADY GONE                        
         CLI   SACT,C'K'                                                        
         BE    PUT                 KEEP IT                                      
         B     DELETE                                                           
         SPACE 1                                                                
PRPSRT   BAS   R8,GETSORT                                                       
         B     PRPNXT                                                           
         EJECT                                                                  
*                                                                               
*  BUDGET RECORDS ARE CHECKED AGAINST A TABLED YEAR (2 BYTES EBCDIC)            
*        IF RECORD YEAR IS GREATER THAN TABLED YEAR, RECORD IS KEPT             
*                                                                               
BUDREC   LA    R2,RBUDKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         OC    DBUDYR(2,R7),DBUDYR(R7)                                          
         BZ    PUT                 DO NOT DELETE BUDGET RECORD                  
         SPACE 1                                                                
         CLC   RBUDKYR(2),DBUDYR(R7)                                            
         BH    PUT                 KEEP IT                                      
         B     DELETE                                                           
         EJECT                                                                  
*                                                                               
*  EOM RECORDS ARE CHECKED AGAINST A TABLED YEAR (1 BYTE BINARY)                
*        IF RECORD YEAR IS GREATER THAN TABLED YEAR, RECORD IS KEPT             
*                                                                               
EOMREC   LA    R2,REOMKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         OC    DEOMYR(1,R7),DEOMYR(R7)                                          
         BZ    PUT                 DO NOT DELETE EOM RECORD                     
         SPACE 1                                                                
         CLC   REOMKYR(1),DEOMYR(R7)                                            
         BH    PUT                 KEEP IT                                      
         B     DELETE                                                           
         EJECT                                                                  
*   COMMISSION RECORDS ARE CHECKED AGAINST A TABLED Y/M (2-BYTE BINY)           
*     IF RECORD DATE IS GREATER THAN TABLED DATE, RECORD IS KEPT                
*                                                                               
COMMREC  EQU   *                                                                
         LA    R2,RCOMKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         OC    DCOMMYR(2,R7),DCOMMYR(R7)                                        
         BZ    PUT                 NO DATE - DON'T DELETE COMM RECORD           
         CLC   RCOMKDAT(2),DCOMMYR(R7)                                          
         BH    PUT                 KEEP IT                                      
         B     DELETE                                                           
         EJECT                                                                  
*   FORECAST RECORDS ARE CHECKED AGAINST A TABLED YMD (COMPRESSED)              
*     IF RECORD DATE IS GREATER THAN TABLED DATE, RECORD IS KEPT                
*                                                                               
FOREREC  EQU   *                                                                
         LA    R2,RSBBKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         OC    DFOREYR(2,R7),DFOREYR(R7)                                        
         BZ    PUT                 NO DATE - DON'T DELETE FCST RECORD           
         CLC   RSBBKPER(2),DFOREYR(R7)                                          
         BH    PUT                 KEEP IT                                      
         B     DELETE                                                           
         EJECT                                                                  
*                                                                               
*  OUTPUT SIDE FOR RECORDS:  DELETE HAS BIT SET                                 
*                                                                               
         SPACE 1                                                                
DELETE   LA    R3,DBUYDEL(R7)      A(DELETE COUNTER BY TYPE, REP)               
         CLI   RBUYKTYP,X'0B'      CHECK RECORD TYPE                            
         BE    DEL1                                                             
         LA    R3,DCONDEL(R7)                                                   
         CLI   RCONKTYP,X'0C'                                                   
         BE    DEL1                                                             
         LA    R3,DINVDEL(R7)                                                   
         CLI   RINVKTYP,X'12'                                                   
         BE    DEL1                                                             
         LA    R3,DAVLDEL(R7)                                                   
         CLI   RAVLKTYP,X'14'                                                   
         BE    DEL1                                                             
         LA    R3,DPRPDEL(R7)                                                   
         CLI   RPRPKTYP,X'16'                                                   
         BE    DEL1                                                             
         LA    R3,DBUDDEL(R7)                                                   
         CLI   RBUDKTYP,X'13'                                                   
         BE    DEL1                                                             
         LA    R3,DEOMDEL(R7)                                                   
         CLI   REOMKTYP,X'18'                                                   
         BE    DEL1                                                             
         LA    R3,DCOMMDEL(R7)                                                  
         CLI   RCOMKTYP,X'29'                                                   
         BE    DEL1                                                             
         LA    R3,DFOREDEL(R7)                                                  
         CLI   RSBBKTYP,X'2D'                                                   
         BE    DEL1                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
DEL1     EQU   *                                                                
         OI    REC+29,X'01'        CLOSED BIT                                   
         AP    0(5,R3),=P'1'                                                    
         AP    RECPURG,=P'1'                                                    
         CLC   =C'PRTBOTH ',PRTOPT PRINTOUT WANTED?                             
         BE    DEL2                YES                                          
         CLC   =C'PRTDROPS',PRTOPT                                              
         BNE   DEL3                                                             
DEL2     EQU   *                                                                
         BAS   RE,DMPPUT                                                        
         SPACE 1                                                                
*                                                                               
*   IF LOADTP=NO, CLOSEOUTS (WITH EXCEPTION OF CONTRACTS AND BUYS)              
*        ARE NOT OUTPUT TO TAPE.  THEY ARE DROPPED DIRECTLY, SAVING             
*        THE NEED TO RUN RELDPURG ACTION TO PHYSICALLY DELETE.                  
*                                                                               
DEL3     EQU   *                                                                
         CLI   LOADTP,C'Y'         WRITE TO TAPE?                               
         BE    PUT                 YES - DON'T NEED TO CHECK TYPE               
         CLI   RBUYKTYP,X'0B'      BUYS ALWAYS WRITTEN TO TAPE                  
         BE    PUT                                                              
         CLI   RCONKTYP,X'0C'      CONTRACTS ALWAYS WRITTEN TO TAPE             
         BNE   GETR2                                                            
PUT      AP    RECOUT,=P'1'                                                     
         PUT   OUT,REC-4                                                        
         CLC   =C'PRTBOTH ',PRTOPT PRINTOUT WANTED?                             
         BE    PUT1                YES                                          
         CLC   =C'PRTREADS',PRTOPT                                              
         BNE   PUT2                                                             
PUT1     EQU   *                                                                
         CP    RECOUT(5),=PL5'100' PRINT FIRST 100 IN                           
         BH    PUT2                                                             
         BAS   RE,DMPGET                                                        
PUT2     EQU   *                                                                
         B     GETR2                                                            
         SPACE 1                                                                
END      CLOSE (IN,)                                                            
         CLI   PHASE,C'2'                                                       
         BE    CLSOUT                                                           
         MVI   PHASE,C'2'                                                       
         B     PHASE2                                                           
CLSOUT   CLOSE (OUT,)                                                           
         GOTO1 SORTER,DMCB,=C'END'                                              
         BAS   RE,PRNTCNTS         PRINT OUT TOTALS                             
         B     ENDJOB                                                           
         SPACE 2                                                                
GETSORT  CLI   STYP,X'FF'                                                       
         BER   R8                                                               
         MVI   STYP,X'FF'                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZR   R8                                                               
         MVC   SORTREC,0(R6)                                                    
         BR    R8                                                               
         EJECT                                                                  
*              PRINT OUT THE ACCUMULATORS                                       
         SPACE 1                                                                
PRNTCNTS EQU   *                                                                
         NTR1                                                                   
         MVI   SPACING+3,C'3'                                                   
         GOTO1 PRINTER                                                          
         MVC   P+1(10),=C'RECORDS IN'                                           
         LA    R3,RECIN                                                         
         MVI   SPACING+3,C'2'                                                   
         BAS   R8,EDTP                                                          
         SPACE 1                                                                
         MVC   P+1(11),=C'RECORDS OUT'                                          
         LA    R3,RECOUT                                                        
         BAS   R8,EDTP                                                          
         SPACE 1                                                                
         MVC   P+1(10),=C'CNTRCTS IN'                                           
         LA    R3,CONINP                                                        
         MVI   SPACING+3,C'2'                                                   
         BAS   R8,EDTP                                                          
         SPACE 1                                                                
         MVI   SPACING+3,C'1'                                                   
         MVC   P+1(14),=C'RECORDS CLOSED'                                       
         GOTO1 PRINTER                                                          
         MVI   P+1,C'-'                                                         
         MVC   P+2(13),P+1                                                      
         GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         SPACE 1                                                                
         LA    R7,REPNTRY          SET A(1ST REP IN TABLE)                      
PRNTBCKT CLC   0(2,R7),=C'00'      END OF TABLE?                                
         BE    PRNTC10             YES - WRAP IT UP                             
         MVI   SPACING+3,C'3'      TRIPLE SPACE                                 
         MVC   P+2(4),=C'REP='                                                  
         MVC   P+6(2),0(R7)        PRINT REP CODE                               
         GOTO1 PRINTER                                                          
         MVI   SPACING+3,C'1'                                                   
         LA    R6,DCONDEL(R7)      A(FIRST BUCKET IN REP)                       
         LA    R5,PURGED                                                        
         LA    R2,NUMDELS          LOOP BY NUMBER OF COUNTERS                   
         SPACE 1                                                                
PRNTC2   MVC   P+2(10),0(R5)                                                    
         LR    R3,R6               UNLOAD BUCKET FROM REP                       
         BAS   R8,EDTP                                                          
         LA    R5,10(R5)                                                        
         LA    R6,DELSIZE(R6)      BUMP A(DELETE BUCKET)                        
         BCT   R2,PRNTC2                                                        
         GOTO1 PRINTER             EXTRA SPACE AFTER REP                        
         MVI   P+1,C'-'                                                         
         MVC   P+2(30),P+1                                                      
         GOTO1 PRINTER                                                          
         LA    R7,LREPNTRY(R7)     BUMP TABLE ENTRY                             
         B     PRNTBCKT            CHECK NEXT TABLE ENTRY                       
         SPACE 1                                                                
PRNTC10  MVC   P+1(12),=C'TOTAL CLOSED'                                         
         LA    R3,RECPURG                                                       
         BAS   R8,EDTP                                                          
         B     EXXMOD                                                           
         SPACE 1                                                                
EDTP     EDIT  (P5,0(R3)),(10,P+18),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 PRINTER                                                          
         BR    R8                                                               
         SPACE 1                                                                
ENDJOB   XBASE                                                                  
         SPACE 5                                                                
         USING RECD,R5                                                          
DMPGET   LA    R6,=CL10'GET'                                                    
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   EQU   *                                                                
         CP    0(5,R3),=PL5'100'   PRINT FIRST 100                              
         BNH   DP002               LESS THAN 101                                
         ZAP   DUB,0(5,R3)         MOVE 5-POS CTR TO 8-POS DUB                  
         DP    DUB,=PL4'200'       DIV 8-POS DUB BY 4-POS CONSTANT              
         CP    DUB+4(4),=PL4'0'    REMAINDER(4 POS) = ZERO?                     
         BNE   DMPNO               NO                                           
DP002    EQU   *                                                                
         LA    R6,=CL10'PUT'       YES                                          
         BAS   R8,DUMP             PRINT IT OUT                                 
DMPNO    BR    RE                                                               
         SPACE 1                                                                
DUMP     NTR1                                                                   
         LA    R5,REC                                                           
*        MVC   HALF,27(R5)                                                      
*        LH    R8,HALF                                                          
         LA    R8,27                                                            
         GOTO1 PRNTBL,DMCB,(10,(R6)),(R5),C'DUMP',(R8),=C'2D'                   
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
         EJECT                                                                  
*              CONSTANTS AND WORKING STORAGE                                    
         SPACE 1                                                                
DMCB     DC    6F'0'                                                            
WORK     DC    CL80' '                                                          
BYTE     DC    X'0'                                                             
FULL     DC    F'0'                                                             
DUB      DC    D'0'                                                             
HALF     DC    H'0'                                                             
         SPACE 1                                                                
ADDAY    DC    V(ADDAY)                                                         
DATCON   DC    V(DATCON)                                                        
GETBROAD DC    V(GETBROAD)                                                      
GETDAY   DC    V(GETDAY)                                                        
RECUP    DC    V(RECUP)                                                         
PRINTER  DC    V(PRINTER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
CARDS    DC    V(CARDS)                                                         
DATVAL   DC    V(DATVAL)                                                        
SORTER   DC    V(SORTER)                                                        
         SPACE 1                                                                
RECIN    DC    PL5'0'                                                           
RECOUT   DC    PL5'0'                                                           
RECPURG  DC    PL5'0'                                                           
PURGED   DC    CL10'CONTRACTS '                                                 
         DC    CL10'BUYS      '                                                 
         DC    CL10'INVENTORY '                                                 
         DC    CL10'AVAILS    '                                                 
         DC    CL10'PROPOSALS '                                                 
         DC    CL10'BUDGETS   '                                                 
         DC    CL10'EOMS      '                                                 
         DC    CL10'COMMISSION'                                                 
         DC    CL10'FORECAST  '                                                 
         SPACE 1                                                                
REPCTR   DC    PL2'00'                                                          
THISREP  DC    F'0'                                                             
NEXTREP  DC    F'0'                                                             
         SPACE 1                                                                
REPNTRY  EQU   *                                                                
REPCDE   DC    CL2'00'                                                          
CONYR    DC    XL3'000000'                                                      
AVLYR    DC    XL3'000000'                                                      
PRPYR    DC    XL3'000000'                                                      
INVBY    DC    XL3'000000'                                                      
INVYR    DC    XL2'0000'                                                        
BUDYR    DC    XL2'0000'           BUDGET YEAR: 2 BYTES ALPHA                   
EOMYR    DC    XL1'00'             EOM YEAR: 1 BYTE BINARY                      
COMMYR   DC    XL2'0000'           COMMISSION YEAR: 2 BYTES BINARY              
FOREYR   DC    XL2'0000'           FORECAST YEAR: 2 BYTES COMPRESSED            
CONDEL   DC    PL5'0'                                                           
BUYDEL   DC    PL5'0'                                                           
INVDEL   DC    PL5'0'                                                           
AVLDEL   DC    PL5'0'                                                           
PRPDEL   DC    PL5'0'                                                           
BUDDEL   DC    PL5'0'                                                           
EOMDEL   DC    PL5'0'                                                           
COMMDEL  DC    PL5'0'                                                           
FOREDEL  DC    PL5'0'                                                           
DELSIZE  EQU   BUYDEL-CONDEL       SIZE OF DELETE COUNTER                       
NUMDELS  EQU   (*-CONDEL)/DELSIZE  NUMBER OF COUNTERS                           
LREPNTRY EQU   *-REPNTRY                                                        
TABSIZE  EQU   (50*LREPNTRY)+2                                                  
MOREREPS DC    (TABSIZE)X'00'            TABLE SIZE= 51 ENTRIES                 
DREP     EQU   0                                                                
DCONYR   EQU   CONYR-REPNTRY                                                    
DAVLYR   EQU   AVLYR-REPNTRY                                                    
DPRPYR   EQU   PRPYR-REPNTRY                                                    
DINVBY   EQU   INVBY-REPNTRY                                                    
DINVYR   EQU   INVYR-REPNTRY                                                    
DBUDYR   EQU   BUDYR-REPNTRY                                                    
DEOMYR   EQU   EOMYR-REPNTRY                                                    
DCOMMYR  EQU   COMMYR-REPNTRY                                                   
DFOREYR  EQU   FOREYR-REPNTRY                                                   
DCONDEL  EQU   CONDEL-REPNTRY                                                   
DBUYDEL  EQU   BUYDEL-REPNTRY                                                   
DINVDEL  EQU   INVDEL-REPNTRY                                                   
DAVLDEL  EQU   AVLDEL-REPNTRY                                                   
DPRPDEL  EQU   PRPDEL-REPNTRY                                                   
DBUDDEL  EQU   BUDDEL-REPNTRY                                                   
DEOMDEL  EQU   EOMDEL-REPNTRY                                                   
DCOMMDEL EQU   COMMDEL-REPNTRY                                                  
DFOREDEL EQU   FOREDEL-REPNTRY                                                  
INVALL   DC    XL1'00'                                                          
         SPACE 1                                                                
*                                                                               
CONINP   DC    PL5'0'                                                           
*                                                                               
CARDIN   DS    CL80                                                             
         SPACE 1                                                                
         SPACE 1                                                                
SORTREC  DS    0CL8                                                             
STYP     DS    CL1                                                              
SREP     DS    CL2                 REP                                          
SCON     DS    CL4                 CONTRACT NO. 9'S COMP. REVERSED              
SACT     DS    CL1                                                              
         SPACE 1                                                                
SORT     DC    C'N'                                                             
PHASE    DC    C'2'                                                             
KEEPSW   DS    CL1                                                              
DEFREP   DC    C'N'                DEFAULT REP ENTERED FLAG                     
REPFOUND DC    C'Y'                REP FOUND IN TABLE FLAG                      
LOADTP   DC    C'N'                DON'T PUT RECORDS TO TAPE                    
DATECHK  DC    C'010100'           DATE CHECK WORK AREA                         
PRTOPT   DC    CL8'        '       RUN-TIME PRINT OPTIONS                       
BADCARD  DC    CL1'N'              ERROR IN CARD INPUT FLAG                     
TYPEFROM DC    XL1'00'             RECORD TYPE CODE START                       
TYPETO   DC    XL1'FF'             RECORD TYPE CODE END                         
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,7,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=8'                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    D                                                                
REC      DS    1008C                                                            
         SPACE 1                                                                
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=01004,                                            X        
               MACRF=GM,                                               X        
               EODAD=END                                                        
         SPACE 2                                                                
OUT      DCB   DDNAME=OUT,             DOS SYS012                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=01004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=08200               X        
               MACRF=PM                                                         
         SPACE 2                                                                
         EJECT                                                                  
* DDDPRINT                                                                      
* REGENALLD                                                                     
*        PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE REGENALLD                                                      
         ORG   RECORD                                                           
       ++INCLUDE REGENCOM                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSBB                                                       
         ORG                                                                    
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069REREPCLX  05/01/02'                                      
         END                                                                    
