*          DATA SET REREPAC02A AT LEVEL 029 AS OF 05/01/02                      
*PHASE REAC02A,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE DAYPAK                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE REGENBUC                                                               
*INCLUDE RECUP                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE QSORT                                                                  
         TITLE 'REREPAC02  (REAC02A) --- ACTUALIZER '                           
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPAC02  -- KATZ ACTUALIZER FOR ALL OF '96              *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
*                                                                  *            
* JUN12/96 (BG ) --- CHANGE GROUP FROM R TO RB                     *            
*                                                                  *            
* DEC13/95 (BU ) --- ORIGINAL ENTRY                                *            
*                                                                  *            
* JUL12/96 (BG ) 6-7 CHANGES FOR 96 ACTUALIZATION                  *            
*                                                                  *            
* SEP04/96 (BG ) 14  CHANGES FOR 96 ACTUALIZATION - JUN            *            
*                                                                  *            
* SEP05/96 (BG ) 15  MOVE ACTCTR ADD                               *            
*                                                                  *            
* OCT07/96 (BG ) 16  CHANGES FOR 96 ACTUALIZATION - JUL & AUG      *            
*                    READ STATION FOR GRP/SUB                      *            
*                                                                  *            
* MAR18/97 (BG ) 17  SET FOR ALL OF '96 - REACTUALIZE              *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  RUN-TIME SWITCHES AND INPUT VALUES:                             *            
*      QUESTOR    =   Y   PRINT CONTRACT DETAIL                    *            
*      QUESTOR+1  =   Y   PRINT ACTUAL DATA                        *            
*      QUESTOR+2  =   Y   DISPLAY CONTRACT UPDATED RECORDS         *            
*      QUESTOR+3  =   Y   DISPLAY KEYS AS DELETED                  *            
*      QUESTOR+4  =   Y   DISPLAY CONTRACTS WITH ANY 04 ELEMS      *            
*                          WITH DOLLARS                            *            
*      QUESTOR+5  =   Y   DISPLAY UNMATCHED TOTALS ACT - CONTR GRP *            
*                                                                  *            
*                                                                  *            
*      QRECORD+20  =  REPS TO SCAN FOR ACTUALIZATION - MAX OF 8    *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REAC02   CSECT                                                                  
         NMOD1 0,**REAC**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
*        L     RC,FILEC                                                         
*        USING FILED,RC                                                         
*                                                                               
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MAIN0900                                                         
*                                                                               
MAIN0020 DS    0H                                                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTRT          SAVE START TIME                              
*                                                                               
* GRIF:  THIS JOB DIDN'T READ A TAPE INPUT.  I ADDED THE TAPE DCB               
* PLUS OPEN AND CLOSE BACK IN JUST FOR SKELETON SAKE.                           
*                                                                               
*                                                                               
         BAS   RE,INITIAL          ESTABLISH WORK AREAS                         
         BAS   RE,TABLINIT         SET UP TOTALS ARRAYS                         
         BAS   RE,CONTPROC         SWEEP THE CONTRACTS                          
         SPACE                                                                  
         GOTO1 DISPTOTS                                                         
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
         CLOSE (INTAPE,REWIND)     CLOSE INPUT FILE                             
MAIN0900 DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*   INITIALIZATIONS ....                                                        
         SPACE                                                                  
         DS    0H                                                               
INITIAL  NTR1                                                                   
         MVC   P+1(16),=C'ENTERING INITIAL'                                     
         GOTO1 REPORT                                                           
         OPEN  (FILOUTA,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OPEN  (FILOUTB,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OPEN  (INTAPE,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',1000000,1000000                                 
*                                  GET 1MEG STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 DS   0H                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LHSELIST,P3         L(ADD'L SPACE GIVEN)                         
         MVC   AHSELIST,P2         HOUSE ACCT LIST AREA                         
         MVC   ANXTHSE,P2                                                       
         SPACE                                                                  
         L     RC,FILEC                                                         
         L     R2,ADCONLST                                                      
         L     R3,MASTC-ADCONSD(R2)                                             
         LA    R4,MCIO-MASTD(,R3)                                               
         MVC   QSTART(12),Q3XTRFRM-QREC3D+160(R4)                               
         MVC   P(80),0(R4)                                                      
         GOTO1 REPORT                                                           
         MVC   P(80),80(R4)                                                     
         GOTO1 REPORT                                                           
         MVC   P(80),160(R4)                                                    
         GOTO1 REPORT                                                           
         SPACE                                                                  
         PACK  DUB,QSTART(2)        YEAR                                        
         CVB   R0,DUB                                                           
         STC   R0,BSTRYM                                                        
         PACK  DUB,QSTART+2(2)      MONTH                                       
         CVB   R0,DUB                                                           
         STC   R0,BSTRYM+1                                                      
         SPACE                                                                  
         PACK  DUB,QEND(2)        YEAR                                          
         CVB   R0,DUB                                                           
         STC   R0,BENDYM                                                        
         PACK  DUB,QEND+2(2)      MONTH                                         
         CVB   R0,DUB                                                           
         STC   R0,BENDYM+1                                                      
         SPACE                                                                  
         MVC   AGYREP,QOPTION1     SAVE REP FOR AGENCY RECORDS                  
         CLC   AGYREP,SPACES       ANY VALUE IN AGENCY REP?                     
         BNE   INIT0040            NO  - USE IT                                 
         MVC   AGYREP,=C'K3'       YES - USE KATZ                               
INIT0040 EQU   *                                                                
         SPACE                                                                  
         LA    R0,1                                                             
         SPACE                                                                  
         L     R3,=A(SKEL96)                                                    
         L     R2,=A(SKEY96)                                                    
         CLC   AGYREP,=C'K3'       THIS A KATZ RUN                              
         BE    INIT0100                                                         
         CLC   AGYREP,=C'BF'       THIS A KATZ RUN                              
         BE    INIT0100                                                         
         DC    H'0'                                                             
INIT0100 MVC   KEY(27),0(R2)       GET SKELETON KEY                             
         MVC   KEY+2(2),AGYREP     INSERT REP CODE                              
         GOTO1 HIGH                READ FOR THE FIRST KEY                       
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R3,AIOAREA                                                       
         GOTO1 GREC                RETRIEVE SKELETON CONTRACT RECORD            
         LA    R2,28(,R2)                                                       
         LA    R3,SKELEN(,R3)                                                   
         BCT   R0,INIT0100                                                      
         SPACE                                                                  
         MVC   SWIREPS,SPACES      CLEAR SWITCH REPS                            
         MVC   SWIREPS(16),QRECORD+20  INSERT REPS TO BE ACTUALIZED             
         SPACE                                                                  
         MVC   P+1(15),=C'LEAVING INITIAL'                                      
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*   TABLINIT:                                                                   
         SPACE                                                                  
         DS    0H                                                               
TABLINIT NTR1                                                                   
         MVC   P+1(17),=C'ENTERING TABLINIT'                                    
         GOTO1 REPORT                                                           
         SPACE                                                                  
         LA    R2,ARRAY                                                         
         BAS   RE,TABLIN10                                                      
         SPACE                                                                  
         L     R2,=A(KARRAY)       FOR KATZ ACTUALS TAPE                        
         BAS   RE,TABLIN40                                                      
         SPACE                                                                  
         L     R2,=A(HARRAY)       HOUSE CONTRACTS ADDED                        
         BAS   RE,TABLIN40                                                      
         SPACE                                                                  
         L     R2,=A(CARRAY)       KATZ CONTRACT TOTALS                         
         BAS   RE,TABLIN40                                                      
         SPACE                                                                  
         MVC   P+1(16),=C'LEAVING TABLINIT'                                     
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE                                                                  
TABLIN10 LA    R0,ARRAYCT                                                       
         LA    R1,96               START WITH '96                               
         LA    RF,JAN96            JUN                                          
TABLIN20 STC   R1,0(,R2)                                                        
         STC   RF,1(,R2)                                                        
         XC    2(4,R2),2(R2)                                                    
         LA    R2,6(,R2)                                                        
         LA    RF,1(,RF)                                                        
         CH    RF,=H'12'                                                        
         BNH   TABLIN30                                                         
         LA    RF,1                                                             
         LA    R1,1(,R1)                                                        
TABLIN30 BCT   R0,TABLIN20                                                      
         BR    RE                                                               
         SPACE                                                                  
TABLIN40 LA    R0,ARRAYCT                                                       
         LA    R1,96               START WITH '96                               
         LA    RF,JAN96                                                         
TABLIN50 STC   R1,0(,R2)                                                        
         STC   RF,1(,R2)                                                        
         ZAP   2(6,R2),=P'0'                                                    
         LA    R2,8(,R2)                                                        
         LA    RF,1(,RF)                                                        
         CH    RF,=H'12'                                                        
         BNH   TABLIN60                                                         
         LA    RF,1                                                             
         LA    R1,1(,R1)                                                        
TABLIN60 BCT   R0,TABLIN50                                                      
         BR    RE                                                               
*                                                                               
*                1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.                                 
OFFTABLE DC    C'ATBOCHDADEHOLAMNNYPHPOSESFSLTO'                                
         DC    X'0000'                                                          
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*   CONTPROC:  SWEEP CONTRACTS FOR EACH REP ON LIST. READ ALL CONTRACTS         
*      AND ACTUALIZE (ADD AN 04 ELEM FOR ALL 03 ELEMS IN REQUESTED              
*      PERIOD. WRITE UPDATED CONTRACTS TO TAPE, AND DELETE CONTRACTS            
*      THAT ARE UPDATED.  THEY ARE RELOADED TO AVOID USING ALL OVERFLOW         
*      WITH THE LARGER RECORDS.                                                 
*                                                                               
         DS    0H                                                               
CONTPROC NTR1                                                                   
         MVC   P+1(17),=C'ENTERING CONTPROC'                                    
         GOTO1 REPORT                                                           
         L     RF,FILEC            SET IOAREA                                   
         ST    RF,AIOAREA                                                       
         LA    R2,SWIREPS                                                       
         CLC   0(2,R2),SPACES      END OF REPS?                                 
         BNE   CCON0020             WHAT?                                       
         DC    H'0'                                                             
CCON0020 DS   0H                                                                
         SPACE                                                                  
* NOW GET HIGH CONTRACT # FOR REP                                               
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),0(R2)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(4),KEY+23 FOR 4                                             
         MVI   WORK+4,X'0C'                                                     
         ZAP   DUB,=P'1000000000'                                               
         SP    DUB,WORK(5)                                                      
         ZAP   SVCON,DUB                                                        
         SPACE                                                                  
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'CC'           INSERT KEY TYPE                              
         MVC   KEY+1(2),0(R2)      INSERT REP CODE                              
         GOTO1 HIGH                READ FOR THE FIRST KEY                       
         MVC   SVREPSTA,KEY+1      SAVE REP/STA/OFF                             
         B     CCON0060                                                         
CCON0040 DS   0H                                                                
         CLC   PUTCTR2,=F'1000'    ** MAX 1000 O/P TEST                         
         BL    CCON0050                                                         
*        BH    CCON1000            EXCEEDED:  END OF JOB                        
         MVC   P+5(29),=C'1000 CONTRACTS ACTUALIZED FOR'                        
         MVC   P+35(2),KEY+1                                                    
         GOTO1 REPORT                                                           
         XC    PUTCTR2,PUTCTR2     CLEAR COUNTER FOR LOOP                       
         SPACE                                                                  
CCON0050 DS   0H                                                                
         GOTO1 SEQ                 READ NEXT                                    
CCON0060 DS   0H                                                                
*        MVC   P+1(07),=C'CONKEY='                                              
*        MVC   P+10(27),KEY                                                     
*        GOTO1 REPORT                                                           
         CLI   KEY,X'CC'           CONTRACT RECORD?                             
         BNE   CCON0900             NO  - BUMP TO NEXT REP                      
         SPACE                                                                  
         CLC   KEY+1(2),0(R2)      SAME REP?                                    
         BNE   CCON0900             NO  - BUMP TO NEXT REP                      
         SPACE                                                                  
         LA    R4,KEY                                                           
         USING RCONREC,R4                                                       
         CLC   RCONSREP(9),SVREPSTA SAME REP/STA/OFF                            
         BE    CCON0064                                                         
         SPACE                                                                  
         L     RF,=A(CARRAY)       CONTRACT TOTALS ARRAY                        
         BAS   RE,ADDARR           ADD FROM ARRAY TO CONT TOT                   
         SPACE                                                                  
         BAS   RE,CKACT            CHECK AGAINST ACTUALIZATION TAPE             
         SPACE                                                                  
         MVC   SVREPSTA,KEY+1                                                   
         SPACE                                                                  
         CLC   REDCTR,=F'200'                                                   
         NOP   CCON1000                                                         
         SPACE                                                                  
CCON0064 L     RF,REDCTR           INCREMENT TOTAL CONTRACTS READ               
         LA    RF,1(RF)                                                         
         ST    RF,REDCTR                                                        
         SPACE                                                                  
         CLI   QUESTOR,C'Y'      DISPLAY CONTRACT INFO?                         
         BNE   CCON0066             NO                                          
         MVC   PREP,RCONSREP       REP                                          
         MVC   PSTA(05),RCONSSTA   STATION                                      
         MVC   POFF(02),RCONSOFF   OFFICE                                       
         GOTO1 HEXOUT,DMCB,RCONKCON,PCON,4,=C'TOG'                              
         MVC   PREASON(13),=C'READ CONTRACT'                                    
         GOTO1 REPORT                                                           
         SPACE                                                                  
CCON0066 DS    0H                                                               
         GOTO1 GREC                RETRIEVE CONTRACT RECORD                     
         SPACE                                                                  
         L     R6,AIOAREA                                                       
*        ZICM  RF,27(R6),2        GET LENGTH OF ENTRY                           
*        GOTO1 =V(PRNTBL),DMCB,(0,(R6)),(R6),C'DUMP',(RF),=C'1D'                
         MVI   FIXINV,0                                                         
         MVI   ELCODE,X'04'        CK FOR ACTUALS                               
         BAS   RE,GETEL                                                         
         BNE   FIX0400                                                          
FIX0100  CLC   2(2,R6),=X'6001'                                                 
         BL    FIX0300                                                          
         CLC   2(2,R6),=X'600C'                                                 
         BH    FIX0350                                                          
         MVI   FIXINV,1                                                         
         MVI   0(R6),X'FF'                                                      
         SPACE                                                                  
FIX0300  BAS   RE,NEXTEL                                                        
         BE    FIX0100                                                          
         SPACE                                                                  
FIX0350  CLI   FIXINV,0                                                         
         BE    FIX0400                                                          
         L     R6,AIOAREA                                                       
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',(R6)),0,0               
         CLI   P4,0                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVI   ELCODE,X'01'        CK FOR ACTUAL CONTRACTS                      
         L     R6,AIOAREA                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONELEM,R6                                                      
         SPACE                                                                  
* WAS THIS A HOUSE ACCT ADDED FOR ACTUALS?                                      
         SPACE                                                                  
         TM    RCONMODR+1,X'01'    IS THIS HOUSE ACCT ADDED                     
         BZ    FIX0360              NO                                          
         L     RF,HSECLR           INCREMENT TOTAL HOUSE ACCTS CLEARED          
         LA    RF,1(,RF)                                                        
         ST    RF,HSECLR                                                        
         B     FIX0400                                                          
         SPACE                                                                  
FIX0360  L     RF,CONCLR           INCREMENT TOTAL CONTRACT CLEARED             
         LA    RF,1(,RF)                                                        
         ST    RF,CONCLR                                                        
         SPACE                                                                  
FIX0400  L     R6,AIOAREA                                                       
*        ZICM  RF,27(R6),2        GET LENGTH OF ENTRY                           
*        GOTO1 =V(PRNTBL),DMCB,(0,(R6)),(R6),C'DUMP',(RF),=C'1D'                
         MVI   ELCODE,X'01'        CK FOR ACTUAL CONTRACTS                      
         L     R6,AIOAREA                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONELEM,R6                                                      
         SPACE                                                                  
* WAS THIS A HOUSE ACCT ADDED FOR ACTUALS?                                      
         SPACE                                                                  
         TM    RCONMODR+1,X'01'    IS THIS HOUSE ACCT ADDED                     
         BZ    CCON0070             NO                                          
         SPACE                                                                  
         L     RF,HSEACT           INCREMENT TOTAL HOUSE ACCTS READ             
         LA    RF,1(,RF)                                                        
         ST    RF,HSEACT                                                        
         SPACE                                                                  
         NOP   CCON0050             BYPASS IT                                   
         SPACE                                                                  
CCON0070 MVC   CDTS(6),RCONDATE                                                 
         DROP  R6                                                               
         SPACE                                                                  
         XC    ELEM,ELEM           ALSO USED AS FLAG - REC IS UPDATED           
         SPACE                                                                  
         XC    CONCTS,CONCTS       THIS WAS IT !$%@#$^%$ MUD                    
         SPACE                                                                  
         SPACE                                                                  
         MVI   ELCODE,X'03'        ADD UP ORDERED                               
         L     R6,AIOAREA                                                       
         BAS   RE,GETEL                                                         
         BNE   CCON0090            NEED TO INCLUDE HOUSE ACCTS                  
*        BNE   CCON0040            BYPASS CONTRACTS WITHOUT ORDERS              
         SPACE                                                                  
         B     CCON0082                                                         
         SPACE                                                                  
CCON0080 BAS   RE,NEXTEL                                                        
         BNE   CCON0090                                                         
         SPACE                                                                  
         USING RCONBKEL,R6                                                      
CCON0082 CLC   BSTRYM,RCONBKYR     THIS BEFORE START DATE                       
         BH    CCON0080             YES, BYPASS                                 
         CLC   BENDYM,RCONBKYR     THIS AFTER END DATE                          
         BL    CCON0080             YES, BYPASS                                 
         SPACE                                                                  
         OC    RCONBKAM,RCONBKAM   ZERO DOLLARS                                 
         BZ    CCON0080             YES, BYPASS                                 
         SPACE                                                                  
         LA    R0,CONCTSN                                                       
         LA    R1,CONCTS                                                        
CCON0084 OC    0(CONCTSL,R1),0(R1)       EMPTY ENTRY                            
         BZ    CCON0086                                                         
         CLC   0(2,R1),RCONBKYR                                                 
         BE    CCON0088                                                         
         LA    R1,CONCTSL(,R1)                                                  
         BCT   R0,CCON0084                                                      
         DC    H'0'                                                             
CCON0086 MVC   0(2,R1),RCONBKYR                                                 
         CLI   RCONBKYR,X'5D'                                                   
         BH    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
CCON0088 ICM   RE,15,2(R1)                                                      
         ICM   RF,15,RCONBKAM                                                   
         AR    RE,RF                                                            
         STCM  RE,15,2(R1)                                                      
         B     CCON0080                                                         
         DROP  R6                                                               
         SPACE                                                                  
CCON0090 BAS   RE,CKAEL            CHECK FOR ACTUAL ELEMENT(S)                  
         SPACE                                                                  
         LA    R5,CONCTSN                                                       
         LA    R6,CONCTS                                                        
CCON0092 OC    0(6,R6),0(R6)                                                    
         BZ    CCON0100                                                         
         SPACE                                                                  
         LA    R0,ARRAYCT                                                       
         LA    R1,ARRAY                                                         
CCON0094 CLC   0(2,R1),0(R6)                                                    
         BE    CCON0096                                                         
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    R1,6(,R1)                                                        
         BCT   R0,CCON0094                                                      
         DC    H'0'                                                             
CCON0096 ICM   RE,15,2(R6)                                                      
         SPACE                                                                  
         ICM   RF,15,2(R1)                                                      
         AR    RF,RE                                                            
         STCM  RF,15,2(R1)                                                      
         SPACE                                                                  
         CLI   6(R6),0             THIS ORDERED                                 
         BE    CCON0097             YES, UPDATE NEEDED                          
         CLI   6(R6),1             THIS ACTUALS                                 
         BE    CCON0098             YES, NO UPDATE NEEDED                       
         DC    H'0'                                                             
         SPACE                                                                  
* BUILD 04 ELEM AND ADD IT TO RECORD                                            
         SPACE                                                                  
CCON0097 LA    R3,ELEM                                                          
         USING RCONSTEL,R3                                                      
         MVI   RCONSTCO,X'04'                                                   
         MVI   RCONSTLN,10                  RCONXEL-RCONSTEL ??                 
         MVC   RCONSTYR(2),0(R6)                                                
         SPACE                                                                  
         MVC   HALF,0(R6)                                                       
         BAS   RE,GETWK                                                         
         MVC   RCONSTWK,HALF                                                    
         SPACE                                                                  
         MVC   RCONSTAM,2(R6)                                                   
         SPACE                                                                  
         OC    RCONSTAM,RCONSTAM   IS AMOUNT ZERO                               
         BZ    CCON0098             YES, DO NOT ADD ELEM                        
         SPACE                                                                  
         L     R0,AIOAREA                                                       
         GOTO1 =V(HELLO),DMCB,(C'P',REPFILE),(R0),(R3),0                        
         CLI   DMCB+12,0           OKAY?                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
CCON0098 ICM   R0,15,2(R6)                                                      
         CVD   R0,DUB                                                           
         CLI   6(R6),0                                                          
         BNE   *+14                                                             
         AP    DORDERED,DUB                                                     
         B     *+10                                                             
         AP    DACTUAL,DUB                                                      
         SPACE                                                                  
         LA    R6,CONCTSL(,R6)                                                  
         BCT   R5,CCON0092                                                      
         DROP  R3                                                               
         SPACE                                                                  
CCON0100 CLI   FIXINV,0            PUT NEW RECORD TO TAPE                       
         BNE   CCON0120                                                         
         MVI   ELCODE,X'01'        CK FOR ACTUAL CONTRACTS                      
         L     R6,AIOAREA                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONELEM,R6                                                      
         TM    RCONMODR+1,X'01'    IS THIS HOUSE ACCT                           
         BO    CCON0130             YES                                         
         OC    ELEM,ELEM           PUT NEW RECORD TO TAPE                       
         BZ    CCON0040             NO, NEVER TOUCHED REC                       
         SPACE                                                                  
* MARK REC AS ACTUALIZED                                                        
         SPACE                                                                  
CCON0120 MVI   ELCODE,X'01'        MARK AS ACTUALIZED                           
         L     R6,AIOAREA                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    RCONMODR+1,X'02'    MARK AS ACTUALIZED                           
         SPACE                                                                  
         TM    RCONMODR+1,X'01'    HOUSE ACCT REC                               
         BZ    CCON0160                                                         
CCON0130 MVI   ELCODE,X'03'                                                     
         L     R6,AIOAREA                                                       
         BAS   RE,GETEL                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'04'                                                     
         L     R6,AIOAREA                                                       
         BAS   RE,GETEL                                                         
         BNE   CCON0140                                                         
         L     RF,HSEUSD           INCREMENT HOUSE ACT HAS 04 ELEM              
         LA    RF,1(RF)                                                         
         ST    RF,HSEUSD                                                        
         B     CCON0160                                                         
         SPACE                                                                  
CCON0140 L     RF,HSEDEL           INCREMENT HOUSE ACT DELETED                  
         LA    RF,1(RF)                                                         
         ST    RF,HSEDEL                                                        
         B     CCON0220                                                         
         SPACE                                                                  
CCON0160 MVI   ELCODE,0                                                         
         SPACE                                                                  
         L     RF,PUTCTR           INCREMENT TOTAL CONTRACTS ACTUALIZED         
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR                                                        
         SPACE                                                                  
         LA    RF,REC              PUT NEW RECORD TO TAPE                       
         LA    R1,2048             JUST IN CASE 2K'S EXIST                      
         L     R6,AIOAREA                                                       
         MOVE  ((RF),(R1)),(R6)             MOVE RECORD TO OUTPUT               
         MVC   REC-4(2),RCONLEN-RCONREC(R6) REC LEN                             
         SPACE                                                                  
         BAS   RE,PUTRECS          GENERATE THE OUTPUT RECORDS FOR CON          
         SPACE                                                                  
         CLI   QUESTOR+2,C'Y'      DISPLAY OUTPUT RECORDS?                      
         BNE   CCON0200            NO                                           
         MVC   IDENT,=CL15'CONTRACT UPDATE'                                     
         L     R6,AIOAREA                                                       
         LA    R6,34(,R6)                                                       
         CLI   0(R6),01                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RCONMODR+1-RCONELEM(R6),X'01'  IS THIS HOUSE ACCT ADDED          
         BZ    *+10                                                             
         MVC  IDENT+8(7),=C'HSE ACT'                                            
         BAS   RE,DISPPUT          YES - DISPLAY OUTPUT RECORDS                 
         SPACE                                                                  
CCON0200 DS   0H                                                                
         CLI   QUESTOR+3,C'Y'      DISPLAY DELETED KEYS?                        
         BNE   CCON0220             NO                                          
         MVC   P+1(08),=C'PRE KEY:'                                             
         GOTO1 HEXOUT,DMCB,KEY,P+10,34,=C'TOG'                                  
         MVC   P+80(23),KEY        INSERT DELETED KEY                           
         GOTO1 REPORT                                                           
         SPACE                                                                  
CCON0220 DS   0H                                                                
         L     RF,DELCTR           INCREMENT DELETED KEYS                       
         LA    RF,1(RF)                                                         
         ST    RF,DELCTR                                                        
         SPACE                                                                  
         CLI   QOPTION3,C'U'       UPDATE?                                      
         BNE   CCON0240             NO                                          
         SPACE                                                                  
* NEED TO GET BACK ORIGINAL REC (DO NOT BLOW OVERFLOW-LARGER REC)               
* JUST WANT TO DELETE IT                                                        
         SPACE                                                                  
         GOTO1 GREC                RETRIEVE CONTRACT RECORD                     
         SPACE                                                                  
         L     R6,AIOAREA                                                       
         OI    RCONCNTL-RCONKEY(R6),X'80'  TURN ON DELETE BIT FOR REC           
         SPACE                                                                  
         GOTO1 PREC                YES - REWRITE REC FOR DELETION               
         SPACE                                                                  
         OI    KEY+27,X'80'                 AND KEY                             
         GOTO1 WRITE               YES - REWRITE KEY FOR DELETION               
         L     RF,DELCTRA          INCREMENT DELETED KEYS                       
         LA    RF,1(RF)                                                         
         ST    RF,DELCTRA                                                       
         SPACE                                                                  
CCON0240 DS   0H                                                                
         CLI   QUESTOR+3,C'Y'      DISPLAY DELETED KEYS?                        
         BNE   CCON0280            NO                                           
         MVC   P+1(08),=C'DEL KEY:'                                             
         GOTO1 HEXOUT,DMCB,KEY,P+10,34,=C'TOG'                                  
         MVC   P+80(23),KEY        INSERT DELETED KEY                           
         GOTO1 REPORT                                                           
CCON0280 DS   0H                                                                
*        LA    R0,ARRAYCT                                                       
*        LA    R1,ARRAY                                                         
*CON0290 XC    2(4,R1),2(R1)                                                    
*        LA    R1,6(,R1)                                                        
*        BCT   R0,CCON0290                                                      
         B     CCON0040            GO BACK FOR NEXT CONTRACT                    
         SPACE                                                                  
* END OF REP, PROCESS NEXT, OR END OF JOB                                       
         SPACE                                                                  
CCON0900 DS   0H                                                                
         L     RF,=A(CARRAY)       CONTRACT TOTALS ARRAY                        
         BAS   RE,ADDARR           ADD FROM ARRAY TO CONT TOT                   
         SPACE                                                                  
         BAS   RE,CKACT            CHECK AGAINST ACTUALIZATION TAPE             
         SPACE                                                                  
         MVI   SVREPSTA+2,X'FF'    FORCE FILE KEY HIGH                          
         SPACE                                                                  
         CLC   AREP(2),0(R2)       SAME REP?                                    
         BE    CCON0900             YES, FINNISH ACTUALS FOR THIS REP           
         SPACE                                                                  
         BAS   RE,REPTOT           GO PRINT TOTALS                              
         SPACE                                                                  
         LA    R2,2(R2)            BUMP TO NEXT REP IN TABLE                    
         CLC   0(2,R2),SPACES      END OF REPS?                                 
         BNE   CCON0020            GO BACK FOR NEXT                             
         SPACE                                                                  
CCON1000 DS   0H                                                                
         SPACE                                                                  
         MVC   P+1(16),=C'LEAVING CONTPROC'                                     
         GOTO1 REPORT                                                           
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
* GET PACKED MONDAY OF WEEK 2ND MONTH AFTER PASSED YR MO                        
         SPACE                                                                  
         DS    0H                                                               
GETWK    NTR1                                                                   
         ZIC   R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         ZIC   R0,HALF+1                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(2),DUB                                                    
         MVC   WORK+4,=C'01'                                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'62'                                     
         MVC   WORK+6+4(2),=C'01'                                               
         MVC   WORK(6),WORK+6                                                   
         GOTO1 GETDAY,(R1),WORK,FULL                                            
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),1                                                          
         BE    GETWK100                                                         
         ZIC   R0,0(R1)                                                         
         LA    R2,8                                                             
         SR    R2,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R2)                                      
GETWK100 GOTO1 DATCON,(R1),(0,WORK+6),(2,HALF)                                  
         XIT1                                                                   
         SPACE 4                                                                
* ADD FROM ARRAY TO A TOTALS ARRAY                                              
         SPACE                                                                  
         DS    0H                                                               
ADDARR   NTR1                                                                   
         LA    R0,ARRAYCT                                                       
         LA    R1,ARRAY                                                         
ADDARR10 ICM   R2,15,2(R1)                                                      
         CVD   R2,DUB                                                           
         AP    2(6,RF),DUB                                                      
         LA    R1,6(,R1)                                                        
         LA    RF,8(,RF)                                                        
         BCT   R0,ADDARR10                                                      
ADDARRX  XIT1                                                                   
         EJECT                                                                  
* PRINT TOTALS FOR REP                                                          
         SPACE                                                                  
         DS    0H                                                               
REPTOT   NTR1                                                                   
         XC    PUTCTR2,PUTCTR2     CLEAR COUNTER FOR LOOP                       
         MVC   P+1(24),=C'CONTRACTS     READ     :'                             
         EDIT  REDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS ACTUALIZED   :'                             
         EDIT  PUTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'HOUSE CONTRACTS ADDED  :'                             
         EDIT  HSECTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ACTUAL RECS READ       :'                             
         EDIT  ACTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         L     R0,REDCTR                                                        
         A     R0,REDCTRT                                                       
         ST    R0,REDCTRT                                                       
         L     R0,ACTCTR                                                        
         A     R0,ACTCTRT                                                       
         ST    R0,ACTCTRT                                                       
         L     R0,PUTCTR                                                        
         A     R0,PUTCTRT                                                       
         ST    R0,PUTCTRT                                                       
         L     R0,HSECTR                                                        
         A     R0,HSECTRT                                                       
         ST    R0,HSECTRT                                                       
         XC    REDCTR,REDCTR                                                    
         XC    ACTCTR,ACTCTR                                                    
         XC    PUTCTR,PUTCTR                                                    
         XC    HSECTR,HSECTR                                                    
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  CKAEL: SEE IF THERE IS AN ACTUAL (X'04') ELEMENT FOR THE 03   *              
*  ORDERED ELEMENTS PASSED TO THIS RTN IN CONCTS TABLE           *              
******************************************************************              
*                                                                               
         DS    0H                                                               
CKAEL    NTR1                                                                   
         SPACE                                                                  
         LA    R0,CONCTSN                                                       
         LA    R1,CONCTS                                                        
         SR    R2,R2                                                            
CKAEL010 OC    0(6,R1),0(R1)                                                    
         BZ    CKAEL014                                                         
         LA    R1,CONCTSL(,R1)                                                  
         LA    R2,1(,R2)                                                        
         BCT   R0,CKAEL010                                                      
         SPACE                                                                  
* SORT TABLE IN YR/MO ORDER                                                     
         SPACE                                                                  
CKAEL014 LTR   R2,R2                                                            
         BZ    CKAEL016                                                         
         SPACE                                                                  
         GOTO1 =V(QSORT),DMCB,CONCTS,(R2),CONCTSL,2,0                           
         SPACE                                                                  
CKAEL016 MVI   BYTE,0                                                           
         MVI   ELCODE,X'04'        FIND ACTUAL                                  
         L     R6,AIOAREA                                                       
         BAS   RE,GETEL                                                         
         BNE   CKAELX                                                           
         B     CKAEL030                                                         
         SPACE                                                                  
CKAEL020 BAS   RE,NEXTEL                                                        
         BNE   CKAEL070                                                         
         SPACE                                                                  
         USING RCONSTEL,R6                                                      
CKAEL030 CLC   BSTRYM,RCONSTYR     THIS BEFORE START DATE                       
         BH    CKAEL020             YES, BYPASS                                 
         CLC   BENDYM,RCONSTYR     THIS AFTER END DATE                          
         BL    CKAEL020             YES, BYPASS                                 
         SPACE                                                                  
         CLC   CSTDT,RCONSTYR      CK VS CONTRACT START YR/MO                   
         BH    CKAEL034             CHECK IF ZERO                               
         SPACE                                                                  
         CLC   CEDDT,RCONSTYR                                                   
         BNL   CKAEL036             BYPASS ZERO CHECK                           
         SPACE                                                                  
* THERE ARE ALL SORTS OF ZERO $ 04 ELEMS FROM CLOSED STATIONS -                 
* CLOSE CREATES ZERO 04 ELEMS FROM CONTRACT DATE TO DATE OF CLOSEOUT            
         SPACE                                                                  
CKAEL034 OC    RCONSTAM,RCONSTAM   IF ZERO DOLLARS                              
         BZ    CKAEL020             BYPASS                                      
         SPACE                                                                  
CKAEL036 LA    R0,CONCTSN                                                       
         LA    R1,CONCTS                                                        
CKAEL040 OC    0(6,R1),0(R1)        EMPTY SLOT                                  
         BZ    CKAEL060                                                         
         CLC   RCONSTYR(2),0(R1)    THIS SAME YR/MO OF SERVICE                  
         BE    CKAEL064              NO                                         
         LA    R1,CONCTSL(,R1)                                                  
         BCT   R0,CKAEL040                                                      
         DC    H'0'                                                             
         SPACE                                                                  
CKAEL060 MVC   0(2,R1),RCONSTYR    FILL IN YR/MO                                
         MVI   BYTE,1              INDICATE SORT NEEDED                         
         MVI   6(R1),1             INDICATE ACTUAL FOUND                        
         B     CKAEL066                                                         
         SPACE                                                                  
CKAEL064 CLI   6(R1),0             ALREADY FOUND ACTUAL                         
         BNE   CKAEL066             YES                                         
         SPACE                                                                  
* MAY NOT BE DOLLARS - THERE WERE 03'S WITH EQ +/- DOLLARS                      
         SPACE                                                                  
         XC    2(4,R1),2(R1)       CLEAR 03 ELEM TOTALS                         
         MVI   6(R1),1             INDICATE ACTUAL FOUND                        
         SPACE                                                                  
CKAEL066 ICM   RE,15,RCONSTAM                                                   
         ICM   RF,15,2(R1)                                                      
         AR    RF,RE                                                            
         STCM  RF,15,2(R1)                                                      
         B     CKAEL020                                                         
         SPACE                                                                  
CKAEL070 CLI   BYTE,0              DOES TABLE HAVE TO BE RESORTED               
         BE    CKAELX               NO                                          
         LA    R0,CONCTSN                                                       
         LA    R1,CONCTS                                                        
         SR    R2,R2                                                            
CKAEL074 OC    0(6,R1),0(R1)                                                    
         BZ    CKAEL076                                                         
         LA    R1,CONCTSL(,R1)                                                  
         LA    R2,1(,R2)                                                        
         BCT   R0,CKAEL074                                                      
         SPACE                                                                  
* SORT TABLE IN YR/MO ORDER                                                     
         SPACE                                                                  
CKAEL076 LTR   R2,R2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         GOTO1 =V(QSORT),DMCB,CONCTS,(R2),CONCTSL,2,0                           
         SPACE                                                                  
CKAELX   DS   0H                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  CKACT: READ ACTUAL TAPE, COMPARE TO SAVED REP/STA/OFF, IF     *              
*  DIFFERENCE, PUT OUT NEW CONTRACT REC WITH DIFFERENCE, IF NO   *              
*  REP/STA/OFF, DISPLAY                                          *              
******************************************************************              
*                                                                               
         DS    0H                                                               
CKACT    NTR1                                                                   
         SPACE                                                                  
         ZAP   CONDOL,=P'0'                                                     
         ZAP   ACTDOL,=P'0'                                                     
         SPACE                                                                  
         CLI   RDACTSW,C'Y'        NEED A RECORD READ                           
         BNE   CKACT020                                                         
         SPACE                                                                  
CKACT010 GET   INTAPE,ACTREC                                                    
         SPACE                                                                  
         L     R1,ACTCTR                                                        
         LA    R1,1(,R1)                                                        
         ST    R1,ACTCTR                                                        
         ZAP   ACTDOL,=P'0'                                                     
         SPACE                                                                  
         CLC   SVRPSTAC,AREP       CHANGE IN STATION                            
         BE    *+8                                                              
         BAS   RE,STATOT           PRINT STATION TOTALS                         
         MVC   SVRPSTAC,AREP       CHANGE IN STATION                            
         SPACE                                                                  
         MVI   RDACTSW,C'N'        DON'T NEED A RECORD READ                     
         SPACE                                                                  
         CLI   QUESTOR+1,C'Y'      DISPLAY ACTUALIZED RECS                      
         BNE   CKACT014             NO                                          
         MVC   P+20(02),AREP       REP                                          
         MVC   P+25(05),ACALL1     STATION                                      
         MVC   P+34(02),AOFFICE    OFFICE                                       
         MVC   P+40(10),=C'ACTUAL REC'                                          
         GOTO1 REPORT                                                           
         LA    R4,AREP             A(RECORD)                                    
         LA    RF,256              GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         SPACE                                                                  
CKACT014 DS   0H                                                                
*        CLC   =C'V3',SVREPSTA     SEE IF TEST FILE                             
*        BNE   CKACT020                                                         
*        CLC   =C'EA',AREP         SEE IF TEST FILE                             
*        BNE   CKACT010                                                         
         SPACE                                                                  
*        MVC   AREP,=C'V3'                                                      
         SPACE                                                                  
CKACT020 CLI   AREP,X'FF'          AT EOF OF ACTUAL FILE                        
         BE    CKACT700             YES                                         
         SPACE                                                                  
         CLC   AREP,0(R2)          IS REP WE ARE WORKING ON                     
         BNE   CKACTX               NO                                          
         SPACE                                                                  
*        L     R1,ACTCTR                                                        
*        LA    R1,1(,R1)                                                        
*        ST    R1,ACTCTR                                                        
         SPACE                                                                  
         CLC   SVREPSTA,AREP      COMPARE TO KATZ ACTUALIZATION REC             
         BH    CKACT600             NO CONTRACTS FOR KATZ ACTUAL                
         BL    CKACT700             NO ACTUAL FOR CONTRACTS                     
         SPACE                                                                  
         L     R1,EQRECCT                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,EQRECCT                                                       
         SPACE                                                                  
         LR    R3,R2                                                            
         L     R2,=A(KARRAY)       KATZ ACTUALS TOTALS                          
         BAS   RE,ADDACT           ADD TO ACTUALS ARRAY                         
         LR    R2,R3                                                            
         SPACE                                                                  
* CK CTRS FOR MATCH, IF NOT, CREATE NEW CONTRACT TO BALANCE                     
         SPACE                                                                  
         MVI   RDACTSW,C'Y'        NEED A RECORD READ                           
         XC    ELEM,ELEM                                                        
         SPACE                                                                  
         LA    RE,ELEM                                                          
         SPACE                                                                  
* CONVERT 96 JUNE TO BINARY                                                     
         SPACE                                                                  
         LA    R0,ARRAYCT                                                       
         LA    R1,ACTRS+48                                                      
CKACT110 ZAP   DUB,0(4,R1)                                                      
         CVB   RF,DUB                                                           
         MH    RF,=H'100'          CONVERT DOLLARS TO DOLLARS/CENTS             
         ST    RF,0(,RE)                                                        
         LA    R1,4(,R1)                                                        
         LA    RE,4(,RE)                                                        
         BCT   R0,CKACT110                                                      
         SPACE                                                                  
* NOW COMPARE KATZ ACTUALS TO REP ARRAY                                         
         SPACE                                                                  
         LA    R0,ARRAYCT                                                       
         LA    R3,ARRAY                                                         
         LA    RE,ELEM                                                          
         MVI   ELCODE,0            SET TO MATCHED                               
         SPACE                                                                  
CKACT120 ICM   RF,15,2(R3)         CONTRACT TOTALS                              
         CVD   RF,DUB                                                           
         AP    CONDOL,DUB                                                       
         AP    SCONDOL,DUB                                                      
         L     R1,0(RE)            ACTUAL TOTALS                                
         CVD   R1,DUB                                                           
         AP    ACTDOL,DUB                                                       
         AP    SACTDOL,DUB                                                      
         SR    R1,RF               SUBTRACT CONTRACT FROM ACTUAL                
         BZ    CKACT140                                                         
         SPACE                                                                  
         MVI   ELCODE,1            SET FOUND UNMATCHED                          
         SPACE                                                                  
CKACT140 STCM  R1,15,2(R3)                                                      
         LA    R3,6(,R3)                                                        
         LA    RE,4(,RE)                                                        
         SPACE                                                                  
         CLC   BENDYM,0(R3)        CK AS AT DATE CUTOFF                         
         BL    CKACT144                                                         
         SPACE                                                                  
         BCT   R0,CKACT120                                                      
         SPACE                                                                  
* IF TOTALS ARE EQUAL, ALL DONE                                                 
         SPACE                                                                  
CKACT144 CLI   ELCODE,0            IF NON=ZERO, TOTALS DO NOT MATCH             
         BNE   CKACT200                                                         
         SPACE                                                                  
         L     R1,BALRECCT                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,BALRECCT                                                      
         MVC   PREP(02),SVREPSTA   REP                                          
         MVC   PSTA(05),SVREPSTA+2 STATION                                      
         MVC   POFF(02),SVREPSTA+7 OFFICE                                       
         EDIT  CONDOL,(13,PCONDOL),2,COMMAS=YES,MINUS=YES                       
         EDIT  ACTDOL,(13,PACTDOL),2,COMMAS=YES,MINUS=YES                       
         MVC   PREASON(24),=C'ACTUAL AGREES WITH CONTR'                         
         GOTO1 REPORT                                                           
         LA    R0,ARRAYCT                                                       
         LA    R1,ARRAY                                                         
CKACT150 XC    2(4,R1),2(R1)                                                    
         LA    R1,6(,R1)                                                        
         BCT   R0,CKACT150                                                      
         SPACE                                                                  
         B     CKACTX                                                           
         SPACE                                                                  
* CONTRACT & ACTUAL TOTALS DON'T MATCH, GO CREATE HOUSE ACCT CONTRACT           
         SPACE                                                                  
CKACT200 DS    0H                                                               
         CLI   QUESTOR+5,C'Y'      DISPLAY OUTPUT RECORDS?                      
         BNE   CKACT300            NO                                           
         BAS   RE,DISPUNM          YES - DISPLAY UNMATCHED                      
         SPACE                                                                  
CKACT300 DS    0H                                                               
         BAS   RE,CKSTA                                                         
         SPACE                                                                  
         BAS   RE,CREATE           CREATE HOUSE CONTRACT(S) FOR DIFF            
         SPACE                                                                  
         MVC   PREP(02),SVREPSTA   REP                                          
         MVC   PSTA(05),SVREPSTA+2 STATION                                      
         MVC   POFF(02),SVREPSTA+7 OFFICE                                       
         MVC   POFF+3(02),SVGRP    GROUP                                        
         EDIT  CONDOL,(13,PCONDOL),2,COMMAS=YES,MINUS=YES                       
         EDIT  ACTDOL,(13,PACTDOL),2,COMMAS=YES,MINUS=YES                       
         SP    ACTDOL,CONDOL                                                    
         EDIT  ACTDOL,(13,PDIFDOL),2,COMMAS=YES,MINUS=YES                       
         MVC   PREASON(29),=C'ACTUAL DIFFERS WITH CONTRACTS'                    
         GOTO1 REPORT                                                           
         B     CKACTX                                                           
         SPACE                                                                  
* THIS ACT REC HAD NO MATCH ON DDS REP FILE, PRINT ERROR,                       
* CREATE HOUSE ACCT CONTRACT                                                    
         SPACE                                                                  
CKACT600 DS    0H                                                               
         CLI   AREP,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LR    R3,R2                                                            
         L     R2,=A(KARRAY)       KATZ ACTUALS TOTALS                          
         BAS   RE,ADDACT           ADD TO ACTUALS ARRAY                         
         LR    R2,R3                                                            
         SPACE                                                                  
         L     R1,UNACTCT                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,UNACTCT                                                       
         SPACE                                                                  
         LA    R0,SVARRAY                                                       
         LA    R1,248                                                           
         LA    RE,ARRAY                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*        MVC   SVARRAY(144),ARRAY                                               
*        MVC   SVARRAY+144(144),ARRAY+144                                       
         MVC   HDREPSTA,SVREPSTA                                                
         SPACE                                                                  
         MVC   SVREPSTA,AREP                                                    
         SPACE                                                                  
         BAS   RE,CKSTA                                                         
         SPACE                                                                  
* ZERO OUT ENTIRE ARRAY                                                         
         SPACE                                                                  
         LA    R0,ARRAYCT                                                       
         LA    R1,ARRAY                                                         
CKACT610 XC    2(4,R1),2(R1)                                                    
         LA    R1,6(,R1)                                                        
         BCT   R0,CKACT610                                                      
         SPACE                                                                  
* FILL ARRAY WITH TOTALS FROM ACTUAL REC                                        
         SPACE                                                                  
* CONVERT JAN-DEC 96 TO BINARY AND DOLLARS & CENTS                              
         SPACE                                                                  
         LA    R0,ARRAYCT                                                       
         LA    R1,ACTRS+48                                                      
         LA    RE,ARRAY                                                         
CKACT640 ZAP   DUB,0(4,R1)                                                      
         CVB   RF,DUB                                                           
         MH    RF,=H'100'          CONVERT DOLLARS TO DOLLARS/CENTS             
         STCM  RF,15,2(RE)                                                      
         SPACE                                                                  
         CVD   RF,DUB                                                           
         AP    ACTDOL,DUB                                                       
         AP    SACTDOL,DUB                                                      
         SPACE                                                                  
         LA    R1,4(,R1)                                                        
         LA    RE,6(,RE)                                                        
         BCT   R0,CKACT640                                                      
         SPACE                                                                  
         BAS   RE,CREATE           CREATE HOUSE CONTRACT(S) FOR ACTUAL          
         SPACE                                                                  
         MVC   PREP(02),AREP       RREP                                         
         MVC   PSTA(05),ACALL1     STATION                                      
         MVC   POFF(02),AOFFICE    OFFICE                                       
         MVC   POFF+3(02),SVGRP    GROUP                                        
         EDIT  ACTDOL,(13,PACTDOL),2,COMMAS=YES,MINUS=YES                       
         MVC   PREASON(32),=C'NO MATCH ON REP FOR THIS ACT REC'                 
         GOTO1 REPORT                                                           
         SPACE                                                                  
* RESTORE CONTRACT DATA                                                         
         SPACE                                                                  
         MVC   SVREPSTA,HDREPSTA                                                
         LA    R0,SVARRAY                                                       
         LA    R1,248                                                           
         LA    RE,ARRAY                                                         
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*        MVC   ARRAY(144),SVARRAY                                               
*        MVC   ARRAY+144(144),SVARRAY+144                                       
         B     CKACT010             GET NEXT REC                                
         SPACE                                                                  
* NO ACTUAL FOR THIS CONTR GRP, CREATE HOUSE CONTRACT FOR ENTIRE AMT *          
         SPACE                                                                  
CKACT700 DS    0H                                                               
         MVI   RDACTSW,C'N'         DON'T NEED A RECORD READ                    
         SPACE                                                                  
         CLI   SVREPSTA+2,X'FF'    THIS END OF REP CONTRACTS                    
         BE    CKACTX               YES                                         
         SPACE                                                                  
         BAS   RE,CKSTA                                                         
         SPACE                                                                  
         L     R1,UNCONCT                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,UNCONCT                                                       
         SPACE                                                                  
         LA    R0,ARRAYCT                                                       
         LA    R1,ARRAY                                                         
CKACT740 ICM   RE,15,2(R1)                                                      
         BZ    CKACT750                                                         
         SPACE                                                                  
         CVD   RE,DUB                                                           
         AP    CONDOL,DUB                                                       
         SPACE                                                                  
         LCR   RE,RE                                                            
         STCM  RE,15,2(R1)                                                      
CKACT750 LA    R1,6(,R1)                                                        
         BCT   R0,CKACT740                                                      
         SPACE                                                                  
         BAS   RE,CREATE           CREATE HOUSE CONTRACT(S) FOR DIFF            
         SPACE                                                                  
         LA    R0,ARRAYCT                                                       
         LA    R1,ARRAY                                                         
CKACT760 XC    2(4,R1),2(R1)                                                    
         LA    R1,6(,R1)                                                        
         BCT   R0,CKACT760                                                      
         SPACE                                                                  
         MVC   PREP(02),SVREPSTA    RREP                                        
         MVC   PSTA(05),SVREPSTA+2  STATION                                     
         MVC   POFF(02),SVREPSTA+7  OFFICE                                      
         MVC   POFF+3(02),SVGRP     GROUP                                       
         EDIT  CONDOL,(13,PCONDOL),2,COMMAS=YES,MINUS=YES                       
         MVC   PREASON(32),=C'NO MATCH ON ACT FOR THIS REP/STA'                 
         GOTO1 REPORT                                                           
         SPACE                                                                  
CKACTX   XIT1                                                                   
         SPACE                                                                  
* END OF ACTUALS FILE *                                                         
         SPACE                                                                  
CKACTEOF MVI   RDACTSW,C'N'        SET OFF REC NEEDED                           
         MVI   AREP,X'FF'                                                       
         B     CKACT020                                                         
         EJECT                                                                  
* PRINT STATION TOTALS AND ZERO                                                 
         SPACE                                                                  
STATOT   NTR1                                                                   
         CP    SCONDOL,=P'0'                                                    
         BNE   STATOT20                                                         
         CP    SACTDOL,=P'0'                                                    
         BE    CKACTX                                                           
STATOT20 EDIT  SCONDOL,(15,PCONDOL),2,COMMAS=YES,MINUS=YES                      
         EDIT  SACTDOL,(14,PACTDOL),2,COMMAS=YES,MINUS=YES                      
         SP    SACTDOL,SCONDOL                                                  
         EDIT  SACTDOL,(13,PDIFDOL),2,COMMAS=YES,MINUS=YES                      
         GOTO1 REPORT                                                           
         ZAP   SACTDOL,=P'0'                                                    
         ZAP   SCONDOL,=P'0'                                                    
         B     CKACTX                                                           
         EJECT                                                                  
* ADD ACTUAL TOTALS TO ARRAY                                                    
*                                                                               
         DS    0H                                                               
ADDACT   NTR1                                                                   
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         SPACE                                                                  
* CONVERT 96 TO BINARY - JAN - DEC                                              
         SPACE                                                                  
         LA    R0,ARRAYCT                                                       
         LA    R1,ACTRS+48                                                      
ADDACT20 ZAP   DUB,0(4,R1)                                                      
         MP    DUB,=P'100'                                                      
         AP    2(6,R2),DUB                                                      
         AP    KACTUAL,DUB                                                      
         LA    R1,4(,R1)                                                        
         LA    R2,8(,R2)                                                        
         BCT   R0,ADDACT20                                                      
         XIT1                                                                   
         SPACE                                                                  
* SEE IF THERE IS A STATION FOR THE ADDED HOUSE ACCT                            
         SPACE                                                                  
CKSTA    NTR1                                                                   
         XC    SVGRP,SVGRP                                                      
         SPACE                                                                  
         LA    R0,MISTABCT                                                      
         L     R1,=A(MISTAB)                                                    
CKSTA20  OC    0(MISTABLN,R1),0(R1)                                             
         BZ    CKSTA40                                                          
         CLC   0(MISTABLN,R1),SVREPSTA                                          
         BE    CKSTAX                                                           
         LA    R1,MISTABLN(,R1)                                                 
         BCT   R0,CKSTA20                                                       
         DC    H'0'                                                             
CKSTA40  DS    0H                                                               
         MVC   KEYWORK,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSTARECD,R4                                                      
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,SVREPSTA                                                
         MVC   RSTAKSTA,SVREPSTA+2                                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    CKSTA60                                                          
         LA    R0,MISTABCT                                                      
         L     R1,=A(MISTAB)                                                    
CKSTA50  OC    0(MISTABLN,R1),0(R1)                                             
         BZ    CKSTA54                                                          
         LA    R1,MISTABLN(,R1)                                                 
         BCT   R0,CKSTA50                                                       
         DC    H'0'                                                             
         SPACE                                                                  
CKSTA54  MVC   0(MISTABLN,R1),SVREPSTA                                          
         MVC   PREASON+34(21),=C'** MISSING STATION **'                         
         MVC   PDIFDOL(2),SVREPSTA                                              
         MVC   PDIFDOL+3(5),SVREPSTA+2                                          
         GOTO1 REPORT                                                           
         SPACE                                                                  
         MVC   KEY,KEYWORK                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    CKSTAX                                                           
         DC    H'0'                                                             
CKSTA60  DS   0H                                                                
         L     R3,AIOAREA                                                       
         L     R6,=A(RECIO)                                                     
         ST    R6,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVI   ELCODE,X'01'        GET GROUP CODE                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVGRP,RSTAGRUP-RSTAELEM(R6)                                      
         SPACE                                                                  
         MVC   KEY,KEYWORK                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    CKSTA80                                                          
         DC    H'0'                                                             
CKSTA80  GOTO1 GREC                                                             
         ST    R3,AIOAREA                                                       
         SPACE                                                                  
CKSTAX   XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CREATE:                                                       *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
         DS    0H                                                               
CREATE   NTR1                                                                   
         LA    R2,ARRAYCT                                                       
         LA    R3,ARRAY                                                         
CREAT100 OC    2(4,R3),2(R3)     ANY DIFFERENCE                                 
         BNZ   CREAT120                                                         
         SPACE                                                                  
         LA    R3,6(,R3)                                                        
         BCT   R2,CREAT100                                                      
         B     CREAT800                                                         
         SPACE                                                                  
CREAT120 L     R4,=A(SKEL96)                                                    
*        CLI   0(R3),93                                                         
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*        LA    R4,SKELEN(,R4)                                                   
*        CLI   0(R3),94                                                         
*        BE    CREAT140                                                         
*        LA    R4,SKELEN(,R4)                                                   
*        CLI   0(R3),95                                                         
*        BE    CREAT140                                                         
*        LA    R4,SKELEN(,R4)                                                   
         CLI   0(R3),96                                                         
         BE    CREAT140                                                         
         DC    H'0'                                                             
CREAT140 LA    R0,REC                                                           
         LA    R1,2000                                                          
         LR    RE,R4                                                            
         LA    RF,SKELEN                                                        
         MVCL  R0,RE                                                            
         MVC   BYTE,0(R3)          SAVE YEAR                                    
         SPACE                                                                  
* NOW HAVE CONTRACT SKELETON IN REC AREA                                        
         SPACE                                                                  
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
         MVC   RCONKREP,SVREPSTA                                                
         MVC   RCONKSTA,SVREPSTA+2                                              
         MVC   RCONKOFF,SVREPSTA+7                                              
         AP    SVCON,=P'10'                                                     
         MVC   RCONKCON,SVCON                                                   
         SPACE                                                                  
         MVC   RCONKGRP,SVGRP                                                   
         SPACE                                                                  
* BUILD LIST OF ADDED HOUSE ACCT RECS                                           
         SPACE                                                                  
         L     RF,ANXTHSE                                                       
         LA    R0,HSELSTLN(,RF)                                                 
         S     R0,AHSELIST                                                      
         C     R0,LHSELIST                                                      
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   0(9,RF),SVREPSTA                                                 
         MVC   9(4,RF),SVCON                                                    
         MVC   13(1,RF),0(R3)      SAVE YEAR                                    
         LA    RF,HSELSTLN(,RF)                                                 
         ST    RF,ANXTHSE                                                       
         DROP  R6                                                               
         MVI   ELCODE,01                                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONELEM,R6                                                      
         MVC   RCONSAL+1(2),SVREPSTA+7 PUT OFFICE IN LAST 2 OF SALES            
         SPACE                                                                  
* MARK REC AS HOUSE ACCT ADDED                                                  
         SPACE                                                                  
         OI    RCONMODR+1,X'01'    MARK AS HOUSE ACCT ADDED                     
         DROP  R6                                                               
         SPACE                                                                  
CREAT160 CLC   BYTE,0(R3)          SAME YEAR                                    
         BNE   CREAT180                                                         
         SPACE                                                                  
         OC    2(4,R3),2(R3)       ANY DIFFERENCE                               
         BZ    CREAT170                                                         
         SPACE                                                                  
* BUILD 04 ELEM AND ADD IT TO RECORD                                            
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RCONSTEL,R6                                                      
         MVI   RCONSTCO,X'04'                                                   
         MVI   RCONSTLN,10   RCONXEL-RCONSTEL ??                                
         MVC   RCONSTYR(2),0(R3)                                                
         SPACE                                                                  
         MVC   HALF,0(R3)                                                       
         BAS   RE,GETWK                                                         
         MVC   RCONSTWK,HALF                                                    
         SPACE                                                                  
         MVC   RCONSTAM,2(R3)                                                   
         ICM   R0,15,2(R3)                                                      
         CVD   R0,DUB                                                           
         AP    HACTUAL,DUB                                                      
         SPACE                                                                  
         LA    R0,REC                                                           
         GOTO1 =V(HELLO),DMCB,(C'P',REPFILE),(R0),(R6),0                        
         CLI   DMCB+12,0           OKAY?                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
CREAT170 LA    R3,6(,R3)                                                        
         BCT   R2,CREAT160                                                      
         SPACE                                                                  
* NEED TO WRITE REC TO TAPE HERE                                                
         SPACE                                                                  
CREAT180 DS   0H                                                                
         SPACE                                                                  
         L     RF,HSECTR           INCREMENT HOUSE CONTRACTS                    
         LA    RF,1(RF)                                                         
         ST    RF,HSECTR                                                        
         SPACE                                                                  
         LA    R6,REC                                                           
         USING RCONREC,R6                                                       
         MVC   REC-4(2),RCONLEN    INSERT LENGTH INTO OUTPUT                    
         DROP  R6                                                               
         BAS   RE,PUTRECS          GENERATE OUTPUT RECS FOR HSE CON             
         SPACE                                                                  
         CLI   QUESTOR+2,C'Y'      DISPLAY OUTPUT RECORDS?                      
         BNE   CREAT200            NO                                           
         MVC   IDENT,=CL15'HOUSE CONT ADD '                                     
         BAS   RE,DISPPUT          YES - DISPLAY OUTPUT RECORDS                 
         SPACE                                                                  
CREAT200 LTR   R2,R2               ANY BUCKETS LEFT                             
         BNZ   CREAT100                                                         
         SPACE                                                                  
* ADD ARRAY TO HOUSE ACCT ARRAY, THEN ZERO IT OUT                               
         SPACE                                                                  
CREAT800 L     RF,=A(HARRAY)       HOUSE ARRAY TOTALS                           
         BAS   RE,ADDARR           ADD FROM ARRAY TO CONT TOT                   
         SPACE                                                                  
         LA    R0,ARRAYCT                                                       
         LA    R1,ARRAY                                                         
CREAT900 XC    2(4,R1),2(R1)                                                    
         LA    R1,6(,R1)                                                        
         BCT   R0,CREAT900                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPTOTS:                                                     *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
         DS    0H                                                               
DISPTOTS NTR1                                                                   
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'CONTRACTS     READ     :'                             
         EDIT  REDCTRT,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS ACTUALIZED   :'                             
         EDIT  PUTCTRT,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS CLEARED      :'                             
         EDIT  CONCLR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ACTUAL RECS   READ     :'                             
         EDIT  ACTCTRT,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'HOUSE CONTRACTS ADDED  :'                             
         EDIT  HSECTRT,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'HOUSE ACTS READ        :'                             
         EDIT  HSEACT,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'HOUSE CLEARED          :'                             
         EDIT  HSECLR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'HOUSE ACTIVE           :'                             
         EDIT  HSEUSD,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'HOUSE DELETED          :'                             
         EDIT  HSEDEL,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS/ACTUAL MATCH :'                             
         EDIT  EQRECCT,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'UNMATCHED ACTUAL RECS  :'                             
         EDIT  UNACTCT,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS/ACTUAL IN BAL:'                             
         EDIT  BALRECCT,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'UNMATCHED CONTRT GRPS  :'                             
         EDIT  UNCONCT,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS WITH BAD 04 E:'                             
         EDIT  BAD04CTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'KEYS TO BE DELETED     :'                             
         EDIT  DELCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'KEYS DELETED           :'                             
         EDIT  DELCTRA,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+09(15),=C'CONTRACT TOTALS'                                     
*        MVC   P+28(19),=C'KATZ MATCHED TOTALS'                                 
*        MVC   P+50(17),=C'HOUSE ACCT TOTALS'                                   
*        MVC   P+71(18),=C'KATZ ACTUAL TOTALS'                                  
         MVC   P+30(17),=C'HOUSE ACCT TOTALS'                                   
         MVC   P+49(18),=C'KATZ ACTUAL TOTALS'                                  
         GOTO1 REPORT                                                           
         L     R3,=A(CARRAY)       CONTRACT ARRAY TOTALS                        
*        L     R4,=A(KARRAYM)      KATZ ACTUALS TOTALS (MATCHED)                
         L     R4,=A(HARRAY)       HOUSE ACCTS TOTALS                           
         L     R5,=A(KARRAY)       KATZ ACTUALS TOTALS                          
         BAS   RE,PRTOT                                                         
         GOTO1 REPORT                                                           
         MVC   P+5(16),=C'MISSING STATIONS'                                     
         GOTO1 REPORT                                                           
         LA    R0,100                                                           
         L     R2,=A(MISTAB)                                                    
DISPT04  OC    0(7,R2),0(R2)                                                    
         BZ    DISPT06                                                          
         MVC   P+5(2),0(R2)                                                     
         MVC   P+10(5),2(R2)                                                    
         GOTO1 REPORT                                                           
         LA    R2,7(,R2)                                                        
         BCT   R0,DISPT04                                                       
         DC    H'0'                                                             
DISPT06  DS    0H                                                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNEND           SAVE END   TIME                              
         MVC   P+1(16),=C'START/END TIMES:'                                     
         GOTO1 HEXOUT,DMCB,RUNSTRT,P+20,4,=C'TOG'                               
         MVI   P+28,C'/'                                                        
         GOTO1 HEXOUT,DMCB,RUNEND,P+29,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         SPACE                                                                  
* NOW PRINT LIST OF HOUSE ACCTG CONTRACTS ADDED *                               
         SPACE                                                                  
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         L     R2,AHSELIST                                                      
DISPT10  MVC   PREP,0(R2)          REP                                          
         MVC   PSTA(05),2(R2)      STATION                                      
         MVC   POFF(02),7(R2)      OFFICE                                       
         GOTO1 HEXOUT,DMCB,9(R2),PCON,4,=C'TOG'                                 
         ZIC   R0,13(R2)                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(2),DUB                                                      
         SPACE                                                                  
         GOTO1 REPORT                                                           
         LA    R2,HSELSTLN(,R2)                                                 
         OC    0(13,R2),0(R2)                                                   
         BNZ   DISPT10                                                          
         SPACE                                                                  
         XIT1                                                                   
* PRINT TOTALS FROM ARRAY                                                       
         SPACE                                                                  
         DS    0H                                                               
PRTOT    NTR1                                                                   
         GOTO1 REPORT                                                           
         LA    R2,ARRAYCT                                                       
PRTOT10  ZIC   R0,0(R3)            DISPLAY YEAR                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+2(2),DUB                                                       
         ZIC   R0,1(R3)            DISPLAY MONTH                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(2),DUB                                                       
         SPACE                                                                  
         AP    TDACTUAL,2(6,R3)                                                 
         EDIT  (P6,2(R3)),(17,P+7),2,COMMAS=YES,MINUS=YES                       
         SPACE                                                                  
         AP    THACTUAL,2(6,R4)                                                 
         EDIT  (P6,2(R4)),(17,P+30),2,COMMAS=YES,MINUS=YES                      
         SPACE                                                                  
         AP    TKACTUAL,2(6,R5)                                                 
         EDIT  (P6,2(R5)),(17,P+50),2,COMMAS=YES,MINUS=YES                      
         SPACE                                                                  
*        EDIT  (P6,2(R6)),(17,P+70),2,COMMAS=YES,MINUS=YES                      
         GOTO1 REPORT                                                           
         LA    R3,8(,R3)                                                        
         LA    R4,8(,R4)                                                        
         LA    R5,8(,R5)                                                        
*        LA    R6,8(,R6)                                                        
         BCT   R2,PRTOT10                                                       
         SPACE                                                                  
         MVI   P+3,C'T'                                                         
*        EDIT  (P6,DACTUAL),(17,P+7),2,COMMAS=YES,MINUS=YES                     
         MVC   P+6(18),=X'4020202020206B2020206B2020214B202060'                 
         ED    P+6(18),TDACTUAL                                                 
         SPACE                                                                  
*        EDIT  (P6,HACTUAL),(17,P+30),2,COMMAS=YES,MINUS=YES                    
         MVC   P+29(18),=X'4020202020206B2020206B2020214B202060'                
         ED    P+29(18),THACTUAL                                                
         SPACE                                                                  
*        EDIT  (P6,KACTUAL),(17,P+50),2,COMMAS=YES,MINUS=YES                    
         MVC   P+49(18),=X'4020202020206B2020206B2020214B202060'                
         ED    P+49(18),TKACTUAL                                                
         SPACE                                                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         SPACE                                                                  
         MVI   P+3,C'O'                                                         
*        EDIT  (P6,DORDERED),(17,P+7),2,COMMAS=YES,MINUS=YES                    
         MVC   P+6(18),=X'4020202020206B2020206B2020214B202060'                 
         ED    P+6(18),DORDERED                                                 
         SPACE                                                                  
         GOTO1 REPORT                                                           
         MVI   P+3,C'A'                                                         
*        EDIT  (P6,DACTUAL),(17,P+7),2,COMMAS=YES,MINUS=YES                     
         MVC   P+6(18),=X'4020202020206B2020206B2020214B202060'                 
         ED    P+6(18),DACTUAL                                                  
         SPACE                                                                  
*        EDIT  (P6,HACTUAL),(17,P+30),2,COMMAS=YES,MINUS=YES                    
         MVC   P+29(18),=X'4020202020206B2020206B2020214B202060'                
         ED    P+29(18),HACTUAL                                                 
         SPACE                                                                  
*        EDIT  (P6,KACTUAL),(17,P+50),2,COMMAS=YES,MINUS=YES                    
         MVC   P+49(18),=X'4020202020206B2020206B2020214B202060'                
         ED    P+49(18),KACTUAL                                                 
         SPACE                                                                  
         GOTO1 REPORT                                                           
         AP    DACTUAL,DORDERED                                                 
         MVI   P+3,C'T'                                                         
*        EDIT  (P6,DACTUAL),(17,P+7),2,COMMAS=YES,MINUS=YES                     
         MVC   P+6(18),=X'4020202020206B2020206B2020214B202060'                 
         ED    P+6(18),DACTUAL                                                  
         SPACE                                                                  
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*    QUESTOR+2  =   Y   DISPLAY CONTRACT OUTPUT RECORDS          *              
*                                                                *              
******************************************************************              
*                                                                               
         DS    0H                                                               
DISPPUT  NTR1                                                                   
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(15),IDENT                                                    
         EDIT  PUTCTR,(7,P+20)                                                  
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 DS   0H                                                                
         XIT1                                                                   
         SPACE 4                                                                
******************************************************************              
*  DISPUNM:  DISPLAY UNMATCHED TOTALS CTRS                       *              
*                                                                *              
******************************************************************              
*                                                                               
         DS    0H                                                               
DISPUNM  NTR1                                                                   
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+2(16),=C'UNMATCHED TOTALS'                                     
         GOTO1 REPORT                                                           
         LA    R2,ARRAYCT                                                       
         LA    R3,ARRAY                                                         
DISPU100 OC    2(4,R3),2(R3)                                                    
         BZ    DISPU140                                                         
         ZIC   R0,0(R3)            DISPLAY YEAR                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+2(2),DUB                                                       
         ZIC   R0,1(R3)            DISPLAY MONTH                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(2),DUB                                                       
         SPACE                                                                  
         EDIT  (B4,2(R3)),(17,P+7),2,COMMAS=YES,MINUS=YES                       
         GOTO1 REPORT                                                           
         SPACE                                                                  
DISPU140 LA    R3,6(,R3)                                                        
         SPACE                                                                  
         CLC   BENDYM,0(R3)        CK AS AT DATE CUTOFF                         
         BL    DISPU160                                                         
         SPACE                                                                  
         BCT   R2,DISPU100                                                      
         SPACE                                                                  
DISPU160 DS   0H                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
*                                                                               
         DS    0H                                                               
PUTRECS  NTR1                                                                   
*                                                                               
*   THIS ENTRY ADDS LENGTH OF CONTROL BYTE TO RECORD CONTROL                    
*                                                                               
*                                                                               
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         L     RF,PUTCTR2          INCREMENT COUNTER                            
         LA    RF,1(,RF)                                                        
         ST    RF,PUTCTR2          SAVE IT                                      
PUTR0040 DS   0H                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
AHSELIST DS    A                   HOUSE KEY LIST AREA                          
LHSELIST DS    F                   LEN OF HOUSE LIST AREA                       
ANXTHSE  DS    A                   NEXT EMPTY SLOT                              
HSELSTLN EQU   16                                                               
REDCTR   DC    F'0'                                                             
REDCTRT  DC    F'0'                                                             
CONCLR   DC    F'0'                                                             
HSEACT   DC    F'0'                                                             
HSECTR   DC    F'0'                                                             
HSECTRT  DC    F'0'                                                             
HSEUSD   DC    F'0'                                                             
HSECLR   DC    F'0'                                                             
HSEDEL   DC    F'0'                                                             
ACTCTR   DC    F'0'                CT OF ACTUAL RECS                            
ACTCTRT  DC    F'0'                                                             
UNCONCT  DC    F'0'                UNMATCHED CONTRACT RECS                      
UNACTCT  DC    F'0'                UNMATCHED ACTUAL RECS                        
BALRECCT DC    F'0'                ACTUAL & CONTRACT RECS IN BAL                
EQRECCT  DC    F'0'                EQUAL ACTUAL & CONTRACT RECS                 
BAD04CTR DC    F'0'                                                             
LOWCTR   DC    F'99999'            LOW DISPLAY COUNT                            
HIGHCTR  DC    F'99999'            HIGH COUNTER                                 
PUTCTR   DC    F'0'                                                             
PUTCTRT  DC    F'0'                                                             
PUTCTR2  DC    F'0'                                                             
DELCTR   DC    F'0'                                                             
DELCTRA  DC    F'0'                                                             
AIOAREA  DS    F                                                                
DATEWORK DS    CL48                DATE WORK AREA                               
COMMAND  DS    CL8                                                              
RUNSTRT  DS    F                                                                
RUNEND   DS    F                                                                
DORDERED DC    PL7'0'              DDS ORDERED                                  
DACTUAL  DC    PL7'0'              DDS ACTUAL                                   
KACTUAL  DC    PL7'0'              KATZ ACTUAL                                  
HACTUAL  DC    PL7'0'              HOUSE ACTUAL                                 
TDACTUAL DC    PL7'0'              DDS TOTALS                                   
THACTUAL DC    PL7'0'              HOUSE TOTALS                                 
TKACTUAL DC    PL7'0'              KATZ TOTALS                                  
         SPACE                                                                  
* 13 MON TABLE - 1-2 YR/MO OF SERVICE                                           
*                3-6 DOLLAR AMT FROM 03 OR 04 ELEM                              
*                  7 NON-ZERO IF FROM 04 ELEM                                   
         SPACE                                                                  
CONCTS   DS    XL91                                                             
CONCTSL  EQU   7                                                                
CONCTSN  EQU   13                                                               
         SPACE                                                                  
ACTREC   DS   0XL300                                                            
AREP     DS    CL02   01-002                                                    
ACALL1   DS    CL05   03-007        STATION CALL LETTERS (AM)                   
AOFFICE  DS    CL02   08-009                                                    
ACALL2   DS    CL05   10-014        STATION CALL LETTERS (FM)                   
         SPACE                                                                  
*  CTRS ARE 5 YEARS, EACH 12 CTRS, 95, 96, 92, 93, 94                           
         SPACE                                                                  
ACTRS    DS  60PL04   15-254                                                    
AMKTNO   DS    CL03  255-257                                                    
AMKTNM   DS    CL18  258-275                                                    
         DS    CL25  276-300       SPARE                                        
AGYREP   DS    CL2                                                              
ELEM     DS    CL256                                                            
SWIREPS  DC    CL24'  '                                                         
BSTRYM   DS    XL2                 BINARY STARTING YR/MON                       
BENDYM   DS    XL2                 BINARY ENDING YR/MON                         
         SPACE                                                                  
CDTS     DS    0XL6                SAVED CONTRACT START/END DATES               
CSTDT    DS    XL2                 USED TO ELIMINATE SPURIOUS 04 EL             
         DS    XL1                                                              
CEDDT    DS    XL2                                                              
         DS    XL1                                                              
         SPACE                                                                  
SVCON    DS    PL5                                                              
         SPACE                                                                  
SVGRP    DS    CL2                                                              
*                                                                               
FOXZEROS DC    C'0000000'                                                       
         SPACE                                                                  
* USED FOR CONTRACT BREAK BY REP/STA/OFF                                        
         SPACE                                                                  
SVREPSTA DC    CL9' '  2-REP, 5-STA, 2-OFF                                      
SVRPSTAC DS    CL7                                                              
         SPACE                                                                  
* USED TO HOLD CONTRACT INFO WHILE PROCESSING MISSING ACTUAL REC                
         SPACE                                                                  
HDREPSTA DC    CL9' '  2-REP, 5-STA, 2-OFF                                      
ELCODE   DC    XL1'00'                                                          
RDACTSW  DC    CL1'Y'                                                           
IDENT    DC    CL15' '                                                          
         SPACE 3                                                                
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=FB,LRECL=256,              X        
               BLKSIZE=8192,MACRF=GM,EODAD=CKACTEOF                             
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 3                                                                
FIXINV   DS    CL1                                                              
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL2048              AREA FOR RECORD                              
         SPACE                                                                  
* ARRAY OF CONTRACT DOLLARS FOR 1 STATION/OFFICE                                
* AT END OF REP/STA/OFF, COMPARED TO ACTUAL REC, AND DIFFERENCES ARE            
* STORED IN ARRAY TO BE WRITTEN TO ADDED CONTRACT RECORDS                       
         SPACE                                                                  
SACTDOL  DC    PL6'0'                                                           
SCONDOL  DC    PL6'0'                                                           
ACTDOL   DC    PL6'0'                                                           
CONDOL   DC    PL6'0'                                                           
         SPACE                                                                  
         DC    C'**ARRAY*'                                                      
*RRAYCT  EQU   24                                                               
ARRAYCT  EQU   12                  JAN THRU DEC                                 
JAN96    EQU   1                                                                
ARRAY    DS    48XL6               1 BYTE YR, 1 BYTE MONTH, 4 BYTES AMT         
SVARRAY  DS    48XL6               1 BYTE YR, 1 BYTE MONTH, 4 BYTES AMT         
         SPACE                                                                  
         DS    0H                                                               
         DC    C'**SKEYS*'                                                      
         SPACE                                                                  
* SKELETON KEY FOR 93 PRODUCTION                                                
         SPACE                                                                  
SKEY93   DC    X'0C00',C'BFRBKACYAATEGAC  EGAC',X'03096769'                     
         DS    0H                                                               
* SKELETON KEY FOR 94 PRODUCTION                                                
         SPACE                                                                  
         DC    X'0C00',C'BFRBKACYAATEGAC  EGAC',X'03096770'                     
         DS    0H                                                               
* SKELETON KEY FOR 95 PRODUCTION                                                
         SPACE                                                                  
         DC    X'0C00',C'BFRBKACYAATEGAC  EGAC',X'03096771'                     
         DS    0H                                                               
* SKELETON KEY FOR 96 PRODUCTION                                                
         SPACE                                                                  
SKEY96   DC    X'0C00',C'BFRBKACYAATEGAC  EGAC',X'03120768'                     
         DS    0H                                                               
* SKELETON KEY FOR 93 TEST                                                      
         SPACE                                                                  
SKEY93T  DC    X'0C00',C'V3R ACCTANYTEST  TEST',X'03091708'                     
         DS    0H                                                               
         DC    X'0C00',C'V3R ACCTANYTEST  TEST',X'03091709'                     
         DS    0H                                                               
         DC    X'0C00',C'V3R ACCTANYTEST  TEST',X'03091710'                     
         DS    0H                                                               
         DS    0D                                                               
SKEL93   DS    XL992               SKELETON FOR 93                              
         DS    XL992                            94                              
         DS    XL992                            95                              
SKEL96   DS    XL992                            96                              
SKELEN   EQU   992                                                              
         SPACE                                                                  
* ACTUAL ARRAY OF DOLLARS FROM KATZ ACTUALS TAPE                                
         SPACE                                                                  
         DC    C'*KARRAY*'                                                      
KARRAY   DS    48XL8       1 BYTE YR, 1 BYTE MONTH, 6 BYTES PACKED AMT          
         SPACE                                                                  
* ACTUAL ARRAY OF DOLLARS FROM KATZ ACTUALS (MATCHED)                           
         SPACE                                                                  
*ARRAYM  DS    48XL8       1 BYTE YR, 1 BYTE MONTH, 6 BYTES PACKED AMT          
         SPACE                                                                  
* ACTUAL ARRAY OF DOLLARS FROM KATZ CONTRACTS                                   
         SPACE                                                                  
         DC    C'*CARRAY*'                                                      
CARRAY   DS    48XL8       1 BYTE YR, 1 BYTE MONTH, 6 BYTES PACKED AMT          
         SPACE                                                                  
* ACTUAL ARRAY OF DOLLARS FROM ADDED KATZ HOUSE CONTRACTS                       
         SPACE                                                                  
         DC    C'*HARRAY*'                                                      
HARRAY   DS    48XL8       1 BYTE YR, 1 BYTE MONTH, 6 BYTES PACKED AMT          
         SPACE                                                                  
MISTAB   DC    100XL7'00'                                                       
MISTABCT EQU   (*-MISTAB)/MISTABLN                                              
MISTABLN EQU   7                                                                
         SPACE                                                                  
RECIO    DS    CL2048                                                           
         EJECT                                                                  
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL2048                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         ORG                                                                    
         EJECT                                                                  
RECORD3  DS    CL1024                                                           
         ORG   RECORD3                                                          
       ++INCLUDE REGENAGY2         AGENCY      RECORD                           
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACSD                                                     
         EJECT                                                                  
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
QREC3D   DSECT                                                                  
       ++INCLUDE REGENREQ3                                                      
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PREP     DS    CL2                                                              
         DS    CL3                                                              
PSTA     DS    CL5                                                              
         DS    CL6                                                              
POFF     DS    CL2                                                              
         DS    CL5                                                              
PCONDOL  DS    0CL13                                                            
         DS    CL4                                                              
PCON     DS    CL8                                                              
         DS    CL1                                                              
PACTDOL  DS    CL13                                                             
         DS    CL2                                                              
PDIFDOL  DS    CL13                                                             
         DS    CL2                                                              
PREASON  DS    CL1                                                              
*********************************************************************           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029REREPAC02A05/01/02'                                      
         END                                                                    
