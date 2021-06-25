*          DATA SET REREP1J02  AT LEVEL 067 AS OF 04/22/15                      
*PHASE RE1J02C,*                                                                
*INCLUDE SORTER                                                                 
         TITLE 'REREP1J02 - RE1J02 - LABEL PRINTING MODULE'                     
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP1J02 --- LABEL PRINTING MODULE                        *           
*                                                                   *           
* NOTE: USES LABEL RECORD (TYPE X'36') PROCESSED BY SFM OVERLAY     *           
*       RESFM27.  THIS PROGRAM TAKES THE FORMAT AS DEFINED BY THE   *           
*       LABEL RECORD AND PRINTS A SERIES OF LABELS REQUESTED BY     *           
*       THE USER.                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 03MAR93 (SKU) --- BEHOLD, THE LABEL HAS COMETH                    *           
*                                                                   *           
* 09JUL93 (SKU) --- OPTION 3 LASER OPTION TO EJECT PAGE             *           
*                                                                   *           
* 03JAN94 (SKU) --- OPTION TO PRINT REQUEST DETAILS                 *           
*                                                                   *           
* 10FEB94 (SKU) --- OPTION FOR DOWNLOAD                             *           
*                                                                   *           
* 14MAR94 (SKU) --- FIX BUG OF ENDING SORT BETWEEN RUNS             *           
*                                                                   *           
* 10MAY95 (SKU) --- ENABLE DETAILS FOR DOWNLOAD                     *           
*                                                                   *           
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                   *           
* 17APR96 (RHV) --- SUPPORT 34 BYTE AGY ADDRESS FIELDS              *           
*                                                                   *           
* 24APR96 (RHV) --- FIX DOWNLOAD OPTION W/34 BYTE ADDRESS BUG       *           
*                                                                   *           
* 23MAY97 (BU ) --- UPGRADE FOR YR 2000                             *           
*                                                                   *           
* 21JAN98 (ASTE) --- OPT 2 = 'S' - SORT AND PAGE BREAK BY STATION   *           
*                                                                   *           
* 20FEB98 (JRD)  --- 4K CONTRACTS                                   *           
*                                                                   *           
*              *** END TOMBSTONE ***                                *           
*                                                                   *           
*HERE****************************************************************           
RE1J02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STOREX-STORED,**RE1J02,R8,RR=R5                                  
         USING STORED,RC                                                        
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
                                                                                
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         TM    PROGSTAT,NOLABEL    IF NO LABEL FORMAT REC, EXIT                 
         BNZ   ENDSORT             FOR ALL OTHER MODES                          
         CLI   MODE,PROCCONT                                                    
         BE    PROC                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
                                                                                
         B     EXIT                                                             
                                                                                
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XMOD1                                                                  
                                                                                
**********************************************************************          
* END SORTER                                                                    
**********************************************************************          
ENDSORT  DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'END',RR=RELO                                  
         B     RUNF                IN CASE OF BATCH RUNS                        
         EJECT                                                                  
**********************************************************************          
* RUNFRST - INIT FLAGS                                                          
**********************************************************************          
RUNF     DS    0H                                                               
         XC    PROGSTAT,PROGSTAT   CLEAR PROGRAM STATUS FLAGS                   
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQFRST - GET LABEL FORMAT RECORD                                             
**********************************************************************          
REQF     DS    0H                                                               
*                                                                               
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         GOTO1 =V(SORTER),DMCB,SRTFLD,RECTYP,0,RR=RELO                          
*                                                                               
         TM    PROGSTAT,HASLABEL   WE'VE THE LABEL FORMAT REC ALREADY           
         BO    REQFX               SKIP ROUTINE                                 
*                                                                               
         LA    R6,KEY              GET LABEL FORMAT RECORD                      
         USING LABELD,R6                                                        
         XC    KEY,KEY                                                          
         MVI   RLABTYP,RLABTYPQ                                                 
         MVC   RLABREP,QREP                                                     
*                                                                               
*        GET LABEL FORMAT NAME FROM 2ND REQUEST CARD                            
*                                                                               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
*                                                                               
         L     R2,VXRQNUM                                                       
         CLI   0(R2),2             NEED 2 CARDS.                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,VXRQCARD         POINT TO THE CARDS AREA                      
         LA    R4,80(R4)           POINT TO 2ND CARD                            
         USING QREC2,R4                                                         
*                                                                               
         MVC   RLABNAME,Q2OPTS     LABEL FORMAT NAME FROM 2ND CARD              
*                                                                               
         CLI   Q2OPT4,C'Y'         PRINT DETAILS?                               
         BNE   REQF05                                                           
         MVI   RCREQREP,C'Y'       SET TO PRINT DETAILS                         
         GOTO1 REQREP,DMCB         PRINT DETAILS                                
         DROP  R4,R5,R6                                                         
*                                                                               
REQF05   DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEYSAVE,KEY,0                         
         CLC   KEY(L'RLABKEY),KEYSAVE                                           
         BE    REQF10                                                           
*                                  NO LABEL FORMAT RECORD FOUND                 
         OI    PROGSTAT,NOLABEL    SET FLAG SO WE'LL SKIP THE REST OF           
         GOTO1 =V(SORTER),DMCB,=C'END',RR=RELO   THE PROGRAM                    
         MVC   P(30),=C'*** LABEL RECORD NOT FOUND ***'                         
         GOTO1 LOCALREP,DMCB,ERRTEXT                                            
         B     REQFX                                                            
*                                                                               
REQF10   DS    0H                                                               
         OI    PROGSTAT,HASLABEL   SET FLAG SO WE DON'T GOTO DATAMGR            
*                                  AGAIN FOR THE LABEL FORMAT RECORD            
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
*                                                                               
REQFX    DS    0H                                                               
         MVC   SVREP,QREP          SAVE FOR LATER                               
         MVC   SVQOPT1,QOPTION1    SAVE FOR LATER                               
         MVC   SVQOPT2,QOPTION2    SAVE FOR LATER                               
         MVC   SVQOPT3,QOPTION3    SAVE FOR LATER                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCCONT - CHECK IF ORDER HAS BUCKETS WITHIN FILTERING PERIOD                 
*            PASS TO SORTER IF FOUND, SKIP IF NOT                               
**********************************************************************          
PROC     DS    0H                                                               
         L     R4,ANEWMON          A(NEW MONTH TABLE) BUILT BY REPORTER         
*                                                                               
PROC10   DS    0H                                                               
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DATE                
         BNL   PROC20              FOUND - CHECK END DATE IF ANY                
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     PROC10              BO BACK FOR NEXT                             
*                                                                               
PROC20   DS    0H                                                               
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END DATE                  
         BH    PROCX               TABLE > END DATE - EXIT                      
         LR    R6,R4               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROL                           
         OC    TOTORD(4,R6),TOTORD(R6) MONEY FOUND, KEEP IT                     
         BNZ   PROC100                                                          
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     PROC10              BO BACK FOR NEXT                             
*                                                                               
PROC100  DS    0H                  ORDER FALL WITHIN PERIOD, KEEP IT            
         CLI   SVQOPT2,C'A'                                                     
         BE    PROC110                                                          
*                                                                               
         MVC   SRTSTA,RCONKSTA                                                  
         CLI   SVQOPT2,C'S'        SORT BY STATION?                             
         BE    *+10                YES, KEEP STA                                
         XC    SRTSTA,SRTSTA       NO, SORT BY BUYER, CLEAR STA                 
         MVC   SRTBUY,RCONBUYR                                                  
         MVC   SRTAGY,RCONKAGY                                                  
         B     PROC120                                                          
*                                                                               
PROC110  DS    0H                                                               
         XC    SRTSTA,SRTSTA       CLEAR STA                                    
         MVC   SRTAGY2,RCONKAGY    SORT BY AGENCY, BUYER                        
         MVC   SRTBUY2,RCONBUYR                                                 
*                                                                               
PROC120  DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC,RR=RELO                           
*                                                                               
PROCX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* REQLAST - GET SORTED LABELS AND PRINT THEM                                    
**********************************************************************          
REQL     DS    0H                  GET FIRST ENTRY AND COMPARE TO NEXT          
         XC    LASTSTA,LASTSTA                                                  
         CLI   RCDNLOAD,C'Y'       IF DOWNLOAD, DON'T PRT TEST PATTERNS         
         BE    REQL05                                                           
         GOTO1 PATTERN             PRINT TEST PATTERNS                          
*                                                                               
REQL05   DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET',RR=RELO                                  
         L     RE,DMCB+4                                                        
         LTR   RE,RE                                                            
         BZ    REQLX                                                            
*                                                                               
         MVC   SRTREC(SRTRECL),0(RE)                                            
*                                                                               
         XCEF  OUTPUT,4356         CLEAR OUTPUT AREA                            
         MVI   LABXLAB,1           SET LABEL INDEX                              
         MVI   NUMLBLDW,1          INIT FOR OPT3, LASER OPT                     
*                                                                               
REQL10   DS    0H                  GET NEXT ENTRY                               
         GOTO1 =V(SORTER),DMCB,=C'GET',RR=RELO                                  
         L     RE,DMCB+4                                                        
         LTR   RE,RE                                                            
         BZ    REQL20                                                           
*                                                                               
         CLC   SRTREC(SRTRECL),0(RE)                                            
         BE    REQL10              IF DUPLICATE, GET NEXT ENTRY                 
*                                                                               
         CLC   SRTREC(L'SRTSTA),0(RE)   SAME STA?                               
         BE    REQL15              YES                                          
         MVI   NUMLBLDW,10         DIFF STA, PAGE BREAK                         
*                                                                               
REQL15   MVC   SAVEREC,0(RE)       UNIQUE, SO SAVE OFF NEXT ENTRY               
         GOTO1 PUTLABEL            PUT FIRST ENTRY ON THE PAGE                  
         MVC   LASTSTA,SRTREC      SAVE PREVIOUS STA                            
         MVC   SRTREC(SRTRECL),SAVEREC  NEXT ENTRY BECOMES THE FIRST            
         B     REQL10                                                           
*                                                                               
REQL20   DS    0H                                                               
         OI    PROGSTAT,LASTLAB    SIGNAL LAST LABEL                            
         GOTO1 PUTLABEL            PROCESS LAST ENTRY                           
*                                                                               
REQLX    DS    0H                                                               
         B     ENDSORT                                                          
         EJECT                                                                  
**********************************************************************          
* PUTLABEL - FORMAT LABELS ON THE PAGE                                          
* SRTREC HAS BUYER AND AGENCY CODE                                              
**********************************************************************          
PUTLABEL NTR1                                                                   
         CLI   RCDNLOAD,C'Y'       IF DOWNLOAD, GO PROCESS                      
         BE    DOWNLOAD                                                         
                                                                                
         CLI   SVQOPT2,C'A'        IF SORT OPTION BY AGENCY, BUYER              
         BNE   PUTL05              REARRANGE SORT RECORD BEFORE WE              
         MVC   WORK(SRTRECL),SRTREC PRINT                                       
         MVC   SRTBUY,WORK+11      * CHANGED BY ASTE FOR STA                    
         MVC   SRTAGY,WORK+5                                                    
*                                                                               
PUTL05   DS    0H                                                               
         GOTO1 GETAGY              GET AGENCY RECORD                            
         BNZ   PUTL500                                                          
*                                                                               
         LA    R6,IOAREA                                                        
         USING RLABREC,R6                                                       
*                                                                               
         CLC   SRTREC(L'SRTSTA),LASTSTA SAME STA?                               
         BE    PUTL06              YES,DON'T PRINT                              
         MVC   P(L'SRTSTA),SRTREC  DIFFERENT STATION, PRINT AT TOP              
         GOTO1 REPORT                                                           
*                                                                               
PUTL06   LA    R3,OUTPUT                                                        
         CLI   RLABDLNE,0          ANY LINES TO SKIP?                           
         BE    PUTL13                                                           
         ZIC   RF,RLABDLNE         YES, SKIP THIS MANY LINES FIRST              
PUTL10   LA    R3,L'OUTPUT(R3)                                                  
         BCT   RF,PUTL10                                                        
*                                                                               
PUTL13   DS    0H                                                               
         LR    R4,R3               R4 ALWAYS PT AT BEGINNING OUTPUT LNE         
         MVC   MAXROW,RLABDROW     SAVE MAXROW                                  
         MVC   LABDCOL,RLABDCOL    SAVE LABEL DEFINED WIDTH                     
         DROP  R6                                                               
*                                                                               
PUTL15   DS    0H                                                               
         MVI   LABXROW,1           START AT FIRST ROW                           
         LA    R6,IOAREA                                                        
         USING RLABFDEF,R6                                                      
*                                                                               
         MVI   ELCODE,RLABFCDQ     GET LABEL DEFINITION RECORD                  
         BAS   RE,GETEL                                                         
         BNE   PUTL500                                                          
*                                                                               
PUTL20   CLC   LABXROW,RLABFROW    LABEL DEFINITION FOR THIS LINE?              
         BNL   PUTL30                                                           
*                                                                               
         ZIC   RF,LABXROW          ADVANCE TO NEXT ROW                          
         LA    RF,1(RF)                                                         
         STC   RF,LABXROW                                                       
         LA    R4,L'OUTPUT(R4)                                                  
         LR    R3,R4                                                            
         CLC   LABXROW,MAXROW      CHECK MAX ROWS PER LABEL                     
         BNH   PUTL20                                                           
         DC    H'0'                                                             
*                                                                               
PUTL30   DS    0H                                                               
         LR    R3,R4               POINT AT BEGINNING OF OUTPUT LINE            
         CLI   LABXLAB,1           IF NOT 1ST LABEL FROM THE LEFT OF PG         
         BNH   PUTL50                                                           
         ZIC   RE,LABDCOL          CALCULATE OFFSET INTO THE OUTPUT PG          
         ZIC   RF,LABXLAB          STARTING COLUMN=                             
*                                      LABEL INDEX X COLUMN PER LABEL           
         BCTR  RF,0                                                             
*                                                                               
PUTL40   AR    R3,RE                                                            
         LA    R3,1(R3)            SKIP ONE COLUMN BETWEEN LABELS               
         BCT   RF,PUTL40                                                        
*                                                                               
PUTL50   ZIC   RE,RLABFCOL         BUMP TO STARTING COLUMN WITHIN LABEL         
         AR    R3,RE                                                            
*                                                                               
         CLI   RLABFLEN,0          0 IF THIS IS A LITERAL                       
         BNE   PUTL100                                                          
*                                                                               
         ZIC   RE,RLABFELN         MOVE IN VARIABLE LENGTH OF LITERAL           
         LA    RF,RLABFOV                                                       
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),RLABFNAM                                                 
*                                                                               
         B     PUTL300                                                          
*                                                                               
PUTL100  DS    0H                  CODE LETTER                                  
         LA    R5,CODETAB                                                       
*                                                                               
PUTL110  DS    0H                                                               
         CLI   0(R5),X'FF'                                                      
         BE    PUTL300             DIDN'T FIND A DEFINITION, SKIP IT            
         CLC   RLABFNAM,0(R5)                                                   
         BE    PUTL120                                                          
         LA    R5,L'CODETAB(R5)                                                 
         B     PUTL110                                                          
*                                                                               
PUTL120  DS    0H                                                               
         CLI   0(R5),C'A'          CASE A SPECIAL, BUYER NAME                   
         BNE   PUTL125                                                          
         LA    RE,SRTBUY                                                        
         B     PUTL140                                                          
*                                                                               
PUTL125  DS    0H                                                               
         CLI   0(R5),C'C'          AGY ADDRESS FIELD?                           
         BNE   PUTL130             NO                                           
         BAS   RE,AGYADDR          YES - DO AGENCY ADDRESS ROUTINE              
         L     R4,PASSR4                                                        
         B     PUTL300                                                          
*                                                                               
PUTL130  DS    0H                                                               
         CLI   0(R5),C'F'          IN CODETAB OFF FIELDS START AT C'G'          
         BH    PUTL135                                                          
*                                                                               
         LA    RE,RAGYREC          GET FIELD'S OFFSET INTO AGY REC              
         B     PUTL138                                                          
*                                                                               
PUTL135  DS    0H                                                               
         CLI   SVQOPT1,C'Y'        SUPPRESS OFFICE RETURN ADDRESS?              
         BE    PUTL300             IF YES, SKIP TO NEXTEL                       
         CLI   0(R5),C'G'                                                       
         BNE   PUTL136                                                          
         LA    RE,RREPREC          REP NAME                                     
         B     PUTL138                                                          
*                                                                               
PUTL136  DS    0H                                                               
         LA    RE,ROFFREC          GET FIELD'S OFFSET INTO AGY REC              
*                                                                               
PUTL138  DS    0H                                                               
         ZIC   RF,1(R5)                                                         
         AR    RE,RF                                                            
*                                                                               
PUTL140  ZIC   R1,RLABFLEN         SET LENGTH OF FIELD TO MOVE                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RE)       MOVE FIELD INTO OUTPUT LINE                  
*                                                                               
PUTL300  DS    0H                  GET NEXT LABEL DEFINITION ELEMENT            
         BAS   RE,NEXTEL                                                        
         BE    PUTL20                                                           
         DROP  R6                                                               
*                                                                               
PUTL500  DS    0H                  NO MORE LABEL DEFINITION ELEMENTS            
         LA    R6,IOAREA                                                        
         USING RLABREC,R6                                                       
*                                                                               
         TM    PROGSTAT,LASTLAB    FLAGGED TO FLUSH? (LAST LABEL?)              
         BO    PUTL510                                                          
*                                                                               
         CLC   SRTREC(L'SRTSTA),SAVEREC SAME STA?                               
         BE    *+10                YES                                          
         MVC   LABXLAB,RLABDNUM    DIFF STA, SET TO MAX # LABELS                
*                                                                               
         CLC   LABXLAB,RLABDNUM    CHECK IF MAX # OF LABELS ACROSS              
         BNL   PUTL510             REACHED                                      
         ZIC   RF,LABXLAB          NO, INCREMENT INDEX                          
         LA    RF,1(RF)                                                         
         STC   RF,LABXLAB                                                       
         B     PUTLX                                                            
*                                                                               
PUTL510  DS    0H                  REACHED MAX NUMBER OF LABELS ACROSS          
         ZIC   R4,RLABDROW         AGE. FLUSH OUTPUT BUFFER                     
         LA    R5,OUTPUT           LOOP THIS MANY ROWS                          
*                                                                               
PUTL520  DS    0H                                                               
         MVC   P(L'OUTPUT),0(R5)                                                
         GOTO1 REPORT                                                           
         MVI   LINE,1              FOOL REPORT, DON'T WANT PAGE BREAK           
         LA    R5,L'OUTPUT(R5)                                                  
         BCT   R4,PUTL520                                                       
         GOTO1 REPORT              SKIP A LINE BETWEEN LABELS                   
         MVI   LINE,1              FOOL REPORT, DON'T WANT PAGE BREAK           
*                                                                               
         XCEF  OUTPUT,4356         CLEAR OUTPUT AREA                            
         MVI   LABXLAB,1           RESET LABEL INDEX                            
*                                                                               
         ZIC   RF,NUMLBLDW         COUNT # LABEL PRINTED DOWN                   
         LA    RF,1(RF)                                                         
         STC   RF,NUMLBLDW                                                      
*                                                                               
         CLI   SVQOPT2,C'S'        SORT & PAGE BREAK BY STATION?                
         BE    PUTL530             YES, CHECK FOR PAGE BREAK                    
*                                                                               
         CLI   SVQOPT3,C'Y'        CHECK IF PRINTING ON LASER PAPER             
         BNE   PUTLX                                                            
*                                                                               
PUTL530  DS    0H                  IF LASER, EJECT EVERY 9 LABELS               
         CLI   NUMLBLDW,10         DOWN PER PAGE                                
         BNH   PUTLX                                                            
         MVI   NUMLBLDW,1          RESET                                        
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PUTLX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* IF RCDNLOAD=Y, DOWNLOAD LABEL DATA WITH FIELD IDENTIFIERS                     
* COLUMN 1="IDENTIFIER", COLUMN 2="DATA"                                        
**********************************************************************          
DOWNLOAD DS    0H                                                               
         CLI   SVQOPT2,C'A'        IF SORT OPTION BY AGENCY, BUYER              
         BNE   DNLD0010            REARRANGE SORT RECORD BEFORE WE              
         MVC   WORK(SRTRECL),SRTREC PRINT                                       
         MVC   SRTBUY,WORK+11      * CHANGED BY ASTE FOR STA                    
         MVC   SRTAGY,WORK+5                                                    
                                                                                
DNLD0010 DS    0H                                                               
         GOTO1 GETAGY              GET AGENCY RECORD                            
         BNZ   DNLDX               CAN'T FIND AGY INFO, SKIP                    
                                                                                
         LA    R6,IOAREA                                                        
         USING RLABFDEF,R6                                                      
         MVI   ELCODE,RLABFCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DNLDX               CAN'T FIND LABEL DATA, SKIP                  
                                                                                
DNLD0020 DS    0H                                                               
         CLI   RLABFLEN,0          SKIP, IF COMMENT                             
         BE    DNLD0300                                                         
                                                                                
DNLD0100 DS    0H                                                               
         LA    R5,CODETAB                                                       
                                                                                
DNLD0110 DS    0H                                                               
         CLI   0(R5),X'FF'                                                      
         BE    DNLD0300            DIDN'T FIND A DEFINITION, SKIP IT            
         CLC   RLABFNAM,0(R5)                                                   
         BE    DNLD0120                                                         
         LA    R5,L'CODETAB(R5)                                                 
         B     DNLD0110                                                         
                                                                                
DNLD0120 DS    0H                                                               
         CLI   0(R5),C'A'          CASE BUYER NAME                              
         BNE   DNLD0130                                                         
         MVC   P(5),=C'BUYER'                                                   
         LA    RE,SRTBUY                                                        
         B     DNLD0210                                                         
                                                                                
DNLD0130 DS    0H                                                               
         CLI   0(R5),C'B'          CASE AGENCY NAME                             
         BNE   DNLD0140                                                         
         MVC   P(11),=C'AGENCY NAME'                                            
         LA    RE,RAGYREC          GET FIELD'S OFFSET INTO AGY REC              
         B     DNLD0200                                                         
                                                                                
DNLD0140 DS    0H                                                               
         CLI   0(R5),C'C'          CASE AGENCY ADD1                             
         BNE   DNLD0150                                                         
         MVC   P(11),=C'AGENCY ADD1'                                            
         LA    RE,RAGYREC          GET FIELD'S OFFSET INTO AGY REC              
         B     DNLD0200                                                         
                                                                                
DNLD0150 DS    0H                                                               
         CLI   0(R5),C'D'          CASE AGENCY ADD2                             
         BNE   DNLD0160                                                         
         MVC   P(11),=C'AGENCY ADD2'                                            
         LA    RE,RAGYREC          GET FIELD'S OFFSET INTO AGY REC              
         B     DNLD0200                                                         
                                                                                
DNLD0160 DS    0H                                                               
         CLI   0(R5),C'E'          CASE AGENCY STATE                            
         BNE   DNLD0170                                                         
         MVC   P(12),=C'AGENCY STATE'                                           
         LA    RE,RAGYREC          GET FIELD'S OFFSET INTO AGY REC              
         B     DNLD0200                                                         
                                                                                
DNLD0170 DS    0H                                                               
         CLI   0(R5),C'F'          CASE AGENCY ZIP                              
         BNE   DNLD0300            NONE FOUND, SO SKIP                          
         MVC   P(10),=C'AGENCY ZIP'                                             
         LA    RE,RAGYREC          GET FIELD'S OFFSET INTO AGY REC              
                                                                                
DNLD0200 DS    0H                                                               
         CLI   0(R5),C'C'          ADDRESS LINE 1?                              
         BNE   DNLD0205                                                         
         TM    RAGYFLAG,X'80'      EXPANDED ADDRESS?                            
         BZ    DNLD0205            NO                                           
         LR    R0,R6               SAVE R6                                      
         MVC   SAVEEL,ELCODE                                                    
         BAS   RE,GETAGY2          GET AGY2 REC                                 
         BZ    *+6                                                              
         DC    H'0'                MUST HAVE REC                                
         MVI   ELCODE,X'20'                                                     
         LA    R6,RAGY2REC                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE ELEMENT                            
         MVC   P+20(L'RAGY2AD1),2(R6)                                           
         MVC   ELCODE,SAVEEL       RESTORE ELCODE                               
         LR    R6,R0               RESTORE R6                                   
         BAS   RE,GETAGY           RESTORE AGY REC                              
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     DNLD0250                                                         
DNLD0205 ZIC   RF,1(R5)                                                         
         AR    RE,RF                                                            
         CLI   0(R5),C'C'                                                       
         BNE   DNLD0210                                                         
         CLI   RLABFLEN,L'RAGYADD1                                              
         BNH   DNLD0210                                                         
         ZIC   RF,=AL1(L'RAGYADD1)                                              
         B     DNLD0215                                                         
DNLD0210 DS    0H                                                               
         ZIC   RF,RLABFLEN         SET LENGTH OF FIELD TO MOVE                  
DNLD0215 LA    R1,DMCB                                                          
         STC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P+20(0),0(RE)       MOVE FIELD INTO OUTPUT LINE                  
                                                                                
DNLD0250 GOTO1 LOCALREP,DMCB,ALLTEXT                                            
                                                                                
DNLD0300 DS    0H                  GET NEXT LABEL DEFINITION ELEMENT            
         BAS   RE,NEXTEL                                                        
         BE    DNLD0020                                                         
                                                                                
DNLDX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* AGENCY ADDRESS ROUTINE - SUPPORTS 2-34 BYTE ADDRESS LINES                     
**********************************************************************          
AGYADDR  DS    0H                                                               
         NTR1                                                                   
         MVC   SAVEEL,ELCODE                                                    
         USING RLABFDEF,R6                                                      
*                                                                               
         ZIC   R2,RLABFLEN             HANG ON TO FIELD LENGTH                  
*                                                                               
         TM    RAGYFLAG,X'80'          EXPANDED ADDRESS?                        
         BO    AA100                   YES                                      
*                                                                               
         CH    R2,=H'20'               IS FIELD LEN > 20                        
         BNH   *+8                     NO - OK                                  
         LA    R2,20                   YES - IMPOSE MAX=20                      
*                                                                               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),RAGYADD1        EX MOVE OF 20 BYTE AGY ADDRESS           
         B     AGYADDRX                DONE HERE                                
         DROP  R6                                                               
*                                                                               
AA100    DS    0H                      DEAL WITH EXTENDED ADDRESS HERE          
         BAS   RE,GETAGY2              GET RAGY2REC                             
         BZ    *+6                                                              
         DC    H'0'                    MUST HAVE RAGY2REC                       
         MVI   ELCODE,X'20'                                                     
         LA    R6,RAGY2REC                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RAGY2AE1,R6                                                      
         CH    R1,=H'34'                                                        
         BNH   *+8                                                              
         LA    R2,34                                                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),RAGY2AD1                                                 
         OC    RAGY2AD2,SPACES                                                  
         CLC   RAGY2AD2,SPACES                                                  
         BE    AGYADDRX                                                         
         LA    R3,L'OUTPUT(R3)                                                  
         LA    R4,L'OUTPUT(R4)                                                  
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),RAGY2AD2                                                 
         DROP  R6                                                               
*                                                                               
AGYADDRX DS    0H                                                               
         ZIC   RF,LABXROW          ADVANCE TO NEXT ROW                          
         LA    RF,1(RF)                                                         
         STC   RF,LABXROW                                                       
         MVC   ELCODE,SAVEEL                                                    
         ST    R4,PASSR4                                                        
         BAS   RE,GETAGY                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LOCAL REP - DOWNLOAD HANDLING                                                 
* P1=A(DEFINITION LIST)                                                         
**********************************************************************          
LOCALREP NTR1                                                                   
         CLI   RCDNLOAD,C'Y'                                                    
         BE    LOCAL005                                                         
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
         B     LOCALX                                                           
                                                                                
LOCAL005 DS    0H                                                               
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
                                                                                
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         L     R1,0(R1)            R1 -> PASSED DEFINITION LIST                 
         DROP  R5                                                               
                                                                                
LOCAL010 DS    0H                                                               
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LOCAL020                                                         
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LOCAL010                                                         
                                                                                
LOCAL020 DS    0H                                                               
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
                                                                                
LOCALX   DS    0H                                                               
         B     EXIT                                                             
                                                                                
ALLTEXT  DC    C'T',AL1(20),C'T',AL1(34),X'0000'                                
ERRTEXT  DC    C'T',AL1(50),X'0000'                                             
BLNKLINE DC    X'0000'                                                          
         EJECT                                                                  
**********************************************************************          
* GETAGY - GET AGENCY RECORD                                                    
* SRTAGY HAS AGENCY CODE + AGENCY OFFICE                                        
* EXITS WITH CONDITION FLAG SET                                                 
**********************************************************************          
GETAGY   NTR1                                                                   
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY(6),SRTAGY                                               
         MVC   RAGYKREP,SVREP                                                   
         MVC   KEY,RAGYKEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         CLC   KEY(L'RAGYKEY),RAGYKEY                                           
         BNE   NO                  SET CONDITION CODE AND EXIT                  
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,RAGYREC,DMWORK                
*                                                                               
         B     YES                                                              
         EJECT                                                                  
**********************************************************************          
* GETAGY2 - GET AGENCY2 RECORD                                                  
* SRTAGY HAS AGENCY CODE + AGENCY OFFICE                                        
* EXITS WITH CONDITION FLAG SET                                                 
**********************************************************************          
GETAGY2  NTR1                                                                   
         XC    RAGY2KEY,RAGY2KEY                                                
         MVI   RAGK2TYP,X'1A'                                                   
         MVC   RAGK2AGY(6),SRTAGY                                               
         MVC   RAGK2REP,SVREP                                                   
         MVC   KEY,RAGY2KEY                                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         CLC   KEY(L'RAGY2KEY),RAGY2KEY                                         
         BNE   NO                  SET CONDITION CODE AND EXIT                  
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,RAGY2REC,DMWORK               
*                                                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT TEST PATTERNS                                                           
***********************************************************************         
PATTERN  NTR1                                                                   
         LA    R6,IOAREA                                                        
         USING RLABREC,R6                                                       
*                                                                               
         CLI   RLABDPAT,0          DON'T PRINT IF NO PATTERN DESIRED            
         BE    PATTX                                                            
         ZIC   R5,RLABDPAT         PRINT THIS MANY TEST PATTERN DOWN            
*                                                                               
PATT10   DS    0H                                                               
         ZIC   R4,RLABDROW         NUMBER OF ROWS PER LABEL                     
*                                                                               
PATT20   DS    0H                                                               
         ZIC   R3,RLABDNUM         NUMBER OF LABELS ACROSS TO PRINT             
         LA    R2,P                                                             
*                                                                               
PATT30   DS    0H                                                               
         MVI   0(R2),C'*'              PRINT THE TEST PATTERN                   
         ZIC   R1,RLABDCOL                                                      
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),0(R2)                                                    
*                                                                               
         CH    R3,=H'1'                                                         
         BNH   PATT40                                                           
*                                                                               
         ZIC   R1,RLABDCOL                                                      
         AR    R2,R1                                                            
         LA    R2,1(R2)            SKIP A COLUMN BETWEEN LABELS                 
         BCT   R3,PATT30                                                        
*                                                                               
PATT40   DS    0H                                                               
         GOTO1 REPORT                                                           
         MVI   LINE,1              FOOL REPORT, DON'T WANT PAGE BREAK           
         BCT   R4,PATT20                                                        
         GOTO1 REPORT              SKIP A LINE BETWEEN LABELS                   
         MVI   LINE,1              FOOL REPORT, DON'T WANT PAGE BREAK           
*                                                                               
         BCT   R5,PATT10                                                        
PATTX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
SRTFLD   DC    CL80'SORT FIELDS=(1,31,A),FORMAT=BI,WORK=1'                      
RECTYP   DC    CL80'RECORD TYPE=F,LENGTH=31     '                               
*                                                                               
       ++INCLUDE REGENCDE                                                       
*                                                                               
* STORAGE AREAS FOR BETWEEN I/O DATA                                            
*                                                                               
         SPACE 2                                                                
RELO     DS    A                                                                
TEMP     DS    6F                                                               
PROGSTAT DS    X                                                                
NOLABEL  EQU   X'80'               LABEL FORMAT REC NOT FOUND, EXIT             
HASLABEL EQU   X'40'               LABEL FORMAT REC HAS BEEN FOUND              
LASTLAB  EQU   X'20'               LAST LABEL, FLUSH OUTPUT BUFFER              
SAVEREC  DS    CL(SRTRECL)         TEMPORARY SAVE OF SORT REC                   
MAXROW   DS    X                   MAX ROW PER LABEL                            
LABDCOL  DS    X                   LABEL DEFINED WIDTH                          
NUMLBLDW DS    X                   NUMBER OF LABELS PRINTED DOWN                
SVREP    DS    CL2                 REP                                          
SVREQOFF DS    CL2                 REQUESTING OFFICE                            
SAVEEL   DS    C                   ELCODE SAVE AREA                             
PASSR4   DS    F                   RETURN R4 FROM SUBROUTINE                    
SVQOPT1  DS    C                   OPTION 1, Y=SUPPRESS RETURN ADDRESS          
SVQOPT2  DS    C                   OPTION 2, A=SORT BY AGENCY/BUYER             
*                                            B=SORT BY BUYER/AGENCY             
SVQOPT3  DS    C                   OPTION 3, Y=EJECT PAGE FOR LASER             
*                                                                               
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
LASTSTA  DS    CL5                 PREVIOUS SAVE STA CALL LETTERS               
*                                                                               
         DS    0D                                                               
IOAREA   DS    CL1000                                                           
         EJECT                                                                  
STORED   DSECT                                                                  
ELCODE   DS    C                                                                
LABXLAB  DS    X                   CURRENT INDEX OF LABEL IN PROCESS            
LABXROW  DS    X                   CURRENT ROW INDEX                            
BUCKETYR DS    CL2                 BUCKET YEAR                                  
BUCKETMN DS    CL2                 BUCKET MONTH                                 
*                                                                               
*                                                                               
* SORTER RECORD - BUYER, AGENCY                                                 
*                                                                               
SRTREC   DS    0F                                                               
SRTSTA   DS    CL5                 STATION CALL LETTERS                         
SRTBUY   DS    CL20                AGENCY BUYER                                 
SRTAGY   DS    CL6                 AGENCY CODE + AGENCY OFFICE                  
SRTRECL  EQU   *-SRTREC                                                         
*                                                                               
*                                                                               
* SORTER RECORD - AGENCY, BUYER                                                 
*                                                                               
         ORG   SRTBUY                                                           
SRTAGY2  DS    CL6                 AGENCY CODE + AGENCY OFFICE                  
SRTBUY2  DS    CL20                AGENCY BUYER                                 
*                                                                               
         DS    0D                                                               
OUTPUT   DS    33CL132             33 LINES OF LENGTH 132 CHARS EACH            
STOREX   EQU   *                                                                
*              FILE CONTROL AND WORKD DSECTS                                    
         PRINT OFF                                                              
       ++INCLUDE REREPRGEQA                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE REGENREQ2                                                      
       ++INCLUDE REGENALL1A                                                     
         ORG  RCONREC                                                           
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
LABELD   DSECT                                                                  
       ++INCLUDE REGENLAB                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067REREP1J02 04/22/15'                                      
         END                                                                    
