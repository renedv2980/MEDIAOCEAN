*          DATA SET REREPSCCR  AT LEVEL 120 AS OF 05/01/02                      
*PHASE REST02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXIN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE SCANNER                                                                
         TITLE 'REREPSCCR - CLEAR CHANNEL RADIO CONVERT'                        
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPSCCR -- RESEQUENCE ENTIRE KATZ RADIO FILE.           *            
*                     RENUMBER ALL ORDERS IN SELECTED COMPANIES.   *            
*                     REASSIGN NEW REPS, GROUP/SUBGROUP BY LISTED  *            
*                     STATIONS.                                    *            
*                     ETC.                                         *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* OCT05/00 (BU ) --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR     Y = DISPLAY STATION TABLE                        *            
*     QUESTOR+1   Y = COVER SHEET PROMPT                           *            
*     QUESTOR+2   Y = CONFIRM COMMENT PROMPT                       *            
*     QUESTOR+3   Y = OUTPUT ONLY FOUND STATION ORDERS/BUYS        *            
*                     (THIS WILL LIMIT VOLUME OF TESTING)          *            
*     QUESTOR+4   Y = SHORT STATION DISPLAY                        *            
*     QUESTOR+5   Y = DISPLAY MATCHED BUYS                         *            
*     QUESTOR+11  Y = MANUALLY CLOSE THE SORT FILE (DON'T DUMP)    *            
*                                                                  *            
*     QRECORD+36  =  'FROM' REP                                    *            
*     QRECORD+38  =  'TO  ' REP                                    *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REST02   CSECT                                                                  
         NMOD1 0,**ST02**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
         XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         OPEN  (INTAPE1,(INPUT))   TAPE1:  STATION LIST                         
         OPEN  (INTAPE2,(INPUT))   TAPE2:  INDETERMINATE NOW                    
*                                                                               
         GOTO1 =A(INITIAL),DMCB,(RC)                                            
*                                                                               
         GOTO1 =A(LOADTABL),DMCB,(RC)                                           
*                                                                               
         CLOSE (INTAPE1,REWIND)                                                 
         CLOSE (INTAPE2,REWIND)                                                 
*                                                                               
         MVC   SORTREC,FOXES                                                    
         GOTO1 =A(SORTGEN)         PUT OUT END OF FILE RECORD                   
*                                                                               
         GOTO1 FILEPROC,DMCB,(RC)  PROCESS BULK OF FILE                         
*                                                                               
*   BOTH BUYS AND COVERSHEETS REQUIRE SORT RECORDS TO DETERMINE WHICH           
*        ORDERS ARE BEING MOVED TO ANOTHER REP.  WHILE THE ADJUSTMENT           
*        OF CONTRACT NUMBERS WITHIN THE COMPANY CAN BE DONE, THERE              
*        IS NO STATION INDICATION WITHIN EITHER OF THESE RECORD                 
*        TYPES TO PERMIT IDENTIFICATION OF A REP SWITCH.  THIS                  
*        INFORMATION IS CONTAINED WITHIN SORT RECORDS GENERATED BY              
*        PROCESSING THE CONTRACT RECORD ITSELF.                                 
*           BUY SORT RECORDS ARE FIRST, FOLLOWED BY COVERSHEETS.                
*                                                                               
         GOTO1 BUYSPROC,DMCB,(RC)  PROCESS BUYLINE RECORDS                      
*                                                                               
         GOTO1 =A(COVRPROC),DMCB,(RC)  PROCESS COVERSHEETS                      
*                                                                               
         GOTO1 =A(DISPTOTS),DMCB,(RC)  DISPLAY TOTALS FOR RUN                   
*                                                                               
*                                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(31),=C'***MAIN PROCESSING COMPLETED***'                      
         GOTO1 REPORT                                                           
*                                                                               
         CLI   QUESTOR+11,C'Y'     MANUALLY CLOSE SORT FILE?                    
         BNE   MAIN0100            NO                                           
         GOTO1 SORTER,DMCB,=C'END',SORTREC                                      
         MVC   P+1(29),=C'**SORT FILE CLOSED MANUALLY**'                        
         GOTO1 REPORT                                                           
MAIN0100 EQU   *                                                                
         XIT1                      EXIT                                         
         EJECT                                                                  
******************************************************************              
*  FILEPROC:  PROCESS FILE WITH EXCEPTION OF BUY RECORDS.        *              
*        BUYS DEPEND ON ACTIONS TAKEN WITH CONTRACTS.  BECAUSE   *              
*        BUYS DO NOT CONTAIN STATION INFORMATION, A SORT RECORD  *              
*        CREATED FROM A MOVING CONTRACT RECORD WILL BE USED TO   *              
*        SIGNAL CHANGE OF REP                                    *              
******************************************************************              
FILEPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,1                                                            
FPRO0010 EQU   *                                                                
         GOTO1 HIGHDIR                                                          
         B     FPRO0040                                                         
FPRO0020 EQU   *                                                                
*                                                                               
         GOTO1 SEQDIR                                                           
FPRO0040 EQU   *                                                                
*                                                                               
*   TEST SKIP                                                                   
*        CLI   KEY,X'0C'           CONTRACT RECORD?                             
*        BNE   TEST0020            NO                                           
*        CLC   KEY+2(9),=C'EAREWGNAF'                                           
*        BE    TEST0020            TEST: PROCESS THIS STATION                   
*        XC    KEY,KEY             NO  - SKIP ALL OTHER CONTRACTS               
*        MVI   KEY,X'0D'           SET KEY PAST CONTRACTS                       
*        B     FPRO0010            START THERE                                  
TEST0020 EQU   *                                                                
*   TEST SKIP END                                                               
*                                                                               
*   FINAL RECORD TYPE ON THIS (KATZ RADIO GROUP) FILE IS TYPE X'49'             
*        COVERSHEET RECORD, WHICH MUST BE SPECIALLY PROCESSED USING             
*        SORT RECORDS.  IDENTIFICATION OF THIS RECORD TYPE COMING UP            
*        WILL MEAN THAT ALL SERIALLY PROCESSED RECORDS HAVE BEEN DONE,          
*        AND THIS ROUTINE CAN CONCLUDE.                                         
*                                                                               
         CLI   KEY,X'49'           END OF FILE? (COVERSHEET FOUND)              
         BE    FPRO0900            YES - END THIS ROUTINE                       
         GOTO1 GETRECRD                                                         
         CLI   KEY,X'02'           STATION RECORD?                              
         BL    FPRO0060            NO  - PRE  STATION                           
         BH    FPRO0050            NO  - POST STATION                           
*                                  YES - STATION                                
         BAS   RE,STATPROC         YES - SPECIAL PROCESSING FOR STNS            
         B     FPRO0020            GO BACK FOR NEXT RECORD                      
FPRO0050 EQU   *                                                                
         CLI   STATFLAG,0          FIRST TIME?                                  
         BNE   FPRO0060            NO                                           
         MVC   P+1(23),=C'BREAK   AFTER STATIONS:'                              
         MVC   P+25(27),REC                                                     
         GOTO1 REPORT                                                           
         MVI   STATFLAG,1          YES - SET TO OFF                             
         BAS   RE,STASWEEP         SWEEP FOR MISSING STATIONS                   
FPRO0060 EQU   *                                                                
         CLI   KEY,X'0B'           BUY RECORD?                                  
         BNE   FPRO0080            NO                                           
*                                                                               
         MVC   P+1(21),=C'SKIPPING TO CONTRACTS'                                
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY             YES - SKIP BUY RECORDS ON PASS               
         MVI   KEY,X'0C'           SET KEY FOR CONTRACTS                        
*                                                                               
*   SPECIAL CONTRACT START:                                                     
*                                                                               
**       MVC   KEY+2(2),=C'EA'     FORCE EASTMAN/RE/WGNA-F                      
**       MVC   KEY+4(2),=C'RE'                                                  
**       MVC   KEY+6(5),=C'WGNAF'                                               
*                                                                               
*   SPECIAL CONTRACT START: MUST BE REMOVED FOR FINAL RUN                       
*                                                                               
         B     FPRO0010            START AT CONTRACTS                           
FPRO0080 EQU   *                                                                
         CLI   KEY,X'0C'           CONTRACT RECORD?                             
         BNE   FPRO0100            NO                                           
         BAS   RE,CONTPROC         YES - PROCESS CONTRACT RECORD                
         B     FPRO0020            GO BACK FOR NEXT RECORD                      
FPRO0100 EQU   *                                                                
         CLI   KEY,X'28'           SWITCH RECORD?                               
         BNE   FPRO0120            NO                                           
*                                                                               
         MVC   P+1(21),=C'DROPPING ALL SWI RECS'                                
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY             YES - SKIP SWITCH RECORDS ON PASS            
         MVI   KEY,X'29'           SET KEY TO SKIP SWITCH RECORDS               
         B     FPRO0010            START AT COMMISSION RECORDS                  
FPRO0120 EQU   *                                                                
         CLI   KEY,X'2C'           AUR RECORD?                                  
         BNE   FPRO0130            NO                                           
*                                                                               
         MVC   P+1(21),=C'DROPPING ALL AUR RECS'                                
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY             YES - SKIP AUR RECORDS ON PASS               
         MVI   KEY,X'2D'           SET KEY TO SKIP AUR RECORDS                  
         B     FPRO0010            START AT CONTRACTS                           
FPRO0130 EQU   *                                                                
         CLI   KEY,X'2A'           OWNER RECORD?                                
         BNE   FPRO0140            NO                                           
*                                                                               
         L     RF,OWNCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,OWNCTR                                                        
**       GOTO1 REPORT                                                           
**       MVC   P+1(06),=C'OWNER:'                                               
**       MVC   P+08(34),KEY                                                     
**       MVI   P+43,C'/'                                                        
**       MVC   P+44(34),REC                                                     
*                                                                               
**       GOTO1 REPORT                                                           
FPRO0140 EQU   *                                                                
         MVC   REC-4(2),REC+RCONLEN-RCONREC                                     
*                                  INSERT LENGTH FOR PUTREC ROUTINE             
         BAS   RE,PUTRECS          WRITE REC TO O/P UNCHANGED                   
         B     FPRO0020            GO BACK FOR NEXT INPUT                       
FPRO0900 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+4(23),=C'**MAIN FILE PROCESSED**'                              
         GOTO1 REPORT                                                           
         XIT1                                                                   
STATFLAG DS    X                                                                
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*   TABLE FOR RECORD TYPES:                                                     
*     BYTE 0:      RECORD TYPE                                                  
*     BYTE 1:      DISPLACEMENT TO REP CODE                                     
*     BYTE 2-3:    SPARE                                                        
         SPACE 1                                                                
RECLIST  DS    0F                                                               
         DC    X'01',AL1(RREPKREP-RREPREC),AL2(0)                               
LRECLIST EQU   *-RECLIST                                                        
         DC    X'02',AL1(RSTAKREP-RSTAREC),AL2(0)                               
         DC    X'03',AL1(RREGKREP-RREGREC),AL2(0)                               
         DC    X'04',AL1(ROFFKREP-ROFFREC),AL2(0)                               
         DC    X'05',AL1(RTEMKREP-RTEMREC),AL2(0)                               
         DC    X'06',AL1(RSALKREP-RSALREC),AL2(0)                               
         DC    X'07',AL1(RGRPKREP-RGRPREC),AL2(0)                               
         DC    X'08',AL1(RADVKREP-RADVREC),AL2(0)                               
         DC    X'09',AL1(RPRDKREP-RPRDREC),AL2(0)                               
         DC    X'0A',AL1(RAGYKREP-RAGYREC),AL2(0)                               
         DC    X'0B',AL1(RBUYKREP-RBUYREC),AL2(0)                               
         DC    X'0C',AL1(RCONKREP-RCONREC),AL2(0)                               
         DC    X'0D',AL1(RCLSKREP-RCLSREC),AL2(0)                               
         DC    X'0F',AL1(RCTGKREP-RCTGREC),AL2(0)                               
         DC    X'11',AL1(06),AL2(0)                                             
         DC    X'12',AL1(RINVKREP-RINVREC),AL2(0)                               
         DC    X'13',AL1(RBUDKREP-RBUDREC),AL2(0)                               
         DC    X'14',AL1(RAVLKREP-RAVLREC),AL2(0)                               
         DC    X'15',AL1(23),AL2(0)                                             
         DC    X'16',AL1(RPRPKREP-RPRPREC),AL2(0)                               
         DC    X'18',AL1(REOMKREP-REOMREC),AL2(0)                               
         DC    X'19',AL1(17),AL2(0)                                             
         DC    X'1A',AL1(RAGYKREP-RAGYREC),AL2(0)                               
         DC    X'1F',AL1(16),AL2(0)                                             
         DC    X'20',AL1(01),AL2(0)                                             
         DC    X'22',AL1(13),AL2(0)                                             
         DC    X'23',AL1(23),AL2(0)                                             
         DC    X'24',AL1(24),AL2(0)                                             
         DC    X'25',AL1(24),AL2(0)                                             
         DC    X'26',AL1(20),AL2(0)                                             
         DC    X'27',AL1(01),AL2(0)                                             
         DC    X'28',AL1(13),AL2(0)                                             
         DC    X'29',AL1(11),AL2(0)                                             
         DC    X'2A',AL1(22),AL2(0)                                             
         DC    X'2B',AL1(21),AL2(0)                                             
         DC    X'2C',AL1(04),AL2(0)                                             
         DC    X'2D',AL1(12),AL2(0)                                             
         DC    X'2E',AL1(15),AL2(0)                                             
         DC    X'2F',AL1(24),AL2(0)                                             
         DC    X'30',AL1(17),AL2(0)                                             
         DC    X'31',AL1(22),AL2(0)                                             
         DC    X'32',AL1(24),AL2(0)                                             
         DC    X'33',AL1(17),AL2(0)                                             
         DC    X'34',AL1(20),AL2(0)                                             
         DC    X'36',AL1(17),AL2(0)                                             
         DC    X'38',AL1(19),AL2(0)                                             
         DC    X'3A',AL1(22),AL2(0)                                             
         DC    X'3B',AL1(23),AL2(0)                                             
         DC    X'3C',AL1(24),AL2(0)                                             
         DC    X'3D',AL1(23),AL2(0)                                             
         DC    X'42',AL1(20),AL2(0)                                             
         DC    X'44',AL1(23),AL2(0)                                             
         DC    X'46',AL1(22),AL2(0)                                             
         DC    X'47',AL1(21),AL2(0)                                             
         DC    X'49',AL1(16),AL2(0)                                             
         DC    X'FF'                                                            
*                                                                               
******************************************************************              
*  STATPROC:  PROCESS STATIONS RECORDS                           *              
*        PROCESSING STEPS NEEDED:                                *              
*        1.  FIND STATION BY REP IN TABLE.                       *              
*        2.  NOT FOUND:  OUTPUT STATION RECORD AS IS             *              
*        3.  FOUND:                                              *              
*            A.  LOOK IN FILED TABLE LIST - IF NOT THERE,        *              
*                IF STATION HAS NO LEAVE DATE, CHANGE OLD TO     *              
*                NEW REP, CHANGE GROUP/SUBGRP, OUTPUT RECORD     *              
*                                                                *              
******************************************************************              
STATPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,ASTAAREA         SET A(STATION TABLE)                         
         LA    R3,REC                                                           
         USING RSTAKEY,R3                                                       
*                                                                               
         L     RF,STAREAD                                                       
         LA    RF,1(RF)                                                         
         ST    RF,STAREAD                                                       
*                                                                               
         MVC   DUB(2),RSTAKREP     SET UP SEARCH ARGUMENT                       
         MVC   DUB+2(5),RSTAKSTA                                                
*                                                                               
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),16,7,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    STAT0030            YES                                          
         L     RF,NOCHGSTA         COUNT UNCHANGED STATION RECS                 
         LA    RF,1(RF)                                                         
         ST    RF,NOCHGSTA                                                      
*                                                                               
***      MVC   P+1(16),=C'STATION SKIPPED:'                                     
***      MVC   P+20(7),RSTAKREP                                                 
***      GOTO1 REPORT                                                           
*                                                                               
         L     RF,STASKIPD                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STASKIPD                                                      
*                                                                               
*   RECORD HAS BEEN ENTERED BY CLIENT, AND IS NOT IN THE STATION                
*        MOVE TABLE AS OLDREP/STATION/NEWREP.  IT WILL BE IN THE                
*        JOIN/LEAVE TABLE AS NEWREP/STATION/OLDREP, AND FLAGGED                 
*        AS 'ALREADY ON FILE'.  IT IS NECESSARY TO CALL THE J/L                 
*        TABLE, AND FIGURE OUT THE LATEST CLOSE DATE FOR THIS                   
*        STATION BEFORE PUTTING THE RECORD OUT.  RECORD MUST BE                 
*        ENTERED AFTER 11/20/00, AND HAVE NO CLOSE DATE, TO BE                  
*        PROCESSED FURTHER.                                                     
*                                                                               
         LA    RF,RSTAELEM         FIND STATION ACTIVITIES ELT                  
STAT0010 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    STAT0800            YES - DON'T PROCESS FURTHER                  
         CLI   0(RF),X'F1'         STA ACT ELT?                                 
         BE    STAT0012            YES                                          
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     STAT0010            GO BACK FOR NEXT                             
STAT0012 EQU   *                                                                
         CLC   =X'640A14',RSTACTCD-RSTACTEL(RF)                                 
         BH    STAT0800            LAST ACT PRE OCT20/00                        
         OC    RSTACLDT,RSTACLDT   ANY CLOSE DATE?                              
         BNZ   STAT0026            YES - LEAVE AS IS                            
*                                  NO                                           
*                                  SET UP SEARCH ARGUMENT                       
         MVC   DMWORK(2),RSTAKREP     THIS IS THE NEW REP                       
         MVC   DMWORK+2(5),RSTAKSTA   STATION CALLS                             
*                                                                               
         L     R2,AJOLAREA         SET A(JOIN/LEAVE TABLE)                      
         L     R4,JOLCTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
*   THIS IS A 7-CHARACTER SEARCH.  THERE WILL BE DUPLICATES.  WILL              
*        BE NECESSARY TO CHECK BEFORE AND AFTER THE ENTRY FOUND.                
*                                                                               
         PRINT GEN                                                              
         GOTO1 =V(BINSRCH),DMCB,DMWORK,(R2),(R4),JOLTABLN,7,(R4)                
         PRINT NOGEN                                                            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BNE   STAT0024            NO  - SKIP THIS ENTRY                        
*                                                                               
         L     R2,DMCB             SET A(RECORD FOUND)                          
         MVC   RSTACLDT,JCLOSDTE(R2)                                            
*                                  INSERT CLOSE DATE                            
         LR    RF,R2               CHECK PREVIOUS ENTRIES                       
*                                                                               
STAT0014 EQU   *                                                                
         S     RF,=F'20'           BACK UP TO PREVIOUS ENTRY                    
         CLC   0(7,RF),0(R2)       SAME NEW REP/STATION?                        
         BNE   STAT0016            NO  - GO CHECK FOLLOWING                     
         CLC   RSTACLDT,JCLOSDTE(RF)                                            
*                                  STATION CLOSE < PREVIOUS CLOSE?              
         BNL   STAT0014            NO  - CHECK NEXT PREVIOUS                    
         MVC   RSTACLDT,JCLOSDTE(RF)                                            
*                                  YES - REPLACE STA CLOSE WITH PREV            
         B     STAT0014            CHECK NEXT PREVIOUS                          
STAT0016 EQU   *                                                                
         LR    RF,R2                                                            
STAT0018 EQU   *                                                                
         LA    RF,JOLTABLN(RF)     BUMP TO NEXT ENTRY                           
         CLC   0(7,RF),0(R2)       SAME NEW REP/STATION?                        
         BNE   STAT0020            NO  - FINISHED FORE AND AFT                  
         CLC   RSTACLDT,JCLOSDTE(RF)                                            
*                                  STATION CLOSE < PREVIOUS CLOSE?              
         BNL   STAT0018            NO  - CHECK NEXT PREVIOUS                    
         MVC   RSTACLDT,JCLOSDTE(RF)                                            
*                                  YES - REPLACE STA CLOSE WITH NEXT            
         B     STAT0018            CHECK NEXT NEXT                              
*                                                                               
STAT0020 EQU   *                                                                
         MVC   P+1(11),=C'CLOSE DATE:'                                          
         MVC   P+13(02),RSTAKREP                                                
         MVI   P+15,C'/'                                                        
         MVC   P+16(5),RSTAKSTA                                                 
         MVI   P+21,C'/'                                                        
         XC    WORK(3),WORK                                                     
         MVC   WORK(2),RSTACLDT                                                 
         GOTO1 DATCON,DMCB,(3,WORK),(5,P+22)                                    
         GOTO1 REPORT                                                           
         B     STAT0800            EXIT CC NOT ZERO                             
*                                                                               
STAT0024 EQU   *                                                                
         MVC   P+1(11),=C'NOT IN J/L:'                                          
         MVC   P+13(02),RSTAKREP                                                
         MVI   P+15,C'/'                                                        
         MVC   P+16(5),RSTAKSTA                                                 
         GOTO1 REPORT                                                           
         B     STAT0800            EXIT CC NOT ZERO                             
STAT0026 EQU   *                                                                
         MVC   P+1(11),=C'ACT/LEAVE :'                                          
         MVC   P+13(02),RSTAKREP                                                
         MVI   P+15,C'/'                                                        
         MVC   P+16(5),RSTAKSTA                                                 
         GOTO1 REPORT                                                           
         B     STAT0800            EXIT CC NOT ZERO                             
STAT0030 EQU   *                                                                
         OC    RSTAEND,RSTAEND     ANY LEAVE DATE?                              
         BZ    STAT0040            NO  - USE IT                                 
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'STA: IN LIST AS:'                                     
         MVC   P+20(7),RSTAKREP                                                 
         MVC   P+30(14),=C'HAS LEAVE DATE'                                      
         GOTO1 REPORT                                                           
*                                                                               
         OI    RSTASTAT,X'40'      TURN ON 'CONTRACT=NO'                        
*                                                                               
         L     RF,STALVDAT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STALVDAT                                                      
*                                                                               
         B     STAT0800            OUTPUT AS IS                                 
STAT0040 EQU   *                                                                
*                                                                               
*   FIRST WRITE THE CURRENT RECORD TO OUTPUT, SO WE DON'T LOSE IT.              
*                                                                               
         L     R2,DMCB             SET A(RECORD FOUND)                          
*                                     DON'T STEP ON R2 !!!  ***                 
         L     RF,STAOUTPT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STAOUTPT                                                      
*                                                                               
         OI    RSTASTAT,X'40'      TURN ON 'CONTRACT=NO'                        
         GOTO1 DATCON,DMCB,(5,WORK),(3,RSTAEND)                                 
*                                  INSERT A LEAVE DATE                          
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUTREC ROUTINE             
         BAS   RE,PUTRECS          WRITE REC TO O/P                             
STAT0055 EQU   *                                                                
         NI    RSTASTAT,X'FF'-X'40'                                             
*                                  TURN OFF 'CONTRACT=NO' FOR NEW REC           
         XC    RSTAEND,RSTAEND     CLEAR THE LEAVE DATE                         
*                                                                               
*   NOW MODIFY THE RECORD AS INDICATED.                                         
*                                                                               
         MVC   RSTAGRUP,SNEWGRP(R2)                                             
*                                  INSERT NEW GROUP FROM TABLE                  
         MVC   SAVEKREP,RSTAKREP   SAVE ORIGINAL REP CODE                       
         MVC   RSTAKREP,SNEWREP(R2)                                             
*                                  INSERT NEW REP   FROM TABLE                  
         OI    SFLAGBYT(R2),X'80'  TURN ON 'STATION COPIED'                     
*                                                                               
*   NEED TO DETERMINE EARLIEST JOIN DATE FOR THIS STATION                       
*                                  SET UP SEARCH ARGUMENT                       
         MVC   DMWORK(2),RSTAKREP     THIS IS THE NEW REP                       
         MVC   DMWORK+2(5),RSTAKSTA   STATION CALLS                             
         MVC   DMWORK+7(2),SAVEKREP   THIS IS THE OLD REP                       
*                                                                               
         L     R2,AJOLAREA         SET A(JOIN/LEAVE TABLE)                      
         L     R4,JOLCTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
****     GOTO1 =V(BINSRCH),DMCB,DMWORK,(R2),(R4),18,9,(R4)                      
         PRINT GEN                                                              
         GOTO1 =V(BINSRCH),DMCB,DMWORK,(R2),(R4),JOLTABLN,9,(R4)                
         PRINT NOGEN                                                            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                MUST BE FOUND:  TABLES ARE                   
*                                     BUILT IN PARALLEL                         
         L     R2,DMCB             SET A(RECORD FOUND)                          
*                                                                               
         TM    JFLAGBYT(R2),X'20'  STATION ALREADY ON FILE?                     
         BO    STAT0160            YES - DON'T PUT OUT ANOTHER                  
*                                                                               
*   BACK UP TO FIND FIRST OF NEW REP/STATION SET:  CURRENT POINTER              
*        IS NEW REP/STATION/OLD REP, WHICH MAY NOT BE FIRST.                    
*                                                                               
STAT0060 EQU   *                                                                
         LR    RF,R2                                                            
         S     RF,=F'20'           BACK UP ONE TABLE ENTRY                      
         CLC   0(7,RF),0(R2)       PREVIOUS = CURRENT NEWREP/STN?               
         BNE   STAT0080            NO  - USE CURRENT                            
         LR    R2,RF               BACK UP CURRENT POINTER                      
         B     STAT0060            GO BACK AND DO IT AGAIN                      
STAT0080 EQU   *                                                                
         CLC   JJOINDAT(3,R2),RSTASTRT                                          
*                                  TABLE JOIN DATE < RECORD?                    
         BNL   STAT0100            NO  - LEAVE RECORD DATE ALONE                
         MVC   RSTASTRT,JJOINDAT(R2)                                            
*                                  YES - REPLACE RECORD DATE WITH TABLE         
*                                     THEN CHECK NEXT SLOT                      
STAT0100 EQU   *                                                                
         LR    RF,R2               CHECK NEXT SLOT                              
         LA    RF,JOLTABLN(RF)     BUMP TO NEXT SLOT                            
         CLC   0(7,RF),0(R2)       NEXT = CURRENT NEWREP/STN?                   
         BNE   STAT0120            NO  - FINISHED                               
         LR    R2,RF               ADVANCE CURRENT POINTER                      
         B     STAT0080            GO BACK AND CHECK DATES                      
STAT0120 EQU   *                                                                
         OI    JFLAGBYT(R2),X'80'  TURN ON 'STATION COPIED'                     
*                                                                               
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0A'        FIND COMBO PARENT ELT                        
         BAS   RE,GETEL                                                         
         BNE   STAT0130            NO COMBO CONTRO ELT                          
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'FOLLOWING STA HAD COMBO PARENT'                       
         GOTO1 REPORT                                                           
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'0A',RSTAREC),0,0            
*                                  DELETE COMBO PARENT ELT                      
STAT0130 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'STA COPIED     :'                                     
         GOTO1 REPORT                                                           
         LA    R4,RSTAREC          A(STATION RECORD)                            
         SR    RF,RF                                                            
         ICM   RF,3,RSTALEN                                                     
         CLI   QUESTOR+4,C'Y'      SHORT DISPLAY                                
         BNE   STAT0140            NO                                           
         LA    RF,32               YES                                          
STAT0140 EQU   *                                                                
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                                                               
         L     RF,STACOPYD                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STACOPYD                                                      
*                                                                               
         B     STAT0800                                                         
STAT0160 EQU   *                                                                
*                                                                               
         OI    JFLAGBYT(R2),X'10'  TURN ON 'ALREADY,ALREADY' FLAG               
         GOTO1 REPORT                                                           
         MVC   P+1(20),=C'STA ALREADY ON FILE:'                                 
         GOTO1 REPORT                                                           
         LA    R4,RSTAREC          A(STATION RECORD)                            
         SR    RF,RF                                                            
         ICM   RF,3,RSTALEN                                                     
         CLI   QUESTOR+4,C'Y'      SHORT DISPLAY                                
         BNE   STAT0180            NO                                           
         LA    RF,32               YES                                          
STAT0180 EQU   *                                                                
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                                                               
         L     RF,STAONFIL         INCREMENT STATION ALREADY ON FILE            
         LA    RF,1(RF)                                                         
         ST    RF,STAONFIL                                                      
*                                                                               
         B     STAT0900                                                         
STAT0800 EQU   *                                                                
*                                                                               
         L     RF,STAOUTPT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STAOUTPT                                                      
*                                                                               
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUTREC ROUTINE             
         BAS   RE,PUTRECS          WRITE REC TO O/P                             
*                                                                               
STAT0900 EQU   *                                                                
         XIT1                                                                   
SAVEKREP DS    CL2                                                              
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
******************************************************************              
*  STASWEEP:  CHECK FOR NON-GENERATED STATION RECORDS.           *              
*        STATIONS WHICH ARE MOVING, BUT HAVE NO ACTIVE STATION   *              
*        RECORD ON ANY REP INDICATED (ALL HAVE LEAVE DATE)       *              
*        NEED TO BE CREATED ON OUTPUT.                           *              
*        JOIN/LEAVE TABLE TO BE SCANNED FOR NEWREP/STA, NO       *              
*        COPY FLAG SET.  STATION RECORDS WILL BE GENERATED WHEN  *              
*        THIS SITUATION IS RECOGNIZED.                           *              
*                                                                *              
******************************************************************              
STASWEEP NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,AJOLAREA         SET A(STATION TABLE)                         
         LR    R3,R2               SET SAME ADDRESS                             
         MVC   SAVEKEY,KEY         SAVE KEY FOR RESTART                         
*                                                                               
*   R3 WILL BE HELD AT THE FIRST ENTRY OF NEWREP/STA, WHILE R2                  
*        WILL CHECK UPCOMING SLOTS.  IF A RECORD NEEDS TO BE                    
*        GENERATED, R3 WILL POINT TO THE FIRST (AND POSSIBLY ONLY)              
*        ENTRY FOR THAT NEWREP/STATION, AND R2 WILL POINT TO WHERE              
*        TO BEGIN ONCE RECORD IS OUTPUT.                                        
*                                                                               
SSWE0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    SSWE0800            YES - RESTART KEY                            
         TM    JFLAGBYT(R2),X'80'  STATION RECORD GENERATED?                    
         BO    SSWE0060            YES - SKIP THIS NEWREP/STATION               
         TM    JFLAGBYT(R2),X'08'  STATION RECORD DOESN'T EXIST?                
         BO    SSWE0065            YES - SKIP THIS NEWREP/STATION               
         TM    JFLAGBYT(R2),X'20'  STATION ALREADY ON FILE?                     
         BO    SSWE0070            YES - SKIP THIS NEWREP/STATION               
         LA    R2,JOLTABLN(R2)     NO  - BUMP TO NEXT SLOT                      
         CLC   0(7,R2),0(R3)       NEXT SLOT NEWREP/STA = CURRENT?              
         BNE   SSWE0100            NO  - NO OUTPUT FOR CURRENT                  
*                                     NEWREP/STA - NEED TO CREATE               
         B     SSWE0020            YES - CHECK NEXT SLOT FOR OUTPUT             
SSWE0060 EQU   *                                                                
         MVC   P+1(18),=C'STATION GENERATED:'                                   
         MVC   P+20(9),0(R2)                                                    
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,STAPASS1                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STAPASS1                                                      
         B     SSWE0080                                                         
SSWE0065 EQU   *                                                                
         MVC   P+1(18),=C'STATION IS GHOST :'                                   
         MVC   P+20(9),0(R2)                                                    
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,STAGHOST                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STAGHOST                                                      
         B     SSWE0080                                                         
SSWE0070 EQU   *                                                                
         TM    JFLAGBYT(R2),X'10'  'ALREADY,ALREADY' SET?                       
         BO    SSWE0080            YES - DON'T COUNT THIS ONE TWICE             
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(20),=C'STA ALREADY ON FILE:'                                 
         MVC   P+24(2),JNEWREP(R2)                                              
         MVC   P+28(5),JSTATION(R2)                                             
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,STAONFIL         INCREMENT STATION ALREADY ON FILE            
         LA    RF,1(RF)                                                         
         ST    RF,STAONFIL                                                      
*                                                                               
         B     SSWE0080                                                         
*                                                                               
SSWE0080 EQU   *                                                                
         LA    R2,JOLTABLN(R2)     BUMP TO NEXT SLOT                            
         CLC   0(7,R2),0(R3)       NEXT SLOT NEWREP/STA = CURRENT?              
         BE    SSWE0080            YES - SKIP IT                                
         LR    R3,R2               NO  - SET R3 TO FIRST ENTRY OF               
*                                     NEWREP/STATION                            
         B     SSWE0020            GO BACK FOR NEXT SLOT                        
SSWE0100 EQU   *                                                                
         BAS   RE,GENSTAT          GENERATE A NEW STATION RECORD                
         LR    R3,R2               SET R3 TO FIRST ENTRY OF                     
*                                     NEWREP/STATION                            
         B     SSWE0020            GO BACK FOR NEXT SLOT                        
SSWE0800 EQU   *                                                                
         MVC   KEY,SAVEKEY         RESTART KEY                                  
         GOTO1 HIGHDIR             READ THE KEY                                 
         GOTO1 GETRECRD            READ THE RECORD                              
         MVC   P+1(23),=C'RESTART AFTER STATIONS:'                              
         MVC   P+25(27),REC                                                     
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   GENERATE A STATION RECORD FOR THE INACTIVE STATION IN THE                   
*        JOIN/LEAVE ARRAY.                                                      
*                                                                               
GENSTAT  NTR1                                                                   
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,2               SET REC TYPE                                 
         MVC   KEY+20(2),JOLDREP(R3)                                            
*                                  INSERT OLD REP FROM 1ST TABLE ENTRY          
         MVC   KEY+22(5),JSTATION(R3)                                           
*                                  INSERT STATION                               
         GOTO1 HIGHDIR             GET KEY                                      
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                NO  - MUST BE ON FILE                        
         GOTO1 GETRECRD                                                         
         LA    R6,REC                                                           
         USING RSTAREC,R6                                                       
         MVC   RSTAKREP,JNEWREP(R3)                                             
*                                  CHANGE THE REP CODE IN THE RECORD            
*                                  RESET DATES IN RECORD                        
         LR    R2,R3                                                            
GENS0020 EQU   *                                                                
         LA    R2,JOLTABLN(R2)     BUMP R2 TO NEXT SLOT                         
         CLC   0(7,R3),0(R2)       CURRENT SLOT = NEXT SLOT?                    
         BNE   GENS0100            NO  - NO MORE DATE CHECK                     
         CLC   JJOINDAT(R2),RSTASTRT                                            
*                                  YES - TABLE JOIN < RECORD JOIN?              
         BNL   GENS0040            NO                                           
         MVC   RSTASTRT,JJOINDAT(R2)                                            
*                                  YES - REPLACE JOIN  DATE                     
GENS0040 EQU   *                                                                
         CLC   RSTAEND,JLEAVDAT(R2)                                             
*                                  RECORD LEAVE DATE < TABLE LEAVE?             
         BNL   GENS0020            NO  - GO BACK FOR NEXT SLOT                  
         MVC   RSTAEND,JLEAVDAT(R2)                                             
*                                  YES - REPLACE LEAVE DATE                     
         B     GENS0020            GO BACK FOR NEXT SLOT                        
GENS0100 EQU   *                                                                
*                                                                               
*   USE OLD REP + STATION TO ACCESS ASTAAREA TABLE TO GET THE                   
*        NEW GROUP/SUBGROUP FOR THIS STATION                                    
*                                                                               
         L     R2,ASTAAREA         SET A(STATION TABLE)                         
         MVC   DUB(2),JOLDREP(R3)  SET UP SEARCH ARGUMENT                       
         MVC   DUB+2(5),JSTATION(R3)                                            
*                                                                               
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),16,7,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                MUST BE FOUND                                
         L     R2,DMCB             SET A(RECORD FOUND)                          
         MVC   RSTAGRUP,SNEWGRP(R2)    INSERT NEW GROUP/SUBGROUP                
*                                                                               
         LA    R7,RSTAELEM                                                      
GENS0120 EQU   *                                                                
         CLI   0(R7),0             END OF RECORD?                               
         BE    GENS0160            YES                                          
         CLI   0(R7),X'0A'         COMBO PARENT ELT?                            
         BE    GENS0140            YES                                          
         ZIC   RF,1(R7)                                                         
         AR    R7,RF                                                            
         B     GENS0120            NO  - BUMP TO NEXT ELT                       
GENS0140 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'FOLLOWING STA HAD COMBO PARENT'                       
         GOTO1 REPORT                                                           
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'0A',RSTAREC),0,0            
*                                  DELETE COMBO PARENT ELT                      
GENS0160 EQU   *                                                                
         MVC   P+1(17),=C'INACT STA COPIED:'                                    
         GOTO1 REPORT                                                           
         LA    R4,RSTAREC          A(STATION RECORD)                            
         SR    RF,RF                                                            
         ICM   RF,3,RSTALEN                                                     
         CLI   QUESTOR+4,C'Y'      SHORT DISPLAY                                
         BNE   GENS0170            NO                                           
         LA    RF,32               YES                                          
GENS0170 EQU   *                                                                
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                                                               
         L     RF,STAPASS2                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STAPASS2                                                      
*                                                                               
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUTREC ROUTINE             
         BAS   RE,PUTRECS          WRITE REC TO O/P                             
*                                                                               
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*  CONTPROC:  PROCESS EACH CONTRACT RECORD                       *              
*        1.  FIND REP/STATION OF CONTRACT IN TABLE               *              
*            A.  NOT FOUND:  OUTPUT RECORD AS IS,EXIT            *              
*            B.  FOUND:  PROCEED TO 2                            *              
*        2.  A.  CHANGE OLD TO NEW REP                           *              
*            B.  CHANGE OLD TO NEW CONTRACT NUMBER - APPLY       *              
*                ADJUSTMENT FACTOR DETERMINED ON ANALYSIS OF     *              
*                COMPANY NUMBERING RELATIONSIPS                  *              
*            C.  GENERATE SORT RECORD TO PERMIT CHANGING CON#    *              
*                OF BUYS TO SAME NUMBER                          *              
*            D.  BLOW AWAY ALL COMBO ORDER ELEMENTS IN THE REPS  *              
*                PARTICIPATING IN THE MOVE                       *              
*            E.  LEAVE SALES ASSISTANT.  ADD CONVERT COMMENT     *              
*                                                                *              
******************************************************************              
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         L     RF,CONCTR           COUNT TOTAL RECORDS SCANNED                  
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR           RESTORE COUNTER                              
*                                                                               
*   TEST DISPLAY                                                                
**       CLC   CONCTR,=F'100'                                                   
**       BH    TSTC0010                                                         
**       EDIT  CONCTR,(4,P+1)                                                   
**       MVC   P+6(09),=C'CONTRACT:'                                            
**       MVC   P+16(27),KEY                                                     
**       GOTO1 REPORT                                                           
TSTC0010 EQU   *                                                                
*                                                                               
         LA    R5,REC              IN CASE RECORD IS DELETED BUT THE            
         USING RCONREC,R5             PASSIVE POINTERS AREN'T                   
*                                                                               
*                                                                               
*   TEST DISPLAY                                                                
***      CLC   RCONKCON,=X'03248802'                                            
***      BNE   TSTC0020                                                         
***      MVC   P+6(22),=C'SPECIAL CONTRACT FOUND'                               
***      MVC   P+40(27),KEY                                                     
***      GOTO1 REPORT                                                           
***      DC    H'0'                                                             
TSTC0020 EQU   *                                                                
*                                                                               
         TM    RCONCNTL,X'80'      SKIP IF RECORD IS REALLY MARKED              
         BO    CONT0260               FOR DELETION                              
*                                                                               
         L     R2,ASTAAREA         SET A(STATION TABLE)                         
         MVC   DUB(2),RCONKREP     SET UP SEARCH ARGUMENT                       
         MVC   DUB+2(5),RCONKSTA                                                
*                                                                               
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),16,7,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    CONT0020            YES                                          
         L     RF,NOCHGCON         COUNT UNCHANGED CONTRACT RECS                
         LA    RF,1(RF)                                                         
         ST    RF,NOCHGCON                                                      
*                                  SET DUMMY REP AREA                           
         MVC   DUMMYREP(2),RCONKREP                                             
         MVC   DUMMYREP+2(5),RCONKSTA                                           
         MVC   DUMMYREP+7(2),RCONKREP                                           
*                                  OLD = NEW REP HERE                           
         MVC   DUMMYREP+9(2),RCONKGRP                                           
         MVC   DUMMYREP+11(2),RCONKGRP                                          
*                                  OLD = NEW GROUP HERE                         
         LA    R2,DUMMYREP         SET A(DUMMYREP) FOR ROUTINE                  
         GOTO1 SETREP#S,DMCB,(RC),(R2),(R5)                                     
*                                                                               
         CLI   QUESTOR+6,C'Y'                                                   
         BNE   CONT0010                                                         
         CLC   NOCHGCON,=F'10'       PROCESS 1ST N RECS ACCEPTED                
         BH    CONT0010                                                         
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR DISPLAY                    
         GOTO1 =A(DISPPUT2),DMCB,(RC),1   DISPLAY NO-CHANGE CONTRACT            
CONT0010 DS    0H                                                               
         CLI   QUESTOR+3,C'Y'      SKIP NOCHG REP/STNS?                         
         BE    CONT0300            YES - DON'T OUTPUT IT                        
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR PUTREC ROUTINE             
         BAS   RE,PUTRECS          PUT OUT CON W/NO REP CHANGE                  
         B     CONT0300            DUMMY 'SWAP' DONE.  SORT REC                 
*                                     HAS BEEN GEN'D FOR BUY PROC               
CONT0020 EQU   *                                                                
         L     RF,CHGCON           COUNT CHANGED CONTRACT RECS                  
         LA    RF,1(RF)                                                         
         ST    RF,CHGCON                                                        
         L     R2,DMCB             SET A(RECORD FOUND)                          
*                                                                               
         CLI   QUESTOR+6,C'Y'                                                   
         BNE   CONT0040                                                         
         CLC   CHGCON,=F'10'       PROCESS 1ST N RECS ACCEPTED                  
         BH    CONT0040                                                         
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR DISPLAY                    
         GOTO1 DISPPUT,DMCB,(RC),4 DISPLAY PRE-CHANGE                           
CONT0040 DS    0H                                                               
*                                                                               
         GOTO1 SETREP#S,DMCB,(RC),(R2),(R5)                                     
*                                  SET NEW REP CODE, CONTRACT #/                
*                                     ALSO OUTPUT SORT                          
         GOTO1 =A(DROPCOMS),DMCB,(RC),(R5)                                      
*                                                                               
*   RETRIEVE CORRECT SALESPERSON CONVERSION INFORMATION                         
*        ROUTINE MUST BE MODIFIED TO SWAP S/P BASED ON OFFICE                   
*                                                                               
         GOTO1 SETSALES,DMCB,(RC),(R5)                                          
*                                                                               
CONT0060 EQU   *                                                                
         GOTO1 SETELTS,DMCB,(RC),(R5),(R2)                                      
***      GOTO1 SASSIST,DMCB,(RC),(R5)                                           
*                                                                               
         LA    R4,RCONELEM         FIND X'20' ELEMENT                           
CONT0080 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CONT0130            YES -                                        
         CLI   0(R4),X'20'         SEND INFO ELEMENT?                           
         BE    CONT0120            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CONT0080            GO BACK FOR NEXT                             
CONT0120 EQU   *                                                                
         XC    2(2,R4),2(R4)       FOUND:  SET SEND ID TO ZERO                  
*                                                                               
*   REP SENDING ID IS BEING CLEARED.  CONTRACT PROGRAM WILL CHECK               
*        THIS FIELD FOR EMPTY, AND REINSERT.                                    
*                                                                               
CONT0130 EQU   *                                                                
CONT0180 EQU   *                                                                
**       CLC   RCONPRD,SPACES      ANY PRODUCT CODE?                            
**       BE    CONT0200            NO  - DON'T RETRIEVE                         
**       GOTO1 PRODCODE,DMCB,(RC),RCONREC                                       
*                                  CHECK/PROCESS PRODUCT CODE                   
CONT0200 EQU   *                                                                
         MVC   OLDSTA,RCONKSTA     SAVE STATION CALL LETTERS                    
         MVC   OLDOFF,RCONKOFF     SAVE OFFICE CODE                             
*                                                                               
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR PUTREC ROUTINE             
         BAS   RE,PUTRECS                                                       
*                                                                               
         CLI   QUESTOR+6,C'Y'                                                   
         BNE   CONT0220                                                         
         CLC   CHGCON,=F'10'       PROCESS 1ST N RECS ACCEPTED                  
         BH    CONT0220                                                         
         GOTO1 DISPPUT,DMCB,(RC),0 DISPLAY POST-CHANGE                          
CONT0220 DS    0H                                                               
         ZICM  RF,RCONLEN,2        ADD ALL CONTRACT LENGTHS                     
         A     RF,CONBYTES                                                      
         ST    RF,CONBYTES                                                      
         B     CONT0300            EXIT CONTRACT PROCESSING                     
*                                                                               
CONT0260 DS    0H                                                               
         L     RF,CONDEL           COUNT DELETED CONTRACTS                      
         LA    RF,1(RF)                                                         
         ST    RF,CONDEL                                                        
         MVC   P+1(16),=C'SKIPPED: DELETED'                                     
         GOTO1 REPORT                                                           
         B     CONT0300            GO BACK FOR NEXT                             
CONT0300 DS    0H                                                               
*                                                                               
         L     RF,CONCTR3          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR3                                                       
         CLC   CONCTR3,=F'1000'    DISPLAY EVERY N RECORDS                      
         BNE   CONT0320                                                         
         XC    CONCTR3,CONCTR3                                                  
         MVC   P+1(21),=C'PROCESSING CONTRACTS:'                                
         EDIT  CONCTR,(7,P+24)                                                  
         GOTO1 REPORT                                                           
CONT0320 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*   SETREP#S:  SET NEW REP CODE.  CONVERT CONTRACT NUMBER BASED ON              
*        OLD REP CODE.  PUT OUT SORT RECORD.                                    
*                                                                               
SETREP#S NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(STATION TABLE ENTRY)                 
         L     R5,8(R1)            RESET A(CONTRACT RECORD IN PROCESS)          
         USING RCONREC,R5                                                       
*                                                                               
         MVC   RCONKSTA,SSTATION(R2)                                            
*                                  INSERT NEW STATION                           
         MVC   RCONKGRP,SNEWGRP(R2)                                             
*                                  INSERT NEW GROUP/SUBGROUP                    
*   THESE VALUES MAY BE FROM THE 'DUMMY', IN WHICH CASE THEY ARE THE            
*        SAME AS THE ORIGINALS:  NO CHANGE.                                     
*                                                                               
         XC    SORTREC,SORTREC     CLEAR SORTRECORD AREA                        
         XC    OLDCONV#,OLDCONV#   CLEAR WORK AREA                              
         MVC   OLDCONV#,SOLDREP(R2)                                             
*                                  SET CONVERSION FACTOR LOOKUP                 
         LA    R3,CONVTABL         SET A(CONVERSION TABLE)                      
SREP0020 EQU   *                                                                
         CLI   0(R3),0             END OF TABLE REACHED?                        
         BE    SREP0800            YES - NO CONVERSION NEEDED                   
         CLC   OLDCONV#,0(R3)      CONVERSION IN TABLE?                         
         BE    SREP0040            FOUND IN TABLE                               
         LA    R3,CONVLEN(R3)                                                   
         B     SREP0020            GO BACK FOR NEXT                             
SREP0040 EQU   *                                                                
*                                                                               
*   CONTRACT IS BEING CONVERTED (CON#, REP CODE).  ALSO MUST DROP               
*        THE COMBO CONTROL ELEMENT.                                             
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'        FIND CONTRACT COMBO CONTROL ELT              
         BAS   RE,GETEL                                                         
         BNE   SREP0100            NO COMBO CONTRO ELT                          
*                                                                               
         CLC   COMBOCTL,=F'5'      DISPLAY FIRST HALF DOZEN                     
         BH    SREP0060                                                         
         GOTO1 REPORT                                                           
         MVC   P+1(16),=C'COMBO DROP: PRE:'                                     
         GOTO1 REPORT                                                           
         LA    R4,RCONREC          A(STATION RECORD)                            
         SR    RF,RF                                                            
         ICM   RF,3,RCONLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                                                               
SREP0060 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'17',RCONREC),0,0            
*                                  DELETE COMBO CONTROL ELT                     
         CLC   COMBOCTL,=F'5'      DISPLAY FIRST HALF DOZEN                     
         BH    SREP0080                                                         
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'COMBO DROP: POST:'                                    
         GOTO1 REPORT                                                           
         LA    R4,RCONREC          A(STATION RECORD)                            
         SR    RF,RF                                                            
         ICM   RF,3,RCONLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                                                               
SREP0080 EQU   *                                                                
         L     RF,COMBOCTL                                                      
         LA    RF,1(RF)                                                         
         ST    RF,COMBOCTL                                                      
*                                                                               
SREP0100 EQU   *                                                                
         MVI   STYP,1              SET RECORD TYPE TO 'CONTRACT'                
         MVC   STOLDREP,RCONKREP      INSERT OLD REP INTO SORT REC              
         MVC   STNEWREP,SNEWREP(R2)   INSERT NEW REP INTO SORT REC              
         GOTO1 HEXOUT,DMCB,RCONKCON,MYWORK,4,=C'TOG'                            
*                                                                               
*   TEST                                                                        
*        MVC   P+1(05),=C'CON#:'                                                
*        MVC   P+6(8),MYWORK                                                    
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         PACK  DUB(8),MYWORK(8)                                                 
*                                                                               
*   TEST                                                                        
*        MVC   P+1(05),=C'PAK#:'                                                
*        MVC   P+6(8),DUB                                                       
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         AP    DUB(8),2(4,R3)      ADD THIS CONT# ADJUSTMENT                    
*                                                                               
*   TEST                                                                        
*        MVC   P+1(05),=C'PAK+:'                                                
*        MVC   P+6(8),DUB                                                       
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         UNPK  MYWORK(8),DUB(8)                                                 
         OI    MYWORK+7,X'F0'      TURN ON ALL ZONE BITS                        
*                                                                               
*   TEST                                                                        
*        MVC   P+1(05),=C'UNP#:'                                                
*        MVC   P+6(8),MYWORK                                                    
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         GOTO1 =V(HEXIN),DMCB,MYWORK,MYWORK+12,8,=C'TOG'                        
*                                                                               
*   TEST                                                                        
*        MVC   P+1(05),=C'C#IN:'                                                
*        MVC   P+6(4),MYWORK+12                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVC   OLDCON,RCONKCON     SAVE ORIGINAL CONTRACT #                     
         CLC   HIGHCON,OLDCON      GREATER THAN LAST HIGH?                      
         BH    SREP0120            NO                                           
         MVC   HIGHCON,OLDCON      YES - REPLACE LAST WITH THIS ONE             
SREP0120 EQU   *                                                                
         CLC   LOWCON,OLDCON       LESS THAN LAST LOW?                          
         BL    SREP0140            NO                                           
         MVC   LOWCON,OLDCON       YES - REPLACE LAST WITH THIS ONE             
SREP0140 EQU   *                                                                
         MVC   NEWCON,MYWORK+12    SAVE NEW CON# FOR BUYS/MG/ETC                
*                                                                               
*   CALCULATE NEW CONTRACT NUMBER REVERSED FOR BUYLINES                         
*                                                                               
         ZAP   MYWORK+30(5),=P'0'                                               
         MVO   MYWORK+30(5),NEWCON                                              
         ZAP   MYWORK+25(5),=P'99999999'                                        
         SP    MYWORK+25(5),MYWORK+30(5) GET 9'S COMPLEMENT                     
         MVO   MYWORK+20(5),MYWORK+25(5) CHANGE TO PWOS                         
                                                                                
         PACK  NEWCONRV+0(1),MYWORK+23(1) REVERSED 9'COMP OF K NUM              
         PACK  NEWCONRV+1(1),MYWORK+22(1)                                       
         PACK  NEWCONRV+2(1),MYWORK+21(1)                                       
         PACK  NEWCONRV+3(1),MYWORK+20(1)                                       
*                                                                               
*                                                                               
*   CALCULATE OLD CONTRACT NUMBER REVERSED FOR SORT RECORDS                     
*                                                                               
         ZAP   MYWORK+30(5),=P'0'                                               
         MVO   MYWORK+30(5),OLDCON                                              
         ZAP   MYWORK+25(5),=P'99999999'                                        
         SP    MYWORK+25(5),MYWORK+30(5) GET 9'S COMPLEMENT                     
         MVO   MYWORK+20(5),MYWORK+25(5) CHANGE TO PWOS                         
                                                                                
         PACK  OLDCONRV+0(1),MYWORK+23(1) REVERSED 9'COMP OF K NUM              
         PACK  OLDCONRV+1(1),MYWORK+22(1)                                       
         PACK  OLDCONRV+2(1),MYWORK+21(1)                                       
         PACK  OLDCONRV+3(1),MYWORK+20(1)                                       
*                                                                               
         MVC   STOLDCON,OLDCONRV   SET SORT OLD CON # REVERSED                  
         MVC   STNEWCON,NEWCONRV   SET SORT NEW CON # REVERSED                  
*                                                                               
         MVI   STYP,1              SET SORT TYPE TO 1                           
         MVC   SAVESORT,SORTREC    SAVE SORTREC                                 
         CLI   QUESTOR+3,C'Y'      OUTPUT ONLY STATIONS BEING CHANGED?          
         BNE   SREP0160            NO  - OUTPUT ALL                             
         CLC   STOLDREP,STNEWREP   OLD = NEW REP?                               
         BE    SREP0180            YES - UNCHANGED:  SKIP SORT OUTPUT           
SREP0160 EQU   *                                                                
         GOTO1 =A(SORTGEN)         OUTPUT SORT RECORD                           
SREP0180 EQU   *                                                                
*                                                                               
*   TEST DISPLAY                                                                
**       MVC   P+1(13),=C'BUYLINE CON#:'                                        
**       MVC   P+14(4),NEWCONRV                                                 
**       GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
*                                                                               
         MVC   RCONKCON,MYWORK+12      INSERT ADJUSTED CONTRACT NUMBER          
         MVC   RCONKGRP,SNEWGRP(R2)    INSERT NEW GROUP FOR STATION             
         MVC   RCONKREP,SNEWREP(R2)                                             
*                                  REPLACE OLD WITH NEW REP CODE                
SREP0800 EQU   *                                                                
*                                                                               
         BAS   RE,CHECKCOV                                                      
         XIT1                                                                   
         EJECT                                                                  
CHECKCOV NTR1                                                                   
         LA    R4,RCONELEM         FIND X'20' ELEMENT                           
CCOV0020 EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BE    CCOV0900            YES -                                        
         CLI   0(R4),X'A6'         CONTRACT COVERSHEET LINK ELT?                
         BE    CCOV0040            YES - CHECK IT                               
         ZIC   RE,1(R4)                                                         
         AR    R4,RE               BUMP TO NEXT ELEMENT                         
         B     CCOV0020            GO BACK FOR NEXT                             
CCOV0040 EQU   *                                                                
         MVC   RCONCVNM+4-RCONCVEL(4,R4),NEWCON                                 
*                                  INSERT NEW CONTRACT NUMBER INTO ELT          
         MVC   SORTREC,SAVESORT    RESET SORT RECORD                            
         MVI   STYP,2              SET SORT RECORD TYPE FOR COVERSHEET          
         MVC   STOLDCON,OLDCON     INSERT OLD CONTRACT NUMBER                   
         MVC   STNEWCON,NEWCON     INSERT NEW CONTRACT NUMBER                   
*                                                                               
         CLI   QUESTOR+1,C'Y'      PRINT COVERFLAG PROMPT?                      
         BNE   CCOV0060            NO                                           
         MVC   P+1(11),=C'COVER OUT :'                                          
         MVC   P+12(16),SORTREC                                                 
         GOTO1 HEXOUT,DMCB,STOLDCON,P+30,4,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,STNEWCON,P+40,4,=C'TOG'                              
         MVC   P+50(5),RCONKSTA                                                 
         GOTO1 REPORT                                                           
*                                                                               
CCOV0060 EQU   *                                                                
         GOTO1 =A(SORTGEN)         OUTPUT SORT RECORD                           
CCOV0900 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
CONVTABL DC    C'CR',P'0000000'  CHRISTAL:      NO ADJUSTMENT                   
CONVLEN  EQU   *-CONVTABL                                                       
         DC    C'NU',P'0000000'  CLEAR CHANNEL: NO ADJUSTMENT                   
         DC    C'S3',P'0500000'  SENTRY       : ADD .5 MILLION                  
         DC    C'EA',P'3000000'  EASTMAN:       ADD 3 MILLION                   
         DC    C'KU',P'6000000'  KATZ RADIO   : ADD 6 MILLION                   
****>>>> DC    C'KF',P'9000000'  KATZ HISPANIC: ADD 9 MILLION                   
         DC    X'0000'                                                          
         EJECT                                                                  
*                                                                               
*   BUYSPROC:  BUYS WILL BE SCANNED.  FOR THOSE WHICH HAVE A SORT               
*        RECORD (THESE WILL BE SEQUENCED IN THE SAME ORDER AS                   
*        THE BUYS), THE REP AND CONTRACT NUMBER WILL BE CHANGED                 
*        APPROPRIATELY.                                                         
*                                                                               
BUYSPROC NTR1                                                                   
         GOTO1 REPORT                                                           
         MVC   P+4(25),=C'***BEGIN BUY PROCESSING**'                            
         GOTO1 REPORT                                                           
         XC    LASTSORT,LASTSORT                                                
         GOTO1 =A(GETSORT)         RETRIEVE FIRST SORT RECORD                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           SET UP FOR FIRST BUY KEY                     
         GOTO1 HIGHDIR                                                          
         B     BPRO0040                                                         
BPRO0020 EQU   *                                                                
         GOTO1 SEQDIR                                                           
BPRO0040 EQU   *                                                                
         CLI   KEY,X'0B'           BUY RECORD?                                  
         BH    BPRO0800            NO  - FINISHED WITH BUYS                     
         L     RF,BUYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
***>>>                                                                          
         MVC   REC(27),KEY                                                      
         LA    R2,REC                                                           
         USING RBUYREC,R2                                                       
BPRO0060 EQU   *                                                                
         CLI   STYP,1              BUY-TYPE SORT RECORD?                        
         BNE   BPRO0240            NO  - O/P AS IS (ALL SORTRECS                
*                                     FOR BUYS HAVE BEEN RETURNED)              
         CLC   RBUYKREP(6),STOLDREP                                             
*                                  BUY REP/CON# = SORT REP/CON#?                
         BE    BPRO0080            YES - UPDATE INFORMATION                     
         BL    BPRO0240            BUY < SORT:  O/P AS IS                       
*                                                                               
*   CHECK FOR SORT RECORD WITH NO BUYS.  IF NOT SAME AS LASTSORT,               
*        NOTHING WAS MATCHED AGAINST IT.  HAVE TO CHECK THIS                    
*        SITUATION OUT.                                                         
*                                                                               
         CLC   SORTREC,LASTSORT    AT LEAST ONE BUY MATCHED?                    
         BE    BPRO0070            YES                                          
         L     RF,NOBUYCON         CONTRACTS WITH NO BUYS                       
         LA    RF,1(RF)                                                         
         ST    RF,NOBUYCON                                                      
**       MVC   P+1(14),=C'CON W/NO BUYS:'                                       
**       MVC   P+15(16),SORTREC                                                 
**       GOTO1 REPORT                                                           
*                                                                               
BPRO0070 EQU   *                                                                
         GOTO1 =A(GETSORT)         BUY > SORT: READ ANOTHER SORT                
         B     BPRO0060            CHECK THIS SORT VS BUY RECORD                
BPRO0080 EQU   *                                                                
         GOTO1 GETRECRD            RETRIEVE MATCHED BUY RECORD                  
         CLC   SORTREC,LASTSORT    THIS SORT KEY ALREADY COUNTED?               
         BE    BPRO0100            YES                                          
         MVC   LASTSORT,SORTREC    NO                                           
         L     RF,MATCHSRT         INCREMENT SORT RECORDS MATCHED               
         LA    RF,1(RF)               BY AT LEAST ONE BUY                       
         ST    RF,MATCHSRT                                                      
BPRO0100 EQU   *                                                                
         L     RF,MATCHBUY         INCREMENT MATCHED BUY COUNTER                
         LA    RF,1(RF)                                                         
         ST    RF,MATCHBUY                                                      
         CLI   QUESTOR+5,C'Y'      DISPLAY BUY MATCHES?                         
         BNE   BPRO0220            NO                                           
         CLC   MATCHBUY,=F'20'     DISPLAY FIRST N MATCHES                      
         BH    BPRO0220                                                         
         MVC   P+1(10),=C'BUY MATCH:'                                           
         MVC   P+12(6),RBUYKREP                                                 
         MVI   P+18,C'/'                                                        
         MVC   P+19(16),SORTREC                                                 
         GOTO1 REPORT                                                           
         MVC   REC-4(2),RBUYLEN    INSERT LENGTH FOR DISPLAY                    
         GOTO1 DISPPUT,DMCB,(RC),4 DISPLAY PRE-CHANGE                           
BPRO0220 EQU   *                                                                
         MVC   RBUYKREP,STNEWREP   INSERT NEW REP ID                            
         MVC   RBUYKCON,STNEWCON   INSERT NEW CONTRACT NUMBER                   
         CLI   QUESTOR+5,C'Y'      DISPLAY BUY MATCHES?                         
         BNE   BPRO0260            NO                                           
         CLC   MATCHBUY,=F'20'     DISPLAY FIRST N MATCHES                      
         BH    BPRO0260                                                         
         MVC   REC-4(2),RBUYLEN    INSERT LENGTH FOR DISPLAY                    
         GOTO1 DISPPUT,DMCB,(RC),0 DISPLAY POST-CHANGE                          
         B     BPRO0260                                                         
BPRO0240 EQU   *                                                                
         L     RF,MATCHBNG         INCREMENT UNMATCHED BUY COUNTER              
         LA    RF,1(RF)                                                         
         ST    RF,MATCHBNG                                                      
         CLI   QUESTOR+3,C'Y'      O/P MATCHED BUYS ONLY?                       
         BNE   BPRO0250            NO  - OUTPUT ALL BUYS                        
         L     RF,UNMATSKP         YES - INCR UNMATCHED BUYS SKIPPED            
         LA    RF,1(RF)                                                         
         ST    RF,UNMATSKP                                                      
         B     BPRO0300                                                         
BPRO0250 EQU   *                                                                
         GOTO1 GETRECRD            RETRIEVE UNMATCHED BUY RECORD                
BPRO0260 EQU   *                                                                
         MVC   REC-4(2),RBUYLEN    INSERT LENGTH FOR PUTREC ROUTINE             
         BAS   RE,PUTRECS                                                       
         B     BPRO0300            GO BACK FOR NEXT BUY RECORD                  
***>>>                                                                          
BPRO0300 EQU   *                                                                
         L     RF,BUYCTR2          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR2                                                       
         CLC   BUYCTR2,=F'10000'   DISPLAY EVERY N RECORDS                      
         BNE   BPRO0320                                                         
         XC    BUYCTR2,BUYCTR2                                                  
         MVC   P+1(21),=C'PROCESSING BUYS     :'                                
         EDIT  BUYCTR,(7,P+24)                                                  
         MVC   P+32(27),KEY                                                     
         MVC   P+64(16),SORTREC                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(06),=C'MATCH:'                                               
         EDIT  MATCHBUY,(7,P+7)                                                 
         MVC   P+20(06),=C'UNMAT:'                                              
         EDIT  MATCHBNG,(7,P+27)                                                
         MVC   P+40(06),=C'SKIPD:'                                              
         EDIT  UNMATSKP,(7,P+47)                                                
         GOTO1 REPORT                                                           
BPRO0320 EQU   *                                                                
         B     BPRO0020            GO BACK FOR NEXT                             
BPRO0800 EQU   *                                                                
         XIT1                                                                   
LASTSORT DS    CL16                                                             
         DROP  R2                                                               
         EJECT                                                                  
DNEWGRP  EQU   5                                                                
         EJECT                                                                  
**CONFIRM COMMENT                                                               
*                                                                               
*   TAKECFC:  LOOK FOR CONFIRM COMMENT RECORD.  IF PRESENT, MOVE                
*        WITH NEW CONTRACT NUMBER.                                              
*                                                                               
TAKECFC  NTR1                                                                   
***>>>                                                                          
         LA    R2,REC                                                           
         USING RCFCREC,R2          SET ADDRESSABILITY TO CFC RECORD             
*                                                                               
         XC    RCFCREC(32),RCFCREC                                              
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,OLDREP     INSERT SOURCE REP INTO KEY                   
         MVC   RCFCKCON,OLDCON     INSERT SOURCE CON # INTO KEY                 
         MVC   KEY,RCFCREC                                                      
         GOTO1 HIGHDIR                                                          
         CLC   KEY(L'RCFCKEY),KEYSAVE                                           
         BNE   TKCF0100            NOT FOUND - EXIT                             
TKCF0020 EQU   *                                                                
         GOTO1 GETRECRD                                                         
*                                                                               
         MVC   RCFCKREP,REPREP     INSERT TARGET REP INTO KEY                   
         MVC   RCFCKCON,NEWCON     INSERT NEW CONTRACT NUMBER                   
*                                                                               
         XC    REC-4(4),REC-4                                                   
         MVC   REC-4(2),RCFCLEN    INSERT RECORD LENGTH                         
         GOTO1 PUTRECS                                                          
         L     RF,CFCCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CFCCTR                                                        
*                                                                               
         CLC   CFCCTR,=F'25'                                                    
         BH    TKCF0100                                                         
*                                                                               
**       MVC   P+1(11),=C'CONFIRM   :'                                          
**       MVC   P+12(27),RCFCREC                                                 
**       GOTO1 REPORT                                                           
TKCF0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
*                                                                               
**CONFIRM COMMENT                                                               
         EJECT                                                                  
**COVERSHEET                                                                    
******************************************************************              
*  SETSALES: FIND OFFICE IN GROUP/SUBGROUP-OFFICE TABLE          *              
*        IF FOUND, REPLACE SALESPERSON, S/P TEAM                 *              
*        IN CONTRACT.                                            *              
*                                                                *              
******************************************************************              
SETSALES NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(CONTRACT RECORD IN PROCESS)          
*                                                                               
         USING RCONREC,R5                                                       
         L     R2,ASALAREA         SET A(SALESPERSON G/SG-OFFICE LIST)          
         MVC   DUB(2),RCONKGRP     SET UP SEARCH ARGUMENT                       
         MVC   DUB+2(2),RCONKOFF                                                
         L     R4,SALCTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,DUB,(R2),(R4),10,4,(R4)                         
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SSAL0020            YES                                          
         L     RF,NOSPCTR          COUNT MISSING S/P ORDERS                     
         LA    RF,1(RF)                                                         
         ST    RF,NOSPCTR                                                       
         B     SSAL0800            EXIT CC NOT ZERO                             
SSAL0020 EQU   *                                                                
         L     R2,DMCB             SET A(RECORD FOUND)                          
         MVC   RCONSAL,4(R2)       INSERT NEW SALESPERSON                       
*                                                                               
**       MVC   P+1(04),=C'S/P:'                                                 
**       MVC   P+05(02),RCONKGRP                                                
**       MVI   P+07,C'/'                                                        
**       MVC   P+08(02),RCONKOFF                                                
**       MVI   P+10,C'/'                                                        
**       MVC   P+11(02),4(R2)                                                   
**       GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0               SET CC ZERO                                  
         B     SSAL0820            EXIT                                         
SSAL0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
SSAL0820 EQU   *                                                                
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
SAVESAL  DS    CL3                                                              
SAVEKOFF DS    CL2                                                              
SAVETEM  DS    CL2                                                              
         EJECT                                                                  
*                                                                               
******************************************************************              
*  SETELTS:  ONLY CONVERT COMMENT ELEMENT IS BEING INSERTED.     *              
*        OTHER COMMENTS IMMEDIATELY BELOW DO NOT APPLY IN        *              
*        THIS MODULE.                                            *              
*                                                                *              
*                                                                *              
*  SETELTS:  ADD 1C DARE TAKEOVER ELEMENT AND 2A MOVE HISTORY    *              
*        ELEMENT.  THIS PERMITS TRACKING A DUPLICATE TAKEOVER    *              
*        LOCKOUT.                                                *              
*        TREAT COMMENTS TO INSERT TKO EFF COMMENT                *              
*                                                                *              
******************************************************************              
SETELTS  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(CONTRACT RECORD IN PROCESS)          
         L     R2,8(R1)            SET A(STATION SLOT)                          
         USING RCONREC,R5                                                       
*                                                                               
**       GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'2A',RCONREC),0,0            
*                                  DELETE OLD TKO/MOVE HIST ELT                 
*                                  INSERT NEW TKO/MOVE HIST ELT                 
**       XC    ELEM(RCONMMLQ),ELEM                                              
**       MVI   ELEM,X'2A'                                                       
**       MVI   ELEM+1,RCONMMLQ     NEW ELEMENT LENGTH                           
**       GOTO1 DATCON,DMCB,(5,WORK),(2,ELEM+2)                                  
*                                  INSERT DATE OF TAKEOVER                      
**       MVC   ELEM+4(4),OLDCON    INSERT ORIGINAL CONTRACT NUMBER              
**       MVC   ELEM+8(2),OLDREP    INSERT ORIGINAL SOURCE REP                   
**       GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELEM,0                 
*                                  INSERT NEW ELEMENT INTO RECORD               
*                                                                               
*   IF THERE WAS AN ORIGINAL 1C ELT, IT IS NOT DROPPED BY THE TAKEOVER          
*        PROGRAM, SO IT IS NOT BEING DROPPED HERE.  CONSISTENCY.                
*                                                                               
***>>>   GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'1C',RCONREC),0,0            
*                                  DELETE DARE TAKEOVER ELT                     
*                                  INSERT DARE TAKEOVER ELT                     
**       XC    ELEM(RCONTKLQ),ELEM                                              
**       MVI   ELEM,X'1C'                                                       
**       MVI   ELEM+1,RCONTKLQ     NEW ELEMENT LENGTH                           
**       MVC   ELEM+2(2),OLDREP    INSERT SOURCE REP                            
**       MVC   ELEM+4(4),OLDCON    INSERT ORIGINAL CONTRACT NUMBER              
**       GOTO1 DATCON,DMCB,(5,WORK),(2,ELEM+8)                                  
*                                  INSERT DATE OF TAKEOVER                      
**       THMS  DDSTIME=YES                                                      
**       ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
**       ST    R1,DUB+4            DDS TIME                                     
**       AP    DUB(4),DUB+4(4)                                                  
**       ICM   R1,15,DUB                                                        
**       SRL   R1,4                SHIFT OFF SIGN                               
**       STCM  R1,7,ELEM+10        INSERT TIME                                  
**       GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELEM,0                 
*                                  INSERT NEW ELEMENT INTO RECORD               
**TKO EFF                                                                       
         GOTO1 DATCON,DMCB,(5,WORK),(5,NWCOMDAT)                                
         LA    R7,COMPCONV         SET A(TABLE)                                 
SETE0020 EQU   *                                                                
         CLI   0(R7),0             END OF TABLE?                                
         BNE   SETE0040            NO                                           
         MVC   NWOLDREP,=C'UNRECOGNIZED REP   '                                 
         B     SETE0080                                                         
SETE0040 EQU   *                                                                
         CLC   13(2,R7),SOLDREP(R2)                                             
*                                  TABLE REP VS SENDING REP?                    
         BE    SETE0060            YES                                          
         LA    R7,LCOMPCNV(R7)                                                  
         B     SETE0040            GO BACK FOR NEXT                             
SETE0060 EQU   *                                                                
         MVC   NWOLDREP(13),0(R7)  INSERT COMPANY NAME FROM LIST                
SETE0080 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,OLDCON,NWOLDCON,4,=C'TOG'                            
*                                  INSERT ORIGINAL CONTRACT NUMBER              
         SR    R1,R1               CLEAR COUNTER                                
         MVI   NWCOMELT,2          SET TO 'CONTRACT COMMENT ELEMENT'            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'        FIND CONTRACT COMMENT ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   SETE0220            NO COMMENT ELEMENT:  INSERT                  
*                                     TKO COMT AS CONTRACT COMMENT              
         B     SETE0180            COMMENT FOUND: CHECK IT                      
SETE0160 EQU   *                                                                
         BAS   RE,NEXTEL           LOOK FOR NEXT COMMENT                        
         BNE   SETE0240            NO COMMENT ELEMENT                           
SETE0180 EQU   *                                                                
         LA    R1,1(R1)            INCREMENT COUNTER                            
         CLC   =C'C=',2(R6)        STANDARD COMMENT?                            
         BE    SETE0200            YES - REUSE IT                               
         CLC   =C'SC=',2(R6)       STORED COMMENT?                              
         BNE   SETE0160            NO  - GO CHECK NEXT COMMENT                  
SETE0200 EQU   *                                                                
         MVI   0(R6),X'FF'         SET CONTRACT COMMENT TO DELETE               
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0            
*                                  DELETE CONTRACT COMMENT                      
SETE0220 EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,NWCOMELT,0             
*                                  INSERT CONTRACT COMMENT ELEMENT              
         B     SETE0320            EXIT                                         
SETE0240 EQU   *                                                                
         STC   R1,BYTE             ONLY 1 COMMENT IN RECORD?                    
         CLI   BYTE,1                                                           
         BE    SETE0220            YES - INSERT TKO CMT AS 2ND COMMENT          
*                                  NO  - NO ROOM: CHECK ORDER COMMENT           
         SR    R1,R1               CLEAR COUNTER                                
         MVI   NWCOMELT,X'82'      SET TO 'ORDER COMMENT ELEMENT'               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'82'        FIND ORDER COMMENT ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   SETE0220            NO COMMENT ELEMENT:  INSERT                  
         B     SETE0280                                                         
*                                     TKO COMT AS ORDER COMMENT                 
SETE0260 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   SETE0300            NOT FOUND                                    
SETE0280 EQU   *                                                                
         ST    R6,FULL             SAVE A(LAST R6 FOUND)                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         B     SETE0260            GO BACK FOR NEXT                             
SETE0300 EQU   *                                                                
         STC   R1,BYTE             CHECK COUNTER                                
         CLI   BYTE,10             ALL ORDER COMMENTS IN USE?                   
         BNE   SETE0220            NO  - ADD COMMENT AS NEXT ONE                
         L     R6,FULL             YES - RESET A(LAST COMMENT)                  
         B     SETE0200            DELETE LAST COMMENT, INSERT                  
*                                     TAKEOVER COMMENT AS LAST                  
SETE0320 EQU   *                                                                
         XIT1                                                                   
*                                                                               
NWCOMELT DC    X'003C'     (2)     NEW COMMENT ELEMENT W/TAKEOVER               
         DC    X'FD'       (1)     FORCE TO SORT LAST                           
         DC    C'CONV'     (4)                                                  
         DC    C'ERT '     (4)                                                  
NWCOMDAT DS    CL8         (8)                                                  
         DC    C' OLD REP/CON '  (13)                                           
NWOLDREP DS    CL19              (19)                                           
         DC    C'#'              (1)                                            
NWOLDCON DS    CL8               (8)                                            
LNWCOMEL EQU   *-NWCOMELT                                                       
**TKO EFF                                                                       
COMPCONV DC    C'CHRISTAL     ',C'CR'                                           
LCOMPCNV EQU   *-COMPCONV                                                       
         DC    C'EASTMAN      ',C'EA'                                           
         DC    C'KATZ RADIO   ',C'KU'                                           
         DC    C'KATZ HISPANIC',C'KF'                                           
         DC    C'SENTRY       ',C'S3'                                           
         DC    C'CLR CHNL A   ',C'NU'                                           
         DC    C'CLR CHNL B   ',C'NU'                                           
         DC    C'CLR CHNL C   ',C'NU'                                           
         DC    X'0000'                                                          
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
******************************************************************              
*  SASSIST:  CLEAR THE SALES ASSISTANT ENTERED IN THE ORDER      *              
*                                                                *              
*                                                                *              
******************************************************************              
SASSIST  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(CONTRACT RECORD IN PROCESS)          
         USING RCONREC,R5                                                       
         LA    RF,RCONELEM                                                      
SASS0020 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    SASS0060            YES - NO X'9F' ELT                           
         CLI   0(RF),X'9F'         EXTRA DESCRIPTOR ELT?                        
         BE    SASS0040            YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     SASS0020            GO BACK FOR NEXT                             
SASS0040 EQU   *                                                                
         XC    RCONXAST-RCONXXEL(9,RF),RCONXAST-RCONXXEL(RF)                    
*                                  RESET SALES ASSISTANT                        
SASS0060 EQU   *                                                                
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
**-->                                                                           
***********************************************************************         
* PRODCODE: CHECK IF PRODUCT CODE HAS BEEN ENTERED.  IF SO,           *         
*        PRODUCT CODE FIELD MUST BE SET TO ZERO, AND A PRODUCT        *         
*        EXPANSION ELEMENT INSERTED INTO THE CONTRACT RECORD.         *         
* P1 HAS ADV CODE                                                     *         
* P2 HAS PRD CODE                                                     *         
* WORK WILL CONTAIN EXPANDED NAME                                     *         
***********************************************************************         
PRODCODE NTR1                                                                   
*        L     RC,0(R1)            RESET A(WORKSPACE)                           
*        XC    WORK,WORK                                                        
*        L     R2,4(R1)            SET A(RCONREC)                               
*        USING RCONREC,R2                                                       
*                                                                               
*        MVC   SAVEKEY,KEY         SAVE KEY FOR RESTART                         
*                                                                               
*        XC    KEY,KEY                                                          
*        LA    R6,KEY                                                           
*        MVI   KEY+RPRDKTYP-RPRDREC,X'09'                                       
*        MVC   KEY+RPRDKADV-RPRDREC(4),OLDADV     INSERT ORIG ADV CODE          
*        MVC   KEY+RPRDKPRD-RPRDREC(3),RCONPRD    INSERT PRODUCT CODE           
*        MVC   KEY+RPRDKREP-RPRDREC(2),OLDREP     INSERT ORIG REP CODE          
*                                                                               
*   TEST                                                                        
**       MVC   P+1(05),=C'PROD:'                                                
**       MVC   P+6(27),KEY                                                      
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*        GOTO1 HIGHDIR                                                          
*        CLC   KEY(L'RPRDKEY),KEYSAVE                                           
*        BE    *+6                                                              
*        DC    H'0'                MUST BE THERE                                
*        GOTO1 GETREC#2                                                         
*                                                                               
*        L     R6,AIO2             SET A(IOAREA # 2)                            
*        USING RPRDREC,R6                                                       
*                                                                               
*                                                                               
*   TEST PRNTBL                                                                 
*        MVC   P+1(09),=C'PRODREC:'                                             
*        ST    R6,P+12                                                          
*        GOTO1 REPORT                                                           
*        LR    R4,R6               A(PRODUCT RECORD)                            
*        LA    RF,256                                                           
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
*        MVC   WORK(2),=X'0516'    SET ELEMENT CODE + LENGTH                    
*        MVC   WORK+2(L'RPRDNAME),RPRDNAME                                      
*        DROP  R6                                                               
*                                                                               
*   TEST                                                                        
**       MVC   P+1(09),=C'PRODWORK:'                                            
**       MVC   P+10(22),WORK                                                    
**       GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*        GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,WORK,0                 
*                                  INSERT PRODUCT EXPANSION                     
*        MVC   RCONPRD,SPACES      CLEAR PRODUCT CODE                           
*                                                                               
*   TEST PRNTBL                                                                 
**       GOTO1 REPORT                                                           
**       LA    R4,RCONREC          A(CONTRACT RECORD)                           
**       SR    RF,RF                                                            
**       ICM   RF,3,RCONLEN                                                     
**       GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
*        MVC   KEY,SAVEKEY         RESTORE CONTRACT KEY                         
*        GOTO1 HIGHDIR             RESTART CONTRACT KEY SEQUENCE                
*        XIT1                                                                   
*        DROP  R2                                                               
**-->                                                                           
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*        4(R1):  IF PRE-PUT, WILL BE '4'                         *              
*                IF POST   , WILL BE '0'                         *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            LOAD PRE/POST VALUE                          
         GOTO1 REPORT                                                           
         MVC   P+1(18),=C'PRE -PUTREC RECORD'                                   
         CLI   REC,X'0C'           CONTRACT?                                    
         BNE   DIPU0010            NO                                           
         EDIT  CHGCON,(6,P+30)                                                  
         EDIT  NOSPCTR,(6,P+40)                                                 
         B     DIPU0015                                                         
DIPU0010 EQU   *                                                                
         EDIT  MATCHBUY,(6,P+30)                                                
DIPU0015 EQU   *                                                                
         LTR   R2,R2               ANY VALUE?                                   
         BNZ   DIPU0020            YES - IT'S PRE                               
         MVC   P+1(18),=C'POST-PUTREC RECORD'                                   
DIPU0020 EQU   *                                                                
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         AR    RF,R2               ADD PRE/POST PUT LENGTH                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
PUTRECS  NTR1                                                                   
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 2                                                                
SEQDIR   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETREC#2 LA    R6,GETREC                                                        
         B     LINKFIL2                                                         
         SPACE 2                                                                
LINKFIL2 NTR1                                                                   
         MVC   DMCB+12(4),AIO2                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               ,(0,DMWORK)                                                      
         B     DMCHECK                                                          
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCHECK                                                          
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        CLC   =X'05721739',KEY+12                                              
*        BNE   TEST0030                                                         
*        MVC   P+1(09),=C'CONTRACT:'                                            
*        MVC   P+12(4),DMCB+8                                                   
*        MVC   P+20(34),KEY                                                     
*        GOTO1 REPORT                                                           
*EST0030 EQU   *                                                                
*                                                                               
         TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'02'        TEST FOR RECORD DELETED                      
         BO    EQXIT               DELETE SET - PROCESS                         
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         XIT1                                                                   
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
         SPACE 1                                                                
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ASPOFFTB DS    A                                                                
ANEXTSPO DS    A                                                                
AMISSSTA DS    A                                                                
AMISSADV DS    A                                                                
SAVEAIO  DS    A                                                                
AIO2     DS    A                                                                
ABLDAREA DS    A                                                                
ASTAAREA DS    A                   STATION AREA                                 
ANEXTSTA DS    A                   NEXT STATION SLOT                            
AJOLAREA DS    A                   STATION JOIN/LEAVE AREA                      
ANEXTJOL DS    A                   NEXT JOIN/LEAVE SLOT                         
JOLCTR   DS    F                                                                
ASALAREA DS    A                                                                
ANEXTSAL DS    A                                                                
SALCTR   DS    F                                                                
ATAPEREC DS    A                                                                
AWORKBLK DS    A                                                                
AWRKBLK2 DS    A                                                                
LBLDAREA DS    F                                                                
CONBYTES DS    F                                                                
BUYBYTES DS    F                                                                
CONCTR   DS    F                                                                
CONCTR2  DS    F                                                                
CONCTR3  DS    F                                                                
CONCTR4  DS    F                                                                
CFCCTR   DS    F                                                                
COVCTR   DS    F                                                                
COVCTR1  DS    F                                                                
NOCONCOV DS    F                                                                
COVRBACK DS    F                                                                
NOSPCTR  DS    F                                                                
NOCHGCON DS    F                                                                
CHGCON   DS    F                                                                
NOCHGSTA DS    F                                                                
CHGSTA   DS    F                                                                
CONDEL   DS    F                                                                
BUYCTR   DS    F                                                                
BUYCTR2  DS    F                                                                
MATCHBUY DS    F                                                                
MATCHBNG DS    F                                                                
UNMATSKP DS    F                                                                
OWNCTR   DS    F                                                                
STACTR   DS    F                                                                
STAREAD  DS    F                                                                
STATOTAL DS    F                                                                
STASKIPD DS    F                                                                
STALVDAT DS    F                                                                
STACOPYD DS    F                                                                
STAOUTPT DS    F                                                                
STAOLDNW DS    F                                                                
STAONFIL DS    F                                                                
STADUPED DS    F                                                                
STAPASS1 DS    F                                                                
STAPASS2 DS    F                                                                
STAGHOST DS    F                                                                
MISMATCH DS    F                                                                
SORTCTR  DS    F                                                                
SORTTYP1 DS    F                                                                
SORTTYP2 DS    F                                                                
STYP1RED DS    F                                                                
STYP2RED DS    F                                                                
MATCHSRT DS    F                                                                
NOBUYCON DS    F                                                                
DROP02C  DS    F                                                                
DROP02SC DS    F                                                                
DROP07C  DS    F                                                                
DROP07SC DS    F                                                                
DROP11C  DS    F                                                                
DROP11SC DS    F                                                                
DROP82C  DS    F                                                                
DROP82SC DS    F                                                                
COMBOCTL DS    F                                                                
         XIT1    .1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6                               
FOXES    DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                              
DUMMYREP DS    CL14                DUMMY 'STATION TABLE' ENTRY                  
*                                     WHEN STATION NOT IN LIST                  
COMPARE  DS    CL6                 COMPARE AREA: SORT VS BUY                    
*                                                                               
MYWORK   DS    CL64                                                             
MYWORK2  DS    CL64                                                             
OLDCON   DS    CL4                 ORIGINAL CONTRACT NUMBER                     
NEWCON   DS    CL4                 NEW      CONTRACT NUMBER                     
NEWCONRV DS    CL4                 NEW      CONTRACT NUMBER (REV)               
OLDCONRV DS    CL4                 OLD      CONTRACT NUMBER (REV)               
OLDSTA   DS    CL5                                                              
OLDOFF   DS    CL2                                                              
OLDADV   DS    CL4                                                              
OLDAGY   DS    CL6                 ORIGINAL AGENCY/OFFICE                       
ELEM     DS    CL64                                                             
*                                                                               
LASTSTA  DS    CL5                                                              
OLDREP   DS    CL2                 OLD REP CODE                                 
NEWREP   DS    CL2                 NEW REP CODE                                 
REPREP   DS    CL2                 REP REP CODE (REPLACEMENT)                   
OLDCONV# DS    CL2                 OLD REPCODE                                  
HIGHCON  DS    XL4                                                              
LOWCON   DS    XL4                                                              
SAVEKEY  DS    CL(L'KEY)                                                        
COVERFLG DS    CL1                                                              
*   EQUATES                                                                     
SALTABLN EQU   13                                                               
DSALTSTN EQU   0                   DISPLACE TO STATION                          
DSALTOSP EQU   5                   DISPLACE TO OLD S/P                          
DSALTNSP EQU   8                   DISPLACE TO NEW S/P                          
DSALTTEM EQU   11                  DISPLACE TO TEAM                             
*                                                                               
STATABLN EQU   16                                                               
JOLTABLN EQU   20                                                               
*                                                                               
SORTREC  DS    0CL16               SORT RECORD                                  
STYP     DS    CL1                 LABEL FOR TESTING EOF                        
STOLDREP DS    CL2                                                              
STOLDCON DS    CL4                                                              
STNEWREP DS    CL2                                                              
STNEWCON DS    CL4                                                              
         DS    CL3                 SPARE                                        
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,07,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=16'                                    
*                                                                               
SAVESORT DS    CL16                SAVED SORT RECORD                            
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
         DS    0H                                                               
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE1  DCB   DDNAME=INTAPE1,DSORG=PS,RECFM=VB,LRECL=255,             X        
               BLKSIZE=6233,MACRF=GM,EODAD=LDIR0400                             
INTAPE2  DCB   DDNAME=INTAPE2,DSORG=PS,RECFM=VB,LRECL=255,             X        
               BLKSIZE=6233,MACRF=GM,EODAD=LSAP0180                             
         SPACE 3                                                                
         SPACE 3                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL4008              AREA FOR RECORD: 4K CONTRACT                 
         DS    0D                                                               
*  INCLUDE REGENREPA               REP RECORD                                   
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENMKG                MAKEGOOD RECORD                              
*  INCLUDE REGENTKO                TKO EQUIV RECORD                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
*                                                                               
NMODAREA CSECT                                                                  
RECD     DSECT                                                                  
RECORD   DS    CL4008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENREPA         REP RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSAL          S/P RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDAR          DARE RECORD                                  
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCOV          COVERSHEET RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCFC          CONFIRM COMMENT RECORD                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENMKG          M/G     RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTKO          TKO EQUIV RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY          AGENCY  RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY2         AGENCY2 RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          ADVERT  RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENREG          REGION  RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOFF          OFFICE  RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTEM          TEAM    RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCTG          CATEGRY RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUD          BUDGET  RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENINV          INVENTORY RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAVLN         AVAIL     RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPRPN         PROPOSAL  RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEOM          EOM       RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCLS          CLASS   RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENGRP          GROUP   RECORD                               
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
       EJECT                                                                    
         CSECT                                                                  
******************************************************************              
* DISPLAY TOTALS                                                                
******************************************************************              
DISPTOTS NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(30),=C'STATIONS      PROCESSED      :'                       
         EDIT  STAREAD,(12,P+36),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS FROM KATZ TABLE     :'                       
         EDIT  STATOTAL,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS NOT BEING CHANGED   :'                       
         EDIT  STASKIPD,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS W/LEAVE DATES       :'                       
         EDIT  STALVDAT,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS OUTPUT (TOTAL)      :'                       
         EDIT  STAOUTPT,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS COPIED TO NEW REP   :'                       
         EDIT  STACOPYD,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS COPIED ON 1ST PASS  :'                       
         EDIT  STAPASS1,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS COPIED ON 2ND PASS  :'                       
         EDIT  STAPASS2,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS DUPED: SAME NEW REP :'                       
         EDIT  STADUPED,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS ALREADY ON FILE     :'                       
         EDIT  STAONFIL,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS WHERE OLD=NEW       :'                       
         EDIT  STAOLDNW,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS THAT DON"T EXIST    :'                       
         EDIT  STAGHOST,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS ALREADY ON FILE WITH:'                       
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'***MISMATCHED GROUP/SUBGROUPS:'                       
         EDIT  MISMATCH,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'CONTRACTS READ               :'                       
         EDIT  CONCTR,(12,P+36),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'CONTRACTS MOVED              :'                       
         EDIT  CHGCON,(12,P+36),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'CONTRACTS OUTPUT AS IS       :'                       
         EDIT  NOCHGCON,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'CONTRACTS MOVED/NO S/P CHANGE:'                       
         EDIT  NOSPCTR,(12,P+36),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'BUYS          PROCESSED      :'                       
         EDIT  BUYCTR,(12,P+36),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'BUYS MATCHED                 :'                       
         EDIT  MATCHBUY,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'BUYS UNMATCHED               :'                       
         EDIT  MATCHBNG,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'CONFIRM COMTS PROCESSED      :'                       
         EDIT  CFCCTR,(12,P+36),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COVER HEADERS PROCESSED      :'                       
         EDIT  COVCTR,(12,P+36),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COVER SUBRECS PROCESSED      :'                       
         EDIT  COVCTR1,(12,P+36),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COVER SORTS RETURNED         :'                       
         EDIT  COVRBACK,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COVERS W/NO CONTRACTS        :'                       
         EDIT  NOCONCOV,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COMBO CONTROL ELTS DROPPED   :'                       
         EDIT  COMBOCTL,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COMMENT 02 C=  ELTS DROPPED  :'                       
         EDIT  DROP02C,(12,P+36),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COMMENT 02 SC= ELTS DROPPED  :'                       
         EDIT  DROP02SC,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COMMENT 07 C=  ELTS DROPPED  :'                       
         EDIT  DROP07C,(12,P+36),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COMMENT 07 SC= ELTS DROPPED  :'                       
         EDIT  DROP07SC,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COMMENT 11 C=  ELTS DROPPED  :'                       
         EDIT  DROP11C,(12,P+36),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COMMENT 11 SC= ELTS DROPPED  :'                       
         EDIT  DROP11SC,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COMMENT 82 C=  ELTS DROPPED  :'                       
         EDIT  DROP82C,(12,P+36),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'COMMENT 82 SC= ELTS DROPPED  :'                       
         EDIT  DROP82SC,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'SORT RECS WRITTEN:  TOTAL    :'                       
         EDIT  SORTCTR,(12,P+36),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'SORT RECS WRITTEN:  BUYS (T1):'                       
         EDIT  SORTTYP1,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'SORT RECS WRITTEN:  COVS (T2):'                       
         EDIT  SORTTYP2,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'SORT RECS READ   :  BUYS (T1):'                       
         EDIT  STYP1RED,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'SORT RECS READ   :  COVS (T2):'                       
         EDIT  STYP2RED,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'SORT RECS MATCHED BY AT LEAST '                       
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C' ONE BUY RECORD (TYPE 1 SORT):'                       
         EDIT  MATCHSRT,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'CONTRACTS WITH NO BUY RECS   :'                       
         EDIT  NOBUYCON,(12,P+36),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'OWNER RECORDS FOUND          :'                       
         EDIT  OWNCTR,(12,P+36),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'LOW  CONTRACT NUMBER         :'                       
         GOTO1 HEXOUT,DMCB,LOWCON,P+36,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'HIGH CONTRACT NUMBER         :'                       
         GOTO1 HEXOUT,DMCB,HIGHCON,P+36,4,=C'TOG'                               
         GOTO1 REPORT                                                           
         L     RF,CONBYTES         CALCULATE AVERAGE RECORD SIZE                
         SR    RE,RE                                                            
         LTR   RF,RF               ANY DIVIDEND?                                
         BZ    NODIV001            NO                                           
         D     RE,CHGCON           NUM BYTES / # CONTRACTS                      
         MVC   P+1(30),=C'AVERAGE CONTRACT RECORD      :'                       
         EDIT  (RF),(12,P+36),COMMAS=YES                                        
         GOTO1 REPORT                                                           
NODIV001 EQU   *                                                                
         L     RF,BUYBYTES         CALCULATE AVERAGE RECORD SIZE                
         SR    RE,RE                                                            
         LTR   RF,RF               ANY DIVIDEND?                                
         BZ    NODIV002            NO                                           
         D     RE,BUYCTR           NUM BYTES / # BUY RECORDS                    
         MVC   P+1(30),=C'AVERAGE BUY      RECORD      :'                       
         EDIT  (RF),(12,P+36),COMMAS=YES                                        
         GOTO1 REPORT                                                           
NODIV002 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*  DISPPUT2: DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*        4(R1):  SPECIAL DISPLAY:  1 = NO-CHANGE CONTRACT        *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT2 NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   DIVALUE,7(R1)       LOAD DISPLAY VALUE                           
         GOTO1 REPORT                                                           
         MVC   P+1(18),=C'NO CHANGE RECORD: '                                   
         CLI   DIVALUE,1           NO CHANGE CONTRACT?                          
         BNE   DIPT0090            NO                                           
         EDIT  NOCHGCON,(6,P+30)                                                
DIPT0020 EQU   *                                                                
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPT0090 EQU   *                                                                
         XIT1                                                                   
DIVALUE  DS    X                                                                
         DS    H'0'                                                             
         EJECT                                                                  
*  GENERATE SORT RECORDS                                                        
*                                                                               
SORTGEN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         L     RF,SORTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SORTCTR                                                       
         CLI   SORTREC,1           SORT TYPE 1 RECORD?                          
         BNE   SGEN0010            NO                                           
         L     RF,SORTTYP1         TYPE 1 COUNTER                               
         LA    RF,1(RF)                                                         
         ST    RF,SORTTYP1                                                      
         B     SGEN0012                                                         
SGEN0010 EQU   *                                                                
         CLI   SORTREC,2           SORT TYPE 2 RECORD?                          
         BNE   SGEN0012            NO                                           
         L     RF,SORTTYP2         TYPE 2 COUNTER                               
         LA    RF,1(RF)                                                         
         ST    RF,SORTTYP2                                                      
SGEN0012 EQU   *                                                                
         CLC   SORTCTR,=F'10'      DISPLAY FIRST N RECORDS                      
         BH    SGEN0020                                                         
         EDIT  SORTCTR,(4,P+1)                                                  
         MVC   P+6(07),=C'SORTGEN' **TEST**                                     
         MVC   P+18(16),SORTREC    **TEST**                                     
         GOTO1 REPORT              **TEST**                                     
SGEN0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*   SORT RETURN AND END-OF-FILE TESTING                                         
*                                                                               
GETSORT  NTR1  BASE=*,LABEL=*                                                   
         CLI   STYP,X'FF'          EOF REACHED?                                 
         BE    GSOR0080            YES                                          
         MVC   SORTREC,FOXES       SET SORTKEY TO X'FF'S                        
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    GSOR0080            TEST RETURN ZERO=EOF                         
         MVC   SORTREC,0(R6)                                                    
*        MVC   P+5(07),=C'GETSORT' **TEST**                                     
*        MVC   P+15(23),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
         CLI   STYP,1              SORT TYPE 1 READ?                            
         BNE   GSOR0020            NO                                           
         L     RF,STYP1RED         INCREMENT TYPE1 SORTS READ                   
         LA    RF,1(RF)                                                         
         ST    RF,STYP1RED                                                      
         B     GSOR0080                                                         
GSOR0020 EQU   *                                                                
         CLI   STYP,2              SORT TYPE 2 READ?                            
         BNE   GSOR0080            NO                                           
         L     RF,STYP2RED         INCREMENT TYPE2 SORTS READ                   
         LA    RF,1(RF)                                                         
         ST    RF,STYP2RED                                                      
         B     GSOR0080                                                         
*                                                                               
GSOR0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*   INITIALIZATIONS ....                                                        
*                                                                               
*   NEED TO LOAD UP A TABLE OF ALL EXISTING STATIONS/REPS ON FILE               
*        TO DETERMINE IF A STATION RECORD MUST BE GENERATED.                    
*                                                                               
*                                                                               
*                                                                               
******************************************************************              
INITIAL  NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   TURN OFF SORT UNTIL SORT TESTING IS BEING DONE.                             
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                  FIRST INITIALIZATION FOR SORT                
         XC    SORTREC,SORTREC                                                  
*                                                                               
         XC    HIGHCON,HIGHCON                                                  
         MVC   LOWCON,=X'FFFFFFFF'                                              
*                                                                               
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',1800000,1800000                                 
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         MVC   P+1(30),=C'CANNOT GET SPACE: COVAIL ABORT'                       
         GOTO1 REPORT                                                           
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVI   RCSUBPRG,2          SET HEADING FLAG                             
         MVC   ABLDAREA,P2         ADDITIONAL WORKSPACE                         
         MVC   ASTAAREA,P2         A(STATION 1 TABLE)                           
         MVC   ANEXTSTA,P2         A(NEXT STATION SLOT)                         
         L     RF,ABLDAREA         ESTABLISH WORKSPACE SETUP                    
         A     RF,=F'160000'       160K FOR STATION 1 STORAGE                   
*                                    10,000 ENTRIES -                           
*                                  +0    2 BYTES OLD REP                        
*                                  +2    5 BYTES STATION CALL LTRS              
*                                  +7    2 BYTES NEW REP                        
*                                  +9    2 BYTES OLD GROUP/SUBGROUP             
*                                  +11   2 BYTES NEW GROUP/SUBGROUP             
*                                  +13   3 BYTES SPARE                          
         ST    RF,ASALAREA         ESTABLISH SALESPERSON AREA                   
         ST    RF,ANEXTSAL         A(NEXT S/P SLOT)                             
         A     RF,=F'20000'        20K FOR SALESPERSON STORAGE:                 
*                                     2,000 ENTRIES -                           
*                                        2 BYTES NEW G/SG (NEW COMP)            
*                                        2 BYTES OFFICE                         
*                                        3 BYTES NEW S/P                        
*                                        3 BYTE  SPARE                          
         ST    RF,AIO2             A(IOAREA #2)                                 
*                                                                               
         A     RF,=F'4000'         4K FOR ALTERNATE IO AREA                     
         ST    RF,ATAPEREC         A(TAPE RECORD READ AREA)                     
*                                                                               
         A     RF,=F'4000'         4K FOR TAPE READ AREA                        
         ST    RF,AWORKBLK         A(WORK BLOCK FOR SCANNER)                    
*                                                                               
         A     RF,=F'6000'         6K FOR WORK BLOCK AREA                       
         ST    RF,AJOLAREA         A(JOIN/LEAVE STATION TABLE)                  
         ST    RF,ANEXTJOL         A(NEXT SLOT JOIN/LEAVE)                      
         A     RF,=F'200000'       200K FOR STATION 1 STORAGE                   
*                                    10,000 ENTRIES -                           
*                                  +0    2 BYTES NEW REP                        
*                                  +2    5 BYTES STATION CALL LTRS              
*                                  +7    2 BYTES OLD REP                        
*                                  +9    3 BYTES JOIN DATE                      
*                                  +12   3 BYTES LEAVE DATE                     
*                                  +15   1 BYTE  FLAGS                          
*                                  +16   2 BYTES NEW GROUP                      
*                                  +18   2 BYTES CLOSE DATE                     
*                                                                               
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
                                                                                
         XC    CONCTR,CONCTR       CLEAR COUNTERS                               
         XC    CONCTR2,CONCTR2                                                  
         XC    CONCTR3,CONCTR3                                                  
         XC    CONCTR4,CONCTR4                                                  
         XC    BUYCTR,BUYCTR                                                    
         XC    STACTR,STACTR                                                    
         XC    SALCTR,SALCTR                                                    
         XC    CONBYTES,CONBYTES                                                
         XC    BUYBYTES,BUYBYTES                                                
         MVC   OLDREP,QRECORD+36   'FROM' REP                                   
         MVC   NEWREP,QRECORD+38   'TO' REP                                     
         MVC   REPREP,QRECORD+40   'REPLACEMENT' REP                            
INIT0120 EQU   *                                                                
         XIT1                                                                   
INIT0200 EQU   *                                                                
         DC    H'0'                NO REFERENCE FOR THIS REP                    
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*   LOAD TABLE VALUES                                                           
******************************************************************              
*                                                                               
*   SCANNER DISPLACEMENT EQUATES: TO BEGINNING OF SCANNER WORK FIELD            
*                                                                               
STATION  EQU   0                                                                
OLDGROUP EQU   52                                                               
OLDREPSC EQU   104                                                              
NEWGROUP EQU   156                                                              
NEWREPSC EQU   208                                                              
*                                                                               
*   STATION SLOT DISPLACEMENT EQUATES: SLOTS IN TABLE ASTAAREA                  
*                                                                               
SOLDREP  EQU   0                                                                
SSTATION EQU   2                                                                
SNEWREP  EQU   7                                                                
SOLDGRP  EQU   9                                                                
SNEWGRP  EQU   11                                                               
SFLAGBYT EQU   15                  FLAGS                                        
*                                  X'80' - STA COPIED SOMEWHERE ELSE            
*                                                                               
*   JOIN/LEAVE SLOT DISPLACEMENT EQUATES: SLOTS IN TABLE AJOLAREA               
*                                                                               
JNEWREP  EQU   0                                                                
JSTATION EQU   2                                                                
JOLDREP  EQU   7                                                                
JJOINDAT EQU   9                                                                
JLEAVDAT EQU   12                                                               
JFLAGBYT EQU   15                  FLAGS                                        
*                                  X'80' - RECORD GENERATED TO O/P              
*                                  X'20' - STATION ALREADY ON FILE              
*                                  X'10' - ALREADY #ED AS 'ON FILE'             
*                                  X'08' - STATION DOESN'T EXIST                
JNEWGRP  EQU   16                                                               
JCLOSDTE EQU   18                  CLOSE DATE                                   
*                                                                               
LOADTABL NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    RF,REC              SET A(RECORD)                                
         ST    RF,AREC                                                          
*                                                                               
         L     R5,ATAPEREC         SET A(TAPE INPUT AREA)                       
LDIR0020 EQU   *                                                                
         L     R7,ANEXTSTA         SET A(NEXT STATION SLOT)                     
         XC    0(250,R5),0(R5)     CLEAR INPUT AREA                             
         GET   INTAPE1,(R5)        READ AGY/ADV TAPE RECORD INTO RDA            
*                                                                               
         L     RF,STATOTAL                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STATOTAL                                                      
*                                     TAPE WILL BE PARSED ON ',' SEPS           
*        GOTO1 REPORT                                                           
*        MVC   P+1(13),=C'INPUT RECORD:'                                        
*        MVC   P+20(80),0(R5)                                                   
*        GOTO1 REPORT                                                           
*                                                                               
         XC    MYWORK,MYWORK       CLEAR WORK SPACE                             
         ZICM  RF,0(R5),2          GET LENGTH OF INPUT RECORD                   
         BCTR  RF,0                SUBTRACT 1 FOR MOVE                          
         EX    RF,LDIRMOV1         MOVE BY LENGTH                               
         B     LDIR0040                                                         
LDIRMOV1 MVC   MYWORK+8(0),4(R5)                                                
LDIR0040 EQU   *                                                                
         ZICM  RF,0(R5),2          LOAD LENGTH OF RECORD                        
         SH    RF,=H'4'            SUBTRACT CONTROL LENGTH                      
         STC   RF,MYWORK+5                                                      
*        MVC   P+1(15),=C'MYWORK  RECORD:'                                      
*        MVC   P+20(80),MYWORK                                                  
*        GOTO1 REPORT                                                           
*                                                                               
LDIR0060 EQU   *                                                                
         BAS   RE,SCANQUOT                                                      
*        MVC   P+1(13),=C'QUOTE RECORD:'                                        
*        MVC   P+14(80),MYWORK                                                  
*        GOTO1 REPORT                                                           
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         GOTO1 =V(SCANNER),DMCB,(30,MYWORK),AWORKBLK,C',=,='                    
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   LDIR0080                                                         
         DC    H'0'                                                             
*                                                                               
LDIR0080 EQU   *                                                                
*                                                                               
*   TEST PRNTBL                                                                 
*        MVC   P+1(12),=C'SCANNER OKAY'                                         
*        GOTO1 REPORT                                                           
*        L     R4,AWORKBLK         A(SCANNER OUTPUT)                            
*        LA    RF,256                                                           
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         LA    RF,256(RF)                                                       
         ST    RF,AWRKBLK2                                                      
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         L     RE,AWORKBLK         SET A(WORKSPACE)                             
         XC    MYWORK2,MYWORK2                                                  
         MVC   MYWORK2+8(12),STATION+12(RE)   SET STATION                       
         MVC   MYWORK2+5(01),STATION(RE)      SET LENGTH                        
*                                  SPLIT STATION/MEDIA INPUT                    
         GOTO1 =V(SCANNER),DMCB,MYWORK2,AWRKBLK2,C',=-='                        
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AWORKBLK         SET A(WORKSPACE)                             
         CLC   =C'N/A',OLDREPSC+12(R2)                                          
*                                  NO-REP STATION?                              
         BNE   LDIR0109            NO                                           
**       MVC   P+1(24),=C'NON-REP STATION  FOUND: '                             
**       MVC   P+30(12),OLDREPSC+12(R2)                                         
**       MVC   P+45(16),0(R5)                                                   
**       GOTO1 REPORT                                                           
         B     LDIR0020            GO BACK FOR NEXT                             
LDIR0109 EQU   *                                                                
         LA    RF,COCONVRT         SET A(COMPANY NAME CONVERSION TABLE)         
LDIR0100 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    LDIR0110            YES - NOT FOUND: SHOULDN'T HAPPEN            
*                                     PUT OUT MESSAGE                           
         CLC   OLDREPSC+12(12,R2),0(RF)                                         
*                                  SCANNER NAME IN TABLE?                       
         BE    LDIR0120            YES                                          
         LA    RF,LCOCONV(RF)      NO  - BUMP TO NEXT SLOT                      
         B     LDIR0100            GO BACK FOR NEXT                             
LDIR0110 EQU   *                                                                
         MVC   P+1(24),=C'OLD COMPANY  NOT FOUND: '                             
         MVC   P+30(12),OLDREPSC+12(R2)                                         
         MVC   P+45(16),0(R5)                                                   
         GOTO1 REPORT                                                           
         B     LDIR0020            GO BACK FOR NEXT                             
LDIR0120 EQU   *                                                                
         MVC   SOLDREP(2,R7),12(RF)                                             
*                                  INSERT OLD REP INTO TABLE                    
*   STATION HAS BEEN SPLIT IN ALTERNATE WORK AREA                               
*                                                                               
*                                                                               
*   TEST PRNTBL                                                                 
*        MVC   P+1(13),=C'STATION SPLIT'                                        
*        GOTO1 REPORT                                                           
*        L     R4,AWRKBLK2         A(STATION SPLIT)                             
*        LA    RF,64                                                            
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
         L     R4,AWRKBLK2                                                      
         MVC   SSTATION(4,R7),12(R4)                                            
*                                  INSERT STATION INTO TABLE                    
         MVC   SSTATION+4(1,R7),44(R4)                                          
*                                  INSERT MEDIA INTO TABLE                      
         LA    RF,COCONVRT         SET A(COMPANY NAME CONVERSION TABLE)         
LDIR0140 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    LDIR0160            YES - NOT FOUND: SHOULDN'T HAPPEN            
*                                     PUT OUT MESSAGE                           
         CLC   NEWREPSC+12(12,R2),0(RF)                                         
*                                  SCANNER NAME IN TABLE?                       
         BE    LDIR0180            YES                                          
         LA    RF,LCOCONV(RF)      NO  - BUMP TO NEXT SLOT                      
         B     LDIR0140            GO BACK FOR NEXT                             
LDIR0160 EQU   *                                                                
         MVC   P+1(24),=C'NEW COMPANY  NOT FOUND: '                             
         MVC   P+30(12),NEWREPSC+12(R2)                                         
         MVC   P+45(16),0(R5)                                                   
         GOTO1 REPORT                                                           
         B     LDIR0020            GO BACK FOR NEXT                             
LDIR0180 EQU   *                                                                
         MVC   SNEWREP(2,R7),12(RF)                                             
*                                  INSERT NEW REP INTO TABLE                    
***>>>                                                                          
         CLC   SOLDREP(2,R7),SNEWREP(R7)                                        
*                                  OLD REP MOVING TO NEW REP?                   
         BNE   LDIR0200            NO  -  CONTINUE PROCESSING                   
         L     RF,STAOLDNW         INCREMENT OLD=NEW COUNTER                    
         LA    RF,1(RF)                                                         
         ST    RF,STAOLDNW                                                      
**       GOTO1 REPORT                                                           
**       MVC   P+1(30),=C'FOLLOWING STA OLD REP=NEW REP '                       
**       GOTO1 REPORT                                                           
**       MVC   P+1(32),MYWORK                                                   
**       MVI   P+33,C'/'                                                        
**       MVC   P+34(16),0(R7)                                                   
**       GOTO1 REPORT                                                           
*                                                                               
         XC    0(16,R7),0(R7)                                                   
         B     LDIR0020            GO BACK FOR NEXT INPUT                       
*                                     DON'T BUMP TABLE:  WILL OVERWRITE         
*                                        THIS ENTRY                             
***>>>                                                                          
*                .2.4.6.8.0.2                                                   
COCONVRT DC    C'CHRISTAL    ',C'CR'                                            
LCOCONV  EQU   *-COCONVRT                                                       
         DC    C'EASTMAN     ',C'EA'                                            
         DC    C'KATZ RADIO  ',C'KU'                                            
***>>>   DC    C'KATZ HISPANI',C'KF'  PER J.BREWER 10/24/00                     
         DC    C'SENTRY      ',C'S3'                                            
         DC    C'CLR CHNL A  ',C'NU'                                            
         DC    C'CLR CHNL B  ',C'NU'                                            
         DC    C'CLR CHNL C  ',C'NU'                                            
         DC    C'CLC A       ',C'NU'                                            
         DC    C'CLC B       ',C'NU'                                            
         DC    C'CLC C       ',C'NU'                                            
         DC    C'CHANNEL     ',C'NU'                                            
         DC    C'DIAMOND     ',C'NU'                                            
         DC    C'EMERALD     ',C'NU'                                            
         DC    C'SAPPHIRE    ',C'NU'                                            
         DC    X'0000'                                                          
         DC    H'0'                                                             
LDIR0200 EQU   *                                                                
         MVC   SOLDGRP(2,R7),OLDGROUP+12(R2)                                    
*                                  INSERT OLD GROUP INTO TABLE                  
         MVC   SNEWGRP(2,R7),NEWGROUP+12(R2)                                    
*                                  INSERT NEW GROUP INTO TABLE                  
         BAS   RE,JOINLEAV         SET INFO INTO JOIN/LEAVE TABLE               
*                                     STATION TBL -> JOIN/LEAVE TBL             
         LA    R7,STATABLN(R7)     BUMP TO NEXT AVAILABLE SLOT                  
         ST    R7,ANEXTSTA                                                      
         L     R1,STACTR                                                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         ST    R1,STACTR           SAVE COUNT                                   
         B     LDIR0020            GO BACK FOR NEXT RECORD                      
LDIR0400 EQU   *                                                                
         L     R4,ASTAAREA         SET A(TABLE)                                 
         L     R3,STACTR           SET COUNTER                                  
         GOTO1 XSORT,DMCB,(0,(R4)),(R3),16,7,0                                  
*                                                                               
         L     R4,AJOLAREA         SET A(TABLE)                                 
         L     R3,JOLCTR           SET COUNTER                                  
         PRINT GEN                                                              
         GOTO1 XSORT,DMCB,(0,(R4)),(R3),JOLTABLN,9,0                            
         PRINT NOGEN                                                            
*                                                                               
         CLI   QUESTOR,C'Y'        PRINT TABLES?                                
         BNE   LDIR0440            **NOOP FOR TABLE PRINT                       
*                                                                               
         L     R2,ASTAAREA         SET A(STATION TABLE)                         
*                                                                               
         LA    R3,1                SET COUNTER                                  
LDIR0420 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    LDIR0440            YES                                          
*                                                                               
         EDIT  (R3),(5,P+1)                                                     
         LA    R3,1(R3)                                                         
         MVC   P+16(04),=C'STA:'                                                
         MVC   P+23(STATABLN),0(R2)                                             
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,STATABLN(R2)     BUMP TO NEXT SLOT                            
         B     LDIR0420            GO BACK FOR NEXT SLOT                        
LDIR0440 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(07),=C'STACTR='                                              
         EDIT  STACTR,(6,P+10)                                                  
         GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,JOLDATES         READ STNS, GET JOIN/LEAVES                   
*                                                                               
         BAS   RE,STAEXIST         SCAN JOIN/LEAVE FOR STATIONS                 
*                                     WHICH ALREADY EXIST ON TARGET REP         
*                                                                               
         BAS   RE,STACLOSE         SCAN JOIN/LEAVE FOR STATIONS                 
*                                     WHICH ALREADY EXIST ON TARGET REP         
*                                        AND SET CLOSE DATE                     
*                                                                               
         L     R2,AJOLAREA         SET A(JOIN/LEAVE TABLE)                      
*                                                                               
         LA    R3,1                SET COUNTER                                  
LDIR0520 EQU   *                                                                
         LA    R6,JOLTABLN(R6)     POSITION R6 TO NEXT SLOT                     
         CLI   0(R2),0             END OF TABLE?                                
         BE    LDIR0540            YES                                          
*                                                                               
         CLI   QUESTOR,C'Y'        PRINT TABLES?                                
         BNE   LDIR0525                                                         
         EDIT  (R3),(5,P+1)                                                     
         LA    R3,1(R3)                                                         
         MVC   P+16(04),=C'J/L:'                                                
         MVC   P+23(JOLTABLN),0(R2)                                             
         TM    JFLAGBYT(R2),X'20'  ON FILE ON TARGET?                           
         BNO   LDIR0523            NO                                           
         MVC   P+57(17),=C'ALREADY ON TARGET'                                   
         OC    JCLOSDTE(2,R2),JCLOSDTE(R2)                                      
         BZ    LDIR0523                                                         
         XC    WORK(3),WORK                                                     
         MVC   WORK(2),JCLOSDTE(R2)                                             
         GOTO1 DATCON,DMCB,(3,WORK),(5,P+45)                                    
LDIR0523 EQU   *                                                                
         GOTO1 REPORT                                                           
LDIR0525 EQU   *                                                                
*                                                                               
         CLC   0(7,R2),JOLTABLN(R2)   NEXT SLOT = THIS SLOT?                    
*                                        NEWREP/STATION                         
         BNE   LDIR0530                                                         
         L     RF,STADUPED         INCREMENT SAME REP/SAME STATION              
         LA    RF,1(RF)                                                         
         ST    RF,STADUPED                                                      
LDIR0530 EQU   *                                                                
         LA    R2,JOLTABLN(R2)     BUMP TO NEXT SLOT                            
         B     LDIR0520            GO BACK FOR NEXT SLOT                        
LDIR0540 EQU   *                                                                
         MVC   P+1(07),=C'JOLCTR='                                              
         EDIT  JOLCTR,(6,P+10)                                                  
         GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,LOADSPER         LOAD S/P DATA                                
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*   LOAD SALESPERSON CONVERSION INFORMATION                                     
*                                                                               
*   S/P CONVERSION EQUATES                                                      
*                                                                               
LOADSPER NTR1                                                                   
         MVI   SALFLG,0            CLEAR ONETIME SWITCH                         
         MVC   P(64),SPACES        CLEAR PRINT LINE                             
         MVC   GSGFILTR(24),SPACES CLEAR FILTER FIELD                           
         L     R5,ATAPEREC         SET A(TAPE INPUT AREA)                       
LSAP0020 EQU   *                                                                
         XC    0(250,R5),0(R5)     CLEAR INPUT AREA                             
         GET   INTAPE2,(R5)        READ S/P BY STA/OFF TAPE RECORD              
*                                     TAPE WILL BE PARSED ON ',' SEPS           
*        GOTO1 REPORT                                                           
*        MVC   P+1(13),=C'INPUT RECORD:'                                        
*        MVC   P+14(80),0(R5)                                                   
*        GOTO1 REPORT                                                           
         XC    MYWORK,MYWORK       CLEAR WORK SPACE                             
         ZICM  RF,0(R5),2          GET LENGTH OF INPUT RECORD                   
         BCTR  RF,0                SUBTRACT 1 FOR MOVE                          
         EX    RF,LSAPMOV1         MOVE BY LENGTH                               
         B     LSAP0040                                                         
LSAPMOV1 MVC   MYWORK+8(0),4(R5)                                                
LSAP0040 EQU   *                                                                
         ZICM  RF,0(R5),2          LOAD LENGTH OF RECORD                        
         SH    RF,=H'4'            SUBTRACT CONTROL LENGTH                      
         STC   RF,MYWORK+5                                                      
**       MVC   P+1(14),=C'MYWORK RECORD:'                                       
**       MVC   P+15(80),MYWORK                                                  
**       GOTO1 REPORT                                                           
*                                                                               
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         XC    256(256,RF),256(RF) CLEAR WORK SPACE                             
         GOTO1 =V(SCANNER),DMCB,MYWORK,AWORKBLK,C',=,='                         
*                                                                               
         CLI   DMCB+4,0                                                         
         BNE   LSAP0060                                                         
         DC    H'0'                                                             
*                                                                               
LSAP0060 EQU   *                                                                
*                                                                               
*   TEST PRNTBL                                                                 
*        MVC   P+1(12),=C'SCANNER OKAY'                                         
*        GOTO1 REPORT                                                           
*        L     R4,AWORKBLK         A(PRODUCT RECORD)                            
*        LA    RF,320                                                           
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
         CLI   SALFLG,0            FIRST RECORD IN?                             
         BNE   LSAP0080            NO                                           
         MVI   SALFLG,1            YES - TURN OFF FLAG                          
         BAS   RE,SETFILTS         SET GSGFILTR TABLE                           
         B     LSAP0020            GO BACK FOR NEXT ENTRY                       
LSAP0080 EQU   *                                                                
         L     R2,ANEXTSAL         SET A(NEXT OPEN SLOT)                        
         L     R4,AWORKBLK         SET A(WORK BLOCK AREA)                       
         LA    R4,12(R4)           BUMP TO OFFICE CODE                          
         L     R5,AWORKBLK         SET A(WORK BLOCK AREA) AGAIN                 
         LA    R5,108(R5)          BUMP TO FIRST S/P CODE IN TABLE              
         LA    R3,GSGFILTR         SET A(GSGFILTR TABLE)                        
         LA    R0,6                SIX COMPANIES INVOLVED                       
*                                  SET LOOP CONTROL                             
LSAP0100 EQU   *                                                                
         CLI   0(R5),C'X'          SKIP INDICATOR?                              
         BE    LSAP0160            YES - BUMP UP W/NO TABLE                     
*                                                                               
*                                  BUILD TABLE SLOT ENTRY                       
*                                                                               
         MVC   0(2,R2),0(R3)       LOAD GSGFILTR (NEW COMPANY)                  
         MVC   2(2,R2),0(R4)       LOAD OFFICE CODE                             
         MVC   4(3,R2),0(R5)       LOAD NEW S/P CODE                            
*                                                                               
*   TEST SALTABLE                                                               
*        MVC   P+1(15),=C'SALTAB ENTRY-->'                                      
*        MVC   P+16(10),0(R2)                                                   
*        GOTO1 REPORT                                                           
*   TEST SALTABLE                                                               
*                                                                               
         LA    R2,10(R2)           BUMP TO NEXT TABLE SLOT                      
         ST    R2,ANEXTSAL         BUMP TO NEXT                                 
         L     RF,SALCTR           INCREMENT S/P COUNTER                        
         LA    RF,1(RF)                                                         
         ST    RF,SALCTR                                                        
LSAP0160 EQU   *                                                                
*                                  S/P+GSGFILTR PTRS GET BUMPED                 
*                                     EVERY TIME                                
         LA    R5,32(R5)           BUMP TO NEXT S/P CODE IN TABLE               
         LA    R3,2(R3)            BUMP TO NEXT GSGFILTR                        
         BCT   R0,LSAP0100                                                      
         B     LSAP0020            GO BACK FOR NEXT TAPE                        
LSAP0180 EQU   *                                                                
         L     R4,ASALAREA         SET A(TABLE)                                 
         L     R3,SALCTR           SET COUNTER                                  
         GOTO1 XSORT,DMCB,(0,(R4)),(R3),10,4,0                                  
*                                                                               
         CLI   QUESTOR,C'Y'        PRINT TABLES?                                
         BNE   LSAP0220            NO                                           
*                                                                               
         L     R2,ASALAREA         SET A(S/P TABLE FOR REDISPLAY)               
LSAP0200 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    LSAP0220                                                         
         MVC   P+1(04),=C'S/P:'                                                 
         MVC   P+09(10),0(R2)                                                   
         GOTO1 REPORT                                                           
         LA    R2,10(R2)                                                        
         B     LSAP0200            GO BACK FOR NEXT                             
LSAP0220 EQU   *                                                                
         MVC   P+1(07),=C'SALCTR='                                              
         EDIT  SALCTR,(6,P+10)                                                  
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
*                                                                               
*   GSGFILTR:  GROUP/SUBGROUP FILTER TABLE FOR SALESPERSON SWAPPING.            
*        THE FIRST ENTRY FROM THE S/P TAPE WILL CONTAIN A SET OF                
*        G/SG CODES WHICH SERVE AS POSITIONAL FILTERS FOR OFFICE/               
*        SALESPERSON ASSIGNMENT.  IF THE STATION BEING MOVED IS                 
*        BEING GIVEN THE 'THIRD' G/SG IN THE LIST, THE PROPER S/P               
*        TO INSERT INTO THE RECORD IS THE 'THIRD' CODE FOR THAT                 
*        OFFICE.  AN 'X' MEANS LEAVE THE CODE ALONE(?).                         
*                                                                               
GSGFILTR DS    12CL2                                                            
LGSGFILT EQU   2                                                                
*                                                                               
SALFLG   DS    X                                                                
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*   THERE ARE SIX COMPANIES.  SIX GSGFILTS WILL BE LOADED INTO THE              
*        FIRST SIX ENTRIES IN THE TABLE, WHICH IS SET TOO BIG.                  
*        FILTERS ARE SCANNER'D INTO 4TH THRU 9TH FIELDS                         
*                                                                               
SETFILTS NTR1                                                                   
         L     RF,AWORKBLK         SET A(SCANNER AREA)                          
         LA    RF,108(RF)          BUMP TO FIRST SCANFIELD                      
         LA    R1,GSGFILTR         SET A(FILTER AREA)                           
         LA    RE,6                SET LOOP CONTROL                             
SFIL0020 EQU   *                                                                
         MVC   0(2,R1),0(RF)                                                    
         LA    RF,32(RF)           BUMP TO NEXT SCANNER FIELD                   
         LA    R1,LGSGFILT(R1)     BUMP TO NEXT FILTER FIELD                    
         BCT   RE,SFIL0020         GO BACK FOR NEXT                             
         GOTO1 REPORT                                                           
         MVC   P+1(10),=C'GSGFILTRS='                                           
         MVC   P+11(32),GSGFILTR                                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
AREC     DS    A                                                                
         EJECT                                                                  
*                                                                               
*   JOINLEAV:  INSERT INFO FROM STATION TABLE INTO JOIN/LEAVE DATE              
*        TABLE FOR DETERMINING PROPER EARLIEST JOIN DATE                        
*        R7 -> CURRENT STATION TABLE ENTRY                                      
*                                                                               
*                                                                               
JOINLEAV NTR1                                                                   
         L     RF,ANEXTJOL         INSERT INFO INTO JOIN/LEAVE                  
         MVC   JNEWREP(2,RF),SNEWREP(R7)                                        
         MVC   JSTATION(5,RF),SSTATION(R7)                                      
         MVC   JOLDREP(2,RF),SOLDREP(R7)                                        
         MVC   JNEWGRP(2,RF),SNEWGRP(R7)                                        
         LA    RF,JOLTABLN(RF)                                                  
         ST    RF,ANEXTJOL         SAVE A(NEXT SLOT)                            
         L     RF,JOLCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,JOLCTR           INCREMENT COUNTER                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  JOLDATES:  FOR EACH ENTRY IN THE JOIN/LEAVE STATION TABLE, ACCESS            
*        THE STATION RECORD FOR THAT COMPANY, AND GET ITS JOIN                  
*        AND LEAVE DATES.  INSERT THESE INTO THE TABLE                          
*                                                                               
JOLDATES NTR1                                                                   
         L     R2,AJOLAREA         SET A(JOIN/LEAVE TABLE)                      
JDAT0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    JDAT0800            YES - FINISHED                               
         XC    KEY,KEY                                                          
         MVI   KEY,2               INSERT RECORD TYPE                           
         MVC   KEY+20(2),7(R2)     INSERT OLD REP CODE                          
         MVC   KEY+22(5),2(R2)     INSERT STATION CALL LETTERS                  
         GOTO1 HIGHDIR             READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    JDAT0040            YES                                          
         MVC   P+1(23),=C'STATION DOES NOT EXIST:'                              
         MVC   P+28(20),0(R2)                                                   
         GOTO1 REPORT                                                           
         OI    JFLAGBYT(R2),X'08'                                               
         B     JDAT0060                                                         
JDAT0040 EQU   *                                                                
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         LA    R6,REC                                                           
         USING RSTAREC,R6                                                       
*                                                                               
         MVC   JJOINDAT(3,R2),RSTASTRT   INSERT STATION JOIN  DATE              
         MVC   JLEAVDAT(3,R2),RSTAEND    INSERT STATION LEAVE DATE              
*                                                                               
JDAT0060 EQU   *                                                                
         LA    R2,JOLTABLN(R2)     BUMP TO NEXT TABLE SLOT                      
         B     JDAT0020            GO BACK FOR NEXT                             
JDAT0800 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*  STAEXIST:  CHECK NEWREP/STATION FOR STATION RECORDS WHICH     *              
*        ALREADY EXIST.  IF THIS IS THE CASE, GENERATING A NEW   *              
*        RECORD RECORD WOULD RESULT IN DUPLICATE KEYS AT LOAD    *              
*        TIME.  IF A STATION RECORD IS FOUND, THE TABLE ENTRY -  *              
*        OR ENTRIES - WILL HAVE THE FLAG BYTE OR'D WITH X'40'.   *              
*                                                                *              
*                                                                *              
*                                                                *              
******************************************************************              
STAEXIST NTR1                                                                   
         L     R2,AJOLAREA         SET A(JOIN/LEAVE TABLE)                      
         LR    R3,R2               SET BOTH TO SAME SLOT IN TABLE               
STEX0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    STEX0800            YES - FINISHED                               
         XC    KEY,KEY                                                          
         MVI   KEY,2               INSERT RECORD TYPE                           
         MVC   KEY+20(2),0(R2)     INSERT NEW REP CODE                          
         MVC   KEY+22(5),2(R2)     INSERT STATION CALL LETTERS                  
         GOTO1 HIGHDIR             READ KEY                                     
*                                                                               
*   TEST                                                                        
**       CLI   QUESTOR,C'Y'        PRINT TABLE WORK?                            
**       BNE   TSTEX010            NO                                           
**       MVC   P+1(12),=C'KEY VS SAVE:'                                         
**       MVC   P+15(27),KEY                                                     
**       MVC   P+45(27),KEYSAVE                                                 
**       GOTO1 REPORT                                                           
TSTEX010 EQU   *                                                                
*   TEST                                                                        
*                                                                               
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   STEX0040            NO                                           
         OI    JFLAGBYT(R2),X'20'  YES - SET FLAG INDICATOR                     
*                                                                               
         GOTO1 GETRECRD            ALREADY ON FILE: CHECK G/SGRP                
         LA    R4,REC                                                           
         USING RSTAREC,R4                                                       
*                                                                               
         CLC   RSTAGRUP,JNEWGRP(R2)                                             
*                                  STATION GROUP = NEW TABLE GROUP?             
         DROP  R4                                                               
*                                                                               
         BE    STEX0030            YES                                          
         MVC   P+1(15),=C'G/SGP MISMATCH:'                                      
         MVC   P+17(JOLTABLN),0(R2)      DISPLAY TABLE ENTRY                    
         GOTO1 REPORT                                                           
         L     RF,MISMATCH                                                      
         LA    RF,1(RF)                                                         
         ST    RF,MISMATCH                                                      
STEX0030 EQU   *                                                                
*   TEST                                                                        
**       CLI   QUESTOR,C'Y'        PRINT TABLE WORK?                            
**       BNE   TSTEX020            NO                                           
**       MVC   P+1(12),=C'JFLAGBYT   :'                                         
**       MVC   P+15(16),0(R2)                                                   
**       GOTO1 REPORT                                                           
TSTEX020 EQU   *                                                                
*   TEST                                                                        
*                                                                               
STEX0040 EQU   *                                                                
         LA    R3,JOLTABLN(R3)     CHECK NEXT SLOT FOR SAME                     
*                                     NEWREP/STATION                            
         CLC   0(7,R2),0(R3)       SAME NEWREP/STATION?                         
         BNE   STEX0060            NO  - LOOP TO TOP                            
         OC    JFLAGBYT(1,R3),JFLAGBYT(R2)                                      
*                                  YES - SET SAME FLAG                          
         B     STEX0040            GO BACK AND CHECK NEXT SLOT                  
STEX0060 EQU   *                                                                
         LR    R2,R3               SET TO FIRST UNMATCHED SLOT                  
         B     STEX0020            GO BACK FOR NEXT                             
STEX0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
**STACLOSE                                                                      
******************************************************************              
*  STACLOSE:  CYCLE AJOLAREA FOR STATIONS MARKED AS 'ALREADY     *              
*        ON FILE.'  ACCESS THE OLD STATION RECORD, AND PLUG IN   *              
*        THE CLOSE DATE FOR THAT STATION.                        *              
*                                                                *              
*                                                                *              
******************************************************************              
STACLOSE NTR1                                                                   
         L     R2,AJOLAREA         SET A(JOIN/LEAVE TABLE)                      
STAC0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    STAC0800            YES - FINISHED                               
         TM    JFLAGBYT(R2),X'20'  NO  - STATION ALREADY ON FILE?               
         BO    STAC0040            YES - GET THE CLOSE DATE                     
         LA    R2,JOLTABLN(R2)     NO  - LEAVE THIS ONE ALONE                   
         B     STAC0020            GO BACK FOR NEXT                             
STAC0040 EQU   *                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,2                     INSERT RECORD TYPE                     
         MVC   KEY+20(2),JOLDREP(R2)     INSERT NEW REP CODE                    
         MVC   KEY+22(5),JSTATION(R2)    INSERT STATION                         
         GOTO1 HIGHDIR                   READ KEY                               
         CLC   KEY(27),KEYSAVE     SAME KEY?                                    
         BNE   STAC0060            NO  - SKIP THIS ENTRY                        
*                                                                               
         GOTO1 GETRECRD            ALREADY ON FILE: CHECK G/SGRP                
         LA    R4,REC                                                           
         USING RSTAREC,R4                                                       
         MVC   JCLOSDTE(2,R2),RSTACLDT                                          
*                                  INSERT CLOSE DATE INTO TABLE                 
*                                                                               
STAC0060 EQU   *                                                                
         LA    R2,JOLTABLN(R2)     BUMP TO NEXT SLOT IN TABLE                   
         B     STAC0020            GO BACK FOR NEXT                             
STAC0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         DROP  R4                                                               
**STACLOSE                                                                      
*                                                                               
******************************************************************              
*   SCANQUOT:  REMOVE ',' FROM " INPUT                                          
******************************************************************              
SCANQUOT NTR1                                                                   
         LA    RF,MYWORK+8           SET A(INPUT)                               
SQUO0020 EQU   *                                                                
         CLI   0(RF),0             BINARY ZERO FOUND?                           
         BE    SQUO0800            YES - FINISHED                               
         CLI   0(RF),C'"'          DOUBLE QUOTE FOUND?                          
         BNE   SQUO0030            NO                                           
***      MVI   0(RF),C' '          YES - REPLACE WITH SPACE                     
         B     SQUO0040            YES                                          
SQUO0030 EQU   *                                                                
         LA    RF,1(RF)            NO  - GO BACK FOR NEXT                       
         B     SQUO0020                                                         
SQUO0040 EQU   *                                                                
         LA    RF,1(RF)            PICK UP NEXT CHARACTER                       
SQUO0060 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD REACHED?                       
         BNE   *+6                 NO                                           
         DC    H'0'                YES - MUST FIND SECOND "                     
         CLI   0(RF),C','                                                       
         BE    SQUO0080            COMMA FOUND                                  
         CLI   0(RF),C'"'                                                       
         BNE   SQUO0040            GO BACK FOR NEXT CHAR                        
***      MVI   0(RF),C' '          REPLACE " WITH SPACE                         
         B     SQUO0800            LAST " FOUND - FINISHED                      
SQUO0080 EQU   *                                                                
         MVI   0(RF),X'40'         REPLACE ',' WITH SPACE                       
         B     SQUO0040            GO BACK FOR NEXT                             
SQUO0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*   COVRPROC:  MOVE COVERSHEET RECORDS.                                         
******************************************************************              
COVRPROC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   P+1(18),=C'**BEGIN COVRPROC**'                                   
         GOTO1 REPORT                                                           
         XC    COVRWORK,COVRWORK                                                
*                                                                               
COVR0020 EQU   *                                                                
*                                                                               
*   TEST COVR SORT DISPLAY                                                      
**       MVC   P+1(10),=C'COVR SORT:'                                           
**       MVC   P+11(16),SORTREC                                                 
***      GOTO1 REPORT                                                           
*   TEST COVR SORT DISPLAY END                                                  
*                                                                               
*                                                                               
*   TEST DUMP                                                                   
***      CLI   STYP,X'FF'          END OF FILE?                                 
***      BNE   *+6                 NO                                           
***      DC    H'0'                                                             
***      GOTO1 =A(GETSORT)         YES - RETRIEVE NEXT SORTREC                  
***      B     COVR0020            GO BACK AND CHECK IT                         
*   TEST DUMP                                                                   
*                                                                               
         CLI   STYP,1              STILL BUY SORT RECORD?                       
         BH    COVR0040            NO                                           
         GOTO1 =A(GETSORT)         YES - RETRIEVE NEXT SORTREC                  
         B     COVR0020            GO BACK AND CHECK IT                         
COVR0040 EQU   *                                                                
         BAS   RE,COVRCOMP         SET UP COMPARISON KEY                        
*                                                                               
         LA    R2,REC                                                           
         USING RCOVREC,R2                                                       
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'49'           INSERT RECORD TYPE                           
         GOTO1 HIGHDIR                                                          
         B     COVR0080                                                         
COVR0060 EQU   *                                                                
         GOTO1 SEQDIR              READ NEXT KEY                                
COVR0080 EQU   *                                                                
         CLI   KEY,X'49'           COVERSHEET RECORD?                           
         BH    COVR0900            NO  - FINISHED WITH COVERSHEET               
         GOTO1 GETRECRD                                                         
*                                                                               
COVR0100 EQU   *                                                                
*                                                                               
*   TEST COVRSHEET DISPLAY                                                      
**       MVC   P+1(05),=C'COVR:'                                                
**       MVC   P+6(27),RCOVREC                                                  
**       MVC   P+35(27),COVRWORK                                                
**       GOTO1 REPORT                                                           
*   TEST COVRSHEET DISPLAY END                                                  
*                                                                               
         CLC   RCOVREC(26),COVRWORK                                             
         BNL   COVR0120                                                         
*                                                                               
*   TEST COVRSHEET DISPLAY                                                      
**       MVC   P+1(05),=C' LOW:'                                                
**       MVC   P+6(27),RCOVREC                                                  
**       MVC   P+35(27),COVRWORK                                                
**       MVC   P+65(12),=C'NO CONTRACT!'                                        
**       GOTO1 REPORT                                                           
         L     RF,NOCONCOV                                                      
         LA    RF,1(RF)                                                         
         ST    RF,NOCONCOV                                                      
         B     COVR0060            GO BACK FOR NEXT 49 RECORD                   
*   TEST COVRSHEET DISPLAY END                                                  
*                                                                               
COVR0120 EQU   *                                                                
         BE    COVR0140                                                         
*                                                                               
*   TEST COVRSHEET DISPLAY 2                                                    
**       MVC   P+1(05),=C'HIGH:'                                                
**       MVC   P+6(27),RCOVREC                                                  
**       MVC   P+35(27),COVRWORK                                                
**       GOTO1 REPORT                                                           
*   TEST COVRSHEET DISPLAY 2 END                                                
*                                                                               
         BAS   RE,COVRSORT         RETRIEVE NEXT SORT RECORD                    
         B     COVR0100            GO BACK AND TEST IT                          
COVR0140 EQU   *                                                                
         XC    REC-4(4),REC-4                                                   
         MVC   RCOVKREP,STNEWREP   INSERT TARGET REP INTO KEY                   
         MVC   RCOVKNAM+4(4),STNEWCON                                           
*                                  INSERT NEW CONTRACT NUMBER                   
         MVC   REC-4(2),RCOVLEN    INSERT LENGTH FOR DISPLAY                    
         GOTO1 PUTRECS                                                          
*                                                                               
         CLI   RCOVREC+26,0        DESCRIPTOR RECORD?                           
         BNE   COVR0200            NO                                           
         L     RF,COVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,COVCTR                                                        
         B     COVR0220                                                         
COVR0200 EQU   *                                                                
         L     RF,COVCTR1          SUBRECORD COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,COVCTR1                                                       
*                                                                               
COVR0220 EQU   *                                                                
         CLC   COVCTR,=F'25'                                                    
         BH    COVR0060                                                         
*                                                                               
         MVC   P+1(11),=C'COVERSHEET:'                                          
         MVC   P+12(27),RCOVREC                                                 
         GOTO1 REPORT                                                           
*                                                                               
         B     COVR0060            GO BACK FOR NEXT RECORD                      
COVR0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
COVRCOMP NTR1                                                                   
         B     CCOM0040                                                         
COVRSORT NTR1                                                                   
         GOTO1 =A(GETSORT)         RETRIEVE NEXT SORT RECORD                    
*                                                                               
         MVC   P+1(11),=C'GETSORT   :'                                          
         MVC   P+12(27),SORTREC                                                 
         GOTO1 REPORT                                                           
*                                                                               
CCOM0040 EQU   *                                                                
         L     RF,COVRBACK                                                      
         LA    RF,1(RF)                                                         
         ST    RF,COVRBACK                                                      
*                                                                               
         MVI   COVRWORK,X'49'                                                   
         MVC   COVRWORK+16(2),STOLDREP                                          
*                                  INSERT OLD REP CODE                          
         MVI   COVRWORK+18,X'FF'                                                
         MVC   COVRWORK+22(4),STOLDCON                                          
*                                  INSERT OLD CONTRACT NUMBER                   
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**TKCV**                                                                        
COVRWORK DS    CL26                WORK AREA FOR COMPARISON KEY                 
*                                                                               
*   DROPCOMS:  RECORD IS BEING MOVED.  SCAN FOR C=/SC= IN COMMENT               
*        FIELDS, DROP COMMENTS IF FOUND.  PROVIDE TOTALS                        
*                                                                               
DROPCOMS NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(CONTRACT RECORD)                     
         MVI   DROPFLAG,0          CLEAR DROPFLAG                               
         USING RCONREC,R5                                                       
         LA    R2,RCONELEM                                                      
DROP0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    DROP0800            YES                                          
         LA    R3,DROP02C          SET A(02 C= COUNTER)                         
         CLI   0(R2),X'02'         02 COMMENT ELEMENT?                          
         BNE   DROP0040            NO                                           
         BAS   RE,CHECKELT         CHECK FOR C=/SC=                             
         B     DROP0200                                                         
DROP0040 EQU   *                                                                
         LA    R3,DROP07C          SET A(07 C=COUNTER)                          
         CLI   0(R2),X'07'         07 COMMENT ELEMENT?                          
         BNE   DROP0060            NO                                           
         BAS   RE,CHECKELT         CHECK FOR C=/SC=                             
         B     DROP0200                                                         
DROP0060 EQU   *                                                                
         LA    R3,DROP11C          SET A(11 C=COUNTER)                          
         CLI   0(R2),X'11'         11 COMMENT ELEMENT?                          
         BNE   DROP0080            NO                                           
         BAS   RE,CHECKELT         CHECK FOR C=/SC=                             
         B     DROP0200                                                         
DROP0080 EQU   *                                                                
         LA    R3,DROP82C          SET A(82 C=COUNTER)                          
         CLI   0(R2),X'82'         82 COMMENT ELEMENT?                          
         BNE   DROP0200            NO                                           
         BAS   RE,CHECKELT         CHECK FOR C=/SC=                             
         B     DROP0200                                                         
DROP0200 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         B     DROP0020            GO BACK TO TEST NEXT ELT                     
DROP0800 EQU   *                                                                
         CLI   DROPFLAG,0          ANYTHING DROPPED?                            
         BE    DROP0900            NO                                           
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0            
*                                  DROP DELETED COMMENT ELTS                    
DROP0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
CHECKELT NTR1                                                                   
         MVC   CMMTFLAG,=C'C= '                                                 
         CLC   =C'C=',2(R2)        STORED COMMENT ELEMENT?                      
         BE    CELT0040            YES                                          
         MVC   CMMTFLAG,=C'SC='                                                 
         LA    R3,4(R3)            NO  - BUMP TO OFFICE COMMENT COUNTER         
         CLC   =C'SC=',2(R2)       OFFICE COMMENT?                              
         BNE   CELT0800            NO  - LEAVE IT ALONE                         
CELT0040 EQU   *                                                                
         L     RF,0(R3)            LOAD COUNTER IN QUESTION                     
         LA    RF,1(RF)            INCREMENT COUNTER                            
         ST    RF,0(R3)            REPLACE COUNTER                              
         CLC   0(4,R3),=F'5'       DISPLAY FIRST N OCCURRENCES                  
         BH    CELT0060                                                         
         MVC   P+1(16),=C'CMMT  DROP: PRE:'                                     
         GOTO1 HEXOUT,DMCB,0(R2),P+26,2,=C'TOG'                                 
         MVC   P+30(10),2(R2)      DISPLAY ELEMENT FOUND                        
         GOTO1 REPORT                                                           
         LA    R4,RCONREC          A(STATION RECORD)                            
         SR    RF,RF                                                            
         ICM   RF,3,RCONLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                                                               
CELT0060 EQU   *                                                                
         MVI   0(R2),X'FF'         MARK ELEMENT FOR DROP                        
*                                                                               
         CLC   0(4,R3),=F'5'       DISPLAY FIRST N OCCURRENCES                  
         BH    CELT0080                                                         
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'CMMT  DROP: POST:'                                    
         MVC   P+20(3),CMMTFLAG                                                 
         EDIT  (4,0(R3)),(3,P+28)                                               
         GOTO1 REPORT                                                           
         LA    R4,RCONREC          A(STATION RECORD)                            
         SR    RF,RF                                                            
         ICM   RF,3,RCONLEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
CELT0080 EQU   *                                                                
         MVI   DROPFLAG,1          TURN ON FLAG:  DROP SOMETHING                
CELT0800 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
CMMTFLAG DS    CL3                                                              
DROPFLAG DS    X                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'120REREPSCCR 05/01/02'                                      
         END                                                                    
