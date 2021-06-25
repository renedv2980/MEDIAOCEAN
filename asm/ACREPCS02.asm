*          DATA SET ACREPCS02  AT LEVEL 026 AS OF 05/23/16                      
*PHASE ACCS02A,+0                                                               
*INCLUDE SORTER                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE SQUASHER                                                               
         TITLE 'CLIENT STATEMENT'                                               
ACCS02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCS**,RA                                                    
         L     R8,0(R1)                                                         
         USING ACWORKD,R8                                                       
         SPACE 1                                                                
         USING ACCSD,R7                                                         
         LA    R7,SPACEND                                                       
         SPACE 5                                                                
************************************************************                    
*    OPTION 1     LEVEL OF THE SR ACCOUNT: 1 FOR 1ST LEVEL *                    
*                                          2 FOR 2ND LEVEL *                    
*                                          3 FOR 3RD LEVEL *                    
*                                  BLANK FOR ACCOUNT LEVEL.*                    
*                                                          *                    
*    OPTION 2     S TO SUPPRESS EST AND JOB NUMBER/NAME.   *                    
*                 N SUPPRESS JOB NAME ONLY (PRINT NUMBER). *                    
*                                                          *                    
*    OPTION 3     S TO SUPPRESS BILLING SOURCE TOTALS.     *                    
*                                                          *                    
*    OPTION 4     P PRODUCTION ONLY                        *                    
*                 M MEDIA ONLY                             *                    
*                 BLANK FOR ALL                            *                    
*                 NOTE: IF NO TYPE ASSUMED TO BE PRODUCTION*                    
*                                                          *                    
*    NARRATIVE    (QSELECT FIELD)  NARRATIVE TO PRINT      *                    
*                 JUST BEFORE ACCOUNT TOTAL. 4 LINES.      *                    
*                                                          *                    
************************************************************                    
         EJECT                                                                  
*              ROUTINE FOR RUN FIRST                                            
         SPACE 1                                                                
RUNF     CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         SPACE 1                                                                
         LA    RE,RELOTAB          RELOCATE MY A TYPES                          
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
         SPACE 1                                                                
         MVC   CSPWK1(CSPLNQ),SPACES    INITIALIZE PRINT LINE 1                 
         MVC   CSPWK2(CSPLNQ),SPACES                          2                 
         MVC   CSPWK3(CSPLNQ),SPACES                          3                 
         MVC   CSPWK4(CSPLNQ),SPACES                          4                 
         LA    R1,LSTMAX                                                        
         STC   R1,MAXLINES              SET MAX LINES PER PAGE                  
         STC   R1,PGEMAX                                                        
         MVC   PAGE,=H'1'                                                       
         LA    R0,2                                                             
*&&DO                                                                           
RUNF3    GOTO1 PRINT,DMCB,P,=C'BC01'    SKIP TO CHANNEL 1                       
         MVI   LINE,1                   PRINT LINE UP                           
         MVI   P+1,C'X'                                                         
         MVC   P+2(81),P+1                                                      
         GOTO1 ACREPORT                                                         
         MVI   GETLNE,XXXLNE            SKIP TO DETAIL LINE - 2                 
         BAS   RE,SKIPLNE                                                       
         MVI   P+1,C'X'                                                         
         MVC   P+2(81),P+1                                                      
         MVC   PSECOND,P                                                        
         GOTO1 ACREPORT                                                         
         BCT   R0,RUNF3                                                         
*&&                                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR REQUEST FIRST                                        
         SPACE 1                                                                
REQF     CLI   MODE,REQFRST                                                     
         BNE   PRCT                                                             
         MVI   RCSUBPRG,0                                                       
         SPACE 1                                                                
         L     R4,ANARRA                                                        
         XC    0(42,R4),0(R4)      CLEAR NARRATIVE KEY                          
         CLC   QSELECT,SPACES      NARRATIVE NUMBER                             
         BE    *+8                 NO- CONTINUE                                 
         BAS   RE,GTNARR                                                        
         MVC   PROFS,PROGPROF                                                   
         XC    ALSORT,ALSORT       CLEAR A(LAST SORT)                           
         LA    R1,SRTKLNQ          SORT KEY LENGTH                              
         CVD   R1,DUB              CONVERT KEY LENGTH TO CHARACTER              
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SRTLNQ           SORT RECORD LENGTH                           
         CVD   R1,DUB              CONVERT RECORD LENGTH TO CHARACTER           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,ENDATE0)                               
         CLC   QSTART,SPACES       USE TODAY AS BILL DATE                       
         BE    *+10                                                             
         MVC   ENDATE0,QSTART      UNLESS BILL DATE SPECIFIED                   
         GOTO1 DATCON,DMCB,(0,ENDATE0),(1,ENDATE1)                              
         SPACE 1                                                                
         MVC   MOS(1),ENDATE0+1            MOVE IN THE YEAR                     
         MVC   MOS+1(1),ENDATE0+3          MONTH IF 9 OR LESS                   
         CLI   ENDATE0+2,C'0'              IS MONTH 09                          
         BE    REQF03                                                           
         SPACE 1                                                                
         MVI   MOS+1,C'A'                  A FOR OCT                            
         CLI   ENDATE0+3,C'0'              IS END MONTH OCT?                    
         BE    REQF03                      YES - MOS COMPLETE                   
         MVI   MOS+1,C'B'                  B FOR NOV                            
         CLI   ENDATE0+3,C'1'              IS IT NOV?                           
         BE    REQF03                      YES - MOS COMPLETE                   
         MVI   MOS+1,C'C'                  IT MUST BE DEC                       
         SPACE 1                                                                
REQF03   ZAP   SOURCTOT,=P'0'      CLEAR BILING SOURCE TOTAL                    
         ZAP   ACTTOT,=P'0'        CLEAR ACCOUNT TOTAL                          
         ZAP   REQTOT,=P'0'        CLEAR REQUEST TOTAL                          
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR PROCESS TRANSACTION                                  
         SPACE 1                                                                
PRCT     CLI   MODE,PROCTRNS                                                    
         BNE   REQL                                                             
         USING TRANSD,R4                                                        
         L     R4,ADTRANS                                                       
         CLI   TRNSEL,X'44'                                                     
         BNE   XIT                                                              
*        CLC   TRNSDATE(2),ENDATE1       YM EQUAL ENDDATE(TRNS MON/YY)          
         CLC   TRNSBTCH(2),MOS           YM EQUAL ENDDATE (MOS)                 
         BNE   XIT                       WRONG MONTH                            
         TM    TRNSSTAT,X'80'            IS IT A DEBIT?                         
         BNO   XIT                                                              
         SPACE 1                                                                
         USING SRTD,R5                                                          
         LA    R5,SRTWRK                                                        
         XC    SRTWRK(SRTLNQ),SRTWRK     CLEAR SORT WORK AREA                   
         ZAP   SRTDBS,=P'0'              CLEAR DEBITS                           
         SPACE 3                                                                
         L     R2,ADTRANS                                                       
         SH    R2,DATADISP               GET TO BEGINNING OF THE KEY            
         USING ACKEYD,R2                                                        
         MVC   SRTACC,ACKEYACC           ACCOUNT CODE                           
         MVC   SRTCON,ACKEYCON           BILLING SOURCE                         
         MVC   SRTDTE,TRNSDATE           TRANSACTION DATE                       
         MVC   SRTINV,TRNSREF            INVOICE NUMBER                         
         SPACE 1                                                                
         ZAP   SRTDBS,TRNSAMNT           DEBIT AMOUNT                           
         SPACE 1                                                                
         L     R2,ADTRANS                GET OTHER NUMBER EL X'4F'              
         SH    R2,DATADISP                                                      
         GOTO1 GETEL,DMCB,(X'4F',(R2)),0                                        
         CLI   ELERR,0                       DID YOU FIND X'4F'                 
         BNE   PRCT2                                                            
         SPACE 1                                                                
         USING TRCPJD,R2                                                        
         L     R2,ELADDR                                                        
         CLI   TRCPTYPE,C'J'                                                    
         BNE   PRCT2                                                            
         MVC   SRTCLI,TRCPCLI            CLIENT                                 
         MVC   SRTPRD,TRCPPROD           PRODUCT                                
         MVC   SRTJOB,TRCPJOB            AND JOB                                
         MVI   SRTTYPE,C'J'                                                     
         B     PRCT3                                                            
         EJECT                                                                  
PRCT2    L     R2,ADTRANS                GET OTHERS ELEMENT X'23'               
         SH    R2,DATADISP                                                      
         GOTO1 GETEL,DMCB,(X'23',(R2)),0                                        
         CLI   ELERR,0                   DID YOU FIND X'23'                     
         BNE   PRCT3                                                            
         SPACE 1                                                                
         USING ACOTHERD,R2                                                      
         L     R2,ELADDR                                                        
         MVC   SRTTYPE,ACOTPROF    J=JOB,S=SPOT EST,P =PRINT EST,ETC.           
         CLI   ACOTPROF,C'J'       IS IT A JOB?                                 
         BNE   *+10                                                             
         MVC   SRTCLI,SRTACC+4     ASSUME SAME CLIENT AS SR ACCOUNT             
         MVC   SRTPRD(9),ACOTNUM   EITHER JOB NUM OR EST BASED ON               
*                                  ACOTPROF AND  PROFNAME TABLE                 
         MVC   SRTDTE2,ACOTDATE    MEDIA MONTH OF SERVICE                       
         SPACE 3                                                                
PRCT3    CLI   QOPT4,C' '          IS PROD OR MEDIA ONLY OPTION BLANK           
         BE    PRCT3C              YES - GO CHECK LEVEL OPTION                  
         CLI   QOPT4,C'P'          FOR PRODUCTION JOBS ONLY                     
         BNE   PRCT3B              NO - IT MUST BE MEDIA ONLY                   
         SPACE 1                                                                
*                                  **PRODUCTION ONLY**                          
         CLI   SRTTYPE,C'J'        IS IT A PROD TRANS                           
         BNH   PRCT3C              IF'J' OR BINARY 0 CONTINUE                   
         B     XIT                                                              
         SPACE 1                                                                
*                                  **MEDIA ONLY**                               
PRCT3B   CLI   SRTTYPE,C'J'        IF NOT HIGHER THAN J WE DONT WANT            
         BNH   XIT                                                              
         SPACE 1                                                                
PRCT3C   CLI   QOPT1,C' '          IS LEVEL OPTION BLANK                        
         BE    PRCT5               YES - GO TO SORTER                           
         SPACE 1                                                                
         L     R2,ADLDGHIR         ADDR OF HIERARCHY ELEMENT                    
         USING ACHEIRD,R2                                                       
         CLI   QOPT1,C'1'          LEVEL ONE?                                   
         BNE   PRCT3D              NO - TRY AGAIN                               
         ZIC   R0,ACHRLEVA        LENGTH OF LEVEL A INTO R0                     
         B     PRCT4                                                            
         SPACE 1                                                                
PRCT3D   CLI   QOPT1,C'2'          LEVEL TWO?                                   
         BNE   PRCT3F              NO - TRY AGAIN                               
         ZIC   R0,ACHRLEVB         LENGTH OF LEVEL B INTO R0                    
         B     PRCT4                                                            
         SPACE 1                                                                
PRCT3F   CLI   QOPT1,C'3'          LEVEL THREE?                                 
         BNE   PRCT5               IGNORE OPTION 1 - USE LOWEST LEVEL           
         ZIC   R0,ACHRLEVC         LENGTH OF LEVEL B INTO R0                    
         SPACE 1                                                                
PRCT4    LA    R1,SRTACC+3         ADDR SRT ACCOUNT(PAST CO,UNIT,LEDG)          
         AR    R1,R0               ADD THE LENGTH OF LEVEL DESIRED              
         LA    R6,11               AN ACCOUNT LENGTH MINUS 1                    
         CR    R6,R0               IS R6 HIGHER THAN R0                         
         BNH   PRCT5               NO - USE THE LOWEST LEVEL                    
         SR    R6,R0               SETS THE LENGTH OF FIELD TO CLEAR            
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SPACES      CLEAR UNNEEDED PORTION OF SRTACC             
         SPACE 1                                                                
PRCT5    DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'PUT',(R5)                                         
         MVI   ALSORT,1            ACTIVITY SWITCH                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR REQUEST LAST                                         
         SPACE 1                                                                
REQL     CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         OC    ALSORT,ALSORT           IS THERE A LAST SORT ADDR                
         BZ    REQL99                  NO DATA                                  
         XC    LSTWRK(SRTLNQ),LSTWRK   CLEAR SAVE AREA FOR PREV REC             
         MVI   ACTIVITY,C'N'                                                    
         MVI   FIRSTIME,C'N'           1ST TIME FOR THIS BILING SOURCE          
         MVI   TOTSW,C'B'              SET TOT SWTCH TO BILLING SOURCE          
         SPACE 1                                                                
REQL2    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R5,DMCB+4                                                        
         ST    R5,ALSORT               ADDRESS OF LAST SORT                     
         LTR   R5,R5                                                            
         BZ    REQL9                   END OF RECORDS FROM SORT                 
         MVC   SRTWRK(SRTLNQ),0(R5)    SAVE CURRENT SORT RECORD                 
         OC    LSTWRK(SRTLNQ),LSTWRK   DO I HAVE ONE SAVED                      
         BNZ   REQL5                   BRANCH IF I DO.                          
REQL3    MVC   LSTWRK(SRTLNQ),SRTWRK   SAVE THIS ONE                            
         B     REQL2                   AND GET NEXT.                            
         SPACE 1                                                                
REQL5    CLC   LSTWRK(SRTKLNQ),SRTWRK                                           
         BNE   REQL9                   NOT SAME KEY                             
         LA    R6,LSTWRK                                                        
         AP    SRTDBS-SRTD(L'SRTDBS,R6),SRTDBS   ADD'EM UP                      
         OC    SRTCLI(12),SRTCLI                 SAVE ANY SIGNIFICANT           
         BZ    *+10                              DATA                           
         MVC   SRTCLI-SRTD(12,R6),SRTCLI                                        
         B     REQL2                             AND GET NEXT                   
         SPACE 1                                                                
REQL9    BAS   RE,FORMAT               SETUP PRINT                              
         OC    ALSORT,ALSORT           IS IT END OF FILE                        
         BNZ   REQL13                  NOT END OF FILE                          
         CLI   QOPT3,C'S'              SUPPRESS BILLING SOURCE TOTS?            
         BE    *+8                     YES - SKIP TO ACCOUNT TOTAL              
         SPACE 1                                                                
         BAS   RE,ACCTOT               IF END BILLING SOURCE TOTAL              
         MVI   TOTSW,C'A'                                                       
         BAS   RE,ACCTOT               IF END PRINT ACCOUNT TOTAL               
         SPACE 1                                                                
REQL11   MVI   TOTSW,C'R'                                                       
         MVI   ACTIVITY,C'N'           NEW PAGE AND HEADS                       
         BAS   RE,ACCTOT               REQUEST TOTAL                            
         B     REQL99                  AND GET OUT                              
         EJECT                                                                  
REQL13   CLI   QOPT3,C'S'              SUPPRESS BILLING SOURCE TOTS?            
         BE    REQL15                  YES - SKIP TO ACCOUNT TOTAL              
         SPACE 1                                                                
         CLC   LSTWRK(SRTBLNQ),SRTWRK      IS IT SAME ACCT/BLING SOURCE         
         BE    REQL3                       IF IT IS GET NEXT                    
         BAS   RE,ACCTOT               IF NOT PRINT B SOURCE TOTAL              
         MVI   FIRSTIME,C'Y'           1ST FOR THIS BILING SOURCE               
         SPACE 1                                                                
REQL15   CLC   LSTWRK(L'SRTACC),SRTWRK     IS IT SAME ACCOUNT                   
         BE    REQL3                       YES - GET NEXT                       
         MVI   TOTSW,C'A'                  SET TOTAL SWITCH TO ACCOUNT          
         BAS   RE,ACCTOT                   PRINT ACCOUNT TOTAL                  
         MVI   TOTSW,C'B'                  RESET SWTCH TO BILL SOURCE           
         B     REQL3                                                            
         SPACE 1                                                                
REQL99   GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FORMAT DETAIL LINE                                       
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         LA    R5,LSTWRK                                                        
         AP    SOURCTOT,SRTDBS     BILLING SOURCE TOTAL                         
         AP    ACTTOT,SRTDBS       ACCOUNT DEBIT TOTAL                          
         AP    REQTOT,SRTDBS       REQUEST TOTAL                                
         SPACE 1                                                                
         USING CSPD,R6                                                          
         LA    R6,CSPWK1                                                        
         MVC   CSPINV,SRTINV       INVOICE                                      
         CLI   QOPT2,C'S'          OPTION2=S  SUPRESS EST/JOB DETAIL            
         BE    FORMAT6                                                          
         SPACE 1                                                                
         LA    R1,TYPTABLE         '23' EL TYPES TABLE                          
         SR    RE,RE               CLEAR RE                                     
FORMAT2  CLI   0(R1),0             END OF TABLE?                                
         BE    FORMAT6             SRTTYPE NOT IN TABLE                         
*                                  ASSUME NO 23 EL AND CONTINUE                 
         SPACE 1                                                                
         CLC   SRTTYPE,1(R1)       LOOK UP TYPE                                 
         BE    FORMAT4             YOU FOUND IT--- CONTINUE                     
         ZIC   RE,0(R1)            LENGTH OF TABLE ENTRY INTO RE                
         AR    R1,RE               BUMP UP R1 BY THIS LENGTH                    
         B     FORMAT2             GO LOOKUP NEXT TAB ENTRY                     
         SPACE 1                                                                
FORMAT4  ZIC   RE,0(R1)            LENGTH OF TAB ENTRY FOR EXECUTE              
         SH    RE,=H'3'            ADJUST TO ACTUAL TAB ENTRY(-1)               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CSPDESC(0),2(R1)                                                 
         SPACE 1                                                                
FORMAT4A LA    R1,CSPDESC         ADDR OF CSPDESC INTO R1                       
         AH    RE,=H'1'           ADJUST DISPLACEMENT  FROM EX ABOVE            
         AR    R1,RE              BUMP ADDR PAST ACTUAL DESCRIP                 
         MVC   0(6,R1),SRTJOB     MOVE JOB OR EST NUM AFTER DESCRIP             
         CLI   QOPT2,C'N'         OPTION2=N SUPPRESS JOB NAME ONLY              
*                                 PRINT EST OR JOB NUMBER                       
         BE    FORMAT6            DON'T BOTHER WITH JOB NAME                    
         SPACE 1                                                                
         CLI   SRTTYPE,C'J'        IS IT A JOB NUMBER                           
         BNE   FORMAT6             NO -- GO GET AMOUNT                          
         MVC   WORK,SPACES         CLEAR WORK TO ACCEPT JOB NAME                
         BAS   RE,GETJOB           GO GET JOB NAME                              
         MVC   CSPNAME,WORK        MOVE IN NAME                                 
         SPACE 1                                                                
FORMAT6  EDIT  (P8,SRTDBS),(13,CSPAMNT),2,MINUS=YES,COMMAS=YES                  
         SPACE 1                                                                
         CLI   ACTIVITY,C'Y'      ACCOUNT ALREADY ACTIVE?                       
         BNE   FORMAT8            NO -- HEAD UP PAGE(AND SET ACTIVE SW)         
         CLI   FIRSTIME,C'Y'      FIRST TIME FOR THIS BILLING SOURCE            
         BNE   FORMAT9            NO --GO PRINT                                 
         MVI   FIRSTIME,C'N'      RESET FIRST TIME SW TO NO                     
         CLI   LINE,GRPLNE        DO WE HAVE ROOM TO START A NEW GROUP          
         BNH   FORMAT9            YES - WE CAN FIT A FEW ON THIS PAGE           
         SPACE 1                                                                
FORMAT8  MVI   ACTIVITY,C'Y'                                                    
         BAS   RE,HEADUP           FIRST FOR ACCOUNT DO HEADS                   
         SPACE 1                                                                
FORMAT9  MVI   PGEMAX,DETMAX       SET MAX FOR DETAIL                           
         BAS   RE,PRNTDET                                                       
         MVI   PGEMAX,LSTMAX       RESET DEFAULT MAX                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT DETAIL LINES                                    
         SPACE 1                                                                
PRNTDET  NTR1                                                                   
         LA    R2,CSPWK1           COMPUTE NUMBER OF LINES TO PRINT             
         SR    R3,R3               CLEAR R3                                     
         LA    R0,4                NUMBER OF WORK LINES                         
PRNT3    CLC   0(CSPLNQ,R2),SPACES        IS PRINT LINE CLEAR                   
         BE    PRNT5                      YES -                                 
         AH    R3,=H'1'                   ADD 1 TO NUMBER TO PRINT              
         LA    R2,CSPLNQ(R2)              BUMP TO NEXT LINE WORK AREA           
         BCT   R0,PRNT3                                                         
         SPACE 1                                                                
PRNT5    DS    0H                                                               
         LTR   R3,R3               IS R3 EMPTY?                                 
         BNZ   *+8                 NO - CONTINUE                                
         AH    R3,=H'1'            WILL PRINT AT LEAST ONE                      
         ZIC   R2,PGEMAX           MAX  ALLOWED FOR PAGE                        
         ZIC   R1,LINE             CURRENT LINE                                 
         AR    R1,R3               PLUS NUMBER I WANT TO PRINT                  
         CR    R1,R2               MUST HAVE ENOUGH LINES                       
         BNH   *+8                                                              
         BAS   RE,HEADUP           OR ELSE HEADUP NEW                           
         SPACE 1                                                                
         LA    R0,4                MOVE MY WORK LINES                           
         LA    R1,P                TO P, PSECOND ETC.                           
         LA    R2,CSPWK1                                                        
PRNT7    MVC   0(CSPLNQ,R1),0(R2)                                               
         MVC   0(CSPLNQ,R2),SPACES     CLEAR MY WORK LINES                      
         LA    R1,132(R1)              NEXT P LINE                              
         LA    R2,CSPLNQ(R2)           NEXT WORK LINE                           
         BCT   R0,PRNT7                                                         
         GOTO1 ACREPORT                AND PRINT IT                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT ACCOUNT TOTALS                                  
         SPACE 1                                                                
ACCTOT   NTR1                                                                   
         USING CSPD,R6                                                          
         LA    R6,CSPWK1                                                        
         LA    R5,LSTWRK                                                        
         CLI   TOTSW,C'B'          IS IT A BILLING SOURCE TOTAL                 
         BE    ACCTOT2                                                          
         CLI   TOTSW,C'A'          IS IT AN ACCOUNT TOTAL                       
         BE    ACCTOT4                                                          
         CLI   TOTSW,C'R'          IS IT A REQUEST TOTAL                        
         BE    ACCTOT5                                                          
         SPACE 1                                                                
ACCTOT2  CLI   LINE,DETMAX         HAVE I REACHED SUBTOTAL LINE MAX             
         BH    ACCTOT3                                                          
         GOTO1 ACREPORT            TO SKIP LINE BEFORE SUBTOTAL                 
         B     *+8                                                              
ACCTOT3  BAS   RE,HEADUP           IF SO HEADUP NEW PAGE                        
         MVC   CSPNAME(10),=C'TOTALS FOR'                                       
         MVC   CSPNAME+11(11),SRTCON+3          NAME OF BILLING SOURCE          
         MVC   CSPAMNT+2(11),=C'-----------'                                    
         EDIT  (P8,SOURCTOT),(14,CSPACDUE),2,MINUS=YES,COMMAS=YES               
         CLI   LINE,GRPLNE        DO WE HAVE ROOM TO START A NEW GROUP          
         BH    *+8                NO -DON'T BOTHER DOUBLE SPACING               
         MVI   SPACING,3          DOUBLE SPACE                                  
         BAS   RE,PRNTDET                                                       
         ZAP   SOURCTOT,=P'0'                   CLEAR SOURCE TOTAL              
         B     ACCTOT9                                                          
         SPACE 1                                                                
ACCTOT4  CLI   LINE,DETMAX         HAVE I REACHED DETAIL LINE MAX               
         BNH   *+8                                                              
         BAS   RE,HEADUP           IF SO HEADUP NEW PAGE                        
         CLC   QSELECT,SPACES      NARRATIVE NUMBER                             
         BE    *+8                 NO- CONTINUE                                 
         BAS   RE,PNARR            PRINT NARRATIVE IF ANY                       
         MVI   GETLNE,TOTLNE       ACCOUNT TOTAL LINE                           
         BAS   RE,SKIPLNE                                                       
         SPACE 1                                                                
         EDIT  (P8,ACTTOT),(14,CSPACDUE),2,MINUS=YES,COMMAS=YES                 
         BAS   RE,PRNTDET                                                       
         ZAP   ACTTOT,=P'0'            CLEAR ACCOUNT TOTAL                      
         MVI   ACTIVITY,C'N'                                                    
         MVC   PAGE,=H'1'          SET PAGE FOR NEXT                            
         B     ACCTOT9                                                          
         SPACE 1                                                                
ACCTOT5  BAS   RE,HEADUP           IF SO HEADUP NEW PAGE                        
         MVI   GETLNE,TOTLNE       ACCOUNT TOTAL LINE                           
         BAS   RE,SKIPLNE                                                       
         MVC   CSPNAME(13),=C'REQUEST TOTAL'                                    
         EDIT  (P8,REQTOT),(14,CSPACDUE),2,MINUS=YES,COMMAS=YES                 
         BAS   RE,PRNTDET                                                       
         ZAP   REQTOT,=P'0'        CLEAR REQUEST TOTAL                          
         SPACE 1                                                                
ACCTOT9  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SETUP AND PRINT HEADLINES                             
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         GOTO1 PRINT,DMCB,P,=C'BC01'    SKIP TO CHANNEL 1                       
         MVI   LINE,1              SET LINE NUMBER                              
         SPACE 1                                                                
         MVI   GETLNE,DTELNE       LINE NUMBER FOR DATE                         
         BAS   RE,SKIPLNE                                                       
         GOTO1 DATCON,DMCB,(1,ENDATE1),(8,P+68)                                 
*        GOTO1 ACREPORT                                                         
         MVI   GETLNE,HEADLNE                                                   
         BAS   RE,SKIPLNE          SKIP TO HEADLINES                            
         CLI   TOTSW,C'R'                                                       
         BE    HEADUP11                                                         
*                                                                               
*              GET ACCOUNT RECORD                                               
*                                                                               
         USING ACKEYD,R3                                                        
         L     R3,AIO                                                           
         LA    R5,LSTWRK                                                        
         CLC   ACKEYACC(15),SRTACC       ALREADY HAVE ACCOUNT                   
         BE    HEADUP1                                                          
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(15),SRTACC                                              
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R3),(R3)                        
         CLC   ACKEYACC(15),SRTACC                                              
         BE    *+6                                                              
         DC    H'0'                CAN'T READ ACCOUNT                           
         SPACE 1                                                                
*                                                                               
*              NOW PRINT THE HEADLINES                                          
*                                                                               
         USING ACNAMED,R4                                                       
HEADUP1  GOTO1 GETEL,DMCB,(X'20',(R3)),0                                        
         CLI   ELERR,0                                                          
         BNE   HEADUP3             NO NAME ELEMENT                              
         L     R4,ELADDR                                                        
         ZIC   R2,ACNMLEN          EL LENGTH INTO R2                            
         SH    R2,=H'2'            ADJ EL LENGTH TO ACCT NAME LENGTH            
         GOTO1 CHOPPER,DMCB,((R2),ACNMNAME),(21,P+41),(C'P',2)                  
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
         USING ACADDD,R4                                                        
HEADUP3  GOTO1 GETEL,DMCB,(X'22',(R3)),0                                        
         CLI   ELERR,0                                                          
         BNE   HEADUP7             NO ADDRESS ELEMENT                           
         L     R4,ELADDR                                                        
         ZIC   R0,ACADLNES                                                      
         LA    R1,P+41                                                          
         LA    R2,ACADADD                                                       
HEADUP5  MVC   0(L'ACADADD,R1),0(R2)                                            
         LA    R1,132(R1)                                                       
         LA    R2,L'ACADADD(R2)                                                 
         BCT   R0,HEADUP5                                                       
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
HEADUP7  MVI   GETLNE,PGELNE       LINE NUMBER FOR PAGE                         
         BAS   RE,SKIPLNE                                                       
         MVC   P+63(4),=C'PAGE'                                                 
         EDIT  (B2,PAGE),(3,P+69),3,ZERO=BLANK                                  
         GOTO1 ACREPORT                                                         
         MVC   HALF,PAGE           UPDATE PAGE NUMBER                           
         LH    R3,HALF                                                          
         AH    R3,=H'1'                                                         
         STH   R3,HALF                                                          
         MVC   PAGE,HALF                                                        
         SPACE 1                                                                
         MVI   GETLNE,ACTLNE       LINE NUMBER FOR ACCOUNT NUMBER               
         BAS   RE,SKIPLNE                                                       
         MVC   P+41(7),=C'AC/ NO.'                                              
         L     R3,AIO                                                           
         MVC   P+49(12),3(R3)      ACCOUNT CODE                                 
HEADUP11 GOTO1 ACREPORT                                                         
         MVI   GETLNE,DETLNE                                                    
         BAS   RE,SKIPLNE          SKIP TO DETAIL LINE                          
         B     XIT                                                              
*                                                                               
*              GET JOB NAME                                                     
*                                                                               
GETJOB   NTR1                                                                   
         USING ACKEYD,R3                                                        
         L     R3,AIO2                                                          
         LA    R5,LSTWRK                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'SJ'                                             
         MVC   ACKEYACC+3(12),SRTCLI                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R3),(R3)                        
         CLC   ACKEYACC+3(12),SRTCLI                                            
         BE    *+8                                                              
         B     XIT                                                              
*        DC    H'0'                CAN'T READ JOB                               
         SPACE 1                                                                
         USING ACNAMED,R4                                                       
         GOTO1 GETEL,DMCB,(X'20',(R3)),0                                        
         CLI   ELERR,0                                                          
         BNE   XIT                 NO NAME ELEMENT                              
         L     R4,ELADDR                                                        
         ZIC   R1,ACNMLEN          JOB NAME                                     
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SKIP TO NEW LINE                                      
         SPACE 1                                                                
SKIPLNE  NTR1                                                                   
         ZIC   R1,GETLNE           LINE NUMBER I WANT TO SKIP TO                
         ZIC   R2,LINE             CURRENT LINE                                 
         SR    R1,R2                                                            
         BZ    XIT                 ALREADY AT THE LINE I WANT                   
         BP    *+6                                                              
         DC    H'0'                LOST TRACK                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SKIP+1(3),DUB+6(2)                                               
         MVC   SKIP(2),=C'BL'                                                   
         AR    R2,R1               ADD SKIP COUNT  TO LINE                      
         STC   R2,LINE                                                          
         GOTO1 PRINT,DMCB,P,SKIP   AND SKIP                                     
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 3                                                                
*                                                                               
*              ROUTINE TO GET NARRATIVE                                         
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
GTNARR   NTR1                                                                   
         L     R4,ANARRA                                                        
         XC    ACKEYACC(42),ACKEYACC                                            
         MVI   ACKEYACC,X'0C'                                                   
         MVC   ACKEYACC+1(1),RCCOMPFL                                           
         PACK  DUB1,QSELECT(2)                                                  
         LA    R2,ACKEYACC+2                                                    
         EDIT  (P8,DUB1),(6,0(R2))                                              
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R4),(R4)                        
         CLI   DMCB+8,0                                                         
         BE    XIT                 FOUND IT                                     
         XC    ACKEYACC(42),ACKEYACC     NOT FOUND CLEAR AREA                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT NARRATIVE                                       
         SPACE 1                                                                
PNARR    NTR1                                                                   
         L     R4,ANARRA                 GET COMMENT EL X'3E'                   
         GOTO1 GETEL,DMCB,(X'3E',(R4)),0                                        
         CLI   ELERR,0                       DID YOU FIND X'3E'                 
         BNE   XIT                                                              
         SPACE 1                                                                
         USING ACOMMD,R4                                                        
         L     R4,ELADDR                                                        
         MVC   CHOPWRK(116),SPACES                                              
         MVC   CHOPWRK+116(100),SPACES                                          
         LA    R3,CHOPWRK                                                       
         LA    R0,3             MAX NUMBER OF SCREEN LINES USED                 
PNARR2   ZIC   R2,ACOMLEN                                                       
         SH    R2,=H'5'                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ACOMMENT                                                 
PNARR3   ZIC   R2,1(R4)            LENGTH INTO R2                               
         AR    R4,R2               BUMP R4 TO NEXT EL                           
         CLI   0(R4),0              THE END                                     
         BE    PNARR4                                                           
         CLI   0(R4),X'3E'                                                      
         BNE   PNARR3                                                           
         LA    R3,72(R3)           BUMPMAX LENGTH OF NEX SCREEN LINE            
         BCT   R0,PNARR2                                                        
PNARR4   DS    0H                                                               
         MVI   GETLNE,NARLNE             LINE NUMBER FOR NARRATIVE              
         BAS   RE,SKIPLNE                                                       
         GOTO1 SQUASHER,DMCB,CHOPWRK,216                                        
         L     R2,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((R2),CHOPWRK),(43,P+14),(C'P',4)                   
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     XIT                                                              
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    V(SORTER)                                                        
         DC    V(HELLO)                                                         
         DC    V(SQUASHER)                                                      
         DC    A(IO)                                                            
         DC    A(IO2)                                                           
         DC    A(NARRA)                                                         
         DC    X'FF'                                                            
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
         SPACE 3                                                                
**********TABLE OF X'23' ELEMENT PROF TYPES************                         
         SPACE 2                                                                
TYPTABLE DS    0C                                                               
         DC    AL1(11),C'S',C'ESTIMATE='                                        
         DC    AL1(11),C'P',C'ESTIMATE='                                        
         DC    AL1(18),C'M',C'CAMPAIGN NUMBER='                                 
         DC    AL1(11),C'N',C'ESTIMATE='                                        
         DC    AL1(13),C'J',C'JOB NUMBER='                                      
         DC    AL1(12),C'X',C'REFERENCE='                                       
         DC    AL1(0)                                                           
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
IO       DS    1008C                                                            
IO2      DS    1008C                                                            
NARRA    DS    0D                                                               
         DS    CL42                                                             
         DS    CL1000                                                           
CHOPWRK DS     CL216            72X3 EACH 3E HAS MAX 72 (SCREEN LINE)           
         EJECT                                                                  
*              DSECT FOR STORAGE AREA                                           
         SPACE 1                                                                
ACCSD    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
SORTER   DS    A                                                                
HELLO    DS    A                                                                
SQUASHER DS    A                                                                
AIO      DS    A                                                                
AIO2     DS    A                                                                
ANARRA   DS    A                                                                
         SPACE 1                                                                
GETLNE   DS    CL1                                                              
PGEMAX   DS    CL1                                                              
DTELNE   EQU   4                   LINE NUMBER TO PRINT DATE                    
HEADLNE  EQU   4                   LINE NUMBER FOR FIRST HEADLINE               
PGELNE   EQU   10                  LINE NUMBER TO PRINT PAGE                    
ACTLNE   EQU   11                  LINE NUMBER TO PRINT ACCT NUMBER             
XXXLNE   EQU   12                  LINE NUMBER FOR DETAIL LINEUP                
DETLNE   EQU   15                  LINE NUMBER FOR DETAIL                       
GRPLNE   EQU   53                  LAST LINE FOR NEW GROUP OF ACCTS             
DETMAX   EQU   55                  LAST DETAIL LINE                             
NARLNE   EQU   57                  NARRATIVE LINE                               
TOTLNE   EQU   63                  LINE NUMBER FOR ACCOUNT TOTAL                
LSTMAX   EQU   65                  LAST LINE ON PAGE                            
         SPACE 1                                                                
ALSORT   DS    A                   A(LAST SORT RECORD)                          
PROFS    DS    CL16                PROGRAM PROFILES                             
SRTWRK   DS    (SRTLNQ)C           WORK AREA FOR SORT RECORD                    
LSTWRK   DS    (SRTLNQ)C           WORK AREA FOR LAST RECORD                    
         SPACE 1                                                                
CSPWK1   DS    (CSPLNQ)C           PRINT LINE 1                                 
CSPWK2   DS    (CSPLNQ)C           PRINT LINE 2                                 
CSPWK3   DS    (CSPLNQ)C           PRINT LINE 3                                 
CSPWK4   DS    (CSPLNQ)C           PRINT LINE 4                                 
         SPACE 1                                                                
ENDATE0  DS    CL6                 AS OF DATE YYMMDD                            
ENDATE1  DS    CL3                            PWOS                              
MOS      DS    CL2                            MONTH OF SERVICE                  
         SPACE 1                                                                
SOURCTOT DS    PL8                 BILLING SOURCE TOTALS                        
ACTTOT   DS    PL8                 ACCOUNT TOTALS                               
REQTOT   DS    PL8                 REQUEST TOTALS                               
         SPACE 1                                                                
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
         SPACE 1                                                                
DUB1     DS    D                                                                
ACTIVITY DS    CL1                 HAS ACCOUNT HAD ACTIVITY                     
FIRSTIME DS    CL1                 1ST TIME FOR THIS BILLING SOURCE             
TOTSW    DS    CL1                 WHAT TOTAL LEVEL                             
SKIP     DS    CL4                                                              
         EJECT                                                                  
*              DSECT FOR SORT RECORD                                            
         SPACE 1                                                                
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTACC   DS    CL15                RECEIVABLE ACCOUNT                           
SRTCON   DS    CL15                BILLING SOURCE                               
SRTBLNQ  EQU   *-SRTKEY            SORT ACCOUNT/BILLING SOURCE LENGTH           
SRTDTE   DS    CL3                 INVOICE DATE (PWOS)                          
SRTINV   DS    CL6                 INVOICE NUMBER                               
SRTKLNQ  EQU   *-SRTKEY            SORT KEY LENGTH                              
SRTCLI   DS    CL3                 X'4F' EL -CLIENT                             
SRTPRD   DS    CL3                          -PRODUCT                            
SRTJOB   DS    CL6                          -JOB OR EST FROM 23 OR 4F           
SRTTYPE  DS    CL1                 X'23' EL -PROFILE                            
SRTDTE2  DS    XL2                          -DATE                               
SRTDBS   DS    PL8                 DEBITS(ORIGINAL AMOUNT)                      
SRTCRD   DS    PL8                 CREDITS(PAYMENTS)                            
SRTLNQ   EQU   *-SRTKEY            RECORD LENGTH                                
         EJECT                                                                  
*              DSECT FOR PRINT LINE                                             
         SPACE 1                                                                
CSPD     DSECT                                                                  
CSP1     DS    CL1                                                              
CSPINV   DS    CL6                 INVOICE NUMBER                               
         DS    CL7                                                              
CSPDESC  DS    CL11                DESCRIPTION - IS IT JOB OR EST               
CSPNUM   DS    CL6                 JOB NUMBER/EST NUMBER                        
         DS    CL2                                                              
CSPNAME  DS    CL21                JOB NAME - BLANK FOR EST                     
         DS    CL1                                                              
CSPAMNT  DS    CL13                INVOICE AMOUNT                               
         ORG   CSPAMNT+12                                                       
CSPACDUE DS    CL14                AMOUNT DUE                                   
CSPLNQ   EQU   *-CSP1              LENGTH OF LINE                               
         EJECT                                                                  
*        DDLOGOD                                                                
*        ACGENBOTH                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        DDCNTRL                                                                
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDREMOTED                                                              
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026ACREPCS02 05/23/16'                                      
         END                                                                    
